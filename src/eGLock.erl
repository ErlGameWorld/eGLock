-module(eGLock).

-include("eGLock.hrl").
-define(CASE(Cond, Then, That), case Cond of true -> Then; _ -> That end).

-export([
	tryLock/1
	, tryLock/2
	, releaseLock/1
	, lockApply/2
	, lockApply/3
]).

-spec tryLock(KeyOrKeys :: term() | [term()]) -> ok | timeout.
tryLock(KeyOrKeys) ->
	tryLock(KeyOrKeys, ?LockTimeOut).

tryLock(KeyOrKeys, TimeOut) ->
	ALockRef = persistent_term:get(?eGLockRef),
	PidInt = eGPidInt:pidToInt(self()),
	case is_list(KeyOrKeys) of
		true ->
			KeyIxs = getKexIxs(KeyOrKeys, []),
			tryLocks(KeyIxs, ALockRef, PidInt, TimeOut);
		_ ->
			tryLock(erlang:phash2(KeyOrKeys, ?eGLockSize) + 1, ALockRef, PidInt, TimeOut)
	end.

tryLock(KeyIx, ALockRef, PidInt, TimeOut) ->
	case tryLockOne(KeyIx, ALockRef, PidInt) of
		ok ->
			ok;
		_ ->
			loopLock(KeyIx, ALockRef, PidInt, TimeOut)
	end.

loopLock(KeyIx, ALockRef, PidInt, TimeOut) ->
	receive
	after ?ReTryTime ->
		LTimeOut = ?CASE(TimeOut == infinity, TimeOut, TimeOut - ?ReTryTime),
		case LTimeOut >= 0 of
			true ->
				case tryLockOne(KeyIx, ALockRef, PidInt) of
					ok ->
						ok;
					_ ->
						loopLock(KeyIx, ALockRef, PidInt, LTimeOut)
				end;
			_ ->
				timeout
		end
	end.

tryLocks(KeyIxs, ALockRef, PidInt, TimeOut) ->
	case tryLockAll(KeyIxs, ALockRef, PidInt, []) of
		ok ->
			ok;
		_ ->
			loopLocks(KeyIxs, ALockRef, PidInt, TimeOut)
	end.

loopLocks(KeyIxs, ALockRef, PidInt, TimeOut) ->
	receive
	after ?ReTryTime ->
		LTimeOut = ?CASE(TimeOut == infinity, TimeOut, TimeOut - ?ReTryTime),
		case LTimeOut >= 0 of
			true ->
				case tryLockAll(KeyIxs, ALockRef, PidInt, []) of
					ok ->
						ok;
					_ ->
						loopLocks(KeyIxs, ALockRef, PidInt, LTimeOut)
				end;
			_ ->
				timeout
		end
	end.

-spec releaseLock(KeyOrKeys :: term() | [term()]) -> ok.
releaseLock(KeyOrKeys) ->
	ALockRef = persistent_term:get(?eGLockRef),
	PidInt = eGPidInt:pidToInt(self()),
	case is_list(KeyOrKeys) of
		true ->
			KeyIxs = getKexIxs(KeyOrKeys, []),
			[atomics:compare_exchange(ALockRef, OneKeyIx, PidInt, 0) || OneKeyIx <- KeyIxs],
			ok;
		_ ->
			atomics:compare_exchange(ALockRef, erlang:phash2(KeyOrKeys, ?eGLockSize) + 1, PidInt, 0),
			ok
	end.

-spec lockApply(KeyOrKeys :: term() | [term()], MFAOrFun :: {M :: atom(), F :: atom(), Args :: list()} | {Fun :: function(), Args :: list()}) -> term().
lockApply(KeyOrKeys, MFAOrFun) ->
	lockApply(KeyOrKeys, MFAOrFun, ?LockTimeOut).

-spec lockApply(KeyOrKeys :: term() | [term()], MFAOrFun :: {M :: atom(), F :: atom(), Args :: list()} | {Fun :: function(), Args :: list()}, TimeOut :: integer() | infinity) -> term().
lockApply(KeyOrKeys, MFAOrFun, TimeOut) ->
	ALockRef = persistent_term:get(?eGLockRef),
	PidInt = eGPidInt:pidToInt(self()),
	case is_list(KeyOrKeys) of
		true ->
			KeyIxs = getKexIxs(KeyOrKeys, []),
			lockApplys(KeyIxs, KeyOrKeys, ALockRef, PidInt, MFAOrFun, TimeOut);
		_ ->
			lockApply(erlang:phash2(KeyOrKeys, ?eGLockSize) + 1, KeyOrKeys, ALockRef, PidInt, MFAOrFun, TimeOut)
	end.

lockApply(KeyIx, Key, ALockRef, PidInt, MFAOrFun, TimeOut) ->
	case tryLockOne(KeyIx, ALockRef, PidInt) of
		ok ->
			try doApply(MFAOrFun)
			catch C:R:S ->
				{error, {lock_apply_error, {C, R, S}}}
			after
				atomics:exchange(ALockRef, KeyIx, 0),
				ok
			end;
		_ ->
			loopApply(KeyIx, Key, ALockRef, PidInt, MFAOrFun, TimeOut)
	end.

loopApply(KeyIx, Key, ALockRef, PidInt, MFAOrFun, TimeOut) ->
	receive
	after ?ReTryTime ->
		LTimeOut = ?CASE(TimeOut == infinity, TimeOut, TimeOut - ?ReTryTime),
		case LTimeOut >= 0 of
			true ->
				case tryLockOne(KeyIx, ALockRef, PidInt) of
					ok ->
						try doApply(MFAOrFun)
						catch C:R:S ->
							{error, {lock_apply_error, {C, R, S}}}
						after
							atomics:exchange(ALockRef, KeyIx, 0),
							ok
						end;
					_ ->
						loopApply(KeyIx, Key, ALockRef, PidInt, MFAOrFun, LTimeOut)
				end;
			_ ->
				{error, {lock_timeout, Key}}
		end
	end.

lockApplys(KeyIxs, Keys, ALockRef, PidInt, MFAOrFun, TimeOut) ->
	case tryLockAll(KeyIxs, ALockRef, PidInt, []) of
		ok ->
			try doApply(MFAOrFun)
			catch C:R:S ->
				{error, {lock_apply_error, {C, R, S}}}
			after
				[atomics:exchange(ALockRef, KeyIx, 0) || KeyIx <- KeyIxs],
				ok
			end;
		_ ->
			loopApplys(KeyIxs, Keys, ALockRef, PidInt, MFAOrFun, TimeOut)
	end.

loopApplys(KeyIxs, Keys, ALockRef, PidInt, MFAOrFun, TimeOut) ->
	receive
	after ?ReTryTime ->
		LTimeOut = ?CASE(TimeOut == infinity, TimeOut, TimeOut - ?ReTryTime),
		case LTimeOut >= 0 of
			true ->
				case tryLockAll(KeyIxs, ALockRef, PidInt, []) of
					ok ->
						try doApply(MFAOrFun)
						catch C:R:S ->
							{error, {lock_apply_error, {C, R, S}}}
						after
							[atomics:exchange(ALockRef, KeyIx, 0) || KeyIx <- KeyIxs],
							ok
						end;
					_ ->
						loopApplys(KeyIxs, Keys, ALockRef, PidInt, MFAOrFun, LTimeOut)
				end;
			_ ->
				{error, {lock_timeout, Keys}}
		end
	end.

getKexIxs([], IxAcc) -> IxAcc;
getKexIxs([Key | Keys], IxAcc) ->
	KeyIx = erlang:phash2(Key, ?eGLockSize) + 1,
	getKexIxs(Keys, ?CASE(lists:member(KeyIx, IxAcc), IxAcc, [KeyIx | IxAcc])).

tryLockOne(KeyIx, ALockRef, PidInt) ->
	case atomics:compare_exchange(ALockRef, KeyIx, 0, PidInt) of
		ok ->
			ok;
		OldPidInt ->
			case is_process_alive(eGPidInt:intToPid(OldPidInt)) of
				true ->
					false;
				_ ->
					case atomics:compare_exchange(ALockRef, KeyIx, OldPidInt, PidInt) of
						ok ->
							ok;
						_ ->
							false
					end
			end
	end.

tryLockAll([], _ALockRef, _PidInt, _LockAcc) ->
	ok;
tryLockAll([KeyIx | KeyIxs], ALockRef, PidInt, LockAcc) ->
	case tryLockOne(KeyIx, ALockRef, PidInt) of
		ok ->
			tryLockAll(KeyIxs, ALockRef, PidInt, [KeyIx | LockAcc]);
		_ ->
			[atomics:compare_exchange(ALockRef, OneLock, PidInt, 0) || OneLock <- LockAcc],
			false
	end.

doApply({M, F, A}) ->
	apply(M, F, A);
doApply({Fun, Args}) ->
	apply(Fun, Args).

	

