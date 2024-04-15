-module(eALock).

-include("eGLock.hrl").
-define(CASE(Cond, Then, That), case Cond of true -> Then; _ -> That end).

-export([
	lockApply/2
	, lockApply/3
]).

-spec lockApply(KeyOrKeys :: term() | [term()], MFAOrFun :: {M :: atom(), F :: atom(), Args :: list()} | {Fun :: function(), Args :: list()}) -> term().
lockApply(KeyOrKeys, MFAOrFun) ->
	lockApply(KeyOrKeys, MFAOrFun, ?LockTimeOut).

-spec lockApply(KeyOrKeys :: term() | [term()], MFAOrFun :: {M :: atom(), F :: atom(), Args :: list()} | {Fun :: function(), Args :: list()}, TimeOut :: integer() | infinity) -> term().
lockApply(KeyOrKeys, MFAOrFun, TimeOut) ->
	ALockRef = persistent_term:get(?eALockRef),
	CurPid = self(),
	PidInt = eGPidInt:pidToInt(CurPid),
	case is_list(KeyOrKeys) of
		true ->
			KeyIxs = getKexIxs(KeyOrKeys, []),
			lockApplys(KeyIxs, KeyOrKeys, ALockRef, CurPid, PidInt, MFAOrFun, TimeOut);
		_ ->
			lockApply(erlang:phash2(KeyOrKeys, ?eALockSize) + 1, KeyOrKeys, ALockRef, CurPid, PidInt, MFAOrFun, TimeOut)
	end.

lockApply(KeyIx, Key, ALockRef, CurPid, PidInt, MFAOrFun, TimeOut) ->
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
			loopTry(KeyIx, Key, ALockRef, CurPid, PidInt, MFAOrFun, TimeOut)
	end.

loopTry(KeyIx, Key, ALockRef, CurPid, PidInt, MFAOrFun, TimeOut) ->
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
						loopTry(KeyIx, Key, ALockRef, CurPid, PidInt, MFAOrFun, LTimeOut)
				end;
			_ ->
				{error, {lock_timeout, Key}}
		end
	end.

lockApplys(KeyIxs, Keys, ALockRef, CurPid, PidInt, MFAOrFun, TimeOut) ->
	CurPid = self(),
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
			loopTrys(KeyIxs, Keys, ALockRef, CurPid, PidInt, MFAOrFun, TimeOut)
	end.

loopTrys(KeyIxs, Keys, ALockRef, CurPid, PidInt, MFAOrFun, TimeOut) ->
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
						loopTrys(KeyIxs, Keys, ALockRef, CurPid, PidInt, MFAOrFun, TimeOut)
				end;
			_ ->
				{error, {lock_timeout, Keys}}
		end
	end.

getKexIxs([], IxAcc) -> IxAcc;
getKexIxs([Key | Keys], IxAcc) ->
	KeyIx = erlang:phash2(Key, ?eALockSize) + 1,
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

	

