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
	GLockMgrPid = persistent_term:get(?eALockMgr),
	ALockRef = persistent_term:get(?eALockRef),
	CurPid = self(),
	PidInt = termInt:termInt(CurPid),
	case is_list(KeyOrKeys) of
		true ->
			KeyIxs = getKexIxs(KeyOrKeys, []),
			lockApplys(KeyIxs, KeyOrKeys, ALockRef, CurPid, PidInt, GLockMgrPid, MFAOrFun, TimeOut);
		_ ->
			lockApply(erlang:phash2(KeyOrKeys, ?eALockSize) + 1, KeyOrKeys, ALockRef, CurPid, PidInt, GLockMgrPid, MFAOrFun, TimeOut)
	end.

lockApply(KeyIx, Key, ALockRef, CurPid, PidInt, GLockMgrPid, MFAOrFun, TimeOut) ->
	link(GLockMgrPid),
	ets:insert(?EtsGLockPid, {CurPid, KeyIx}),
	case atomics:compare_exchange(ALockRef, KeyIx, 0, PidInt) of
		ok ->
			try doApply(MFAOrFun)
			catch C:R:S ->
				{error, {lock_apply_error, {C, R, S}}}
			after
				atomics:exchange(ALockRef, KeyIx, 0),
				ets:delete(?EtsGLockPid, CurPid),
				unlink(GLockMgrPid),
				ok
			end;
		_ ->
			loopTry(KeyIx, Key, ALockRef, CurPid, PidInt, GLockMgrPid, MFAOrFun, TimeOut)
	end.

loopTry(KeyIx, Key, ALockRef, CurPid, PidInt, GLockMgrPid, MFAOrFun, TimeOut) ->
	receive
	after ?ReTryTime ->
		LTimeOut = ?CASE(TimeOut == infinity, TimeOut, TimeOut - ?ReTryTime),
		case LTimeOut >= 0 of
			true ->
				case atomics:compare_exchange(ALockRef, KeyIx, 0, PidInt) of
					ok ->
						try doApply(MFAOrFun)
						catch C:R:S ->
							{error, {lock_apply_error, {C, R, S}}}
						after
							atomics:exchange(ALockRef, KeyIx, 0),
							ets:delete(?EtsGLockPid, CurPid),
							unlink(GLockMgrPid),
							ok
						end;
					_ ->
						loopTry(KeyIx, Key, ALockRef, CurPid, PidInt, GLockMgrPid, MFAOrFun, LTimeOut)
				end;
			_ ->
				ets:delete(?EtsGLockPid, CurPid),
				unlink(GLockMgrPid),
				{error, {lock_timeout, Key}}
		end
	end.

lockApplys(KeyIxs, Keys, ALockRef, CurPid, PidInt, GLockMgrPid, MFAOrFun, TimeOut) ->
	CurPid = self(),
	link(GLockMgrPid),
	ets:insert(?EtsGLockPid, {CurPid, KeyIxs}),
	case tryLockAll(KeyIxs, ALockRef, PidInt, []) of
		ok ->
			try doApply(MFAOrFun)
			catch C:R:S ->
				{error, {lock_apply_error, {C, R, S}}}
			after
				[atomics:exchange(ALockRef, KeyIx, 0) || KeyIx <- KeyIxs],
				ets:delete(?EtsGLockPid, CurPid),
				unlink(GLockMgrPid),
				ok
			end;
		_ ->
			loopTrys(KeyIxs, Keys, ALockRef, CurPid, PidInt, GLockMgrPid, MFAOrFun, TimeOut)
	end.

loopTrys(KeyIxs, Keys, ALockRef, CurPid, PidInt, GLockMgrPid, MFAOrFun, TimeOut) ->
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
							ets:delete(?EtsGLockPid, CurPid),
							unlink(GLockMgrPid),
							ok
						end;
					_ ->
						loopTrys(KeyIxs, Keys, ALockRef, CurPid, PidInt, GLockMgrPid, MFAOrFun, TimeOut)
				end;
			_ ->
				ets:delete(?EtsGLockPid, CurPid),
				unlink(GLockMgrPid),
				{error, {lock_timeout, Keys}}
		end
	end.

getKexIxs([], IxAcc) -> IxAcc;
getKexIxs([Key | Keys], IxAcc) ->
	KeyIx = erlang:phash2(Key, ?eALockSize) + 1,
	getKexIxs(Keys, ?CASE(lists:member(KeyIx, IxAcc), IxAcc, [KeyIx | IxAcc])).

tryLockAll([], _ALockRef, _PidInt, _LockAcc) ->
	ok;
tryLockAll([KeyIx | KeyIxs], ALockRef, PidInt, LockAcc) ->
	case atomics:compare_exchange(ALockRef, KeyIx, 0, PidInt) of
		ok ->
			tryLockAll(KeyIxs, ALockRef, PidInt, [KeyIx | LockAcc]);
		_ ->
			[atomics:exchange(ALockRef, OneLock, 0) || OneLock <- LockAcc],
			false
	end.

doApply({M, F, A}) ->
	apply(M, F, A);
doApply({Fun, Args}) ->
	apply(Fun, Args).

	

