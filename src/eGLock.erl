-module(eGLock).

-define(CASE(Cond, Then, That), case Cond of true -> Then; _ -> That end).

%% 默认超时时间单位:Ms
-define(LockTimeOut, 5000).
%% 超时重试时间单位:Ms
-define(ReTryTime, 3).
%% 数组数量
-define(eGLockSize, 2097152).

-export([
	tryLock/1
	, tryLock/2
	, releaseLock/1
	, getLockPid/1
	, lockApply/2
	, lockApply/3
]).

-spec tryLock(KeyOrKeys :: term() | [term()]) -> true | ltimeout.
tryLock(KeyOrKeys) ->
	tryLock(KeyOrKeys, ?LockTimeOut).

tryLock(KeyOrKeys, TimeOut) ->
	case is_list(KeyOrKeys) of
		true ->
			KeyIxs = getKexIxs(KeyOrKeys, []),
			doTryLocks(KeyIxs, TimeOut);
		_ ->
			doTryLock(erlang:phash2(KeyOrKeys, ?eGLockSize), TimeOut)
	end.

doTryLock(KeyIx, TimeOut) ->
	case eNifLock:tryLock(KeyIx) of
		true ->
			true;
		_ ->
			loopLock(KeyIx, TimeOut)
	end.

loopLock(KeyIx, TimeOut) ->
	receive
	after ?ReTryTime ->
		LTimeOut = ?CASE(TimeOut == infinity, TimeOut, TimeOut - ?ReTryTime),
		case LTimeOut >= 0 of
			true ->
				case eNifLock:tryLock(KeyIx) of
					true ->
						true;
					_ ->
						loopLock(KeyIx, LTimeOut)
				end;
			_ ->
				ltimeout
		end
	end.

doTryLocks(KeyIxs, TimeOut) ->
	case eNifLock:tryLocks(KeyIxs) of
		true ->
			true;
		_ ->
			loopLocks(KeyIxs, TimeOut)
	end.

loopLocks(KeyIxs, TimeOut) ->
	receive
	after ?ReTryTime ->
		LTimeOut = ?CASE(TimeOut == infinity, TimeOut, TimeOut - ?ReTryTime),
		case LTimeOut >= 0 of
			true ->
				case eNifLock:tryLocks(KeyIxs) of
					true ->
						true;
					_ ->
						loopLocks(KeyIxs, LTimeOut)
				end;
			_ ->
				ltimeout
		end
	end.

-spec releaseLock(KeyOrKeys :: term() | [term()]) -> ok.
releaseLock(KeyOrKeys) ->
	case is_list(KeyOrKeys) of
		true ->
			KeyIxs = getKexIxs(KeyOrKeys, []),
			eNifLock:releaseLocks(KeyIxs);
		_ ->
			eNifLock:releaseLock(erlang:phash2(KeyOrKeys, ?eGLockSize))
	end.

-spec getLockPid(KeyOrKeys :: term() | [term()]) -> ok.
getLockPid(KeyOrKeys) ->
	case is_list(KeyOrKeys) of
		true ->
			[{OneKey, eNifLock:getLockPid(erlang:phash2(OneKey, ?eGLockSize))} || OneKey <- KeyOrKeys];
		_ ->
			{KeyOrKeys, eNifLock:getLockPid(erlang:phash2(KeyOrKeys, ?eGLockSize))}
	end.

-spec lockApply(KeyOrKeys :: term() | [term()], MFAOrFun :: {M :: atom(), F :: atom(), Args :: list()} | {Fun :: function(), Args :: list()}) -> term() | {error, ltimeout} | {error, {lock_apply_error, term()}}.
lockApply(KeyOrKeys, MFAOrFun) ->
	lockApply(KeyOrKeys, MFAOrFun, ?LockTimeOut).

-spec lockApply(KeyOrKeys :: term() | [term()], MFAOrFun :: {M :: atom(), F :: atom(), Args :: list()} | {Fun :: function(), Args :: list()}, TimeOut :: integer() | infinity) -> term().
lockApply(KeyOrKeys, MFAOrFun, TimeOut) ->
	case is_list(KeyOrKeys) of
		true ->
			KeyIxs = getKexIxs(KeyOrKeys, []),
			case doTryLocks(KeyIxs, TimeOut) of
				true ->
					try doApply(MFAOrFun)
					catch C:R:S ->
						{error, {lock_apply_error, {C, R, S}}}
					after
						eNifLock:releaseLocks(KeyIxs),
						ok
					end;
				ltimeout ->
					{error, ltimeout}
			end;
		_ ->
			KeyIx = erlang:phash2(KeyOrKeys, ?eGLockSize),
			case doTryLock(KeyIx, TimeOut) of
				true ->
					try doApply(MFAOrFun)
					catch C:R:S ->
						{error, {lock_apply_error, {C, R, S}}}
					after
						eNifLock:releaseLock(KeyIx),
						ok
					end;
				ltimeout ->
					{error, ltimeout}
			end
	end.

getKexIxs([], IxAcc) -> IxAcc;
getKexIxs([Key | Keys], IxAcc) ->
	KeyIx = erlang:phash2(Key, ?eGLockSize),
	getKexIxs(Keys, ?CASE(lists:member(KeyIx, IxAcc), IxAcc, [KeyIx | IxAcc])).

doApply({M, F, Args}) ->
	apply(M, F, Args);
doApply({Fun, Args}) ->
	apply(Fun, Args).

	

