-module(eGLock).

-define(CASE(Cond, Then, That), case Cond of true -> Then; _ -> That end).

%% 默认超时时间单位:Ms
-define(LockTimeOut, 5000).
%% 超时重试时间单位:Ms
-define(ReTryTime, 3).

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
	doTryLock(KeyOrKeys, TimeOut).

doTryLock(KeyOrKeys, TimeOut) ->
	case eNifLock:tryLock(KeyOrKeys) of
		true ->
			true;
		_ ->
			loopLock(KeyOrKeys, TimeOut)
	end.

loopLock(KeyOrKeys, TimeOut) ->
	receive
	after ?ReTryTime ->
		LTimeOut = ?CASE(TimeOut == infinity, TimeOut, TimeOut - ?ReTryTime),
		case LTimeOut >= 0 of
			true ->
				case eNifLock:tryLock(KeyOrKeys) of
					true ->
						true;
					_ ->
						loopLock(KeyOrKeys, LTimeOut)
				end;
			_ ->
				ltimeout
		end
	end.


-spec releaseLock(KeyOrKeys :: term() | [term()]) -> ok.
releaseLock(KeyOrKeys) ->
	eNifLock:releaseLock(KeyOrKeys).

-spec getLockPid(KeyOrKeys :: term() | [term()]) -> ok.
getLockPid(KeyOrKeys) ->
	case is_list(KeyOrKeys) of
		true ->
			[{OneKey, eNifLock:getLockPid(OneKey)} || OneKey <- KeyOrKeys];
		_ ->
			{KeyOrKeys, eNifLock:getLockPid(KeyOrKeys)}
	end.

-spec lockApply(KeyOrKeys :: term() | [term()], MFAOrFun :: {M :: atom(), F :: atom(), Args :: list()} | {Fun :: function(), Args :: list()}) -> term() | {error, ltimeout} | {error, {lock_apply_error, term()}}.
lockApply(KeyOrKeys, MFAOrFun) ->
	lockApply(KeyOrKeys, MFAOrFun, ?LockTimeOut).

-spec lockApply(KeyOrKeys :: term() | [term()], MFAOrFun :: {M :: atom(), F :: atom(), Args :: list()} | {Fun :: function(), Args :: list()}, TimeOut :: integer() | infinity) -> term().
lockApply(KeyOrKeys, MFAOrFun, TimeOut) ->
	case doTryLock(KeyOrKeys, TimeOut) of
		true ->
			try doApply(MFAOrFun)
			catch C:R:S ->
				{error, {lock_apply_error, {C, R, S}}}
			after
				eNifLock:releaseLock(KeyOrKeys),
				ok
			end;
		ltimeout ->
			{error, ltimeout}
	end.

doApply({M, F, Args}) ->
	apply(M, F, Args);
doApply({Fun, Args}) ->
	apply(Fun, Args).

	

