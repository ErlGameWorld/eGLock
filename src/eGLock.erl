-module(eGLock).

-include("eGLock.hrl").

-export([
	lockApply/2
	, lockApply/3
]).

-spec lockApply(KeyOrKeys :: term() | [term()], MFAOrFun :: {M :: atom(), F :: atom(), Args :: list()} | {Fun :: function(), Args :: list()}) -> term().
lockApply(KeyOrKeys, MFAOrFun) ->
	lockApply(KeyOrKeys, MFAOrFun, ?LockTimeOut).

-spec lockApply(KeyOrKeys :: term() | [term()], MFAOrFun :: {M :: atom(), F :: atom(), Args :: list()} | {Fun :: function(), Args :: list()}, TimeOut :: integer() | infinity) -> term().
lockApply(KeyOrKeys, MFAOrFun, TimeOut) ->
	link(whereis(?eGLockMgr)),
	Pid = self(),
	case is_list(KeyOrKeys) of
		true ->
			KeyPids = [{OneKey, Pid} || OneKey <- KeyOrKeys],
			lockApplys(KeyPids, KeyOrKeys, MFAOrFun, TimeOut, erlang:system_time(millisecond));
		_ ->
			lockApply({KeyOrKeys, Pid}, KeyOrKeys, MFAOrFun, TimeOut, erlang:system_time(millisecond))
	end.

-define(CASE(Cond, Then, That), case Cond of true -> Then; _ -> That end).
lockApply(KeyPid, Key, MFAOrFun, TimeOut, FirstTime) ->
	case ets:insert_new(?EtsGLockKey, KeyPid) of
		true ->
			try doApply(MFAOrFun)
			catch C:R:S ->
				{error, {lock_apply_error, {C, R, S}}}
			after
				ets:delete(?EtsGLockKey, Key),
				unlink(whereis(?eGLockMgr)),
				ok
			end;
		_ ->
			loopTry(KeyPid, Key, MFAOrFun, TimeOut, FirstTime)
	end.

loopTry(KeyPid, Key, MFAOrFun, TimeOut, FirstTime) ->
	receive
	after ?ReTryTime ->
		case ets:lookup(?EtsGLockKey, Key) of
			[] ->
				lockApply(KeyPid, Key, MFAOrFun, TimeOut, FirstTime);
			_ ->
				case TimeOut of
					infinity ->
						loopTry(KeyPid, Key, MFAOrFun, TimeOut, FirstTime);
					_ ->
						LTimeOut = TimeOut - abs(erlang:system_time(millisecond) - FirstTime),
						case LTimeOut =< 0 of
							true ->
								unlink(whereis(?eGLockMgr)),
								{error, {lock_timeout, Key}};
							_ ->
								loopTry(KeyPid, Key, MFAOrFun, TimeOut, FirstTime)
						end
				end
		end
	end.

lockApplys(KeyPids, Keys, MFAOrFun, TimeOut, FirstTime) ->
	case ets:insert_new(?EtsGLockKey, KeyPids) of
		true ->
			try doApply(MFAOrFun)
			catch C:R:S ->
				{error, {lock_apply_error, {C, R, S}}}
			after
				[ets:delete(?EtsGLockKey, OneKey) || OneKey <- Keys],
				unlink(whereis(?eGLockMgr)),
				ok
			end;
		_ ->
			loopTrys(KeyPids, Keys, MFAOrFun, TimeOut, FirstTime)
	end.

loopTrys(KeyPids, Keys, MFAOrFun, TimeOut, FirstTime) ->
	receive
	after ?ReTryTime ->
		[Key | _] = Keys,
		case ets:lookup(?EtsGLockKey, Key) of
			[] ->
				lockApplys(KeyPids, Keys, MFAOrFun, TimeOut, FirstTime);
			_ ->
				case TimeOut of
					infinity ->
						loopTrys(KeyPids, Keys, MFAOrFun, TimeOut, FirstTime);
					_ ->
						LTimeOut = TimeOut - abs(erlang:system_time(millisecond) - FirstTime),
						case LTimeOut =< 0 of
							true ->
								unlink(whereis(?eGLockMgr)),
								{error, {lock_timeout, Keys}};
							_ ->
								loopTrys(KeyPids, Keys, MFAOrFun, TimeOut, FirstTime)
						end
				end
		end
	end.

doApply({M, F, A}) ->
	apply(M, F, A);
doApply({Fun, Args}) ->
	apply(Fun, Args).
