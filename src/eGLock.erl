-module(eGLock).

-include("eGLock.hrl").

-export([
	lockApply/2
	, lockApply/3
]).

-spec lockApply(KeyOrKeys :: tuple() |[tuple()], MFAOrFun :: {M :: atom(), F :: atom(), Args :: list()} | {Fun :: function(), Args :: list()}) -> term().
lockApply(KeyOrKeys, MFAOrFun) ->
	lockApply(KeyOrKeys, MFAOrFun, ?LockTimeOut).

-spec lockApply(KeyOrKeys :: tuple() |[tuple()], MFAOrFun :: {M :: atom(), F :: atom(), Args :: list()} | {Fun :: function(), Args :: list()}, TimeOut :: integer() | infinity) -> term().
lockApply(KeyOrKeys, MFAOrFun, TimeOut) ->
	case KeyOrKeys of
		{_, _} ->
			lockApply(KeyOrKeys, MFAOrFun, TimeOut, erlang:system_time(millisecond));
		_ ->
			lockApplys(KeyOrKeys, MFAOrFun, TimeOut, erlang:system_time(millisecond))
	end.

-define(CASE(Cond, Then, That), case Cond of true -> Then; _ -> That end).
lockApply(Key, MFAOrFun, TimeOut, FirstTime) ->
	SelfPid = self(),
	PidInfo = {Key, SelfPid},
	ets:insert(?EtsGLockPid, PidInfo),
	case ets:insert_new(?EtsGLockKey, Key) of
		true ->
			try doApply(MFAOrFun)
			catch C:R:S ->
				{error, {lock_apply_error, {C, R, S}}}
			after
				ets:delete_object(?EtsGLockPid, PidInfo),
				ets:delete(?EtsGLockKey, element(1, Key)),
				WaitLockList = ets:lookup(?EtsGLockPid, Key),
				[WaitPid ! ?ReTryLockApply || {_Key, WaitPid} <- WaitLockList],
				ok
			end;
		_ ->
			LTimeOut = ?CASE(TimeOut == infinity, TimeOut, max(0, TimeOut - max(erlang:system_time(millisecond) - FirstTime, 0))),
			receive
				?ReTryLockApply ->
					lockApply(Key, MFAOrFun, TimeOut, FirstTime)
			after LTimeOut ->
				ets:delete_object(?EtsGLockPid, PidInfo),
				{error, {lock_timeout, Key}}
			end
	end.

lockApplys(Keys, MFAOrFun, TimeOut, FirstTime) ->
	SelfPid = self(),
	AllPidInfo = [{OneKey, SelfPid} || OneKey <- Keys],
	ets:insert(?EtsGLockPid, AllPidInfo),
	case ets:insert_new(?EtsGLockKey, Keys) of
		true ->
			try doApply(MFAOrFun)
			catch C:R:S ->
				{error, {lock_apply_error, {C, R, S}}}
			after
				[ets:delete_object(?EtsGLockPid, OnePidInfo) || OnePidInfo <- AllPidInfo],
				[ets:delete(?EtsGLockKey, element(1, OneKey)) || OneKey <- Keys],
				notifyKeys(Keys, #{}),
				ok
			end;
		_ ->
			LTimeOut = ?CASE(TimeOut == infinity, TimeOut, max(0, TimeOut - max(erlang:system_time(millisecond) - FirstTime, 0))),
			receive
				?ReTryLockApply ->
					lockApplys(Keys, MFAOrFun, TimeOut, FirstTime)
			after LTimeOut ->
				[ets:delete_object(?EtsGLockPid, OnePidInfo) || OnePidInfo <- AllPidInfo],
				{error, {lock_timeout, Keys}}
			end
	end.

doApply({M, F, A}) ->
	apply(M, F, A);
doApply({Fun, Args}) ->
	apply(Fun, Args).

notifyKeys([], _AccMap) -> ok;
notifyKeys([OneKey | Keys], AccMap) ->
	WaitLockList = ets:lookup(?EtsGLockPid, OneKey),
	NewAccMap = notifyToPid(WaitLockList, AccMap),
	notifyKeys(Keys, NewAccMap).

notifyToPid([], AccMap) -> AccMap;
notifyToPid([{_OneKey, WaitPid} | WaitLockList], AccMap) ->
	case maps:is_key(WaitPid, AccMap) of
		true ->
			notifyToPid(WaitLockList, AccMap);
		_ ->
			WaitPid ! ?ReTryLockApply,
			notifyToPid(WaitLockList, AccMap#{WaitPid => true})
	end.