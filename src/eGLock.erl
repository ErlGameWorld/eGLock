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
	GLockMgrPid = persistent_term:get(?eGLockMgr),
	link(GLockMgrPid),
	Pid = self(),
	case is_list(KeyOrKeys) of
		true ->
			KeyPids = [{OneKey, Pid} || OneKey <- KeyOrKeys],
			[FirstKey | _] = KeyOrKeys,
			lockApplys(KeyPids, KeyOrKeys, FirstKey, GLockMgrPid, MFAOrFun, TimeOut);
		_ ->
			lockApply({KeyOrKeys, Pid}, KeyOrKeys, GLockMgrPid, MFAOrFun, TimeOut)
	end.

-define(CASE(Cond, Then, That), case Cond of true -> Then; _ -> That end).
lockApply(KeyPid, Key, GLockMgrPid, MFAOrFun, TimeOut) ->
	case ets:insert_new(?EtsGLockKey, KeyPid) of
		true ->
			try doApply(MFAOrFun)
			catch C:R:S ->
				{error, {lock_apply_error, {C, R, S}}}
			after
				ets:delete(?EtsGLockKey, Key),
				unlink(GLockMgrPid),
				ok
			end;
		_ ->
			loopTry(KeyPid, Key, GLockMgrPid, MFAOrFun, TimeOut)
	end.

loopTry(KeyPid, Key, GLockMgrPid, MFAOrFun, TimeOut) ->
	receive
	after ?ReTryTime ->
		LTimeOut = ?CASE(TimeOut == infinity, TimeOut, TimeOut - ?ReTryTime),
		case LTimeOut >= 0 of
			true ->
				case ets:lookup(?EtsGLockKey, Key) of
					[] ->
						lockApply(KeyPid, Key, GLockMgrPid, MFAOrFun, LTimeOut);
					_ ->
						loopTry(KeyPid, Key, GLockMgrPid, MFAOrFun, LTimeOut)
				end;
			_ ->
				unlink(GLockMgrPid),
				{error, {lock_timeout, Key}}
		end
	end.

lockApplys(KeyPids, Keys, FirstKey, GLockMgrPid, MFAOrFun, TimeOut) ->
	case ets:insert_new(?EtsGLockKey, KeyPids) of
		true ->
			try doApply(MFAOrFun)
			catch C:R:S ->
				{error, {lock_apply_error, {C, R, S}}}
			after
				[ets:delete(?EtsGLockKey, OneKey) || OneKey <- Keys],
				unlink(GLockMgrPid),
				ok
			end;
		_ ->
			loopTrys(KeyPids, Keys, FirstKey, GLockMgrPid, MFAOrFun, TimeOut)
	end.

loopTrys(KeyPids, Keys, FirstKey, GLockMgrPid, MFAOrFun, TimeOut) ->
	receive
	after ?ReTryTime ->
		LTimeOut = ?CASE(TimeOut == infinity, TimeOut, TimeOut - ?ReTryTime),
		case LTimeOut >= 0 of
			true ->
				case ets:lookup(?EtsGLockKey, FirstKey) of
					[] ->
						lockApplys(KeyPids, Keys, FirstKey, GLockMgrPid, MFAOrFun, LTimeOut);
					_ ->
						loopTrys(KeyPids, Keys, FirstKey, GLockMgrPid, MFAOrFun, LTimeOut)
				end;
			_ ->
				unlink(GLockMgrPid),
				{error, {lock_timeout, Keys}}
		end
	end.

doApply({M, F, A}) ->
	apply(M, F, A);
doApply({Fun, Args}) ->
	apply(Fun, Args).
