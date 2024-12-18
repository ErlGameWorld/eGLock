-module(eGLock).

-define(CASE(Cond, Then, That), case Cond of true -> Then; _ -> That end).
-define(CASE(Expr, Expect, Then, ExprRet, That), case Expr of Expect -> Then; ExprRet -> That end).

%% 默认超时时间单位:Ms
-define(LockTimeOut, 5000).
%% 超时重试时间单位:Ms
-define(ReTryTime, 3).
%% 数组数量
-define(eGLockSize, 2097152).
%% 没有ets 表的key
-define(undefTab, undefTab).

-export([
	tryLock/1
	, tryLock/2
	, releaseLock/1
	, getLockPid/1
	, lockApply/2
	, lockApply/3
]).

-export([
	lockGet/1
	, lockGet/2
	, transaction/2
	, transaction/3
]).

-spec tryLock(KeyOrKeys :: term() | [term()]) -> true | lockTimeout.
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
				lockTimeout
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
				lockTimeout
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

-spec lockApply(KeyOrKeys :: term() | [term()], MFAOrFun :: {M :: atom(), F :: atom(), Args :: list()} | {Fun :: function(), Args :: list()}) -> term().
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
						error({lockApplyError, KeyOrKeys, MFAOrFun, {C, R, S}})
					after
						eNifLock:releaseLocks(KeyIxs),
						ok
					end;
				lockTimeout ->
					error({lockTimeout, KeyOrKeys, MFAOrFun})
			end;
		_ ->
			KeyIx = erlang:phash2(KeyOrKeys, ?eGLockSize),
			case doTryLock(KeyIx, TimeOut) of
				true ->
					try doApply(MFAOrFun)
					catch C:R:S ->
						error({lockApplyError, KeyOrKeys, MFAOrFun, {C, R, S}})
					after
						eNifLock:releaseLock(KeyIx),
						ok
					end;
				lockTimeout ->
					error({lockTimeout, KeyOrKeys, MFAOrFun})
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

-spec lockGet(KeyOrKeys :: term() | [term()]) -> map().
lockGet(KeyOrKeys) ->
	lockGet(KeyOrKeys, ?LockTimeOut).

-spec lockGet(KeyOrKeys :: term() | [term()], TimeOut :: integer() | infinity) -> map().
lockGet({?undefTab, Key = GetKey}, TimeOut) ->
	KeyIx = erlang:phash2(GetKey, ?eGLockSize),
	case doTryLock(KeyIx, TimeOut) of
		true ->
			try
				#{{?undefTab, GetKey} => undefined}
			catch C:R:S ->
				error({lockGetError, {?undefTab, Key}, {C, R, S}})
			after
				eNifLock:releaseLock(KeyIx),
				ok
			end;
		lockTimeout ->
			error({lockTimeout, {?undefTab, Key}})
	end;
lockGet({EtsTab, Key} = GetKey, TimeOut) ->
	KeyIx = erlang:phash2(GetKey, ?eGLockSize),
	case doTryLock(KeyIx, TimeOut) of
		true ->
			try
				#{GetKey => getEtsTabValue(EtsTab, Key, undefined)}
			catch C:R:S ->
				error({lockGetError, GetKey, {C, R, S}})
			after
				eNifLock:releaseLock(KeyIx),
				ok
			end;
		lockTimeout ->
			error({lockTimeout, GetKey})
	end;
lockGet({?undefTab, Key = GetKey, DefValue}, TimeOut) ->
	KeyIx = erlang:phash2(GetKey, ?eGLockSize),
	case doTryLock(KeyIx, TimeOut) of
		true ->
			try
				#{{?undefTab, GetKey} => getDefValue(DefValue)}
			catch C:R:S ->
				error({lockGetError, {?undefTab, Key, DefValue}, {C, R, S}})
			after
				eNifLock:releaseLock(KeyIx),
				ok
			end;
		lockTimeout ->
			error({lockTimeout, {?undefTab, Key, DefValue}})
	end;
lockGet({EtsTab, Key, DefValue}, TimeOut) ->
	GetKey = {EtsTab, Key},
	KeyIx = erlang:phash2(GetKey, ?eGLockSize),
	case doTryLock(KeyIx, TimeOut) of
		true ->
			try
				#{GetKey => getEtsTabValue(EtsTab, Key, DefValue)}
			catch C:R:S ->
				error({lockGetError, {EtsTab, Key, DefValue}, {C, R, S}})
			after
				eNifLock:releaseLock(KeyIx),
				ok
			end;
		lockTimeout ->
			error({lockTimeout, {EtsTab, Key, DefValue}})
	end;
lockGet(EtsTabKeys, TimeOut) ->
	{KeyIxs, KeysMap} = getKeyIxAndMaps(EtsTabKeys, [], #{}),
	case doTryLocks(KeyIxs, TimeOut) of
		true ->
			try
				#{OneGetKey => getEtsTabValue(OneEtsTab, OneKey, OneDefValue) || {OneEtsTab, OneKey} = OneGetKey := OneDefValue <- KeysMap}
			catch C:R:S ->
				error({lockGetError, EtsTabKeys, {C, R, S}})
			after
				eNifLock:releaseLocks(KeyIxs),
				ok
			end;
		lockTimeout ->
			error({lockTimeout, EtsTabKeys})
	end.

getKeyIxAndMaps([], IxAcc, KeysMap) -> {IxAcc, KeysMap};
getKeyIxAndMaps([Key | Keys], IxAcc, KeysMap) ->
	case Key of
		{?undefTab, JustKey} ->
			GetKey = Key,
			LDefValue = undefined,
			KeyIx = erlang:phash2(Key, ?eGLockSize);
		{?undefTab, JustKey, DefValue} ->
			GetKey = {?undefTab, JustKey},
			LDefValue = DefValue,
			KeyIx = erlang:phash2(Key, ?eGLockSize);
		{EtsTab, TabKey} ->
			GetKey = Key,
			LDefValue = undefined,
			KeyIx = erlang:phash2(Key, ?eGLockSize);
		{EtsTab, TabKey, DefValue} ->
			GetKey = {EtsTab, TabKey},
			LDefValue = DefValue,
			KeyIx = erlang:phash2(Key, ?eGLockSize)
	end,
	getKeyIxAndMaps(Keys, ?CASE(lists:member(KeyIx, IxAcc), IxAcc, [KeyIx | IxAcc]), KeysMap#{GetKey => LDefValue}).

transactionApply({M, F, Args}, EtsTabValue) ->
	apply(M, F, [EtsTabValue | Args]);
transactionApply({Fun, Args}, EtsTabValue) ->
	apply(Fun, [EtsTabValue | Args]).

-spec transaction(KeyOrKeys :: term() | [term()], MFAOrFun :: {M :: atom(), F :: atom(), Args :: list()} | {Fun :: function(), Args :: list()}) -> term().
transaction(EtsTabKeys, MFAOrFun) ->
	transaction(EtsTabKeys, MFAOrFun, ?LockTimeOut).

-spec transaction(KeyOrKeys :: term() | [term()], MFAOrFun :: {M :: atom(), F :: atom(), Args :: list()} | {Fun :: function(), Args :: list()}, TimeOut :: integer() | infinity) -> term().
transaction(EtsTabKeys, MFAOrFun, TimeOut) ->
	{KeyIxs, KeysMap} = getKeyIxAndMaps(EtsTabKeys, [], #{}),
	case doTryLocks(KeyIxs, TimeOut) of
		true ->
			try
				EtsTabValue = #{OneGetKey => getEtsTabValue(OneEtsTab, OneKey, OneDefValue) || {OneEtsTab, OneKey} = OneGetKey := OneDefValue <- KeysMap},
				case transactionApply(MFAOrFun, EtsTabValue) of
					{alterTab, AlterTab} ->
						[changeEtsTabValue(OneEtsTab, OneKey, ChangeValue) || {OneEtsTab, OneKey} := ChangeValue <- AlterTab],
						ok;
					{alterTab, Ret, AlterTab} ->
						[changeEtsTabValue(OneEtsTab, OneKey, ChangeValue) || {OneEtsTab, OneKey} := ChangeValue <- AlterTab],
						Ret;
					OtherRet ->
						OtherRet
				end
			catch
				throw:Throw -> Throw;
				C:R:S ->
					error({lockTransactionError, EtsTabKeys, MFAOrFun, {C, R, S}})
			after
				eNifLock:releaseLocks(KeyIxs),
				ok
			end;
		lockTimeout ->
			error({lockTimeout, EtsTabKeys, MFAOrFun})
	end.

getDefValue(undefined) -> undefined;
getDefValue({DefFun, Args}) when is_function(DefFun) -> erlang:apply(DefFun, Args);
getDefValue(DefValue) -> DefValue.

getEtsTabValue(?undefTab, _Key, DefValue) ->
	getDefValue(DefValue);
getEtsTabValue(Ets, Key, DefValue) ->
	case ets:lookup(Ets, Key) of
		[] ->
			getDefValue(DefValue);
		[OneValue] ->
			OneValue
	end.

changeEtsTabValue(?undefTab, _Key, _Value) ->
	ok;
changeEtsTabValue(Ets, Key, delete) ->
	ets:delete(Ets, Key);
changeEtsTabValue(Ets, _Key, Value) ->
	ets:insert(Ets, Value).