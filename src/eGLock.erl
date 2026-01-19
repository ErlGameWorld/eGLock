-module(eGLock).

-compile(inline).
-compile({inline_size, 128}).

-define(CASE(Cond, Then, That), case Cond of true -> Then; _ -> That end).
-define(CASE(Expr, Expect, Then, ExprRet, That), case Expr of Expect -> Then; ExprRet -> That end).

%% 默认超时时间单位:Ms
-define(LockTimeOut, 5000).
%% 快速自旋次数（无延迟重试）
-define(SpinCount, 3).
%% 第一次超时重试时间单位:Ms
-define(ReTryTime1, 1).
%% 第二次超时重试时间单位:Ms
-define(ReTryTime2, 1).
%% 第三次超时重试时间单位:Ms
-define(ReTryTime3, 2).
%% 最后每次超时重试时间单位:Ms
-define(ReTryTimeL, 5).

%% 数组数量
-define(eGLockSize, 2097152).
%% 没有ets 表的key
-define(noneTab, noneTab).

-export([
	tryLock/1
	, tryLock/2
	, releaseLock/1
	, getLockPid/1
	, lockApply/2
	, lockApply/3
	
	, lockGet/1
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
	%% 先快速自旋重试，减少receive开销
	case spinTryLock(?SpinCount, KeyIx) of
		true ->
			true;
		_ ->
			loopLock1(KeyIx, TimeOut)
	end.

spinTryLock(0, _KeyIx) ->
	false;
spinTryLock(Count, KeyIx) ->
	case eNifLock:tryLock(KeyIx) of
		true ->
			true;
		_ ->
			spinTryLock(Count - 1, KeyIx)
	end.

loopLock1(KeyIx, TimeOut) ->
	receive
	after ?ReTryTime1 ->
		LTimeOut = ?CASE(TimeOut == infinity, TimeOut, TimeOut - ?ReTryTime1),
		case LTimeOut >= 0 of
			true ->
				case eNifLock:tryLock(KeyIx) of
					true ->
						true;
					_ ->
						loopLock2(KeyIx, LTimeOut)
				end;
			_ ->
				lockTimeout
		end
	end.

loopLock2(KeyIx, TimeOut) ->
	receive
	after ?ReTryTime2 ->
		LTimeOut = ?CASE(TimeOut == infinity, TimeOut, TimeOut - ?ReTryTime2),
		case LTimeOut >= 0 of
			true ->
				case eNifLock:tryLock(KeyIx) of
					true ->
						true;
					_ ->
						loopLock3(KeyIx, LTimeOut)
				end;
			_ ->
				lockTimeout
		end
	end.

loopLock3(KeyIx, TimeOut) ->
	receive
	after ?ReTryTime3 ->
		LTimeOut = ?CASE(TimeOut == infinity, TimeOut, TimeOut - ?ReTryTime3),
		case LTimeOut >= 0 of
			true ->
				case eNifLock:tryLock(KeyIx) of
					true ->
						true;
					_ ->
						loopLockL(KeyIx, LTimeOut)
				end;
			_ ->
				lockTimeout
		end
	end.

loopLockL(KeyIx, TimeOut) ->
	receive
	after ?ReTryTimeL ->
		LTimeOut = ?CASE(TimeOut == infinity, TimeOut, TimeOut - ?ReTryTimeL),
		case LTimeOut >= 0 of
			true ->
				case eNifLock:tryLock_ca(KeyIx) of
					true ->
						true;
					_ ->
						loopLockL(KeyIx, LTimeOut)
				end;
			_ ->
				lockTimeout
		end
	end.

doTryLocks(KeyIxs, TimeOut) ->
	%% 先快速自旋重试
	case spinTryLocks(KeyIxs, ?SpinCount) of
		true ->
			true;
		_ ->
			loopLocks1(KeyIxs, TimeOut)
	end.

spinTryLocks(_KeyIxs, 0) ->
	false;
spinTryLocks(KeyIxs, Count) ->
	case eNifLock:tryLocks(KeyIxs) of
		true ->
			true;
		_ ->
			spinTryLocks(KeyIxs, Count - 1)
	end.

loopLocks1(KeyIxs, TimeOut) ->
	receive
	after ?ReTryTime1 ->
		LTimeOut = ?CASE(TimeOut == infinity, TimeOut, TimeOut - ?ReTryTime1),
		case LTimeOut >= 0 of
			true ->
				case eNifLock:tryLocks(KeyIxs) of
					true ->
						true;
					_ ->
						loopLocks2(KeyIxs, LTimeOut)
				end;
			_ ->
				lockTimeout
		end
	end.

loopLocks2(KeyIxs, TimeOut) ->
	receive
	after ?ReTryTime2 ->
		LTimeOut = ?CASE(TimeOut == infinity, TimeOut, TimeOut - ?ReTryTime2),
		case LTimeOut >= 0 of
			true ->
				case eNifLock:tryLocks(KeyIxs) of
					true ->
						true;
					_ ->
						loopLocks3(KeyIxs, LTimeOut)
				end;
			_ ->
				lockTimeout
		end
	end.

loopLocks3(KeyIxs, TimeOut) ->
	receive
	after ?ReTryTime3 ->
		LTimeOut = ?CASE(TimeOut == infinity, TimeOut, TimeOut - ?ReTryTime3),
		case LTimeOut >= 0 of
			true ->
				case eNifLock:tryLocks(KeyIxs) of
					true ->
						true;
					_ ->
						loopLocksL(KeyIxs, LTimeOut)
				end;
			_ ->
				lockTimeout
		end
	end.

loopLocksL(KeyIxs, TimeOut) ->
	receive
	after ?ReTryTimeL ->
		LTimeOut = ?CASE(TimeOut == infinity, TimeOut, TimeOut - ?ReTryTimeL),
		case LTimeOut >= 0 of
			true ->
				case eNifLock:tryLocks_ca(KeyIxs) of
					true ->
						true;
					_ ->
						loopLocksL(KeyIxs, LTimeOut)
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
					catch
						throw:Throw -> Throw;
						C:R:S ->
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
					catch
						throw:Throw -> Throw;
						C:R:S ->
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

-spec lockGet(KeyOrKeys :: term() | [term()]) -> list().
lockGet(KeyOrKeys) ->
	lockGet(KeyOrKeys, ?LockTimeOut).

-spec lockGet(KeyOrKeys :: term() | [term()], TimeOut :: integer() | infinity) -> list().
lockGet({?noneTab, JustKey} = GetKey, TimeOut) ->
	KeyIx = erlang:phash2(JustKey, ?eGLockSize),
	case doTryLock(KeyIx, TimeOut) of
		true ->
			try
				[{GetKey, getEtsTabValue(?noneTab, JustKey, undefined)}]
			catch C:R:S ->
				error({lockGetError, GetKey, {C, R, S}})
			after
				eNifLock:releaseLock(KeyIx),
				ok
			end;
		lockTimeout ->
			error({lockTimeout, GetKey})
	end;
lockGet({EtsTab, Key} = GetKey, TimeOut) ->
	KeyIx = erlang:phash2(GetKey, ?eGLockSize),
	case doTryLock(KeyIx, TimeOut) of
		true ->
			try
				[{GetKey, getEtsTabValue(EtsTab, Key, undefined)}]
			catch C:R:S ->
				error({lockGetError, GetKey, {C, R, S}})
			after
				eNifLock:releaseLock(KeyIx),
				ok
			end;
		lockTimeout ->
			error({lockTimeout, GetKey})
	end;
lockGet({?noneTab, JustKey, DefValue} = Args, TimeOut) ->
	GetKey = {?noneTab, JustKey},
	KeyIx = erlang:phash2(JustKey, ?eGLockSize),
	case doTryLock(KeyIx, TimeOut) of
		true ->
			try
				[{GetKey, getEtsTabValue(?noneTab, JustKey, DefValue)}]
			catch C:R:S ->
				error({lockGetError, Args, {C, R, S}})
			after
				eNifLock:releaseLock(KeyIx),
				ok
			end;
		lockTimeout ->
			error({lockTimeout, Args})
	end;
lockGet({EtsTab, Key, DefValue} = Args, TimeOut) ->
	GetKey = {EtsTab, Key},
	KeyIx = erlang:phash2(GetKey, ?eGLockSize),
	case doTryLock(KeyIx, TimeOut) of
		true ->
			try
				[{GetKey, getEtsTabValue(EtsTab, Key, DefValue)}]
			catch C:R:S ->
				error({lockGetError, Args, {C, R, S}})
			after
				eNifLock:releaseLock(KeyIx),
				ok
			end;
		lockTimeout ->
			error({lockTimeout, Args})
	end;
lockGet(EtsTabKeys, TimeOut) ->
	KeyIxs = getKeyIxAndMaps(EtsTabKeys, []),
	case doTryLocks(KeyIxs, TimeOut) of
		true ->
			try
				[
					begin
						case OneGKey of
							{?noneTab, JustKey} ->
								{OneGKey, getEtsTabValue(?noneTab, JustKey, undefined)};
							{?noneTab, JustKey, DefValue} ->
								{{?noneTab, JustKey}, getEtsTabValue(?noneTab, JustKey, DefValue)};
							{EtsTab, TabKey} ->
								{OneGKey, getEtsTabValue(EtsTab, TabKey, undefined)};
							{EtsTab, TabKey, DefValue} ->
								{{EtsTab, TabKey}, getEtsTabValue(EtsTab, TabKey, DefValue)}
						end
					end || OneGKey <- EtsTabKeys
				]
			catch C:R:S ->
				error({lockGetError, EtsTabKeys, {C, R, S}})
			after
				eNifLock:releaseLocks(KeyIxs),
				ok
			end;
		lockTimeout ->
			error({lockTimeout, EtsTabKeys})
	end.

getKeyIxAndMap(Key) ->
	case Key of
		{?noneTab, JustKey} ->
			erlang:phash2(JustKey, ?eGLockSize);
		{?noneTab, JustKey, _DefValue} ->
			erlang:phash2(JustKey, ?eGLockSize);
		{EtsTab, TabKey} ->
			erlang:phash2(Key, ?eGLockSize);
		{EtsTab, TabKey, _DefValue} ->
			erlang:phash2({EtsTab, TabKey}, ?eGLockSize)
	end.

getKeyIxAndMaps([], IxAcc) -> IxAcc;
getKeyIxAndMaps([Key | Keys], IxAcc) ->
	case Key of
		{?noneTab, JustKey} ->
			KeyIx = erlang:phash2(JustKey, ?eGLockSize);
		{?noneTab, JustKey, _DefValue} ->
			KeyIx = erlang:phash2(JustKey, ?eGLockSize);
		{EtsTab, TabKey} ->
			KeyIx = erlang:phash2(Key, ?eGLockSize);
		{EtsTab, TabKey, _DefValue} ->
			KeyIx = erlang:phash2({EtsTab, TabKey}, ?eGLockSize)
	end,
	getKeyIxAndMaps(Keys, ?CASE(lists:member(KeyIx, IxAcc), IxAcc, [KeyIx | IxAcc])).

transactionApply({M, F, Args}, EtsTabValue) ->
	M:F(Args, EtsTabValue);
transactionApply({Fun, Args}, EtsTabValue) ->
	Fun(Args, EtsTabValue).

-spec transaction(KeyOrKeys :: term() | [term()], MFAOrFun :: {M :: atom(), F :: atom(), Args :: list()} | {Fun :: function(), Args :: list()}) -> term().
transaction(EtsTabKeys, MFAOrFun) ->
	transaction(EtsTabKeys, MFAOrFun, ?LockTimeOut).

%% the fun need return {alterTab, AlterTab} |  {alterTab, Ret, AlterTab} | term()
-spec transaction(KeyOrKeys :: term() | [term()], MFAOrFun :: {M :: atom(), F :: atom(), Args :: list()} | {Fun :: function(), Args :: list()}, TimeOut :: integer() | infinity) -> term().
transaction(EtsTabKeys, MFAOrFun, TimeOut) ->
	case is_list(EtsTabKeys) of
		true ->
			KeyIxs = getKeyIxAndMaps(EtsTabKeys, []),
			case doTryLocks(KeyIxs, TimeOut) of
				true ->
					try
						EtsTabValue =
							[
								begin
									case OneGKey of
										{?noneTab, JustKey} ->
											{OneGKey, getEtsTabValue(?noneTab, JustKey, undefined)};
										{?noneTab, JustKey, DefValue} ->
											{{?noneTab, JustKey}, getEtsTabValue(?noneTab, JustKey, DefValue)};
										{EtsTab, TabKey} ->
											{OneGKey, getEtsTabValue(EtsTab, TabKey, undefined)};
										{EtsTab, TabKey, DefValue} ->
											{{EtsTab, TabKey}, getEtsTabValue(EtsTab, TabKey, DefValue)}
									end
								end || OneGKey <- EtsTabKeys
							],
						case transactionApply(MFAOrFun, EtsTabValue) of
							{alterTab, AlterTab} ->
								[changeEtsTabValue(OneEtsTab, OneKey, ChangeValue) || {{OneEtsTab, OneKey}, ChangeValue} <- AlterTab],
								ok;
							{alterTab, Ret, AlterTab} ->
								[changeEtsTabValue(OneEtsTab, OneKey, ChangeValue) || {{OneEtsTab, OneKey}, ChangeValue} <- AlterTab],
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
			end;
		_ ->
			KeyIx = getKeyIxAndMap(EtsTabKeys),
			case doTryLock(KeyIx, TimeOut) of
				true ->
					try
						TEtsTabValue =
							case EtsTabKeys of
								{?noneTab, JustKey} ->
									{EtsTabKeys, getEtsTabValue(?noneTab, JustKey, undefined)};
								{?noneTab, JustKey, DefValue} ->
									{{?noneTab, JustKey}, getEtsTabValue(?noneTab, JustKey, DefValue)};
								{EtsTab, TabKey} ->
									{EtsTabKeys, getEtsTabValue(EtsTab, TabKey, undefined)};
								{EtsTab, TabKey, DefValue} ->
									{{EtsTab, TabKey}, getEtsTabValue(EtsTab, TabKey, DefValue)}
							end,
						EtsTabValue = [TEtsTabValue],
						case transactionApply(MFAOrFun, EtsTabValue) of
							{alterTab, AlterTab} ->
								[changeEtsTabValue(OneEtsTab, OneKey, ChangeValue) || {{OneEtsTab, OneKey}, ChangeValue} <- AlterTab],
								ok;
							{alterTab, Ret, AlterTab} ->
								[changeEtsTabValue(OneEtsTab, OneKey, ChangeValue) || {{OneEtsTab, OneKey}, ChangeValue} <- AlterTab],
								Ret;
							OtherRet ->
								OtherRet
						end
					catch
						throw:Throw -> Throw;
						C:R:S ->
							error({lockTransactionError, EtsTabKeys, MFAOrFun, {C, R, S}})
					after
						eNifLock:releaseLock(KeyIx),
						ok
					end;
				lockTimeout ->
					error({lockTimeout, EtsTabKeys, MFAOrFun})
			end
	end.

getDefValue(undefined) -> undefined;
getDefValue({DefFun, Args}) when is_function(DefFun) -> erlang:apply(DefFun, Args);
getDefValue(DefValue) -> DefValue.

getEtsTabValue(?noneTab, _Key, DefValue) ->
	getDefValue(DefValue);
getEtsTabValue(Ets, Key, DefValue) ->
	case ets:lookup(Ets, Key) of
		[] ->
			getDefValue(DefValue);
		[OneValue] ->
			OneValue
	end.

changeEtsTabValue(?noneTab, _Key, _Value) ->
	ok;
changeEtsTabValue(Ets, Key, '$delete') ->
	ets:delete(Ets, Key);
changeEtsTabValue(Ets, _Key, Value) ->
	ets:insert(Ets, Value).
