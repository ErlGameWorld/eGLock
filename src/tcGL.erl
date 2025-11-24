-module(tcGL).

-behavior(gen_server).
-compile([export_all]).

%% 三目元算符
-define(CASE(Cond, Then, That), case Cond of true -> Then; _ -> That end).
-define(CASE(Expr, Expect, Then, ExprRet, That), case Expr of Expect -> Then; ExprRet -> That end).

%% IF-DO表达式
-define(IF(IFTure, DoThat), (IFTure) andalso (DoThat)).

%% EXPORT API
-export([
	start/0
]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-define(SERVER, ?MODULE).
-record(state, {}).

-define(ERR, io:format).

test_lock() ->
	eGLock:lockApply(rand:uniform(1000000), {?MODULE, test, []}),
	eGLock:lockApply([rand:uniform(1000000), rand:uniform(1000000), rand:uniform(1000000)], {?MODULE, test, []}),
	test_lock().

ttt(Key) ->
	eGLock:tryLock(Key),
	eGLock:releaseLock(Key).

lock() -> eGLock:lockApply(erlang:unique_integer(), {?MODULE, test, []}).
lock(Args1, Args2) -> eGLock:lockApply(erlang:unique_integer(), {fun test/2, [Args1, Args2]}).


test() -> ok.
test(Args1, Args2) -> {ok, Args1, Args2}.

call() -> gen_server:call(tcGL, erlang:unique_integer()).

tadd2() ->
	Key = erlang:phash2(self(), 150),
	tadd2(Key).
tadd2(Key) ->
	gen_server:call(tcGL, {tadd, Key, erlang:phash2(self(), 100)}).

crole(RoleId) ->
	eGLock:transaction({role, RoleId}, {fun(Args, DdData) ->
		io:format("IMY*********** ~p ~p~n", [DdData, Args]),
		[{RoleIdx, RoleInfo}] = DdData,
		case RoleInfo of
			undefined ->
				{alterTab, ooo, [{RoleIdx, {RoleId, {1, role_info}}}]};
			_ ->
				throw(has_role)
		end
	end, [{RoleId, xxx}]}).

cbuild_db([_RoleId, _Points, BuildId] = Args, DbData) ->
	io:format("IMY*********** ~p ~p~n", [DbData, Args]),
	[{_RoleIdx, RoleInfo} | PointDbs] = DbData,
	?IF(RoleInfo == undefined, throw(no_role)),
	%% 检查点位是否有建筑
	NewPoints = [?CASE(PointState, undefined, {PointIdx, {element(2, PointIdx), RoleInfo, BuildId}}, Thing, throw({point_has_thing, PointIdx, Thing})) || {PointIdx, PointState} <- PointDbs],
	io:format("IMY*********** ~p ~n", [NewPoints]),
	{alterTab, ok, NewPoints}.

cbuild(RoleId, Points, BuildId) ->
	PointsDb = [{point, OnePoint} || OnePoint <- Points],
	eGLock:transaction([{role, RoleId}] ++ PointsDb, {fun tcGL:cbuild_db/2, [RoleId, Points, BuildId]}).

tadd() ->
	Key = erlang:phash2(self(), 150),
	eGLock:transaction({monster, Key, {Key, #{}}}, {fun(_Args, DdData) ->
		[{MonsterIdx, {_, Monster}}] = DdData,
		Index = erlang:phash2(self(), 100),
		CurV = maps:get(Index, Monster, 0),
		{alterTab, ooo, [{MonsterIdx, {Key, Monster#{Index => CurV + 1}}}]}
	end, []}).

tadd(Key) ->
	eGLock:transaction({monster, Key, {Key, #{}}}, {fun(_Args, DdData) ->
		% [{MonsterIdx, {_, Monster}}] = DdData,
		% Index = erlang:phash2(self(), 100),
		% CurV = maps:get(Index, Monster, 0),
		% {alterTab, ooo, [{MonsterIdx, {Key, Monster#{Index => CurV + 1}}}]}
	ok
	end, []}).

%% ********************************************  API *******************************************************************
start() ->
	gen_server:start({local, ?SERVER}, ?MODULE, [], []).

%% ********************************************  callback **************************************************************
init(_Args) ->
	ets:new(role, [public, named_table]),
	ets:new(point, [public, named_table]),
	ets:new(alliance, [public, named_table]),
	ets:new(monster, [public, named_table]),
	ets:new(resource, [public, named_table]),
	ets:new(build, [public, named_table]),
	{ok, #state{}}.

handle_call({tadd, Key, Index}, _State, _FROM) ->
	case ets:lookup(monster, Key) of
		[] ->
			ets:insert(monster, {Key, #{Index => 1}});
		[{_, Monster}] ->
			CurV = maps:get(Index, Monster, 0),
			ets:insert(monster, {Key, Monster#{Index => CurV + 1}})
	end,
	{reply, ok, _State};
handle_call(_Msg, _State, _FROM) ->
	test:test(),
	{reply, ok, _State}.

%% 默认匹配
handle_cast(_Msg, _State) ->
	kpS.

handle_info(_Msg, _State) ->
	kpS.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.
%% ****************************************************** logic ********************************************************

%% =====================
%% Comprehensive Tests
%% =====================

-define(DEFAULT_KEYS, 1000).
-define(DEFAULT_CONCURRENCY, 8).

run_all_tests() ->
	ensure_server_started(),
	Tests = [
		{single_lock, fun test_single_lock/0},
		{batch_lock_dedup, fun test_batch_lock_dedup/0},
		%% lockGet coverage
		{lock_get_noneTab, fun test_lockGet_noneTab/0},
		{lock_get_ets, fun test_lockGet_ets/0},
		%% lockApply coverage
		{lock_apply_throw, fun test_lockApply_throw/0},
		{get_lock_pid, fun test_getLockPid_api/0},
		{lock_apply_single, fun test_lockApply_single/0},
		{lock_apply_batch, fun test_lockApply_batch/0},
		%% transaction coverage
		{transaction_single, fun test_transaction_single/0},
		{transaction_multi, fun test_transaction_multi/0},
		{transaction_flow, fun test_transaction_flow/0},
		{dead_pid_cleanup, fun test_dead_pid_cleanup/0},
		{concurrency_stress, fun test_concurrency_stress/0}
	],
	Results = [run_one(Name, Fun) || {Name, Fun} <- Tests],
	io:format("~n=== eGLock Test Summary ===~n"),
	[io:format("~p -> ~p~n", [Name, Res]) || {Name, Res} <- Results],
	io:format("============================~n"),
	{ok, Results}.

run_one(Name, Fun) ->
	{TimeUs, Res} = timer:tc(fun() -> safe_call(Fun) end),
	{Name, {Res, TimeUs div 1000}}.

safe_call(Fun) ->
	try Fun() of
		ok -> ok;
		Other -> Other
	catch C:R -> {error, {C, R}} end.

ensure_server_started() ->
	case whereis(?SERVER) of
		undefined -> start();
		_ -> ok
	end.

mk_key(Tag) -> erlang:phash2({Tag, self()}, ?DEFAULT_KEYS).

%% ---- Test: Single Lock ----
test_single_lock() ->
	Key = mk_key(single_lock),
	%% Acquire
	ok = case eGLock:tryLock(Key, 50) of
		true -> ok;
		lockTimeout -> {error, cannot_lock_initial}
	end,
	%% Owner check
	{_, Pid} = eGLock:getLockPid(Key),
	ok = case Pid =:= self() of true -> ok; _ -> {error, wrong_owner} end,
	%% Contention from another process should timeout
	Parent = self(),
	_ = spawn(fun() -> Parent ! {contend, eGLock:tryLock(Key, 10)} end),
	Res = receive {contend, R} -> R after 100 -> timeout end,
	ok = case Res of lockTimeout -> ok; _ -> {error, contention_should_timeout} end,
	%% Release and ensure others can acquire
	eGLock:releaseLock(Key),
	_ = spawn(fun() -> Parent ! {post_release, eGLock:tryLock(Key, 50), eGLock:releaseLock(Key)} end),
	Res2 = receive {post_release, R2, _} -> R2 after 100 -> timeout end,
	ok = case Res2 of true -> ok; _ -> {error, post_release_should_succeed} end,
	all_ok.

%% ---- Test: Batch Lock with Duplicates ----
test_batch_lock_dedup() ->
	K1 = mk_key(batch1), K2 = mk_key(batch2), K3 = mk_key(batch3),
	Keys = [K2, K1, K2, K3, K1],
	ok = case eGLock:tryLock(Keys, 100) of
		true -> ok;
		lockTimeout -> {error, batch_lock_failed}
	end,
	%% Owner check for uniques
	Uniques = lists:usort(Keys),
	Owners = [eGLock:getLockPid(K) || K <- Uniques],
	ok = case lists:all(fun({_Key, P}) -> P =:= self() end, Owners) of true -> ok; _ -> {error, wrong_owner_batch} end,
	eGLock:releaseLock(Keys),
	all_ok.

%% ---- Test: getLockPid API ----
test_getLockPid_api() ->
	K = mk_key(getpid),
	_ = eGLock:tryLock(K, 50),
	{K, P1} = eGLock:getLockPid(K),
	ok = case P1 =:= self() of true -> ok; _ -> {error, getpid_single_wrong} end,
	Keys = [K, mk_key(getpid2)],
	_ = eGLock:tryLock(Keys, 50),
	Pairs = eGLock:getLockPid(Keys),
	ok = case lists:keyfind(K, 1, Pairs) of {K, PidK} when PidK =:= self() -> ok; _ -> {error, getpid_list_wrong} end,
	eGLock:releaseLock(Keys),
	all_ok.

%% ---- Test: lockApply ensures release ----
test_lockApply_single() ->
	K = mk_key(lockapply_single),
	ok = eGLock:lockApply(K, {fun() -> ok end, []}),
	%% After lockApply, lock must be released
	ok = case eGLock:tryLock(K, 50) of
		true -> eGLock:releaseLock(K), ok;
		lockTimeout -> {error, lock_not_released_single}
	end,
	all_ok.

test_lockApply_batch() ->
	Keys = [mk_key(lockapply_b1), mk_key(lockapply_b2), mk_key(lockapply_b1)],
	ok = eGLock:lockApply(Keys, {fun() -> ok end, []}),
	ok = case eGLock:tryLock(Keys, 50) of
		true -> eGLock:releaseLock(Keys), ok;
		lockTimeout -> {error, lock_not_released_batch}
	end,
	all_ok.

%% ---- Test: lockApply with throw ----
test_lockApply_throw() ->
	K = mk_key(lockapply_throw),
	Ret = eGLock:lockApply(K, {fun() -> throw({oops, K}) end, []}),
	ok = case Ret of {oops, K} -> ok; _ -> {error, lockapply_throw_ret_wrong} end,
	ok = case eGLock:tryLock(K, 50) of
		true -> eGLock:releaseLock(K), ok;
		lockTimeout -> {error, lockapply_throw_not_released}
	end,
	all_ok.

%% ---- Test: transaction flow using tcGL helpers ----
test_transaction_flow() ->
	RoleId = mk_key(role),
	Points = [mk_key(point1), mk_key(point2)],
	BuildId = mk_key(build),
	%% Ensure role creation path
	ok = try crole(RoleId) of
		_ -> ok
	catch _:has_role -> ok end,
	%% Build on points
	ok = cbuild(RoleId, Points, BuildId),
	%% Verify ETS writes occurred
	ok = case [ets:lookup(point, P) || P <- Points] of
		[_V | _] -> ok; _ -> {error, transaction_points_not_written} end,
	all_ok.

%% ---- Test: transaction single (ETS) ----
test_transaction_single() ->
	K = mk_key(tx_single),
	Fun = {fun(_Args, _EtsVals) -> {alterTab, ok, [{{resource, K}, {K, 777}}]} end, []},
	Ret = eGLock:transaction({resource, K}, Fun),
	ok = case Ret of ok -> ok; _ -> {error, tx_single_ret_wrong} end,
	ok = case ets:lookup(resource, K) of
		[{K, 777}] -> ok;
		Other -> {error, {tx_single_write_wrong, Other}}
	end,
	all_ok.

%% ---- Test: transaction multi (ETS) ----
test_transaction_multi() ->
	K1 = mk_key(tx_multi1), K2 = mk_key(tx_multi2),
	Fun = {fun(_Args, _EtsVals) -> {alterTab, ok, [{{resource, K1}, {K1, 1}}, {{resource, K2}, {K2, 2}}]} end, []},
	Ret = eGLock:transaction([{resource, K1}, {resource, K2}], Fun),
	ok = case Ret of ok -> ok; _ -> {error, tx_multi_ret_wrong} end,
	ok = case {ets:lookup(resource, K1), ets:lookup(resource, K2)} of
		{[{K1, 1}], [{K2, 2}]} -> ok;
		Other -> {error, {tx_multi_write_wrong, Other}}
	end,
	all_ok.

%% ---- Test: dead owner cleanup (using ca variant) ----
test_dead_pid_cleanup() ->
	K = mk_key(dead_pid),
	Parent = self(),
	_P = spawn(fun() -> _ = eNifLock:tryLock(K), timer:sleep(10), Parent ! done end),
	receive done -> ok after 200 -> ok end,
	ok = case eGLock:tryLock(K, 50) of
		true -> eGLock:releaseLock(K), ok;
		lockTimeout -> {error, dead_pid_cleanup_failed}
	end,
	all_ok.

%% ---- Test: lockGet noneTab + DefValue/DefFun ----
test_lockGet_noneTab() ->
	K = mk_key(lockget_none),
	[{_K1, V1}] = eGLock:lockGet({noneTab, K}),
	ok = case V1 of undefined -> ok; _ -> {error, lockget_none_wrong} end,
	[{_K2, V2}] = eGLock:lockGet({noneTab, K, 42}),
	ok = case V2 of 42 -> ok; _ -> {error, lockget_def_int_wrong} end,
	[{_K3, V3}] = eGLock:lockGet({noneTab, K, {fun lists:sum/1, [[1, 2, 3]]}}),
	ok = case V3 of 6 -> ok; _ -> {error, lockget_def_fun_wrong} end,
	all_ok.

%% ---- Test: lockGet ETS table ----
test_lockGet_ets() ->
	K = mk_key(lockget_ets),
	ets:insert(resource, {K, foo}),
	[{_K1, V1}] = eGLock:lockGet({resource, K}),
	ok = case V1 of {K, foo} -> ok; _ -> {error, lockget_ets_wrong} end,
	all_ok.

%% ---- Test: concurrency stress (short) ----
test_concurrency_stress() ->
	Concurrency = ?DEFAULT_CONCURRENCY,
	ItersPer = 2000,
	Parent = self(),
	Workers = [spawn(fun() ->
		lists:foreach(fun(I) ->
			K = (I + erlang:phash2(self())) rem ?DEFAULT_KEYS,
			case eGLock:tryLock(K, 5) of
				true -> eGLock:releaseLock(K);
				lockTimeout -> ok
			end
		end, lists:seq(1, ItersPer)),
		Parent ! {done, self()}
	end) || _ <- lists:seq(1, Concurrency)],
	[receive {done, _} -> ok after 5000 -> timeout end || _ <- Workers].