-module(eNifLock).

-export([
	tryLock/1
	, tryLocks/1
	, tryLock_ca/1
	, tryLocks_ca/1
	, releaseLock/1
	, releaseLocks/1
	, getLockPid/1
]).

-on_load(init/0).

init() ->
	SoName =
		case code:priv_dir(?MODULE) of
			{error, _} ->
				case code:which(?MODULE) of
					Filename when is_list(Filename) ->
						filename:join([filename:dirname(Filename), "../priv", "eNifLock"]);
					_ ->
						filename:join("../priv", "eNifLock")
				end;
			Dir ->
				filename:join(Dir, "eNifLock")
		end,
	erlang:load_nif(SoName, 0).

-spec tryLock(KeyIx :: non_neg_integer()) -> true | false.
tryLock(_KeyIx) ->
	erlang:nif_error({not_loaded, [{module, ?MODULE}, {line, ?LINE}]}).

-spec tryLocks(KeyIxs :: [non_neg_integer()]) -> true | false.
tryLocks(_KeyIxs) ->
	erlang:nif_error({not_loaded, [{module, ?MODULE}, {line, ?LINE}]}).

-spec tryLock_ca(KeyIx :: non_neg_integer()) -> true | false.
tryLock_ca(_KeyIx) ->
	erlang:nif_error({not_loaded, [{module, ?MODULE}, {line, ?LINE}]}).

-spec tryLocks_ca(KeyIxs :: [non_neg_integer()]) -> true | false.
tryLocks_ca(_KeyIxs) ->
	erlang:nif_error({not_loaded, [{module, ?MODULE}, {line, ?LINE}]}).

-spec releaseLock(KeyIx :: non_neg_integer()) -> true | false.
releaseLock(_KeyIx) ->
	erlang:nif_error({not_loaded, [{module, ?MODULE}, {line, ?LINE}]}).

-spec releaseLocks(KeyIxs :: [non_neg_integer()]) -> true | false.
releaseLocks(_KeyIxs) ->
	erlang:nif_error({not_loaded, [{module, ?MODULE}, {line, ?LINE}]}).

-spec getLockPid(KeyIx :: non_neg_integer()) -> pid() | undefined.
getLockPid(_KeyIx) ->
	erlang:nif_error({not_loaded, [{module, ?MODULE}, {line, ?LINE}]}).
