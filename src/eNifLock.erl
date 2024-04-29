-module(eNifLock).

-export([
	tryLock/1
	, releaseLock/1
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

-spec tryLock(_KeyOrKeys :: term() | [term()]) -> true | false.
tryLock(_KeyOrKeys) ->
	erlang:nif_error({not_loaded, [{module, ?MODULE}, {line, ?LINE}]}).

-spec releaseLock(_KeyOrKeys :: term() | [term()]) -> true | false.
releaseLock(_KeyOrKeys) ->
	erlang:nif_error({not_loaded, [{module, ?MODULE}, {line, ?LINE}]}).

-spec getLockPid(OneKey :: term()) -> pid() | undefined.
getLockPid(_OneKey) ->
	erlang:nif_error({not_loaded, [{module, ?MODULE}, {line1, ?LINE}]}).
