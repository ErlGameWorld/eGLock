-module(eGPidInt).

-export([pidToInt/1, intToPid/1]).

-on_load(init/0).

init() ->
	SoName =
		case code:priv_dir(?MODULE) of
			{error, _} ->
				case code:which(?MODULE) of
					Filename when is_list(Filename) ->
						filename:join([filename:dirname(Filename), "../priv", "eGPidInt"]);
					_ ->
						filename:join("../priv", "eGPidInt")
				end;
			Dir ->
				filename:join(Dir, "eGPidInt")
		end,
	erlang:load_nif(SoName, 0).

pidToInt(_Term) ->
	erlang:nif_error({not_loaded, [{module, ?MODULE}, {line, ?LINE}]}).

intToPid(_Term) ->
	erlang:nif_error({not_loaded, [{module, ?MODULE}, {line, ?LINE}]}).