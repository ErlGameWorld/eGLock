-module(termInt).

-export([termInt/1]).

-on_load(init/0).

init() ->
	SoName =
		case code:priv_dir(?MODULE) of
			{error, _} ->
				case code:which(?MODULE) of
					Filename when is_list(Filename) ->
						filename:join([filename:dirname(Filename), "../priv", "termInt"]);
					_ ->
						filename:join("../priv", "termInt")
				end;
			Dir ->
				filename:join(Dir, "termInt")
		end,
	erlang:load_nif(SoName, 0).

termInt(_Term) ->
	erlang:error({"NIF not implemented in nif_test at line", ?LINE}).