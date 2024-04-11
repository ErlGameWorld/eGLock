-module(eGLock_sup).

-behaviour(supervisor).

-include("eGLock.hrl").

-export([start_link/0]).
-export([init/1]).

-define(SERVER, ?MODULE).
start_link() ->
	supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
	SupFlags = #{strategy => one_for_all, intensity => 0, period => 1},
	ets:new(?EtsGLockKey, [named_table, set, public, {write_concurrency, auto}, {read_concurrency, true}]),
	{ok, {SupFlags, []}}.
