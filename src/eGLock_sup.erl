-module(eGLock_sup).

-behaviour(supervisor).

-include("eGLock.hrl").

-export([start_link/0]).
-export([init/1]).

-define(SERVER, ?MODULE).
start_link() ->
	supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
	SupFlags = #{strategy => one_for_all, intensity => 100, period => 3600},
	ChildSpecs = [
		#{
			id => ?eELockMgr,
			start => {eELockMgr, start_link, []},
			restart => permanent,
			shutdown => 3000,
			type => worker,
			modules => [eELockMgr]
		},
		#{
			id => eALockMgr,
			start => {eALockMgr, start_link, []},
			restart => permanent,
			shutdown => 3000,
			type => worker,
			modules => [eALockMgr]
		}
	],
	{ok, {SupFlags, ChildSpecs}}.
