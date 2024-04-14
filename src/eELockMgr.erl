-module(eELockMgr).

-behaviour(gen_server).

-include("eGLock.hrl").

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
	code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {}).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link() ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
	process_flag(trap_exit, true),
	persistent_term:put(?eELockMgr, self()),
	ets:new(?EtsGLockKey, [named_table, set, public, {write_concurrency, auto}, {read_concurrency, true}]),
	{ok, #state{}}.

handle_call(_Request, _From, State) ->
	{reply, ok, State}.

handle_cast(_Request, State) ->
	{noreply, State}.

handle_info({'EXIT', Pid, _Reason}, State) ->
	ets:match_delete(?EtsGLockKey, {'_', Pid}),
	{noreply, State};
handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.
