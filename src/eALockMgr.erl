-module(eALockMgr).

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
	ATLockRef = atomics:new(?eALockSize, [{signed, false}]),
	persistent_term:put(?eALockRef, ATLockRef),
	persistent_term:put(?eALockMgr, self()),
	ets:new(?EtsGLockPid, [named_table, set, public, {write_concurrency, auto}]),
	{ok, #state{}}.

handle_call(_Request, _From, State) ->
	{reply, ok, State}.

handle_cast(_Request, State) ->
	{noreply, State}.

handle_info({'EXIT', Pid, _Reason}, State) ->
	case ets:take(?EtsGLockPid, Pid) of
		[] -> ignore;
		[{_Pid, KeyIxOrKeyIxs}] ->
			ALockRef = persistent_term:get(?eALockRef),
			PidInt = termInt:termInt(Pid),
			case is_integer(KeyIxOrKeyIxs) of
				true ->
					atomics:compare_exchange(ALockRef, KeyIxOrKeyIxs, PidInt, 0);
				_ ->
					[atomics:compare_exchange(ALockRef, KeyIx, PidInt, 0) || KeyIx <- KeyIxOrKeyIxs],
					ok
			end
	end,
	{noreply, State};
handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.
