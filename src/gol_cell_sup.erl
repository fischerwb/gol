%%%-------------------------------------------------------------------
%%% @author Brian Fischer <brian.fischer.mh@gmail.com>
%%% @copyright (C) 2018, Brian Fischer
%%% @doc
%%%
%%% @end
%%% Created : 30 Mar 2018 by Brian Fischer <brian.fischer.mh@gmail.com>
%%%-------------------------------------------------------------------
-module(gol_cell_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_child/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).


-spec start_child(gol:x_y()) -> {ok, pid()}.
start_child(Location) ->
    case gol_store:get(Location) of
        {ok, Pid} -> {ok, Pid};
        {error, not_found} ->
            {ok, Pid} = supervisor:start_child(?SERVER,  [Location]),
            gol_store:put(Location, Pid),
            {ok, Pid}
    end.


%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart intensity, and child
%% specifications.
%%
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    gol_store:init(),

    SupFlags = #{strategy => simple_one_for_one,
                 intensity => 1,
                 period => 5},

    ChildSpec = #{id => gol_cell,
                  start => {gol_cell, start_link, []},
                  restart => transient,
                  shutdown => 1000,
                  type => worker,
                  modules => [gol_cell]},

    {ok, {SupFlags, [ChildSpec]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
