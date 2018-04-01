%%%-------------------------------------------------------------------
%%% @author Brian Fischer <brian.fischer.mh@gmail.com>
%%% @copyright (C) 2018, Brian Fischer
%%% @doc
%%%
%%% @end
%%% Created : 30 Mar 2018 by Brian Fischer <brian.fischer.mh@gmail.com>
%%%-------------------------------------------------------------------
-module(gol_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

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

    SupFlags = #{strategy => rest_for_one,
                 intensity => 1,
                 period => 5},

    Api = #{id => gol,
            start => {gol, start_link, []},
            restart => permanent,
            shutdown => 2000,
            type => worker,
            modules => [gol]},

    CellSup = #{id => gol_cell_sup,
                start => {gol_cell_sup, start_link, []},
                restart => permanent,
                shutdown => 1000,
                type => supervisor,
                modules => [gol_cell_sup]},

    {ok, {SupFlags, [Api, CellSup]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
