%%%-------------------------------------------------------------------
%%% @author Brian Fischer <brian.fischer.mh@gmail.com>
%%% @copyright (C) 2018, Brian Fischer
%%% @doc
%%%
%%% @end
%%% Created : 30 Mar 2018 by Brian Fischer <brian.fischer.mh@gmail.com>
%%%-------------------------------------------------------------------
-module(gol_cell).

-behaviour(gen_server).

%% API
-export([start_link/1]).
-export([create/2, create_neighbors/2, kill/1, set_alive/2, next_state/1, get_location/1]).

%% For debugging
-export([state/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API
%%%===================================================================

-spec create(gol:x_y(), [gol:x_y()]) -> pid().
create(Location, NeighborLocations) ->
    {ok, Pid} = gol_cell_sup:start_child(Location),
    set_alive(Pid, true),
    create_neighbors(Pid, NeighborLocations),
    Pid.

-spec kill(pid()) -> ok.
kill(Pid) ->
    gen_server:call(Pid, kill).

-spec set_alive(pid(), boolean()) -> ok.
set_alive(Pid, IsAlive) ->
    gen_server:call(Pid, {set_alive, IsAlive}).

-spec next_state(pid()) -> [{kill | create, pid()}].
next_state(Pid) ->
    gen_server:call(Pid, {next_state, 1}).

-spec get_location(pid()) -> gol:x_y().
get_location(Pid) ->
    gen_server:call(Pid, get_location).

-spec state(pid(), integer()) -> map().
state(Pid, Level) ->
    gen_server:call(Pid, {state, Level}).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Location) ->
    gen_server:start_link(?MODULE, [Location], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([Location]) ->
    {ok, #{x_y=>Location, live=>false, live_neighbors=>0, neighbors=>[]}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({create_neighbors, Locations}, _From, State) ->
    CreateNeighbor =
        fun(Location) ->
            {ok, Pid} = gol_cell_sup:start_child(Location),
            increment_live_neighbors_count(Pid),
            Pid
        end,
    Neighbors = [CreateNeighbor(L) || L <- Locations],
    {reply, Neighbors, State#{neighbors=>Neighbors}};

handle_call(kill, _From, State=#{live_neighbors:=NeighborCount, neighbors:=Neighbors}) ->
    IsAlive = false,
    [decrement_live_neighbors_count(Pid) || Pid <- Neighbors],
    maybe_delete(self(), IsAlive, NeighborCount),
    {reply, ok, State#{live=>IsAlive}};

handle_call({set_alive, IsAlive}, _From, State) ->
    {reply, ok, State#{live=>IsAlive}};

handle_call({next_state, Level}, _From, State=#{live:=true, live_neighbors:=Count, neighbors:=Neighbors}) ->
    ThisReply =
        case Count of
            2 -> [];
            3 -> [];
            _ -> {kill, self()}
        end,

    %% Only call neighbors if this cell is a first level live cell.
    NeighborsReply =
        case Level of
            1 -> [next_state(NPid, Level+1) || NPid <- Neighbors];
            _ -> []
        end,

    Reply = lists:flatten([ThisReply|NeighborsReply]),
    {reply, Reply, State};

handle_call({next_state, _Level}, _From, State=#{live:=false, live_neighbors:=Count}) ->
    Reply =
        case Count of
            3 -> [{create, self()}];
            _ -> []
        end,
    {reply, Reply, State};

handle_call(get_location, _From, State=#{x_y:=Location}) ->
    {reply, Location, State};

handle_call({state, Level}, _From, State=#{x_y:=Location, live:=IsAlive, live_neighbors:=Count, neighbors:=Neighbors}) ->
    case Level of
        1 ->
            NStates = [state(NPid, Level+1) || NPid <- Neighbors],
            {reply, #{cell_pid=>self(), cell_state=>#{x_y=>Location, live=>IsAlive, live_neighbors=>Count, neighbors=>NStates}}, State};
        _ ->
            {reply, #{cell_pid=>self(), cell_state=>State}, State}
    end;

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(increment_live_neighbors_count, State=#{live_neighbors:=Count}) ->
    NewCount = Count + 1,
    {noreply, State#{live_neighbors=>NewCount}};

handle_cast(decrement_live_neighbors_count, State=#{live:=IsAlive, live_neighbors:=Count}) ->
    NewCount = Count - 1,
    maybe_delete(self(), IsAlive, NewCount),
    {noreply, State#{live_neighbors=>NewCount}};

handle_cast(delete, State) ->
    {stop, normal, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    gol_store:delete(self()),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec create_neighbors(pid(), [gol:x_y()]) -> [pid()].
create_neighbors(Pid, Locations) ->
    gen_server:call(Pid, {create_neighbors, Locations}).

-spec maybe_delete(pid(), boolean(), integer()) -> ok.
maybe_delete(Pid, IsAlive, NeighborCount) ->
    case {IsAlive, NeighborCount} of
        {false, 0} ->
            gen_server:cast(Pid, delete),
            ok;
        _ -> ok
    end.

-spec increment_live_neighbors_count(pid()) -> ok.
increment_live_neighbors_count(Pid) ->
    gen_server:cast(Pid, increment_live_neighbors_count),
    ok.

-spec decrement_live_neighbors_count(pid()) -> ok.
decrement_live_neighbors_count(Pid) ->
    gen_server:cast(Pid, decrement_live_neighbors_count),
    ok.

-spec next_state(pid(), integer()) -> [{kill | create, pid()}].
next_state(Pid, Level) ->
    gen_server:call(Pid, {next_state, Level}).

