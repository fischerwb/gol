%%%-------------------------------------------------------------------
%%% @author Brian Fischer <brian.fischer.mh@gmail.com>
%%% @copyright (C) 2018, Brian Fischer
%%% @doc
%%%
%%% @end
%%% Created : 30 Mar 2018 by Brian Fischer <brian.fischer.mh@gmail.com>
%%%-------------------------------------------------------------------
-module(gol).

-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([next/0, start/0, start/1, stop/0]).
-export([set_universe_size/2, add_shape/2, clear/0]).
-export([save/1, load/1]).

%% For debugging
-export([state/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-type standard_shape() :: glider
                          | small_exploder
                          | exploder
                          | spaceship
                          | tumbler
                          | ten_cell_row.

-type x_y() :: {integer(), integer()}.
-type custom_shape() :: [x_y()].
-type location() :: center | x_y().

-export_type([x_y/0]).


-define(DEFAULT_WIDTH, 100).
-define(DEFAULT_HEIGHT, 100).
-define(DEFAULT_FREQUENCY, 1).


-define(GLIDER, [        {1, 0},
                                 {2, 1},
                 {0, 2}, {1, 2}, {2,2}]).

-define(SMALL_EXPLODER, [        {1, 0},
                         {0, 1}, {1, 1}, {2,1},
                         {0, 2},         {2, 2},
                                 {1,3}]).

-define(EXPLODER, [{0, 0},        {2, 0},        {4, 0},
                   {0, 1},                       {4, 1},
                   {0, 2},                       {4, 2},
                   {0, 3},                       {4, 3},
                   {0, 4},        {2, 4},        {4, 4}]).

-define(TEN_CELL_ROW, [{0, 0}, {1, 0}, {2, 0}, {3,0}, {4, 0}, {5, 0}, {6, 0}, {7, 0}, {8, 0}, {9, 0}]).

-define(SPACESHIP, [        {1, 0}, {2, 0}, {3, 0}, {4, 0},
                    {0, 1},                         {4, 1},
                                                    {4, 2},
                    {0, 3},                         {4, 3}]).

-define(TUMBLER, [        {1, 0}, {2, 0},        {4, 0}, {5, 0},
                          {1, 1}, {2, 1},        {4, 1}, {5, 1},
                                  {2, 2},        {4, 2},
                  {0, 3},         {2, 3},        {4, 3},         {6, 3},
                  {0, 4},         {2, 4},        {4, 4},         {6, 4},
                  {0, 5}, {1, 5},                        {5, 5}, {6, 5}]).


%%%===================================================================
%%% API
%%%===================================================================

-spec next() -> ok.
next() ->
    gen_server:cast(?SERVER, next).

-spec start() -> ok.
start() ->
    start(?DEFAULT_FREQUENCY).

-spec start(integer()) -> ok.
start(Frequency) ->
    gen_server:call(?SERVER, {start, Frequency * 1000}).

stop() ->
    gen_server:call(?SERVER, stop).

-spec set_universe_size(integer(), integer()) -> ok.
set_universe_size(Width, Height) when is_integer(Width), is_integer(Height) ->
    gen_server:call(?SERVER, {set_universe_size, {Width, Height}}).


-spec add_shape(location(), standard_shape()) -> ok;
               (location(), custom_shape()) -> ok.
add_shape(Location, glider) ->
    add_shape(Location, ?GLIDER);
add_shape(Location, small_exploder) ->
    add_shape(Location, ?SMALL_EXPLODER);
add_shape(Location, exploder) ->
    add_shape(Location, ?EXPLODER);
add_shape(Location, ten_cell_row) ->
    add_shape(Location, ?TEN_CELL_ROW);
add_shape(Location, spaceship) ->
    add_shape(Location, ?SPACESHIP);
add_shape(Location, tumbler) ->
    add_shape(Location, ?TUMBLER);
add_shape(center=Location, Shape) ->
    gen_server:cast(?SERVER, {add_shape, {Location, Shape}});
add_shape({X, Y}=Location, Shape) when is_integer(X), is_integer(Y) ->
    gen_server:cast(?SERVER, {add_shape, {Location, Shape}}).

-spec clear() -> ok.
clear() ->
    stop(),
    gen_server:call(?SERVER, clear).

-spec save(file:name_all()) -> ok | {error, Reason}
    when Reason :: file:posix() | badarg | terminated | system_limit.
save(Filename) ->
    gen_server:call(?SERVER, {save, Filename}).

-spec load(file:name_all()) -> ok.
load(Filename) ->
    clear(),
    gen_server:call(?SERVER, {load, Filename}).

-spec state() -> map().
state() ->
    gen_server:call(?SERVER, state).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

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
init([]) ->
    {ok, #{width=>?DEFAULT_WIDTH, height=>?DEFAULT_HEIGHT, live_cells=>[], generation=>0, start_time=>0, frequency=>infinity}}.

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
handle_call({start, Frequency}, _From, State) ->
    {reply, ok, State#{start_time=>get_milliseconds_now(), frequency=>Frequency}, 0};

handle_call(stop, _From, State=#{generation:=Generation}) ->
    error_logger:info_msg("Stopped after ~p generations.~n", [Generation]),
    {reply, ok, State#{start_time=>0, frequency=>infinity}, infinity};

handle_call({set_universe_size, {Width, Height}}, _From, State) ->
    {reply, ok, State#{width=>Width, height=>Height}};

handle_call(clear, _From, State=#{live_cells:=LiveCells}) ->
    [gol_cell:kill(Pid) || Pid <- LiveCells],
    error_logger:info_msg("Cleared the universe!~n"),
    {reply, ok, State#{live_cells=>[], generation=>0}, infinity};

handle_call({save, Filename}, _From, State=#{width:=Width, height:=Height, live_cells:=LiveCells, start_time:=StartTime, frequency:=Frequency}) ->
    Locations = [gol_cell:get_location(Pid) || Pid <- LiveCells],
    SaveData = #{width=>Width, height=>Height, live_cells=>Locations},
    BinData = term_to_binary(SaveData),
    Ret = file:write_file(Filename, BinData),
    {reply, Ret, State, time_left(StartTime, Frequency)};

handle_call({load, Filename}, _From, State) ->
    {ok, BinData} = file:read_file(Filename),
    Data = binary_to_term(BinData),

    #{width:=Width, height:=Height, live_cells:=Locations} = Data,
    add_shape({0,0}, Locations),

    {reply, ok, State#{width=>Width, height=>Height}, infinity};

handle_call(state, _From, State=#{live_cells:=LiveCells, start_time:=StartTime, frequency:=Frequency}) ->
    CellStates = [gol_cell:state(Pid, 1) || Pid <- LiveCells],
    {reply, #{application_state=>State, cell_states=>CellStates}, State, time_left(StartTime, Frequency)};

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
handle_cast(next, State=#{live_cells:=OldCells, width:=Width, height:=Height, generation:=Generation, frequency:=Frequency}) ->
    ThisGeneration = Generation + 1,

    %% determine cells to create & kill
    NextStates = lists:flatten([gol_cell:next_state(Pid) || Pid <- OldCells]),

    FoldFun =
        fun({kill, Pid}, {Kill, Create}) -> {[Pid|Kill], Create};
            ({create, Pid}, {Kill, Create}) -> {Kill, [Pid|Create]};
            (_Elem, AccIn) -> AccIn
        end,

    {Kill, Create} = lists:foldl(FoldFun, {[], []}, NextStates),
    ToKill = lists:usort(Kill),
    ToCreate = lists:usort(Create),

    %% create new cell processes
    Locations = [gol_cell:get_location(Pid) || Pid <- ToCreate],
    Cells = create_live_cells(Locations, Width, Height, OldCells),

    %% kill dead cell processes
    [gol_cell:kill(Pid) || Pid <- ToKill],

    LiveCells = Cells -- ToKill,
    graph_cells(LiveCells, ThisGeneration),

    StartTime = get_milliseconds_now(),
    {noreply, State#{live_cells=>Cells -- ToKill, generation=>ThisGeneration, start_time=>StartTime}, Frequency};

handle_cast({add_shape, {center, Shape}}, State=#{width:=Width, height:=Height, live_cells:=OldCells, generation:=Generation, start_time:=StartTime, frequency:=Frequency}) ->
    ThisGeneration = Generation + 1,

    CenterX = trunc(Width/2),
    CenterY = trunc(Height/2),

    {Xs, Ys} = lists:unzip(Shape),
    ShapeWidth = lists:max(Xs) - lists:min(Xs),
    AdjX = trunc(ShapeWidth/2),
    ShapeHeight = lists:max(Ys) - lists:min(Ys),
    AdjY = trunc(ShapeHeight/2),
    ActualShape = [{CenterX + X - AdjX, CenterY + Y - AdjY} || {X, Y} <- Shape],

    Cells = create_live_cells(ActualShape, Width, Height, OldCells),

    graph_cells(Cells, ThisGeneration),

    {noreply, State#{live_cells=>Cells, generation=>ThisGeneration}, time_left(StartTime, Frequency)};

handle_cast({add_shape, {{LocX, LocY}, Shape}}, State=#{width:=Width, height:=Height, live_cells:=OldCells, generation:=Generation, start_time:=StartTime, frequency:=Frequency}) ->
    ThisGeneration = Generation + 1,

    ActualShape = [{LocX + X, LocY + Y} || {X, Y} <- Shape],
    Cells = create_live_cells(ActualShape, Width, Height, OldCells),

    graph_cells(Cells, ThisGeneration),

    {noreply, State#{live_cells=>Cells, generation=>ThisGeneration}, time_left(StartTime, Frequency)};

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
handle_info(timeout, State) ->
    next(),
    {noreply, State};

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

-spec create_live_cell(x_y(), integer(), integer()) -> pid().
create_live_cell(Location, Width, Height) ->
    NeighborLocations = determine_neighbors(Location, Width, Height),
    gol_cell:create(Location, NeighborLocations).

-spec create_live_cells([x_y()], integer(), integer(), [pid()]) -> [pid()].
create_live_cells([], _Width, _Height, AccCells) ->
    lists:usort(AccCells);

create_live_cells([Location | Tail], Width, Height, AccCells) ->
    create_live_cells(Tail, Width, Height, [create_live_cell(Location, Width, Height) | AccCells]).

-spec determine_neighbors(x_y(), integer(), integer()) -> [x_y()].
determine_neighbors({X, Y}, Width, Height) ->
    NXs = lists:seq(X-1, X+1),
    NYs = lists:seq(Y -1, Y +1),
    [{NX, NY} || NX <- NXs, NY <- NYs, not(NX == X andalso NY == Y), NX >= 0, NX =< Width, NY >= 0, NY =< Height].

-spec sort_locations(x_y(), x_y()) -> boolean().
sort_locations({AX, AY}, {BX, BY}) ->
    case AY =:= BY of
        true -> AX =< BX;
        _ -> AY < BY
    end.

-spec graph_cells([pid()], integer()) -> ok.
graph_cells(Cells, Generation) ->
    Locations = [gol_cell:get_location(Pid) || Pid <- Cells],
    SortedLocations = lists:sort(fun sort_locations/2, Locations),

    {Xs, Ys} = lists:unzip(SortedLocations),
    X = lists:min(Xs),
    XWidth = lists:max(Xs) - X,
    Y = lists:min(Ys),

    Fun =
        fun({CX, CY}, Acc) ->
            AdjX = CX - X,
            AdjY = CY - Y,
            case lists:keyfind(AdjY, 1, Acc) of
                false ->
                    lists:keystore(AdjY, 1, Acc, {AdjY, [AdjX]});
                {_, XList} ->
                    lists:keystore(AdjY, 1, Acc, {AdjY, [AdjX|XList]})
            end
        end,
    Rows = lists:foldl(Fun, [], SortedLocations),

    PrintFun =
        fun({_CY, XList}, AllRows) ->
            InnerFun =
                fun(CX, Str) ->
                    case lists:member(CX, XList) of
                        false -> [Str, " "];
                        true -> [Str, "X"]
                    end
                end,
            [unicode:characters_to_list(lists:foldl(InnerFun, [], lists:seq(0, XWidth))) | AllRows]
        end,
    GraphedRows = lists:foldr(PrintFun, [], Rows),
    FormatString = unicode:characters_to_list(lists:foldl(fun(_X, Str) -> [Str, "~p~n"] end, "Generation ~p:~nLocation: ~p~n~n", lists:seq(1, length(GraphedRows)))),
    error_logger:info_msg(FormatString, [Generation, {X, Y} | GraphedRows]),
    ok.

-spec time_left(integer(), infinity | integer()) -> infinity | integer().
time_left(_StartTime, infinity) -> infinity;
time_left(StartTime, Frequency) ->
    CurrentTime = get_milliseconds_now(),
    TimeElapsed = CurrentTime - StartTime,
    case Frequency - TimeElapsed of
        Time when Time =< 0 -> 0;
        Time -> Time
    end.

-spec get_milliseconds_now() -> integer().
get_milliseconds_now() ->
    erlang:system_time(millisecond).

