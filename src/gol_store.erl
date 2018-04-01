%%%-------------------------------------------------------------------
%%% @author Brian Fischer <brian.fischer.mh@gmail.com>
%%% @copyright (C) 2018, Brian Fischer
%%% @doc
%%%
%%% @end
%%% Created : 30 Mar 2018 by Brian Fischer <brian.fischer.mh@gmail.com>
%%%-------------------------------------------------------------------
-module(gol_store).

%% API
-export([init/0, get/1, put/2, delete/1]).


-define(TABLE_ID, ?MODULE).

%%%===================================================================
%%% API
%%%===================================================================

-spec init() -> ok.
init() ->
    ets:new(?TABLE_ID, [public, named_table]),
    ok.

-spec get(gol:x_y()) -> {ok, pid()} | {error, not_found}.
get(Key) ->
    case ets:lookup(?TABLE_ID, Key) of
        [{Key, Pid}] -> {ok, Pid};
        [] -> {error, not_found}
    end.

-spec put(gol:x_y(), pid()) -> true.
put(Key, Pid) ->
    ets:insert(?TABLE_ID, {Key, Pid}).

-spec delete(pid()) -> true.
delete(Pid) ->
    ets:match_delete(?TABLE_ID, {'_', Pid}).

%%%===================================================================
%%% Internal functions
%%%===================================================================
