-module(annotated).
-export([demo/0, bar/0, foo/1]).
-include_lib("annotations/include/annotations.hrl").

%% available at runtime even without debug_info

demo() ->
    io:format("All annotations for ~p:~n", [?MODULE]),
    [ io:format("   ~p~n", [A]) || A <- annotations:list(?MODULE) ],
    io:format("All annotations of type ~p in ~p:~n", [transactional, ?MODULE]),
    [ io:format("   ~p~n", [A]) || A <- annotations:find(transactional, ?MODULE) ],
    io:format("Accessing function annotations:~n"
              "    by Mod + FunctionName: ~p~n", 
              [annotations:find_by_function(?MODULE, foo)]),
    io:format("    by Type + Mod + FunctionName: ~p~n",
              [annotations:find(logging, ?MODULE, foo)]),
    io:format("    for unrecognised type(s): ~p~n",
            [annotations:find(barking, ?MODULE, bar)]).

-transactional(requires_new).
-logging(info).
-spec(foo/1 :: (term()) -> {foo, string(), term()}).
foo(T) ->
    {foo, T}.

-logging(debug).
bar() ->
    bar.
