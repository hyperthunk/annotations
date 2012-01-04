-module(annotated).
-export([demo/0, foo/1]).
-include_lib("annotations/include/annotations.hrl").

%% available at runtime even without debug_info

demo() ->
    L = [{age, 18}, {foo, "Joe"}],
    io:format("Is Joe 18 or over? ... ~p~n", [foo(L)]),
    L2 = [{age, 13}, {foo, "Mike"}],
    io:format("Is Mike 18 or over? ... ~p~n", [foo(L2)]).

-conditional({age, 'gt', 17}).
-spec(foo/1 :: (term()) -> {foo, string(), term()}).
foo(Inputs) ->
    lists:keyfind(foo, 1, Inputs).
