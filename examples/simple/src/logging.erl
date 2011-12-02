-module(logging).
-annotation('function').
-include_lib("annotations/include/types.hrl").
-compile(export_all).

%% examples
before_advice(#annotation{data=Lvl}, M, F, Inputs) ->
    log(Lvl, "before_advice - M: ~p, F: ~p, Inputs: ~p~n", [M, F, Inputs]),
    Inputs.

after_advice(#annotation{data=Lvl}, M, F, _Inputs, Result) ->
    log(Lvl, "after_advice - M: ~p, F: ~p, Result: ~p~n", [M, F, Result]),
    Result.

log(Lvl, Message, Args) ->
    case get(loglevel) of
        Lvl ->
            io:format(Message, Args);
        _ ->
            ok
    end.
