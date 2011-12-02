-module(logging).
-annotation('function').

-compile(export_all).

%% examples
before_advice(_Annot, M, F, Inputs) ->
    io:format("M: ~p, F: ~p, Inputs: ~p~n", [M, F, Inputs]),
    Inputs.

after_advice(_Annot, M, F, _Inputs, Result) ->
    io:format("M: ~p, F: ~p, Result: ~p~n", [M, F, Result]),
    Result.

