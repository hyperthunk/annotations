%% -----------------------------------------------------------------------------
%%
%% Copyright (c) 2008-2011 Tim Watson (watson.timothy@gmail.com)
%%
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%% THE SOFTWARE.
%%
%% -----------------------------------------------------------------------------
-module(annotation).
-export([behaviour_info/1]).
-export([has_advice/1, has_advice/2]).
-export([process_annotation/2, get_scope/0]).
-annotation(['application', 'package', 'module']).

-include("types.hrl").

behaviour_info(callbacks) ->
    [{process_annotation, 2}, {get_scope, 0}];
behaviour_info(_) ->
    undefined.

process_annotation(A, _Data) -> A.
get_scope() -> 'function'.

%% this is a special case annotation that doesn't actually get parse-transformed.

has_advice(#annotation{name=Module}) ->
    lists:member(true, check_advice(Module)).

has_advice(after_advice, #annotation{name=Module}) ->
    erlang:function_exported(Module, after_advice, 5);
has_advice(Advice, #annotation{name=Module}) ->
    erlang:function_exported(Module, Advice, 4).

check_advice(Module) ->
    [ erlang:function_exported(Module, F, 4) || F <- [before_advice,
                                                      around_advice] ] ++
    [ erlang:function_exported(Module, after_advice, 5) ].

%% examples
% before_advice(Annot, M, F, Inputs) ->
%    io:format("M: ~p, F: ~p, Inputs: ~p~n", [M, F, Inputs]),
%    [ I * 2 || I <- Inputs ].

% after_advice(Annot, M, F, Inputs, Result) ->
%    io:format("M: ~p, F: ~p, Inputs: ~p~n", [M, F, Inputs]),
%    Result * 2.

% around_advice(Annot, M, F, Inputs) ->
%    apply(M, F, [ I * 2 || I <- Inputs ]) * 2.
