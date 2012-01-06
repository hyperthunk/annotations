%% -----------------------------------------------------------------------------
%% Copyright (c) 2002-2012 Tim Watson (watson.timothy@gmail.com)
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
%% -----------------------------------------------------------------------------
-module(conditional).
-annotation('function').
-include_lib("annotations/include/types.hrl").
-compile(export_all).

%% examples
around_advice(#annotation{data={PropName, 'gt', Val}}, M, F, Inputs) ->
    io:format("Checking for ~p in ~p~n", [PropName, Inputs]),
    case lists:keyfind(PropName, 1, erlang:hd(Inputs)) of
        false ->
            false;
        {PropName, GivenValue} ->
            case GivenValue > Val of
                false ->
                    false;
                true ->
                    annotation:call_advised(M, F, Inputs)
            end
    end.
