%% -*- tab-width: 4;erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
%% ----------------------------------------------------------------------------
%%
%% Copyright (c) 2008-2011 Tim Watson (watson.timothy@gmail.com)
%%
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), deal
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
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
%% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
%% IN THE SOFTWARE.
%% ----------------------------------------------------------------------------
-module(annotations_inst_pt).
-export([parse_transform/2]).

-include("types.hrl").

parse_transform(Forms, Options) ->
    parse_trans:top(fun do_transform/2, Forms, Options).

do_transform(Forms, Context) ->
    File = get_file_loc(Forms),
    io:format(user, "File = ~p~n", [File]),
    {ok, Bin} = file:read_file(File),
    Comments = erl_comment_scan:string(binary_to_list(Bin)),
    io:format(user, "comments = ~p~n", [Comments]),
    Forms2 = erl_recomment:recomment_forms(Forms, Comments),
    {Forms3, _Acc2} =
        parse_trans:do_transform(fun xform_fun/4, [],
                                 [Forms2], Context),
    parse_trans:revert(Forms3).

% xform_fun(application, Form, Ctx, Conf) ->
xform_fun(_Thing, Form, _Ctx, Conf) ->
    io:format("Form: ~p~n", [Form]),
    {[], Form, [], true, Conf}.

get_file_loc(Forms) ->
    Mod = element(4, lists:keyfind(module, 3, Forms)),
    proplists:get_value(source,
                        proplists:get_value(compile, Mod:module_info())).

