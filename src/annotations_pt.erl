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
-module(annotations_pt).
-export([parse_transform/2]).

parse_transform(Forms0, Options) ->
    {ok, P} = file:get_cwd(),
    io:format("~s: Options: ~p~n", [P, Options]),
    Opt = merge_options(Options),
    {Forms, FunAccs, _} = 
        lists:foldl(fun pick_annotations/2, {[], [], Opt}, Forms0),
    process_fun_accs(FunAccs, Forms).

process_fun_accs(FuncAccs, Forms) ->
    io:format("FunAccs: ~p~n", [FuncAccs]),
    Attributes = [ A || {attribute, _, _, _}=A <- Forms ],
    Other = [ B || B <- Forms, element(1, B) =/= attribute ],
    io:format("Attributes: ~p~n", [Attributes]),
    io:format("Other: ~p~n", [Other]),
    lists:reverse(Attributes) ++ lists:reverse(Other).

pick_annotations({attribute, _, _, _}=Form, 
                    {Forms, [{maybe,_}|_]=FuncAccs, Opt}) ->
    {[Form|Forms], FuncAccs, Opt};
pick_annotations({attribute, _, Name, _}=Form, {Forms, FuncAccs, Opt}) ->
    case lists:member(Name, proplists:get_value(annotations, Opt, [])) of
        true ->
            {Forms, [{maybe, Form}|FuncAccs], Opt};
        false ->
            {[Form|Forms], FuncAccs, Opt}
    end;
pick_annotations({function, _, FName, _, _}=Form,
                 {Forms, [{maybe, Annotation}|FuncAccs], Opt}) ->
    case lists:keyfind(FName, 1, FuncAccs) of
        false ->
            {[Form|Forms], [{FName, [Annotation]}|FuncAccs], Opt};
        {FName, Annotations} ->
            {[Form|Forms],
                lists:keyreplace(FName, 1, FuncAccs,
                    {FName, [Annotation|Annotations]}), Opt}
    end;
pick_annotations(Form, {Acc1, Acc2, Opt}) -> 
    {[Form|Acc1], Acc2, Opt}.

merge_options(Options) ->
    {ok, Dir} = file:get_cwd(),
    case filelib:fold_files(Dir, "annotations.config", true, fun accf/2, []) of
        [] ->
            Options;
        [Config|_] ->
            case file:consult(Config) of
                {ok, Terms} -> Terms;
                _ -> Options
            end
    end.

accf(F, Acc) -> [F | Acc].
