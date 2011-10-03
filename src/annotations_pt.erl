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
    process_fun_accs(FunAccs, Forms, Opt).

process_fun_accs(FuncAccs, Forms, Opt) ->
    
    %% TODO: should these annotations be able to alter
    %%       the things they are applied to!?
    
    io:format("FunAccs: ~p~n", [FuncAccs]),
    Mod = current_scope(Forms),
    ScopedAnnotations =
        [ annotations:from_ast(Form, {function, {Mod,Fn,A}}) ||
                              {{Fn, A}, Mapped} <- FuncAccs, Form <- Mapped ],
    io:format("ScopedAnnotations: ~p~n", [ScopedAnnotations]),
    Attributes = [ process_attribute(A, Mod, Opt) ||
                                            {attribute, _, _, _}=A <- Forms ],
    Other = [ B || B <- Forms, element(1, B) =/= attribute ],
    io:format("Attributes: ~p~n", [Attributes]),
    io:format("Other: ~p~n", [Other]),
    NewForms = lists:reverse(Attributes) ++
               [ {attribute, 3, annotation, A} || A <- ScopedAnnotations ] ++
               lists:reverse(Other),
    io:format("~p~n", [NewForms]),
    NewForms.

process_attribute({attribute, L, N, _}=A, Mod, Opt) ->
    case is_annotation(N, Opt) of
        false -> A;
        _True ->
            {attribute, L, annotation, annotations:from_ast(A, {'module', Mod})}
    end.

pick_annotations({attribute, _, _, _}=Form,
                    {Forms, [{maybe,_}|_]=FuncAccs, Opt}) ->
    {[Form|Forms], FuncAccs, Opt};
pick_annotations({attribute, _, Name, _}=Form, {Forms, FuncAccs, Opt}) ->
    case is_annotation(Name, Opt) of
        false ->
            {[Form|Forms], FuncAccs, Opt};
        _True ->
            {Forms, [{maybe, Form}| FuncAccs], Opt}
    end;
pick_annotations({function, _, FName, Arity, _}=Form,
                 {Forms, [{maybe, Annotation}|FuncAccs], Opt}) ->
    case lists:keyfind(FName, 1, FuncAccs) of
        false ->
            {[Form|Forms], [{{FName, Arity}, [Annotation]}|FuncAccs], Opt};
        {FName, Annotations} ->
            {[Form|Forms],
                lists:keyreplace({FName, Arity}, 1, FuncAccs,
                                {FName, [Annotation|Annotations]}), Opt}
    end;
pick_annotations(Form, {Acc1, Acc2, Opt}) ->
    {[Form|Acc1], Acc2, Opt}.

current_scope(Forms) ->
    {attribute,_,module,Name} = lists:keyfind(module, 3, Forms),
    Name.

is_annotation(Name, Opt) ->
    Reg = proplists:get_value(registered, Opt, []),
    case lists:member(Name, Reg) of
        true ->
            true;
        false ->
            annotations:is_annotation(Name)
    end.

merge_options(Options) ->
    {ok, Dir} = file:get_cwd(),
    progress_message("Look for annotations.config in ~s~n", [Dir]),
    case filelib:fold_files(Dir, "annotations.config", true, fun accf/2, []) of
        [] ->
            progress_message("No annotations.config found - "
                             "proceeding with defalut settings.~n", []),
            Options;
        [Config|_] ->
            progress_message("Found ~s!~n", [Config]),
            case file:consult(Config) of
                {ok, [Terms]} -> Terms;
                _ -> Options
            end
    end.

accf(F, Acc) -> [F | Acc].

progress_message(Msg, Args) ->
    case is_verbose() of
        true ->
            io:format(Msg, Args);
        rebar_verbose ->
            rebar_log:log(debug, Msg, Args);
        _ -> ok
    end.

is_verbose() ->
    case application:get_env(rebar_global, verbose) of
        false     -> check_env();
        {ok, "0"} -> check_env();
        {ok, "1"} -> rebar_verbose
    end.

check_env() ->
    case os:getenv("ANNOTATIONS_TRANSFORM_VERBOSE") of
        false ->
            case get('annotations.transform.verbose') of
                undefined -> false;
                – -> true
            end;
        _Other ->
            true
    end.
