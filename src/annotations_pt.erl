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

-include("types.hrl").

parse_transform(Forms0, Options) ->
    {ok, P} = file:get_cwd(),
    progress_message("~s: Options: ~p~n", [P, Options]),
    Opt = merge_options(Options),
    {Forms, FunAccs, _} =
        lists:foldl(fun pick_annotations/2, {[], [], Opt}, Forms0),
    process_fun_accs(FunAccs, Forms, Opt).

process_fun_accs(FuncAccs, Forms, Opt) ->
    %% TODO: move the whole sweep to using parse_trans
    Mod = current_scope(Forms),
    ScopedAnnotations =
        [ annotations:from_ast(Form, {function, {Mod,Fn,A}}) ||
                              {{Fn, A}, Mapped} <- FuncAccs, Form <- Mapped ],
    Attributes = [ process_attribute(A, Mod, Opt) ||
                                            {attribute, _, _, _}=A <- Forms ],
    Other = [ B || B <- Forms, element(1, B) =/= attribute ],
    NewForms = lists:reverse(Attributes) ++
               [ {attribute, 3, annotation, A} || A <- ScopedAnnotations ] ++
               lists:reverse(Other),
    maybe_apply_changes(NewForms, ScopedAnnotations).

maybe_apply_changes(Forms0, ScopedAnnotations) ->
    progress_message("Applying ~p scoped annotations~n",
                     [length(ScopedAnnotations)]),
    parse_trans:top(fun(Forms, _Context) ->
                        {Forms2, {_, Acc2}} =
                            parse_trans:transform(fun xform_fun/4,
                                    {ScopedAnnotations, []}, Forms, []),
                        progress_message("Acc2 = ~p~n", [Acc2]),
                        Forms3 = lists:foldl(fun export_orig/2, Forms2, Acc2),
                        parse_trans:revert(Forms3)
                    end, Forms0, []).

export_orig({FN, FA}, Acc) ->
    parse_trans:export_function(FN, FA, Acc).

xform_fun(function, Form, Ctx, {Annotations, Acc}) ->
    Module = parse_trans:context(module, Ctx),
    {Name, Arity} = erl_syntax_lib:analyze_function(Form),
    progress_message("Processing ~p:~p/~p ...~n", [Module, Name, Arity]),
    Applicable = [ A || A <- Annotations,
                        check_scope({function, {Module, Name, Arity}}, A) ],
    progress_message("Found ~p applicable annotations for ~p~n",
                     [length(Applicable), Module]),
    {NewForms, Form2, Acc2} = maybe_transform(Module, Form,
                                              {Applicable, Acc, []}),
    {NewForms, Form2, [], true, {Annotations, Acc2}};
xform_fun(_Thing, Form, _Ctx, Acc) ->
    {[], Form, [], true, Acc}.

%% TODO: don't move the tuple elements around unnecessarily
maybe_transform(_Module, Form, {[], Acc, NewForms}) ->
    {NewForms, Form, Acc};
maybe_transform(Module, Form, {[Annotation|Rest], Acc, NewForms}) ->
    case annotation:has_advice(Annotation) of
        false ->
            case annotation:has_codegen(Annotation) of
                true ->
                    {ExtraForms, Exp} = gen_forms(Module, Form, Annotation),
                    progress_message("Extra Forms: ~p~n", [ExtraForms]),
                    progress_message("More Exports: ~p~n", [Exp]),
                    maybe_transform(Module, Form,
                                {Rest, Exp ++ Acc, ExtraForms ++ NewForms});
                false ->
                    progress_message("Skipping unadvised annotation ~p~n",
                                     [Annotation]),
                    maybe_transform(Module, Form, {Rest, Acc, NewForms})
            end;
        true ->
            {Form2, ExtraForms, Exp} = rewrite_form(Module, Form, Annotation),
            maybe_transform(Module, Form2,
                            {Rest, [Exp|Acc], ExtraForms ++ NewForms})
    end.

gen_forms(Mod, Form, #annotation{name=AnnotationMod}=A) ->
    lists:unzip([ gen_function(Mod, Form, Def, A) || Def <-
                                AnnotationMod:codegen(A, Mod, Form) ]).

%%
%% @private
%% We will eventually support three kinds of specification:
%% 1. {AdviceFunctionName, TargetFunctionName, Data}
%%    NB: this one takes its arity from the annotated target
%% 2. {AdviceFunctionName, TargetFunctionName, Arity, Data}
%% 3. {funName, fun()} (generation completely handled
%% by the annotation callback module)
%%
gen_function(Mod, Form,
            {Advice, Target, Arity, Data},
                #annotation{name=AnnotMod}) when is_atom(Target) ->
    progress_message("gen_function ~p:~p/~p -> ~p~n",
                     [Mod, Target, Arity, Advice]),
    Pos = erl_syntax:get_pos(Form),
    {FName, _FArity} = erl_syntax_lib:analyze_function(Form),
    NewName = {atom, Pos, Target},
    VarNames = erl_syntax_lib:new_variable_names(Arity, sets:new()),
    Vars = erl_syntax:list([ {var, Pos, V} || V <- VarNames ]),
    ModAST = {atom, Pos, AnnotMod},
    FunctionReturnValue =
    erl_syntax:application(ModAST, {atom, Pos, Advice},
                           [erl_syntax:abstract(Data),
                            {atom, Pos, Mod}, {atom, Pos, FName}, Vars]),
    Patterns = [ {var, Pos, V} || V <- VarNames ],
    MainClause = erl_syntax:clause(Patterns, none,
                                   [FunctionReturnValue]),
    NewImpl = erl_syntax:function(NewName, [MainClause]),
    {NewImpl, {Target, Arity}}.

rewrite_form(Module, Form, Annotation) ->
    case annotation:has_advice(around_advice, Annotation) of
        false ->
            do_rewrite_form(Module, Form, Annotation);
        true ->
            do_rewrite_around_form(Module, Form, Annotation)
    end.

%% TODO: de-duplicate these two functions

do_rewrite_form(Module, Form, #annotation{name=AnnotationMod}=A) ->
    Pos = erl_syntax:get_pos(Form),
    {FName, FArity} = erl_syntax_lib:analyze_function(Form),
    Clauses = erl_syntax:function_clauses(Form),
    OrigFN = annotations:advised_name(FName),
    NewName = {atom, Pos, OrigFN},
    OrigImpl = erl_syntax:function(NewName, Clauses),

    VarNames = erl_syntax_lib:new_variable_names(FArity, sets:new()),
    Vars = erl_syntax:list([ {var, Pos, V} || V <- VarNames ]),
    ModAST = {atom, Pos, AnnotationMod},
    BeforeAdviceVarAST = {var,Pos,'AfterAdvised'},
    PassThrough = case annotation:has_advice(before_advice, A) of
        false ->
            erl_syntax:match_expr(BeforeAdviceVarAST, Vars);
        true ->
            ApplyBeforeAdviceAST =
                erl_syntax:application(ModAST, {atom, Pos, before_advice},
                                       [erl_syntax:abstract(A),
                                        ModAST, {atom, Pos, FName}, Vars]),
            erl_syntax:match_expr(BeforeAdviceVarAST, ApplyBeforeAdviceAST)
    end,
    %% TODO: should we consider doing a try/catch here!?
    MatchCallExprAST = {var, Pos, 'ActualResult'},
    ActualCallAST =
    erl_syntax:match_expr(MatchCallExprAST,
        erl_syntax:application({atom, Pos, erlang}, {atom, Pos, apply},
                              [{atom, Pos, Module}, NewName,
                              {var, Pos, 'AfterAdvised'}])),

    FinalResult =
    case annotation:has_advice(after_advice, A) of
        false ->
            {var, Pos, 'ActualResult'};
        true ->
            erl_syntax:application(ModAST, {atom, Pos, after_advice},
                                   [erl_syntax:abstract(A),
                                    ModAST, {atom, Pos, FName}, Vars,
                                    {var, Pos, 'ActualResult'}])
    end,
    Patterns = [ {var, Pos, V} || V <- VarNames ],
    MainClause = erl_syntax:clause(Patterns, none,
                                   [PassThrough, ActualCallAST, FinalResult]),
    NewImpl = erl_syntax:function({atom, Pos, FName}, [MainClause]),
    {OrigImpl, [NewImpl], {OrigFN, FArity}}.

do_rewrite_around_form(Module, Form, #annotation{name=AnnotationMod}=A) ->
    Pos = erl_syntax:get_pos(Form),
    {FName, FArity} = erl_syntax_lib:analyze_function(Form),
    Clauses = erl_syntax:function_clauses(Form),
    OrigFN = annotations:advised_name(FName),
    NewName = {atom, Pos, OrigFN},
    OrigImpl = erl_syntax:function(NewName, Clauses),

    VarNames = erl_syntax_lib:new_variable_names(FArity, sets:new()),
    Vars = erl_syntax:list([ {var, Pos, V} || V <- VarNames ]),
    ModAST = {atom, Pos, AnnotationMod},
    FinalResult =
    erl_syntax:application(ModAST, {atom, Pos, around_advice},
                           [erl_syntax:abstract(A),
                            {atom, Pos, Module}, {atom, Pos, FName}, Vars]),
    io:format("Application: ~p~n", [FinalResult]),
    Patterns = [ {var, Pos, V} || V <- VarNames ],
    MainClause = erl_syntax:clause(Patterns, none,
                                   [FinalResult]),
    NewImpl = erl_syntax:function({atom, Pos, FName}, [MainClause]),
    {OrigImpl, [NewImpl], {OrigFN, FArity}}.

check_scope(Scope, Annotation) ->
    Allowed = annotations:get_scope(Annotation),
    case annotations:check_scope(Scope, Allowed) of
        ok ->
            true;
        Other ->
            progress_message("Non-matching scope: ~p~n", [Other]),
            false
    end.

process_attribute({attribute, L, N, _}=A, Mod, Opt) ->
    case is_annotation(N, Opt) of
        false -> A;
        _True ->
            {attribute, L, annotation,
                annotations:from_ast(A, {'module', Mod})}
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
    %% NB: we assume that if rebar is running, we're going to use the
    %% rebar_log module, regardless of *what* the actual log level is
    case application:get_env(rebar_global, verbose) of
        undefined -> check_env();
        {ok, _}   -> rebar_verbose
    end.

check_env() ->
    case os:getenv("ANNOTATIONS_TRANSFORM_VERBOSE") of
        false ->
            case get('annotations.transform.verbose') of
                undefined -> false;
                _ -> true
            end;
        _Other ->
            true
    end.
