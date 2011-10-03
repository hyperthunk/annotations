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
-module(annotations).

-export([is_annotation/1, process_annotation/1]).

-export([list/1, find_by_function/2, find/2, find/3]).

%% parse_transform and runtime introspection utilities
-export([parse_transform/2, from_ast/2]).

-type(target() :: atom() | {atom(), atom(), integer()}).
-type(scope() :: 'application' | 'package' | 'module' | 'function' | '_').
-type(annotation_scope() :: scope() | [scope()] |
                            {scope(), target()} |
                            {[scope()] | target() } |
                            'undefined').
-export_type([target/0, scope/0, annotation_scope/0]).

-include("types.hrl").

-type(annotation() :: #annotation{}).
-export_type([annotation/0]).

-define(DEFAULT_SCOPE, 'module').

list(Target) when is_atom(Target) ->
    [ A || {annotation,[A]} <- Target:module_info(attributes) ];
list({Scope, Target}) when is_atom(Scope) andalso is_atom(Target) ->
    [ hd(erlang:element(2, A)) || {annotation,
                    [#annotation{scope={AnnotationScope,_}}]}=A <-
                    Target:module_info(attributes), AnnotationScope =:= Scope ];
list({'function', {M,F,A}}) when is_atom(M) andalso
                                 is_atom(F) ->
    [ hd(erlang:element(2, Ann)) || {annotation,
                        [#annotation{scope={_,{MA, FA, AA}}}]}=Ann <-
                                   M:module_info(attributes),
                                   check_scope(M,MA) =:= ok andalso
                                   check_scope(F,FA) =:= ok andalso
                                   check_scope(A,AA) =:= ok ].

find_by_function(Mod, Func) when is_atom(Mod) andalso is_atom(Func) ->
    list({'function', {Mod, Func, '_'}}).

find(Thing, Mod) when is_atom(Thing) andalso is_atom(Mod) ->
    filter(Thing, list(Mod));
find(Type, Target) when is_atom(Type) ->
    [ A || {annotation, [#annotation{name=N}]}=A <- list(Target), N =:= Type ].

find(Thing, Mod, Func) when is_atom(Thing) andalso
                            is_atom(Mod) andalso
                            is_atom(Func) ->
    filter(Thing, find_by_function(Mod, Func)).

-spec(process_annotation/1 :: (#annotation{}) -> #annotation{} | term()).
process_annotation(#annotation{ name=Name, data=Data }=A) ->
    try Name:process_annotation(A, Data)
    catch error:undef -> A
    end.

-spec(get_scope/1 :: (#annotation{} | atom() | tuple()) -> scope()).
get_scope(#annotation{ scope=Scope }) ->
    Scope;
get_scope(Annotation) ->
    try
        Attrs = Annotation:module_info(attributes),
        case lists:keyfind(annotation, 3, Attrs) of
            {_,_,_,Scope} ->
                Scope;
            _ ->
                case erlang:function_exported(Annotation, get_scope, 0) of
                    true -> Annotation:get_scope();
                    false -> '_'
                end
        end
    catch _:_ ->
        '_'
    end.

-spec(is_annotation/1 :: (#annotation{} | atom() | tuple()) -> boolean()).
is_annotation(#annotation{name=Name}) -> {true, is_annotation(Name)};
is_annotation(MaybeAnnotation) ->
    case catch(MaybeAnnotation:module_info(attributes)) of
        [_|_]=Attrs ->
            case lists:keyfind(annotation, 3, Attrs) of
                [_H|_] -> true;
                false ->
                    case lists:keyfind(behaviour, 1, Attrs) of
                        {behaviour, Behaviours} ->
                            lists:member(annotation, Behaviours);
                        false ->
                            false
                    end
            end;
        _ -> false
    end.

-spec(parse_transform/2 :: (list(term()), list({atom(),term()})) -> term()).
parse_transform(AST, Options) ->
    annotations_pt:parse_transform(AST, Options).

-spec(from_ast/2 :: ({attribute,integer(),atom(),term()},
                      scope()) -> #annotation{} | term()).
from_ast({attribute, _Ln, annotation, Scope}, _) ->
    #annotation{ name=annotation, scope=Scope };
from_ast({attribute, _Ln, AnnotationName, AnnotationData}, ActualScope) ->
    AllowedScope = get_scope(AnnotationName),
    Check = check_scope(ActualScope, AllowedScope),
    case Check of
        {invalid_scope, _} ->
            throw(Check);
        ok ->
            ok
    end,
    Annotation = #annotation{ name=AnnotationName,
                              scope=ActualScope,
                              data=AnnotationData },
    process_annotation(Annotation).

%%
%% Internal API
%%

filter(Type, Things) ->
    [ A || #annotation{name=N}=A <- Things, N == Type ].

check_scope(RequiredScope, AllowedScope) when is_list(RequiredScope),
                                              is_list(AllowedScope) ->
    %% [SuppScope, SuppMod, SuppFunc, SuppArity] = unpack_decl(AllowedScope),
    %% [ReqScope, ReqMod, ReqFunc, ReqArity] = unpack_decl(RequiredScope),
    [ check_scope(A,B) || {A,B} <- lists:zip(unpack_decl(AllowedScope),
                                             unpack_decl(RequiredScope)) ];
check_scope('_', _) -> ok;
check_scope(_, '_') -> ok;
check_scope(Allowed, Required) when is_integer(Allowed) andalso
                                    is_integer(Required) -> Allowed =:= Required;
check_scope(Allowed, Required) when is_atom(Allowed) andalso
                                    is_atom(Required) ->
    case re:run(atom_to_list(Required), atom_to_list(Allowed)) of
        {match, _} -> ok;
        _ -> {invalid_scope, {Allowed, Required}}
    end.

unpack_decl({Scope, Target}) when is_atom(Scope) ->
    unpack_decl([Scope], Target);
unpack_decl({Scope, Target}) when is_list(Scope) andalso is_atom(Target) ->
    unpack_decl(Scope, [Target, '_', '_']).

unpack_decl(Scope, [M,F,A]=T) when is_list(Scope) andalso
                                   is_atom(M)     andalso
                                   is_atom(F)     andalso
                                   is_integer(A) -> Scope ++ T.
