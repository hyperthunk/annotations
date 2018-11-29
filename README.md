# Annotations - MetaData and Code Instrumentation for Erlang/OTP

This library provides additional features for annotating code with metadata, as 
well as support for compile time code instrumentation via a parse transform. See [here](https://github.com/nebularis/memoize/blob/master/src/memoize.erl) for a concrete usage example.

## Examples

Annotating specific functions:

```erlang
-module(db).
-export([find/2, add/2]).
-include("db.hrl").

find(DbHandle, What) ->
    qlc:q([I || I <- table_source(DbHandle), I.key == What]).

-transactional(requires_new).
add(DbHandle, Item) ->
    db_backend:insert(DbHandle, Item).
```

Note that the `-transactional` attribute is not usually allowed at this scope, but
the `annotations` parse transform moves the metadata around and tags the
annotation so that it maps the right function.

## Adding behaviour to Annotations

An annotation can be backed by a user-defined module, in which case the module may
implement certain callback functions that can be used to provide instrumentation
at runtime. For example, consider this logging annotation:

```erlang
-module(logging).
-annotation('function').    %% the scope of this annotation
-compile(export_all).

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
```

Naturally we'd normally want to use a proper logging framework and pass on the
log level instead, but we've kept our example simple to use/understand here.

We can now use the logging annotated in our actual code:

```erlang
-module(annotated).
-export([foo/1]).
-include_lib("annotations/include/annotations.hrl").

-logging(info).
-spec(foo/1 :: (term()) -> {foo, string(), term()}).
foo(T) ->
    {foo, T}.
```

## Building with rebar based project

We'll need to export the `logging` annotation config in order for the parse
transform to work, which we can do using the supplied rebar plugin:

```erlang
%% rebar.config

{deps, [
    {parse_trans, ".*",
        {git, "https://github.com/esl/parse_trans.git"}},
    {annotations, ".*",
        {git, "https://github.com/hyperthunk/annotations.git"}}
]}.

{plugins, [rebar_annotations_plugin]}.
{annotations, [{registered, [logging]}]}.

{erl_first_files, ["logging.erl"]}.
```

## Building with erlang.mk based project

An erlang.mk plugin is provided for easy integration with your project.

```make
DEPS = annotations
DEP_PLUGINS = annotations

ANNOTATIONS = logging
```

## Run

And now for an example session:

    t4@malachi:simple $ rebar clean compile
    # output snipped...
    t4@malachi:simple $ erl -pa ebin/
    Erlang R14B01 (erts-5.8.2) [source] [64-bit] [smp:2:2] [rq:2] [async-threads:0] [hipe] [kernel-poll:false]

    Eshell V5.8.2  (abort with ^G)
    1> annotated:foo("Hello world").
    {foo,"Hello world"}
    2> put(loglevel, info).
    undefined
    3> annotated:foo("Hello world").
    before_advice - M: logging, F: foo, Inputs: ["Hello world"]
    after_advice - M: logging, F: foo, Result: {foo,"Hello world"}
    {foo,"Hello world"}
    4>
    BREAK: (a)bort (c)ontinue (p)roc info (i)nfo (l)oaded
           (v)ersion (k)ill (D)b-tables (d)istribution
    a
    t4@malachi:simple $ 

For an example of using *around advice* take a look in the `examples` directory.

## Generating code at build time

Annotations can also opt to export additional code at runtime. Currently there
are two supported methods for doing this:

1. Generate a function that calls back into your (annotation) module
2. Generate a function *by hand*

To generate code, you should export a function `codegen/3` which takes three
inputs: the annotation record, the module in which the target function resides
and the current function AST (as handled by erl_syntax). This function must
return a list of *targets*, which can contain one of the following two kinds
of specification:

In order to utilise (1), your `codegen/3` function should return tuples which
contains `{AdviceFunctionName, TargetFunctionName, TargetFuncArity, Data}`.
The `AdviceFunctionName` is the name of a function which will be called by
the generated function, the target function name and arity indicate what you
wish to generate and the `Data` is a literal/AST containing the input(s) you
want sent to your `AdviceFunctionName` at runtime. The result of this will be
a function which calls back into your module directly.

In order to utilise (2), your `codegen/3` should return tuples which contain
`{Name, fun()}` where `Name` is the name you wish to export and the 
corresponding fun is the function you wish to generate. The idea of this API 
is that you can use the `codegen` module from `parse_trans` to generate your
function conveniently, and have the annotation processing engine deal with the
rest.

For an example of code generation in practise, take a look at the 
[delegate](https://github.com/hyperthunk/delegate) library.

```erlang
%% delegate.erl
-module(delegate).
-include_lib("annotations/include/types.hrl").
-export([codegen/3, delegate_advice/4]).

codegen(A=#annotation{data=Data}, _Mod, AST) ->
    io:format("Annotation Data: ~p~n",[lists:keyfind(arity, 1, Data)]),
    case lists:keyfind(delegate, 1, Data) of
        {delegate, Delegates} when is_list(Delegates) ->
            %% NB: you need a *little* understanding of erl_syntax here
            {_FN, FA} = erl_syntax_lib:analyze_function(AST),
            Arity = case lists:keyfind(arity, 1, Data) of
                {arity, N} when is_integer(N) -> N;
                _ -> FA
            end,
            [ build_spec(D, Arity, A) || D <- Delegates ];
        Other ->
            %% TODO: clearer error handling API
            io:format("Other: ~p~n", [Other]),
            {error, "no delegates defined"}
    end.

delegate_advice(A, M, F, Inputs) ->
    Argv = make_args(A, M, F, Inputs),
    erlang:apply(M, F, Argv).

%% etc....
```

And the corresponding usage pattern:

```erlang
-module(simple_log).
-export([log/3]).
-compile({no_auto_import, [error/2]}).
-include_lib("annotations/include/annotations.hrl").

-delegate([{delegate, ["info", "warn", "error"]},
           {args, ['$T', '$I']},
           {arity, 2}]).
log(Level, Message, Args) ->
    case erlang:get({?MODULE, loglevel}) of
        Level ->
            io:format(Message, Args);
        _ ->
            ok
    end.
```

Which when the annotation processing has finished, looks like this:

    % escript deps/parse_trans/ebin/parse_trans_pp.beam ebin/simple_log.beam

```erlang
%% snip....
-annotation({annotation, delegate,
	     {function, {simple_log, log, 3}},
	     [{delegate, ["info", "warn", "error"]},
	      {args, ['$T', '$I']}, {arity, 2}]}).

-export([info/2]).

-export([warn/2]).

-export([error/2]).

info(V73, V45) ->
    delegate:delegate_advice({annotation, delegate,
			      {function, {simple_log, log, 3}},
			      [{target, info},
			       {delegate, ["info", "warn", "error"]},
			       {args, ['$T', '$I']}, {arity, 2}]},
			     simple_log, log, [V73, V45]).

warn(V51, V95) ->
    delegate:delegate_advice({annotation, delegate,
			      {function, {simple_log, log, 3}},
			      [{target, warn},
			       {delegate, ["info", "warn", "error"]},
			       {args, ['$T', '$I']}, {arity, 2}]},
			     simple_log, log, [V51, V95]).

error(V60, V32) ->
    delegate:delegate_advice({annotation, delegate,
			      {function, {simple_log, log, 3}},
			      [{target, error},
			       {delegate, ["info", "warn", "error"]},
			       {args, ['$T', '$I']}, {arity, 2}]},
			     simple_log, log, [V60, V32]).

log(Level, Message, Args) ->
    case erlang:get({simple_log, loglevel}) of
      Level -> io:format(Message, Args);
      _ -> ok
    end.
%% snip....
```

## License

This work is distributed under a permissive BSD-style license.

## Versioning

This project will adhere to the principles of
[semantic versioning](http://semver.org) once a first public API is declared.

## Roadmap

See the `TODO.md` file for upcoming plans/changes.
