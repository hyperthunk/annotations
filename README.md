# Annotations - MetaData and Code Instrumentation for Erlang/OTP

This library provides additional features for annotating code with metadata, as 
well as support for compile time code instrumentation via a parse transform.

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

## License

This work is distributed under a permissive BSD-style license.

## Versioning

This project will adhere to the principles of
[semantic versioning](http://semver.org) once a first public API is declared.

## Roadmap

See the `TODO.md` file for upcoming plans/changes.
