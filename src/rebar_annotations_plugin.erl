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
-module(rebar_annotations_plugin).

-export([pre_compile/2, post_compile/2]).

pre_compile(_, undefined) -> ok;
pre_compile(Config, _) ->
    case get_config(Config) of
        {config, AnnotationsConfig} ->
            file:write_file(config_file(), 
                            printable(AnnotationsConfig), [write]);
        _ ->
            ok
    end.

post_compile(_, undefined) -> ok;
post_compile(Config, _) ->
    case get_config(Config) of
        {config, _} ->
            file:delete(config_file()),
            ok;
        _ ->
            ok
    end.

printable(Term) ->
    erl_prettypr:format(erl_parse:abstract(Term))  ++ ".".

config_file() ->
    filename:join(rebar_utils:get_cwd(), "annotations.config").

get_config(Config) ->
    case is_basedir() of
        false ->
            ignored;
        true ->
            case rebar_config:get_local(Config, annotations, []) of
                [] -> 
                    ignored;
                AnnotationsConfig ->
                    {config, AnnotationsConfig}
            end
    end.

is_basedir() ->
    rebar_utils:get_cwd() == rebar_config:get_global(base_dir, undefined).
