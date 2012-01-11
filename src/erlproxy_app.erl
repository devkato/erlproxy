%%%-------------------------------------------------------------------
%%% @author devkato
%%% @copyright (C) 2012, devkato
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlproxy_app).
-author("devkato").
-vsn("0.0.1").

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    erlproxy_sup:start_link().

stop(_State) ->
    ok.
