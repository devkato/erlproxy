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

%% ----------------------------------------------------------------------
%% @spec start(StartType, StartArgs) -> Result
%%  StartType = atom()
%%  StartArgs = restart_type()
%%  Result = ok | {error, Reason}
%%  Reason = term()
%%
%% @doc start application.
%% @end
%% ----------------------------------------------------------------------
start(StartType, StartArgs) ->
    erlproxy_sup:start_link().

%% ----------------------------------------------------------------------
%% @spec stop(State) -> Result
%%  State = atom()
%%  Result = atom()
%%
%% @doc stop application.
%% @end
%% ----------------------------------------------------------------------
stop(State) ->
    ok.
