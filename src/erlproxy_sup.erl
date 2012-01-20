%%%-------------------------------------------------------------------
%%% @author devkato
%%% @copyright (C) 2012, devkato
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlproxy_sup).
-author("devkato").
-vsn("0.0.1").


-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
  {ok, InitOptions} = application:get_env(erlproxy, server_info),

  ListenPort = proplists:get_value(port, InitOptions),
  BindAddress = proplists:get_value(bind, InitOptions),
  MaxConnections = proplists:get_value(max_connections, InitOptions),

  SocketOptions = [
    binary,
    {packet, raw},
    {ip, BindAddress},
    {reuseaddr, true},
    %{nodelay, true},
    {backlog, 4096},
    {keepalive, true},
    %{recbuf, default},
    {active, false}
  ],

  MonitorSpec = {erlproxy_monitor,
    {erlproxy_monitor, start_link, []},
    permanent, 5000, worker, [erlproxy_monitor]
  },

  case gen_tcp:listen(ListenPort, SocketOptions) of
    {ok, ListenSocket} ->
      Connections = [
        {{erlproxy_connection, N},
          {erlproxy_connection, start_link, [ListenSocket, ListenPort]},
          permanent, brutal_kill, worker, [erlproxy_connection]
        } || N <- lists:seq(1, MaxConnections) ],

      {ok, { {one_for_one, 5, 10}, lists:flatten([Connections, MonitorSpec])} };
    {error, Reason} -> 
      io:format("~p~n", [Reason]),
      {error, Reason}
  end.

