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

-include("include/dev.hrl").

%% ===================================================================
%% API functions
%% ===================================================================

%% ----------------------------------------------------------------------
%% @spec start_link() -> Result
%%
%% @doc start link
%% @end
%% ----------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

%% ----------------------------------------------------------------------
%% @spec init(Options) -> Result
%%  Options = list()
%%
%% @doc 
%% @end
%% ----------------------------------------------------------------------
-spec init(Options::list()) -> ok.
init([]) ->
  {ok, InitOptions} = application:get_env(erlproxy, server_info),

  % Proxy Server Configurations
  ListenPort = proplists:get_value(port, InitOptions),
  PoolConnections = proplists:get_value(pool_connections, InitOptions),
  SocketOptions = proplists:get_value(socket_options, InitOptions),

  % Load Backends Configurations
  BackendOptions = proplists:get_value(backends, InitOptions),

  % Backend Servers
  BackendServers = proplists:get_value(servers, InitOptions),

  % backends list for round-robin policy
  RoundRobinTable = ets:new(roundrobin, [set, public]),
  ets:insert(RoundRobinTable, {current_server, 0}),

  case gen_tcp:listen(ListenPort, SocketOptions) of
    {ok, ListenSocket} ->
      ConnectionInitOptions = [
        ListenSocket,
        ListenPort,
        BackendOptions,
        BackendServers,
        RoundRobinTable
      ],

      ?APP_INFO("Create ~p connection pool ... ", [PoolConnections]),

      Connections = [
        {{erlproxy_connection, N},
          {erlproxy_connection, start_link, ConnectionInitOptions},
          permanent, brutal_kill, worker, [erlproxy_connection]
        } || N <- lists:seq(1, PoolConnections) ],

      {ok, { {one_for_one, 5, 10}, lists:flatten(Connections)} };
    {error, Reason} -> 
      io:format("~p~n", [Reason]),
      {error, Reason}
  end.

