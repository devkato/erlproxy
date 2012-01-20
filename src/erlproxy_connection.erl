%%%-------------------------------------------------------------------
%%% @author devkato
%%% @copyright (C) 2012, devkato
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlproxy_connection).
-author("devkato").
-vsn("0.0.1").

-export([start_link/4, init/4]).

-include("include/dev.hrl").

-record(proxy_options, {
    backend_servers,
    backends_socket_options,
    policy
  }).

%% ----------------------------------------------------------------------
%% spawn new process within supervisor tree.
%%
%% ListenSocket : the socket of this server waiting for clients.
%% ListenPort : the port listening.
%% ----------------------------------------------------------------------
start_link(ListenSocket, ListenPort, BackendOptions, BackendServers) ->
  Pid = proc_lib:spawn_link(?MODULE, init, [ListenSocket, ListenPort, BackendOptions, BackendServers]),
  {ok, Pid}.


%% ----------------------------------------------------------------------
%% initialize this process and start accept loop.
%%
%% ListenSocket : the socket of this server waiting for clients.
%% ListenPort : the port listening.
%% ----------------------------------------------------------------------
init(ListenSocket, ListenPort, BackendOptions, BackendServers) ->
  ?APP_DEBUG("init called.", []),
  
  % expand, and add all configurations to ProxyOptions
  ProxyOptions = #proxy_options {
    backend_servers = BackendServers,
    backends_socket_options = proplists:get_value(socket_options, BackendOptions),
    policy = proplists:get_value(policy, BackendOptions)
  },

  %?APP_DEBUG("~p", [ProxyOptions]),

  accept(ListenSocket, ListenPort, ProxyOptions).


%% ----------------------------------------------------------------------
%% wait clients to connect this server.
%%
%% ListenSocket : the socket of this server waiting for clients.
%% ListenPort : the port listening.
%% ----------------------------------------------------------------------
accept(ListenSocket, ListenPort, ProxyOptions) ->
  ?APP_DEBUG("accept called.", []),
  case gen_tcp:accept(ListenSocket) of
    {ok, RecvSocket} ->
      Proxy = spawn(fun() ->
        start_controlling_process(RecvSocket, ProxyOptions)
      end),
      gen_tcp:controlling_process(RecvSocket, Proxy),
      Proxy ! set,
      accept(ListenSocket, ListenPort, ProxyOptions);
    {error, Error} ->
      ?APP_ERROR("~p~n", [Error]),
      accept(ListenSocket, ListenPort, ProxyOptions)
  end.


%% ----------------------------------------------------------------------
%% get controll with the socket.
%% 
%% RecvSocket : the connection with the original client.
%% ----------------------------------------------------------------------
start_controlling_process(RecvSocket, ProxyOptions) ->
  receive
    set ->
      connect_to_remote(RecvSocket, ProxyOptions),
      ?APP_DEBUG("end start_controlling_process", [])
  after 60000 ->
    ?APP_ERROR("message timeout, close socket.", []),
    gen_tcp:close(RecvSocket)
  end.


%% ----------------------------------------------------------------------
%% establish connection to selected remote host, starting recv/send loop.
%%
%% RecvSocket : the connection with the original client.
%% ----------------------------------------------------------------------
connect_to_remote(RecvSocket, ProxyOptions) ->
  {ok, _HostName, RemoteHost, RemotePort, RemoteTimeout} = select_backend(ProxyOptions),

  inet:setopts(RecvSocket,[{active, true}]),

  case gen_tcp:connect(
      RemoteHost,
      RemotePort,
      ProxyOptions#proxy_options.backends_socket_options,
      RemoteTimeout) of
    {ok, ProxySock} ->
      send_receive_data(RecvSocket, ProxySock);
    {error, Reason} ->
      ?APP_ERROR("~p~n", [Reason])
  end,
  ?APP_DEBUG("end handle_data", []).


%% ----------------------------------------------------------------------
%% receive, send packets on receiving a message.
%%
%% RecvSocket : the connection with the original client.
%% ProxySocket : the connection with selected remote host.
%% ----------------------------------------------------------------------
send_receive_data(RecvSocket, ProxySocket) ->
  receive
    {tcp, RecvSocket, Data} ->
      gen_tcp:send(ProxySocket, Data),
      send_receive_data(RecvSocket, ProxySocket);
    {tcp, ProxySocket, Data} ->
      gen_tcp:send(RecvSocket, Data),
      send_receive_data(RecvSocket, ProxySocket);
    {tcp_closed, RecvSocket} ->
      ?APP_DEBUG("RecvSocket closed", []),
      ok;
    {tcp_closed, ProxySocket} ->
      ?APP_DEBUG("ProxySocket closed", []),
      ok
  after 60000 ->
    ?APP_ERROR("timeout, close sockets", []),
    gep_tcp:close(RecvSocket),
    gep_tcp:close(ProxySocket),
    ok
  end.

%% ----------------------------------------------------------------------
%% select a backend server with specified policy.
%% ----------------------------------------------------------------------
select_backend(ProxyOptions) ->
  ?APP_DEBUG("policy -> ~p", [ProxyOptions#proxy_options.policy]),

  [
    {name, HostName},
    {host, RemoteHost},
    {port, RemotePort},
    {weight, _Weight},
    {timeout, RemoteTimeout}
  ] = case ProxyOptions#proxy_options.policy of
    random ->
      % @TODO now() is slow
      random:seed(now()),
      lists:nth(random:uniform(length(ProxyOptions#proxy_options.backend_servers)), ProxyOptions#proxy_options.backend_servers);
    _ ->
      ?APP_ERROR("unknown policy", []),
      [{name, ""}, {host, ""}, {port, 0}, {weight, 0}, {timeout, 0}]
  end,

  ?APP_DEBUG("HostName -> ~p", [HostName]),
  ?APP_DEBUG("RemoteHost -> ~p", [RemoteHost]),
  ?APP_DEBUG("RemotePort -> ~p", [RemotePort]),
  ?APP_DEBUG("RemoteTimeout -> ~p", [RemoteTimeout]),

  {ok, HostName, RemoteHost, RemotePort, RemoteTimeout}.


