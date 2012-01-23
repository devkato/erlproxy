%%%-------------------------------------------------------------------
%%% @author devkato
%%% @copyright (C) 2012, devkato
%%% @doc Connection module to wait server socket accept new connection.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlproxy_connection).
-author("devkato").
-vsn("0.0.1").

-export([start_link/5, init/5]).

-include("include/dev.hrl").

-record(proxy_options, {
  backend_servers::list(),
  backend_server_num::integer(),
  backends_socket_options::list(),
  policy::atom(),
  table::ets:tid()
}).


%% ----------------------------------------------------------------------
%% @spec start_link(ListenSocket, ListenPort, BackendOptions, BackendServers, RRT) -> Result
%%  ListenSocket = integer()
%%  ListenPort = integer()
%%  BackendOptions = list(tuple())
%%  BackendServers = [[{ Key::atom(), Value::string()|integer() }]]
%%  RRT = ets:tid()
%%  Result = {ok, pid()}
%%
%% @doc spawn new process within supervisor tree.
%% @end
%% ----------------------------------------------------------------------
-spec start_link(
  ListenSocket::integer(),
  ListenPort::integer(),
  BackendOptions::list(tuple()),
  BackendServers::[[{ Key::atom(), Value::string()|integer() }]],
  RRT::ets:tid()
) -> {ok, pid()}.
start_link(ListenSocket, ListenPort, BackendOptions, BackendServers, RRT) ->
  Pid = proc_lib:spawn_link(?MODULE, init, [ListenSocket, ListenPort, BackendOptions, BackendServers, RRT]),
  {ok, Pid}.


%% ----------------------------------------------------------------------
%% @spec init(ListenSocket, ListenPort, BackendOptions, BackendServers, RRT) -> Result
%%  ListenSocket = integer()
%%  ListenPort = integer()
%%  BackendOptions = list(tuple())
%%  BackendServers = [[{ Key::atom(), Value::string()|integer() }]]
%%  RRT = ets:tid()
%%  Result = atom()
%%
%% @doc initialize this process and start accept loop.
%% @end
%% ----------------------------------------------------------------------
init(ListenSocket, ListenPort, BackendOptions, BackendServers, RRT) ->
  ?APP_DEBUG("init called.", []),
  
  % expand, and add all configurations to ProxyOptions
  ProxyOptions = #proxy_options {
    backend_servers         = BackendServers,
    backend_server_num      = length(BackendServers),
    backends_socket_options = proplists:get_value(socket_options, BackendOptions),
    policy                  = proplists:get_value(policy, BackendOptions),
    table                   = RRT
  },

  accept(ListenSocket, ListenPort, ProxyOptions).


%% ----------------------------------------------------------------------
%% @spec accept(ListenSocket, ListenPort, ProxyOptions) -> Result
%%  ListenSocket = integer()
%%  ListenPort = integer()
%%  ProxyOptions = list()
%%  Result = ok
%%
%% @doc wait clients to connect this server.
%% @end
%% ----------------------------------------------------------------------
-spec accept(ListenSocket::integer(), ListenPort::integer(), ProxyOptions::list()) -> atom().
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
  end,

  ok.


%% ----------------------------------------------------------------------
%% @spec start_controlling_process(RecvSocket, ProxyOptions) -> Result
%%  RecvSocket = integer()
%%  ProxyOptions = list()
%%  Result = atom()
%%
%% @doc get controll with the socket.
%% @end
%% ----------------------------------------------------------------------
-spec start_controlling_process(RecvSocket::integer(), ProxyOptions::list()) -> ok.
start_controlling_process(RecvSocket, ProxyOptions) ->
  receive
    set ->
      connect_to_remote(RecvSocket, ProxyOptions),
      ?APP_DEBUG("end start_controlling_process", [])
  after 60000 ->
    ?APP_ERROR("message timeout, close socket.", []),
    gen_tcp:close(RecvSocket)
  end,

  ok.


%% ----------------------------------------------------------------------
%% @spec connect_to_remote(RecvSocket, ProxyOptions) -> ok
%%  RecvSocket = integer()
%%  ProxyOptions = list()
%%  Result = atom()
%%
%% @doc establish connection to selected remote host, starting recv/send loop.
%% @end
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
  ?APP_DEBUG("end handle_data", []),

  ok.


%% ----------------------------------------------------------------------
%% @spec send_receive_data(RecvSocket, ProxySocket) -> Result
%%  RecvSocket = integer()
%%  ProxySocket = integer()
%%  Result = atom()
%%
%% @doc receive, send packets on receiving a message.
%% @end
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
%% @spec select_backend(ProxyOptions) -> Result
%%  ProxyOptions = list()
%%  Result = {
%%    atom(),
%%    string(),
%%    integer(),
%%    integer()
%%  }
%%
%% @doc select a backend server with specified policy.
%% @end
%% ----------------------------------------------------------------------
-spec select_backend(ProxyOptions::proxyoptions) -> {
    atom(),
    string(),
    string(),
    integer(),
    integer()
  }.
select_backend(ProxyOptions) ->
  ?APP_DEBUG("policy -> ~p", [ProxyOptions#proxy_options.policy]),

  [
    {name, HostName},
    {host, RemoteHost},
    {port, RemotePort},
    {weight, _Weight},
    {timeout, RemoteTimeout}
  ] = case ProxyOptions#proxy_options.policy of
    roundrobin ->
      NextServer = ets:update_counter(
        ProxyOptions#proxy_options.table,
        current_server,
        {2, 1, ProxyOptions#proxy_options.backend_server_num, 1}
      ),
      ?APP_DEBUG("NextServer -> ~p", [NextServer]),

      lists:nth(NextServer, ProxyOptions#proxy_options.backend_servers);
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


