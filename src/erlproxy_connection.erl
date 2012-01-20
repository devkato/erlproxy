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

-export([start_link/2, init/2]).

-include("include/dev.hrl").


%% ----------------------------------------------------------------------
%% supervisorにプロセスをヒモ付て起動
%%
%% ListenSocket : サーバーが開放しているソケット
%% ListenPort : サーバーが開放しているポート
%% ----------------------------------------------------------------------
start_link(ListenSocket, ListenPort) ->
  Pid = proc_lib:spawn_link(?MODULE, init, [ListenSocket, ListenPort]),
  {ok, Pid}.


%% ----------------------------------------------------------------------
%% 初期化処理後、acceptループを開始
%%
%% ListenSocket : サーバーが開放しているソケット
%% ListenPort : サーバーが開放しているポート
%% ----------------------------------------------------------------------
init(ListenSocket, ListenPort) ->
  ?APP_DEBUG("init called.~n", []),
  accept(ListenSocket, ListenPort).


%% ----------------------------------------------------------------------
%% TCP接続要求を待つ。接続してきた時点でaccept以下が処理される。
%% それまではgen_tcp:acceptで待機状態になる。
%%
%% ListenSocket : サーバーが開放しているソケット
%% ListenPort : サーバーが開放しているポート
%% ----------------------------------------------------------------------
accept(ListenSocket, ListenPort) ->
  ?APP_DEBUG("accept called.~n", []),
  case gen_tcp:accept(ListenSocket) of
    {ok, RecvSocket} ->
      Proxy = spawn(fun() ->
        start_controlling_process(RecvSocket)
      end),
      gen_tcp:controlling_process(RecvSocket, Proxy),
      Proxy ! set,
      accept(ListenSocket, ListenPort);
    {error, Error} ->
      ?APP_ERROR("~p~n", [Error]),
      accept(ListenSocket, ListenPort)
  end.


%% ----------------------------------------------------------------------
%% acceptした後で渡されたデータを実際に処理するプロセスが実行する関数。
%% 
%% RecvSocket : 元々接続してきたクライアントとの接続
%% ----------------------------------------------------------------------
start_controlling_process(RecvSocket) ->
  receive
    set ->
      connect_to_remote(RecvSocket),
      ?APP_DEBUG("end start_controlling_process~n", [])
  after 60000 ->
    ?APP_ERROR("message timeout, close socket.", []),
    gen_tcp:close(RecvSocket)
  end.


%% ----------------------------------------------------------------------
%% プロキシ先との接続を確立し、データのやり取りのループ処理を開始。
%%
%% RecvSocket : 元々接続してきたクライアントとの接続
%% ----------------------------------------------------------------------
connect_to_remote(RecvSocket) ->
  {ok, SelectedHost} = erlproxy_monitor:select_host(),
  ?APP_DEBUG("selected host => ~p~n", [proplists:get_value(name, SelectedHost)]),

  RemoteHost = proplists:get_value(host, SelectedHost),
  RemotePort = proplists:get_value(port, SelectedHost),
  RemoteTimeout = case proplists:get_value(timeout, SelectedHost) of
    undefined ->
      10000;
    Timeout ->
      Timeout
  end,

  ?APP_INFO("RemoteTimeout -> ~p~n", [RemoteTimeout]),

  inet:setopts(RecvSocket,[{active, true}]),
  case gen_tcp:connect(RemoteHost, RemotePort, [binary, {active, true}, {packet, 0} ], RemoteTimeout) of
    {ok, ProxySock} ->
      send_receive_data(RecvSocket, ProxySock);
    {error, Reason} ->
      ?APP_ERROR("~p~n", [Reason])
  end,
  ?APP_DEBUG("end handle_data~n", []).


%% ----------------------------------------------------------------------
%% 各ソケットからデータを取得して、もう片方にデータを受け渡す。
%%
%% RecvSocket : 元々接続してきたクライアントとの接続
%% ProxySocket : プロキシ先との接続
%% ----------------------------------------------------------------------
send_receive_data(RecvSocket, ProxySocket) ->
  receive
    {tcp, RecvSocket, Data} ->
      %?APP_DEBUG("recv -> proxy : ~p~n", [Data]),
      gen_tcp:send(ProxySocket, Data),
      send_receive_data(RecvSocket, ProxySocket);
    {tcp, ProxySocket, Data} ->
      %?APP_DEBUG("proxy -> recv : ~p~n", [Data]),
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


