%%%-------------------------------------------------------------------
%% @doc dss public API
%% @end
%%%-------------------------------------------------------------------

-module(dss_app).

-behaviour(application).

-export([start/0,start/1, start/3, stop/1]).
-include("records.hrl").


start() ->
    %launch operation mode gui
    Wx = wx:new(),
    {OpMode,ProxyIP,Cap} = gui_logic:op_mode_dialog(Wx),
    wx:destroy(),
    %start Node
    case OpMode of
        storage ->
            start(storage,ProxyIP,Cap);
        proxy ->
            start(proxy)
    end,
    %launch main gui
    gui_genserver:start(0).


start(storage, ProxyNode, Capacity) ->
    %Starts mnesia and connect to proxy node
    mnesia:start(),
    Pong = net_adm:ping(ProxyNode),
    io:format("~p~n",[Pong]),
    %Wait until shared DB received
    global:sync(),
    mnesia:wait_for_tables([?GlobalDB,?StatisticsDB], 10),
    %spawn storage node with supervisor
    dss_storage_sup:start_link(),
    %calculate number of vNodes according to resources (to do)
    VNodes = calculate_VNodes(Capacity),
    %add storage node to hash ring
    proxy_genserver_calls:add_node(node(), node(), VNodes),
    %add node statistics
    database_logic:statistics_add_node(node(), {Capacity, VNodes}).

start(proxy) ->
    %Create schema and start mnesia
    mnesia:create_schema([node()]),
    mnesia:start(),
    %Init databases
    database_logic:initDB(),
    %spawn proxy node with supervisor
    Ret = dss_proxy_sup:start_link(),
    io:format("Proxy node Online - ~p ~n", [Ret]).



stop(_State) ->
    ok.

%% internal functions
%Used to calculate vNodes according to the resources
calculate_VNodes(Capacity) -> 5.
