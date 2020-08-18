%%%-------------------------------------------------------------------
%% @doc dss public API
%% @end
%%%-------------------------------------------------------------------

-module(dss_app).

-behaviour(application).

-export([start/1, start/3, stop/1]).
-include("records.hrl").

start(storage, ProxyNode, Capacity) ->
    %Starts mnesia and connect to proxy node
    mnesia:start(),
    net_adm:ping(ProxyNode),
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
    dss_proxy_sup:start_link().



stop(_State) ->
    ok.

%% internal functions

%Used to calculate vNodes according to the resources
calculate_VNodes(Capacity) ->
    Vnodes = idiv(Capacity, ?VNODE_SIZE),
    Vnodes.


floor(X) when X < 0 ->
    T = trunc(X),
    case X - T == 0 of
        true -> T;
        false -> T - 1
    end;

floor(X) ->
    trunc(X).

idiv(A, B) ->
    floor(A / B).