%%%-------------------------------------------------------------------
%%% @author asorg
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 14. Jul 2020 21:35
%%%-------------------------------------------------------------------
-module(proxy_node_calls).
-author("asorg").

-compile(export_all).
-include("records.hrl").

%%%===================================================================
%%% Proxy node gen_server calls
%%%===================================================================
%@doc - return position of N replicas of "FileName"
get_positions(FileName, N) ->
  gen_server:call({global, ?LoadBalancer}, {get_positions, FileName, N}).
%@doc - return position of N replicas of "FileName" for every part
get_positions(FileName, PartsNum, N) ->
  gen_server:call({global, ?LoadBalancer}, {get_positions, FileName, PartsNum, N}).
%@doc - add node to CH ring
add_node(Node, VNodes) ->
  gen_server:call({global, ?LoadBalancer}, {add_node, Node, VNodes}).