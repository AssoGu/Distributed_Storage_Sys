%%%-------------------------------------------------------------------
%%% @author asorg
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 14. Jul 2020 21:35
%%%-------------------------------------------------------------------
-module(proxy_genserver_calls).
-author("asorg").

-export([get_positions/1,get_positions/2,add_node/2,is_exists/1]).
-include("records.hrl").

%%%===================================================================
%%% Proxy node gen_server calls
%%%===================================================================
%@doc - return file position.
get_positions(FileName) ->
  gen_server:call({global, ?LoadBalancer}, {get_positions, FileName}).
%@doc - return position of N replicas of "FileName" for every part
get_positions(FileName, PartsNum) ->
  gen_server:call({global, ?LoadBalancer}, {get_positions, FileName, PartsNum}).
%@doc - add node to CH ring
add_node(Node, VNodes) ->
  gen_server:call({global, ?LoadBalancer}, {add_node, Node, VNodes}).
%@doc - checks if file exists on database, return exists/not_exists
is_exists(FileName) ->
  gen_server:call({global,?LoadBalancer}, {is_exists,FileName}).