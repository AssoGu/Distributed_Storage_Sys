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

-export([get_positions/1,get_positions/2,add_node/3, is_exists/1, exit_node/1,terminate/0]).
-include("records.hrl").

%%%===================================================================
%%% Proxy node gen_server calls
%%%===================================================================
%@doc - return file position.
%%1.	get positions:
%%•	call proxy_genserver in order to receive file positions
get_positions(FileName) ->
  gen_server:call({global, ?LoadBalancer}, {get_positions, FileName}).
%@doc - return position of N parts
get_positions(FileName, PartsNum) ->
  gen_server:call({global, ?LoadBalancer}, {get_positions, FileName, PartsNum}).

%%2.	add node:
%%•	call proxy gen_server in order to add a node to the CH ring.
%@doc - add node to CH ring
add_node(Node, StorageGenPid, {Cap, VNodes}) ->
  gen_server:call({global, ?LoadBalancer}, {add_node, Node, StorageGenPid,VNodes,Cap}, infinity).
%@doc - checks if file exists on database, return exists/not_exists

exit_node(Node) ->
  gen_server:call({global, ?LoadBalancer}, {exit_node, Node}, infinity).
%@doc - checks if file exists on database, return exists/not_exists

%%3.	Is exists:
%%•	call proxy gen_server in order to check if a node is exists
is_exists(FileName) ->
  gen_server:call({global,?LoadBalancer}, {is_exists,FileName}).

terminate() ->
  gen_server:cast({global,?LoadBalancer}, terminate).

