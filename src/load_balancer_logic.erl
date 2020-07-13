%%%-------------------------------------------------------------------
%%% @author adircohen
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. Jul 2020 0:09
%%%-------------------------------------------------------------------
-module(load_balancer_logic).
-author("adircohen").
-compile(export_all).
-define(HASH, md5).

%@doc
%% Input - Nodes: List of Nodes
%% VNodes: List, number of VNodes created for each Node in "Nodes" respectively
%% Output
new_ring(Nodes, VNodes) ->
  %lists:zip create lists of  [{NodeName, NumOfVNodes}...]
  NodesHashes = [node_position(X) || X <- lists:zip(Nodes, VNodes)],
  %Build gb_tree
  gb_trees:from_orddict(lists:keysort(1,lists:flatten(NodesHashes))).

%@doc 
%% Input - Key - hash of object
%% Input - Ring - gb_tree
%% Input - N Closest Nodes to Key
%% Output - [Node ID1, Node ID2 ..., Node IDN]
ring_lookup(Key, Ring, N) ->
  Iter = gb_trees:iterator_from(Key, Ring),
  ring_lookup(Iter, Ring, N, []).

ring_lookup(_,_,0,Nodes) -> Nodes;
ring_lookup(Iter, Ring, N, Nodes) ->
  %Iterate over hash ring nodes, starting from "Iter"
  case gb_trees:next(Iter) of
    none ->
      %End of the ring, start iterate from the beginning of the ring
      {SmallestKey, _} = gb_trees:smallest(Ring),
      ring_lookup(gb_trees:iterator_from(SmallestKey, Ring), Ring, N, Nodes);
    {_,Node, IterRest} ->
      %Check if Node already exists in "Nodes" list.
      IsExists = lists:member(Node, Nodes),
      if
        IsExists == false ->
          %Node does not exists, append to "Nodes"
          ring_lookup(IterRest, Ring, N-1, [Node|Nodes]);
        true ->
          %Node exists, skip to next node
          ring_lookup(IterRest, Ring, N, Nodes)
      end
  end.

%@doc
%% Input - Key - hash of object
%% Input - Ring - gb_tree
%% Output - Node ID
ring_lookup(Key, Ring) ->
  Iter = gb_trees:iterator_from(Key, Ring),
  %Get first element greater or equal to Key
  case gb_trees:next(Iter) of
    none ->
      %Not found, return the smallest element in the tree -> the beginning of the hash ring
      {_,Node} = gb_trees:smallest(Ring);
    {_,Node,_} ->
      %Key found, return node
      Node
  end.

%@doc
%% Input - FileName
%% Input - Ring - gb_tree
%% Input - N - Number of replicas
%% Output - [{PartName0,[Pos1,Pos2..],{PartName1, [Pos1,Pos2..]}...],
get_positions(FileName, ChunksNum, N, Ring) when ChunksNum > 1 ->
  PartNames =  gen_part_names(FileName, ChunksNum),
  [{PartName, ring_lookup(hash(PartName),Ring, N)} || PartName <- PartNames];
get_positions(FileName, _, N, Ring) ->
  {FileName, ring_lookup(hash(FileName),Ring, N)}.

%%====================================================================
%% Internal functions
%%====================================================================

node_position({Node,VNodes}) ->
  [{hash(Node, Indx), Node} ||  Indx <- lists:seq(1, VNodes)].

hash(X) ->
  XBin = term_to_binary(X),
  crypto:hash(?HASH, <<XBin/binary>>).

hash(X, Y) ->
  XBin = term_to_binary(X),
  YBin = term_to_binary(Y),
  crypto:hash(?HASH, <<XBin/binary, YBin/binary>>).

gen_part_names(FileName, PartsNum) ->
  gen_part_names(FileName, PartsNum, []).
gen_part_names(_,0,PartNames) ->
  PartNames;
gen_part_names(FileName, PartsNum, PartNames) ->
  PartName = FileName ++ "." ++ "part" ++ integer_to_list(PartsNum - 1),
  gen_part_names(FileName, PartsNum-1, [PartName|PartNames]).

test_ring() ->
  new_ring(["10.0.0.2","10.0.0.3","10.0.0.7","10.0.0.5", "10.10.10.10"],[2,3,4,3,4]).