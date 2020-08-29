%%%-------------------------------------------------------------------
%%% @author asorg
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(proxy_genserver).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

-include("records.hrl").

-record(state,{}).


%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================
start_link() ->
  gen_server:start_link({global, ?LoadBalancer}, ?MODULE, [], []).

init([]) ->
  {ok, #state{}}.


%%%===================================================================
%%% Handle call
%%%===================================================================


%%.	Is exists:
%%•	Check in the main DB if the node is exists

handle_call({is_exists, FileName}, _From, State = #state{}) ->
  IsExists = database_logic:global_is_exists(FileName),
  {reply, IsExists, State};

%%•	Check if the node is not already exists.
%%•	If not, contact with load balancer to find the positions of this new node.
%%•	Contact the load balancer in order to re balance the ring and the file stored in each node.
%%•	Update the DB

handle_call({add_node, Node, StorageGenPid, VNodes, Cap}, _From, State = #state{}) ->
  database_logic:share_db(Node),
  database_logic:statistics_add_node(atom_to_list(Node), {Cap, VNodes},"Storage"),
  case get(?HashRing) of
    undefined ->
      io:format("new ring created~n"),
      gui_genserver_calls:log("New node has been joined!"),
      load_balancer_logic:new_ring([StorageGenPid],[VNodes]);
    _else ->
      io:format("new node~n"),
      gui_genserver_calls:log("New node has been joined!"),
      load_balancer_logic:add_node(StorageGenPid,VNodes),
      load_balancer_logic:rebalance_ring()
  end,
  {reply, ok, State};

% exit node
handle_call({exit_node, Node}, _From, State = #state{}) ->
  RetVal = global:whereis_name(Node),
  case RetVal of
    _ ->
      io:format("handle exit node~n"),
      gui_genserver_calls:log("Node ~p disconnected",atom_to_list(Node)),
      %delete the node from the tree
      load_balancer_logic:delete_node(Node),
      % re construct files on the ring for the new one.
      load_balancer_logic:rebalance_ring(),
      storage_genserver_calls:exit_node(Node);
    undefined ->
      {reply, undefined}
  end,
  {reply, ok, State};

%%1.	get positions:
%%•	interact with the load balancer to get positions:
%%i.	create the hash of each file part and construct the positions

handle_call({get_positions, FileName}, _From, State = #state{}) ->
  Positions = load_balancer_logic:get_positions(FileName, ?Replicas),
  {reply, Positions, State};

handle_call({get_positions, FileName, PartsNum}, _From, State = #state{}) ->
  Positions = load_balancer_logic:get_positions(FileName, PartsNum, ?Replicas),
  {reply, Positions, State}.

%%%===================================================================
%%% Handle cast
%%%===================================================================


handle_cast(terminate, State = #state{}) ->
  mnesia:stop(),
  gui_genserver_calls:terminate(),
  supervisor:terminate_child(whereis(proxySup), self()),
  {noreply, State};
  %{stop, normal,State};

handle_cast(test, State = #state{}) ->
  test_ring(),
  {noreply, State}.

handle_info(_Info, State = #state{}) ->
  {noreply, State}.

terminate(_Reason, _State = #state{}) ->
  ok.

code_change(_OldVsn, State = #state{}, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
test_ring() ->
  load_balancer_logic:new_ring(["10.0.0.2","10.0.0.3","10.0.0.7","10.0.0.5", "10.10.10.10"],[2,3,4,3,4]).