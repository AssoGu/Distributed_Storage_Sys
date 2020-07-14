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
-record(state, {}).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================
start_link() ->
  gen_server:start_link({global, ?LoadBalancer}, ?MODULE, [], []).

init([]) ->
  {ok, #state{}}.

handle_call({add_node, Node, VNodes}, _From, State = #state{}) ->
  case get(?HashRing) of
    undefined ->
      load_balancer_logic:new_ring([Node],[VNodes]);
    _else ->
      load_balancer_logic:add_node(Node,VNodes)
  end,
  {reply, ok, State};

handle_call({get_positions, FileName, N}, _From, State = #state{}) ->
  Positions = load_balancer_logic:get_positions(FileName, N),
  {reply, Positions, State};

handle_call({get_positions, FileName, PartsNum, N}, _From, State = #state{}) ->
  Positions = load_balancer_logic:get_positions(FileName, PartsNum, N),
  {reply, Positions, State}.

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