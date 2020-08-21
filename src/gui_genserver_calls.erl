%%%-------------------------------------------------------------------
%%% @author asorg
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. Aug 2020 10:22
%%%-------------------------------------------------------------------
-module(gui_genserver_calls).
-author("asorg").


%% API
-export([log/1, log/2]).

-include("records.hrl").

log(Msg,Arg) ->
  Str = string:replace(Msg,"~p",Arg,all),
  wx_object:cast(?Gui,{log,Str}).

log(Msg) ->
  wx_object:cast(?Gui,{log,Msg}).