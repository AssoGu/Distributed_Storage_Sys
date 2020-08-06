%%%-------------------------------------------------------------------
%%% @author asorg
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. Aug 2020 20:31
%%%-------------------------------------------------------------------
-module(gui).
-author("asorg").
-include_lib("wx/include/wx.hrl").

%% API
-export([start/0]).


start() -> wx:new(),
  Frame = wxFrame:new(wx:null(), 1, "Countdown"),
  Button0 = wxButton:new(Frame, ?wxID_ANY, [{label, "Proxy"}]),
  Button1 = wxButton:new(Frame, ?wxID_ANY, [{label, "Storage"}]),
  Button2 = wxButton:new(Frame, ?wxID_ANY, [{label, "Client"}]),
  %wxButton:connect(Button, command_button_clicked, [{callback, fun handle_click/2}, {userData, #{counter => Counter, env => wx:get_env()}}]),
  wxFrame:show(Frame).