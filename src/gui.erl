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
-export([start/0,storage_button_click/2]).


start() ->
  %Create Top Level window
  wx:new(),
  Frame = wxFrame:new(wx:null(), 1, "Distributed Storage System"),
  wxFrame:setSize(Frame, {300,100}),

%% build and layout the GUI components
  Label = wxStaticText:new(Frame, ?wxID_ANY, "Choose operation mode"),
  ProxyButton = wxButton:new(Frame, ?wxID_ANY, [{label, "Proxy"}]),
  StorageButton = wxButton:new(Frame, ?wxID_ANY, [{label, "Storage"}]),
  %Set Font size
  Font = wxFont:new(16, ?wxFONTFAMILY_DEFAULT, ?wxFONTSTYLE_NORMAL, ?
  wxFONTWEIGHT_BOLD),
  wxTextCtrl:setFont(ProxyButton, Font),
  wxTextCtrl:setFont(StorageButton, Font),
  %Set button background color
  wxButton:setBackgroundColour(ProxyButton,?wxBLACK),
  wxButton:setBackgroundColour(StorageButton,?wxBLACK),
  %Set component sizes and positoins
  MainSizer = wxBoxSizer:new(?wxVERTICAL),
  wxSizer:add(MainSizer, Label, [{flag, ?wxALIGN_CENTER bor ?wxEXPAND},{border,5}]),
  wxSizer:add(MainSizer, ProxyButton, [{flag,?wxALIGN_CENTER bor ?wxALL },{border,5}]),
  wxSizer:add(MainSizer, StorageButton, [{flag,?wxALIGN_CENTER bor ?wxALL},{border,5}]),
  wxWindow:setSizer(Frame, MainSizer),
  %connect buttons to functions
  wxButton:connect(ProxyButton, command_button_clicked, [{callback, proxy_button_click}, {userData, Frame}]),
  wxButton:connect(StorageButton, command_button_clicked, [{callback, fun(Evt, Obj) -> storage_button_click(Evt, Obj)end}, {userData, Frame}]),
  %launch gui
  wxFrame:show(Frame).

proxy_button_click(#wx{ userData = #{userData := Frame}}, _Event)->
  W = wxWindow:new(Frame, ?wxID_ANY,"Storage mode"),
  wxWindow:show(W).

storage_button_click(Evt, _Obj) ->
  Frame = Evt#wx.userData,
  W = wxFrame:new(Frame, ?wxID_ANY, "Storage mode"),
  %Text Boxes
  LabelIP = wxStaticText:new(W, ?wxID_ANY, "Enter Proxy node ip address:"),
  LabelCapacity = wxStaticText:new(W, ?wxID_ANY, "Enter Storage node HDD Capacity (MB):"),
  Ip = wxTextCtrl:new(W, ?wxID_ANY, [{value, " "}]),
  Capacity = wxTextCtrl:new(W, ?wxID_ANY, [{value, " "}]),
  %Button
  StartButton = wxButton:new(W, ?wxID_ANY, [{label, "Start"}]),
  Font = wxFont:new(16, ?wxFONTFAMILY_DEFAULT, ?wxFONTSTYLE_NORMAL, ?
  wxFONTWEIGHT_BOLD),
  wxTextCtrl:setFont(StartButton, Font),
  %Set button background color
  wxButton:setBackgroundColour(StartButton,?wxBLACK),

  %Set component sizes and positions
  WindowSizer = wxBoxSizer:new(?wxVERTICAL),
  wxSizer:add(WindowSizer, LabelIP, [{flag,?wxALIGN_CENTER bor ?wxALL },{border,5}]),
  wxSizer:add(WindowSizer, Ip, [{flag,?wxALIGN_CENTER bor ?wxALL },{border,5}]),
  wxSizer:add(WindowSizer, LabelCapacity, [{flag,?wxALIGN_CENTER bor ?wxALL },{border,5}]),
  wxSizer:add(WindowSizer, Capacity, [{flag,?wxALIGN_CENTER bor ?wxALL},{border,5}]),
  wxSizer:add(WindowSizer, StartButton, [{flag,?wxALIGN_CENTER bor ?wxALL },{border,5}]),
  wxWindow:setSizer(W, WindowSizer),
  wxFrame:show(W).

