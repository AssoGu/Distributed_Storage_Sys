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
-export([main/0,op_mode_dialog/1, storage_button_click/2]).


%%%==============================================================
%%% Main gui
%%%==============================================================

main() ->
  wx:new(),
  MainFrame = wxFrame:new(wx:null(), ?wxID_ANY, "Distributed Storage System"),
  wxFrame:setSize(MainFrame, {1000,800}),
  wxFrame:show(MainFrame),
  op_mode_dialog(MainFrame).




%%%==============================================================
%%% Operation mode
%%%==============================================================

op_mode_dialog(Frame) ->
  %Create Top Level window
  Dialog = wxDialog:new(Frame, ?wxID_ANY, "Distributed Storage System"),
  wxDialog:setSize(Dialog, {300,100}),

%% build and layout the GUI components
  Label = wxStaticText:new(Dialog, ?wxID_ANY, "Choose operation mode"),
  ProxyButton = wxButton:new(Dialog, ?wxID_ANY, [{label, "Proxy"}]),
  StorageButton = wxButton:new(Dialog, ?wxID_ANY, [{label, "Storage"}]),
  %Set Font size
  Font = wxFont:new(16, ?wxFONTFAMILY_DEFAULT, ?wxFONTSTYLE_NORMAL, ?
  wxFONTWEIGHT_BOLD),
  wxTextCtrl:setFont(ProxyButton, Font),
  wxTextCtrl:setFont(StorageButton, Font),
  %Set button background color
  wxButton:setBackgroundColour(ProxyButton,?wxBLACK),
  wxButton:setBackgroundColour(StorageButton,?wxBLACK),
  %Set component sizes and positions
  MainSizer = wxBoxSizer:new(?wxVERTICAL),
  wxSizer:add(MainSizer, Label, [{flag, ?wxALIGN_CENTER bor ?wxEXPAND},{border,5}]),
  wxSizer:add(MainSizer, ProxyButton, [{flag,?wxALIGN_CENTER bor ?wxALL },{border,5}]),
  wxSizer:add(MainSizer, StorageButton, [{flag,?wxALIGN_CENTER bor ?wxALL},{border,5}]),
  wxWindow:setSizer(Dialog, MainSizer),
  %connect buttons to functions
  %wxButton:connect(ProxyButton, command_button_clicked, [{callback, fun main_window/2}, {userData, {Dialog, "Proxy"}}]),
  wxButton:connect(StorageButton, command_button_clicked, [{callback, fun storage_button_click/2}, {userData, {Frame, Dialog}}]),
  %launch gui
  wxDialog:showModal(Dialog).

storage_button_click(Evt, _Obj) ->
  {TopFrame, LastDialog} = Evt#wx.userData,
  %Close last dialog
  wxDialog:close(LastDialog),
  NewDialog = wxDialog:new(TopFrame, ?wxID_ANY, "Storage mode"),
  %Text Boxes
  LabelIP = wxStaticText:new(NewDialog, ?wxID_ANY, "Enter Proxy node ip address:"),
  LabelCapacity = wxStaticText:new(NewDialog, ?wxID_ANY, "Enter Storage node HDD Capacity (MB):"),
  Ip = wxTextCtrl:new(NewDialog, ?wxID_ANY, [{value, " "}]),
  Capacity = wxTextCtrl:new(NewDialog, ?wxID_ANY, [{value, " "}]),
  %Button
  StartButton = wxButton:new(NewDialog, ?wxID_ANY, [{label, "Start"}]),
  Font = wxFont:new(16, ?wxFONTFAMILY_DEFAULT, ?wxFONTSTYLE_NORMAL, ?
  wxFONTWEIGHT_BOLD),
  wxTextCtrl:setFont(StartButton, Font),
  wxButton:setBackgroundColour(StartButton,?wxBLACK),
  wxButton:connect(StartButton, command_button_clicked),

  %Set component sizes and positions
  WindowSizer = wxBoxSizer:new(?wxVERTICAL),
  wxSizer:add(WindowSizer, LabelIP, [{flag,?wxALIGN_CENTER bor ?wxALL },{border,5}]),
  wxSizer:add(WindowSizer, Ip, [{flag,?wxALIGN_CENTER bor ?wxALL },{border,5}]),
  wxSizer:add(WindowSizer, LabelCapacity, [{flag,?wxALIGN_CENTER bor ?wxALL },{border,5}]),
  wxSizer:add(WindowSizer, Capacity, [{flag,?wxALIGN_CENTER bor ?wxALL},{border,5}]),
  wxSizer:add(WindowSizer, StartButton, [{flag,?wxALIGN_CENTER bor ?wxALL },{border,5}]),
  wxWindow:setSizer(NewDialog, WindowSizer),

  %connect buttons to functions
  wxDialog:showModal(NewDialog),
  receive
    _click ->
      Input_ip  = wxTextCtrl:getValue(Ip),
      Input_cap = wxTextCtrl:getValue(Capacity),
      io:format("ip ~p cap ~p ~n",[Input_ip,Input_cap])
  end.




%%%==============================================================
%%% Main window gui
%%%==============================================================

main_window(Evt,_Obj) ->
  {Frame, Op_mode} = Evt#wx.userData,
  wxFrame:close(Frame),
  MainFrame = wxFrame:new(wx:null(), ?wxID_ANY, "Distributed Storage System - " ++ Op_mode ++ " mode"),
  wxFrame:setSize(MainFrame, {1000,1000}),
  wxFrame:show(MainFrame).



