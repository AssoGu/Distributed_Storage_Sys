%%%-------------------------------------------------------------------
%%% @author asorg
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 19. Aug 2020 21:04
%%%-------------------------------------------------------------------
-module(gui_genserver).
-author("asorg").

-behavior(wx_object).


%% wx_object callbacks
-export([start/1, init/1, terminate/2,  code_change/3,
  handle_info/2, handle_call/3, handle_cast/2, handle_event/2]).

-include_lib("wx/include/wx.hrl").
-include("records.hrl").
%% Menu definitions
-define(menuDownload, 10).
-define(menuUpload, 11).
-define(menuDelete, 12).
-define(storageButton, 30).
-define(proxyButton, 40).
-define(pi, wxAuiPaneInfo).

-record(state,
{
  frame,
  filesLB,
  statLB,
  infoTC
}).

start(Confing) ->
  wx_object:start_link(?MODULE,Confing, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init(_) ->
  Wx = wx:new(),
  {Frame, StatListBox, InfoTextCtrl, FilesListBox} = wx:batch(fun() -> gui_logic:create_window(Wx) end),
  wxWindow:show(Frame),
  erlang:send_after(1, self(), updateGui),
  {Frame, #state{frame=Frame, filesLB=FilesListBox, statLB=StatListBox, infoTC=InfoTextCtrl}}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Callbacks handled as normal gen_server callbacks
handle_cast(Msg, State) ->
  io:format("Got cast ~p~n",[Msg]),
  {noreply,State}.

%% Async Events are handled in handle_event as in handle_info
handle_event(Event=#wx{event=#wxCommand{type=command_menu_selected}},
    State) ->
  ID = Event#wx.id,
  case ID of
    ?menuDownload ->
      Prompt = "Please enter file name here.",
      MD = wxTextEntryDialog:new(State#state.frame, Prompt, [{caption, "Download"}]),
      case wxTextEntryDialog:showModal(MD) of
        ?wxID_OK ->
          Str = wxTextEntryDialog:getValue(MD);
        _ -> ok
      end,
      wxDialog:destroy(MD);
    ?menuUpload ->
      MD = wxFileDialog:new(State#state.frame),
      case wxFileDialog:showModal(MD) of
        ?wxID_OK ->
          Str = wxFileDialog:getPath(MD);
        ?wxID_CANCEL -> ok
      end,
      wxFileDialog:destroy(MD);
    ?menuDelete ->
      Prompt = "Please enter file name here.",
      MD = wxTextEntryDialog:new(State#state.frame,Prompt, [{caption, "Delete"}]),
      case wxTextEntryDialog:showModal(MD) of
        ?wxID_OK ->
          Str = wxTextEntryDialog:getValue(MD);
        _ -> ok
      end,
      wxDialog:destroy(MD)
  end,
  {noreply, State};

handle_event(Event=#wx{event=#wxCommand{type=command_listbox_doubleclicked}},
    State) ->
  %Extract selected file
  File = Event#wx.event#wxCommand.cmdString,
  [Record] = mnesia:dirty_read(?GlobalDB,File),
  Pos = Record#?GlobalDB.location,
  %Create new dialog to show positions of files
  Dialog = wxDialog:new(State#state.frame, ?wxID_ANY, "Files locations"),
  wxDialog:setSize(Dialog, {350,350}),
  ListBox = wxListBox:new(Dialog, ?wxID_ANY, [{style,?wxLB_HSCROLL},{size, {300,200}}]),
  wxListBox:set(ListBox, gui_logic:prepare_for_gui(Pos)),
  wxDialog:showModal(Dialog),
  {noreply, State}.


handle_info(updateGui, State) ->
  %Update Files ListBox
  Files = mnesia:dirty_all_keys(?GlobalDB),
  if
    Files == [] ->
      ok;
    true ->
      wxListBox:set(State#state.filesLB, Files)
  end,

  %Update Statistics ListBox
  erlang:send_after(5000, self(), updateGui),
  {noreply,State}.

handle_call(_, _From, State) ->
  {stop, normal, ok, State}.

code_change(_, _, State) ->
  {stop, ignore, State}.

terminate(_Reason, _) ->
  ok.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%% Internal Functions %%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
