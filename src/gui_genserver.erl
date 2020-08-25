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
-define(pi, wxAuiPaneInfo).

-record(state,
{
  frame,
  filesLB,
  statLB,
  infoTC
}).

start(Config) ->
  wx_object:start_link(?MODULE,Config, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init(_) ->
  register(?Gui,self()),
  Wx = wx:new(),
  {Frame, StatListBox, InfoTextCtrl, FilesListBox} = wx:batch(fun() -> gui_logic:create_window(Wx) end),
  wxWindow:show(Frame),
  erlang:send_after(1, self(), updateGui),
  {Frame, #state{frame=Frame, filesLB=FilesListBox, statLB=StatListBox, infoTC=InfoTextCtrl}}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Callbacks handled as normal gen_server callbacks
handle_cast({log,Msg}, State) ->
  {H,M,_} = time(),
  Time = integer_to_list(H)++":"++integer_to_list(M)++"  ",
  Str = Time ++ Msg ++"\n",
  wxTextCtrl:appendText(State#state.infoTC,Str),
  {noreply,State};

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
          Str = wxTextEntryDialog:getValue(MD),
          storage_logic:download_file(Str);
        _ -> ok
      end,
      wxDialog:destroy(MD);
    ?menuUpload ->
      MD = wxFileDialog:new(State#state.frame),
      case wxFileDialog:showModal(MD) of
        ?wxID_OK ->
          Str = wxFileDialog:getPath(MD),
          storage_logic:upload_file(Str);
        ?wxID_CANCEL -> ok
      end,
      wxFileDialog:destroy(MD);
    ?menuDelete ->
      Prompt = "Please enter file name here.",
      MD = wxTextEntryDialog:new(State#state.frame,Prompt, [{caption, "Delete"}]),
      case wxTextEntryDialog:showModal(MD) of
        ?wxID_OK ->
          Str = wxTextEntryDialog:getValue(MD),
          storage_logic:delete_file(Str);
        _ -> ok
      end,
      wxDialog:destroy(MD)
  end,
  {noreply, State};

handle_event(Event=#wx{id=Id,event=#wxCommand{type=command_listbox_doubleclicked}},
    State) ->

  case Id of
    ?FilesWin ->
      %Extract selected file
      File = Event#wx.event#wxCommand.cmdString,
      [Record] = mnesia:dirty_read(?GlobalDB,File),
      Pos = Record#?GlobalDB.location,
      %Create new dialog to show positions of files
      Dialog = wxDialog:new(State#state.frame, ?wxID_ANY, "Files locations"),
      wxDialog:setSize(Dialog, {350,350}),
      ListBox = wxListBox:new(Dialog, ?wxID_ANY, [{style,?wxLB_HSCROLL},{size, {300,200}}]),
      wxListBox:set(ListBox, gui_logic:prepare_for_gui(Pos)),
      wxDialog:showModal(Dialog);
    ?OnlineWin ->
      Node = Event#wx.event#wxCommand.cmdString,
      {_,[Entry]} = database_logic:statistics_get_node(Node),
      Dialog = wxDialog:new(State#state.frame, ?wxID_ANY, "Node information"),
      wxDialog:setSize(Dialog, {350,200}),

      T0 = wxStaticText:new(Dialog,?wxID_ANY,"Rule: " ++ Entry#?StatisticsDB.rule),
      T1 = wxStaticText:new(Dialog,?wxID_ANY,"Node: " ++ Node),
      T2 = wxStaticText:new(Dialog,?wxID_ANY,"Capacity: " ++ integer_to_list(Entry#?StatisticsDB.storage_cap_free div 1000000)
      ++"/"++ integer_to_list(Entry#?StatisticsDB.storage_cap div 1000000) ++" MB"),
      T3 = wxStaticText:new(Dialog,?wxID_ANY,"VNodes: " ++ integer_to_list(Entry#?StatisticsDB.vNodes)),

      DialogSizer = wxBoxSizer:new(?wxVERTICAL),
      wxSizer:add(DialogSizer, T0, [{flag,?wxALIGN_LEFT bor ?wxALL bor ?wxEXPAND},{border,5}]),
      wxSizer:add(DialogSizer, T1, [{flag,?wxALIGN_LEFT bor ?wxALL bor ?wxEXPAND },{border,5}]),
      wxSizer:add(DialogSizer, T2, [{flag,?wxALIGN_LEFT bor ?wxALL bor ?wxEXPAND},{border,5}]),
      wxSizer:add(DialogSizer, T3, [{flag,?wxALIGN_LEFT bor ?wxALL bor ?wxEXPAND},{border,5}]),
      wxWindow:setSizer(Dialog, DialogSizer),

      Font = wxFont:new(16, ?wxFONTFAMILY_DEFAULT, ?wxFONTSTYLE_NORMAL, ?
      wxFONTWEIGHT_BOLD),
      wxTextCtrl:setFont(T0, Font),
      wxTextCtrl:setFont(T1, Font),
      wxTextCtrl:setFont(T2, Font),
      wxTextCtrl:setFont(T3, Font),
      wxDialog:showModal(Dialog)
  end,

  {noreply, State}.


handle_info(updateGui, State) ->
  %Update Files ListBox
  Files = mnesia:dirty_all_keys(?GlobalDB),
  if
    Files == [] ->
      wxListBox:clear(State#state.filesLB);
    true ->
      wxListBox:set(State#state.filesLB, Files)
  end,
  Stat = mnesia:dirty_all_keys(?StatisticsDB),
  if
    Stat == [] ->
      wxListBox:clear(State#state.statLB);
    true ->
      wxListBox:set(State#state.statLB, Stat)
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
