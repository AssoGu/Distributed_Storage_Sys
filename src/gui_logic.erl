-module(gui_logic).
-include_lib("wx/include/wx.hrl").

-export([create_window/1, op_mode_dialog/1,prepare_for_gui/1]).

-include("records.hrl").
%% Menu definitions

-define(pi, wxAuiPaneInfo).


create_window(Wx)->
	%% Create Frame
	Frame = wxFrame:new(Wx,
		-1,
		"Distributed Storage System",
		%%[{size,{300,200}}]),
		[{size,{-1,-1}}]),

	%% Create Panel
	%%Panel1 = wxPanel:new(Frame),
	Panel = wxPanel:new(Frame, []),

	%% Setup sizers
	MainSizer = wxBoxSizer:new(?wxVERTICAL),

	%% Create AuiManager
	Manager = wxAuiManager:new([{managed_wnd, Panel}
	]),

	%% Create wxAuiPaneInfo
	Pane = ?pi:new(),
	?pi:closeButton(Pane),
	?pi:right(Pane),
	?pi:dockable(Pane, [{b, true}]),
	?pi:floatingSize(Pane, 300,200),
	?pi:minSize(Pane, {50,50}),
	?pi:paneBorder(Pane),
	?pi:floatable(Pane, [{b, true}]),

	%% Create Pane Clone?
	%create_pane(Panel, Manager, Pane),
	StatListBox = create_listbox(Panel, Manager,
		?pi:caption(?pi:left(?pi:new(Pane)), "Online Nodes"),?OnlineWin),
	InfoTextCtrl = create_pane(Panel, Manager,
		?pi:caption(?pi:bottom(?pi:new(Pane)), "Log")),
	FilesListBox = create_listbox(Panel, Manager,
		?pi:caption(?pi:centre(?pi:new(Pane)), "Files"),?FilesWin),
	MenuBar = create_menu(Frame),

	wxAuiManager:connect(Manager, aui_pane_button, [{skip,true}]),
	wxAuiManager:connect(Manager, aui_pane_maximize, [{skip,true}]),
	wxAuiManager:update(Manager),

	%% Set Connect Close
	wxFrame:connect(Frame, close_window),

	{Frame, StatListBox, InfoTextCtrl, FilesListBox,MenuBar}.

create_pane(Parent, Manager, Pane) ->
	TextCtrl = wxTextCtrl:new(Parent, ?wxID_ANY, [{size, {300,200}},
		{value, ""},
		{style, 0
			bor ?wxDEFAULT
			bor ?wxTE_MULTILINE}]),
	wxAuiManager:addPane(Manager, TextCtrl, Pane),
	TextCtrl.

create_listbox(Parent, Manager, Pane,Id) ->
	ListBox = wxListBox:new(Parent, Id, [{size, {300,200}}]),
	wxListBox:connect(ListBox,command_listbox_doubleclicked,[]),
	wxAuiManager:addPane(Manager, ListBox, Pane),
	ListBox.

create_menu(Parent) ->
	MenuBar = wxMenuBar:new(),
	setup_menubar(MenuBar),
	wxFrame:connect(Parent, command_menu_selected,[]),
	wxFrame:setMenuBar(Parent, MenuBar),
	MenuBar.

setup_menubar(Menu) ->
	Opt = wxMenu:new(),
	wxMenu:append(Opt, ?menuDownload,"&Download"),
	wxMenu:append(Opt, ?menuUpload,"&Upload"),
	wxMenu:append(Opt, ?menuDelete,"&Delete"),
	wxMenu:appendSeparator(Opt),
	wxMenuBar:append(Menu, Opt, "&Options").





%%%==============================================================
%%% Operation mode
%%%==============================================================

op_mode_dialog(Wx) ->
	%% Create Frame
	Frame = wxFrame:new(Wx,
		-1,
		"Distributed Storage System",
		%%[{size,{300,200}}]),
		[{size,{-1,-1}}]),
	%Create Top Level window
	Dialog = wxDialog:new(Frame, ?wxID_ANY, "Distributed Storage System"),
	wxDialog:setSize(Dialog, {300,100}),

%% build and layout the GUI components
	Label = wxStaticText:new(Dialog, ?wxID_ANY, "Choose operation mode"),
	ProxyButton = wxButton:new(Dialog, ?proxyButton, [{label, "Proxy"}]),
	StorageButton = wxButton:new(Dialog, ?storageButton, [{label, "Storage"}]),
	%Set Font size
	Font = wxFont:new(16, ?wxFONTFAMILY_DEFAULT, ?wxFONTSTYLE_NORMAL, ?
	wxFONTWEIGHT_BOLD),
	wxTextCtrl:setFont(ProxyButton, Font),
	wxTextCtrl:setFont(StorageButton, Font),
	%Set button background color
	wxButton:setBackgroundColour(ProxyButton,?wxBLACK),
	wxButton:setBackgroundColour(StorageButton,?wxBLACK),
	%Set component sizes and positions
	DialogSizer = wxBoxSizer:new(?wxVERTICAL),
	wxSizer:add(DialogSizer, Label, [{flag, ?wxALIGN_CENTER bor ?wxEXPAND},{border,5}]),
	wxSizer:add(DialogSizer, ProxyButton, [{flag,?wxALIGN_CENTER bor ?wxALL },{border,5}]),
	wxSizer:add(DialogSizer, StorageButton, [{flag,?wxALIGN_CENTER bor ?wxALL},{border,5}]),
	wxWindow:setSizer(Dialog, DialogSizer),
	%connect buttons to functions
	wxButton:connect(ProxyButton, command_button_clicked,[]),
	wxButton:connect(StorageButton, command_button_clicked,[]),
	%launch gui
	wxDialog:show(Dialog),
	receive
		#wx{id=?storageButton, event=#wxCommand{type=command_button_clicked}}->
			storage_button_click(Frame,Dialog);

		#wx{id=?proxyButton, event=#wxCommand{type=command_button_clicked}}->
			proxy_button_click(Dialog)
	end.

storage_button_click(TopFrame, LastDialog) ->
	%Close last dialog
	wxDialog:close(LastDialog),
	NewDialog = wxDialog:new(TopFrame, ?wxID_ANY, "Storage mode"),
	%Text Boxes
	LabelIP = wxStaticText:new(NewDialog, ?wxID_ANY, "Enter Proxy node ip address:"),
	LabelCapacity = wxStaticText:new(NewDialog, ?wxID_ANY, "Enter Storage node HDD Capacity (MB):"),
	Ip = wxTextCtrl:new(NewDialog, ?wxID_ANY, [{value, ""}]),
	Capacity = wxTextCtrl:new(NewDialog, ?wxID_ANY, [{value, ""}]),
	%Button
	StartButton = wxButton:new(NewDialog, ?wxID_ANY, [{label, "Start"}]),
	Font = wxFont:new(16, ?wxFONTFAMILY_DEFAULT, ?wxFONTSTYLE_NORMAL, ?
	wxFONTWEIGHT_BOLD),
	wxTextCtrl:setFont(StartButton, Font),
	wxButton:setBackgroundColour(StartButton,?wxBLACK),
	wxButton:connect(StartButton, command_button_clicked,[{callback, fun close_window_click/2}, {userData, NewDialog}]),

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
	Input_ip  = wxTextCtrl:getValue(Ip),
	Input_cap = wxTextCtrl:getValue(Capacity),
	{storage,list_to_atom(Input_ip),list_to_integer(Input_cap)}.

proxy_button_click(Dialog) ->
	wxDialog:close(Dialog),
	{proxy,null,null}.

close_window_click(Evt, _Obj) ->
	Window = Evt#wx.userData,
	wxDialog:close(Window).





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%% helper function %%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Prepare the given list for gui representation
prepare_for_gui(List) ->
	prepare_for_gui(List,[]).

prepare_for_gui([],Acc) ->
	Acc;
prepare_for_gui([{PartName,Loc}|Rest], Acc) ->
	Str = PartName ++ ", Location: " ++ list_to_string(Loc,[]),
	prepare_for_gui(Rest, Acc++[Str]).



list_to_string(List = [H|T],Acc) when length(List) > 1 ->
	Str = "["++atom_to_list(H)++"]"++",",
	list_to_string(T,Acc ++ Str);

list_to_string([H|T],Acc) ->
	Str = "["++atom_to_list(H)++"]"++ ".",
	Acc++Str.

