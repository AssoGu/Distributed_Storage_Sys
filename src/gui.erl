-module(gui).
-include_lib("wx/include/wx.hrl").

-export([start/0]).
%% Menu definitions
-define(menuDownload, 10).
-define(menuUpload, 11).
-define(menuDelete, 12).
-define(pi, wxAuiPaneInfo).

start()->
	Wx = wx:new(),
	Frame = wx:batch(fun() -> create_window(Wx) end),
	wxWindow:show(Frame),
	op_mode_dialog(Frame),
	loop(Frame),
	wx:destroy(),
	ok.

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
	create_pane(Panel, Manager,
		?pi:caption(?pi:left(?pi:new(Pane)), "Statistics")),
	create_pane(Panel, Manager,
		?pi:caption(?pi:bottom(?pi:new(Pane)), "Log")),
	create_listbox(Panel, Manager,
		?pi:caption(?pi:centre(?pi:new(Pane)), "Files")),
	create_menu(Frame),

	wxAuiManager:connect(Manager, aui_pane_button, [{skip,true}]),
	wxAuiManager:connect(Manager, aui_pane_maximize, [{skip,true}]),
	wxAuiManager:update(Manager),

	%% Set Connect Close
	wxFrame:connect(Frame, close_window),

	Frame.

create_pane(Parent, Manager, Pane) ->
	TextCtrl = wxTextCtrl:new(Parent, ?wxID_ANY, [{size, {300,200}},
		{value, "An empty pane"},
		{style, 0
			bor ?wxDEFAULT
			bor ?wxTE_MULTILINE}]),
	wxAuiManager:addPane(Manager, TextCtrl, Pane),
	TextCtrl.

create_listbox(Parent, Manager, Pane) ->
	ListBox = wxListBox:new(Parent, ?wxID_ANY, [{size, {300,200}}]),
	wxAuiManager:addPane(Manager, ListBox, Pane),
	ListBox.

create_menu(Parent) ->
	MenuBar = wxMenuBar:new(),
	setup_menubar(MenuBar),
	wxFrame:connect(Parent, command_menu_selected),
	wxFrame:setMenuBar(Parent, MenuBar).

setup_menubar(Menu) ->
	Opt = wxMenu:new(),
	wxMenu:append(Opt, ?menuDownload,"&Download"),
	wxMenu:append(Opt, ?menuUpload,"&Upload"),
	wxMenu:append(Opt, ?menuDelete,"&Delete"),
	wxMenu:appendSeparator(Opt),
	%wxMenu:append(File, ?wxID_EXIT, "&Quit"),
	wxMenuBar:append(Menu, Opt, "&Options").

loop(Frame) ->
	receive
		#wx{id=?menuDownload, event=#wxCommand{type=command_menu_selected}}->
			Prompt = "Please enter file name here.",
			MD = wxTextEntryDialog:new(Frame,Prompt, [{caption, "Download"}]),
			case wxTextEntryDialog:showModal(MD) of
				?wxID_OK ->
					Str = wxTextEntryDialog:getValue(MD),
					io:format("downloading ~p~n", [Str]);
				_ -> ok
			end,
			wxDialog:destroy(MD),
			loop(Frame);

		#wx{id=?menuUpload, event=#wxCommand{type=command_menu_selected}}->
			io:format("Upload~n"),
			loop(Frame);
		#wx{id=?menuDelete, event=#wxCommand{type=command_menu_selected}}->
			io:format("delete~n"),
			loop(Frame)
	end.




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
	DialogSizer = wxBoxSizer:new(?wxVERTICAL),
	wxSizer:add(DialogSizer, Label, [{flag, ?wxALIGN_CENTER bor ?wxEXPAND},{border,5}]),
	wxSizer:add(DialogSizer, ProxyButton, [{flag,?wxALIGN_CENTER bor ?wxALL },{border,5}]),
	wxSizer:add(DialogSizer, StorageButton, [{flag,?wxALIGN_CENTER bor ?wxALL},{border,5}]),
	wxWindow:setSizer(Dialog, DialogSizer),
	%connect buttons to functions
	wxButton:connect(ProxyButton, command_button_clicked, [{callback, fun proxy_button_click/2}, {userData, Dialog}]),
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
	dss_app:start(storage, Input_ip, Input_cap).

proxy_button_click(Evt, _Obj) ->
	Window = Evt#wx.userData,
	wxDialog:close(Window),
	dss_app:start(proxy).

close_window_click(Evt, _Obj) ->
	Window = Evt#wx.userData,
	wxDialog:close(Window).



