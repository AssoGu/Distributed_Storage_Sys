%%%-------------------------------------------------------------------
%%% @author asorg
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. Jul 2020 16:55
%%%-------------------------------------------------------------------
-module(database_logic).
-author("asorg").

-define(LocalDB_folder, "/localDB/").
-define(LocalDB, localDB).
-define(GlobalDB, globalDB).
-define(TopologyDB, topologyDB).
%% databases records
-record(?GlobalDB, {filename, creation_date, location, valid}).
-record(?TopologyDB, {ip, capacity, vNodes_count, vNodes_keys}).

-compile(export_all).


%%%==============================================================
%%% Databases Initialization API
%%%==============================================================
%% Schema_create, mnesia:start() needed to be called before using DB api's

%%@ Global Databases, needed to be called from supervisor node only.

initDB()->
  mnesia:create_table(?GlobalDB, [{disc_copies, [node()]},{type, set},{attributes, record_info(fields,?GlobalDB)}]),
  mnesia:create_table(?TopologyDB, [{disc_copies, [node()]},{type, set},{attributes, record_info(fields, ?TopologyDB)}]).

%%@ Scans Stored files folder and builds ets database.
initLocalDB() ->
  Files = file:list_dir(?LocalDB_folder),
  ets:new(localDB, [set, named_table]),
  lists:foreach(fun(Key) -> ets:insert(?LocalDB, {Key, valid}) end, Files).

%%%==============================================================
%%% Local Database API
%%%==============================================================

local_insert_file(FileName) ->
  ets:insert(?LocalDB, {FileName, valid}).

local_delete_file(FileName) ->
  ets:delete(?LocalDB, FileName).

local_find_file(FileName) ->
  ets:lookup(?LocalDB, FileName).

%%%==============================================================
%%% Global Database API
%%%==============================================================

%@doc
%% Inputs - FileName , type String
%% Output - {atomic,ok}
global_insert_file(FileName) ->
  Entry = #?GlobalDB{filename=FileName, creation_date = calendar:universal_time(), location = [node()], valid = 1},
  Fun = fun() ->
    mnesia:write(Entry)
        end,
  mnesia:transaction(Fun).

%@doc
%% Input - FileName , type String
%% Output - {atomic,[{globalDB,"Index0.JPG",
%%                   {{2020,7,9},{19,24,8}},
%%                   ['node1@MacBook-Pro'],
%%                   1}]}
global_find_file(FileName) ->
  Entry = {?GlobalDB, FileName},
  Fun = fun() ->
    mnesia:read(Entry)
        end,
  mnesia:transaction(Fun).

%@doc
%% Input - FileName , type String
%% Output - {atomic,[{globalDB,"Index0.JPG",
%%                   {{2020,7,9},{19,24,8}},
%%                   ['node1@MacBook-Pro'],
%%                   1}]}
global_update_file(FileName) ->
  Entry = {?GlobalDB, FileName},
  Fun = fun() ->
    global_delete_file(Entry),
    global_insert_file(Entry)
    end,
  mnesia:transaction(Fun).

%@doc
%% Inputs - FileName , type String
%% Output - {atomic,ok}
global_delete_file(FileName) ->
  Entry = {?GlobalDB, FileName},
  Fun = fun() ->
    mnesia:delete(Entry)
        end,
  mnesia:transaction(Fun).

%%%==============================================================
%%% General functions
%%%==============================================================

add_node(Node) ->
  mnesia:change_config(extra_db_nodes, [Node]).

%%%==============================================================
%%% General functions
%%%==============================================================
test() ->
  global_insert_file(#?GlobalDB{filename="Index0.JPG", creation_date = calendar:universal_time(), location = [node()], valid = 1}),
  global_insert_file(#?GlobalDB{filename="Index1.JPG", creation_date = calendar:universal_time(), location = [node()], valid = 1}),
  global_insert_file(#?GlobalDB{filename="Index2.JPG", creation_date = calendar:universal_time(), location = [node()], valid = 1}).