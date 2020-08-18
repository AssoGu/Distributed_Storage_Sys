%%%-------------------------------------------------------------------
%%% @author asorg
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 11. Jul 2020 13:51
%%%-------------------------------------------------------------------
-author("asorg").

-define(StorageNode, node()).
-define(LocalDB_folder, "localDB/").
-define(Downloads_folder, "downloads/").
-define(LocalDB, localDB).
-define(GlobalDB, globalDB).
-define(StatisticsDB, statDB).
-define(HashRing,ring).
-define(CHUNK_SIZE, 65536). %64KB chunks
-define(VNODE_SIZE, 655360). %640KB chunks
-define(LoadBalancer, loadbalancer).
%% databases records
-record(?GlobalDB, {filename, creation_date, location, valid}).
-record(?StatisticsDB, {ip, storage_cap, storage_cap_free, vNodes}).