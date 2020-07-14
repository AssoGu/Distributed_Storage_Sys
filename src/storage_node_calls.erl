%%%-------------------------------------------------------------------
%%% @author asorg
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 14. Jul 2020 21:35
%%%-------------------------------------------------------------------
-module(storage_node_calls).
-author("asorg").

%% API
-compile(export_all).

%%%===================================================================
%%% Storage gen_server calls
%%%===================================================================

%@doc
%% Input - FileName , String
%% Output - {FileName,Binary}
%% Output error - {FileName, notFound}
download_file(FileName, Dest) ->
  gen_server:call(Dest, {download_file, FileName}).

%@doc
%% Input - {FileName, Binary}
%% Output - ok
%% Output error - {error,Reason}
upload_file(File, Dest) ->
  gen_server:call(Dest, {upload_file, File}).

%@doc
%% Input - FileName , String
%% Output - ok
%% Output error - {error, Reason}
delete_file(FileName, Dest) ->
  gen_server:call(Dest, {delete_file, FileName}).