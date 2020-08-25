%%%-------------------------------------------------------------------
%%% @author asorg
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 14. Jul 2020 21:35
%%%-------------------------------------------------------------------
-module(storage_genserver_calls).
-author("asorg").

%% API
-export([download_file/2,delete_file/2,upload_file/2,update_file/2,transfer/1]).

%%%===================================================================
%%% Storage gen_server calls
%%%===================================================================

%@doc
%% Input - FileName , String
%% Output - {FileName,Binary}
%% Output error - {FileName, notFound}
download_file(FileName, Dest) ->
  RetVal = global:whereis_name(Dest),
  case RetVal of
    _ ->
      gen_server:call({global, Dest}, {download_file, FileName});
    undefined ->
      {reply, undefined}
  end.

%@doc
%% Input - {FileName, Binary}
%% Output - ok
%% Output error - {error,Reason}
upload_file(File, Dest) ->
  RetVal = global:whereis_name(Dest),
  case RetVal of
    _ ->
      gen_server:call({global, Dest}, {upload_file, File}, infinity);
    undefined ->
      {reply, undefined}
  end.

%@doc
%% Input - FileName , String
%% Output - ok
%% Output error - {error, Reason}
delete_file(FileName, Dest) ->
  RetVal = global:whereis_name(Dest),
  case RetVal of
    _ ->
      gen_server:call({global, Dest}, {delete_file, FileName}, infinity);
    undefined ->
      {reply, undefined}
  end.

%@doc
%% Input - FileName , String
%% Output - ok
%% Output error - {error, Reason}
update_file(FileName, Dest) ->
  RetVal = global:whereis_name(Dest),
  case RetVal of
    _ ->
      gen_server:call({global, Dest}, {update_file, FileName}, infinity);
    undefined ->
      {reply, undefined}
  end.

transfer([]) ->
  ok;

transfer([{Old,New}|Rest]) ->
  transfer_file(Old,New),
  transfer(Rest).


%%% --------------------------- %%%
%%%  Internal Functions %%%
%%% --------------------------- %%%
transfer_file({_,[]},[]) ->
  ok;
transfer_file({PartName,[Dest|Rest]},[NewDest|Rest2]) ->
  if
    Dest /= NewDest ->
      gen_server:cast({global,Dest},{transfer,{PartName,NewDest}});
    true ->
      ok
  end,
  transfer_file({PartName,Rest},Rest2).
