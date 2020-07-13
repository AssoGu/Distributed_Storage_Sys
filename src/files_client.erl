%%%-------------------------------------------------------------------
%%% @author adircohen
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 09. Jul 2020 18:13
%%%-------------------------------------------------------------------
-module(files_client).
-author("adircohen").

download_file(FileName) ->
  files_server:download_file(FileName).

upload_file(FileName) ->
  files_server:upload_file(FileName).

delete_file(FileName) ->
  files_server:delete_file(FileName).

%% API
-export([]).
