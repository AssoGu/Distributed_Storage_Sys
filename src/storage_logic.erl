%%%-------------------------------------------------------------------
%%% @author adircohen
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Description:
%%%     Storage interface for storage nodes.
%%% Created : 09. Jul 2020 21:28
%%%-------------------------------------------------------------------
-module(storage_logic).
-author("adircohen").
-include("records.hrl").


%% API
-export([upload_file/1, download_file/1, delete_file/1]).



% 1. check if the file already exists
% 3. read the file
% 4. split file to chunks
% 5. hash each chunk and create list of 3 positions per chunk
% 6. create tuple of {storage_node, [chunks to upload]}
% 7. upload the chunks to the storage nodes
% 8. update mnesia DB with valid 1

upload_file(FileName) ->
  io:format("uploading file= ~w .... ~n",[FileName]),
  % 1. check if the file already exists
  Result = database_logic:global_find_file(FileName),
  if
    Result == exists ->
      io:format("file= ~w already exists, please use update_file instead ~n",[FileName]),
      file_already_exists;
    true ->
    % 3. read the file
      File = files_logic:read_file(FileName,?LocalDB_folder),
    % 4. split file to chunks
      Chunks = files_logic:split_to_chunks(File, ?CHUNK_SIZE, []),
    % 5. hash each chunk and create list of 3 positions per chunk
      ChunksNum = length(Chunks),
      % positions looks like: [ {PartName, [Pos1, Pos2]}, {PartName, [Pos1, Pos2]}... ]
      Positions = load_balancer_logic:get_positions(FileName,ChunksNum),
    % 6. upload the chunks to the storage nodes
      upload_chunks(Positions, Chunks),
    % 7. update mnesia DB with valid 1
      database_logic:global_insert_file(FileName,Positions),
      io:format("Finish upload file= ~w ~n",[FileName])
  end.

upload_chunks([],[]) ->
  io:format("finish upload chunks ~n");

upload_chunks([PartNameAndLocations|T],[Chunk|Rest]) ->
  spawn(?MODULE, upload_chunk,[PartNameAndLocations,Chunk]),
  upload_chunks(T,Rest).

upload_chunk({FileName, [Pos|T]}, Chunk) ->
  RetVal = storage_genserver:upload_file({FileName, Chunk}, Pos),
  case RetVal of
    ok    ->
      upload_chunk({FileName, T}, Chunk);
    _Else ->
      io:format("Upload ~p failed~n",[FileName]),
      upload_chunk({FileName, T}, Chunk)
  end.

download_file(FileName) ->
  io:format("downloading file= ~w .... ~n",[FileName])
%PROXY NODE
% 1. check if the file exists in mnesia DB
% 2. collect a list of parts locations
% 3. generate tuple {PID, [chunks to download]}
% STORAGE NODE
%   foreach
%     gen_server:call(PID, {storage_logic:send_file,FileName})
% 4. download the chunks from the storage nodes
% 5. combine_chunks
% 6. save_to_disk
.

%%% -----------------
%%% delete file logic
%%% -----------------
% 1. check if the file already exists
% 2. update mnesia DB with valid 0
% 3. get from mnesia DB the locations of each chunk
% 4. create tuple of {storage_node, [chunks to upload]}
% 5. delete the chunks from the storage nodes
% 6. update mnesia DB

delete_file(FileName) ->
  io:format("deleting file= ~w .... ~n",[FileName])
% 1. check if the file already exists
% 2. update mnesia DB with valid 0
% 3. get from mnesia DB the locations of each chunk
% 4. create tuple of {storage_node, [chunks to upload]}
% 5. delete the chunks from the storage nodes
% 6. update mnesia DB
.


%%% Internal functions

%upload_chunks(FileName, Chunks, Node)

%download_chunks(FileName, Chunks, Node)

%delete_chunks(FileName, Chunks, Node)

%send_chunks(FileName, Chunks, Node)
