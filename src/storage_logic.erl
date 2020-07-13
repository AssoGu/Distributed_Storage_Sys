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
% 6. upload the chunks to the storage nodes
% 7. update mnesia DB with valid 1

upload_file(FileName) ->
  io:format("uploading file= ~p .... ~n",[FileName]),
  % 1. check if the file already exists
  Result = database_logic:global_is_exists(FileName),
  if
    Result == exists ->
      io:format("file= ~p already exists, please use update_file instead ~n",[FileName]),
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
      io:format("Finish upload file= ~p ~n",[FileName])
  end.

% 1. check if the file exists in mnesia DB
% 2. collect a list of parts locations
% 3. download the chunks from the storage nodes
% 4. combine_chunks and save to disk
% 5. delete all temporary chunks saved to memory in step #3.


download_file(FileName) ->
  io:format("downloading file= ~p .... ~n",[FileName]),
  % 1. check if the file exists in mnesia DB
  Result = database_logic:global_is_exists(FileName),
  if
    Result == exists ->
      % 2. collect a list of parts locations
      File = database_logic:global_find_file(FileName),
      Positions = File#?GlobalDB.location,
      % 3. download the chunks from the storage nodes
      download_chunks(Positions),
      % 4. combine_chunks and save to disk
      ChunksNum = length(Positions),
      files_logic:combine_chunks(FileName, ChunksNum),
      % 5. delete all temporary chunks saved to memory in step #3.
      delete_chunks(Positions);
    true ->
      io:format("file= ~p does not exists in global DB ~n",[FileName])
  end.

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

%%% --------------------------- %%%
%%%  Upload Internal Functions  %%%
%%% --------------------------- %%%

% Handle each the full list of chunks and positions
upload_chunks([],[]) ->
  io:format("finish upload chunks ~n");

upload_chunks([PartNameAndLocations|T],[Chunk|Rest]) ->
  spawn(?MODULE, upload_chunk,[PartNameAndLocations,Chunk]),
  upload_chunks(T,Rest).

% Handle one part of a file
upload_chunk({FileName, []}, _) ->
  io:format("finish upload chunk= ~p ~n",[FileName]),
  ok;

upload_chunk({FileName, [Pos|T]}, Chunk) ->
  RetVal = storage_genserver:upload_file({FileName, Chunk}, Pos),
  case RetVal of
    ok    ->
      upload_chunk({FileName, T}, Chunk);
    _Else ->
      io:format("Upload ~p failed~n",[FileName]),
      upload_chunk({FileName, T}, Chunk)
  end.

%%% --------------------------- %%%
%%% Download Internal Functions %%%
%%% --------------------------- %%%

% Handle each part from the full list of chunks and positions
download_chunks([]) ->
  io:format("finish download chunks ~n");

download_chunks([PartNameAndLocations|T]) ->
  spawn(?MODULE, download_chunk,[PartNameAndLocations]),
  download_chunks(T).

% Handle one part of a file
download_chunk({PartName, []}) ->
  io:format("ERROR - There is no more servers to download from the part ~p ~n",[PartName]);

% Steps:
% 1. Downaload the Chunk
% 2. If ok, store in memory, Else - try next storage for this part
download_chunk({PartName, [Pos|T]}) ->
  RetVal = storage_genserver:download_file(PartName, Pos),
  case RetVal of
    ok    ->
      {_, Chunk} = RetVal,
      files_logic:save_to_disk(PartName, [Chunk], ?LocalDB_folder),
      io:format("Finish saving part ~p into memory ~n",[PartName]);
    _Else ->
      io:format("Download ~p failed, trying to download from next server ~n",[PartName]),
      download_chunk({PartName, T})
  end.

%%% --------------------------- %%%
%%%  Delete Internal Functions  %%%
%%% --------------------------- %%%

% Delete all parts of a file from memory
delete_chunks([]) ->
  io:format("Finish deleting all parts ~n");

delete_chunks([{PartName, _}|T]) ->
  files_logic:delete_file(PartName, ?LocalDB_folder),
  io:format("Finish deleting part ~p from memory ~n",[PartName]),
  delete_chunks(T).


%upload_chunks(FileName, Chunks, Node)

%download_chunks(FileName, Chunks, Node)

%delete_chunks(FileName, Chunks, Node)

%send_chunks(FileName, Chunks, Node)
