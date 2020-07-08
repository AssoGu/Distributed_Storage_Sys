%%%-------------------------------------------------------------------
%%% @author asorg
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. Jul 2020 11:23 AM
%%%-------------------------------------------------------------------
-module(files_logic).
-author("asorg").

%% API
-export([readFile/1, split_to_chunks/3, save_to_disk/2, combine_chunks/3, delete_file/2]).
%-define(CHUNK_SIZE, 65536) %64KB chunks

%@read file
readFile(FileName) ->
  {Reason, Binary} = file:read_file(FileName),
  case Reason of
    ok -> Binary;
    _  -> io:format("readFile: File not found~n")
  end.


%@Split files to chunks
split_to_chunks(Bin, LenPart, Acc) when byte_size(Bin) =< LenPart ->
  lists:reverse([Bin | Acc]);
split_to_chunks(Bin, LenPart, Acc) ->
  <<Part:LenPart/binary, Rest/binary>> = Bin,
  split_to_chunks(Rest, LenPart, [Part | Acc]).

%@Save file / chunks to disk
save_to_disk(Bin, FileName) ->
  if
    length(Bin) == 1 ->
      file:write_file(FileName, Bin);
    true ->
      save_chunks(Bin, FileName, 0)
  end .
save_chunks([], _,_) -> ok;
save_chunks(Bin, FileName, PartNo) ->
  PartName = FileName ++ "." ++ "part" ++ integer_to_list(PartNo),
  file:write_file(PartName, hd(Bin)),
  save_chunks(tl(Bin), FileName, PartNo + 1).

%Combining parts to single file
combine_chunks(FileName,0,Acc) ->
  file:write_file(FileName, list_to_binary(lists:reverse(Acc)));
combine_chunks(FileName, ChunksNum, Acc) ->
  PartName = FileName ++ "." ++ "part" ++ integer_to_list(ChunksNum - 1),
  {ok, Bin} = file:read_file(PartName),
  combine_chunks(FileName, ChunksNum-1, Acc++[Bin]).

%@delete file / chunks from disk
delete_file(FileName, ChunksNum) ->
  if
    ChunksNum == 1 ->
      file:delete(FileName) ;
    true ->
      delete_chunks(FileName, ChunksNum)
  end.
delete_chunks(_,0) -> ok;
delete_chunks(FileName, ChunksNum) ->
  PartName = FileName ++ "." ++ "part" ++ integer_to_list(ChunksNum - 1),
  file:delete(PartName),
  delete_chunks(FileName, ChunksNum-1).