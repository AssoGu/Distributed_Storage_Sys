%%%-------------------------------------------------------------------
%%% @author adircohen
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 09. Jul 2020 18:13
%%%-------------------------------------------------------------------
-module(files_server).
-author("adircohen").

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

%% client API
-export([download_file/1, upload_file/1, delete_file/1]).

-define(SERVER, ?MODULE).

-record(files_server_state, {}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Spawns the server and registers the local name (unique)
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server
-spec(init(Args :: term()) ->
  {ok, State :: #files_server_state{}} | {ok, State :: #files_server_state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([]) ->
  io:format("~p (~p) starting.... ~n",[{local, ?MODULE}, self()]),
  {ok, #files_server_state{}}.

%% @private
%% @doc Handling call messages
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #files_server_state{}) ->
  {reply, Reply :: term(), NewState :: #files_server_state{}} |
  {reply, Reply :: term(), NewState :: #files_server_state{}, timeout() | hibernate} |
  {noreply, NewState :: #files_server_state{}} |
  {noreply, NewState :: #files_server_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #files_server_state{}} |
  {stop, Reason :: term(), NewState :: #files_server_state{}}).

handle_call(_Request, _From, State = #files_server_state{}) ->
  {reply, ok, State}.

%%% =======================
%%%         NEW
%%% =======================

handle_call({download_file,FileName}, _From, State = #files_server_state{}) ->
  %check if exists and blabla
  File = files_logic:readFile()
  {reply, File, State}.

handle_call({upload_file,FileName}, _From, State = #files_server_state{}) ->
  {reply, database_logic:upload_file(FileName), State}.

handle_call({delete_file,FileName}, _From, State = #files_server_state{}) ->
  {reply, database_logic:delete_file(FileName), State}.

%%% =======================

%% @private
%% @doc Handling cast messages
-spec(handle_cast(Request :: term(), State :: #files_server_state{}) ->
  {noreply, NewState :: #files_server_state{}} |
  {noreply, NewState :: #files_server_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #files_server_state{}}).
handle_cast(_Request, State = #files_server_state{}) ->
  {noreply, State}.

%% @private
%% @doc Handling all non call/cast messages
-spec(handle_info(Info :: timeout() | term(), State :: #files_server_state{}) ->
  {noreply, NewState :: #files_server_state{}} |
  {noreply, NewState :: #files_server_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #files_server_state{}}).
handle_info(_Info, State = #files_server_state{}) ->
  {noreply, State}.

%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #files_server_state{}) -> term()).
terminate(_Reason, _State = #files_server_state{}) ->
  ok.

%% @private
%% @doc Convert process state when code is changed
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #files_server_state{},
    Extra :: term()) ->
  {ok, NewState :: #files_server_state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State = #files_server_state{}, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
