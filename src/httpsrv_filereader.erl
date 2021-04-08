-module(httpsrv_filereader).
-behaviour(gen_server).
-export([start_link/0, read_file/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
		 code_change/3, terminate/2]).

-include("httpsrv.hrl").

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

read_file(FilePath, Timeout) ->
	gen_server:call(?MODULE, {read_file, FilePath}, Timeout).

% callback functions
init([]) ->
	{ok, {}}.

handle_call({read_file, FilePath}, _From, State) ->
	{reply, read_file(FilePath), State};

handle_call(_Msg, _From, State) ->
	{noreply, State}.

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(Msg, State) ->
	io:format("Unknown message to filereader: ~p~n", [Msg]),
	{noreply, State}.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

terminate(_Reason, _State) ->
	ok.

% internal functions
read_file(FilePath) ->
	case file:open(FilePath, [binary]) of
		{ok, IoFile} ->
			read_file_loop(IoFile);
		{error, Reason} ->
			{error_open, Reason}
	end.

read_file_loop(IoFile) ->
	case file:read(IoFile, ?FILE_PART_SIZE) of
		eof -> ok;
		{ok, _Data} ->
			read_file_loop(IoFile);
		{error, Reason} ->
			{error_read, Reason}
	end.
