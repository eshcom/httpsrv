-module(httpcli_dispatch).
-behaviour(gen_server).
-export([start/1, set_cliresult/2, cli_completed/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
		 code_change/3, terminate/2]).

-import(httpcli_utils, [cast2int/1, cast2str/1]).

-record(dispstate, {query_str, cli_cnt,
					complete_cli_cnt = 0,
					rq_cnt, cli_result}).

-record(cliresult, {http_200 = 0, http_400 = 0,
					http_404 = 0, http_500 = 0,
					http_503 = 0, http_520 = 0}).

start([ServerHost, ServerPort, CliCnt, RqCnt, FileName]) ->
	Query = lists:concat(["http://", cast2str(ServerHost), ":", ServerPort,
						  "/?file=", cast2str(FileName)]),
	io:format("Client query: ~ts~n", [Query]),
	gen_server:start_link(?MODULE, {Query,
									cast2int(CliCnt),
									cast2int(RqCnt)}, []).

set_cliresult(DispatchPid, Result) ->
	gen_server:cast(DispatchPid, {set_cliresult, Result}).

cli_completed(DispatchPid) ->
	gen_server:cast(DispatchPid, cli_completed).

%callback functions
init({Query, CliCnt, RqCnt}) ->
	%~ {ok, _} = application:ensure_all_started(httpsrv),
	{ok, _} = application:ensure_all_started(inets),
	self() ! run,
	{ok, #dispstate{query_str = Query,
					cli_cnt = CliCnt,
					rq_cnt = RqCnt,
					cli_result = #cliresult{}}}.

handle_info(run, State) ->
	ok = run_cli(State),
	{noreply, State};

handle_info(Msg, State) ->
	io:format("Unknown message to clidispatch: ~p~n", [Msg]),
	{noreply, State}.

handle_call(_Msg, _From, State) ->
	{noreply, State}.

handle_cast({set_cliresult, Result},
			S = #dispstate{cli_result = CliResult = 
								#cliresult{http_200 = Http200,
										   http_400 = Http400,
										   http_404 = Http404,
										   http_500 = Http500,
										   http_503 = Http503,
										   http_520 = Http520}}) ->
	NewCliResult = case Result of
						200 -> CliResult#cliresult{http_200 = Http200 + 1};
						400 -> CliResult#cliresult{http_400 = Http400 + 1};
						404 -> CliResult#cliresult{http_404 = Http404 + 1};
						500 -> CliResult#cliresult{http_500 = Http500 + 1};
						503 -> CliResult#cliresult{http_503 = Http503 + 1};
						520 -> CliResult#cliresult{http_520 = Http520 + 1}
				   end,
	{noreply, S#dispstate{cli_result = NewCliResult}};

handle_cast(cli_completed,
			S = #dispstate{cli_cnt = CliCnt,
						   complete_cli_cnt = CompleteCliCnt,
						   cli_result = #cliresult{http_200 = Http200,
												   http_400 = Http400,
												   http_404 = Http404,
												   http_500 = Http500,
												   http_503 = Http503,
												   http_520 = Http520}}) ->
	NewCompleteCliCnt = CompleteCliCnt + 1,
	case NewCompleteCliCnt < CliCnt of
		true ->
			{noreply, S#dispstate{complete_cli_cnt = NewCompleteCliCnt}};
		false ->
			io:format("Http200: ~p~nHttp400: ~p~nHttp404: ~p~n"
					  "Http500: ~p~nHttp503: ~p~nHttp520: ~p~n",
					  [Http200, Http400, Http404, Http500, Http503, Http520]),
			{stop, normal, S}
	end;

handle_cast(_Msg, State) ->
	{noreply, State}.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

terminate(_Reason, _State) ->
	%~ httpsrv:stop(),
	init:stop().

% internal functions
run_cli(State) ->
	run_cli_loop(State, 0).

run_cli_loop(S = #dispstate{query_str = Query,
							cli_cnt = CliCnt,
							rq_cnt = RqCnt},
			 RunCliCnt) ->
	case RunCliCnt < CliCnt of
		true ->
			{ok, _} = httpcli_worker:start_link(self(), Query, RqCnt),
			%~ io:format("Client running...~n", []),
			run_cli_loop(S, RunCliCnt + 1);
		false -> ok
	end.
