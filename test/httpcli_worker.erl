-module(httpcli_worker).
-export([start_link/3]).

-import(httpcli_utils, [cast2int/1, cast2str/1]).
-import(httpcli_dispatch, [set_cliresult/2, cli_completed/1]).

-record(clistate, {dispatch_pid, query_str,
				   rq_cnt, run_rq_cnt = 0}).

start_link(DispatchPid, Query, RqCnt) ->
	WrkPid = spawn_link(fun() ->
							ok = request(#clistate{dispatch_pid = DispatchPid,
												   query_str = Query,
												   rq_cnt = RqCnt})
						end),
	{ok, WrkPid}.

%callback functions
request(State) ->
	request_loop(State, 0).

request_loop(S = #clistate{dispatch_pid = DispatchPid,
						   query_str = Query,
						   rq_cnt = RqCnt},
			 RunRqCnt) ->
	case RunRqCnt < RqCnt of
		true ->
			Result = case httpc:request(get, {Query, []},
										[{connect_timeout, 10000},
										 {timeout, 10000}],
										[{body_format, binary}]) of
						{ok, {{_, HttpCode, _HttpMsg}, _Headers, _HttpBody}} ->
							HttpCode;
						{error, _Error} ->
							%~ io:format("Request error = ~p~n", [_Error]),
							520
					 end,
			set_cliresult(DispatchPid, Result),
			request_loop(S, RunRqCnt + 1);
		false ->
			cli_completed(DispatchPid),
			ok
	end.
