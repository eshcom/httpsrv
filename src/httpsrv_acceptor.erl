-module(httpsrv_acceptor).
-export([start_link/0]).

-include("httpsrv.hrl").

start_link() ->
	AcceptorPid = spawn_link(fun() ->
								{ok, ListenSocket} = gen_tcp:listen(?SERVER_PORT,
																	?SOCKET_OPTIONS),
								io:format("Started listening socket on port ~p~n",
										  [?SERVER_PORT]),
								acceptor_loop(ListenSocket)
							 end),
	{ok, AcceptorPid}.

%~ internal functions
acceptor_loop(ListenSocket) ->
	case gen_tcp:accept(ListenSocket) of
		{ok, ClientSocket} ->
			{ok, WrkPid} = supervisor:start_child(httpsrv_wrksup,
												  [ClientSocket]),
			gen_tcp:controlling_process(ClientSocket, WrkPid);
		{error, Reason} ->
			io:format("Listen socket acception error: ~p~n", [Reason])
	end,
	acceptor_loop(ListenSocket).
