-module(httpsrv_worker).
-export([start_link/1]).
-import(httpsrv_filereader, [read_file/2]).

-include("httpsrv.hrl").

start_link(ClientSocket) ->
	WrkPid = spawn_link(fun() -> handler(ClientSocket) end),
	{ok, WrkPid}.

%~ internal functions
handler(ClientSocket) ->
	receive
		{tcp, _, Request} ->
			{ok, {http_request, _Method, {abs_path, Query}, _}, _} =
						erlang:decode_packet(http, Request, []),
			%~ io:format("~ts ~ts~n", [_Method, Query]),
			Response = case string:prefix(Query, "/?file=") of
							nomatch -> http_400("File name not specified.");
							FileName ->
								case catch read_file(FileName, ?FILE_READ_TIMEOUT) of
									ok ->
										http_200("File read successfully.");
									{error_open, _} ->
										http_404("File not found.");
									{error_read, _} ->
										http_500("File read error.");
									% timeout generate by read_file/2
									{'EXIT', {timeout, _}} ->
										http_503("File read timeout.")
								end
					   end,
			gen_tcp:send(ClientSocket, Response),
			handler(ClientSocket);
		{tcp_closed, _} ->
			%~ io:format("Client closed the socket.~n", []),
			ok;
		Unknown ->
			io:format("Unknown message to srvworker: ~p~n", [Unknown]),
			handler(ClientSocket)
	after ?WAIT_TIMEOUT ->
		ok
	end.

http_200(TextBody) ->
	http_response("200 OK", TextBody).
http_400(TextBody) ->
	http_response("400 Bad Request", TextBody).
http_404(TextBody) ->
	http_response("404 Not Found", TextBody).
http_500(TextBody) ->
	http_response("500 Internal Server Error", TextBody).
http_503(TextBody) ->
	http_response("503 overloaded", TextBody).

http_response(Status, TextBody) ->
	BinBody = unicode:characters_to_binary(TextBody),
	io_lib:format(lists:concat(["HTTP/1.0 ", Status, "\n",
								"Content-Type: text/html; charset=utf-8\n",
								"Content-Length: ~p\n\n~s"]),
				  [size(BinBody), BinBody]).
