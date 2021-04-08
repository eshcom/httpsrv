-module(httpsrv).
-export([start/0, stop/0]).

start() ->
	application:start(httpsrv).

stop() ->
	application:stop(httpsrv).
