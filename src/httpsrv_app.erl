-module(httpsrv_app).
-behaviour(application).
-export([start/2, stop/1]).

start(normal, _Args) ->
	httpsrv_appsup:start_link().

stop(_State) ->
	ok.
