-module(httpsrv_wrksup).
-behaviour(supervisor).
-export([start_link/1, init/1]).

start_link(WrkModule) ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, WrkModule).

init(WrkModule) ->
	MaxRestart = 0,
	MaxTime = 10, % sec
	{ok, {{simple_one_for_one, MaxRestart, MaxTime},
			[{WrkModule,
				{WrkModule, start_link, []},
				temporary,
				4000, % ms
				worker,
				[WrkModule]}]}}.

