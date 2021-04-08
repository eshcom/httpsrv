-module(httpsrv_appsup).
-behaviour(supervisor).
-export([start_link/0, init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	MaxRestart = 0,
	MaxTime = 60, % sec
	{ok, {{one_for_one, MaxRestart, MaxTime},
			[{httpsrv_wrksup,
				{httpsrv_wrksup, start_link, [httpsrv_worker]},
				permanent,
				5000, %ms
				supervisor,
				[httpsrv_wrksup]},
			 {httpsrv_filereader,
				{httpsrv_filereader, start_link, []},
				permanent,
				5000, %ms
				worker,
				[httpsrv_filereader]},
			 {httpsrv_acceptor,
				{httpsrv_acceptor, start_link, []},
				permanent,
				5000, %ms
				worker,
				[httpsrv_acceptor]}]}}.
