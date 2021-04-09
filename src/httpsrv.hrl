-import(httpsrv_env, [get_dicenv_int/2, get_dicenv_str/2]).

%~ macros
-define(SERVER_PORT, get_dicenv_int(server_port, 9000)).
-define(FILE_PART_SIZE, get_dicenv_int(file_part_size, 1000)).
-define(FILE_READ_TIMEOUT, get_dicenv_int(file_read_timeout, 100)).

-define(SOCKET_OPTIONS, [{reuseaddr, true},
						 {active, true},
						 {mode, binary}]).

-define(WAIT_TIMEOUT, 4000).

%~ records
-record(frstate, {data_path}).
