-module(httpsrv_env).
-export([get_dicenv_int/2, get_dicenv_str/2]).

%~ dic/env funcs
get_dicenv_int(Key, Def) ->
	get_dicenv_val(Key, fun() ->
		get_env_int(Key, Def) end).

get_dicenv_str(Key, Def) ->
	get_dicenv_val(Key, fun() ->
		get_env_str(Key, Def) end).

%~ env funcs
get_env_str(Key, Def) ->
	case application:get_env(Key) of
		undefined -> Def;
		{ok, Val} ->
			case check_env_str(Val, true) of
				"" -> Def;
				TrimVal -> TrimVal
			end
	end.

% internal functions
get_dicenv_val(Key, EnvFun) ->
	case get(Key) of
		undefined ->
			EnvVal = EnvFun(),
			put(Key, EnvVal),
			EnvVal;
		DicVal -> 
			DicVal
	end.

get_env_int(Key, Def) ->
	case application:get_env(Key) of
		undefined -> Def;
		{ok, Val} -> check_env_int(Val)
	end.

check_env_str(Val, Trim) when is_list(Val) ->
	case Trim of
		true -> string:trim(Val);
		false -> Val
	end;
check_env_str(Val, _) ->
	Error = lists:flatten(io_lib:format(
				"Environment configuration error: "
				"value ~p is not a string.", [Val])),
	throw({bad_env_param, Error}).

check_env_int(Val) when is_integer(Val) -> Val;
check_env_int(Val) ->
	Error = lists:flatten(io_lib:format(
				"Environment configuration error: "
				"value ~p is not an integer.", [Val])),
	throw({bad_env_param, Error}).
