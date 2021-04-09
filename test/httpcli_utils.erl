-module(httpcli_utils).
-export([cast2int/1, cast2str/1, cast2bin/1, check_int/2]).

cast2int(Val) when is_integer(Val) ->
	Val;
cast2int(Val) when is_list(Val) ->
	list_to_integer(Val);
cast2int(Val) when is_atom(Val) ->
	cast2int(atom_to_list(Val)).

cast2str(Val) when is_list(Val) ->
	Val;
cast2str(Val) when is_atom(Val) ->
	atom_to_list(Val);
cast2str(Val) when is_binary(Val) ->
	unicode:characters_to_list(Val).

cast2bin(Val) when is_binary(Val) ->
	Val;
cast2bin(Val) when is_list(Val) ->
	unicode:characters_to_binary(Val).

check_int(Val, _) when is_integer(Val)-> Val;
check_int(_, Def) -> Def.
