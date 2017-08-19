%% Module that contains a process for finding the shortest path
%% from the Heathrow airport to London.
%% Wade Bonkowski - 08/03/2017

-module(road).
-compile(export_all).

%% Entry point for the program.
main(FileName) ->
	{ok, Binary} = file:read_file(FileName),
	Map = parse_map(Binary),
	io:format("~p~n", [optimal_path(Map)]),
	erlang:halt().

%% Parse the input map into a list of integers.
parse_map(Bin) when is_binary(Bin) ->
	parse_map(binary_to_list(Bin));
parse_map(Str) when is_list(Str) ->
	Values = [list_to_integer(X) || X <- string:tokens(Str, "\r\n\t ")],
	group_vals(Values, []).

%% Group the values into triples.
group_vals([], Acc) ->
	lists:reverse(Acc);
group_vals([A, B, X | Rest], Acc) ->
	group_vals(Rest, [{A ,B, X} | Acc]).

%% Find the next shortest step given the already taken paths and
%% distance, as well as the next paths.
shortest_step({A, B, X}, {{DistA, PathA}, {DistB, PathB}}) ->
	OptA1 = {DistA + A, [{a, A} | PathA]},
	OptA2 = {DistB + B + X, [{x, X}, {b, B} | PathB]},
	OptB1 = {DistA + A + X, [{x, X}, {a, A} | PathA]},
	OptB2 = {DistB + B, [{b, B} | PathB]},
	{erlang:min(OptA1, OptA2), erlang:min(OptB1, OptB2)}.

%% Finds the optimal path given a valid map.
optimal_path(Map) ->
	{A, B} = lists:foldl(fun shortest_step/2, {{0, []}, {0, []}}, Map),
	{_Dist, Path} = if hd(element(2, A)) =/= {x, 0} -> A;
				hd(element(2, B)) =/= {x, 0} -> B
			end,
	lists:reverse(Path).
