%% Module for exploration of pattern matching with functions.
%% Wade Bonkowski - 03/13/2017

-module(functions).
-export([head/1, second/1, same/2, valid_time/1]).

%% Returnt the head of a list.
head([H|_]) -> H.

%% Return the second element of the list.
second([_, X | _]) -> X.

%% Return true if both of the variables passed to a function
%% are the same.
%% Return false otherwise.
same(X, X) ->
	true;
same(_, _) ->
	false.

%% Print a date only if it is correctly formatted.
%% Not properly checking input though.
valid_time({Date = {Y, M, D}, Time = {H, Min, S}}) ->
	io:format(
		"The Date tuple (~p) says today is: ~p/~p/~p,~n",
		[Date, Y, M, D]
	),
	io:format(
		"The time tuple (~p) indicates: ~p:~p:~p,~n",
		[Time, H, Min, S]
	);
valid_time(_) ->
	io:format("Stop feeding me wrong data!~n").
