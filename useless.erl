%% First module written when learning Erlang
%% Wade Bonkowski - 03/13/2017

-module(useless).
-export([add/2, hello/0, greet_and_add_two/1, greet/2]).

add(A, B) ->
	A + B.

%% Shows greetings!
%% io:format/1 is the standard function for text output.
hello() ->
	io:format("Hello World!~n").

greet_and_add_two(X) ->
	hello(),
	add(X, 2).

%% Function that contains multiple clauses.
greet(male, Name) ->
	io:format("Hello, Mr. ~s~n", [Name]);
greet(female, Name) ->
	io:format("Hello, Mrs. ~s~n", [Name]);
greet(_, Name) ->
	io:format("Hello, ~s~n", [Name]).
