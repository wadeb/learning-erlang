%% Module with functions written when learning about higher order functions
%% with Erlang.
%% Wade Bonkowski - 04/3/2017

-module(hhfuns).
-compile(export_all).

one() -> 1.
two() -> 2.

add(X, Y) -> X() + Y().

increment([]) -> [];
increment([H|T]) -> [H + 1|increment(T)].

decrement([]) -> [];
decrement([H|T]) -> [H - 1|decrement(T)].

%% Factor out the common pieces of the function above and make the
%% operator a function parameter.
map(_, []) -> [];
map(F, [H|T]) -> [F(H)|map(F, T)].

%% Functions to be used as parameters for map().
incr(X) -> X + 1.
decr(X) -> X - 1.

%% Messing around with lambdas.
base(A) ->
	B = A + 1,
	F = fun() -> A * B end,
	F().

%% Working with closures.
a() ->
	Secret = "pony",
	fun() -> Secret end.

b(F) ->
	"a/0's password is " ++ F().

%% Seeing what "Shadowing" is.
another_base() ->
	A = 1,
	(fun(A) -> A = 2 end)(2).

%% Only keep even numbers from a list.
even(L) -> lists:reverse(even(L, [])).

even([], Acc) -> Acc;
even([H|T], Acc) when H rem 2 == 0 ->
	even(T, [H|Acc]);
even([_|T], Acc) ->
	even(T, Acc).

%% This is weird function to write...
%% Only keep men that are older than 60.
old_men(L) -> lists:reverse(old_men(L, [])).

old_men([], Acc) -> Acc;
old_men([Person = {male, Age}|People], Acc) when Age > 60 ->
	old_men(People, [Person|Acc]);
old_men([_|People], Acc) ->
	old_men(People, Acc).

%% Now parameterize the prior two functions by making them take functions.
filter(Pred, L) -> lists:reverse(filter(Pred, L, [])).

filter(_, [], Acc) -> Acc;
filter(Pred, [H|T], Acc) ->
	case Pred(H) of
		true -> filter(Pred, T, [H|Acc]);
		false -> filter(Pred, T, Acc)
	end.

%% Fold function that works as general list reduction.
fold(_, Start, []) -> Start;
fold(F, Start, [H|T]) -> fold(F, F(H, Start), T).

%% Implementing some other functions as fold.
reverse(L) ->
	fold(fun(X, Acc) -> [X|Acc] end, [], L).

map2(F, L) ->
	reverse(fold(fun(X, Acc) -> [F(X)|Acc] end, [], L)).

filter2(Pred, L) ->
	F = fun(X, Acc) ->
		case Pred(X) of
			true -> [X|Acc];
			false -> Acc
		end
	end,
	reverse(fold(F, [], L)).
