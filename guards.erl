%% Module that explores using guard clauses
%% Wade Bonkowski - 03/14/2017

-module(guards).
-export([old_enough/1, right_age/1, wrong_age/1, atom_or_int/1]).

%% Some functions testing and returns bools about driving age.
old_enough(X) when X >= 16 -> true;
old_enough(_) -> false.

right_age(X) when X >= 16, X =< 104 ->
	true;
right_age(_) ->
	false.

wrong_age(X) when X < 16; X > 104 ->
	true;
wrong_age(_) ->
	false.

%% Messing with using BIFs in guards.
atom_or_int(X) when is_integer(X) or is_atom(X) ->
	true;
atom_or_int(_) ->
	false.
