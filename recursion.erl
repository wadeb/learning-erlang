%% Module for practicing basic recursion in Erlang
%% Wade Bonkowski - 03/17/2017

-module(recursion).
-export(
	[
		fac/1,
		len/1,
		tail_fac/1,
		tail_len/1,
		duplicate/2,
		tail_duplicate/2,
		reverse/1,
		tail_reverse/1,
		sublist/2,
		tail_sublist/2,
		zip/2,
		lenient_zip/2,
		tail_zip/2,
		quicksort/1,
		lc_quicksort/1
	]
).

fac(0) -> 1;
fac(N) when N > 0 -> N * fac(N - 1).

%% Recursively determine the length of a list.
len([]) -> 0;
len([_]) -> 1;
len([_|T]) -> 1 + len(T).

%% First tail recursive function written in Erlang.
tail_fac(N) -> tail_fac(N, 1).

tail_fac(0, Acc) -> Acc;
tail_fac(N, Acc) when N > 0 -> tail_fac(N-1, N*Acc).

%% Recursive version of the len() function.
tail_len(List) -> tail_len(List, 0).

tail_len([], Acc) -> Acc;
tail_len([_|T], Acc) -> tail_len(T, Acc+1).

%% Recursive generation.
duplicate(0, _) ->
	[];
duplicate(N, Term) when N > 0 ->
	[Term|duplicate(N-1, Term)].

%% Tail recursive version of the generation.
tail_duplicate(N, Term) ->
	tail_duplicate(N, Term, []).

tail_duplicate(0, _, List) ->
	List;
tail_duplicate(N, Term, List) when N > 0 ->
	tail_duplicate(N-1, Term, [Term|List]).

%% Recursive function to reverse list.
reverse([]) ->
	[];
reverse([H|T]) ->
	reverse(T) ++ [H].

%% Tail recursive reverse function.
tail_reverse(List) ->
	tail_reverse(List, []).

tail_reverse([], ReverseList) ->
	ReverseList;
tail_reverse([H|T], ReverseList) ->
	tail_reverse(T, [H|ReverseList]).

%% Recursive sublist function.
sublist(_, 0) ->
	[];
sublist([], _) ->
	[];
sublist([H|T], N) when N > 0 ->
	[H|sublist(T, N-1)].

%% Tail recursive version of the sublist.
tail_sublist(List, N) ->
	%% Reversed because the tail recursion reverses the sublist.
	tail_reverse(tail_sublist(List, N, [])).

tail_sublist(_, 0, SubList) ->
	SubList;
tail_sublist([], _, SubList) ->
	SubList;
tail_sublist([H|T], N, SubList) when N > 0 ->
	tail_sublist(T, N-1, [H|SubList]).

%% Recursive zip function.
zip([], []) ->
	[];
zip([XH|XT], [YH|YT]) ->
	[{XH, YH}|zip(XT, YT)].

%% Recursive function that zips two lists of varying length.
%% Will zip the shortest sublists of the two input lists.
lenient_zip([], _) ->
	[];
lenient_zip(_, []) ->
	[];
lenient_zip([X|Xs], [Y|Ys]) ->
	[{X, Y}|lenient_zip(Xs, Ys)].

%% Tail recursive general purpose zip function. Works like lenient_zip.
tail_zip(XList, YList) -> tail_reverse(tail_zip(XList, YList, [])).

tail_zip([], _, ZipList) -> ZipList;
tail_zip(_, [], ZipList) -> ZipList;
tail_zip([X|Xs], [Y|Ys], ZipList) ->
	tail_zip(Xs, Ys, [{X, Y}|ZipList]).

%% First really interesting recursive function written in Erlang.
quicksort([]) -> [];
quicksort([Pivot|Rest]) ->
	{Smaller, Larger} = partition(Pivot, Rest, [], []),
	quicksort(Smaller) ++ [Pivot] ++ quicksort(Larger).

%% Helper partition function for quicksort.
partition(_, [], Smaller, Larger) -> {Smaller, Larger};
partition(Pivot, [H|T], Smaller, Larger) ->
	if H =< Pivot -> partition(Pivot, T, [H|Smaller], Larger);
		H > Pivot -> partition(Pivot, T, Smaller, [H|Larger])
	end.

%% Quicksort that uses less code and is often seen on Quora.
%% This honestly is easier for me to follow.
%% Has to traverse the list twice though.
lc_quicksort([]) -> [];
lc_quicksort([Pivot|Rest]) ->
	lc_quicksort([Smaller || Smaller <- Rest, Smaller =< Pivot])
	++ [Pivot] ++
	lc_quicksort([Larger || Larger <- Rest, Larger > Pivot]).
