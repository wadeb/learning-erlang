%% RPN Calculator.
%% Wade Bonkowski - 07/30/2017

-module(calc).
-export([rpn/1, rpn_test/0]).

%% Entry point and main loop.
rpn(L) when is_list(L) ->
	[Res] = lists:foldl(fun rpn/2, [], string:tokens(L, " ")),
	Res.

%% Main functiont that actually performs the recursion.
%% Can also write sum and prod in terms of rpn()/2 itself.
rpn("+", [N1, N2|S]) -> [N2 + N1|S];
rpn("-", [N1, N2|S]) -> [N2 - N1|S];
rpn("*", [N1, N2|S]) -> [N2 * N1|S];
rpn("/", [N1, N2|S]) -> [N2 / N1|S];
rpn("^", [N1, N2|S]) -> [math:pow(N2, N1)|S];
rpn("ln", [N|S]) -> [math:log(N)|S];
rpn("log10", [N|S]) -> [math:log10(N)|S];
rpn("sum", S) when is_list(S) ->
	[lists:foldl(fun(H, Acc) -> H + Acc end, 0, S)];
rpn("prod", S) when is_list(S) ->
	[lists:foldl(fun(H, Acc) -> H * Acc end, 1, S)];
rpn(X, Stack) -> [read(X)|Stack].

%% Helper function for reading an integer from a string.
read(N) ->
	case string:to_float(N) of
		{error,no_float} -> list_to_integer(N);
		{F,_} -> F
	end.

%% Simple Test code to make sure the program works as expected.
rpn_test() ->
	5 = rpn("2 3 +"),
	87 = rpn("90 3 -"),
	-4 = rpn("10 4 3 + 2 * -"),
	-2.0 = rpn("10 4 3 + 2 * - 2 /"),
	ok = try
		rpn("90 34 12 33 55 66 + * - +")
	catch
		error:{badmatch, [_|_]} -> ok
	end,
	4037 = rpn("90 34 12 33 55 66 + * - + -"),
	8.0 = rpn("2 3 ^"),
	true = math:sqrt(2) == rpn("2 0.5 ^"),
	true = math:log(2.7) == rpn("2.7 ln"),
	true = math:log10(2.7) == rpn("2.7 log10"),
	50 = rpn("10 10 10 20 sum"),
	-42000 = rpn("10 20 30 -7 prod").
