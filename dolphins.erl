%%
%% Module that showcases conccurency primitives using Dolphins.
%% Wade Bonkowski - 09/17/2017
%%


-module(dolphins).
-compile(export_all).

%% First function that showcases `receive`,
dolphin1() ->
	receive
		do_a_flip ->
			io:format("How about no?~n");
		fish ->
			io:format("So long and thanks for all the fish!~n");
		_ ->
			io:format("Heh, we're smarter than you humans.~n")
	end.

%% Function that receives from another process, and then responds to it.
dolphin2() ->
	receive
		{From, do_a_flip} ->
			From ! "How about no?";
		{From, fish} ->
			From ! "So long and thanks for all the fish!";
		_ ->
			io:format("Heh, we're smarter than you humans.~n")
	end.

%% Dolphin function that continues to receive and send messages after spanwing.
dolphin3() ->
	receive
		{From, do_a_flip} ->
			From ! "How about no?",
			dolphin3();
		{From, fish} ->
			From ! "So long and thanks for all the fish!";
		_ ->
			io:format("Heh, we're smarter than you humans.~n"),
			dolphin3()
	end.
