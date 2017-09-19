%% 
%% Module that contains user-defined tools for multiprocessing.
%% Wade Bonkowski - 09/19/2017
%%


-module(multiproc).
-compile(export_all).

%% Implementation of the sleep function.
sleep(T) ->
	receive
	after T -> ok
	end.

flush() ->
	receive
		_ -> flush()
	after 0 ->
		ok
	end.

%% Function that receives important messages,
%% than normal messages after.
important() ->
	receive
		{Priority, Message} when Priority > 10 ->
			[Message | important()]
	after 0 ->
		normal()
	end.

%% Function that receives normal messages.
normal() ->
	receive
		{_, Message} ->
			[Message | normal()]
	after 0 ->
		[]
	end.
