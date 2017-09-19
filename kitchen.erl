%%
%% Module that contains a function for a process to operate like
%% a refrigerator with add and removal operations.
%% Wade Bonkowski - 09/19/2017
%%


-module(kitchen).
-compile(export_all).

%% Function that allows a process to act as a refrigerator.
%% A first version that doesn't actually store anything..
fridge1() ->
	receive
		{From, {store, _Food}} ->
			From ! {self(), ok},
			fridge1();
		{From, {take, _Food}} ->
			%% Uh...
			From ! {self(), not_found},
			fridge1();
		terminate ->
			ok
	end.

%% Second version of the fridge process function that maintains
%% refrigerator state as a list passed as a parameter.
fridge2(FoodList) ->
	receive
		{From, {store, Food}} ->
			From ! {self(), ok},
			fridge2([Food | FoodList]);
		{From, {take, Food}} ->
			case lists:member(Food, FoodList) of
				true ->
					From ! {self(), {ok, Food}},
					fridge2(lists:delete(Food, FoodList));
				false ->
					From ! {self(), not_found},
					fridge2(FoodList)
			end;
		terminate ->
			ok
	end.

%% Store function that acts as an abstraction around telling a refrigerator
%% process to store food.
store(Pid, Food) ->
	Pid ! {self(), {store, Food}},
	receive
		{Pid, Msg} -> Msg
	end.

%% Take function that acts as an abstraction around telling a refrigerator
%% process to take food from it.
take(Pid, Food) ->
	Pid ! {self(), {take, Food}},
	receive
		{Pid, Msg} -> Msg
	end.

%% Start function to abstract away spawning of the process.
start(FoodList) ->
	spawn(?MODULE, fridge2, [FoodList]).

%% Store function that stops waiting for a response after some amount of time.
store2(Pid, Food) ->
	Pid ! {self(), {store, Food}},
	receive
		{Pid, Msg} -> Msg
	after 3000 ->
		timeout
	end.

%% Take function that stops waiting for a response after some amount of time.
take2(Pid, Food) ->
	Pid ! {self(), {take, Food}},
	receive
		{Pid, Msg} -> Msg
	after 3000 ->
		timeout
	end.
