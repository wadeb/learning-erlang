%% Module that defines a binary tree data structure and functions
%% that operate on them.
%% Wade Bonkowski - 03/19/2017

-module(trees).
-export([empty/0, insert/3, lookup/2, has_value/2, has_value1/2]).

empty() -> {node, 'nil'}.


%% Function to recursively insert new data into the tree.
%% Interface is insert(Key, Value, Tree).
insert(Key, Value, {node, 'nil'}) ->
	{node, {Key, Value, {node, 'nil'}, {node, 'nil'}}};
insert(NewKey, NewValue, {node, {Key, Value, Smaller, Larger}})
when NewKey < Key ->
	{node, {Key, Value, insert(NewKey, NewValue, Smaller), Larger}};
insert(NewKey, NewValue, {node, {Key, Value, Smaller, Larger}})
when NewKey > Key ->
	{node, {Key, Value, Smaller, insert(NewKey, NewValue, Larger)}};
insert(Key, Value, {node, {Key, _, Smaller, Larger}}) ->
	{node, {Key, Value, Smaller, Larger}}.


%% Function to recursively lookup data from the tree by key.
%% Interface is lookup(Key, Tree).
lookup(_, {node, 'nil'}) ->
	undefined;
lookup(Key, {node, {Key, Value, _, _}}) ->
	{ok, Value};
lookup(Key, {node, {NodeKey, _, Smaller, _}}) when Key < NodeKey ->
	lookup(Key, Smaller);
lookup(Key, {node, {_, _, _, Larger}}) ->
	lookup(Key, Larger).

%% Showcasing the actual use of exceptions.
%% Looks for a given value 'Val' in the tree.
has_value(Val, Tree) ->
	try has_value1(Val, Tree) of 
		false -> false
	catch
		true -> true
	end.

%% Helper function that throws when the result is found.
has_value1(_, {node, 'nil'}) ->
	false;
has_value1(Val, {node, {_, Val, _, _}}) ->
	throw(true);
has_value1(Val, {node, {_, _, Left, Right}}) ->
	has_value1(Val, Left),
	has_value1(Val, Right).
