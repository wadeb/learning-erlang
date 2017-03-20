%% Module that defines a binary tree data structure and functions
%% that operate on them.
%% Wade Bonkowski - 03/19/2017

-module(trees).
-export([empty/0, insert/3, lookup/2]).

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
