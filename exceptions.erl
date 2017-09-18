%% Module for practicing with Erlang exceptions.
%% Wade Bonkowski - 07/18/2017

-module(exceptions).
-compile(export_all).

%% Throw and catch some throws
throws(F) ->
	try F() of
		_ -> ok
	catch
		Throw -> {throw, caught, Throw}
	end.

%% Throw and catch some errors.
errors(F) ->
	try F() of
		_ -> ok
	catch
		error:Error -> {error, caught, Error}
	end.

%% Throw and catch some exits.
exits(F) ->
	try F() of
		_ -> ok
	catch
		exit:Exit -> {exit, caught, Exit}
	end.

%% Function to enumerate all possible errors that we want to catch.
sword(1) -> throw(slice);
sword(2) -> error(cut_arm);
sword(3) -> exit(cut_leg);
sword(4) -> throw(punch);
sword(5) -> exit(cross_bridge).

%% Monty Python function to handle all error types.
black_knight(Attack) when is_function(Attack, 0) ->
	try Attack() of
		_ -> "None shall pass."
	catch
		throw:slice -> "It is but a scratch.";
		error:cut_arm -> "I've had worse.";
		exit:cut_leg -> "Come on you pansy!";
		_:_ -> "Just a flesh wound."
	end.

%% Simple funtion that doesn't crash.
talk() -> "blah blah".

%% Testing out a try/catch with multiple statements of execution.
whoa() ->
	try
		talk(),
		_Knight = "None shall pass!",
		_Doubles = [N*2 || N <- lists:seq(1, 100)],
		throw(up),
		_WillReturnThis = tequila
	of
		tequila -> "Hey, this worked!"
	catch
		Exception:Reason -> {caught, Exception, Reason}
	end.

%% Try and catch without an "of" clause.
im_impressed() ->
	try
		talk(),
		_Knight = "None shall pass!",
		_Doubles = [N*2 || N <- lists:seq(1, 100)],
		throw(up),
		_WillReturnThis = tequila
	catch
		Exception:Reason -> {caught, Exception, Reason}
	end.

%% Common way of using catch keyword. 
catcher(X, Y) ->
	case catch X/Y of
		{'EXIT', {badarith, _}} -> "uh oh";
		N -> N
	end.

%% Showcasing how the catch keyword can be bad.
one_or_two(1) -> return;
one_or_two(2) -> throw(return).
