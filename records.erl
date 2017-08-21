-module(records).
-include("records.hrl").
-compile(export_all).

-record(robot, {name,
		type=industrial,
		hobbies,
		details=[]}).

-record(user, {id, name, group, age}).

%% Creates a robot record.
first_robot() ->
	#robot{name="Mechatron",
		type=handmade,
		details=["Moved by a small man inside"]}.

%% Returns a robot that has its hobbies initialized.
car_factory(CorpName) ->
	#robot{name=CorpName, hobbies="building cars"}.

%% Use pattern matching to filter.
admin_panel(#user{name=Name, group=admin}) ->
	Name ++ " is allowed!";
admin_panel(#user{name=Name}) ->
	Name ++ " is not allowed".

%% Can extend user without problem.
adult_section(U = #user{}) when U#user.age > 18 ->
	%% Show stuff that can't be written in such a text.
	allowed;
adult_section(_) ->
	%% Redirect to sesame street site
	forbidden.

%% Update the details of the robot record.
repairman(Rob) ->
	Details = Rob#robot.details,
	NewRob = Rob#robot{details=["Repaired by repairman" | Details]},
	{repaired, NewRob}.

%% Use an included record.
included() -> #included{some_field="Some value"}.
