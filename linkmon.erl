%%
%% Module for learning about process linking.
%% Wade Bonkowski - 12/13/2017
%%

-module(linkmon).
-compile(export_all).

myproc() ->
	timer:sleep(5000),
	exit(reason).

chain(0) ->
	receive
		_ -> ok
	after 2000 ->
		exit("chain dies here")
	end;
chain(N) ->
	Pid = spawn(fun() -> chain(N-1) end),
	link(Pid),
	receive
		_ -> ok
	end.
