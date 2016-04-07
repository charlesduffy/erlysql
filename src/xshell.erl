-module(xshell).
-export([start/0]).

start() ->
	io:fwrite("xshell test SQL client~n"),
	do_repl(),
	io:fwrite("Exiting xshell~n"),
	ok.


%%initial version
%% --planner / parser as modules, not gen_server processes. This will change.
%% --everything spawned / run from xshell environment. 


%% ===================================================

spawn_pipeline(CS) ->
	spawn_pipeline(CS, [])
.

spawn_pipeline([Childspec|List], Acc) ->
	Acc2 = supervisor:start_child( xndbmicro_sup , Childspec),
	spawn_pipeline(List, [ Acc ] ++ Acc2)
;

spawn_pipeline([], Acc) ->
	io:fwrite("return from spawn: ~p~n" , [ Acc ]),
	Acc
.	

%% ===================================================
process_query([$\\|[Q|T]]) ->
	%%meta command. 
	case Q of
		$q ->    ok;
		$g ->    visualiser:gv_write(T,"parsetree.dot"),
			 do_repl();
		$x ->    visualiser:gv_explain_plan(T,"explainplan.dot"),
			 do_repl();
		_Else -> false 
	end;

process_query(_X) ->
	ParseTree = parser:parseQuery(_X),
	io:fwrite("parsetree:~n~p~n", [ParseTree]),
	Program = planner:plan_query(ParseTree),		%%this will change to message to planner gen_server process 
	io:fwrite("program:~n~p~n", [Program]),
	CS = despatch:generate_childspec(Program),		%%for present generates a list of child specs for sequential launch
	io:fwrite("~n^^^^^^^^^^^^^^^^^^~n childspec:~n~p~n", [CS]),
%%	Check = supervisor:check_childspecs(CS),
%%	io:fwrite("~n^^^^^^^^^^^^^^^^^^~n check output :~n~p~n", [Check]),
%%	spawn_pipeline(CS),
	do_repl()	
	.

do_repl() ->
	QueryText = [ Q || Q <- io:get_line("xshell> "), Q /= $\n ],
	
	process_query(QueryText).
	%% plan parse tree
	
	
	

