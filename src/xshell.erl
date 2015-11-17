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

%%trim_querystring(Qstring) ->
%%
%%.

process_query([$\\|[Q|_]]) ->
	%%meta command. 
	case Q of
		$q -> ok;
		_Else -> false 
	end;

process_query(_X) ->
	io:fwrite("No slash~n"),
	ParseTree = parser:parseQuery(_X),
	io:fwrite("parsetree:~n~p~n", [ParseTree]),
	Plan = planner:plan_query(ParseTree),		%%this will change to message to planner gen_server process 
	io:fwrite("plan:~n~p~n", [Plan]),
	do_repl()	
	.

do_repl() ->
	QueryText = [ Q || Q <- io:get_line("xshell> "), Q /= $\n ],
	
	process_query(QueryText).
	%% plan parse tree
	
	
	

