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

gv_write(T) ->
	ParseTree = parser:parseQuery(T),
	gv_traverse(ParseTree).

gv_traverse(ParseTree) ->
	sellist_traverse(maps:get(select_list, ParseTree)),
	{ok}
.	

sellist_traverse([]) ->
	{ok};

sellist_traverse([H|T]) ->
	io:fwrite("here is select list: ~n~p~n", [ H ] ),
	sellist_traverse(T),
	{ok}.

process_query([$\\|[Q|T]]) ->
	%%meta command. 
	case Q of
		$q -> ok;
		$g -> gv_write(T);
		_Else -> false 
	end;

process_query(_X) ->
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
	
	
	

