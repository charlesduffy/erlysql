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

%% @doc write out a Graphviz dot file of the entire parse tree
%% @todo move the Graphviz stuff into a seperate module

gv_write(QueryText, OutFile) ->
	ParseTree = parser:parseQuery(QueryText),
	DotText = gv_traverse(ParseTree, QueryText),
	file:write_file(OutFile, DotText ),
	io:fwrite([DotText]),
	{ok}
.
	
%% @doc traverse the parse tree, return the dot text to write out

gv_traverse(ParseTree, QueryText) ->
	%% Obtain dot file header
	gv_gen_header(QueryText) ++
	%% Traverse select list
	sellist_traverse(maps:get(select_list, ParseTree)) ++
	whereclause_traverse(maps:get(where_clause, ParseTree)) ++ 
	"
	}
	"
.	

%% @doc prints dotfile header text

gv_gen_header(QueryText) ->
	"graph \"clusterparsetree\" {
		node [ fontsize=12 ];
		graph [ fontsize=10 ];
		label = \""++ QueryText ++ "\"
"
.

%% @doc Traverse where clause and print

whereclause_traverse(T) -> 
	L = proc_sexpr(T , { 0 , 0 , [] , [] } ),
	write_whereclause_accs(L)
.

%% @doc Write out where clause dot code
write_whereclause_accs(L) ->
	{ _ , _ , Nodes , Links } = L,
	"
	subgraph clusterwhereclause 
	{
		label = \"Where Clause\";
		color = \"green\"
	" ++
		print_nodes(Nodes, 0) ++
		print_links(Links, 0) ++
	"
		}
	"	
.

%% @doc traverse select list and process each s-expression in it
%% Each sexpr is traversed, and each node pushes a graphviz
%% node definition string and a graphviz link string to 
%% two accumulators (Nacc and Lacc). 
%% 
%% Accumulator is of the form:
%% 
%% { select list ID , sexpr Node ID, Node Accumulator, Link Accumulator }

sellist_traverse(T) -> sellist_traverse(T, 1, "").

sellist_traverse([H|T], Sl_id , Acc ) ->
	L = proc_sexpr(H , { Sl_id , 0 , [] , [] } ),
	Text = write_sellist_accs(L, Sl_id),
	sellist_traverse(T , Sl_id + 1, Acc ++ Text )
;

sellist_traverse([], _ , TextAcc ) -> TextAcc.

write_sellist_accs(L, Sl_id) ->
	{ _ , _ , Nodes , Links } = L,
	SL = integer_to_list(Sl_id),
	"
	subgraph clustersellist_" ++ SL ++ " { 
		label=\"select list item " ++ SL ++ "\"
		color=\"blue\" 
	" ++
		print_nodes(Nodes, Sl_id) ++ 
		print_links(Links, Sl_id) ++ 
	"
		}
	"
.

print_links(L, Sl_id) -> print_links(L, Sl_id, "", 0).

print_links([H|T], Sl_id, TextAcc, LinkIdAcc) ->
	{ Nfrom , Nto } = H , 
	Text = lists:flatten(io_lib:format("\"sexpr~p_~p\" -- \"sexpr~p_~p\" [ id = ~p ];~n", [ Sl_id, Nfrom, Sl_id, Nto, LinkIdAcc ])),
	print_links(T, Sl_id, string:concat(Text, TextAcc), LinkIdAcc + 1);

print_links([] , _, TextAcc, _) -> TextAcc.

print_nodes(L, Sl_id ) -> print_nodes(L, Sl_id , "").

print_nodes([H|T], Sl_id , TextAcc) ->
	{ Nid , Nlabel } = H , 
	NLabelArg = 
		case is_list(Nlabel) of
		true -> "~p";
		false -> "\"~p\""
	end,
	Text = lists:flatten(
		io_lib:format("\"sexpr~p_~p\" [ label = " ++ NLabelArg ++ "];~n", 
			[ Sl_id, Nid, Nlabel ]
				)
		),
	print_nodes(T, Sl_id, string:concat(Text, TextAcc));

print_nodes([] , _ , TextAcc) -> TextAcc.

%% @doc recursive sexpr processor functions. Traverse the structure
%% while pushing nodes on to a Node accumulator, and links on to a Link accumulator.
%% Returns both accumulators for printing the Graphviz dot output. 
%% @todo some leaf nodes are difficult to pattern match; some are single-element
%% tuples, some are maps. This has to be fixed to remove redundant function declarations
%% @todo consider putting the graphvis visualisers into their own module

proc_sexpr([ { type , _ } , { value , NodeVal } ] , { Sl_id , Sn_id , NAcc , LAcc } )  ->
	% Leaf node
	{ PSn_id , _ } = lists:last(NAcc),
	{	 Sl_id, 
		 Sn_id + 1, 
		 [ { Sn_id  , NodeVal } ] ++ NAcc,
		 LAcc ++ [ { PSn_id , Sn_id } ]
	}
;

%% @doc process sexpr when we are the root node
proc_sexpr( { O , L, R } , { Sl_id , Sn_id , NAcc , _ } ) when NAcc == [] ->
	[ { type , _ } , { value , NodeVal } ] = O,
	Acc = {	 Sl_id, 
		 Sn_id + 1, 
		 [ { Sn_id  , NodeVal } ],
		 []
	      },
	AccL = proc_sexpr( L , Acc ),
	proc_sexpr( R , AccL )
;

proc_sexpr( { [ { type , _ } , { value, NodeVal } ] , L, R } , { Sl_id , Sn_id , NAcc , LAcc } ) when NAcc /= [] ->

	{ PSn_id , _ } = lists:last(NAcc),
				
	%push link to Lacc
	
	%get own ID
	%push ID to Nacc

	Acc = {	 Sl_id, 
		 Sn_id + 1, 
		 NAcc ++ [ { Sn_id  , NodeVal } ],
		 LAcc ++ [ { PSn_id , Sn_id } ]
	      },
	AccL = proc_sexpr( L , Acc ),
	AccR = proc_sexpr( R , AccL ),

	%shift node accumulator

        { Sl_id2 , Sn_id2 , NAcc2 , LAcc2 } = AccR,
	NAcc3 = [ lists:last(NAcc2) | lists:droplast(NAcc2) ],
	{ Sl_id2 , Sn_id2 , NAcc3 , LAcc2 }		
.

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
		$g ->    gv_write(T,"parsetree.dot"),
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
	
	
	

