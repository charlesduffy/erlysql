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
	DotText = gv_traverse(ParseTree),
	file:write_file(OutFile, 
		io_lib:fwrite(	"~p", 
				[ DotText ])
			),
	

gv_traverse(ParseTree) ->
	sellist_traverse(maps:get(select_list, ParseTree), 0),
	{ok}
.	

%% @doc traverse select list and process each s-expression in it
%% Each sexpr is traversed, and each node pushes a graphviz
%% node definition string and a graphviz link string to 
%% two accumulators (Nacc and Lacc). 
%% 
%% Accumulator is of the form:
%% 
%% { select list ID , sexpr Node ID, Node Accumulator, Link Accumulator }

sellist_traverse([], Sl_id) ->
	{ok};
sellist_traverse([H|T], Sl_id ) ->
	%%io:fwrite("here is select list: ~n~p~n", [ H ] ),
	L = proc_sexpr(H , { 0 , 0 , [] , [] } ),
	%%io:fwrite("accumulators: ~n~p" , [ L ]),
	write_accs(L),
	sellist_traverse(T , Sl_id + 1)
	.

write_accs(L) ->

	Header = " digraph g { graph [ rankdir = \"LR\" ];
		   node [ fontsize  = \"16\" shape = \"ellipse\" ];
		   edge [ ]; ",
	{ _ , _ , Nodes , Links } = L,
	NodeText = print_nodes(Nodes),
	LinkText = print_links(Links),
	file:write_file("gv/parsetree.dot", 
		io_lib:fwrite("~s ~n ~s ~n ~s ~n }", 
		[ Header , NodeText , LinkText ])),
	{ok}
.

print_links(L) -> print_links(L, "", 0).

print_links([H|T], TextAcc, LinkIdAcc) ->
	{ Nfrom , Nto } = H , 
	Text = io_lib:format("\"sexpr0_~p\" -> \"sexpr0_~p\" [ id = ~p ];  ~n", [ Nfrom, Nto, LinkIdAcc ]),
	print_links(T, string:concat(Text, TextAcc), LinkIdAcc + 1);

print_links([] , TextAcc, _) -> TextAcc.

print_nodes(L) -> print_nodes(L, "").

print_nodes([H|T], TextAcc) ->
	{ Nid , Nlabel } = H , 
	NLabelArg = 
		case is_list(Nlabel) of
		true -> "~p";
		false -> "\"~p\""
	end,
	Text = io_lib:format("\"sexpr0_~p\" [ label = " ++ NLabelArg ++ "];~n~n", [ Nid, Nlabel ]),
	print_nodes(T, string:concat(Text, TextAcc));

print_nodes([] , TextAcc) -> TextAcc.

%% @doc recursive sexpr processor functions. Traverse the structure
%% while pushing nodes on to a Node accumulator, and links on to a Link accumulator.
%% Returns bot accumulators for printing the Graphviz dot output. 
%% @todo some leaf nodes are difficult to pattern match; some are single-element
%% tuples, some are maps. This has to be fixed to remove redundant function declarations
%% @todo consider putting the graphvis visualisers into their own module


proc_sexpr(  V , { Sl_id , Sn_id , NAcc , LAcc } ) when not is_tuple(V) ->
	% @doc process Leaf node
	{ PSn_id , Plabel } = lists:last(NAcc),
	{	 Sl_id, 
		 Sn_id + 1, 
		 [ { Sn_id  , maps:get(value, V) } ] ++ NAcc,
		 LAcc ++ [ { PSn_id , Sn_id } ]
	}
;
proc_sexpr( { V } , { Sl_id , Sn_id , NAcc , LAcc } ) when not is_tuple(V) ->
	% Leaf node
	{ PSn_id , Plabel } = lists:last(NAcc),
	{	 Sl_id, 
		 Sn_id + 1, 
		 [ { Sn_id  , maps:get(value, V) } ] ++ NAcc,
		 LAcc ++ [ { PSn_id , Sn_id } ]
	}
;

%% @doc process sexpr when we are the root node
proc_sexpr( { Op, L, R } , { Sl_id , Sn_id , NAcc , LAcc } ) when NAcc == [] ->
	Acc = {	 Sl_id, 
		 Sn_id + 1, 
		 [ { Sn_id  , maps:get(value, Op) } ],
		 []
	      },
	AccL = proc_sexpr( L , Acc ),
	proc_sexpr( R , AccL )
;

proc_sexpr( { Op, L, R } , { Sl_id , Sn_id , NAcc , LAcc } ) when NAcc /= [] ->
	%% @doc process sexpr when we are the parent (ie, node accumulator is empty)
	{ PSn_id , Plabel } = lists:last(NAcc),
				
	%push link to Lacc
	
	%get own ID
	%push ID to Nacc

	Acc = {	 Sl_id, 
		 Sn_id + 1, 
		 NAcc ++ [ { Sn_id  , maps:get(value, Op) } ],
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
	
	
	

