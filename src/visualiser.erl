-module(visualiser).
-export([gv_write/2, gv_explain_plan/2]).


gv_explain_plan(QueryText, OutFile) ->
	ParseTree = parser:parseQuery(QueryText),
	PlanTree = planner:find_subtrees1(ParseTree),
	io:fwrite("PLAN TREE IS~n================~n ~p ~n--------------~n",[PlanTree]),
	DotTree = process_plan_tree(PlanTree),
%%	file:write_file(OutFile, DotText ),
%%	{ok}
	io:fwrite("DOT TREE IS ~n================~n ~p ~n--------------~n",[DotTree]),
	DotText = gv_mkdot_plan_tree(DotTree),
	%%io:fwrite("DOT TEXT IS ~n================~n ~p ~n--------------~n",[DotText])
%%	DotText
	%%file:write_file(OutFile, DotText ),
	io:fwrite([DotText])
.

gv_mkdot_plan_tree(PlanAccumulator) ->
	{ NodeAccumulator , LinkAccumulator , _MaxId , _LinkId } = PlanAccumulator,
	NodeDotText = gv_mk_nodes(NodeAccumulator, ""),
	LinkDotText = gv_mk_links(LinkAccumulator, ""),
	"graph \"explain plan\" { " ++ LinkDotText ++ NodeDotText ++ "}"	
.

gv_mk_nodes([Node|NodeTail], DotTextAccumulator) ->
	NodeId = proplists:get_value(id, Node),
	gv_mk_nodes(NodeTail, DotTextAccumulator ++
		io_lib:format("\"plan_node_~p\" [ label = ~p ];~n", [ NodeId , NodeId  ])
		)
	
;

gv_mk_nodes([], DotTextAccumulator) ->
	DotTextAccumulator
.

gv_mk_links([Link|LinkTail], DotTextAccumulator) ->
	{ FromNode , ToNode } = Link,
	gv_mk_links(LinkTail, DotTextAccumulator ++
		io_lib:format("\"plan_node_~p\" -- \"plan_node_~p\" [ id = ~p ];~n", [ FromNode , ToNode , 0 ])
	)

;

gv_mk_links([], DotTextAccumulator) ->
	DotTextAccumulator
.

get_plan_node_data(PlanNode) ->

%% @doc populate the data block of a plan node graph element

	[ { Key , Value } || { Key , Value } <- PlanNode , case Key of type -> true; joinpred -> true; relation -> true; predicate -> true; _Else -> false end ]
.

process_plan_tree(PlanTree) ->
	process_plan_tree(PlanTree, { [] , [] , 0 , -1 })
.

process_plan_tree([] , Accumulator) ->
	Accumulator
;

process_plan_tree(PlanNode,  Accumulator ) ->
	{ NodeAccumulator, LinkAccumulator, MaxId , ParentId } = Accumulator,
	
	%%io:fwrite("CURRENT PLAN NODE IS~n================~n ~p ~n--------------~n",[PlanNode]),
	%%io:fwrite("CURRENT NODE DATA LIST IS~n================~n ~p ~n--------------~n",[get_plan_node_data(PlanNode)]),
	%% set node variables. NodeID = MaxID. Type, predicate, etc.
	CurrentNode = [ { id , MaxId } , {parentid , ParentId} ] ++ get_plan_node_data(PlanNode),	
	%%io:fwrite("CURRENT ACCUMULATOR ITEM IS~n================~n ~p ~n--------------~n",[CurrentNode]),

	%% push current node on to node accumulator.
	%% if not root node, push link from parent on to link accumulator

	NewAccumulator = { 
				%% Node Accumulator
				NodeAccumulator ++ [ CurrentNode ] , 
				
				%% Link Accumulator
				case ParentId of
					-1 -> LinkAccumulator;
					_Else -> lists:flatten(LinkAccumulator ++ [ { ParentId , MaxId } ])
				end ,

				MaxId + 1,

				MaxId
					
			  },

	%% if leaf node, return
	%% call self on left child, incrementing MaxId and passing current NodeID as ParentId.
	%% call self on right child, incrementing MaxId and passing current NodeID as ParentId.
	case proplists:lookup(leaf, PlanNode) of
		{leaf, true } -> NewAccumulator;
	
		{leaf, false} -> 	{LnewNodeAccumulator, LnewLinkAccumulator, LnewMaxId, _LnewParentId } = 
						process_plan_tree(proplists:get_value(left, PlanNode), NewAccumulator),
				 	process_plan_tree(proplists:get_value(right, PlanNode), 
						{LnewNodeAccumulator, LnewLinkAccumulator, LnewMaxId, ParentId}  )
				
	end
.

%% @doc write out a Graphviz dot file of the entire parse tree

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
%% @todo add some logic to detect presence of where clause
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
%% @todo consider putting the graphvis visualisers into their own module

proc_sexpr({[ { type , _ } , { value , NodeVal } ]} , { Sl_id , Sn_id , NodeAcc , LinkAcc } )  when NodeAcc == []  ->
	% Solitary leaf node
	{	 Sl_id, 
		 1, 
		 [ { Sn_id  , NodeVal } ] ,
		 []
	}
;

proc_sexpr([ { type , _ } , { value , NodeVal } ] , { Sl_id , Sn_id , NodeAcc , LinkAcc } )  ->
	{ PSn_id , _ } = lists:last(NodeAcc),
	{	 Sl_id, 
		 Sn_id + 1, 
		 [ { Sn_id  , NodeVal } ] ++ NodeAcc,
		 LinkAcc ++ [ { PSn_id , Sn_id } ]
	}
;

%% @doc process sexpr when we are the root node
proc_sexpr( { O , L, R } , { Sl_id , Sn_id , NodeAcc , _ } ) when NodeAcc == [] ->
	[ { type , _ } , { value , NodeVal } ] = O,
	Acc = {	 Sl_id, 
		 Sn_id + 1, 
		 [ { Sn_id  , NodeVal } ],
		 []
	      },
	AccL = proc_sexpr( L , Acc ),
	proc_sexpr( R , AccL )
;

proc_sexpr( { [ { type , _ } , { value, NodeVal } ] , L, R } , { Sl_id , Sn_id , NodeAcc , LinkAcc } ) when NodeAcc /= [] ->

	{ PSn_id , _ } = lists:last(NodeAcc),
				
	%push link to Lacc
	
	%get own ID
	%push ID to Nacc

	Acc = {	 Sl_id, 
		 Sn_id + 1, 
		 NodeAcc ++ [ { Sn_id  , NodeVal } ],
		 LinkAcc ++ [ { PSn_id , Sn_id } ]
	      },
	AccL = proc_sexpr( L , Acc ),
	AccR = proc_sexpr( R , AccL ),

	%shift node accumulator

        { Sl_id2 , Sn_id2 , NodeAcc2 , LinkAcc2 } = AccR,
	NodeAcc3 = [ lists:last(NodeAcc2) | lists:droplast(NodeAcc2) ],
	{ Sl_id2 , Sn_id2 , NodeAcc3 , LinkAcc2 }		
.

