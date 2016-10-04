-module(planner).

-export([plan_query/1, find_subtrees1/1, find_subtrees1/2]).

%% @doc Initial planner. 
%% 	Does no plan optimisation at all - merely generates a viable execution plan

plan_query(ParseTree) ->

%%	PlanTree = find_subtrees1(ParseTree),	
%%	Program = generate_code(PlanTree),
%%	Program.

	find_subtrees1(ParseTree)
.	

%%	Basic dumb planner algorithm for SELECT

%%	Iterate through FROM clause list. 
%%	  generate list of base relations (scan nodes)
%%	  populate base relation projection list
%%	  populate base relation selection predicates

%%	Check for ORDER BY
%%	  if present, add sort node and link to scan nodes

%%	Iterate through select list
%%	  if present, add aggregation node and link to lower level nodes
%%	  add output node.
%%	     populate transform list
%%	     populate rename list

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------


%% @doc <h2> subtree finder </h2>
%% this is some more text

find_subtrees1 (ParseTree) ->
	RelMap = "bleh",
	WhereClause = parser:pt_get_where_clause(ParseTree),
	find_subtrees1(WhereClause ,  RelMap )
.




find_subtrees1 ({ Self , Lchild , Rchild }, RelMap ) ->
	
	%%io:fwrite("Left Child: ~p~n  SELF: ~p~n==========~n ", [ Lchild , Self] ),
	[{clade,LClade}|LCladeTail] = find_subtrees1(Lchild, RelMap),
	[{clade,RClade}|RCladeTail] = find_subtrees1(Rchild, RelMap), 
	[{clade,SClade}|SelfTail]  = find_subtrees1(Self, RelMap),	

	%%io:fwrite("subtrees with scan nodes are >~p<  >~p<~n~n", [ LClade , RClade ] ),
	% if same rel return scan
	% if different rel return join op with trees
	% if join + scan return join op

	case
		[ true || {type, scan} <- LClade, {type, scan} <- RClade ] of
	
			[ true ] -> merge_subtrees1( 
						[ Lrel || {relation, Lrel} <- LClade , {relation, Rrel} <- RClade, (Rrel==Lrel) or (Rrel == []) or (Lrel == []) ],
						LClade,
						RClade,
						Self);

			[ ] -> merge_subtrees1( [] ,
						Lchild,
						Rchild,
						Self)
	end
;

find_subtrees1 (ParseNode, RelMap) ->
	%%process_pt_node(ParseNode, [], RelMap)
	%%io:fwrite("find_subtrees1 ~p~n", [ ParseNode ] ),
	Predicate = [ {Key, Val} || { Key , Val } <- ParseNode , lists:member(Key,[ class, sqltype, value]) ],
	CladeType = 
	  case [ Val || { class , Val } <- ParseNode ] of
		["identifier"] -> scan;
		["operator"] -> oper;
		["literal"] -> scan		
	  end,
	RelSpec = [ {Key,Val} || { Key, Val } <- ParseNode, Key==reference ],
	CladeId = 0,
	[ 
	  { clade, 
		[ 
		  {type, CladeType},
		  {relation, RelSpec},
		  {predicate, Predicate},
		  {clade_id, CladeId}
		]
	   }
	]	
.

%% do join
%%merge_subtrees1 (  [ ] , LsubTree , RsubTree, [ {type , NodeType } , {value , NodeVal} ] ) ->
merge_subtrees1 (  [ ] , LsubTree , RsubTree, Self ) ->
	
	case 
		[ true || { type , oper } <- LsubTree , { type , oper } <- RsubTree ] of 
		
			[ ] -> 
					
				[ LPred1 ] = [ LPr|| { predicate , LPr} <- LsubTree ] ,
 				[ RPred1 ] = [ RPr|| { predicate , RPr} <- RsubTree ] ,
				[ 
%					{type, join} , 
%			     		{ joinpred , { [{type,NodeType},{value , NodeVal}] , LPred1 , RPred1 }}, 
%					{ left , LsubTree }, 
%					{ right, RsubTree }, { relation , null } 
				 ] ;
%
%			[ true ] -> [ {type, join} , { joinpred, NodeVal } , {left, LsubTree } , {right, RsubTree } , { relation, null } , {leaf, false} ]
			[ true ] -> ok
	end
;

%% @doc merge subtrees containing the same relation into a single scan node
merge_subtrees1 (  [ RelSpec ] , LsubTree , RsubTree, Self ) ->
	%%io:fwrite("merge_subtrees1 with scan nodes called ~p~n*****~n~p*****~n~p~n", [ LsubTree , RsubTree, Self ] ),
	CladeId = 0, %%CHANGE!!	
	%%LClade = [{clade,LsubTree}],	
	%%RClade = [{clade,RsubTree}],	
	LClade = LsubTree,
	RClade = RsubTree,
	[ Lpred ] = [ Lpredicate || { predicate , Lpredicate } <- LClade ], 
	[ Rpred ] = [ Rpredicate || { predicate , Rpredicate } <- RClade ], 
%%	[ Spred ] = [ Spredicate || { predicate , Spredicate } <- Self ], 
	%[ {type, scan} , { predicate , { Spred , Lpred , Rpred } } , { relation , RelName } ]
	%%%
	[ 
	  { clade, 
		[ 
		  {type, scan},
		  {relation, RelSpec},
		  {predicate, {Self, Lpred, Rpred}},
		  {clade_id, CladeId}
		]
	   }
	]	
.

%% @doc generate code from plan tree to execute the query
%%	generate instructions for a join node
generate_code ( PlanTree ) ->
	io:fwrite("Plan Tree is: ~n~p~n", [ PlanTree ]),
	{ Code , _MaxID } = generate_code ( PlanTree, 0, 0, left ),
	Code.

%% @doc generate code from plan tree to execute the query
%%	generate instructions for a join node
%% @todo avoid searching the instruction list to get the NodeID for the right subtree
%%	and refactor to get tail recursion

generate_code ( [{ type, join }|Node], ParentNodeID,  CurMaxID, Descent ) ->

	[{ joinpred, Predicate }, { left , Left }, { right, Right }, { relation, null }, { leaf, false }] = Node,

%%	io:fwrite("my node id ~p~n", [ NodeID ]),

	NodeID = case Descent of
		left -> ParentNodeID + 1;
		right -> CurMaxID + 1
		end,
	
	Instruction = { action , spawn , [
			{ id , NodeID }, 
			{ target , ParentNodeID },  
			{ module , join },
			{ predicate , Predicate } ]
		},

	{ LinstrList, LMaxID }  = generate_code(Left, NodeID, CurMaxID, left),

	NewMaxID = if LMaxID > CurMaxID -> LMaxID;
			true -> CurMaxID
		   end,
			
	{ RinstrList, _RMaxID }  = generate_code(Right, NodeID, NewMaxID, right),

	{ lists:flatten( [ Instruction ] ++ [ LinstrList ] ++ [ RinstrList ]) , NewMaxID }

;

%% @doc generate code from plan tree to execute the query
%%	generate instructions for a scan node
generate_code ( [{ type, scan }|Node], ParentNodeID, CurMaxID, Descent ) ->

	io:fwrite("LEAF NodeID is ~p  ~p ~p ~n~n----", [ ParentNodeID , CurMaxID , Descent ]),	
	
	NodeID = case Descent of
		left -> ParentNodeID + 1;
		right -> CurMaxID + 1
		end,

	[{ predicate , Predicate }, { relation, Relation }, { leaf, true }] = Node,

	{{ action , command , [
			    { id , NodeID },
			    { target , ParentNodeID }, 
		    	    { module , seq_scan },
			    { predicate , Predicate },
			    { relation , Relation } ] 
	}, NodeID }
.



