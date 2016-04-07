-module(planner).

-export([plan_query/1, find_subtrees1/1]).

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------


%% @doc <h2> subtree finder </h2>
%% this is some more text

find_subtrees1 (ParseTree) ->
%%  RelMap = #{ "a" => "A" , "b" => "A" , "c" => "B" , "d" => "B" },
%% RelMap is a temporary data dictionary for testing. To be replaced by proper catalogue server

%% We are going to pass a relmap that contains the projection list and relation list in OID form. 

%% example catalogue insertion commands
%% gen_server:call(catalogue, { write , { "A" , [  [ {attname , "a" } , {atttype, integer }   ] , [ {attname, "b" } , {atttype,integer  }   ]    ] }}).
%% gen_server:call(catalogue, { write , { "B" , [  [ {attname , "c" } , {atttype, integer }   ] , [ {attname, "d" } , {atttype,integer  }   ]    ] }}).

%% Consider adding a 'comments' or 'debug' field to the plan for inspection

%% Object proplist should be improved. Something like:

%% { colref , [ { name , "customer" } , { oid , 901231 } ] }

%% Planner also needs to know current xation because (currently in design) DDL is transactional

%% Plan flow
%% - get select list map (Relmap). Error if rels / atts incorrect / not found
%% - disambiguate select list references. Ie, resolve A.a , "a" to an OID for all further comparison
%% -- probably leave the textual colref names for plan debug purposes
%% - Process where clause and perform planning. 


	%% get relation list from parse tree

	

	%% get attribute relation list from system catalogue
	
	RelMap = proto:get_relmap(
			       	    maps:get(from_clause, ParseTree)
			 ),
io:fwrite("RelMap is: ~p~n", [ RelMap ] ),

	find_subtrees1( maps:get(where_clause, ParseTree), RelMap)		
.

find_subtrees1([ { type , colref } , { value , NodeVal } ], RelMap ) ->
	[ {type , scan } , { predicate , [ { type , colref } , { value , NodeVal } ] }, { relation , maps:get(NodeVal, RelMap) }, { leaf , true } ]
	%% @todo get rid of the 'leaf' tag. Figure out some other way of achieving the same thing. 
;

find_subtrees1([ { type , NodeType } , { value , NodeVal } ], _ ) ->
	[ {type , scan } , { predicate , [ { type , NodeType } , { value , NodeVal } ] }, { relation , null } , { leaf , true } ]
;


find_subtrees1 ({ Self , Lchild , Rchild }, RelMap ) ->
	
	LsubTree = find_subtrees1(Lchild, RelMap),
	RsubTree = find_subtrees1(Rchild, RelMap), 	

	% if same rel return scan
	% if different rel return join op with trees
	% if join + scan return join op

	case
		[ true || {type, scan} <- LsubTree , {type, scan} <- RsubTree  ] of
	
			[ true ] -> merge_subtrees1( 
						[ Lrel || {relation, Lrel} <- LsubTree , {relation, Rrel} <- RsubTree , (Rrel==Lrel) or (Rrel == null) or (Lrel == null) ],
						LsubTree,
						RsubTree,
						Self);

			[ ] -> merge_subtrees1( [] ,
						LsubTree,
						RsubTree,
						Self)
	end
.

%% do join
merge_subtrees1 (  [ ] , LsubTree , RsubTree, [ {type , NodeType } , {value , NodeVal} ] ) ->
	case 
		[ true || { leaf , true } <- LsubTree , { leaf , true } <- RsubTree ] of 
		
			[ true ] -> 
					
				[ LPred1 ] = [ LPr|| { predicate , LPr} <- LsubTree ] ,
 				[ RPred1 ] = [ RPr|| { predicate , RPr} <- RsubTree ] ,
				[ 
					{type, join} , 
			     		{ joinpred , { [{type,NodeType},{value , NodeVal}] , LPred1 , RPred1 }} , 
					{ left , LsubTree } , 
					{ right, RsubTree } , { relation , null } , {leaf, false}
				 ] ;

			[ ] -> [ {type, join} , { joinpred, NodeVal } , {left, LsubTree } , {right, RsubTree } , { relation, null } , {leaf, false} ]
	end
;

%% @doc merge subtrees containing the same relation into a single scan node
merge_subtrees1 (  [ RelName ] , LsubTree , RsubTree, Self ) ->
	%%io:fwrite("merge_subtrees1 with scan nodes called ~n~p~n~p", [ LsubTree , RsubTree ] ),
	[ Lpred ] = [ Lpredicate || { predicate , Lpredicate } <- LsubTree ] , 
	[ Rpred ] = [ Rpredicate || { predicate , Rpredicate } <- RsubTree ] , 
	[ {type, scan} , { predicate , { Self , Lpred , Rpred } } , { relation , RelName } , { leaf , true } ]
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



%% @doc Initial planner. 
%% 	Does no plan optimisation at all - merely generates a viable execution plan

plan_query(ParseTree) ->

%%	PlanTree = find_subtrees1(ParseTree),	
%%	Program = generate_code(PlanTree),
%%	Program.

	generate_code(
			find_subtrees1(ParseTree)
		     )
.	

%%% execution plan generic node

%%  Elements required in code/plan per node

%% Node Type: scan, join, agg, filter
%% Predicate: an s-expr in the case of join/scan/filter, function for agg, 
%% Node ID: ID code for this node instance
%% Source ID set: tuple of the source nodes for this node. (may not be required)
%% Target ID: target node for this node. 


%% comments below deprecated

%%       #{ id => int , module => atom (name of executor module) , target => id of node to deliver tuples to , 
%%	    sourcelist => list (nodes delivering to this node. Review need for this). , 
%%	    instruction => map (execution node instruction list)}
%%	    
%%	types of node instruction and structure

%%	scan

%%	#{ relname => text , projection => list (list of column names) , selection => sexpr ( predicate tree structure ) }

%% 	sort

%%	#{ key => list (list of projections to sort by) , collation => list (list of collations, one per projection) , algo => atom (algorithm)}

%%	join

%%	#{ key => list (list of projections to join on) , type => atom (join type) }

%% 	aggregation

%%	transform


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

