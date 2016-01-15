-module(planner).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([plan_query/1]).
%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Args) ->
    io:format("%% planner starting~n"),
    {ok, Args}.

handle_call({pla, ParseTree } , _From, State) ->
    io:fwrite("Planner received parsetree: ~n~p~n", [ ParseTree ] ),
    plan_query(ParseTree),
    {reply, ok, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------



find_subtrees1 (ParseTree) ->
	RelMap = #{ "a" => "A" , "b" => "A" , "c" => "B" , "d" => "B" },
%% RelMap is a temporary data dictionary for testing. To be replaced by proper catalogue server

%%	RelMap = #{ "c_custkey" => "customer" , "o_custkey" => "orders" , "l_orderkey" => "lineitem" , 
%%		 "o_orderkey" => "orders" , "l_suppkey" => "lineitem" , "s_suppkey" => "supplier" , 
%%		 "c_nationkey" => "customer" , "s_nationkey" => "supplier" , "n_nationkey" => "nation" , 
%%		 "n_regionkey" => "nation" , "r_regionkey" => "region" , "r_name" => "region"},
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
				[ {type, join} , 
			     	{ joinpred , { [{type,NodeType},{value , NodeVal}] , LPred1 , RPred1 }} , 
				{ left , LsubTree } , 
				{ right, RsubTree } , { relation , null } , {leaf, false} ] ;


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

%%	io:fwrite("my Left List:  ~p~n", [ LinstrList ]),

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

	PlanTree = find_subtrees1(ParseTree),	
	Program = generate_code(PlanTree),
	Program.	

%%% execution plan generic node

%% @doc Elements required in code/plan per node

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

