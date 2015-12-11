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

%%	RelMap = #{ "c_custkey" => "customer" , "o_custkey" => "orders" , "l_orderkey" => "lineitem" , 
%%		 "o_orderkey" => "orders" , "l_suppkey" => "lineitem" , "s_suppkey" => "supplier" , 
%%		 "c_nationkey" => "customer" , "s_nationkey" => "supplier" , "n_nationkey" => "nation" , 
%%		 "n_regionkey" => "nation" , "r_regionkey" => "region" , "r_name" => "region"},
	find_subtrees1( maps:get(where_clause, ParseTree), RelMap)		
.

find_subtrees1([ { type , colref } , { value , NodeVal } ], RelMap ) ->
	[ {type , scan } , { predicate , [ { type , colref } , { value , NodeVal } ] }, { relation , maps:get(NodeVal, RelMap) }, { leaf , true } ]
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
				{ right, RsubTree } , { relation , null } ] ;


			[ ] -> [ {type, join} , { joinpred , NodeVal } , {left , LsubTree } , {right, RsubTree } , { relation , null } ]
	end
;
%% do merge 
merge_subtrees1 (  [ RelName ] , LsubTree , RsubTree, Self ) ->
	%%io:fwrite("merge_subtrees1 with scan nodes called ~n~p~n~p", [ LsubTree , RsubTree ] ),
	[ Lpred ] = [ Lpredicate || { predicate , Lpredicate } <- LsubTree ] , 
	[ Rpred ] = [ Rpredicate || { predicate , Rpredicate } <- RsubTree ] , 
	[ {type, scan} , { predicate , { Self , Lpred , Rpred } } , { relation , RelName } , { leaf , false} ]
.

%% @doc Produce list of scan nodes from parse tree	
%%      including projections and selection predicates			

make_scan_nodes(ParseTree) ->

%%	Brels = mk_brels(maps:get(from_clause, ParseTree)),
%%	io:fwrite("Base relations: ~p", [ Brels ])
%%	ScanNodes = get_sel(InitScanNodes)
	ST = find_subtrees1(ParseTree),
	
	io:fwrite("subtrees : ~p~n~n ========= ~n~n~w", [ ST , ST ])
.

%% @doc Initial planner. 
%% 	Does no plan optimisation at all - merely generates a viable execution plan

plan_query(ParseTree) ->

	ScanNodes = make_scan_nodes(ParseTree),	

	{ok , ScanNodes }.	


%%% execution plan generic node

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

