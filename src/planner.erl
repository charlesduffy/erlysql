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


%% @doc Make list of base relations from FromClause list
%%
%% This is a list of tuples of the form:
%% { name:string , alias:string }
%% 
%% alias is blank if not present

mk_brels([H|T]) -> mk_brels(T, { maps:get(name , H) , maps:get(alias, H, "") } ).

mk_brels([H|T] , Acc) -> mk_brels(T , [ { maps:get(name , H) , maps:get(alias, H, "") } ] ++ Acc);

mk_brels([], Acc) -> Acc.

%% @doc Process a single FromClause entry and produce a scan node
%% 
%% To get scan node selection predicates, we need to search the Where
%% clause to find relevant subexpressions (ie, those which do not compare
%% attributes from different relations, becoming join conditions)
%% 
%% We search for the largest subexpressions containing only:
%% 1. value expressions
%% 2. Comparison operators
%% 3. Attributes from the same relation

mk_scnode(N) ->
       #{   
	    id => 0, 	
	    module => seqscan , 
	    target => 0,
	    sourcelist => [], 
    	    instruction => #{ 	
				relname => maps:get(name, N), 
				alias => maps:get(alias, N, ""),
				projection => [] ,
				selection =>  [] 
			    }
	}.
			
%% @doc Populate initial scan nodes with selection information
get_sel(D)->
	{ok}.

get_sel(S , W) ->
	{ok}	
.

%% @doc prototype subtree matcher
%% 
%% Find largest subtrees containing references to a single base
%% relation 
%% * Recurse left until leaf node, noting Node IDs (or pushing them on to a stack)
%% * Note if colref, determine what relation it is member of
%% * Go back up. 
%% * Heuristic - check if immediate subtree matches OP,[col|val],[col|val] stereotype
%%   -- if match, set Node ID entry for table in question to NodeID
%%   -- No match, continue
%% * Recurse right / left downward
%%   -- 

%% blah new idea
%% recurse down creating a list for each node. 
%% list contains all distinct relations in the subtree below
%% push list recusrsively as progresses


get_subtree({ Op , L , R } , { Relname , Relalias } ) ->
	{ok}	
.

%%match_subtree ( { Op, L, R } , { Relname , Relalias } ) when ( Op == ">" or Op == "<" or Op == "=" or Op == "!=" )  ->
%%	{ok}	
%%.

%% colrefs and value expr are always leaf nodes
%% 



%% @doc Produce list of scan nodes from parse tree	
%%      including projections and selection predicates			

make_scan_nodes(ParseTree) ->

	Brels = mk_brels(maps:get(from_clause, ParseTree)),
	io:fwrite("Base relations: ~p", [ Brels ])
%%	ScanNodes = get_sel(InitScanNodes)
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

