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


%% Make list of base relations from FromClause list

mk_brels([H|T]) ->
	N = mk_scnode(H),
	mk_brels(T, N).

mk_brels([H|T] , Acc) ->
	N = mk_scnode(H),
	mk_brels(T , [N] ++ Acc);

mk_brels([], Acc) -> Acc.

%% Process a single FromClause entry and produce a scan node

mk_scnode(N) ->
       #{   
	    id => 0, 	
	    module => seqscan , 
	    target => 0,
	    sourcelist => [], 
    	    instruction => #{ 	
				relname => maps:get(name, N), 
				alias => maps:get(alias, N),
				projection => [] ,
				selection =>  [] 
			    }
	}.
			
%% Populate initial scan nodes with selection information
get_sel(D)->
	{ok}.

get_sel(S , W) ->
	{ok}	
.

make_scan_nodes(ParseTree) ->
	%% Produce list of scan nodes from parse tree	
	%% including projections and selection predicates			

	InitScanNodes = mk_brels(maps:get(from_clause, ParseTree)),
	ScanNodes = get_sel(InitScanNodes)
.

plan_query(ParseTree) ->
	%% Initial planner. 
	%% Does no plan optimisation at all - merely generates a viable execution plan

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

