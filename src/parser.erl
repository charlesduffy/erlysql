%% @doc <h3> Parser </h3>
%% 
%% <h4> Parse Tree data structure </h4>
%% 
%% 
%% @end
-module(parser).
-export([parseQuery/1, ptGetRangeTable/1, ptGetFromClause/1, ptGetFromClause2/1, ptGetSubtree/2]).
-on_load(init/0).

init() ->
    ok = erlang:load_nif("./priv/nif_convert", 0).
%%
%%foo(_X) ->
  %%  exit(nif_library_not_loaded).
%%bar(_Y) ->
 %%   exit(nif_library_not_loaded).

parseQuery(_X) ->
    exit(nif_library_not_loaded).

%% @doc Get an element of a select statment 
%% consider generalising to all statement lists
%% takes: SelectStmt, returns FromClause

ptGetFromClause([SelectStmtHead|SelectStmtTail]) ->
    { Tag , Data } = SelectStmtHead,
    io:fwrite("pt1 select stmt head is: ~p ~n", [ SelectStmtHead ] ),
    case Tag of
	table_expr -> ptGetFromClause2(Data);
	_Else -> ptGetFromClause(SelectStmtTail)
    end
.

ptGetFromClause2([SelectStmtHead|SelectStmtTail]) ->
    { Tag , Data } = SelectStmtHead,
    io:fwrite("pt2 select stmt head is: ~p ~n", [ SelectStmtHead ] ),
    case Tag of
	from_clause -> SelectStmtHead;
	_Else -> ptGetFromClause2(SelectStmtTail)
    end
.

ptGetSubtree(Data, []) ->
    Data
;

ptGetSubtree([StmtHead|StmtTail], Path) ->
    { Tag , Data } = StmtHead,
    [PathHead|PathTail] = Path,
    io:fwrite("get subtree is: ~n Tag: ~p~n Data: ~p~n StmtHead ~p~n StmtTail ~p~n PathHead ~p~n PathTail ~p~n~n ---- ",
	 [ Tag, Data,  StmtHead, StmtTail, PathHead, PathTail ] ),
		case Tag of 
		    PathHead -> ptGetSubtree(Data , PathTail); 
		    _Else -> ptGetSubtree(StmtTail, Path)
		end
.


%% rewrite to generic! Should get clauses now matter how deep nested
%% needs to know the path to the nested elem, ie, table_expr, from_clause

%%o:fwrite("CURRENT NODE DATA LIST IS~n================~n ~p ~n--------------~n",[get_plan_node_data(PlanNode)]),

%% @doc Returns a subtree of a parse tree

ptGetRangeTable( [{query,[{statement_type, "select_statement" }, { select_statement, SelectStmt }]}] ) ->

%% get first tuple in tuplist
%% if tag matches, return tuple subtree
%% if tag doesn't match, search tuple subtree (depth-first tree search)
    io:fwrite("select stmt is: ~p ~n", [ SelectStmt ] ),
    ptGetFromClause(SelectStmt),
    ptGetSubtree(SelectStmt, [ table_expr,from_clause ] )
.


