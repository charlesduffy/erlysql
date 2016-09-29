%% @doc <h3> Parser </h3>
%% 
%% <h4> Parse Tree data structure </h4>
%% 
%% 
%% @end
-module(parser).
-export([parse_query/1, pt_get_range_table/1, pt_get_where_clause/1]).
-on_load(init/0).

init() ->
    ok = erlang:load_nif("./priv/nif_convert", 0).

parse_query(_X) ->
    exit(nif_library_not_loaded).

%% @doc Get a subtree of the parse tree.
%% Takes the portion of the parse tree and a search path. 

pt_get_subtree(Data, []) ->
    Data
;

pt_get_subtree([StmtHead|StmtTail], Path) ->
    { Tag , Data } = StmtHead,
    [PathHead|PathTail] = Path,
		case Tag of 
		    PathHead -> pt_get_subtree(Data , PathTail); 
		    _Else -> pt_get_subtree(StmtTail, Path)
		end
.

%% @doc Returns the range table of a query

pt_get_range_table( [{query,[{statement_type, "select_statement" }, { select_statement, SelectStmt }]}] ) ->
    pt_get_subtree(SelectStmt, [ table_expr,from_clause ] )
.


%% @doc Returns the where clause of a query

pt_get_where_clause( [{query,[{statement_type, "select_statement" }, { select_statement, SelectStmt }]}] ) ->
    pt_get_subtree(SelectStmt, [ table_expr,where_clause ] )
.

