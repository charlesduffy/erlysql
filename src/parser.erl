%% @doc <h3> Parser </h3>
%% 
%% <h4> Parse Tree data structure </h4>
%% 
%% 
%% @end
-module(parser).
-export([parseQuery/1, ptGetRangeTable/1]).
-on_load(init/0).

init() ->
    ok = erlang:load_nif("./priv/nif_convert", 0).

parseQuery(_X) ->
    exit(nif_library_not_loaded).

%% @doc Get a subtree of the parse tree.
%% Takes the portion of the parse tree and a search path. 

ptGetSubtree(Data, []) ->
    Data
;

ptGetSubtree([StmtHead|StmtTail], Path) ->
    { Tag , Data } = StmtHead,
    [PathHead|PathTail] = Path,
		case Tag of 
		    PathHead -> ptGetSubtree(Data , PathTail); 
		    _Else -> ptGetSubtree(StmtTail, Path)
		end
.

%% @doc Returns the range table of a query

ptGetRangeTable( [{query,[{statement_type, "select_statement" }, { select_statement, SelectStmt }]}] ) ->
    ptGetSubtree(SelectStmt, [ table_expr,from_clause ] )
.
