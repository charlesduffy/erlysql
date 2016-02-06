-module(proto).
-export([get_relmap/1, flush_catalogue/0]).

%%% protocol functions wrapping gen_server client funcs

%% catalogue 

get_relmap(RelList) ->
	gen_server:call(catalogue , {map , RelList})
.

flush_catalogue() ->
	gen_server:call(catalogue , {save , []})
.
