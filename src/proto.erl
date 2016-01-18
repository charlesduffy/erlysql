-module(proto).
-export([get_relmap/1]).

%%% protocol functions wrapping gen_server client funcs

%% catalogue 

get_relmap(RelList) ->
	gen_server:call(catalogue , {map , RelList})
.
