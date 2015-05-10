-module(parser).
-export([foo/1, bar/1, parseQuery/1]).
-on_load(init/0).

init() ->
    ok = erlang:load_nif("./priv/parser_nif", 0).

foo(_X) ->
    exit(nif_library_not_loaded).
bar(_Y) ->
    exit(nif_library_not_loaded).
parseQuery(_X) ->
    exit(nif_library_not_loaded).
