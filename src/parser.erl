%% @doc <h3> Parser </h3>
%% 
%% <h4> Parse Tree data structure </h4>
%% Parse Tree:
%% 
%%	 #{from_clause => [[{name,"A"}],[{name,"B"}],[{name,"C"}]],
 %%	  select_list => [[{type,colref},{value,"a"}],
 %%	   {[{type,operator},{value,"-"}],
 %%	    [{type,colref},{value,"b"}],
 %%	    [{type,int},{value,1}]},
 %%	   {[{type,operator},{value,"+"}],
 %%	    [{type,colref},{value,"c"}],
  %%	   {[{type,operator},{value,"/"}],
 %%	     [{type,colref},{value,"a"}],
 %%	     [{type,int},{value,2}]}}],
 %%	  where_clause => {[{type,operator},{value,"AND"}],
 %%	   {[{type,operator},{value,"AND"}],
  %%	   {[{type,operator},{value,"<"}],
 %%	     [{type,colref},{value,"a"}],
 %%	     [{type,int},{value,2}]},
 %%	    {[{type,operator},{value,"="}],
 %%	     [{type,colref},{value,"b"}],
 %%	     [{type,colref},{value,"c"}]}},
 %%	   {[{type,operator},{value,"<"}],
 %%	    {[{type,operator},{value,"+"}],
 %%	     [{type,colref},{value,"a"}],
 %%	     [{type,int},{value,1}]},
  %%	   [{type,int},{value,65}]}}}
%% 
%% @end
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
