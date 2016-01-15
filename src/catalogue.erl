-module(catalogue).
-behaviour(gen_server).
-define(SERVER, ?MODULE).
-define(CATALOGUE_PATH, '/home/ccd/xndb-micro/data/catalogue_data/').

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Args) ->
    io:fwrite("* catalogue starting~n"),
    init_ets_catalogues(),	
    {ok, Args}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    write_ets_catalogues(),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------


%% System catalogue format
%% 1. relation
%% { relname:string , oid:integer , chunkserver:integer , owner:integer }
%% 2. attribute
%% { attname:string , oid:integer , type:atom , order:integer }

%% clumsy, rotten design. Refactor ASAP

get_catalogue_list() ->
    [ cat_relations , cat_attributes ]
.

init_ets_catalogues() ->

    DefaultCatProps = [ bag, protected, named_table ],
    CatList = [ ets:new(Name , DefaultCatProps) || Name <- get_catalogue_list() ],
    load_catalogue_tables(CatList),
    {ok}
.

write_ets_catalogues() ->

    CatList =  get_catalogue_list(),
    save_catalogue_tables(CatList),
    {ok}
.

save_catalogue_tables([Catalogue|CatList]) ->

	[ CatName ] = [ Name || { name , Name } <- ets:info(Catalogue) ],
	{ ok , DetsName } = dets:open_file(lists:flatten(io:format("~s~s", [ ?CATALOGUE_PATH,CatName ])) , [ {type,bag} ]),
	ets:to_dets(CatName, DetsName ),
	save_catalogue_tables(CatList)	
;

save_catalogue_tables([]) ->
	{ok}
.

load_catalogue_tables([Catalogue|CatList]) ->
	% create / load DETS table
	% for eat ETS catalogue, load the corresponding DETS and copy 

	[ CatName ] = [ Name || { name , Name } <- ets:info(Catalogue) ],
	{ ok , DetsName } = dets:open_file(lists:flatten(io_lib:format("~s~s", [ ?CATALOGUE_PATH,CatName ])) , [ {type,bag} ]),
	dets:to_ets(DetsName , CatName),
	load_catalogue_tables(CatList)	
;

load_catalogue_tables([]) ->
	{ok}
.

get_entry(RelName) ->
{ok}
.

set_entry(Oid , Catalogue , Data ) ->
{ok}
.

%% For loading / saving we use a DETS table. 

