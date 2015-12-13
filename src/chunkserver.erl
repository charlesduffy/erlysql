-module(chunkserver).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(ServerName) ->
    gen_server:start_link({local, ServerName}, ?MODULE, [], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------


%% Initialise the chunk ETS table
%% -- add error checking
init(Args) ->
    io:format('%% chunkserver starting - ~p~n', [ Args ]),
    Table = ets:new(chunktable, [ set, public ]),
    populate_table_tmp(Table, Args),
    State = { Table },
    { ok, State }.


%% the Query format is simple right now
handle_call({ sca,  Query } , _From , State) ->
    io:format('chunkserver call SCA: ' ),
    { Table } = State, 
    Reply = scan_table( Table , Query ),
    {reply, Reply, State};	

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({ sca,  Query } , State) ->
    io:format('$$ chunkserver $$ received: ' ),
    {noreply, State};	

handle_cast({ ins,  Query } , State) ->
    R = io_lib:format("~p", [ Query ]),
    io:format('$$ chunkserver INS $$ received: ~s~n', [ R ] ),
    io:format('$$ chunkserver state $$ received: ~n' ),
    State2 = exec_ins( Query , State ),
    {noreply, State2};	

handle_cast({ sel,  Query } , State) ->
    R = io_lib:format("~p", [ Query ]),
    io:format('$$ chunkserver SEL $$ received: ~s~n', [ R ] ),
    io:format('$$ chunkserver state $$ received: ~n' ),
    State2 = exec_sel( Query , State ),
    {noreply, State2};	

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

scan_table(Table , Query) ->
	Result = ets:match(Table , Query ),			
	{ ok , Result }.

%% temporary for test data
populate_table_tmp (Table, Args) ->
	%%  open file
	%%  read file line
	%%  load ETS table
	{ ok, FileHandle } = file:open('/usr/share/dict/words', [read]),
	try get_lines(FileHandle, Table, 1) 
		after file:close(FileHandle)	
	end,
	{ ok }.

get_lines (FileHandle, Table, Index) ->

	case file:read_line(FileHandle) of
		eof -> [];
		{ok , Line } -> insert_helper( FileHandle, Line, Table, Index)
	end.

insert_helper ( FileHandle, Line, Table , Index) ->
	ets:insert(Table, { Index , Line }),
	get_lines(FileHandle, Table, Index + 1). 
	

exec_ins(Instup, State) ->
    ets:insert(chunktable, Instup),
    {ok, State}.

exec_sel(Selpred, State) ->
    ets:match(chunktable, { Selpred } ),
    {ok, State}.
