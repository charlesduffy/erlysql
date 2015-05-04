-module(chunkserver).
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

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Args) ->
    io:format('$$ chunkserver $$ initialising ~n'),
    ets:new(chunktable, [ set , named_table ]),
    {ok, Args}.

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

exec_ins(Instup, State) ->
    ets:insert(chunktable, Instup),
    {ok, State}.

exec_sel(Selpred, State) ->
    ets:match(chunktable, { Selpred } ),
    {ok, State}.
