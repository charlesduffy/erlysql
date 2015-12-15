-module(despatch).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).
-export([generate_childspec/1]).

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
    {ok, Args}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

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

%% @doc generate a child process specification from the generated code
generate_childspec({ok, Program}) ->
	generate_childspec(Program, [])
.

generate_childspec([[{action , spawn}|Instr]|Program], ChildspecAcc) ->
	[{id,Id}, {target,_Target}, {module,_Module}, {predicate,_Predicate}] = Instr,
	generate_childspec(Program,
		 ChildspecAcc ++ 
		[ { join_process, { nestedloop_join , start_link , [ Id ] } , permanent , 2000 , worker , [ join ] } ])
;
generate_childspec([[{action , _ }|_Instr]|Program], ChildspecAcc) ->
	generate_childspec(Program,ChildspecAcc)
;

generate_childspec([], ChildspecAcc) ->
%% add the extra bits required and return	
	{ok , { {one_for_one, 5, 10}, ChildspecAcc }}
.

