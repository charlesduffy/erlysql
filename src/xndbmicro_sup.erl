-module(xndbmicro_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, { {one_for_one, 5, 10}, [
%%monitor the listener process
	{ listener_process , { listener , start_link , [] } , permanent , 2000 , worker , [ listener ] } , 
	{ despatch_process , { despatch , start_link , [] } , permanent , 2000 , worker , [ despatch ] } , 
	{ planner_process , { planner , start_link , [] } , permanent , 2000 , worker , [ planner ] } , 
%%monitor the chunk server process
	{ chunk_server_process , { chunkserver , start_link , [] } , permanent , 2000 , worker , [ chunkserver ] } 
	]} }.

