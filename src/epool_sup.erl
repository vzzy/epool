%%%-------------------------------------------------------------------
%% @doc epool top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(epool_sup).

-behaviour(supervisor).

%% API
-export([
	start_link/0,
		
	add_pool/4,
	stop_pool/1
]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
	ets:new(epool_name, [named_table, public]),
	ChildSpec = {epool_name,{epool_name, start_link, []}, permanent,brutal_kill,worker,[epool_name]},
    {ok, {{one_for_one, 1000, 3600}, [ChildSpec]}}.

%%====================================================================
%% Internal functions
%%====================================================================

%% 添加一个pool
add_pool(Pool_name, PoolSize,MFAs,ChildMods)->
	supervisor:start_child(?SERVER,{
		Pool_name,
		{epool_worker_sup, start_link,[Pool_name, PoolSize,MFAs,ChildMods]},
		transient, 
		2000, 
		supervisor, 
		[epool_worker_sup | ChildMods]
	}).
%% 删除一个pool
%% return ok | {error, Error}
stop_pool(Pool_name)->
	supervisor:terminate_child(?SERVER, Pool_name),
	supervisor:delete_child(?SERVER, Pool_name).








