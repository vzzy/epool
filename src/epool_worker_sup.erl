%%%-------------------------------------------------------------------
%% @doc epool top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(epool_worker_sup).

-behaviour(supervisor).

%% API
-export([
	start_link/4,
	start_worker/3,
	get_worker/1,
	get_pool_size/1	
]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

-spec start_link(atom(), integer(), integer(), [atom()], {atom(), atom(), [term()]}) -> {ok, pid()} | ignore | {error, term()}.
start_link(PoolName, PoolSize,MFAs,ChildMods) ->
	start_link(PoolName, PoolSize,10*PoolSize,MFAs,ChildMods).
start_link(PoolName, PoolSize,MaxRestarts,MFAs,ChildMods) ->
    Args = [PoolName, PoolSize, MaxRestarts,MFAs,ChildMods],
	SupName = list_to_atom(lists:concat([?MODULE,"_",PoolName])),
	supervisor:start_link({local, SupName}, ?MODULE, Args).

-spec get_worker(atom()) -> pid().
get_worker(PoolName) ->
    [{pool_size, PoolSize}] = ets:lookup(PoolName, pool_size),
    N = ets:update_counter(PoolName, seq, {2, 1, PoolSize, 1}),
    [{N, Worker}] = ets:lookup(PoolName, N),
    Worker.

get_pool_size(PoolName) ->
    [{pool_size, PoolSize}] = ets:lookup(PoolName, pool_size),
    PoolSize.

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([PoolName, PoolSize, MaxRestarts,MFAs,ChildMods]) ->
	PoolTable = ets:new(PoolName, [named_table, public]),
	Len = length(MFAs),
	ets:insert(PoolTable, {pool_size, PoolSize*Len}),
	ets:insert(PoolTable, {seq, 0}),
	{ChildSpec,_} = lists:foldl(fun(MFA,{Temp_ChildSpec,Temp_no})-> 
		Fun = fun(Id) ->
	    	{?MODULE, start_worker, [Id, PoolTable, MFA]}
	    end,								
		{[{N, Fun(N), transient, 2000, worker, ChildMods} || N <- lists:seq((Temp_no-1)*PoolSize+1, Temp_no*PoolSize)] ++ Temp_ChildSpec,Temp_no+1}				
	end,{[],1},MFAs),
    {ok, {{one_for_one, MaxRestarts, PoolSize*Len}, ChildSpec}}.

%%====================================================================
%% Internal functions
%%====================================================================
-spec start_worker(integer(), atom(), {module(), atom(), [term()]}) -> {ok, pid()}.
start_worker(Id, PoolTable, {M, F, A}) ->
    {ok, Pid} = apply(M, F, A),
    ets:insert(PoolTable, {Id, Pid}),
    {ok, Pid}.


