%% @author bai
%% @doc @todo Add description to epool.


-module(epool).

%% ====================================================================
%% API functions
%% ====================================================================
-export([
	start/0,	 
		 
	get_worker/1,
				 
	add_pool/4,
	remove_pool/1,
	gc_pool_list/0	
]).

%% ====================================================================
%% Internal functions
%% ====================================================================

%% 启动方法
start()->
	%% 含连接从节点过程。
	ok = start(?MODULE),
	ok.

%% 获取工作进程
%% @PoolName atom
%% return {ok,Pid} | {error,Reason}
get_worker(PoolName)->
	case ets:lookup(epool_name, PoolName) of
		[{PoolName, TempPoolName}] ->
			case catch epool_worker_sup:get_worker(TempPoolName) of
				Pid when is_pid(Pid) ->
					{ok,Pid};
				{'exit',R}->
					{error,R};
				Others->
					{error,Others}
			end;
		_ ->
			Reason = list_to_binary(lists:concat([PoolName,"is not exist"])),
			{error,Reason}
	end.
	
%% 添加一个pool(约定:同项目里不允许同名池，否则覆盖)
%% @PoolName atom
%% @PoolSize int
%% @ChildMFA {M,F,A}
%% @ChildMods [atom]  module name
%% return {ok,Pid} | {ok,Pid,Info} | {error,Reason}
add_pool(Pool_name, PoolSize,ChildMFA,ChildMods)->
	epool_name:add_pool(Pool_name, PoolSize,ChildMFA,ChildMods).
%% 删除一个pool
%% return ok 
remove_pool(Pool_name)->
	epool_name:remove_pool(Pool_name),
	ok.
%% 未被回收的池
%% return []
gc_pool_list()->
	epool_name:gc_pool_list().

	

%% 启动App
start(App) ->
    start_ok(App, application:start(App, permanent)).
start_ok(_App, ok) -> ok;
start_ok(_App, {error, {already_started, _App}}) -> ok;
start_ok(App, {error, {not_started, Dep}}) ->
    ok = start(Dep),
    start(App);
start_ok(App, {error, Reason}) ->
    erlang:error({aps_start_failed, App, Reason}).

