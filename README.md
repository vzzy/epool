epool
=====

An OTP application

Build
-----

    $ rebar3 compile
    $ rebar3 shell
    > epool:start().
    ok

	%% 添加一个pool(约定:同项目里不允许同名池，否则覆盖)
	%% 注:请不要超级频繁添加同名池，因为是延迟关闭，会导致大量进程滞留。
	%% @PoolName atom
	%% @PoolSize int
	%% @ChildMFA {M,F,A}
	%% @ChildMods [atom]  module name
	%% return {ok,Pid} | {ok,Pid,Info} | {error,Reason}
	epool:add_pool(Pool_name,PoolSize,ChildMFA,ChildMods).
	
	%% 删除一个pool
	%% return ok
	epool:remove_pool(Pool_name).
	
	%% 获取工作进程
	%% @PoolName atom
	%% return {ok,Pid} | {error,Reason}
	epool:get_worker(Pool_name).      
	    
	    
	    