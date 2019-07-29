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
	%% @PoolSize int    池大小 = PoolSize * len(MFAs)
	%% @MFAs [{M,F,A}]
	%% @ChildMods [atom]  module name
	%% return {ok,Pid} | {error,Reason}
	epool:add_pool(Pool_name,PoolSize,MFAs,ChildMods).
	
	%% 删除一个pool
	%% return ok
	epool:stop_pool(Pool_name).
	
	%% 获取工作进程
	%% @PoolName atom
	%% return {ok,Pid} | {error,Reason}
	epool:get_worker(Pool_name).  
	
	%% 获取池大小
	%% @PoolName atom
	%% return int 
	epool:get_pool_size(Pool_name).    
	    
	%% 用法举例
	epool:add_pool(demo,1,[{epool_demo,start_link,[1]},{epool_demo,start_link,[2]}],[epool_demo]).   
	{ok,Pid} = epool:get_worker(demo). 
	epool_demo:test(demo). 
	epool:get_pool_size(demo). 
	epool:stop_pool(demo).
	
	
	
	
	   
	    