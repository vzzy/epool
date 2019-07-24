epool
=====

An OTP application

Build
-----

    $ rebar3 compile
    $ rebar3 shell
    > epool:start().
    ok

	epool:add_pool(Pool_name,PoolSize,ChildMFA,ChildMods).
	
	epool:remove_pool(Pool_name).
	
	epool:get_worker(Pool_name).      
	    
	    
	    