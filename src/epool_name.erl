%% @author bai
%% @doc @todo Add description to epool_name.


-module(epool_name).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-define(INTERVAL, 60*1000).
%% ====================================================================
%% API functions
%% ====================================================================
-export([
	start_link/0,
	add_pool/4,
	stop_pool/1,
	
	gc_pool_list/0		 
]).

%% ====================================================================
%% Behavioural functions
%% ====================================================================
-record(state, {
	tref = none,				

	last_time = 0, 
	seq = 1,				

	gc_pool_list = []				
}).

start_link()->
	gen_server:start_link({local,?MODULE}, ?MODULE, [], []). 

%% 添加一个pool，如果存在老的同名pool(默认同名即同服务)，会延迟关闭
add_pool(Pool_name,PoolSize,MFAs,ChildMods)->
	gen_server:call(?MODULE,{add_pool,Pool_name,PoolSize,MFAs,ChildMods}).

%% 删除Pool
stop_pool(Pool_name)->
	gen_server:cast(?MODULE,{stop_pool,Pool_name}).

%% gc列表
gc_pool_list()->
	gen_server:call(?MODULE,gc_pool_list).


%% init/1
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:init-1">gen_server:init/1</a>
-spec init(Args :: term()) -> Result when
	Result :: {ok, State}
			| {ok, State, Timeout}
			| {ok, State, hibernate}
			| {stop, Reason :: term()}
			| ignore,
	State :: term(),
	Timeout :: non_neg_integer() | infinity.
%% ====================================================================
init([]) ->
	TRef = erlang:start_timer(?INTERVAL, self(), gc),
    {ok, #state{
		tref = TRef			
	}}.


%% handle_call/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_call-3">gen_server:handle_call/3</a>
-spec handle_call(Request :: term(), From :: {pid(), Tag :: term()}, State :: term()) -> Result when
	Result :: {reply, Reply, NewState}
			| {reply, Reply, NewState, Timeout}
			| {reply, Reply, NewState, hibernate}
			| {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason, Reply, NewState}
			| {stop, Reason, NewState},
	Reply :: term(),
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity,
	Reason :: term().
%% ====================================================================
handle_call(gc_pool_list, _From, #state{gc_pool_list = Gc_pool_list} = State) ->
	Reply = [P || {P,_T} <-Gc_pool_list],
	{reply, Reply, State};
handle_call({add_pool,PoolName,PoolSize,MFAs,ChildMods}, _From, #state{last_time = Last_time,seq = Seq,gc_pool_list = Gc_pool_list} = State) ->
	Now = get_timestamp(),
	New_Gc_pool_list = case ets:lookup(epool_name, PoolName) of
		[{PoolName, TempPoolName}] ->
			[{TempPoolName,Now}] ++ Gc_pool_list;
		_ ->
			Gc_pool_list
	end,
	New_Seq = if
	  	Now =:= Last_time ->
			1;
	  	true->
			Seq + 1		  
	end,
	
	New_TempPoolName = list_to_atom(lists:concat([PoolName,"_",Now,"_",New_Seq])),
	Ret = epool_sup:add_pool(New_TempPoolName, PoolSize,MFAs,ChildMods),
	New_State = case Ret of
		{error,_}->
			State;
		_-> %% 成功启动
			ets:insert(epool_name, {PoolName, New_TempPoolName}),
			State#state{
				last_time = Now,
				seq = New_Seq,
				gc_pool_list = New_Gc_pool_list			
			}
	end,
	New_Ret = case Ret of
		{ok,Child,_}->
			{ok,Child};
		_->
			Ret
	end,
	{reply, New_Ret, New_State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.


%% handle_cast/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_cast-2">gen_server:handle_cast/2</a>
-spec handle_cast(Request :: term(), State :: term()) -> Result when
	Result :: {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason :: term(), NewState},
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity.
%% ====================================================================
handle_cast({stop_pool,PoolName},#state{gc_pool_list = Gc_pool_list} = State) ->
	Now = get_timestamp(),
	New_Gc_pool_list = case ets:lookup(epool_name, PoolName) of
		[{PoolName, TempPoolName}] ->
			ets:delete(epool_name, PoolName),
			[{TempPoolName,Now}] ++ Gc_pool_list;
		_ ->
			Gc_pool_list
	end,
	{noreply,State#state{
		gc_pool_list = New_Gc_pool_list			
	}};
handle_cast(_Msg, State) ->
    {noreply, State}.


%% handle_info/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_info-2">gen_server:handle_info/2</a>
-spec handle_info(Info :: timeout | term(), State :: term()) -> Result when
	Result :: {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason :: term(), NewState},
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity.
%% ====================================================================
handle_info({timeout, _TRef, gc}, #state{tref = OldTRef,gc_pool_list = Gc_pool_list} = State) ->
	Now = get_timestamp(),
	New_Gc_pool_list = lists:foldl(fun({TempPoolName,Time} = E,Temp)-> 
		if
			Now-Time>?INTERVAL-> %% 超出间隔，回收
				epool_sup:stop_pool(TempPoolName),
				Temp;
			true->
				[E] ++ Temp
		end
	end,[],Gc_pool_list),
	
	catch erlang:cancel_timer(OldTRef),
	TRef = erlang:start_timer(?INTERVAL, self(), gc),
    {noreply, State#state{
		tref = TRef,
		gc_pool_list = New_Gc_pool_list					  
	}};
handle_info(_Info, State) ->
    {noreply, State}.


%% terminate/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:terminate-2">gen_server:terminate/2</a>
-spec terminate(Reason, State :: term()) -> Any :: term() when
	Reason :: normal
			| shutdown
			| {shutdown, term()}
			| term().
%% ====================================================================
terminate(_Reason, _State) ->
    ok.


%% code_change/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:code_change-3">gen_server:code_change/3</a>
-spec code_change(OldVsn, State :: term(), Extra :: term()) -> Result when
	Result :: {ok, NewState :: term()} | {error, Reason :: term()},
	OldVsn :: Vsn | {down, Vsn},
	Vsn :: term().
%% ====================================================================
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% ====================================================================
%% Internal functions
%% ====================================================================
%%获取从公元1970年到当前时间的毫秒数
get_timestamp() ->
	{MegaSecs,Secs,Micro} = os:timestamp(),
	Micros = (MegaSecs*1000000 + Secs)*1000000 + Micro,
	Micros div 1000.

