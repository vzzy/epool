%% @author bai
%% @doc @todo Add description to epool_demo.


-module(epool_demo).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([
	start_link/1,
	test/1		 
]).
-record(state, {
	no				
}).

start_link(No)->
	gen_server:start_link(?MODULE, [No], []).

test(Poolname)->
	case epool:get_worker(Poolname) of
		{ok,Pid} ->
			gen_server:cast(Pid,test);
		R ->
			R
	end.

init([No]) ->
    {ok, #state{
		no = No			
	}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, #state{no = No} = State) ->
	io:format("no........~p~n",[No]),
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


