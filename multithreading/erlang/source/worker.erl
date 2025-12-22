-module(worker).
-behaviour(gen_server).
-export([start_link/0, process_number/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

start_link() ->
    gen_server:start_link(?MODULE, [], []).

process_number(Pid, Number) ->
    gen_server:call(Pid, {process, Number}).

init([]) ->
    erlang:send_after(30000, self(), terminate),
    {ok, #{}}.

handle_call({process, Number}, _From, State) ->
    Result = is_prime(Number),
    {reply, {Number, Result}, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(terminate, State) ->
    {stop, normal, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

is_prime(N) when N < 2 -> false;
is_prime(2) -> true;
is_prime(N) when N rem 2 =:= 0 -> false;
is_prime(N) -> is_prime(N, 3).

is_prime(N, K) when K * K > N -> true;
is_prime(N, K) when N rem K =:= 0 -> false;
is_prime(N, K) -> is_prime(N, K + 2).