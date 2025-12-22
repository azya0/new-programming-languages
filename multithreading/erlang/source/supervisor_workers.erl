-module(supervisor_workers).
-behaviour(supervisor).
-export([start/2, stop/0, init/1]).
-export([process_numbers/2]).

start(N, Numbers) ->
    case supervisor:start_link({local, ?MODULE}, ?MODULE, [N]) of
        {ok, Pid} ->
            process_numbers(Pid, Numbers),
            {ok, Pid};
        Error -> Error
    end.

stop() ->
    exit(whereis(?MODULE), normal).

init([N]) ->
    SupFlags = #{strategy => one_for_one, intensity => 5, period => 10},
    ChildSpecs = lists:map(fun(I) ->
        #{id => list_to_atom("worker_" ++ integer_to_list(I)),
          start => {worker, start_link, []},
          restart => permanent,
          shutdown => 5000,
          type => worker}
    end, lists:seq(1, N)),
    {ok, {SupFlags, ChildSpecs}}.

process_numbers(Supervisor, Numbers) ->
    Workers = supervisor:which_children(Supervisor),
    Pids = [Pid || {_, Pid, _, _} <- Workers],
    Results = lists:map(fun(Number) ->
        Worker = lists:nth(rand:uniform(length(Pids)), Pids),
        worker:process_number(Worker, Number)
    end, Numbers),
    io:format("Results: ~p~n", [Results]).
