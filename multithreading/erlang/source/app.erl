-module(app).
-export([main/0]).

main() ->
    application:ensure_all_started(supervisor_workers),
    Numbers = lists:seq(10000000000000061, 10000000000000081),
    supervisor_workers:start(5, Numbers),
    timer:sleep(40000),
    supervisor_workers:stop().