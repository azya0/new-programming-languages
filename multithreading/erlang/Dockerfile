FROM erlang

WORKDIR /app

COPY ./source ./src/

RUN mkdir -p ebin && \
    mv src/*.app.src ebin/supervisor_workers.app 2>/dev/null || true && \
    erlc -o ebin/ src/*.erl

CMD ["erl", "-noshell", "-pa", "ebin", \
     "-eval", \
     "Numbers = lists:seq(10000000000000061, 10000000000000081), \
      case supervisor_workers:start(5, Numbers) of \
        {ok, _Pid} -> \
          timer:sleep(5000), \
          supervisor_workers:stop(), \
          init:stop(); \
        Error -> \
          io:format(\"Error: ~p~n\", [Error]), \
          init:stop(1) \
      end."]