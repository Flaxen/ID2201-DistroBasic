-module(wait).
-export([hello/0, ping/0, pong/0]).

hello()->
  receive
    X -> io:format("aaa! a message: ~s~n", [X])
  end.


ping() ->
  {ponger, 'bob@192.168.68.123'} ! self(),
  receive
    X -> io:format("aaa! a message: ~s~n", [X])
  end.

pong() ->
  receive
    PID -> PID ! "pong!"
  end.
