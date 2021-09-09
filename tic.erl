-module(tic).
-export([hello/0, world/0, first/0]).

world()->
  "Hello World!".

hello()->
  receive
    X -> io:format("aaa! a message: ~s~n", [X])
  end.

first()->
  receive
    {tic, X} ->
      io:format("tic: ~w~n", [X]),
      second()
  end.

second()->
  receive
    {tac, X} ->
      io:format("tac: ~w~n", [X]),
      last();
    {toe, X} ->
      io:format("toe: ~w~n", [X]),
      last()
  end.

last() ->
  receive
    X ->
      io:format("end: ~w~n", [X])
  end.
