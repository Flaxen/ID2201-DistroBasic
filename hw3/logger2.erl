-module(logger2).
-export([start/1, stop/1, logSafe/3]).

start(Nodes) ->
  % {ok, Fd} = file:open("out.txt", [append]),
  spawn_link(fun() -> init(Nodes) end).

stop(Logger) ->
  Logger ! stop.

init(Nodes) ->
  Clock = time:clock(Nodes), % chagne here for vect revert
  loop(Clock, []).

loop(Clock, HoldbackQueue) ->
  receive
    {log, From, Time, Msg} ->
      C1 = time:update(From, Time, Clock),  % change here for vect
      HQ1 = logSafe(C1, [], lists:keysort(3, [{log, From, Time, Msg}|HoldbackQueue])),
      % io:format("C1 ~w~n, HQ1 ~w~n", [C1, HQ1]),

      % log(From, Time, Msg),
      % loop(Clock, HoldbackQueue);
      loop(C1, HQ1);

    stop->
      ok
  end.

logSafe(_, Unsafe, []) ->
  Unsafe;
logSafe(Clock, Unsafe, [{log, From, Time, Msg}|T]) ->

  % Length = length([{log, From, Time, Msg}|T]),
  % io:format("~w~n", [Length]),
  %
  % file:write(Fd,integer_to_list(Length) ++ "\n"),

  Bool = time:safe(Time, Clock),  % change here for revert vect
  if
    Bool == true ->
      log(From, Time, Msg),
      U1 = Unsafe;
    true ->
      U1 = [{log, From, Time, Msg}|Unsafe]
  end,
  logSafe(Clock, U1, T).

log(From, Time, Msg) ->
  io:format("log: ~w ~w ~p~n", [From, Time, Msg]).






































%
