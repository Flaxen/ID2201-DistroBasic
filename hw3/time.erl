-module(time).
-export([zero/0, inc/2, merge/2, leq/2, clock/1, update/3, safe/2]).

zero() ->
  0.

inc(_, T) ->
  T+1.

merge(Ti, Tj) ->
  if Ti > Tj ->
    Ti;
  true ->
    Tj
  end.

leq(Ti, Tj) ->
  if Ti =< Tj ->
    true;
  true ->
    false
  end.

clock(Nodes) ->
  lists:map(fun(X) -> {X, zero()} end, Nodes).

update(Node, Time, Clock) ->
  lists:keystore(Node, 1, Clock, {Node, Time}).

safe(Time, Clock) ->
  L1 = lists:filter(fun({_, N}) -> not leq(Time, N) end, Clock),
  if
    L1 == [] ->
      true;
    true ->
      false
  end.





















%
