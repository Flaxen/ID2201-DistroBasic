-module(time).
-export([zero/0, inc/2, merge/2, leq/2]).

zero() ->
  0.

inc(Name, T) ->
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

% assumption Node is identifier not actual node.
clock(Nodes) ->
  Clock = lists:map(fun(X) -> {zero(), X} end, Nodes).

update(Node, Time, Clock) ->
  list:keyreplace(Node, 2, Clock, {Time, Node}).

safe(Time, Clock) ->
  
