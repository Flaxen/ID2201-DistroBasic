-module(dijkstra).
-export([update/4, iterate/3]).

entry(Node, Sorted) ->
  Tuple = lists:keyfind(Node, 1, Sorted),
  case Tuple of
    false -> 0;
    {_, Len, _} -> Len
  end.

replace(Node, N, Gateway, Sorted) ->
  L1 = lists:keydelete(Node, 1, Sorted),
  L2 = [{Node, N, Gateway}|L1],
  lists:keysort(2, L2).

update(Node, N, Gateway, Sorted) ->
  Len = entry(Node, Sorted),
  if
    Len =< N -> Sorted;
    N < Len ->
      replace(Node, N, Gateway, Sorted);
    true -> error
  end.

updateAll(_, _, [], _, Sorted) ->
  Sorted;
updateAll(Node, N, [H|T], Map, Sorted) ->
  Sorted2 = update(H, N, Node, Sorted),
  {_, Links} = lists:keyfind(H, 1, Map),
  Sorted3 = updateAll(H, N+1, Links, Map, Sorted2),
  updateAll(Node, N, T, Map, Sorted3).

iterate([], _, Table) ->
  Table;
iterate([{_, inf, _}|_], _, Table) ->
  Table;
iterate(Sorted, Map, Table) -> % sorted, map, table
  [{Node, N, Gateway}|SortedT] = Sorted,
  {_, Links} = lists:keyfind(Node, 1, Map),

  [H|T] = updateAll(Node, N+1, Links, Map, Sorted),
  iterate(T, Map, [{Node, Gateway}|Table]).



  % lägg till alla för nodens barn
  % lägg till nod



































%
