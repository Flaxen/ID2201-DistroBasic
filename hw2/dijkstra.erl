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

updateAll(_, _, [], _, Sorted, _) ->
  Sorted;
updateAll(Node, N, [H|T], Map, Sorted, Gateway) ->
  Sorted2 = update(H, N, Gateway, Sorted), % update(H, N, Node, Sorted) ??? senaste eller från början, från början bara paris
  Found = lists:keyfind(H, 1, Map),
  if Found == false ->
    updateAll(Node, N, T, Map, Sorted2, Gateway);
    true ->
      {_, Links} = Found,
      updateAll(H, N+1, Links, Map, Sorted2, Gateway)
  end.

iterate([], _, Table) ->
  Table;
iterate([{_, inf, _}|_], _, Table) ->
  Table;
iterate(Sorted, Map, Table) ->
  [{Node, N, Gateway}|SortedT] = Sorted,
  Found = lists:keyfind(Node, 1, Map),
  if Found == false ->
    iterate(SortedT, Map, [{Node, Gateway}|Table]);
    true ->
      {_, Links} = Found,
      [_|T] = updateAll(Node, N+1, Links, Map, Sorted, Gateway),
      iterate(T, Map, [{Node, Gateway}|Table])
  end.



































%
