-module(dijkstra).
-export([table/2, route/2]).

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
  Old_N = entry(Node, Sorted),
  if
    N < Old_N ->
      replace(Node, N, Gateway, Sorted);
    true ->
      Sorted
  end.

% update(Node, N, Gateway, Sorted) ->
%   Len = entry(Node, Sorted),
%   if
%     Len =< N -> Sorted;
%     N < Len ->
%       replace(Node, N, Gateway, Sorted);
%     true -> error
%   end.
%
% updateAll(_, _, [], _, Sorted, _) ->
%   Sorted;
% updateAll(Node, N, [H|T], Map, Sorted, Gateway) ->
%   Sorted2 = update(H, N, Gateway, Sorted), % update(H, N, Node/Gateway, Sorted) ??? senaste eller från början, från början bara paris
%   if Sorted2 == Sorted ->
%     updateAll(Node, N, T, Map, Sorted2, Gateway);
%     true ->
%     Found = lists:keyfind(H, 1, Map),
%     if Found == false ->
%       updateAll(Node, N, T, Map, Sorted2, Gateway);
%       true ->
%         {_, Links} = Found,
%         updateAll(H, N+1, Links, Map, Sorted2, Gateway)
%     end
%   end.
%
iterate([], _, Table) ->
  Table;
iterate([{_, inf, _}|_], _, Table) ->
  Table;
% something broke in special case. going over to presented solution.
% iterate(Sorted, Map, Table) ->
%   [{Node, N, Gateway}|SortedT] = Sorted,
%   Found = lists:keyfind(Node, 1, Map),
%   if Found == false ->
%     iterate(SortedT, Map, [{Node, Gateway}|Table]);
%     true ->
%       {_, Links} = Found,
%       [_|T] = updateAll(Node, N+1, Links, Map, Sorted, Gateway),
%       iterate(T, Map, [{Node, Gateway}|Table])
%   end.

% iterate part by Klas Segeljakt
iterate([{Node, N, Gateway}|Sorted], Map, Table) ->
  Links = map:reachable(Node, Map),
  F = fun(Link, Sorted1) ->
    update(Link, N+1, Gateway, Sorted1)
  end,
  UpdatedSorted = lists:foldl(F, Sorted, Links),
  UpdatedTable = [{Node, Gateway}|Table],
  iterate(UpdatedSorted, Map, UpdatedTable).


table(Gateways, Map) ->
  Nodes = lists:map(fun format_node/1, map:all_nodes(Map)),
  Gates = lists:map(fun format_gateway/1, Gateways),
  Sorted = Gates ++ Nodes,
  io:format("Sorted: ~w~n", [Sorted]),
  iterate(Sorted, Map, []).

format_node(Node) ->
  {Node, inf, unknown}.

format_gateway(Gateway) ->
  {Gateway, 0, Gateway}.

route(Node, Table) ->
  Tuple = lists:keyfind(Node, 1, Table),
  case Tuple of
    false -> notfound;
    Tuple ->
      {Node, Gateway} = Tuple,
      {ok, Gateway}
  end.

%
% Sorted: [{stockholm,0,stockholm},{oslo,0,oslo},{lund,0,lund},{oslo,inf,unknown},{lund,inf,unknown},{amsterdam,inf,unknown},{london,inf,unknown},{stockholm,inf,unknown},{tokyo,inf,unknown},{singapore,inf,unknown}]
%
% Sorted: [{oslo,0,oslo},{stockholm,0,stockholm},{lund,0,lund},{oslo,inf,unknown},{lund,inf,unknown},{amsterdam,inf,unknown},{london,inf,unknown},{stockholm,inf,unknown},{tokyo,inf,unknown},{singapore,inf,unknown}]
%

























%
