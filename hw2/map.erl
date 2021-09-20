-module(map).
-export([new/0, update/3, reachable/2, all_nodes/1]).

new() ->
  [].

update(Node, Links, Map) ->
  Map2 = lists:keydelete(Node, 1, Map),
  [{Node, Links} | Map2].

reachable(Node, Map) ->
  Tuple = lists:keyfind(Node, 1, Map),
  case Tuple of
    false -> [];
    Tuple -> {_, List} = Tuple,
    List
end.

all_nodes(Map) ->
  lists:foldl(fun mergeUnique/2, [], Map).


% help functions
mergeNode(Node, List) ->
  Bool = lists:member(Node, List),
  if Bool == false ->
    [Node|List];
  true -> List
end.

mergeTuple([], List) ->
  List;
mergeTuple([H|T], List) ->
  Bool = lists:member(H, List),
  if Bool == false ->
    mergeTuple(T, [H|List]);
  true ->
    mergeTuple(T, List)
  end.

mergeUnique({Node, Links}, List) ->
  L2 = mergeNode(Node, List),
  mergeTuple(Links, L2).






































%
