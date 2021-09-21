-module(hist).
-export([new/1, update/3]).

new(Name) ->
  [{Name, inf}].

update(Node, N, History) ->
  Tuple = lists:keyfind(Node, 1, History),
  case Tuple of
    false -> {new, [{Node, N}|History]};
    Tuple -> {_, N2} = Tuple,
    if N2 < N ->
      Updated = lists:keyreplace(Node, 1, History, {Node, N}),
      {new, Updated};
    true->
      old
    end
  end.
