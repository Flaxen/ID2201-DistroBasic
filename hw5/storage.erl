-module(storage).
-export([create/0, add/3, lookup/2, split/3, merge/2]).

create() ->
  [].

add(Key, Value, Store) ->
  [{Key, Value} | Store].

lookup(Key, Store) ->
  lists:keyfind(Key, 1, Store).

split(From, To, Store) ->
  Out = lists:partition(fun({Key, _}) -> (Key > From) and (Key =< To) end, Store),
  % io:format("Out split: ~w~n", [Out]),
  Out.

merge(Entries, Store) ->
  lists:append(Entries, Store).
