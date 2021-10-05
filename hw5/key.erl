-module(key).
-export([generate/0, between/3]).

generate() ->
  rand:uniform(1000000000).

between(Key, From, To) ->
  if
    (Key =< To) and (Key > From) ->
      true;
    (From > To) and ((Key =< To) or (Key > From)) ->
      true;
    From == To ->
      true;
    true ->
      false
  end.
