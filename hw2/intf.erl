-module(intf).
-export([new/0, remove/2, add/4, lookup/2, ref/2, name/2, list/1, broadcast/2]).

new() ->
  [].

add(Name, Ref, Pid, Intf) ->
  [{Name, Ref, Pid} | Intf].

remove(Name, Intf) ->
  lists:keydelete(Name, 1, Intf).

lookup(Name, Intf) ->
  Tuple = lists:keyfind(Name, 1, Intf),
  case Tuple of
    false -> notfound;
    Tuple -> {_, _, Pid} = Tuple,
    {ok, Pid}
  end.

ref(Name, Intf) ->
  Tuple = lists:keyfind(Name, 1, Intf),
  case Tuple of
    false -> notfound;
    Tuple -> {_, Ref, _} = Tuple,
    {ok, Ref}
  end.

name(Ref, Intf) ->
  Tuple = lists:keyfind(Ref, 2, Intf),
  case Tuple of
    false -> notfound;
    Tuple -> {Name, _, _} = Tuple,
    {ok, Name}
  end.

list(Intf) ->
  lists:map(fun({Name, _, _}) -> Name end, Intf).

broadcast(Message, Intf) ->
  List = list(Intf),
  broadcastList(Message, List, Intf).

% help functions
broadcastList(_, [], _) ->
  ok;
broadcastList(Message, [H|T], Intf) ->
  {ok, Pid} = lookup(H, Intf),
  Pid ! Message,
  broadcastList(Message, T, Intf).


















%
