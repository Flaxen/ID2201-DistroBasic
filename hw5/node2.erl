-module(node2).
-export([start/1, start/2]).
-define(Stabilize, 1000).
-define(Timeout, 10000).

start(Id) ->
  start(Id, nil).

start(Id, Peer) ->
  timer:start(),
  spawn(fun() -> init(Id, Peer) end).

init(Id, Peer) ->
  Predecessor = nil,
  {ok, Successor} = connect(Id, Peer),
  schedule_stabalize(),
  node(Id, Predecessor, Successor, storage:create()).

connect(Id, nil) ->
  {ok, {Id, self()}};
connect(_, Peer) ->
  Qref = make_ref(),
  Peer ! {key, Qref, self()},
  receive
    {Qref, Skey} ->
      {ok, {Skey, Peer}}
    after ?Timeout ->
      io:format("Time out: no response~n", [])
  end.

node(Id, Predecessor, Successor, Store) ->
  receive
    {key, Qref, Peer} ->
      % io:format("Node ~w: got key from ~w ~n", [Id, Peer]),
      Peer ! {Qref, Id},
      node(Id, Predecessor, Successor, Store);

    {notify, New} ->
      {Pred, PreStore} = notify(New, Id, Predecessor, Store),
      node(Id, Pred, Successor, PreStore);

    {request, Peer} ->
      request(Peer, Predecessor),
      node(Id, Predecessor, Successor, Store);

    {status, Pred} ->
      % io:format("Pred in status msg: ~w~n", [Pred]),
      Succ = stabilize(Pred, Id, Successor),
      node(Id, Predecessor, Succ, Store);

    stabilize ->
      stabilize(Successor),
      node(Id, Predecessor, Successor, Store);

    probe ->
      create_probe(Id, Successor, Store),
      node(Id, Predecessor, Successor, Store);

    {probe, Id, Nodes, T} ->
      remove_probe(T, Nodes),
      node(Id, Predecessor, Successor, Store);

    {probe, Ref, Nodes, T} ->
      forward_probe(Ref, T, Nodes, Id, Successor, Store),
      node(Id, Predecessor, Successor, Store);

    {add, Key, Value, Qref, Client} ->
      Added = add(Key, Value, Qref, Client, Id, Predecessor, Successor, Store),
      node(Id, Predecessor, Successor, Added);

    {lookup, Key, Qref, Client} ->
      lookup(Key, Qref, Client, Id, Predecessor, Successor, Store),
      node(Id, Predecessor, Successor, Store);

    {handover, Elements} ->
      Merged = storage:merge(Store, Elements),
      node(Id, Predecessor, Successor, Merged);

    stop ->
      ok;

    Msg ->
      io:format("strange message: ~w~n", [Msg])
  end.

create_probe(Id, {_, Spid}, Store) ->
  Time = erlang:system_time(micro_seconds),
  Spid ! {probe, Id, [{Id, Store}], Time}.

remove_probe(T, Nodes) ->
  Time = erlang:system_time(micro_seconds) - T,
  io:format("Probe done, Time in micro_seconds: ~w~n Nodes: ~w~n", [Time, Nodes]).

forward_probe(Ref, T, Nodes, Id, {_, Spid}, Store) ->
  Spid ! {probe, Ref, lists:append(Nodes, [{Id, Store}]), T}.

stabilize({_, Spid}) ->
  Spid ! {request, self()}.

stabilize(Pred, Id, Successor) ->
  {Skey, Spid} = Successor,
  case Pred of
    nil ->
      Spid ! {notify, {Id, self()}}, % we're before some pleb in queue
      Successor;

    {Id, _} ->
      Successor;

    {Skey, _} ->
      Spid ! {notify, {Id, self()}},
      Successor;

    {Xkey, Xpid} ->
      case key:between(Xkey, Id, Skey) of
        true ->

          % io:format("Xpid in stab: ~w~n", [Xpid]),
          Xpid ! {request, self()}, % before us in queue
		      Pred;

        false ->
          Spid ! {notify, {Id, self()}},
          Successor
      end
  end.

schedule_stabalize() ->
  timer:send_interval(?Stabilize, self(), stabilize).

request(Peer, Predecessor) ->
  case Predecessor of
    nil ->
      Peer ! {status, nil};

    {Pkey, Ppid} ->
      Peer ! {status, {Pkey, Ppid}}
  end.

notify({Nkey, Npid}, Id, Predecessor, Store) ->
  case Predecessor of
    nil ->
      Keep = handover(Id, Store, Nkey, Npid),
      {{Nkey, Npid}, Keep};

    {Pkey, _} ->
      case key:between(Nkey, Pkey, Id) of
        true ->
          Keep = handover(Id, Store, Nkey, Npid),
          {{Nkey, Npid}, Keep};

        false ->
          {Predecessor, Store}
      end
  end.

add(Key, Value, Qref, Client, Id, {Pkey, _}, {_, Spid}, Store) ->
  case key:between(Key, Pkey, Id) of
    true ->
      Client ! {Qref, ok},
      storage:add(Key, Value, Store);
    false ->
      Spid ! {add, Key, Value, Qref, Client},
      Store
  end.

lookup(Key, Qref, Client, Id, {Pkey, _}, Successor, Store) ->
  case key:between(Key, Pkey, Id) of
    true ->
      % io:format("Key: ~w~n Store: ~w~n", [Key, Store]),
      Result = storage:lookup(Key, Store),
      Client ! {Qref, Result};
    false ->
      {_, Spid} = Successor,
      % io:format("ID: ~w: Cant find, redirecting~n", [Id]),
      Spid ! {lookup, Key, Qref, Client}
  end.

handover(Id, Store, Nkey, Npid) ->
  {Rest, Keep} = storage:split(Id, Nkey, Store),
  Npid ! {handover, Rest},
  Keep.













%
