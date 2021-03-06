-module(gms3).
-export([leader/5, start/1, start/2, init/3, init/4, slave/7]).
-define(timeout, 1000).
-define(arghh, 100).

start(Id) ->
  Rnd = random:uniform(1000),
  Self = self(),
  {ok, spawn_link(fun() -> init(Id, Rnd, Self) end)}.

start(Id, Grp) ->
  Rnd = random:uniform(1000),
  Self = self(),
  {ok, spawn_link(fun() -> init(Id, Grp, Rnd, Self) end)}.

init(Id, Rnd, Master) ->
  random:seed(Rnd, Rnd, Rnd),
  leader(Id, Master, [], 1, [Master]).

init(Id, Grp, Rnd, Master) ->
  random:seed(Rnd, Rnd, Rnd),
  Self = self(),
  Grp ! {join, Master, Self},
  receive
    {view, N, [Leader|Slaves], Group} ->
      erlang:monitor(process, Leader),
      Master ! {view, Group},
      slave(Id, Master, Leader, N+1, {view, N, [Leader|Slaves], Group}, Slaves, Group)

  after ?timeout ->
    Master ! {error, "no reply from leader"}
  end.

leader(Id, Master, Slaves, N, Group) ->
  receive
    {mcast, Msg} ->
      bcast(Id, {msg, N, Msg}, Slaves),
      Master ! Msg,
      leader(Id, Master, Slaves, N+1, Group);
    {join, Wrk, Peer} ->
      Slaves2 = lists:append(Slaves, [Peer]),
      Group2 = lists:append(Group, [Wrk]),
      bcast(Id, {view, N, [self()|Slaves2], Group2}, Slaves2),
      Master ! {view, Group2},
      leader(Id, Master, Slaves2, N+1, Group2);
    stop ->
      ok
  end.

slave(Id, Master, Leader, N, Last, Slaves, Group) ->
  receive
    {mcast, Msg} ->
      Leader ! {mcast, Msg},
      slave(Id, Master, Leader, N, Last, Slaves, Group);

    {join, Wrk, Peer} ->
      Leader ! {join, Wrk, Peer},
      slave(Id, Master, Leader, N, Last, Slaves, Group);

    {msg, MsgN, Msg} ->
      case MsgN == N of
        true ->
          Master ! Msg,
          slave(Id, Master, Leader, N+1, {msg, MsgN, Msg}, Slaves, Group);
        false ->
          slave(Id, Master, Leader, N, Last, Slaves, Group)
      end;

    {view, ViewN, [Leader|Slaves2], Group2} ->
      case ViewN == N of
        true ->
          Master ! {view, Group2},
          slave(Id, Master, Leader, N+1, {view, ViewN, [Leader|Slaves2], Group2}, Slaves2, Group2);
        false ->
          slave(Id, Master, Leader, N, Last, Slaves2, Group2)
      end;

    {'DOWN', _Ref, process, Leader, _Reason} ->
      election(Id, Master, N, Last, Slaves, Group);

    stop ->
      ok
  end.

election(Id, Master, N, Last, Slaves, [_|Group]) ->
  Self = self(),
  case Slaves of
    [Self|Rest] ->
      bcast(Id, Last, Rest),
      bcast(Id, {view, N, Slaves, Group}, Rest),
      Master ! {view, Group},
      leader(Id, Master, Rest, N+1, Group);
    [Leader|Rest] ->
      erlang:monitor(process, Leader),
      slave(Id, Master, Leader, N, Last, Rest, Group)
  end.



bcast(Id, Msg, Nodes) ->
  lists:foreach(fun(Node) -> Node ! Msg, crash(Id) end, Nodes).

crash(Id) ->
  case random:uniform(?arghh) of
    ?arghh ->
      io:format("leader ~w: crash ~n", [Id]),
      exit(no_luck);
    _ ->
      ok
  end.
