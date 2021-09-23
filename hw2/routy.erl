-module(routy).
-export([start/2, stop/1, router/6]).

start(Reg, Name) ->
  register(Reg, spawn(fun() -> init(Name) end)).

stop(Node) ->
  Node ! stop,
  unregister(Node).

init(Name) ->
  Intf = intf:new(),
  Map = map:new(),
  Table = dijkstra:table(Intf, Map),
  Hist = hist:new(Name),
  router(Name, 0, Hist, Intf, Table, Map).

router(Name, N, Hist, Intf, Table, Map) ->
  receive

    {route, Name, From, Message} ->
      io:format("~w: received message: (~w) from: ~w~n", [Name, Message, From]),
      router(Name, N, Hist, Intf, Table, Map);
    {route, To, From, Message} ->
      io:format("~w: routing message (~w)", [Name, Message]),
      case dijkstra:route(To, Table) of
        {ok, Gw} ->
          io:format(" via: ~w~n", [Gw]),
          case intf:lookup(Gw, Intf) of
            {ok, Pid} ->
              Pid ! {route, To, From, Message};

            notfound ->
              ok
          end;
        notfound ->
          ok
      end,
      router(Name, N, Hist, Intf, Table, Map);

    {send, To, Message} ->
      self() ! {route, To, Name, Message},
      router(Name, N, Hist, Intf, Table, Map);

    {links, Node, R, Links} ->
      % io:format("~w: 1\n", [Name]),
      % Case = hist:update(Node, R, Hist),
      % io:format("Node: ~w, R: ~w, Case: ~w", [Node, R, Case]),
      case hist:update(Node, R, Hist) of
        {new, Hist1} ->
          % io:format("~w: 2\n", [Name]),
          intf:broadcast({links, Node, R, Links}, Intf),
          % io:format("~w: Node: ~w, Links: ~w, Map: ~w~n", [Name, Node, Links, Map]),
          Map1 = map:update(Node, Links, Map),
          io:format("~w: new map: ~w\n", [Name, Map1]),
          router(Name, N, Hist1, Intf, Table, Map1);
        old ->
          % io:format("~w: 4\n", [Name]),
          router(Name, N, Hist, Intf, Table, Map)
        % _ ->
          % io:format("~w: no match\n", [Name])
      end;

    {add, Node, Pid} ->
      Ref = erlang:monitor(process, Pid),
      Intf1 = intf:add(Node, Ref, Pid, Intf),
      router(Name, N, Hist, Intf1, Table, Map);

    {remove, Node} ->
      {ok, Ref} = intf:ref(Node, Intf),
      erlang:demonitor(Ref),
      Intf1 = intf:remove(Node, Intf),
      router(Name, N, Hist, Intf1, Table, Map);

    {'DOWN', Ref, process, _, _} ->
      {ok, Down} = intf:name(Ref, Intf),
      io:format("~w: exit received from ~w~n", [Name, Down]),
      Intf1 = intf:remove(Down, Intf),
      router(Name, N, Hist, Intf1, Table, Map);

    {status, {NameIN, NIN, HistIN, IntfIN, TableIN, MapIN}} ->
      io:format("~w: Name: ~w\n, N: ~w\n, Hist: ~w\n, Intf: ~w\n, Table: ~w\n, Map: ~w\n", [Name, NameIN, NIN, HistIN, IntfIN, TableIN, MapIN]),
      router(Name, N, Hist, Intf, Table, Map);

    {getstatus, Pid} ->
      Pid ! {status, self()},
      router(Name, N, Hist, Intf, Table, Map);

    {status, From} ->
      From ! {status, {Name, N, Hist, Intf, Table, Map}},
      router(Name, N, Hist, Intf, Table, Map);

    update ->
      Table1 = dijkstra:table(intf:list(Intf), Map),
      % io:format("~w: based on intf:list(Intf): ~w, new table: ~w\n", [Name, intf:list(Intf), Table1]),
      router(Name, N, Hist, Intf, Table1, Map);

    table ->
      io:format("~w: Table: ~w\n", [Name, Table]),
      router(Name, N, Hist, Intf, Table, Map);

    map ->
      io:format("~w: Map: ~w\n", [Name, Map]),
      router(Name, N, Hist, Intf, Table, Map);

    broadcast ->
      Message = {links, Name, N, intf:list(Intf)},
      intf:broadcast(Message, Intf),
      router(Name, N+1, Hist, Intf, Table, Map);

    stop ->
      ok
  end.
















%
