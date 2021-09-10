-module(rudy).
-export([start/1, stop/0]).


start(Port) ->
  register(rudy, spawn(fun() -> init(Port) end)).

stop() ->
  exit(whereis(rudy), "time to die").

init(Port) ->
  Opt = [list, {active, false}, {reuseaddr, true}],
  case gen_tcp:listen(Port, Opt) of
    {ok, Listen} ->
      % ..
      handler(Listen),
      % ..
      gen_tcp:close(Listen);
      % ok;
    {error, Error} ->
      io:format("rudy: error1: ~w~n", [Error])
      % Error
  end.

handler(Listen) ->
  case gen_tcp:accept(Listen) of
    {ok, Client} ->
      % ..
      request(Client);
      % ..
    {error, Error} ->
      io:format("rudy: error2: ~w~n", [Error])
      % Error
  end,
  handler(Listen).

request(Client) ->
  Recv = gen_tcp:recv(Client, 0),
  case Recv of
    {ok, Str} ->
      % ..
      Request = http:parse_request(Str),
      % ..
      Response = reply(Request),

      io:format("rudy:: ~w~n", ["before"]),
      gen_tcp:send(Client, Response),
      io:format("rudy:: ~w~n", ["after"]);

    {error, Error} ->
      io:format("rudy: error3: ~w~n", [Error])
  end,
  gen_tcp:close(Client).


reply({{get, [$/|URI], _}, _, _}) ->
  timer:sleep(40),
  io:format("Filename: ~s~n", [URI]),

  {ok, Size} = http:file_size(URI),
  {ok, Type} = http:get_type(URI),
  {ok, Binary} = file:read_file(URI),

  http:ok2(Binary, Size, Type).
  % http:ok2(file:read_file(""), Size, Type).





















%
