-module(worker).
-export ([start/5, stop/1, peers/2]).

start(Name, Logger, Seed, Sleep, Jitter) ->
  spawn_link(fun() -> init(Name, Logger, Seed, Sleep, Jitter) end).

stop(Worker) ->
  Worker ! stop.

init(Name, Log, Seed, Sleep, Jitter) ->
  random:seed(Seed, Seed, Seed),
  MyTime = time:zero(),
  receive
    {peers, Peers} ->
      loop(Name, Log, Peers, Sleep, Jitter, MyTime);

    stop ->
      ok
  end.

peers(Wrk, Peers) ->
  Wrk ! {peers, Peers}.

loop(Name, Log, Peers, Sleep, Jitter, MyTime) ->
  Wait = random:uniform(Sleep),
  receive
    {msg, Time, Msg} ->

      MyTime1 = time:merge(Time, MyTime),
      MyTime2 = time:inc(name, MyTime1),

      Log ! {log, Name, MyTime2, {received, Msg}},
      loop(Name, Log, Peers, Sleep, Jitter, MyTime2);

    stop ->
      ok;

    Error ->
      Log ! {log, Name, MyTime, {error, Error}} % small time here or typo in instructions?

  after Wait ->
    Selected = select(Peers),
    % Time = na,
    Message = {hello, random:uniform(100)},
    Selected ! {msg, MyTime, Message},
    jitter(Jitter),
    Log ! {log, Name, MyTime, {sending, Message}},
    loop(Name, Log, Peers, Sleep, Jitter, MyTime)
  end.

select(Peers) ->
  lists:nth(random:uniform(length(Peers)), Peers).

jitter(0) -> ok;
jitter(Jitter) ->
  timer:sleep(random:uniform(Jitter)).
































%
