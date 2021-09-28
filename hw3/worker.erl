-module(worker).
-export ([start/5, stop/1, peers/2]).

start(Name, Logger, Seed, Sleep, Jitter) ->
  spawn_link(fun() -> init(Name, Logger, Seed, Sleep, Jitter) end).

stop(Worker) ->
  Worker ! stop.

init(Name, Log, Seed, Sleep, Jitter) ->
  rand:uniform(Seed), %diff
  MyTime = vect:zero(),  % change  here to revert vect
  receive
    {peers, Peers} ->
      loop(Name, Log, Peers, Sleep, Jitter, MyTime);

    stop ->
      ok
  end.

peers(Wrk, Peers) ->
  Wrk ! {peers, Peers}.

loop(Name, Log, Peers, Sleep, Jitter, MyTime) ->
  Wait = rand:uniform(Sleep),
  receive
    {msg, Time, Msg} ->

      MyTime1 = vect:merge(MyTime, Time),  % change here for revert vect
      MyTime2 = vect:inc(Name, MyTime1),   % change here for revert vect

      Log ! {log, Name, MyTime2, {received, Msg}},
      loop(Name, Log, Peers, Sleep, Jitter, MyTime2);

    stop ->
      ok;

    Error ->
      Log ! {log, Name, time, {error, Error}}

  after Wait ->
    Selected = select(Peers),
    Time = vect:inc(Name, MyTime),
    Message = {hello, rand:uniform(100)},
    Selected ! {msg, Time, Message},
    jitter(Jitter),
    Log ! {log, Name, Time, {sending, Message}},
    loop(Name, Log, Peers, Sleep, Jitter, Time)
  end.

select(Peers) ->
  lists:nth(rand:uniform(length(Peers)), Peers).

jitter(0) -> ok;
jitter(Jitter) ->
  timer:sleep(rand:uniform(Jitter)).
































%
