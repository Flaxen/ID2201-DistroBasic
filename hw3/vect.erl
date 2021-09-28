-module(vect).
-export([zero/0, inc/2, merge/2, leq/2, clock/1, update/3, safe/2]).

zero() ->
  [].

inc(Name, Time) ->
  case lists:keyfind(Name, 1, Time) of
     {Name, N} ->
      lists:keyreplace(Name, 1, Time, {Name, N+1});
    false ->
      [{Name, 1}|Time] % kanske 0?
  end.

merge([], Time) ->
  Time; % kanske tom/tomma listan?
merge([{Name, Ti}|Rest], Time) ->
  case lists:keyfind(Name, 1, Time) of
    {Name, Tj} ->
      [{Name, max(Ti,Tj)}| merge(Rest, lists:keydelete(Name, 1, Time))];
    false ->
      [{Name, Ti}| merge(Rest, Time)] % också kanske oklart? först tuple. ganska klart Ti
  end.

leq([], _) ->
  true;
leq([{Name, Ti}|Rest], Time) ->
  case lists:keyfind(Name, 1, Time) of
    {Name, Tj} ->
      if
        Ti =< Tj ->
          leq(Rest, Time);
        true ->
          false
      end;
    false ->
      false % no check for 0 since we would not have 0 in list.
  end.

clock(_) ->
  [].

update(From, Time, Clock) ->
  {From, N} = lists:keyfind(From, 1, Time),
  case lists:keyfind(From, 1, Clock) of
    {From, _} ->
      lists:keyreplace(From, 1, Clock, {From, N});
    false ->
      [{From, N}| Clock]
  end.


safe(Time, Clock) ->
  leq(Time, Clock).





















%
