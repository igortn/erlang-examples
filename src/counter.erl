-module(counter).
-author("igortn").

%% API
-export([start/0, increment/1, value/1, stop/1, loop/1]).

start() ->
  spawn(counter, loop, [0]).

increment(Counter) ->
  Counter ! increment.

value(Counter) ->
  Counter ! {self(), value},
  receive
    {Counter, Value} -> Value
  after 2000 -> no_response
  end.

stop(Counter) ->
  Counter ! stop.

loop(Val) ->
  receive
    increment ->
      loop(Val + 1);
    {From, value} ->
      From ! {self(), Val},
      loop(Val);
    stop -> true;
    _ -> loop(Val)
  end.
