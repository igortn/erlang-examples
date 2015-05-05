-module(number_registry).

%% Number registry server. lookup, insert and a number store are not implemented.

%% API
-export([start/0, server/1]).
-export([add_number/2, analyze/1]).

start() ->
  register(num_registry, spawn(number_registry, server, [nil])).

%% interface
add_number(Seq, Dest) ->
  request({add_number, Seq, Dest}).

analyze(Seq) ->
  request({analyze, Seq}).

request(Req) ->
  num_registry ! {self(), Req},
  receive
    {num_registry, Reply} ->
      Reply
  end.

%% server
server(NumTable) ->
  receive
    {From, {analyze, Seq}} ->
      Result = lookup(Seq, NumTable),
      From ! {num_registry, Result},
      server(NumTable);
    {From, {add_number, Seq, Dest}} ->
      From ! {num_registry, ack},
      server(insert(Seq, Dest, NumTable))
  end.

lookup(Seq, NumTable) ->
  nil.

insert(Seq, Dest, NumTable) ->
  nil.