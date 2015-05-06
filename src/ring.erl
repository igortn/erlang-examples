-module(ring).

%% Creates a ring of processes of a given size and sends a message around the ring N times.

%% API
-export([ring/3, ring_node/1]).

ring(Size, Msg, N) ->
  StartNode = setup(Size, self()),
  io:format("set up the ring~n"),
  master_send_receive(StartNode, Msg, N).

setup(1, Node) ->
  Node;
setup(N, Node) ->
  Next = spawn(ring, ring_node, [Node]),
  setup(N-1, Next).

master_send_receive(StartNode, _, 0) ->
  io:format("done, killing children~n"),
  StartNode ! quit;
master_send_receive(StartNode, Msg, N) ->
  io:format("~p laps to go, sending to ~p~n", [N, StartNode]),
  StartNode ! {pass, Msg},
  receive
    {pass, Msg} ->
      master_send_receive(StartNode, Msg, N-1)
  end.

ring_node(Next) ->
  receive
    {pass, Msg} ->
      io:format("~p received: ~s, passing to ~p~n", [self(), Msg, Next]),
      Next ! {pass, Msg},
      ring_node(Next);
    quit ->
      io:format("~p is quitting, sending quit to ~p~n", [self(), Next]),
      Next ! quit
  end.
