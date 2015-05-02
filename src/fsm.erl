%% Finite state machine where states are modelled as functions and a state transition is caused by receiving a message.

%% S1 -- a --> S2,  S1 -- b --> S3,
%% S2 -- c --> S3,  S2 -- d --> S4,
%% S3 -- e --> S1,  S3 -- f --> S2,
%% S4 -- g --> S3.

-module(fsm).

%% API
-export([start/0, stop/1,send/2, get_state/1, s1/0, s2/0, s3/0, s4/0]).

start() ->
  spawn(fsm, s1, []).

send(SM, Msg) ->
  SM ! {self(), Msg},
  receive
    {SM, Receipt} -> io:format("From ~p: ~p~n", [SM, Receipt])
  after 1000 -> no_response
  end.

stop(SM) ->
  SM ! {self(), stop},
  receive
    {SM, true} -> stopped
  after 1000 -> no_response
  end.

get_state(SM) ->
  SM ! {self(), state},
  receive
    {SM, State} -> State
  after 1000 -> no_response
  end.

s1() ->
  receive
    {From, a} ->
      From ! {self(), receipt(a, s2)},
      s2();
    {From, b} ->
      From ! {self(), receipt(b, s3)},
      s3();
    {From, state} ->
      From ! {self(), s1},
      s1();
    {From, stop} ->
      From ! {self(), true},
      true;
    {From, X} ->
      From ! {self(), unknown(X)},
      s1()
  end.

s2() ->
  receive
    {From, c} ->
      From ! {self(), receipt(c, s3)},
      s3();
    {From, d} ->
      From ! {self(), receipt(d, s4)},
      s4();
    {From, state} ->
      From ! {self(), s2},
      s2();
    {From, stop} ->
      From ! {self(), true},
      true;
    {From, X} ->
      From ! {self(), unknown(X)},
      s2()
  end.

s3() ->
  receive
    {From, e} ->
      From ! {self(), receipt(e, s1)},
      s1();
    {From, f} ->
      From ! {self(), receipt(f, s2)},
      s2();
    {From, state} ->
      From ! {self(), s3},
      s3();
    {From, stop} ->
      From ! {self(), true},
      true;
    {From, X} ->
      From ! {self(), unknown(X)},
      s3()
  end.

s4() ->
  receive
    {From, g} ->
      From ! {self(), receipt(g, s3)},
      s3();
    {From, state} ->
      From ! {self(), s4},
      s4();
    {From, stop} ->
      From ! {self(), true},
      true;
    {From, X} ->
      From ! {self(), unknown(X)},
      s4()
  end.

receipt(Msg, State) ->
  "received " ++ atom_to_list(Msg) ++ ", switched to state " ++ atom_to_list(State).

unknown(Msg) ->
  "unknown message " ++ atom_to_list(Msg).
