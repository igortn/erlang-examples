-module(misc).

%% API
-export([sleep/1, flush_mailbox/0, priority_receive/0]).

% Suspend the current process for Time milliseconds.
sleep(Time) ->
  receive
  after Time ->
    true
  end.

% Empties the mailbox of the current process.
flush_mailbox() ->
  receive
    _ ->
      flush_mailbox()
  after 0 ->
    true
  end.

% Processes the priority message first and then anything else.
priority_receive() ->
  receive
    priority_msg ->
      priority_msg_processed
  after 0 ->
    receive
      Anything ->
        anything_processed
    end
  end.
