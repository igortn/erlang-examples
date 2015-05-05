-module(allocator).

%% Resources allocator service.

%% API
-export([start/1, server/2, alloc/0, free/1]).

start(Resources) ->
  Pid = spawn(allocator, server, [Resources, []]),
  register(resource_alloc, Pid).

server(Free, Allocated) ->
  receive
    {From, alloc} ->
      do_alloc(Free, Allocated, From);
    {From, {free, R}} ->
      do_free(Free, Allocated, From, R)
  end.

do_alloc([], Allocated, From) ->
  From ! {resource_alloc, false},
  server([], Allocated);
do_alloc([R|Free], Allocated, From) ->
  From ! {resource_alloc, true},
  server(Free, [{R,From} | Allocated]).

do_free(Free, Allocated, From, R) ->
  case lists:member({From,R}, Allocated) of
    true ->
      From ! {resource_alloc, true},
      server([R|Free], lists:delete({From,R}, Allocated));
    false ->
      From ! {resource_alloc, false},
      server(Free, Allocated)
  end.

% interface
alloc() ->
  request(alloc).

free(Resource) ->
  request({free, Resource}).

request(Request) ->
  resource_alloc ! {self(), Request},
  receive
    {resource_alloc, Reply} ->
      Reply
  end.
