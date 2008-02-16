-module (merl.controller).
-author ("Ray Morgan").

-export ([new/2]).
% -import (lists).


new(Requester, Request) ->
  spawn(fun() -> loop(Requester, Request) end).


loop(Requester, Request) ->
  receive
    {do, V} -> Requester ! {doing, V}, loop(Requester, Request)
  end.