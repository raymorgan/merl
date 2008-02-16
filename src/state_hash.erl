-module (merl.state_hash).
-author ("Ray Morgan").

-export ([new/0, new/1, merge/2, get/2, get_raw/1, set/3]).

-import (lists).

new() ->
  spawn(fun() -> loop([]) end).
new(Init) ->
  spawn(fun() -> loop(Init) end).
  
loop(State) ->
  receive
    {From, merge, TL} -> 
      State1 = merl.tuple_list:merge(State, TL),
      From ! {self(), merged, State1},
      loop(State1);
    {From, get, Key} -> 
      From ! {self(), lists:keysearch(Key, 1, State)},
      loop(State);
    {From, get_raw} ->
      From ! {self(), State},
      loop(State)
  end.
  
  
merge(SHash, Data) ->
  SHash ! {self(), merge, Data},
  receive
    {SHash, merged, Merged_data} -> Merged_data
  end.
  
get(SHash, Key) when is_atom(Key) ->
  SHash ! {self(), get, Key},
  receive
    {SHash, {value, {Key, V}}} -> V;
    {SHash, false} -> nil
  end;
get(Value, []) -> Value;
get(SHash, [Key|Next]) -> get(get(SHash, Key), Next).

get_raw(SHash) ->
  SHash ! {self(), get_raw},
  receive
    {SHash, V} -> V
  end.
    
set(SHash, Key, Value) -> merge(SHash, [{Key, Value}]).
