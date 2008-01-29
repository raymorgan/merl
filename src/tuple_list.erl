-module (merl.tuple_list).
-author ("Ray Morgan").

-export ([merge/2]).
-import (lists).

merge(List, []) -> List;
merge(List, [H|T]) ->
  {Key, _} = H,
  merge(lists:keystore(Key, 1, List, H), T).