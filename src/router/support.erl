-module (merl.router.support).
-author ("Ray Morgan").

-export ([split_path/1, atomize/1, bindings_from/1, is_var/1]).

-import (string).
-import (lists).

split_path(Path) ->
  [P|_] = string:tokens(Path, [$?, $.]),
  atomize(string:tokens(P, [$/])).
  
atomize([]) -> [];
atomize([H|T]) ->
  [list_to_atom(H)|atomize(T)].

bindings_from(Path) ->
  P = split_path(Path),
  find_bindings(P, []).
find_bindings([], Bindings) -> Bindings;
find_bindings([H|T], Bindings) ->
  case atom_to_list(H) of
    [$:|V] -> find_bindings(T, lists:append(Bindings, [list_to_atom(V)]));
    _ -> find_bindings(T, Bindings)
  end.

is_var(Route) ->
  R = atom_to_list(Route),
  [H|_] = R,
  H =:= $:.