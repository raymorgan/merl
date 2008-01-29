-module (merl.router.add).
-author ("Ray Morgan").

-export ([add/3]).

-import (merl.router.support, [is_var/1]).
-import (lists).

add([], [], _, Match) -> Match;
add([], _, _, _) -> no_match;
add(_, [], _, _) -> no_match;
add([Path|PT], [Route|RT], Matcher, Match) ->
  case is_var(Route) of  %Route =:= ':var' of
    true ->
      [Cur|MT] = Matcher,
      add(PT, RT, MT, lists:keystore(Cur, 1, Match, {Cur, Path}));
    false ->
      case Path =:= Route of
        true -> add(PT, RT, Matcher, Match);
        false -> no_match
      end
  end.
add(Path, Route, Matcher) -> add(Path, Route, Matcher, []).