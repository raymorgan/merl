-module (router_test).

-include_lib ("eunit/include/eunit.hrl").
  
-define (it (Name, Expects, Actual),
    (fun({expects, E}, {actual, A}) ->
      {Name, ?_assertMatch(E, A)} end)(Expects, Actual)
  ).
  
my_test_() -> [
    ?it("should ensure 1 is 1",
      {expects, 1},
      {actual, 1})
  ].