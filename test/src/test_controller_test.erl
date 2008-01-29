-module(test_controller_test).

-behaviour(erunit_test).
-export([tests/0, run/0]).

-import(erunit, [test/2, assertEquals/2, run/1]).

run() ->
  run(tests()).

tests() ->
    [
      test("do() should return doing", fun() ->
        assertEquals(merl.controller.test:do(), doing)
      end)
    ].