-module (controller_spec).

-export ([specs/0]).

-include ("../espec/include/espec.hrl").

specs() -> [

?describe("Controller", [

  ?before(each, fun() ->
    merl.controller:new(self(), nil)
  end),
  
  it("should work", fun(Controller) ->
    Expects = {doing, something_cool},
    Controller ! {do, something_cool},
    receive V -> Actual = V end,
    ?bdd(Actual, should, be, Expects)
  end)
])

].