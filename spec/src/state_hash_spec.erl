-module (state_hash_spec).

-export ([specs/0]).

-include ("../espec/include/espec.hrl").

specs() -> [

?describe("A new merl.State_hash", [

  ?before(each, fun() ->
    merl.state_hash:new()
  end),
  
  it("should be able to merge data", fun(State_hash) ->
    Expects = [{this, that}, {those, these}],
    Actual = merl.state_hash:merge(State_hash, [{this, that}, {those, these}]),
    ?bdd(Actual, should, be, Expects)
  end),
  
  it("should be able to add a pair", fun(State_hash) ->
    Expects = [{this, that}],
    Actual = merl.state_hash:set(State_hash, this, that),
    ?bdd(Actual, should, be, Expects)
  end)
]),

?describe("An existing merl.State_hash", [

  ?before(each, fun() ->
    merl.state_hash:new([{this, that}, {those, these}])
  end),
  
  it("should be get an item by its key", fun(State_hash) ->
    Expects = these,
    Actual = merl.state_hash:get(State_hash, those),
    ?bdd(Actual, should, be, Expects)
  end),
  
  it("should be able to replace an existing item via set", fun(State_hash) ->
    Expects = [{this, that}, {those, these2}],
    Actual = merl.state_hash:set(State_hash, those, these2),
    ?bdd(Actual, should, be, Expects)
  end),
  
  it("should be able to merge correctly", fun(State_hash) ->
    Expects = [{this, that}, {those, these2}, {please, sir}],
    Actual = merl.state_hash:merge(State_hash, [{those, these2}, {please, sir}]),
    ?bdd(Actual, should, be, Expects)
  end)
]),

?describe("A nested merl.State_hash", [

  ?before(each, fun() -> begin
    State_hash = merl.state_hash:new(),
    merl.state_hash:merge(State_hash, [{person, merl.state_hash:new([{name, "Ray"}])}]),
    State_hash
  end end),
  
  it("should be able to access inner state hashes", fun(State_hash) ->
    Expects = "Ray",
    Actual = merl.state_hash:get(State_hash, [person, name]),
    ?bdd(Actual, should, be, Expects)
  end)
]),

?describe("A deep nested merl.State_hash", [

  ?before(each, fun() -> 
    merl.state_hash:new([
      {person, merl.state_hash:new([
        {height, "6.1'"},
        {name, merl.state_hash:new([
          {first, "Ray"},
          {last, "Morgan"}
        ])},
        {age, 20}
      ])}
    ])
  end),
  
  it("should be able to access inner state hashes", fun(State_hash) ->
    Expects = "Morgan",
    Actual = merl.state_hash:get(State_hash, [person, name, last]),
    ?bdd(Actual, should, be, Expects)
  end)
])

].