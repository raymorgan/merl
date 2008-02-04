-module (router_spec).

-include ("../espec/include/espec.hrl").

-export ([specs/0]).

specs() -> [

?describe("Resources routes should match", [
  
  ?it("GET to /posts", fun() -> begin
    Expects = {match, [{controller, posts}, {action, index}]},
    Actual = merl.router:match(routes(), "/posts", get),
    ?bdd(Actual, should, be, Expects)
  end end),
  
  ?it("GET to /moderator/report/by-date/2007", fun() -> begin
    Expects = {match, [{namespace, moderator}, {controller, report}, {action, by_date}, {year, '2007'}]},
    Actual = merl.router:match(routes(), "/moderator/report/by-date/2007", get),
    ?bdd(Actual, should, be, Expects)
  end end)
  
])

].


%resources_routes_should_match() ->
%  [
%    test("GET to /posts", fun() ->
%      Expects = {match, [{controller, posts}, {action, index}]},
%      Actual = merl.router:match(routes(), "/posts", get),
%      assertEquals(Expects, Actual)
%    end),
%    
%    test("GET to /posts/1", fun() ->
%      Expects = {match, [{controller, posts}, {action, show}, {id, '1'}]},
%      Actual = merl.router:match(routes(), "/posts/1", get),
%      assertEquals(Expects, Actual)
%    end),
%    
%    test("GET to /posts/new", fun() ->
%      Expects = {match, [{controller, posts}, {action, new}]},
%      Actual = merl.router:match(routes(), "/posts/new", get),
%      assertEquals(Expects, Actual)
%    end),
%    
%    test("GET to /posts/1/edit", fun() ->
%      Expects = {match, [{controller, posts}, {action, edit}, {id, '1'}]},
%      Actual = merl.router:match(routes(), "/posts/1/edit", get),
%      assertEquals(Expects, Actual)
%    end),
%    
%    test("POST to /posts", fun() ->
%      Expects = {match, [{controller, posts}, {action, create}]},
%      Actual = merl.router:match(routes(), "/posts", post),
%      assertEquals(Expects, Actual)
%    end),
%    
%    test("PUT to /posts/1", fun() ->
%      Expects = {match, [{controller, posts}, {action, update}, {id, '1'}]},
%      Actual = merl.router:match(routes(), "/posts/1", put),
%      assertEquals(Expects, Actual)
%    end),
%    
%    test("DELETE to /posts/1", fun() ->
%      Expects = {match, [{controller, posts}, {action, destroy}, {id, '1'}]},
%      Actual = merl.router:match(routes(), "/posts/1", delete),
%      assertEquals(Expects, Actual)
%    end)
%  ].
%






routes() -> [
  {resources, posts},
  {resource, theme},
  {namespace, admin, {resources, posts}},
  {namespace, moderator, [
    {resources, posts},
    {resources, comments},
    {add, {"/report/by-date/:year", [{controller, report}, {action, by_date}]}}
  ]},
  
  {add, {"/report/by-date/:year", [{controller, report}, {action, by_date}]}},
  {add, {"/report/by-date/:year/:month", [{controller, report}, {action, by_date}]}},
  {default, [{controller, about}, {action, index}]}
].