-module(test_router).
-author ("Ray Morgan").

-behaviour(erunit_test).
-export([tests/0, run/0]).

-import(erunit, [test/2, assertEquals/2, run/1]).

run() ->
  run(tests()).

tests() -> 
  resources_routes_should_match() ++
  bad_resources_routes_should_not_match() ++
  resource_routes_should_match() ++
  bad_resource_routes_should_not_match() ++
  namespaced_resources_should_match() ++
  default_route() ++
  custom_routes_should_match() ++
  bad_custom_routes_should_not_match().
  
resources_routes_should_match() ->
  [
    test("GET to /posts", fun() ->
      Expects = [{controller, posts}, {action, index}],
      Actual = merl.router:match(routes(), "/posts", get),
      assertEquals(Expects, Actual)
    end),
    
    test("GET to /posts/1", fun() ->
      Expects = [{controller, posts}, {action, show}, {id, '1'}],
      Actual = merl.router:match(routes(), "/posts/1", get),
      assertEquals(Expects, Actual)
    end),
    
    test("GET to /posts/new", fun() ->
      Expects = [{controller, posts}, {action, new}],
      Actual = merl.router:match(routes(), "/posts/new", get),
      assertEquals(Expects, Actual)
    end),
    
    test("GET to /posts/1/edit", fun() ->
      Expects = [{controller, posts}, {action, edit}, {id, '1'}],
      Actual = merl.router:match(routes(), "/posts/1/edit", get),
      assertEquals(Expects, Actual)
    end),
    
    test("POST to /posts", fun() ->
      Expects = [{controller, posts}, {action, create}],
      Actual = merl.router:match(routes(), "/posts", post),
      assertEquals(Expects, Actual)
    end),
    
    test("PUT to /posts/1", fun() ->
      Expects = [{controller, posts}, {action, update}, {id, '1'}],
      Actual = merl.router:match(routes(), "/posts/1", put),
      assertEquals(Expects, Actual)
    end),
    
    test("DELETE to /posts/1", fun() ->
      Expects = [{controller, posts}, {action, destroy}, {id, '1'}],
      Actual = merl.router:match(routes(), "/posts/1", delete),
      assertEquals(Expects, Actual)
    end)
  ].

bad_resources_routes_should_not_match() ->
  [
    test("GET to /posts/12/something", fun() ->
      Expects = no_match,
      Actual = merl.router:match(routes(), "/posts/12/something", get),
      assertEquals(Expects, Actual)
    end),
    
    test("GET to /postsbad", fun() ->
      Expects = no_match,
      Actual = merl.router:match(routes(), "/postsbad", get),
      assertEquals(Expects, Actual)
    end),
    
    test("POST to /posts/12", fun() ->
      Expects = no_match,
      Actual = merl.router:match(routes(), "/posts/12", post),
      assertEquals(Expects, Actual)
    end),
    
    test("PUT to /posts", fun() ->
      Expects = no_match,
      Actual = merl.router:match(routes(), "/posts", put),
      assertEquals(Expects, Actual)
    end),
    
    test("DELETE to /posts", fun() ->
      Expects = no_match,
      Actual = merl.router:match(routes(), "/posts", delete),
      assertEquals(Expects, Actual)
    end)
  ].
  
resource_routes_should_match() ->
  [
    test("GET to /theme", fun() ->
      Expects = [{controller, theme}, {action, index}],
      Actual = merl.router:match(routes(), "/theme", get),
      assertEquals(Expects, Actual)
    end),
    
    test("GET to /theme/new", fun() ->
      Expects = [{controller, theme}, {action, new}],
      Actual = merl.router:match(routes(), "/theme/new", get),
      assertEquals(Expects, Actual)
    end),
    
    test("GET to /theme/edit", fun() ->
      Expects = [{controller, theme}, {action, edit}],
      Actual = merl.router:match(routes(), "/theme/edit", get),
      assertEquals(Expects, Actual)
    end),
    
    test("POST to /theme", fun() ->
      Expects = [{controller, theme}, {action, create}],
      Actual = merl.router:match(routes(), "/theme", post),
      assertEquals(Expects, Actual)
    end),
    
    test("PUT to /theme", fun() ->
      Expects = [{controller, theme}, {action, update}],
      Actual = merl.router:match(routes(), "/theme", put),
      assertEquals(Expects, Actual)
    end),
    
    test("DELETE to /theme", fun() ->
      Expects = [{controller, theme}, {action, destroy}],
      Actual = merl.router:match(routes(), "/theme", delete),
      assertEquals(Expects, Actual)
    end)
  ].
  
bad_resource_routes_should_not_match() ->
  [
    test("GET to /theme/1", fun() ->
      Expects = no_match,
      Actual = merl.router:match(routes(), "/theme/1", get),
      assertEquals(Expects, Actual)
    end)
  ].
  
namespaced_resources_should_match() ->
  [
    test("GET to /admin/posts", fun() ->
      Expects = [{namespace, admin}, {controller, posts}, {action, index}],
      Actual = merl.router:match(routes(), "/admin/posts", get),
      assertEquals(Expects, Actual)
    end)
  ].
  
default_route() ->
  [
    test("GET to '/'", fun() ->
      Expects = [{controller, about}, {action, index}],
      Actual = merl.router:match(routes(), "/", get),
      assertEquals(Expects, Actual)
    end)
  ].
  
custom_routes_should_match() ->
  [
    test("GET to /report/by-date/2007", fun() ->
      Expects = [{controller, report}, {action, by_date}, {year, '2007'}],
      Actual = merl.router:match(routes(), "/report/by-date/2007", get),
      assertEquals(Expects, Actual)
    end),
    
    test("GET to /report/by-date/2007/11", fun() ->
      Expects = [{controller, report}, {action, by_date}, {year, '2007'}, {month, '11'}],
      Actual = merl.router:match(routes(), "/report/by-date/2007/11", get),
      assertEquals(Expects, Actual)
    end)
  ].

bad_custom_routes_should_not_match() ->
  [
    test("GET to /report/by-date/2007/11/23", fun() ->
      Expects = no_match,
      Actual = merl.router:match(routes(), "/report/by-date/2007/11/23", get),
      assertEquals(Expects, Actual)
    end),
    
    test("GET to /report/by-date", fun() ->
      Expects = no_match,
      Actual = merl.router:match(routes(), "/report/by-date", get),
      assertEquals(Expects, Actual)
    end)
  ].


%% Helper Methods %%

routes() ->
  [
    {resources, posts},
    {resource, theme},
    {namespace, admin, {resources, posts}},
    
    {add, {"/report/by-date/:year", [{controller, report}, {action, by_date}]}},
    {add, {"/report/by-date/:year/:month", [{controller, report}, {action, by_date}]}},
    {default, [{controller, about}, {action, index}]}
  ].