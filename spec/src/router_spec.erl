-module (router_spec).

-export ([specs/0]).

-include ("../espec/include/espec.hrl").


specs() -> [

?describe("Resources routes should match", [
  
  it("GET to /posts", fun() ->
    Expects = {match, [{controller, posts}, {action, index}]},
    Actual = merl.router:match(routes(), "/posts", get),
    ?bdd(Actual, should, be, Expects)
  end),
  
  it("GET to /moderator/report/by-date/2007", fun() ->
    Expects = {match, [{namespace, moderator}, {controller, report}, {action, by_date}, {year, '2007'}]},
    Actual = merl.router:match(routes(), "/moderator/report/by-date/2007", get),
    ?bdd(Actual, should, be, Expects)
  end),
  
  it("GET to /posts/new", fun() ->
    Expects = {match, [{controller, posts}, {action, new}]},
    Actual = merl.router:match(routes(), "/posts/new", get),
    ?bdd(Actual, should, be, Expects)
  end),
  
  it("GET to /posts/1/edit", fun() ->
    Expects = {match, [{controller, posts}, {action, edit}, {id, '1'}]},
    Actual = merl.router:match(routes(), "/posts/1/edit", get),
    ?bdd(Actual, should, be, Expects)
  end),
  
  it("POST to /posts", fun() ->
    Expects = {match, [{controller, posts}, {action, create}]},
    Actual = merl.router:match(routes(), "/posts", post),
    ?bdd(Actual, should, be, Expects)
  end),
  
  it("PUT to /posts/1", fun() ->
    Expects = {match, [{controller, posts}, {action, update}, {id, '1'}]},
    Actual = merl.router:match(routes(), "/posts/1", put),
    ?bdd(Actual, should, be, Expects)
  end),
  
  it("DELETE to /posts/1", fun() ->
    Expects = {match, [{controller, posts}, {action, destroy}, {id, '1'}]},
    Actual = merl.router:match(routes(), "/posts/1", delete),
    ?bdd(Actual, should, be, Expects)
  end)
  
]),

?describe("Bad Resources Routes Should Not Match", [
  it("GET to /posts/12/something", fun() ->
    Expects = no_match,
    Actual = merl.router:match(routes(), "/posts/12/something", get),
    ?bdd(Actual, should, be, Expects)
  end),
  
  it("GET to /postsbad", fun() ->
    Expects = no_match,
    Actual = merl.router:match(routes(), "/postsbad", get),
    ?bdd(Actual, should, be, Expects)
  end),
  
  it("POST to /posts/12", fun() ->
    Expects = no_match,
    Actual = merl.router:match(routes(), "/posts/12", post),
    ?bdd(Actual, should, be, Expects)
  end),
  
  it("PUT to /posts", fun() ->
    Expects = no_match,
    Actual = merl.router:match(routes(), "/posts", put),
    ?bdd(Actual, should, be, Expects)
  end),
  
  it("DELETE to /posts", fun() ->
    Expects = no_match,
    Actual = merl.router:match(routes(), "/posts", delete),
    ?bdd(Actual, should, be, Expects)
  end)
]),

?describe("Resource Routes Should Match", [
  it("GET to /theme", fun() ->
    Expects = {match, [{controller, theme}, {action, index}]},
    Actual = merl.router:match(routes(), "/theme", get),
    ?bdd(Actual, should, be, Expects)
  end),
  
  it("GET to /theme/new", fun() ->
    Expects = {match, [{controller, theme}, {action, new}]},
    Actual = merl.router:match(routes(), "/theme/new", get),
    ?bdd(Actual, should, be, Expects)
  end),
  
  it("GET to /theme/edit", fun() ->
    Expects = {match, [{controller, theme}, {action, edit}]},
    Actual = merl.router:match(routes(), "/theme/edit", get),
    ?bdd(Actual, should, be, Expects)
  end),
  
  it("POST to /theme", fun() ->
    Expects = {match, [{controller, theme}, {action, create}]},
    Actual = merl.router:match(routes(), "/theme", post),
    ?bdd(Actual, should, be, Expects)
  end),
  
  it("PUT to /theme", fun() ->
    Expects = {match, [{controller, theme}, {action, update}]},
    Actual = merl.router:match(routes(), "/theme", put),
    ?bdd(Actual, should, be, Expects)
  end),
  
  it("DELETE to /theme", fun() ->
    Expects = {match, [{controller, theme}, {action, destroy}]},
    Actual = merl.router:match(routes(), "/theme", delete),
    ?bdd(Actual, should, be, Expects)
  end)
]),
  
?describe("Bad Resource Routes Should Not Match", [
  it("GET to /theme/1", fun() ->
    Expects = no_match,
    Actual = merl.router:match(routes(), "/theme/1", get),
    ?bdd(Actual, should, be, Expects)
  end)
]),
  
?describe("Namespaced Resources Should Match", [
  it("GET to /admin/posts", fun() ->
    Expects = {match, [{namespace, admin}, {controller, posts}, {action, index}]},
    Actual = merl.router:match(routes(), "/admin/posts", get),
    ?bdd(Actual, should, be, Expects)
  end),
  
  it("GET to /moderator/posts", fun() ->
    Expects = {match, [{namespace, moderator}, {controller, posts}, {action, index}]},
    Actual = merl.router:match(routes(), "/moderator/posts", get),
    ?bdd(Actual, should, be, Expects)
  end),
  
  it("GET to /moderator/comments", fun() ->
    Expects = {match, [{namespace, moderator}, {controller, comments}, {action, index}]},
    Actual = merl.router:match(routes(), "/moderator/comments", get),
    ?bdd(Actual, should, be, Expects)
  end),
  
  it("GET to /moderator/report/by-date/2007", fun() ->
    Expects = {match, [{namespace, moderator}, {controller, report}, {action, by_date}, {year, '2007'}]},
    Actual = merl.router:match(routes(), "/moderator/report/by-date/2007", get),
    ?bdd(Actual, should, be, Expects)
  end)
]),
  
?describe("Default Route", [
  it("GET to '/'", fun() ->
    Expects = {match, [{controller, about}, {action, index}]},
    Actual = merl.router:match(routes(), "/", get),
    ?bdd(Actual, should, be, Expects)
  end)
]),
  
?describe("Custom Routes Should Match", [
  it("GET to /report/by-date/2007", fun() ->
    Expects = {match, [{controller, report}, {action, by_date}, {year, '2007'}]},
    Actual = merl.router:match(routes(), "/report/by-date/2007", get),
    ?bdd(Actual, should, be, Expects)
  end),
  
  it("GET to /report/by-date/2007/11", fun() ->
    Expects = {match, [{controller, report}, {action, by_date}, {year, '2007'}, {month, '11'}]},
    Actual = merl.router:match(routes(), "/report/by-date/2007/11", get),
    ?bdd(Actual, should, be, Expects)
  end)
]),

?describe("Bad Custom Routes Should Not Match", [
  it("GET to /report/by-date/2007/11/23", fun() ->
    Expects = no_match,
    Actual = merl.router:match(routes(), "/report/by-date/2007/11/23", get),
    ?bdd(Actual, should, be, Expects)
  end),
  
  it("GET to /report/by-date", fun() ->
    Expects = no_match,
    Actual = merl.router:match(routes(), "/report/by-date", get),
    ?bdd(Actual, should, be, Expects)
  end)
])

]. %% end specs


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