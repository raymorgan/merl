-module (merl.router.resource).
-author ("Ray Morgan").

-export ([resource/3]).

resource(Name, Path, get) ->
  case Path of
    [Name] -> [{controller, Name}, {action, index}];
    [Name, new] -> [{controller, Name}, {action, new}];
    [Name, edit] -> [{controller, Name}, {action, edit}];
    _ -> no_match
  end;
  
resource(Name, Path, post) ->
  case Path of
    [Name] -> [{controller, Name}, {action, create}];
    _ -> no_match
  end;
  
resource(Name, Path, put) ->
  case Path of
    [Name] -> [{controller, Name}, {action, update}];
    _ -> no_match
  end;
  
resource(Name, Path, delete) ->
  case Path of
    [Name] -> [{controller, Name}, {action, destroy}];
    _ -> no_match
  end.