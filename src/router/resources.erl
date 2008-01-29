-module (merl.router.resources).
-author ("Ray Morgan").

-export ([resources/3]).

resources(Name, Path, get) ->
  case Path of
    [Name] -> [{controller, Name}, {action, index}];
    [Name, new] -> [{controller, Name}, {action, new}];
    [Name, Id] -> [{controller, Name}, {action, show}, {id, Id}];
    [Name, Id, edit] -> [{controller, Name}, {action, edit}, {id, Id}];
    _ -> no_match
  end;
resources(Name, Path, post) ->
  case Path of
    [Name] -> [{controller, Name}, {action, create}];
    _ -> no_match
  end;
resources(Name, Path, put) ->
  case Path of
    [Name, Id] -> [{controller, Name}, {action, update}, {id, Id}];
    _ -> no_match
  end;
resources(Name, Path, delete) ->
  case Path of
    [Name, Id] -> [{controller, Name}, {action, destroy}, {id, Id}];
    _ -> no_match
  end.