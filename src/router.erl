-module (merl.router).
-author ("Ray Morgan").

-export ([match/3, build/3]).
-import (lists).

-import (merl.router.support, [split_path/1, atomize/1, bindings_from/1, is_var/1]).

-import (merl.router.resources, [resources/3]).
-import (merl.router.resource, [resource/3]).
-import (merl.router.add, [add/3]).
-import (merl.router.namespace, [namespace/4]).

match(Routes, Path, Method) ->
  build(split_path(Path), Method, Routes).
    
build(_Path, _Method, []) -> no_match;

build(Path, Method, [{default, R}|T]) ->
  case Path of
    [] -> R;
    _ -> build(Path, Method, T)
  end;
  
build(Path, Method, [{namespace, Name, Route}|T]) ->
  case lists:flat_length(Path) > 1 of
    true -> 
      case namespace(Name, Route, Path, Method) of
        no_match -> build(Path, Method, T);
        V -> V
      end;
    false -> build(Path, Method, T)
  end;
  
build(Path, Method, [{resources, Name}|T]) ->
  case resources(Name, Path, Method) of
    no_match -> build(Path, Method, T);
    V -> V
  end;

build(Path, Method, [{resource, Name}|T]) ->
  case resource(Name, Path, Method) of
    no_match -> build(Path, Method, T);
    V -> V
  end;
  
build(Path, Method, [{add, Route}|T]) ->
  {R, Defaults} = Route,
  Bindings = bindings_from(R),
  case add(Path, split_path(R), Bindings) of
    no_match -> build(Path, Method, T);
    V -> merl.tuple_list:merge(Defaults, V)
  end;
  
build(Path, Method, [_|T]) -> build(Path, Method, T).