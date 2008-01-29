-module (merl.router.namespace).
-author ("Ray Morgan").

-export ([namespace/4]).

namespace(Name, Route, [Namespace|Namespaced_path], Method) ->
  case Namespace =:= Name of
    true ->
      case merl.router:build(Namespaced_path, Method, [Route]) of
        no_match -> no_match;
        V -> merl.tuple_list:merge([{namespace, Namespace}], V)
      end;
    false -> no_match
  end.