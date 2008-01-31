-module(eunit_suite).
-export([run/0, run/1, main/0, main/1]).
-compile(export_all).

main() -> io:format("-Running in current directory.~n"), main(".").
main(Dir) ->
	case filelib:is_dir(Dir) of
		false ->
			io:format("Not a directory: ~p~n", [Dir]),
			exit(not_a_directory);
		true ->
			add_to_path(Dir),
			Value = run(Dir),
			io:format("~p~n", [Value]),
			case Value of
				ok -> ok;
				_ -> exit(Value)
			end
	end.

add_to_path(Dir) ->
	{ok, Cwd} = file:get_cwd(),
	FullPath = filename:join(Cwd, Dir),
	case has_seperate_test_ebin_folders(FullPath) of
		false ->
			code:add_patha(FullPath);
		true ->
			code:add_patha(filename:join(FullPath, "ebin_test")),
			code:add_patha(filename:join(FullPath, "ebin"))
	end.

has_seperate_test_ebin_folders(Dir) ->
	(filelib:is_dir(filename:join(Dir, "ebin")))
	and
	(filelib:is_dir(filename:join(Dir, "ebin_test"))).

run() -> run(".").
run(Dir) ->
	TestModules = all_test_modules(Dir),
	case length(TestModules) > 0 of
		true  -> 
		  io:format("~n-----------[Running Tests]------------~n", []),
		  run_tests(TestModules), 
		  io:format("~n--------------------------------------~n~n", []),
		  ok;
		false ->
			io:format("-No tests found in ~p~n", [Dir]),
			ok
	end.

run_tests([]) -> [];
run_tests([Module|Rest]) ->
  io:format("~n~p: ", [Module]),
  Module:test(),
  run_tests(Rest).

tests_from([]) -> [];
tests_from([Module|Rest]) -> [{Module, Module:tests()} | tests_from(Rest)].

all_test_modules(Dir) ->
	all_test_modules(list_dir_recursive([Dir]), []).

list_dir_recursive([]) -> [];
list_dir_recursive([Dir|Rest]) ->
	CurrentDirs = filter_dirs(Dir),
	["."] ++ CurrentDirs ++ list_dir_recursive(CurrentDirs ++ Rest).

filter_dirs(Dir) ->
	{ok, Files} = file:list_dir(Dir),
	FilePaths = [filename:join(Dir, X) || X <- Files],
	[X || X <- FilePaths, filelib:is_dir(X)].

all_test_modules([], Modules) -> Modules;
all_test_modules([Dir|Rest], Modules) ->
	TestFiles = [filename:basename(X, ".beam") || X <- filelib:wildcard("*_test.beam", Dir)],
	case length(TestFiles) > 0 of
		true ->
			io:format("-Loading ~s:~n\t~p~n", [Dir, TestFiles]),
			code:add_patha(Dir),
			all_test_modules(Rest, (Modules ++ as_modules(TestFiles)));
		false -> all_test_modules(Rest, Modules)
	end.

as_modules(TestFiles) ->
	lists:map(fun(File) ->
		list_to_atom(File)
	end, TestFiles).

is_erunit_test(Module) ->
	case lists:keysearch(behaviour, 1, Module:module_info(attributes)) of
		{value, {behaviour, Behaviours}} -> lists:member(eunit_test, Behaviours);
		_ -> false
	end.