-module(day7).
-import(file, [read_file/1]).
-export([solve/0]).

process_input(Path) ->
    {ok, File} = file:read_file(Path),
    File.

solve() -> 
    Input = process_input("./input/day7"),
    Input.