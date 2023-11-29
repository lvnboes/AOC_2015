-module(day1).
-import(util, [timed/1]).
-export([solve/0]).

process_input(Uri) ->
    {ok, File} = file:read_file(Uri),
    binary_to_list(File).

count_brackets([], Open, Close, _Level, CellarEntry) -> {{part1, Open-Close}, {part2, CellarEntry}};
count_brackets([H|T], Open, Close, _Level, CellarEntry) when [H]=="(" -> count_brackets(T, Open+1, Close, Open+1-Close, CellarEntry);
count_brackets([H|T], Open, Close, 0, 0) when [H]==")" -> count_brackets(T, Open, Close+1, Open-Close-1, Open+Close+1);
count_brackets([H|T], Open, Close, _Level, CellarEntry) when [H]==")" -> count_brackets(T, Open, Close+1, Open-Close-1, CellarEntry).

solve() ->
    Input = process_input("./input/day1.txt"),
    util:timed(fun() -> count_brackets(Input, 0, 0, 0, 0) end).