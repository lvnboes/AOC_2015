-module(day8).
-import(binary, [split/3]).
-import(lists, [flatten/1]).
-import(util, [timed/1]).
-export([solve/0]).

process_input(Path) ->
    {ok, File} = file:read_file(Path),
    flatten([binary_to_list(X) || X <- split(File, <<"\n">>, [global])]).

count(Chars) -> count(Chars, 0, 0, 0).
count([], Acc1, Acc2, Acc3) -> {{part1, Acc1-Acc2}, {part2, Acc3-Acc1}};
count([$\"|T], Acc1, Acc2, Acc3) -> count(T, Acc1 + 1, Acc2, Acc3 + 3);
count([$\\, B | T], Acc1, Acc2, Acc3) when (B == $\") or (B == $\\) -> count(T, Acc1 + 2, Acc2 + 1, Acc3 + 4);
count([$\\, $x, _C, _D | T], Acc1, Acc2, Acc3) -> count(T, Acc1 + 4, Acc2 + 1, Acc3 + +5);
count([_H|T], Acc1, Acc2, Acc3) -> count(T, Acc1 + 1, Acc2 + 1, Acc3 + 1).

solve() ->
    Input = process_input("./input/day8.txt"),
    timed(fun() -> count(Input) end).