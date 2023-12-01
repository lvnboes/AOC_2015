-module(day8).
-import(binary, [split/3]).
-import(lists, [flatten/1]).
-import(util, [timed/1]).
-export([solve/0]).

process_input(Path) ->
    {ok, File} = file:read_file(Path),
    flatten([binary_to_list(X) || X <- split(File, <<"\n">>, [global])]).

count_chars(Chars) -> count_chars(Chars, 0, 0, 0).
count_chars([], Acc1, Acc2, Acc3) -> {{part1, Acc1-Acc2}, {part2, Acc3-Acc1}};
count_chars([$\"|T], Acc1, Acc2, Acc3) -> count_chars(T, Acc1 + 1, Acc2, Acc3 + 3);
count_chars([$\\, B | T], Acc1, Acc2, Acc3) when (B == $\") or (B == $\\) -> count_chars(T, Acc1 + 2, Acc2 + 1, Acc3 + 4);
count_chars([$\\, $x, _C, _D | T], Acc1, Acc2, Acc3) -> count_chars(T, Acc1 + 4, Acc2 + 1, Acc3 + +5);
count_chars([_H|T], Acc1, Acc2, Acc3) -> count_chars(T, Acc1 + 1, Acc2 + 1, Acc3 + 1).

solve() ->
    Input = process_input("./input/day8.txt"),
    timed(fun() -> count_chars(Input) end).