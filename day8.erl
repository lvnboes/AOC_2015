-module(day8).
-import(binary, [split/3]).
-import(lists, [flatten/1]).
-import(util, [timed/1]).
-export([solve/0]).

process_input(Path) ->
    {ok, File} = file:read_file(Path),
    flatten([binary_to_list(X) || X <- split(File, <<"\n">>, [global])]).

is_hex_char(Char) -> is_hex_char(Char, "0123456789abcdef").
is_hex_char(_Char, []) -> false;
is_hex_char(Char, [H|_T]) when Char == H -> true;
is_hex_char(Char, [_H|T]) -> is_hex_char(Char, T).

count_chars(Chars) -> count_chars(Chars, 0, 0, 0).
count_chars([], Acc1, Acc2, Acc3) -> {{part1, Acc1-Acc2}, {part2, Acc3-Acc1}};
count_chars([$\"|T], Acc1, Acc2, Acc3) -> count_chars(T, Acc1 + 1, Acc2, Acc3 + 3);
count_chars([$\\, B | T], Acc1, Acc2, Acc3) when (B == $\") or (B == $\\) -> count_chars(T, Acc1 + 2, Acc2 + 1, Acc3 + 4);
count_chars([$\\, $x, C, D | T], Acc1, Acc2, Acc3) ->
    IsHex = is_hex_char(C) and is_hex_char(D),
    if
        IsHex -> count_chars(T, Acc1 + 4, Acc2 + 1, Acc3 + +5);
        true -> count_chars([$x|[C|[D|T]]], Acc1 + 1, Acc2 +1, Acc3 + 1)
    end;
count_chars([_H|T], Acc1, Acc2, Acc3) -> count_chars(T, Acc1 + 1, Acc2 + 1, Acc3 + 1).


solve() ->
    Input = process_input("./input/day8.txt"),
    timed(fun() -> count_chars(Input) end).