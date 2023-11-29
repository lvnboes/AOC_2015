-module(day2).
-import(file, [read_file/1]).
-import(lists, [min/1, sort/1]).
-import(util, [timed/1]).
-export([solve/0]).

process_import(Path) -> 
    {ok, File} = file:read_file(Path),
    Lines = binary:split(File, <<"\n">>, [global]),
    [[ binary_to_integer(Y) || Y <- binary:split(X, <<"x">>, [global])] || X <- Lines].

required_paper([L, W, H]) -> 
    Surface1 = L*W, 
    Surface2 = L*H, 
    Surface3 = W*H,
    2*Surface1 + 2*Surface2 + 2*Surface3 + lists:min([Surface1, Surface2, Surface3]).

required_ribbon([L, W, H]) ->
    [A, B, _C] = lists:sort([L, W, H]),
    2*A + 2*B + L*W*H.

required_materials([], Paper, Ribbon) -> {{part1, Paper}, {part2, Ribbon}};
required_materials([H|T], Paper, Ribbon) -> required_materials(T, Paper+required_paper(H), Ribbon+required_ribbon(H)).

solve() ->
    Input = process_import("./input/day2.txt"),
    util:timed(fun() -> required_materials(Input, 0, 0) end).