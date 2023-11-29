-module(day3).
-import(file, [read_file/1]).
-import(lists,[last/1]).
-import(sets, [from_list/1, to_list/1]).
-import(util, [timed/1]).
-export([solve/0]).

process_import(Path) ->
    {ok, File} = file:read_file(Path),
    binary_to_list(File).

move({X, Y}, $>) -> {X+1, Y};
move({X, Y}, $<) -> {X-1, Y};
move({X, Y}, $^) -> {X, Y+1};
move({X, Y}, $v) -> {X, Y-1}.

positions([], Positions) -> Positions;
positions([H|T], Positions) -> positions(T, Positions++[move(lists:last(Positions), H)]).

positions_2([], Santa, RoboSanta) -> Santa ++ RoboSanta;
positions_2([S,R|T], Santa, RoboSanta) -> positions_2(T, Santa++[move(lists:last(Santa), S)], RoboSanta++[move(lists:last(RoboSanta), R)]).

unique_positions(Positions) -> length(sets:to_list(sets:from_list(Positions))).

solve() -> 
    Input = process_import("./input/day3.txt"),
    util:timed(
        fun() ->
            {
                {part1, unique_positions(positions(Input, [{0,0}]))}, 
                {part2, unique_positions(positions_2(Input, [{0,0}], [{0,0}]))}
            }
        end
    ).