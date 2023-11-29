-module(day6).
-import(file, [read_file/1]).
-import(binary, [split/2, split/3]).
-import(lists, [seq/2, max/1]).
-import(ets, [new/2, insert/2, lookup/2]).
-import(util, [timed/1]).
-export([solve/0]).

process_input(Path) ->
    {ok, File} = file:read_file(Path),
    [[X || X <- binary:split(Line, <<" ">>, [global]), X =/= <<"turn">>, X =/= <<"through">>] || Line <- binary:split(File, <<"\n">>, [global])].

sum_values_rem_2({_Key, Value}, Sum) -> Sum + (Value rem 2).

sum_values({_Key, Value}, Sum) -> Sum + Value.

bin_to_coordinate(Binary) -> [binary_to_integer(X) || X <- binary:split(Binary, <<",">>)].

coordinates_to_keys(C1, C2, Size) -> 
    [X1, Y1] = bin_to_coordinate(C1),
    [X2, Y2] = bin_to_coordinate(C2),
    [Y*Size+X || X <- lists:seq(X1,X2), Y <- lists:seq(Y1,Y2)].

bump_toggle(_Table, [], _Incr) -> ok;
bump_toggle(Table, [Key|Keys], Incr) ->
    Result = ets:lookup(Table, Key),
    if
        Result == [] -> ets:insert(Table, {Key, lists:max([Incr, 0])});
        true ->
            [{_K, V}] = Result,
            ets:insert(Table, {Key, lists:max([V+Incr, 0])})
    end,
    bump_toggle(Table, Keys, Incr).

execute_action_1([Action, C1, C2], Lights) ->
    Keys = coordinates_to_keys(C1, C2, 1000),
    case Action of
        <<"toggle">> -> bump_toggle(Lights, Keys, 1);
        <<"on">> -> ets:insert(Lights, [{Key, 1} || Key <- Keys]);
        <<"off">> -> ets:insert(Lights, [{Key, 0} || Key <- Keys])
    end.

execute_action_2([Action, C1, C2], Lights) ->
    Keys = coordinates_to_keys(C1, C2, 1000),
    case Action of
        <<"toggle">> -> bump_toggle(Lights, Keys, 2);
        <<"on">> -> bump_toggle(Lights, Keys, 1);
        <<"off">> -> bump_toggle(Lights, Keys, -1)
    end.

execute_actions([], Lights, _ExecF, SumF) -> ets:foldl(SumF, 0, Lights);
execute_actions([H|T], Lights, ExecF, SumF) ->
    ExecF(H, Lights),
    execute_actions(T, Lights, ExecF, SumF).

get_final_state(Actions, ExecF, SumF) ->
    Lights = ets:new(lights, [set]),
    Lit = execute_actions(Actions, Lights, ExecF, SumF),
    ets:delete(Lights),
    Lit.

solve() -> 
    Input = process_input("./input/day6.txt"),
    util:timed(
        fun() -> 
            {
                {part1, get_final_state(Input, fun execute_action_1/2, fun sum_values_rem_2/2)},
                {part2, get_final_state(Input, fun execute_action_2/2, fun sum_values/2)}
            }
        end
    ).