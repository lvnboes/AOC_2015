-module(day5).
-import(file, [read_file/1]).
-import(binary, [split/2]).
-import(util, [timed/1]).
-export([solve/0]).

process_input(Path) ->
    {ok, File} = file:read_file(Path),
    [binary_to_list(Line) || Line <- binary:split(File, <<"\n">>, [global])].

vowels(String) -> length([X || X <- String, length(sets:to_list(sets:from_list([X|"aeiou"]))) == 5]) >= 3.

double_letter(String) when length(String) < 2 -> false;
double_letter([A,B|_T]) when A == B -> true;
double_letter([_H|T]) -> double_letter(T).

contains(String, _Forbidden) when length(String) < 2 -> false;
contains([A,B|_T], Forbidden) when [A,B] == Forbidden -> true;
contains([_H|T], Forbidden) -> contains(T, Forbidden).

not_forbidden(String) ->
    not contains(String, "ab") and not contains(String, "cd") and not contains(String, "pq") and not contains(String, "xy").

double_pair(String) when length(String) < 2 -> false;
double_pair([A,B|T]) ->
    ContainsPair = contains(T, [A,B]),
    if
        ContainsPair -> true;
        true -> double_pair([B]++T)
    end.

aba_pattern(String) when length(String) < 3 -> false;
aba_pattern([A,_B,C|_T]) when A == C -> true;
aba_pattern([_H|T]) -> aba_pattern(T).

check_polite(_String, []) -> true;
check_polite(String, [H|T]) -> 
    Impolite = not H(String),
    if
        Impolite -> false;
        true -> check_polite(String, T)
    end.

count_polite(List, PreConditions) -> count_polite(List, PreConditions, 0).
count_polite([], _PreConditions, Acc) -> Acc;
count_polite([H|T], PreConditions, Acc) ->
    Polite = check_polite(H, PreConditions),
    if
        Polite -> count_polite(T, PreConditions, Acc+1);
        true -> count_polite(T, PreConditions, Acc)
    end.
    
solve() -> 
    Input = process_input("./input/day5.txt"),
    util:timed(
        fun() ->
            {
                {part1, count_polite(Input, [fun vowels/1, fun double_letter/1, fun not_forbidden/1])},
                {part2, count_polite(Input, [fun double_pair/1, fun aba_pattern/1])}
            }
        end
    ).