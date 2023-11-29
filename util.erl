-module(util).
-import(lists, [append/1, sublist/2, sublist/3]).
-export([timed/1, count_occurences/2]).

timed(F) ->
    Start = erlang:system_time(microsecond),
    Result = F(),
    End = erlang:system_time(microsecond),
    {{result, Result}, {microsecs, End-Start}}.

count_occurences(_Item, []) -> 0;
count_occurences(Item, [H|T]) when Item == H -> 1 + count_occurences(Item, T);
count_occurences(Item, [_H|T]) -> count_occurences(Item, T).
