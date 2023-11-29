-module(util).
-import(lists, [append/1, sublist/2, sublist/3]).
-export([timed/1, count/2]).

timed(F) ->
    Start = erlang:system_time(microsecond),
    Result = F(),
    End = erlang:system_time(microsecond),
    {{result, Result}, {microsecs, End-Start}}.

count(_Item, []) -> 0;
count(Item, [H|T]) when Item == H -> 1 + count(Item, T);
count(Item, [_H|T]) -> count(Item, T).
