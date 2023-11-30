-module(day7).
-import(file, [read_file/1]).
-import(binary, [split/3]).
-export([solve/0]).

process_input(Path) ->
    {ok, File} = read_file(Path),
    [split(Line, <<" ">>, [global]) || Line <- split(File, <<"\n">>, [global])].

get_value(Table, Key) ->
    [{Key, Value}] = ets:lookup(Table, Key),
    Value.

execute_instruction([Sig, <<"->">>, Var], Table) -> ets:insert(Table, {Var, binary_to_integer(Sig)});
execute_instruction([Ref1, <<"AND">>, Ref2, <<"->">>, Var], Table) -> ets:insert(Table, {Var, get_value(Table, Ref1) band get_value(Table, Ref2)});
execute_instruction([Ref1, <<"OR">>, Ref2, <<"->">>, Var], Table) -> ets:insert(Table, {Var, get_value(Table, Ref1) bor get_value(Table, Ref2)});
execute_instruction([Ref, <<"LSHIFT">>, By, <<"->">>, Var], Table) -> ets:insert(Table, {Var, get_value(Table, Ref) bsl binary_to_integer(By)});
execute_instruction([Ref, <<"RSHIFT">>, By, <<"->">>, Var], Table) -> ets:insert(Table, {Var, get_value(Table, Ref) bsr binary_to_integer(By)});
execute_instruction([<<"NOT">>, Ref, <<"->">>, Var], Table) -> ets:insert(Table, {Var, bnot get_value(Table, Ref)}).

execute_instructions([], Table) -> get_value(Table, <<"a">>);
execute_instructions([I1|Instructions], Table) ->
    execute_instruction(I1, Table),
    execute_instructions(Instructions, Table).

get_final_state(Instructions) ->
    Table = ets:new(table, [set]),
    Result = execute_instructions(Instructions, Table),
    ets:delete(Table),
    Result.

solve() -> 
    Input = process_input("./input/test.txt"),
    get_final_state(Input).