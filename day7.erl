-module(day7).
-import(file, [read_file/1]).
-import(binary, [split/3]).
-import(lists, [flatten/1, seq/2, nth/2, sublist/2, sublist/3]).
-import(string, [pad/4]).
-export([solve/0, test_signal/1]).

process_input(Path) ->
    {ok, File} = read_file(Path),
    [split(Line, <<" ">>, [global]) || Line <- split(File, <<"\n">>, [global])].

test_signal(Sig) ->
    try binary_to_integer(Sig) of 
        _ -> true
    catch error:_Error -> false
    end.

swap_bits($1) -> $0;
swap_bits($0) -> $1.

my_bnot(BinStr) -> [swap_bits(Bit) || Bit <- BinStr].

bit_and(B1, B2) when (B1 == $1) and (B2 == $1) -> $1;
bit_and(_B1, _B2) -> $0.

my_band(BinStr1, BinStr2) -> [bit_and(nth(X, BinStr1), nth(X, BinStr2)) || X <- seq(1, 16)].

bit_or(B1, B2) when (B1 == $0) and (B2 == $0) -> $0;
bit_or(_B1, _B2) -> $1.

my_bor(BinStr1, BinStr2) -> [bit_or(nth(X, BinStr1), nth(X, BinStr2)) || X <- seq(1, 16)].

my_bsr(BinStr, By) -> flatten(pad(sublist(BinStr, 16 - By), 16, leading, $0)).

my_bsl(BinStr, By) -> flatten(pad(sublist(BinStr, By+1, 16-By), 16, trailing, $0)).

signal_to_bin_string(Sig) -> flatten(pad(integer_to_list(Sig, 2), 16, leading, $0)).

bin_string_to_signal(BinStr) -> list_to_integer(BinStr, 2).

get_value(Table, Key) ->
    [{Key, Value}] = ets:lookup(Table, Key),
    Value.

execute_instruction([Sig, <<"->">>, Var], Table) -> ets:insert(Table, {Var, signal_to_bin_string(binary_to_integer(Sig))});
execute_instruction([Ref1, <<"AND">>, Ref2, <<"->">>, Var], Table) -> ets:insert(Table, {Var, my_band(get_value(Table, Ref1), get_value(Table, Ref2))});
execute_instruction([Ref1, <<"OR">>, Ref2, <<"->">>, Var], Table) -> ets:insert(Table, {Var, my_bor(get_value(Table, Ref1), get_value(Table, Ref2))});
execute_instruction([Ref, <<"LSHIFT">>, By, <<"->">>, Var], Table) -> ets:insert(Table, {Var, my_bsl(get_value(Table, Ref), binary_to_integer(By))});
execute_instruction([Ref, <<"RSHIFT">>, By, <<"->">>, Var], Table) -> ets:insert(Table, {Var, my_bsr(get_value(Table, Ref), binary_to_integer(By))});
execute_instruction([<<"NOT">>, Ref, <<"->">>, Var], Table) -> ets:insert(Table, {Var, my_bnot(get_value(Table, Ref))}).

%execute_instructions([], Table) -> get_value(Table, <<"i">>);
execute_instructions([], Table) -> [{Key, bin_string_to_signal(Val)} || {Key, Val} <- ets:tab2list(Table)];
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
    % TODO: implement sorting / looping to process instructions in good order.
    get_final_state(Input).