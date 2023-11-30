-module(day7).
-import(file, [read_file/1]).
-import(binary, [split/3]).
-import(lists, [flatten/1, seq/2, nth/2, sublist/2, sublist/3]).
-import(string, [pad/4]).
-import(maps, [get/2, is_key/2]).
-import(util, [timed/1]).
-export([solve/0]).

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

bit_and(B1, B2) when (B1 == $1) and (B2 == $1) -> $1;
bit_and(_B1, _B2) -> $0.

bit_or(B1, B2) when (B1 == $0) and (B2 == $0) -> $0;
bit_or(_B1, _B2) -> $1.

my_bnot(BinStr) -> [swap_bits(Bit) || Bit <- BinStr].

my_band(BinStr1, BinStr2) -> [bit_and(nth(X, BinStr1), nth(X, BinStr2)) || X <- seq(1, 16)].

my_bor(BinStr1, BinStr2) -> [bit_or(nth(X, BinStr1), nth(X, BinStr2)) || X <- seq(1, 16)].

my_bsr(BinStr, By) -> flatten(pad(sublist(BinStr, 16 - By), 16, leading, $0)).

my_bsl(BinStr, By) -> flatten(pad(sublist(BinStr, By+1, 16-By), 16, trailing, $0)).

signal_to_bin_string(Sig) -> flatten(pad(integer_to_list(Sig, 2), 16, leading, $0)).

bin_string_to_signal(BinStr) -> list_to_integer(BinStr, 2).

get_val(X, Map) ->
    IsRef = is_key(X, Map),
    if
        IsRef -> get(X, Map);
        true -> signal_to_bin_string(binary_to_integer(X))
    end.

execute_instruction([Sig, <<"->">>, Var], Map) -> Map#{Var => get_val(Sig, Map)};
execute_instruction([Ref1, <<"AND">>, Ref2, <<"->">>, Var], Map) -> Map#{Var => my_band(get_val(Ref1, Map), get_val(Ref2, Map))};
execute_instruction([Ref1, <<"OR">>, Ref2, <<"->">>, Var], Map) -> Map#{Var => my_bor(get_val(Ref1, Map), get_val(Ref2, Map))};
execute_instruction([Ref, <<"LSHIFT">>, By, <<"->">>, Var], Map) -> Map#{Var => my_bsl(get_val(Ref, Map), binary_to_integer(By))};
execute_instruction([Ref, <<"RSHIFT">>, By, <<"->">>, Var], Map) -> Map#{Var => my_bsr(get_val(Ref, Map), binary_to_integer(By))};
execute_instruction([<<"NOT">>, Ref, <<"->">>, Var], Map) -> Map#{Var => my_bnot(get_val(Ref, Map))}.


pass(I1, Instructions, Map, Ignore) ->
    UpdatedMap = execute_instruction(I1, Map),
    execute_instructions(Instructions, UpdatedMap, Ignore).

fail(I1, Instructions, Map, Ignore) ->
    UpdatedInstructions = Instructions ++ [I1],
    execute_instructions(UpdatedInstructions, Map, Ignore).

execute_instructions([], Map, _Ignore) -> get(<<"a">>, Map);
execute_instructions([I1|Instructions], Map, Ignore) ->
    case I1 of
        [Sig, <<"->">>, Var] -> 
            TestSig = (test_signal(Sig) or is_key(Sig, Map)),
            if 
                Var == Ignore -> execute_instructions(Instructions, Map, Ignore);
                TestSig -> pass(I1, Instructions, Map, Ignore);
                true -> fail(I1, Instructions, Map, Ignore)
            end;
        [Ref1, Op, Ref2, <<"->">>, Var] when ((Op == <<"AND">>) or (Op == <<"OR">>)) -> 
            TestRefs = (is_key(Ref1, Map) or test_signal(Ref1)) and (is_key(Ref2, Map) or test_signal(Ref2)),
            if
                Var == Ignore -> execute_instructions(Instructions, Map, Ignore);
                TestRefs -> pass(I1, Instructions, Map, Ignore);
                true -> fail(I1, Instructions, Map, Ignore)
            end;
        [Ref, _Shift, _By, <<"->">>, Var] -> 
            TestRef = (is_key(Ref, Map) or test_signal(Ref)),
            if
                Var == Ignore -> execute_instructions(Instructions, Map, Ignore);
                TestRef -> pass(I1, Instructions, Map, Ignore);
                true -> fail(I1, Instructions, Map, Ignore)
            end;
        [<<"NOT">>, Ref, <<"->">>, Var] ->
            TestRef = (is_key(Ref, Map) or test_signal(Ref)),
            if
                Var == Ignore -> execute_instructions(Instructions, Map, Ignore);
                TestRef -> pass(I1, Instructions, Map, Ignore);
                true -> fail(I1, Instructions, Map, Ignore)
            end
    end.

get_a(Instructions) ->
    Map1 = maps:new(),
    Result1 = execute_instructions(Instructions, Map1, <<"ignore">>),
    Map2 = #{<<"b">> => signal_to_bin_string(956)},
    Result2 = execute_instructions(Instructions, Map2, <<"b">>),
    {{part1,bin_string_to_signal(Result1)},{part2,bin_string_to_signal(Result2)}}.

solve() -> 
    Input = process_input("./input/day7.txt"),
    timed(
        fun() -> get_a(Input) end
    ).