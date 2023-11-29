-module(day4).
-import(file, [read_file/1]).
-import(crypto, [hash/2]).
-import(binary, [encode_hex/1]).
-import(util, [timed/1]).
-export([solve/0]).

process_input() -> 
    {ok, File} = file:read_file("./input/day4.txt"),
    File.

get_hash(InStart, Int) ->
    IntBin = list_to_binary(integer_to_list(Int)),
    HashBase = <<InStart/binary, IntBin/binary>>,
    binary_to_list(binary:encode_hex(crypto:hash(md5, HashBase))).

test_hash_5([A,B,C,D,E|_T]) -> [A,B,C,D,E] == "00000".

test_hash_6([A,B,C,D,E,F|_T]) -> [A,B,C,D,E,F] == "000000".

test_hashes(InStart, Int, TestFunction) ->
    Hash = get_hash(InStart, Int),
    Valid = TestFunction(Hash),
    if
        Valid -> Int;
        true -> test_hashes(InStart, Int+1, TestFunction)
    end.

solve() ->
    Input = process_input(),
    util:timed(
        fun() ->
            {
                {part1, test_hashes(Input, 0, fun test_hash_5/1)},
                {part2, test_hashes(Input, 0, fun test_hash_6/1)}
            }
        end
    ).