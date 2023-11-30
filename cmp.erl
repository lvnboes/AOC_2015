-module(cmp).
-export([c/0, c/1]).

% A module to compile all days or one particular day

find_erl_files() ->
    {ok, Files} = file:list_dir("."),
    [File || File <- Files, check_erl(File)].

check_erl(File_name) -> lists:sublist(lists:reverse(File_name), 4) == "lre.".

compile_modules([]) -> ok;
compile_modules([FileName|FileNames]) -> 
    if
        FileName == "cmp.erl" -> ignore;
        true -> compile:file(FileName)
    end,
    compile_modules(FileNames).

c() -> 
    Files = find_erl_files(),
    compile_modules(Files).
c(Day) -> compile:file("day"++integer_to_list(Day)++".erl").