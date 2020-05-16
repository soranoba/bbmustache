%% @copyright 2020 Hinagiku Soranoba All Rights Reserved.

-module(bbmustache_escript_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

%%----------------------------------------------------------------------------------------------------------------------
%% 'common_test' Callback API
%%----------------------------------------------------------------------------------------------------------------------

all() ->
    [
      {group, long},
      {group, short},
      {group, with_args},

      help, all_key_and_format_type, multiple_data_files, enoent
    ].

groups() ->
    [
      {long,      [version, help, key_type]},
      {short,     [version, help, key_type]},
      {with_args, [version, help]}
    ].

init_per_suite(Config) ->
    Escript = case os:getenv("CMD_TOOL") of
                  Path when is_list(Path) -> Path
              end,
    [{escript, Escript} | Config].

end_per_suite(_) ->
    ok.

init_per_testcase(TestCase, Config) ->
    Args = case {group_name(Config), TestCase} of
               {long, version}      -> ["--version"];
               {short, version}     -> ["-v"];
               {with_args, version} -> ["-v", "file"];

               {long, help}      -> ["--help"];
               {short, help}     -> ["-h"];
               {with_args, help} -> ["-h", "file"];
               {undefined, help} -> [];

               {long, key_type}    -> ["--key-type", "atom",
                                       "--data-file", data_file_name(atom, basic, Config),
                                       ?config(data_dir, Config) ++ "template.mustache"];
               {short, key_type}   -> ["-k", "atom",
                                       "-d", data_file_name(atom, basic, Config),
                                       ?config(data_dir, Config) ++ "template.mustache"];
               _ -> []
           end,
    [{args, Args} | Config].

end_per_testcase(_, _) ->
    ok.

%%----------------------------------------------------------------------------------------------------------------------
%% Common Test Functions
%%----------------------------------------------------------------------------------------------------------------------

version(Config) ->
    Got = run(Config),
    ?assertMatch({match, _}, re:run(Got, "bbmustache v[0-9]+\.[0-9]+\.[0-9]+.*")).

help(Config) ->
    Got = run(Config),
    ?assertMatch({match, _}, re:run(Got, "Usage: .*")).

key_type(Config) ->
    Got = run(Config),
    Expect = read_file("template.result", Config),
    ?assertEqual(Expect, Got).

all_key_and_format_type(Config) ->
    KeyTypes = [atom, string, binary, undefined],
    FormatTypes = [basic, assoc, maps],
    Expect = read_file("template.result", Config),
    TempalteFile = ?config(data_dir, Config) ++ "template.mustache",
    ok = lists:foreach(fun({KeyType, FormatType}) ->
        ct:log("KeyType = ~p, FormatType = ~p", [KeyType, FormatType]),
        Got = run(Config, options(KeyType, FormatType, Config) ++ [TempalteFile]),
        ?assertEqual(Expect, Got)
    end, [{K, F} || K <- KeyTypes, F <- FormatTypes]).

multiple_data_files(Config) ->
    KeyTypes = [atom, string, binary, undefined],
    FormatTypes = [basic, assoc, maps],
    Expect = read_file("template.overlay.result", Config),
    TempalteFile = ?config(data_dir, Config) ++ "template.mustache",
    ok = lists:foreach(fun({KeyType, FormatType}) ->
        ct:log("KeyType = ~p, FormatType = ~p", [KeyType, FormatType]),
        Got = run(Config, options(KeyType, FormatType, Config)
                          ++ ["-d", data_file_name(KeyType, overlays, Config)] ++ [TempalteFile]),
        ?assertEqual(Expect, Got)
    end, [{K, F} || K <- KeyTypes, F <- FormatTypes]).

enoent(Config) ->
    Got0 = run(Config, ["-d", "no_file", ?config(data_dir, Config) ++ "template.mustache"]),
    ?assertEqual(<<"ERROR: no_file is unable to read. (enoent)\n">>, Got0),

    Got1 = run(Config, ["no_file"]),
    ?assertEqual(<<"ERROR: no_file is unable to read.\n">>, Got1).

%%----------------------------------------------------------------------------------------------------------------------
%% Internal Functions
%%----------------------------------------------------------------------------------------------------------------------

-spec group_name([term()]) -> atom().
group_name(Config) ->
    proplists:get_value(name, ?config(tc_group_properties, Config)).

-spec run([term()]) -> binary().
run(Config) ->
    run(Config, []).

-spec run([term()], [string()]) -> binary().
run(Config, Args) ->
    Cmd = ?config(escript, Config) ++ " " ++ string:join(?config(args, Config) ++ Args, " "),
    ct:log("$ ~s", [Cmd]),
    Ret = os:cmd(Cmd),
    ct:log("~s", [Ret]),
    list_to_binary(Ret).

-spec read_file(file:filename_all(), [term()]) -> binary().
read_file(FileName, Config) ->
    {ok, File} = file:read_file(filename:join([?config(data_dir, Config), FileName])),
    File.

-spec data_file_name(atom(), atom(), [term()]) -> string().
data_file_name(undefined, FormatType, Config) ->
    data_file_name(string, FormatType, Config);
data_file_name(KeyType, FormatType, Config) ->
    ?config(data_dir, Config) ++ atom_to_list(KeyType) ++ "." ++ atom_to_list(FormatType).

-spec options(atom(), atom(), [term()]) -> [string()].
options(undefined, FormatType, Config) ->
    ["-d", data_file_name(string, FormatType, Config)];
options(KeyType, FormatType, Config) ->
    ["-k", atom_to_list(KeyType),
     "-d", data_file_name(KeyType, FormatType, Config)].

