%% @copyright 2016 Hinagiku Soranoba All Rights Reserved.
%%
%% This common test is checked that bbmustache meets the reference implementation.
%%
%% Reference implementation:
%%   https://github.com/mustache/spec
%%

-module(bbmustache_spec_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(SKIP_FILES, [
                     % e.g. "name.json"
                     "~lambdas.json",
                     "~inheritance.json"
                    ]).
-define(SKIP_CASES, [
                     % e.g. {"name.json", <<"Test Case">>}
                    ]).

%%----------------------------------------------------------------------------------------------------------------------
%% 'common_test' Callback API
%%----------------------------------------------------------------------------------------------------------------------

all() ->
    [specs].

specs(Config) ->
    Jsons = case filelib:wildcard("../../**/mustache_spec.app") of
                [AppPath | _] ->
                    filelib:wildcard(filename:join([filename:absname(filename:dirname(AppPath)), "..", "specs", "*.json"]));
                [] ->
                    ct:log("mustache_spec.app is not found. please check the deps."),
                    ?assert(spec_is_not_found)
            end,
    %% NOTE: priv_dir did write partial files.
    ok = file:set_cwd(?config(priv_dir, Config)),
    lists:foreach(fun ?MODULE:spec_tests/1, Jsons).

spec_tests(JsonPath) ->
    ct:log("---- ~s -----", [Basename = filename:basename(JsonPath)]),
    case lists:member(Basename, ?SKIP_FILES) of
        true ->
            ct:log("This test case did skip...");
        false ->
            {ok, JsonBin} = file:read_file(JsonPath),
            JsonDec   = jsone:decode(JsonBin, [{object_format, proplist}]),
            Tests0    = proplists:get_value(<<"tests">>, JsonDec),
            SkipTests = proplists:get_all_values(Basename, ?SKIP_CASES),
            Tests     = lists:foldl(
                          fun(T, Acc) ->
                                  case lists:member(proplists:get_value(<<"name">>, T), SkipTests) of
                                      true  -> Acc;
                                      false -> [T | Acc]
                                  end
                          end, [], lists:reverse(Tests0)),
            lists:foreach(fun ?MODULE:spec_test/1, Tests)
    end.

spec_test(Assoc) ->
    Name     = proplists:get_value(<<"name">>, Assoc),
    Data     = proplists:get_value(<<"data">>, Assoc),
    Expected = proplists:get_value(<<"expected">>, Assoc),
    Template = proplists:get_value(<<"template">>, Assoc),
    Partials = proplists:get_value(<<"partials">>, Assoc, []),

    ok = clean_dir("."),
    ok = lists:foreach(fun ?MODULE:write_file/1, Partials),

    ct:log("CASE: ~s", [Name]),
    ?assertEqual(Expected, bbmustache:render(Template, Data, [{key_type, binary}])).

clean_dir(Dir) ->
    lists:foreach(fun(F) -> file:delete(F) end,
                  filelib:wildcard(filename:join(Dir, "*.mustache"))).

write_file({PartialFilename, PartialData}) ->
    ok = file:write_file(<<PartialFilename/binary, ".mustache">>, PartialData);
write_file(_) ->
    ok.
