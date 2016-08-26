#!/usr/bin/env escript
%% -*- erlang -*-

-define(TEST_LIBRARIES,
        [
         {"bbmustache",
          fun (Template, Data) ->
                  bbmustache:render(Template, Data, [{key_type, binary}])
          end},
         {"mustache.erl",
          fun (Template, Data) ->
                  list_to_binary(mustache:render(binary_to_list(Template),
                                                 dict:from_list(key_conv_recursive(Data, fun(B) -> binary_to_atom(B, latin1) end))))
          end}
        ]).

main(_) ->
    ok = code:add_pathsz(filelib:wildcard(filename:absname("_build/**/ebin"))),
    ok = file:set_cwd("benchmarks/.tmp"),

    Jsons = filelib:wildcard("../../_build/test/lib/mustache_spec/specs/*.json"),
    io:format("test files:~n"),
    [io:format("    ~s~n", [Json]) || Json <- Jsons],
    Result0 = main1(Jsons, []),
    Result  = Result0 ++ [{<<"benches">>, bench_main()}],
    ok = file:write_file("../README.md",
                         bbmustache:compile(bbmustache:parse_file("../output.mustache"), Result, [{key_type, binary}])).

bench_main() ->
    Template = "Hello {{name}} You have just won {{value}} dollars! {{#in_ca}} Well, {{taxed_value}} dollars, after taxes. {{/in_ca}}",
    MapData  = #{<<"name">> => "Chris", <<"value">> => 10000, <<"taxed_value">> => 10000 - (10000 * 0.4), <<"in_ca">> => true},
    DictData = dict:from_list([{name, "Chris"}, {value, 10000}, {taxed_value, 10000 - (10000 * 0.4)}, {in_ca, true}]),
    Benches  = [
                {"bbmustache", fun(T, D) -> bbmustache:render(T, D, [{key_type, binary}]) end,
                 list_to_binary(Template), MapData},
                {"mustache.erl", fun(T, D) -> mustache:render(T, D) end,
                 Template, DictData}
               ],
    [
     [{<<"library">>, Library},
      {<<"result">>,  bench_run(Render, T, D)}] || {Library, Render, T, D} <- Benches
    ].

bench_run(Render, Template, Data) ->
    lists:sum([begin {T, _} = timer:tc(Render, [Template, Data]), T end || _ <- lists:seq(1, 1000)]).

main1([], Result) ->
    [{<<"spec_files">>, lists:reverse(Result)}];
main1([JsonPath | Rest], Result) ->
    {ok, JsonBin} = file:read_file(JsonPath),
    JsonDec = jsone:decode(JsonBin, [{object_format, proplist}]),
    Tests   = proplists:get_value(<<"tests">>, JsonDec),
    Ret = main2(Tests, []),
    main1(Rest, [[{<<"spec">>, filename:basename(JsonPath, ".json")},
                  {<<"libraries">>, [[{<<"library">>, L}] || {L, _} <- ?TEST_LIBRARIES]},
                  {<<"tests">>, Ret}
                 ] | Result]).

main2([], Result) ->
    lists:reverse(Result);
main2([Test | Tests], Result) ->
    Ret = [spec_test(Render, Test) || {_, Render} <- ?TEST_LIBRARIES],
    main2(Tests, [[
                   {<<"test">>, proplists:get_value(<<"name">>, Test)},
                   {<<"results">>, Ret}
                  ] | Result]).

spec_test(Render, Assoc) ->
    Data     = proplists:get_value(<<"data">>, Assoc),
    Expected = proplists:get_value(<<"expected">>, Assoc),
    Template = proplists:get_value(<<"template">>, Assoc),
    Partials = proplists:get_value(<<"partials">>, Assoc, []),

    ok = clean_dir("."),
    ok = lists:foreach(fun(P) -> write_file(P) end, Partials),

    {Pid, Ref} = spawn_monitor(fun() -> Expected = Render(Template, Data) end),
    Result     = receive
                     {'DOWN', Ref, _, _, Reason} -> Reason =:= normal
                 after 3000 ->
                         exit(Pid, kill),
                         demonitor(Ref, [flush]),
                         false
                 end,
    [{<<"result">>, Result}].

clean_dir(Dir) ->
    lists:foreach(fun(F) -> file:delete(F) end,
                  filelib:wildcard(filename:join(Dir, "*.mustache"))).

write_file({PartialFilename, PartialData}) ->
    ok = file:write_file(<<PartialFilename/binary, ".mustache">>, PartialData);
write_file(_) ->
    ok.

key_conv_recursive([{} | Rest], ConvFun) ->
    key_conv_recursive(Rest, ConvFun);
key_conv_recursive([{_, _} | _] = AssocList, ConvFun) ->
    lists:foldl(fun({Key, [{_, _} | _] = Value}, Acc) ->
                        [{ConvFun(Key), key_conv_recursive(Value, ConvFun)} | Acc];
                   ({Key, Value}, Acc) when is_list(Value) ->
                        [{ConvFun(Key), [key_conv_recursive(X, ConvFun) || X <- Value]} | Acc];
                   ({Key, Value}, Acc) ->
                        [{ConvFun(Key), Value} | Acc]
                end, [], AssocList);
key_conv_recursive(Other, _) ->
    Other.

list_to_dict_recursive([{_, _} | _] = AssocList) ->
    lists:foldl(fun({Key, [{_, _} | _] = Value}, Dict) ->
                        dict:store(Key, list_to_dict_recursive(Value), Dict);
                   ({Key, Value}, Dict) when is_list(Value) ->
                        dict:store(Key, [list_to_dict_recursive(X) || X <- Value], Dict);
                   ({Key, Value}, Dict) ->
                        dict:store(Key, Value, Dict)
                end, dict:new(), AssocList);
list_to_dict_recursive(Other) ->
    Other.
