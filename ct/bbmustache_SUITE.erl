%% @copyright 2015 Hinagiku Soranoba All Rights Reserved.

-module(bbmustache_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([
         all/0, groups/0,
         init_per_suite/1, end_per_suite/1,
         init_per_group/2, end_per_group/2,

         variables_ct/1, sections_ct1/1, sections_ct2/1, sections_ct3/1, sections_ct4/1,
         lambdas_ct/1, comments_ct/1, partials_ct/1, delimiter_ct/1, dot_ct/1, dot_unescape_ct/1,
         indent_partials_ct/1
        ]).
-define(ALL_TEST, [variables_ct, sections_ct1, sections_ct2, sections_ct3, sections_ct4,
                   lambdas_ct, comments_ct, partials_ct, delimiter_ct, dot_ct, dot_unescape_ct,
                   indent_partials_ct]).

-define(config2, proplists:get_value).
-define(debug(X), begin io:format("~p", [X]), X end).

-ifdef(namespaced_types).
-define(OTP17(X, Y), X).
-else.
-define(OTP17(X, Y), Y).
-endif.

%%----------------------------------------------------------------------------------------------------------------------
%% 'common_test' Callback API
%%----------------------------------------------------------------------------------------------------------------------

all() ->
    [
     {group, assoc_list},
     {group, maps},
     {group, assoc_list_into_maps},
     {group, maps_into_assoc_list},
     {group, atom_key},
     {group, binary_key}
    ].

groups() ->
    [
     {assoc_list,           [], ?ALL_TEST},
     {maps,                 [], ?ALL_TEST},
     {assoc_list_into_maps, [], ?ALL_TEST},
     {maps_into_assoc_list, [], ?ALL_TEST},
     {atom_key,             [], ?ALL_TEST},
     {binary_key,           [], ?ALL_TEST}
    ].

init_per_suite(Config) ->
    ct:log(?OTP17("otp17 or later", "before otp17")),
    Config.

end_per_suite(_) ->
    ok.

init_per_group(assoc_list, Config) ->
    [{data_conv, fun(X) -> X end} | Config];
init_per_group(maps, _Config) ->
    ?OTP17([{data_conv, fun list_to_maps_recursive/1} | _Config],
           {skip, map_is_unsupported});
init_per_group(assoc_list_into_maps, _Config) ->
    ?OTP17([{data_conv, fun maps:from_list/1} | _Config],
           {skip, map_is_unsupported});
init_per_group(maps_into_assoc_list, _Config) ->
    ?OTP17([{data_conv, fun(X) -> deps_list_to_maps(X, 2) end} | _Config],
           {skip, map_is_unsupported});
init_per_group(atom_key, Config) ->
    [{data_conv, fun(X) -> key_conv_recursive(X, fun erlang:list_to_atom/1) end},
     {options, [{key_type, atom}]}
     | Config];
init_per_group(binary_key, Config) ->
    [{data_conv, fun(X) -> key_conv_recursive(X, fun erlang:list_to_binary/1) end},
     {options, [{key_type, binary}]}
     | Config].

end_per_group(_, _) ->
    ok.

%%----------------------------------------------------------------------------------------------------------------------
%% Common Test Functions
%%----------------------------------------------------------------------------------------------------------------------

variables_ct(Config) ->
    Template   = bbmustache:parse_file(filename:join([?config(data_dir, Config), <<"variables.mustache">>])),
    {ok, File} = file:read_file(filename:join([?config(data_dir, Config), <<"variables.result">>])),

    Data = [{"name", "Chris"}, {"company", "<b>GitHub</b>"}],
    ?assertEqual(File, bbmustache:compile(Template, ?debug((?config(data_conv, Config))(Data)), ?config2(options, Config, []))).

sections_ct1(Config) ->
    Template   = bbmustache:parse_file(filename:join([?config(data_dir, Config), <<"false_values.mustache">>])),
    {ok, File} = file:read_file(filename:join([?config(data_dir, Config), <<"false_values.result">>])),

    Data1 = [{"person", false}],
    Data2 = [{"person", []}],
    Data3 = [],
    [?assertEqual(File, bbmustache:compile(Template, ?debug((?config(data_conv, Config))(X)), ?config2(options, Config, [])))
    || X <- [Data1, Data2, Data3]].

sections_ct2(Config) ->
    Template   = bbmustache:parse_file(filename:join([?config(data_dir, Config), <<"non-empty.mustache">>])),
    {ok, File} = file:read_file(filename:join([?config(data_dir, Config), <<"non-empty.result">>])),

    Data = [{"repo", [ [{"name", "resque"}], [{"name", "hub"}], [{"name", "rip"}]]}],
    ?assertEqual(File, bbmustache:compile(Template, ?debug((?config(data_conv, Config))(Data)), ?config2(options, Config, []))).

sections_ct3(Config) ->
    Template   = bbmustache:parse_file(filename:join([?config(data_dir, Config), <<"non-false.mustache">>])),
    {ok, File} = file:read_file(filename:join([?config(data_dir, Config), <<"non-false.result">>])),

    Data = [{"person?", [{"name", "Jon"}]}],
    ?assertEqual(File, bbmustache:compile(Template, ?debug((?config(data_conv, Config))(Data)), ?config2(options, Config, []))).

sections_ct4(Config) ->
    Template   = bbmustache:parse_file(filename:join([?config(data_dir, Config), <<"invarted.mustache">>])),
    {ok, File} = file:read_file(filename:join([?config(data_dir, Config), <<"invarted.result">>])),

    Data = [{"repo", []}],
    ?assertEqual(File, bbmustache:compile(Template, ?debug((?config(data_conv, Config))(Data)), ?config2(options, Config, []))).

lambdas_ct(Config) ->
    Template   = bbmustache:parse_file(filename:join([?config(data_dir, Config), <<"lambdas.mustache">>])),
    {ok, File} = file:read_file(filename:join([?config(data_dir, Config), <<"lambdas.result">>])),

    F = fun(Text, Render) -> ["<b>", Render(Text), "</b>"] end,
    Data = [{"name", "Willy"}, {"wrapped", F}],
    ?assertEqual(File, bbmustache:compile(Template, ?debug((?config(data_conv, Config))(Data)), ?config2(options, Config, []))).

comments_ct(Config) ->
    Template   = bbmustache:parse_file(filename:join([?config(data_dir, Config), <<"comment.mustache">>])),
    {ok, File} = file:read_file(filename:join([?config(data_dir, Config), <<"comment.result">>])),

    Data = [],
    ?assertEqual(File, bbmustache:compile(Template, ?debug((?config(data_conv, Config))(Data)), ?config2(options, Config, []))).

partials_ct(Config) ->
    Template   = bbmustache:parse_file(filename:join([?config(data_dir, Config), <<"partial.mustache">>])),
    {ok, File} = file:read_file(filename:join([?config(data_dir, Config), <<"partial.result">>])),

    Data = [{"names", [[{"name", "alice"}], [{"name", "bob"}]]}],
    ?assertEqual(File, bbmustache:compile(Template, ?debug((?config(data_conv, Config))(Data)), ?config2(options, Config, []))).

delimiter_ct(Config) ->
    Template   = bbmustache:parse_file(filename:join([?config(data_dir, Config), <<"delimiter.mustache">>])),
    {ok, File} = file:read_file(filename:join([?config(data_dir, Config), <<"delimiter.result">>])),

    Data = [{"default_tags", "tag1"}, {"erb_style_tags", "tag2"}, {"default_tags_again", "tag3"}],
    ?assertEqual(File, bbmustache:compile(Template, ?debug((?config(data_conv, Config))(Data)), ?config2(options, Config, []))).

dot_ct(Config) ->
    Template   = bbmustache:parse_file(filename:join([?config(data_dir, Config), <<"dot.mustache">>])),
    {ok, File} = file:read_file(filename:join([?config(data_dir, Config), <<"dot.result">>])),

    Data = [{"mylist", ["<b>Item 1</b>", "<b>Item 2</b>", "<b>Item 3</b>"]}],
    ?assertEqual(File, bbmustache:compile(Template, ?debug((?config(data_conv, Config))(Data)), ?config2(options, Config, []))).

dot_unescape_ct(Config) ->
    Template   = bbmustache:parse_file(filename:join([?config(data_dir, Config), <<"dot_unescape.mustache">>])),
    {ok, File} = file:read_file(filename:join([?config(data_dir, Config), <<"dot_unescape.result">>])),

    Data = [{"mylist", ["<b>Item 1</b>", "<b>Item 2</b>", "<b>Item 3</b>"]}],
    ?assertEqual(File, bbmustache:compile(Template, ?debug((?config(data_conv, Config))(Data)), ?config2(options, Config, []))).

indent_partials_ct(Config) ->
    Template   = bbmustache:parse_file(filename:join([?config(data_dir, Config), <<"a.mustache">>])),
    {ok, File} = file:read_file(filename:join([?config(data_dir, Config), <<"a.result">>])),

    Data = [{"sections", [[{"section", "1st section"}], [{"section", "2nd section"}]]}],
    ?assertEqual(File, bbmustache:compile(Template, ?debug((?config(data_conv, Config))(Data)), ?config2(options, Config, []))).

%%----------------------------------------------------------------------------------------------------------------------
%% Internal Functions
%%----------------------------------------------------------------------------------------------------------------------

-ifdef(namespaced_types).
%% @doc Recursively converted `map' into `assoc list'.
list_to_maps_recursive([{_, _} | _] = AssocList) ->
    lists:foldl(fun({Key, [{_, _} | _] = Value}, Map) ->
                        maps:put(Key, list_to_maps_recursive(Value), Map);
                   ({Key, Value}, Map) when is_list(Value) ->
                        maps:put(Key, [list_to_maps_recursive(X) || X <- Value], Map);
                   ({Key, Value}, Map) ->
                        maps:put(Key, Value, Map)
                end, maps:new(), AssocList);
list_to_maps_recursive(Other) ->
    Other.

%% @doc Convert `map' into `assoc list' that exist at the specified depth.
-spec deps_list_to_maps([{term(), term()}], Deps :: pos_integer()) -> [{term(), term()}] | #{}.
deps_list_to_maps(AssocList, 1) ->
    maps:from_list(AssocList);
deps_list_to_maps(AssocList, Deps) when Deps > 1 ->
    R = lists:foldl(fun({Key, [{_, _} | _] = Value}, Acc) ->
                            [{Key, deps_list_to_maps(Value, Deps - 1)} | Acc];
                       ({Key, Value}, Acc) ->
                            [{Key, Value} | Acc]
                    end, [], AssocList),
    lists:reverse(R).

-endif.

%% @doc Recursively converted keys in assoc list.
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
