%% @copyright 2015 Hinagiku Soranoba All Rights Reserved.

-module(bbmustache_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([
         all/0, groups/0,
         init_per_suite/1, end_per_suite/1,
         init_per_group/2, end_per_group/2,

         variables_ct/1, sections_ct1/1, sections_ct2/1, sections_ct3/1, sections_ct4/1,
         lambdas_ct/1, comments_ct/1, partials_ct/1, delimiter_ct/1, dot_ct/1, dot_unescape_ct/1
        ]).
-define(ALL_TEST, [variables_ct, sections_ct1, sections_ct2, sections_ct3, sections_ct4,
                   lambdas_ct, comments_ct, partials_ct, delimiter_ct, dot_ct, dot_unescape_ct]).

-ifdef(namespaced_types).
-define(OTP17(X, Y), X).
-else.
-define(OTP17(X, Y), Y).
-endif.

%%----------------------------------------------------------------------------------------------------------------------
%% 'common_test' Callback API
%%----------------------------------------------------------------------------------------------------------------------

all() ->
    A = [
         {group, assoc_list}
        ],
    B = [
         {group, maps},
         {group, assoc_list_into_maps},
         {group, maps_into_assoc_list}
        ],
    A ++ ?OTP17(B, []).

groups() ->
    A = [
         {assoc_list,           [], ?ALL_TEST}
        ],
    B = [
         {maps,                 [], ?ALL_TEST},
         {assoc_list_into_maps, [], ?ALL_TEST},
         {maps_into_assoc_list, [], ?ALL_TEST}
        ],
    A ++ ?OTP17(B, []).

init_per_suite(Config) ->
    ct:log(?OTP17("otp17 or later", "before otp17")),
    Config.

end_per_suite(_) ->
    ok.

init_per_group(maps, Config) ->
    [{data_conv, ?OTP17(fun list_to_maps_recursive/1, ok)} | Config];
init_per_group(assoc_list_into_map, Config) ->
    [{data_conv, ?OTP17(fun maps:from_list/1, ok)} | Config];
init_per_group(maps_into_assoc_list, Config) ->
    [{data_conv, ?OTP17(fun(X) -> deps_list_to_maps(X, 2) end, ok)} | Config];
init_per_group(_, Config) ->
    F = fun(X) -> X end,
    [{data_conv, F} | Config].

end_per_group(_, _) ->
    ok.

%%----------------------------------------------------------------------------------------------------------------------
%% Common Test Functions
%%----------------------------------------------------------------------------------------------------------------------

variables_ct(Config) ->
    Template   = bbmustache:parse_file(filename:join([?config(data_dir, Config), <<"variables.mustache">>])),
    {ok, File} = file:read_file(filename:join([?config(data_dir, Config), <<"variables.result">>])),

    Data = [{"name", "Chris"}, {"company", "<b>GitHub</b>"}],
    ?assertEqual(File, bbmustache:compile(Template, (?config(data_conv, Config))(Data))).

sections_ct1(Config) ->
    Template   = bbmustache:parse_file(filename:join([?config(data_dir, Config), <<"false_values.mustache">>])),
    {ok, File} = file:read_file(filename:join([?config(data_dir, Config), <<"false_values.result">>])),

    Data1 = [{"person", false}],
    Data2 = [{"person", []}],
    Data3 = [],
    [?assertEqual(File, bbmustache:compile(Template, (?config(data_conv, Config))(X)))
    || X <- [Data1, Data2, Data3]].

sections_ct2(Config) ->
    Template   = bbmustache:parse_file(filename:join([?config(data_dir, Config), <<"non-empty.mustache">>])),
    {ok, File} = file:read_file(filename:join([?config(data_dir, Config), <<"non-empty.result">>])),

    Data = [{"repo", [ [{"name", "resque"}], [{"name", "hub"}], [{"name", "rip"}]]}],
    ?assertEqual(File, bbmustache:compile(Template, (?config(data_conv, Config))(Data))).

sections_ct3(Config) ->
    Template   = bbmustache:parse_file(filename:join([?config(data_dir, Config), <<"non-false.mustache">>])),
    {ok, File} = file:read_file(filename:join([?config(data_dir, Config), <<"non-false.result">>])),

    Data = [{"person?", [{"name", "Jon"}]}],
    ?assertEqual(File, bbmustache:compile(Template, (?config(data_conv, Config))(Data))).

sections_ct4(Config) ->
    Template   = bbmustache:parse_file(filename:join([?config(data_dir, Config), <<"invarted.mustache">>])),
    {ok, File} = file:read_file(filename:join([?config(data_dir, Config), <<"invarted.result">>])),

    Data = [{"repo", []}],
    ?assertEqual(File, bbmustache:compile(Template, (?config(data_conv, Config))(Data))).

lambdas_ct(Config) ->
    Template   = bbmustache:parse_file(filename:join([?config(data_dir, Config), <<"lambdas.mustache">>])),
    {ok, File} = file:read_file(filename:join([?config(data_dir, Config), <<"lambdas.result">>])),

    F = fun(Text, Render) -> ["<b>", Render(Text), "</b>"] end,
    Data = [{"name", "Willy"}, {"wrapped", F}],
    ?assertEqual(File, bbmustache:compile(Template, (?config(data_conv, Config))(Data))).

comments_ct(Config) ->
    Template   = bbmustache:parse_file(filename:join([?config(data_dir, Config), <<"comment.mustache">>])),
    {ok, File} = file:read_file(filename:join([?config(data_dir, Config), <<"comment.result">>])),

    Data = [],
    ?assertEqual(File, bbmustache:compile(Template, (?config(data_conv, Config))(Data))).

partials_ct(Config) ->
    Template   = bbmustache:parse_file(filename:join([?config(data_dir, Config), <<"partial.mustache">>])),
    {ok, File} = file:read_file(filename:join([?config(data_dir, Config), <<"partial.result">>])),

    Data = [{"names", [[{"name", "alice"}], [{"name", "bob"}]]}],
    ?assertEqual(File, bbmustache:compile(Template, (?config(data_conv, Config))(Data))).

delimiter_ct(Config) ->
    Template   = bbmustache:parse_file(filename:join([?config(data_dir, Config), <<"delimiter.mustache">>])),
    {ok, File} = file:read_file(filename:join([?config(data_dir, Config), <<"delimiter.result">>])),

    Data = [{"default_tags", "tag1"}, {"erb_style_tags", "tag2"}, {"default_tags_again", "tag3"}],
    ?assertEqual(File, bbmustache:compile(Template, (?config(data_conv, Config))(Data))).

dot_ct(Config) ->
    Template   = bbmustache:parse_file(filename:join([?config(data_dir, Config), <<"dot.mustache">>])),
    {ok, File} = file:read_file(filename:join([?config(data_dir, Config), <<"dot.result">>])),

    Data = [{"mylist", ["<b>Item 1</b>", "<b>Item 2</b>", "<b>Item 3</b>"]}],
    ?assertEqual(File, bbmustache:compile(Template, (?config(data_conv, Config))(Data))).

dot_unescape_ct(Config) ->
    Template   = bbmustache:parse_file(filename:join([?config(data_dir, Config), <<"dot_unescape.mustache">>])),
    {ok, File} = file:read_file(filename:join([?config(data_dir, Config), <<"dot_unescape.result">>])),

    Data = [{"mylist", ["<b>Item 1</b>", "<b>Item 2</b>", "<b>Item 3</b>"]}],
    ?assertEqual(File, bbmustache:compile(Template, (?config(data_conv, Config))(Data))).

%%----------------------------------------------------------------------------------------------------------------------
%% Internal Functions
%%----------------------------------------------------------------------------------------------------------------------

-ifdef(namespaced_types).
%% @doc Recursively converted `map' into `assoc list'.
-spec list_to_maps_recursive([{term(), term()}]) -> #{}.
list_to_maps_recursive(AssocList) ->
    lists:foldl(fun({Key, [{_, _} | _] = Value}, Map) ->
                        maps:put(Key, list_to_maps_recursive(Value), Map);
                   ({Key, Value}, Map) ->
                        maps:put(Key, Value, Map)
                end, maps:new(), AssocList).

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
