%% @copyright 2015 Hinagiku Soranoba All Rights Reserved.

-module(bbmustache_tests).
-include_lib("eunit/include/eunit.hrl").

%%----------------------------------------------------------------------------------------------------------------------
%% Unit Tests
%%----------------------------------------------------------------------------------------------------------------------

-define(PARSE_ERROR, incorrect_format).
-define(FILE_ERROR,  file_not_found).

-define(NT_S(X, Y), ?_assertMatch({_, X, _, _, _}, bbmustache:parse_binary(Y))).
%% parse_binary_test generater (success case)
-define(NT_F(X, Y), ?_assertError(X,               bbmustache:parse_binary(Y))).
%% parse_binary_test generater (failure case)

parse_file_test_() ->
    [
     {"file_not_exist", ?_assertError(?FILE_ERROR, bbmustache:parse_file(<<"not_exist">>))}
    ].

parse_binary_test_() ->
    [
     {"bbmustache:template/0 format check", ?NT_S([], <<>>)},

     {"{{tag}}",     ?NT_S([<<"a">>, {n, <<"t">>}, <<"b">>],   <<"a{{t}}b">>)},
     {"{{ tag }}",   ?NT_S([{n, <<"t">>}],                     <<"{{ t }}">>)},
     {"{{ ta g }}",  ?NT_S([{n, <<"tag">>}],                   <<"{{ ta g }}">>)},
     {"{{}}",        ?NT_S([{n, <<>>}],                        <<"{{}}">>)},
     {"{{ }}",       ?NT_S([{n, <<>>}],                        <<"{{ }}">>)},
     {"{{tag",       ?NT_F({?PARSE_ERROR, unclosed_tag},       <<"{{tag">>)},

     {"{{{tag}}}",   ?NT_S([<<"a">>, {'&', <<"t">>}, <<"b">>], <<"a{{{t}}}b">>)},
     {"{{{ tag }}}", ?NT_S([{'&', <<"t">>}],                   <<"{{{ t }}}">>)},
     {"{{{ ta g }}}",?NT_S([{'&', <<"tag">>}],                 <<"{{{ ta g }}}">>)},
     {"{{{tag",      ?NT_F({?PARSE_ERROR, unclosed_tag},       <<"{{{tag">>)},

     {"{{& tag}}",   ?NT_S([<<"a">>, {'&', <<"t">>}, <<"b">>], <<"a{{& t}}b">>)},
     {"{{ & tag }}", ?NT_S([{'&', <<"t">>}],                   <<"{{ & t }}">>)},
     {"{{ & ta g }}",?NT_S([{'&', <<"tag">>}],                 <<"{{ & ta g }}">>)},
     {"{{&ta g }}",  ?NT_S([{'&', <<"tag">>}],                 <<"{{&ta g}}">>)},
     {"{{&tag}}",    ?NT_S([{'&', <<"t">>}],                   <<"{{&t}}">>)},

     {"{{/tag}}",    ?NT_F({?PARSE_ERROR, {section_is_incorrect, <<"tag">>}},       <<"{{/tag}}">>)},
     {"{{#tag}}",    ?NT_F({?PARSE_ERROR, {section_end_tag_not_found, <<"/tag">>}}, <<"{{#tag}}">>)},
     {"{{#tag1}}{{#tag2}}{{name}}{{/tag1}}{{/tag2}}",
      ?NT_S([<<"a">>, {'#', <<"t1">>, [<<"b">>,
                                       {'#', <<"t2">>, [<<"c">>, {n, <<"t3">>}, <<"d">>], <<"c{{t3}}d">>},
                                       <<"e">>], <<"b{{#t2}}c{{t3}}d{{/t2}}e">>}, <<"f">>],
            <<"a{{#t1}}b{{#t2}}c{{t3}}d{{/t2}}e{{/t1}}f">>)},
     {"{{#tag1}}{{#tag2}}{{/tag1}}{{/tag2}}",
      ?NT_F({?PARSE_ERROR, {section_is_incorrect, <<"t1">>}}, <<"{{#t1}}{{#t2}}{{/t1}}{{/t2}}">>)},

     {"{{# tag}}{{/ tag}}",     ?NT_S([{'#', <<"tag">>, [], <<>>}], <<"{{# tag}}{{/ tag}}">>)},
     {"{{ #tag }}{{ / tag }}",  ?NT_S([{'#', <<"tag">>, [], <<>>}], <<"{{ #tag }}{{ / tag }}">>)},
     {"{{ # tag }}{{ /tag }}",  ?NT_S([{'#', <<"tag">>, [], <<>>}], <<"{{ # tag }}{{ /tag }}">>)},
     {"{{ # ta g}}{{ / ta g}}", ?NT_S([{'#', <<"tag">>, [], <<>>}], <<"{{ # ta g}}{{ / ta g}}">>)},

     {"{{!comment}}",           ?NT_S([<<"a">>, <<"c">>], <<"a{{!comment}}c">>)},
     {"{{! comment }}",         ?NT_S([],                 <<"{{! comment }}">>)},
     {"{{! co mmen t }}",       ?NT_S([],                 <<"{{! co mmen t }}">>)},
     {"{{ !comment }}",         ?NT_S([],                 <<"{{ !comment }}">>)},
     {" {{ !comment }}  \r\n",  ?NT_S([],                 <<" {{ !comment }}  \r\n">>)},

     {"{{^tag}}", ?NT_F({?PARSE_ERROR, {section_end_tag_not_found, <<"/tag">>}}, <<"a{{^tag}}b">>)},
     {"{{^tag1}}{{^tag2}}{{name}}{{/tag2}}{{/tag1}}",
      ?NT_S([<<"a">>, {'^', <<"t1">>, [<<"b">>, {'^', <<"t2">>, [<<"c">>, {n, <<"t3">>}, <<"d">>]}, <<"e">>]}, <<"f">>],
            <<"a{{^t1}}b{{^t2}}c{{t3}}d{{/t2}}e{{/t1}}f">>)},
     {"{{^tag1}}{{^tag2}}{{/tag1}}{{tag2}}",
      ?NT_F({?PARSE_ERROR, {section_is_incorrect, <<"t1">>}}, <<"{{^t1}}{{^t2}}{{/t1}}{{/t2}}">>)},

     {"{{^ tag}}{{/ tag}}",     ?NT_S([{'^', <<"tag">>, []}], <<"{{^ tag}}{{/ tag}}">>)},
     {"{{ ^tag }}{{ / tag }}",  ?NT_S([{'^', <<"tag">>, []}], <<"{{ ^tag }}{{ / tag }}">>)},
     {"{{ ^ tag }}{{ /tag }}",  ?NT_S([{'^', <<"tag">>, []}], <<"{{ ^ tag }}{{ /tag }}">>)},
     {"{{ ^ ta g}}{{ / t ag}}", ?NT_S([{'^', <<"tag">>, []}], <<"{{ ^ ta g}}{{ / t ag}}">>)},

     {"{{=<< >>=}}{{n}}<<n>><<={{ }}=>>{{n}}<<n>>",
      ?NT_S([<<"a">>, <<"b{{n}}c">>, {n, <<"n">>}, <<"d">>, <<"e">>, {n, <<"m">>}, <<"f<<m>>g">>],
            <<"a{{=<< >>=}}b{{n}}c<<n>>d<<={{ }}=>>e{{m}}f<<m>>g">>)},
     {"{{=<< >>=}}<<#tag>><<{n}>><</tag>>",
      ?NT_S([{'#', <<"tag">>, [{'&', <<"n">>}], <<"<<{n}>>">>}],
            <<"{{=<< >>=}}<<#tag>><<{n}>><</tag>>">>)},
     {"{{=<<  >>=}}<<n>>",      ?NT_S([{n, <<"n">>}], <<"{{=<<  >>=}}<<n>>">>)},
     {"{{ = << >> = }}<<n>>",   ?NT_S([{n, <<"n">>}], <<"{{ = << >> = }}<<n>>">>)},
     {"{{=<= =>=}}<=n=>",       ?NT_F({?PARSE_ERROR, delimiters_may_not_contain_equals},      <<"{{=<= =>=}}<=n=>">>)},
     {"{{ = < < >> = }}< <n>>", ?NT_F({?PARSE_ERROR, delimiters_may_not_contain_whitespaces}, <<"{{ = < < >> = }}< <n>>">>)},
     {"{{=<< >>}}",             ?NT_F({?PARSE_ERROR, {unsupported_tag, <<"=<< >>">>}},        <<"{{=<< >>}}">>)}
    ].

assoc_list_render_test_() ->
    [
     {"integer, float, binary, string",
      fun() ->
              ?assertEqual(<<"1, 1.5, hoge, fugo, atom">>,
                           bbmustache:render(<<"{{i}}, {{f}}, {{b}}, {{s}}, {{a}}">>,
                                           [{"i", 1}, {"f", 1.5}, {"b", <<"hoge">>}, {"s", "fugo"}, {"a", atom}]))
      end}
    ].

atom_and_binary_key_test_() ->
    [
     {"atom key",
      fun() ->
              F = fun(Text, Render) -> ["<b>", Render(Text), "</b>"] end,
              ?assertEqual(<<"<b>Willy is awesome.</b>">>,
                           bbmustache:render(<<"{{#wrapped}}{{name}} is awesome.{{dummy_atom}}{{/wrapped}}">>,
                                           [{name, "Willy"}, {wrapped, F}], [{key_type, atom}])),
              ?assertError(_, binary_to_existing_atom(<<"dummy_atom">>, utf8))
      end},
     {"binary key",
      fun() ->
              F = fun(Text, Render) -> ["<b>", Render(Text), "</b>"] end,
              ?assertEqual(<<"<b>Willy is awesome.</b>">>,
                           bbmustache:render(<<"{{#wrapped}}{{name}} is awesome.{{dummy}}{{/wrapped}}">>,
                                           [{<<"name">>, "Willy"}, {<<"wrapped">>, F}], [{key_type, binary}]))
      end}
    ].

unsupported_data_test_() ->
    [
     {"dict", ?_assertError(function_clause, bbmustache:render(<<>>, dict:new()))}
    ].
