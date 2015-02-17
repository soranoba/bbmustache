%% coding: latin-1
%% @copyright 2015 Hinagiku Soranoba All Rights Reserved.

-module(mustache_tests).
-include_lib("eunit/include/eunit.hrl").

%%----------------------------------------------------------------------------------------------------------------------
%% Unit Tests
%%----------------------------------------------------------------------------------------------------------------------

-define(NT_S(X, Y), ?_assertMatch({_, X}, mustache:parse_binary(Y))).
%% parse_binary_test generater (success case)
-define(NT_F(X),    ?_assertError(_,      mustache:parse_binary(X))).
%% parse_binary_test generater (failure case)

parse_binary_test_() ->
    [
     {"mustache:template/0 format check", ?NT_S([<<>>], <<>>)},
     {"{{tag}}",     ?NT_S([<<"a">>, {n, <<"t">>}, <<"b">>],   <<"a{{t}}b">>)},
     {"{{ tag }}",   ?NT_S([<<>>, {n, <<"t">>}, <<>>],         <<"{{ t }}">>)},
     {"{{ ta g }}",  ?NT_S([<<>>, {n, <<"ta g">>}, <<>>],      <<"{{ ta g }}">>)},

     {"{{{tag}}}",   ?NT_S([<<"a">>, {'&', <<"t">>}, <<"b">>], <<"a{{{t}}}b">>)},
     {"{{{ tag }}}", ?NT_S([<<>>, {'&', <<"t">>}, <<>>],       <<"{{{ t }}}">>)},
     {"{{{ ta g }}}",?NT_S([<<>>, {'&', <<"ta g">>}, <<>>],    <<"{{{ ta g }}}">>)},

     {"{{& tag}}",   ?NT_S([<<"a">>, {'&', <<"t">>}, <<"b">>], <<"a{{& t}}b">>)},
     {"{{ & tag }}", ?NT_S([<<>>, {'&', <<"t">>}, <<>>],       <<"{{ & t }}">>)},
     {"{{ & ta g }}",?NT_S([<<>>, {'&', <<"ta g">>}, <<>>],    <<"{{ & ta g }}">>)},
     {"{{&ta g }}",  ?NT_S([<<>>, {'&', <<"ta g">>}, <<>>],    <<"{{&ta g}}">>)},
     {"{{&tag}}",    ?NT_S([<<>>, {'&', <<"t">>}, <<>>],       <<"{{&t}}">>)},

     {"{{#tag}}",    ?NT_F(<<"{{#tag}}">>)},
     {"{{#tag1}}{{#tag2}}{{name}}{{/tag1}}{{/tag2}}",
      ?NT_S([<<"a">>, {'#', <<"t1">>, [<<"b">>,
                                       {'#', <<"t2">>, [<<"c">>, {n, <<"t3">>}, <<"d">>], <<"c{{t3}}d">>},
                                       <<"e">>], <<"b{{#t2}}c{{t3}}d{{/t2}}e">>}, <<"f">>],
            <<"a{{#t1}}b{{#t2}}c{{t3}}d{{/t2}}e{{/t1}}f">>)},
     {"{{#tag1}}{{#tag2}}{{/tag1}}{{/tag2}}", ?NT_F(<<"{{#t1}}{{#t2}}{{/t1}}{{/t2}}">>)},

     {"{{# tag}}{{/ tag}}",     ?NT_S([<<>>, {'#', <<"tag">>,  [<<>>], <<>>}, <<>>], <<"{{# tag}}{{/ tag}}">>)},
     {"{{ #tag }}{{ / tag }}",  ?NT_S([<<>>, {'#', <<"tag">>,  [<<>>], <<>>}, <<>>], <<"{{ #tag }}{{ / tag }}">>)},
     {"{{ # tag }}{{ /tag }}",  ?NT_S([<<>>, {'#', <<"tag">>,  [<<>>], <<>>}, <<>>], <<"{{ # tag }}{{ /tag }}">>)},
     {"{{ # ta g}}{{ / ta g}}", ?NT_S([<<>>, {'#', <<"ta g">>, [<<>>], <<>>}, <<>>], <<"{{ # ta g}}{{ / ta g}}">>)},

     {"{{!comment}}",           ?NT_S([<<"a">>, <<"c">>], <<"a{{!comment}}c">>)},
     {"{{! comment }}",         ?NT_S([<<>>, <<>>],       <<"{{! comment }}">>)},
     {"{{! co mmen t }}",       ?NT_S([<<>>, <<>>],       <<"{{! co mmen t }}">>)},
     {"{{ !comment }}",         ?NT_S([<<>>, <<>>],       <<"{{ !comment }}">>)},

     {"{{^tag}}",    ?NT_F(<<"a{{^tag}}b">>)},
     {"{{^tag1}}{{^tag2}}{{name}}{{/tag2}}{{/tag1}}",
      ?NT_S([<<"a">>, {'^', <<"t1">>, [<<"b">>, {'^', <<"t2">>, [<<"c">>, {n, <<"t3">>}, <<"d">>]}, <<"e">>]}, <<"f">>],
            <<"a{{^t1}}b{{^t2}}c{{t3}}d{{/t2}}e{{/t1}}f">>)},
     {"{{^tag1}}{{^tag2}}{{/tag1}}{{tag2}}", ?NT_F(<<"{{^t1}}{{^t2}}{{/t1}}{{/t2}}">>)},

     {"{{^ tag}}{{/ tag}}",     ?NT_S([<<>>, {'^', <<"tag">>,  [<<>>]}, <<>>], <<"{{^ tag}}{{/ tag}}">>)},
     {"{{ ^tag }}{{ / tag }}",  ?NT_S([<<>>, {'^', <<"tag">>,  [<<>>]}, <<>>], <<"{{ ^tag }}{{ / tag }}">>)},
     {"{{ ^ tag }}{{ /tag }}",  ?NT_S([<<>>, {'^', <<"tag">>,  [<<>>]}, <<>>], <<"{{ ^ tag }}{{ /tag }}">>)},
     {"{{ ^ ta g}}{{ / ta g}}", ?NT_S([<<>>, {'^', <<"ta g">>, [<<>>]}, <<>>], <<"{{ ^ ta g}}{{ / ta g}}">>)},

     {"{{=<< >>=}}{{n}}<<n>><<={{ }}=>>{{n}}<<n>>",
      ?NT_S([<<"a">>, <<"b{{n}}c">>, {n, <<"n">>}, <<"d">>, <<"e">>, {n, <<"m">>}, <<"f<<m>>g">>],
            <<"a{{=<< >>=}}b{{n}}c<<n>>d<<={{ }}=>>e{{m}}f<<m>>g">>)},
     {"{{=<< >>=}}<<#tag>><<{n}>><</tag>>",
      ?NT_S([<<>>, <<>>, {'#', <<"tag">>, [<<>>, {'&', <<"n">>}, <<>>], <<"<<{n}>>">>}, <<>>], <<"{{=<< >>=}}<<#tag>><<{n}>><</tag>>">>)},

     {"{{=<<  >>=}}<<n>>",      ?NT_S([<<>>, <<>>, {n, <<"n">>}, <<>>], <<"{{=<<  >>=}}<<n>>">>)},
     {"{{ = << >> = }}<<n>>",   ?NT_S([<<>>, <<>>, {n, <<"n">>}, <<>>], <<"{{ = << >> = }}<<n>>">>)},
     {"{{=<= =>=}}<=n=>",       ?NT_F(<<"{{=<= =>=}}<=n=>">>)},
     {"{{ = < < >> = }}< <n>>", ?NT_F(<<"{{ = < < >> = }}< <n>>">>)}
    ].

-define(PATH(File), <<"../test/test_data/", File/binary>>).
%% TestData Path

manual_test_() ->
    [
     {"Variables",
      fun() ->
              Template   = mustache:parse_file(?PATH(<<"variables.mustache">>)),
              {ok, File} = file:read_file(?PATH(<<"variables.result">>)),
              ?assertEqual(File, mustache:compile(Template, #{ "name" => "Chris", "company" => "<b>GitHub</b>"}))
      end},
     {"Sections : False Values or Empty Lists",
      fun() ->
              Template   = mustache:parse_file(?PATH(<<"false_values.mustache">>)),
              {ok, File} = file:read_file(?PATH(<<"false_values.result">>)),
              ?assertEqual(File, mustache:compile(Template, #{ "person" => false })),
              ?assertEqual(File, mustache:compile(Template, #{ "person" => []})),
              ?assertEqual(File, mustache:compile(Template, #{}))
      end},
     {"Sections : Non-Empty Lists",
      fun() ->
              Template   = mustache:parse_file(?PATH(<<"non-empty.mustache">>)),
              {ok, File} = file:read_file(?PATH(<<"non-empty.result">>)),
              ?assertEqual(File, mustache:compile(Template, #{ "repo" => [
                                                                          #{ "name" => "resque" },
                                                                          #{ "name" => "hub" },
                                                                          #{ "name" => "rip" }
                                                                         ]}))
      end},
     {"Sections : Lamdas",
      fun() ->
              Template   = mustache:parse_file(?PATH(<<"lamdas.mustache">>)),
              {ok, File} = file:read_file(?PATH(<<"lamdas.result">>)),

              F = fun(Text, Render) -> ["<b>", Render(Text), "</b>"] end,
              ?assertEqual(File, mustache:compile(Template, #{ "name" => "Willy", "wrapped" => F}))
      end},
     {"Sections : Non-False Values",
      fun() ->
              Template   = mustache:parse_file(?PATH(<<"non-false.mustache">>)),
              {ok, File} = file:read_file(?PATH(<<"non-false.result">>)),
              ?assertEqual(File, mustache:compile(Template, #{ "person?" => #{ "name" => "Jon" }}))
      end},
     {"Inverted Sections",
      fun() ->
              Template   = mustache:parse_file(?PATH(<<"invarted.mustache">>)),
              {ok, File} = file:read_file(?PATH(<<"invarted.result">>)),
              ?assertEqual(File, mustache:compile(Template, #{ "repo" => []}))
      end},
     {"Comments",
      fun() ->
              Template   = mustache:parse_file(?PATH(<<"comment.mustache">>)),
              {ok, File} = file:read_file(?PATH(<<"comment.result">>)),
              ?assertEqual(File, mustache:compile(Template, #{}))
      end},
     {"Partials",
      fun() ->
              Template   = mustache:parse_file(?PATH(<<"partial.mustache">>)),
              {ok, File} = file:read_file(?PATH(<<"partial.result">>)),
              ?assertEqual(File, mustache:compile(Template, #{ "names" => [
                                                                           #{ "name" => "alice" },
                                                                           #{ "name" => "bob" }
                                                                          ]}))
      end},
     {"Set Delimiter",
      fun() ->
              Template   = mustache:parse_file(?PATH(<<"delimiter.mustache">>)),
              {ok, File} = file:read_file(?PATH(<<"delimiter.result">>)),
              ?assertEqual(File, mustache:compile(Template, #{ "default_tags"       => "tag1",
                                                               "erb_style_tags"     => "tag2",
                                                               "default_tags_again" => "tag3"
                                                             }))
      end}
    ].

render_test_() ->
    [
     {"integer, float, binary, string",
      fun() ->
              ?assertEqual(<<"1, 1.5, hoge, fugo">>,
                           mustache:render(<<"{{i}}, {{f}}, {{b}}, {{s}}">>,
                                           #{"i" => 1, "f" => 1.5, "b" => <<"hoge">>, "s" => "fugo"}))
      end}
    ].
