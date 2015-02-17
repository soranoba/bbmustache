-module(bench).

-compile(export_all).

run() ->
    lists:sum([begin {T, _} = timer:tc(?MODULE, render, []), T end || _ <- lists:seq(1, 1000)]).

render() ->
    Ctx = #{"name" => "Chris", "value" => 10000, "taxed_value" => 10000 - (10000 * 0.4), "in_ca" => true},
    mustache:render(<<"Hello {{name}} You have just won {{value}} dollars! {{#in_ca}} Well, {{taxed_value}} dollars, after taxes. {{/in_ca}}">>, Ctx).
