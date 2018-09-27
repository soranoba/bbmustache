.PHONY: ct
all: compile eunit ct xref dialyze edoc

compile:
	@./rebar3 as dev compile

xref:
	@./rebar3 xref

clean:
	@./rebar3 clean

ct:
	@./rebar3 ct

cover:
	@./rebar3 cover

eunit:
	@./rebar3 eunit

edoc:
	@./rebar3 as dev edoc

start:
	@./rebar3 as dev shell

dialyze:
	@./rebar3 dialyzer

bench:
	@./rebar3 as test compile
	@./rebar3 as bench compile
	@./benchmarks/bench.escript
