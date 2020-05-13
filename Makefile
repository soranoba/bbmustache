.PHONY: ct
all: compile eunit ct xref dialyze edoc escriptize

compile:
	@./rebar3 as dev compile

xref:
	@./rebar3 as dev xref

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
	@./rebar3 as dev dialyzer

bench:
	@./rebar3 as test compile
	@./rebar3 as bench compile
	@./benchmarks/bench.escript

escriptize:
	@./rebar3 as dev escriptize

install: escriptize
	cp _build/dev/bin/bbmustache /usr/local/bin
