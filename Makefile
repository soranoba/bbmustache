CWD=$(shell pwd)

.PHONY: ct
all: compile escriptize eunit ct xref dialyze edoc
ci: compile escriptize eunit ct xref dialyze

compile:
	@rebar3 as dev compile

xref:
	@rebar3 as dev xref

clean:
	@rebar3 clean

ct: escriptize
	@CMD_TOOL=$(CWD)/bbmustache rebar3 ct

cover:
	@rebar3 cover

eunit:
	@rebar3 eunit

edoc:
	@rebar3 as doc edoc

start:
	@rebar3 as dev shell

dialyze:
	@rebar3 as dev dialyzer

bench:
	@rebar3 as test compile
	@rebar3 as bench compile
	@./benchmarks/bench.escript

escriptize:
	@rebar3 as dev escriptize
	@cp _build/dev/bin/bbmustache .

install: escriptize
	cp bbmustache /usr/local/bin
