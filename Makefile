APP=mustache
DIALYZER_OPTS=-Werror_handling -Wrace_conditions -Wunmatched_returns

LIBS=$(ERL_LIBS):deps

all: compile ct xref dialyze

compile:
	@./rebar3 compile

xref:
	@./rebar3 xref

clean:
	@./rebar3 clean

ct:
	@./rebar3 ct

eunit:
	@./rebar3 eunit

edoc:
	@./rebar3 doc skip_deps=true

start: compile
	@ERL_LIBS=$(LIBS) erl +stbt db +K true -pz ebin -s reloader -eval 'erlang:display(application:ensure_all_started($(APP))).'

dialyze:
	@./rebar3 dialyzer
