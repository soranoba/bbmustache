APP=mustache
DIALYZER_OPTS=-Werror_handling -Wrace_conditions -Wunmatched_returns

LIBS=$(ERL_LIBS):deps

all: compile xref eunit dialyze

init:
	@./rebar get-deps compile

refresh-deps:
	@./rebar refresh-deps

compile:
	@./rebar compile skip_deps=true

xref:
	@./rebar xref skip_deps=true

clean:
	@./rebar clean skip_deps=true

distclean:
	git clean -ffdx

eunit:
	@./rebar eunit skip_deps=true

edoc:
	@./rebar doc skip_deps=true

start: compile
	@ERL_LIBS=$(LIBS) erl +stbt db +K true -pz ebin -s reloader -eval 'erlang:display(application:ensure_all_started($(APP))).'

.dialyzer.plt:
	touch .dialyzer.plt
	ERL_LIBS=$(LIBS) dialyzer --build_plt --plt .dialyzer.plt --apps erts \
		$(shell ERL_LIBS=$(LIBS) erl -noshell -pa ebin -eval '{ok, _} = application:ensure_all_started($(APP)), [erlang:display(Name) || {Name, _, _} <- application:which_applications(), Name =/= $(APP)], halt().')

dialyze: .dialyzer.plt compile
	ERL_LIBS=$(LIBS) dialyzer --no_native -pa ebin --plt .dialyzer.plt -I deps -r ebin $(DIALYZER_OPTS)

# Interactive eunit (Experimental)
iunit: compile
	@ERL_LIBS=$(LIBS) erl +K true -pz ebin .eunit -s reloader -eval 'erlang:display(application:ensure_all_started($(APP))).'

