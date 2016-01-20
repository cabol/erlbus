REBAR = $(shell which rebar3 || echo ./rebar3)

ifdef REBAR_PROFILE
PROFILE = $(REBAR_PROFILE)
else
PROFILE = default
endif

BUILD_PATH = ./_build/$(PROFILE)/lib/*/ebin

CONFIG ?= test/test.config

CT_OPTS = -cover test/cover.spec -erl_args -config ${CONFIG}
CT_SUITES = ebus_task_SUITE \
            ebus_ps_local_SUITE \
            ebus_ps_SUITE \
            ebus_handler_SUITE \
            ebus_dist_SUITE

.PHONY: all compile clean distclean dialyze tests shell doc

all: compile

compile:
	$(REBAR) compile

clean:
	rm -rf ebin/* test/*.beam logs log
	$(REBAR) clean

distclean: clean
	$(REBAR) clean --all
	rm -rf _build logs log doc

dialyze:
	$(REBAR) dialyzer

tests: compile
	mkdir -p logs
	ct_run -dir test -suite $(CT_SUITES) -pa $(BUILD_PATH) -logdir logs $(CT_OPTS)
	rm -rf test/*.beam

shell: compile
	erl -pa $(BUILD_PATH) -s ebus -config ${CONFIG}

doc:
	$(REBAR) edoc
