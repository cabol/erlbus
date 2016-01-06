REBAR = ./rebar3

BUILD_PATH = ./_build/default/lib/*/ebin

CONFIG ?= test/test.config

CT_OPTS = -cover test/cover.spec -erl_args -config ${CONFIG}
CT_SUITES = ebus_ps_local_SUITE ebus_ps_SUITE ebus_handler_SUITE ebus_dist_SUITE

.PHONY: all compile clean distclean dialyze tests shell

all: compile

compile:
	$(REBAR) compile

clean:
	rm -rf ebin/* test/*.beam logs log
	$(REBAR) clean

distclean: clean
	$(REBAR) clean --all
	rm -rf _build logs log

dialyze:
	$(REBAR) dialyzer

tests: compile
	mkdir -p logs
	ct_run -dir test -suite $(CT_SUITES) -pa $(BUILD_PATH) -logdir logs $(CT_OPTS)
	rm -rf test/*.beam

shell: compile
	erl -pa $(BUILD_PATH) -s ebus -config ${CONFIG}
