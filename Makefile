PROJECT = ebus

CONFIG ?= test/config/test.config

ifdef EBUS_DIST
DEPS = gproc poolboy riak_core
else
DEPS = gproc poolboy
endif

TEST_DEPS =

dep_gproc     = git https://github.com/uwiger/gproc.git    0.4
dep_poolboy   = git https://github.com/devinus/poolboy.git 1.5.1
dep_riak_core = git https://github.com/basho/riak_core     2.1.1

DIALYZER_DIRS := ebin/
DIALYZER_OPTS := --verbose --statistics -Werror_handling \
                 -Wrace_conditions #-Wunmatched_returns

include erlang.mk

ERLC_OPTS += +debug_info +fail_on_warning

TEST_ERLC_OPTS += +debug_info +fail_on_warning
CT_SUITES = ebus_pg2_gproc ebus_local
CT_OPTS += -cover test/cover.spec -erl_args -config ${CONFIG} -ebus ebus_dist all

CT_SUITES1 = ebus_pg2_gproc ebus_local
CT_SUITES2 = ebus_dist
CT_OPTS1 += -cover test/cover.spec -erl_args -config ${CONFIG}
CT_OPTS2 += -cover test/cover.spec -erl_args -config ${CONFIG} -ebus ebus_dist all

SHELL_OPTS = -name ${PROJECT}@`hostname` -s ${PROJECT} -config ${CONFIG}

test1:
	mkdir -p logs/ ; \
	$(gen_verbose) $(CT_RUN) -suite $(addsuffix _SUITE,$(CT_SUITES1)) $(CT_OPTS1)

test2:
	mkdir -p logs/ ; \
	$(gen_verbose) $(CT_RUN) -suite $(addsuffix _SUITE,$(CT_SUITES2)) $(CT_OPTS2)

test: test-build test1 test2
	$(gen_verbose) rm -f test/*.beam

test-shell: build-ct-suites app
	erl -pa ebin -pa deps/*/ebin -pa test -s ebus -config ${CONFIG}

devtests: tests
	open logs/index.html

quicktests: app build-ct-suites
	@if [ -d "test" ] ; \
	then \
		mkdir -p logs/ ; \
		$(CT_RUN) -suite $(addsuffix _SUITE,$(CT_SUITES)) $(CT_OPTS) ; \
	fi
	$(gen_verbose) rm -f test/*.beam

erldocs: app
	erldocs . -o doc/
