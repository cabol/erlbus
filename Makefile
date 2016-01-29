REBAR = $(shell which rebar3 || echo ./rebar3)

ifdef REBAR_PROFILE
PROFILE = $(REBAR_PROFILE)
else
PROFILE = default
endif

BUILD_ROOT = ./_build/$(PROFILE)/lib
BUILD_PATH = $(BUILD_ROOT)/*/ebin

ELIXIR = $(BUILD_ROOT)/elixir
ELIXIR_LIB = $(ELIXIR)/lib
ELIXIR_BIN = $(ELIXIR)/bin
ELIXIR_PATH = $(ELIXIR_LIB)/*/ebin

CONFIG ?= test/test.config

CT_OPTS = -cover test/cover.spec -erl_args -config ${CONFIG}
CT_SUITES = ebus_task_SUITE \
            ebus_ps_local_SUITE \
            ebus_ps_SUITE \
            ebus_handler_SUITE \
            ebus_dist_SUITE

.PHONY: all compile clean distclean dialyze tests shell doc

all: compile

compile_phoenix_pubsub:
	MIX_END=dev $(ELIXIR_BIN)/mix deps.get
	MIX_END=dev $(ELIXIR_BIN)/mix compile
	rm -rf $(BUILD_ROOT)/phoenix_pubsub
	cp -a ./_build/dev/lib/phoenix_pubsub $(BUILD_ROOT)

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
	ct_run -dir test -suite $(CT_SUITES) -pa $(BUILD_PATH) $(ELIXIR_PATH) -logdir logs $(CT_OPTS)
	rm -rf test/*.beam

shell: compile
	erl -pa $(BUILD_PATH) $(ELIXIR_PATH) -s ebus -config ${CONFIG}

doc:
	$(REBAR) edoc
