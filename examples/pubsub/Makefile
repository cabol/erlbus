REBAR = ../../rebar3

BUILD_PATH = ./_build/default/lib/*/ebin

CONFIG ?= config/sys.config

.PHONY: all compile clean distclean run

all: compile

compile:
	$(REBAR) compile

clean:
	$(REBAR) clean

distclean: clean
	$(REBAR) clean --all
	rm -rf _build *.lock

run: all
	erl -pa $(BUILD_PATH) -s ebus -s pubsub -config ${CONFIG}
