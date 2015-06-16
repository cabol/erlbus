PROJECT = pub_sub

DEPS = erlbus

dep_erlbus = git https://github.com/cabol/erlbus.git master

include erlang.mk

run: all
	erl -pa ebin -pa deps/*/ebin \
	-s pub_sub
