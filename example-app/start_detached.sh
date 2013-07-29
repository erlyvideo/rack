#!/usr/bin/env bash

run_erl -daemon /tmp/ralog/ /tmp/ralog/ "erl +K true -smp enable -pa ebin deps/*/ebin -eval 'echo_get:start(\"./priv\").'"
#erl +K true -smp enable -detached -pa ebin deps/*/ebin \
#    -eval "echo_get:start(\"./priv\")."
