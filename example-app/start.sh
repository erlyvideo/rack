#!/usr/bin/env bash

erl +K true -smp enable -pa ebin deps/*/ebin \
    -eval "echo_get:start(\"./priv\")."
