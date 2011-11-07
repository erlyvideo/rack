

all: getdeps
	rebar compile

getdeps:
	[ -d ./deps/cowboy ] || ../rebar get-deps
