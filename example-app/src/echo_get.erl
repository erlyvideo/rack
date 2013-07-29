%% Feel free to use, reuse and abuse the code in this file.

-module(echo_get).

%% API.
-export([start/1]).

%% API.

start(Path) ->
	ok = application:start(crypto),
	ok = application:start(ranch),
	ok = application:start(cowboy),
	
	application:set_env(echo_get, rails_app_path, Path),
	ok = application:start(echo_get).
