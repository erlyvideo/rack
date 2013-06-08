%% Feel free to use, reuse and abuse the code in this file.

%% @private
-module(echo_get_app).
-behaviour(application).

%% API.
-export([start/2]).
-export([stop/1]).

%% API.

start(_Type, _Args) ->
	% get rails app path
	{ok, Path} = application:get_env(echo_get, rails_app_path),

	Dispatch = cowboy_router:compile([
		{'_', [
			{"/[...]", cowboy_rack_handler, [{path, Path}]}
		]}
	]),
	{ok, _} = cowboy:start_http(http, 100, [{port, 8085}], [
		{env, [{dispatch, Dispatch}]}
	]),
	echo_get_sup:start_link().

stop(_State) ->
	ok.
