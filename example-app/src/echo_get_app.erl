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

	% start event manager
	echo_event_manager:start(),

	Dispatch = cowboy_router:compile([
		{'_', [
			{"/ws", 	cowboy_websocket_rack_handler, 		[{path, Path}, {handler, echo_handler}, {event_manager, echo_event_manager}]},
			{'_', 		cowboy_rack_handler, 				[{path, Path}, {handler, echo_handler}]}
		]}
	]),
	{ok, _} = cowboy:start_http(http, 100, [{port, 8085}], [
		{env, [{dispatch, Dispatch}]}
	]),
	echo_get_sup:start_link().

stop(_State) ->
	ok.
