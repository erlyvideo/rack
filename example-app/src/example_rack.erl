-module(example_rack).
-author('Max Lapshin <max@maxidoors.ru>').

-export([start/1, start/2, stop/1]).

start(Path) ->
  application:load(example_rack),
  application:set_env(example_rack, path, Path),
  application:start(cowboy),
  application:start(example_rack).


start(_, _) ->
  {ok, Path} = application:get_env(example_rack, path),
  Dispatch = [
		{'_', [
			{['...'], cowboy_rack_handler, [{path, Path}]}
		]}
	],
	cowboy:start_listener(http, 1,
		cowboy_tcp_transport, [{port, 8080}],
		cowboy_http_protocol, [{dispatch, Dispatch}]
	).

stop(_) ->
  ok.
