Rack
====


This is a library, that launched Ruby on Rails (or any other Rack application) behind Erlang HTTP server.

All erlang cool features like Comet, WebSockets and session management are accessible to you now!


Some details
-----

Rack for erlang is an application, that spawns several [rack](http://rack.rubyforge.org/) handlers and share requests between them.

You can think about it as a replacement for Passenger.

Currently it is compatible with [Cowboy HTTP server](https://github.com/extend/cowboy).



Quick start
----------


Add rack dependency to rebar.conf:

```erlang
{deps, [
	{rack, ".*", {git, "git://github.com/erlyvideo/rack.git", "master"}}
]}
```


Add dispatch handler to cowboy:

```erlang
Dispatch = [
	{'_', [
	  {['...'], cowboy_rack_handler, [{path,"../my_rails_app"}]}
	]}
],
cowboy:start_listener(http, 1,
	cowboy_tcp_transport, [{port, 8080}],
	cowboy_http_protocol, [{dispatch, Dispatch}]
)
```
  
Launch you application.

If you don't understand, where to put these lines, than go to example-app subdirectory and run:

```
make
run ~/Sites/my_rails_app
```



Go to http://localhost:8080/ and you will see your started Rails app.

Handler should reload application if config.ru mtime is changed, so it should be compatible with capistrano deploy.