Rack
====

This is a library that allows running Ruby on Rails (or ever any Rack application) on top of Erlang HTTP servers.

All cool Erlang features like Comet, WebSockets and session management are accessible to you now!

Some details
-----

Rack for Rrlang is an application that spawns several [rack](http://rack.rubyforge.org/) handlers and shares requests between them. You can think of it as a replacement for Passenger.

Currently it is compatible with [Cowboy HTTP server](https://github.com/extend/cowboy).

Prerequisites
-------------
Erlang/OTP R14 (erlang-base, erlang-eunit and erlang-dev packages for Ubuntu)
GNU Make

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
  
Launch your application.

If you have no idea where to put these lines to then go to example-app subfolder and run:

```
make
./run ~/Sites/my_rails_app
```

Navigate to http://localhost:8080/ and you will see your started Rails app.

Handler should reload application if config.ru mtime is changed, so it should be compatible with capistrano deploy.
