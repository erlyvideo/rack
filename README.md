Rack
====


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
	  {['...'], rack_handler, [{path,"../my_rails_app"}]}
	]}
],
cowboy:start_listener(http, 1,
	cowboy_tcp_transport, [{port, 8080}],
	cowboy_http_protocol, [{dispatch, Dispatch}]
)
```
  
Launch you application