-module(echo_event_manager).
-author('Konstantin Kiselyov, <kiseljovkn@gmail.com>').

-export([
	start/0,
	reconnect/0
]).

start() ->
	{ok, _Pid} = gen_event:start_link({global, ?MODULE}).

reconnect() ->
	gen_event:notify({global, ?MODULE}, reconnect).