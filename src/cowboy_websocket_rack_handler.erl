-module(cowboy_websocket_rack_handler).
-author('Konstantin Kiselyov <kiseljovkn@gmail.com>').
-behaviour(cowboy_websocket_handler).

-export([init/3]).
-export([websocket_init/3]).
-export([websocket_handle/3]).
-export([websocket_info/3]).
-export([websocket_terminate/3]).

-record(state, {
  	path,
  	handler
}).

%%% Cowboy websocket behaviour methods

init({tcp, http}, _Req, _Options) ->
  	{upgrade, protocol, cowboy_websocket}.
 
websocket_init(_TransportName, Req, Options) ->
	Path = proplists:get_value(path, Options, "./priv"),
	Handler = proplists:get_value(handler, Options),
    {ok, Req, #state{path = Path, handler = Handler}}.
 
websocket_handle({text, Msg}, Req, #state{path = ServerPath, handler = Handler} = State) ->
	ProtocolMessage = try Handler:parse_message(Msg)
	catch 
		_ -> message_parsing_error
	end, 

	Response = case ProtocolMessage of
		message_parsing_error ->
			<<"{\"ws_error\": \"Protocol message parsing error\"}">>;
		_ ->
			{RequestMethod, Path, QueryString, ServerName, ServerPort} = Handler:request_info_for_message(ProtocolMessage),
			{ok, {Status, Headers, Body}} = perform_rack_request(RequestMethod, Path, QueryString, ServerName, ServerPort, ServerPath),
			Handler:encode_rack_response(Status, Headers, Body)
	end,
	WSResponse = Handler:encode_transport_response(Response, ProtocolMessage),
    {reply, {text, WSResponse}, Req, State};

websocket_handle(_Data, Req, State) ->
    {ok, Req, State}.
 
websocket_info({timeout, _Ref, Msg}, Req, State) ->
	{reply, {text, Msg}, Req, State};

websocket_info(_Info, Req, State) ->
	{ok, Req, State}.

websocket_terminate(_Reason, _Req, _State) ->
    ok.

%%% RACK helpers

standard_http_headers(HttpHost) ->
  [
    {<<"HTTP_CONNECTION">>, <<"keep-alive">>},
    {<<"HTTP_ACCEPT_ENCODING">>, <<"gzip, deflate">>},
    {<<"HTTP_ACCEPT_LANGUAGE">>, <<"en-us">>},
    {<<"HTTP_CACHE_CONTROL">>, <<"max-age=0">>},
    {<<"HTTP_ACCEPT">>, <<"text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8">>},
    {<<"HTTP_USER_AGENT">>, <<"Mozilla/5.0 (Macintosh; Intel Mac OS X 10_8_3) AppleWebKit/536.29.13 (KHTML, like Gecko) Version/6.0.4 Safari/536.29.13">>},
    {<<"HTTP_HOST">>, HttpHost}  
  ].

perform_rack_request(RequestMethod, RequestPath, QueryString, ServerName, ServerPort, ServerPath) ->
  HttpHost = <<ServerName/binary, ":", (list_to_binary(integer_to_list(ServerPort)))/binary>>,
  RackSession = [
    {<<"REQUEST_METHOD">>, RequestMethod},
    {<<"SCRIPT_NAME">>, <<"">>}, 
    {<<"PATH_INFO">>, RequestPath},
    {<<"QUERY_STRING">>, QueryString},
    {<<"SERVER_NAME">>, ServerName},
    {<<"SERVER_PORT">>, list_to_binary(integer_to_list(ServerPort))},
    {<<"HTTP_HOST">>, HttpHost}
  ] 
  ++ 
  standard_http_headers(HttpHost),
  
  %%% TODO: For POST we should think how to transfer body with JSON (see #ra_protocol_message.params)
  %%%		May be it should be Base64 body encoding.
  Body = <<"">>,

  case rack:request(ServerPath, RackSession, Body) of
    {ok, {_Status, _ReplyHeaders, _ReplyBody} = Reply} ->
      {ok, Reply};
    {error, busy} ->
      {ok, {503, [], <<"{\"error\":\"Backend overloaded\"}">>}};
    {error, timeout} ->
      {ok, {504, [], <<"{\"error\":\"Backend timeout\"}">>}};
    {error, Error} ->
      {ok, {500, [], iolist_to_binary(io_lib:format("{\"error\":\"Internal server error: ~p\"}\r\n", [Error]))}}
  end.




