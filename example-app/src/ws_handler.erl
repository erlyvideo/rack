-module(ws_handler).
-author('Konstantin Kiselyov <kiseljovkn@gmail.com>').
% -behaviour(cowboy_websocket_handler).

% -export([init/3]).
% -export([websocket_init/3]).
% -export([websocket_handle/3]).
% -export([websocket_info/3]).
% -export([websocket_terminate/3]).

% -record(state, {
%   	path
% }).

% %%% Application layer protocol

% -record(ra_protocol_message, {
% 	unique = <<"">>,
% 	ts = 0,
% 	action = <<"">>,
% 	method = <<"GET">>,
% 	query_str = <<"">>,
% 	params = {struct, []},
% 	response = <<"{}">>
% }).


% if_undefined_then_value(X, Value) ->
% 	case X of 
% 		undefined -> Value;
% 		_ -> X
% 	end.

% %%% Cowboy websocket behaviour methods

% init({tcp, http}, _Req, Options) ->
%   	{upgrade, protocol, cowboy_websocket}.
 
% websocket_init(_TransportName, Req, Options) ->
% 	Path = proplists:get_value(path, Options, "./priv"),
%     {ok, Req, #state{path = Path}}.
 
% websocket_handle({text, Msg}, Req, #state{path = ServerPath} = State) ->
% 	io:format("Message:~n~p~n~n", [Msg]),
% 	ProtocolMessage = try parse_message(Msg)
% 	catch 
% 		_ -> message_parsing_error
% 	end, 

% 	Response = case ProtocolMessage of
% 		message_parsing_error ->
% 			<<"{\"ws_error\": \"message_parsing_error\"}">>;
% 		_ ->
% 			RequestMethod = ProtocolMessage#ra_protocol_message.method,
% 			Path = ProtocolMessage#ra_protocol_message.action,
% 			QueryString = ProtocolMessage#ra_protocol_message.query_str,
% 			ServerName = <<"localhost">>,
% 			ServerPort = 8080,

% 			{ok, {Status, Headers, Body}} = perform_rack_request(RequestMethod, Path, QueryString, ServerName, ServerPort, ServerPath),
% 			RailsResponse = encode_rails_response(Status, Headers, Body),
% 			io:format("Rails Response:~n~p~n~n", [RailsResponse]),
% 			RailsResponse
% 			%%% TODO: request to RACK
% 	end,
% 	WSResponse = encode_response(Response, ProtocolMessage),
% 	io:format("Response:~n~p~n", [WSResponse]),
%     {reply, {text, WSResponse}, Req, State};

% websocket_handle(_Data, Req, State) ->
%     {ok, Req, State}.
 
% websocket_info({timeout, _Ref, Msg}, Req, State) ->
% 	{reply, {text, Msg}, Req, State};

% websocket_info(_Info, Req, State) ->
% 	{ok, Req, State}.

% websocket_terminate(_Reason, _Req, _State) ->
%     ok.

% %%% Helper methods

% parse_message(Message) ->
% 	{struct, PropList} = mochijson2:decode(Message),

% 	% extract parameters
% 	Unique 	= if_undefined_then_value(proplists:get_value(<<"unique">>, PropList), <<"">>),
% 	Ts 		= if_undefined_then_value(proplists:get_value(<<"ts">>, 	PropList), 0),
% 	Action 	= if_undefined_then_value(proplists:get_value(<<"action">>, PropList), <<"">>),
% 	Query 	= if_undefined_then_value(proplists:get_value(<<"query_str">>, PropList), <<"">>),
% 	Method 	= if_undefined_then_value(proplists:get_value(<<"method">>, PropList), <<"GET">>),
% 	Params 	= if_undefined_then_value(proplists:get_value(<<"params">>, PropList), {struct, []}),

% 	% create result message
% 	#ra_protocol_message{
% 		unique = Unique, 
% 		ts = Ts,
% 		action = Action,
% 		query_str = Query,
% 		method = Method,
% 		params = Params
% 	}.

% encode_response(Response, Message) ->
% 	Unique 	= Message#ra_protocol_message.unique,
% 	Ts 		= list_to_binary(integer_to_list(Message#ra_protocol_message.ts)),
% 	Action 	= Message#ra_protocol_message.action,
% 	Query 	= Message#ra_protocol_message.query_str,
% 	Method 	= Message#ra_protocol_message.method,
% 	Params 	= mochijson2:encode(Message#ra_protocol_message.params),

% 	UniquePart 		= 	<<"\"unique\":", 	"\"", Unique/binary, "\"">>,
% 	TsPart 			= 	<<"\"ts\":", 		Ts/binary>>,
% 	ActionPart 		= 	<<"\"action\":", 	"\"", Action/binary, "\"">>,
% 	QueryPart 		= 	<<"\"query_str\":", 	"\"", Query/binary, "\"">>,
% 	MethodPart 		= 	<<"\"method\":", 	"\"", Method/binary, "\"">>,
% 	ParamsPart 		= 	<<"\"params\":", 	Params/binary>>,
% 	ResponsePart	= 	<<"\"response\":", 	Response/binary>>,

% 	_Result = <<"{", 
% 		UniquePart/binary, ",",
% 		TsPart/binary, ",",
% 		ActionPart/binary, ",",
% 		QueryPart/binary, ",",
% 		MethodPart/binary, ",",
% 		ParamsPart/binary, ",",
% 		ResponsePart/binary,
% 	"}">>.
	
% encode_rails_response(Status, ResponseHeaders, ResponseBody) ->
% 	StatusValue = list_to_binary(integer_to_list(Status)),
% 	ResponseHeadersValue = ResponseHeaders,
% 	ResponseBodyValue = ResponseBody,
% 	<<"{",
% 		"\"status\":",  StatusValue/binary, ",",
% 		% "\"headers\":\"",  ResponseHeadersValue/binary, "\",",
% 		"\"body\":",  ResponseBodyValue/binary,
% 	"}">>.

% %%% RACK helpers

% standard_http_headers(HttpHost) ->
%   [
%     {<<"HTTP_CONNECTION">>, <<"keep-alive">>},
%     {<<"HTTP_ACCEPT_ENCODING">>, <<"gzip, deflate">>},
%     {<<"HTTP_ACCEPT_LANGUAGE">>, <<"en-us">>},
%     {<<"HTTP_CACHE_CONTROL">>, <<"max-age=0">>},
%     {<<"HTTP_ACCEPT">>, <<"text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8">>},
%     {<<"HTTP_USER_AGENT">>, <<"Mozilla/5.0 (Macintosh; Intel Mac OS X 10_8_3) AppleWebKit/536.29.13 (KHTML, like Gecko) Version/6.0.4 Safari/536.29.13">>},
%     {<<"HTTP_HOST">>, HttpHost}  
%   ].

% perform_rack_request(RequestMethod, RequestPath, QueryString, ServerName, ServerPort, ServerPath) ->
%   HttpHost = <<ServerName/binary, ":", (list_to_binary(integer_to_list(ServerPort)))/binary>>,
%   RackSession = [
%     {<<"REQUEST_METHOD">>, RequestMethod},%atom_to_binary(RequestMethod, latin1)},
%     {<<"SCRIPT_NAME">>, <<"">>}, %join(lists:sublist(ScriptName, length(ScriptName) - length(PathInfo)), <<"/">>)},
%     {<<"PATH_INFO">>, RequestPath}, %join(PathInfo, <<"/">>)},
%     {<<"QUERY_STRING">>, QueryString},
%     {<<"SERVER_NAME">>, ServerName}, %join(ServerName, ".")},
%     {<<"SERVER_PORT">>, list_to_binary(integer_to_list(ServerPort))},
%     {<<"HTTP_HOST">>, HttpHost}%<<(join(ServerName, "."))/binary, ":", (list_to_binary(integer_to_list(ServerPort)))/binary>>}
%   ] 
%   ++ 
%   standard_http_headers(HttpHost),
  
%   io:format("************************~nRACK:~n~p~n************************~n", [RackSession]),

%   %%% TODO: For POST we should think how to transfer body with JSON (see #ra_protocol_message.params)
%   %%%		May be it should be Base64 body encoding.
%   Body = <<"">>,

%   Response = case rack:request(ServerPath, RackSession, Body) of
%     {ok, {_Status, _ReplyHeaders, _ReplyBody} = Reply} ->
%       % ?D({_Status, RequestMethod, join(PathInfo, <<"/">>), iolist_size(_ReplyBody)}),
%       {ok, Reply};
%     {error, busy} ->
%       {ok, {503, [], <<"{\"error\":\"Backend overloaded\"}">>}};
%     {error, timeout} ->
%       {ok, {504, [], <<"{\"error\":\"Backend timeout\"}">>}};
%     {error, Error} ->
%       {ok, {500, [], iolist_to_binary(io_lib:format("{\"error\":\"Internal server error: ~p\"}\r\n", [Error]))}}
%   end.

% % translate_headers(Headers) ->
% %   lists:foldl(fun
% %   ({'Host', _}, Acc) ->
% %     Acc;
% %   ({K,V}, Acc) when is_binary(K) ->
% %     Name = "HTTP_" ++ re:replace(string:to_upper(binary_to_list(K)), "\\-", "_"),
% %     [{list_to_binary(Name), V}|Acc];
% %   ({K,V}, Acc) when is_atom(K) ->
% %     Name = "HTTP_" ++ re:replace(string:to_upper(atom_to_list(K)), "\\-", "_"),
% %     [{list_to_binary(Name), V}|Acc]
% %   end, [], Headers).

% % handle(Req, Env, Path) ->
% %   case file:read_file_info(filename:join(Path, "config.ru")) of
% %     {ok, _} ->
% %       % ?D({rack_call,Env}),
% %       {ok, {Status, ReplyHeaders, ReplyBody}, Req1} = handle(Req, Path),
% %       case proplists:get_value(<<"X-Accel-Redirect">>, ReplyHeaders) of
% %         undefined ->
% %           {ok, Req2} = cowboy_req:reply(Status, ReplyHeaders, ReplyBody, Req1),
% %           {ok, Req2};
% %         Redirect ->
% %           % ?D({rack_redirect,Redirect}),
% %           {unhandled, Req, lists:keystore(path, 1, Env, {path, Redirect})}
% %       end;
% %     {error, _} ->
% %       unhandled
% %   end.





