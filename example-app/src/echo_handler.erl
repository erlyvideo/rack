-module(echo_handler).
-author('Konstantin Kiselyov, <kiseljovkn@gmail.com>').

-export([
	parse_message/1,
	request_info_for_message/1,
	encode_transport_response/2,
	encode_rack_response/3,
	handle_reconnect/1
]).

%%% Application layer protocol

-record(ra_protocol_message, {
	unique = <<"">>,
	ts = 0,
	action = <<"">>,
	method = <<"GET">>,
	query_str = <<"">>,
	params = {struct, []},
	response = <<"{}">>
}).

if_undefined_then_value(X, Value) ->
	case X of 
		undefined -> Value;
		_ -> X
	end.

parse_message(Message) ->
	{struct, PropList} = mochijson2:decode(Message),

	% extract parameters
	Unique 	= if_undefined_then_value(proplists:get_value(<<"unique">>, PropList), <<"">>),
	Ts 		= if_undefined_then_value(proplists:get_value(<<"ts">>, 	PropList), 0),
	Action 	= if_undefined_then_value(proplists:get_value(<<"action">>, PropList), <<"">>),
	Query 	= if_undefined_then_value(proplists:get_value(<<"query_str">>, PropList), <<"">>),
	Method 	= if_undefined_then_value(proplists:get_value(<<"method">>, PropList), <<"GET">>),
	Params 	= if_undefined_then_value(proplists:get_value(<<"params">>, PropList), {struct, []}),

	% create result message
	#ra_protocol_message{
		unique = Unique, 
		ts = Ts,
		action = Action,
		query_str = Query,
		method = Method,
		params = Params
	}.

request_info_for_message(ProtocolMessage) ->
	RequestMethod = ProtocolMessage#ra_protocol_message.method,
	Path = ProtocolMessage#ra_protocol_message.action,
	QueryString = ProtocolMessage#ra_protocol_message.query_str,
	ServerName = <<"localhost">>,
	ServerPort = 8080,
	{RequestMethod, Path, QueryString, ServerName, ServerPort}.

encode_transport_response(Response, Message) ->
	Unique 	= Message#ra_protocol_message.unique,
	Ts 		= list_to_binary(integer_to_list(Message#ra_protocol_message.ts)),
	Action 	= Message#ra_protocol_message.action,
	Query 	= Message#ra_protocol_message.query_str,
	Method 	= Message#ra_protocol_message.method,
	Params 	= mochijson2:encode(Message#ra_protocol_message.params),

	UniquePart 		= 	<<"\"unique\":", 	"\"", Unique/binary, "\"">>,
	TsPart 			= 	<<"\"ts\":", 		Ts/binary>>,
	ActionPart 		= 	<<"\"action\":", 	"\"", Action/binary, "\"">>,
	QueryPart 		= 	<<"\"query_str\":", 	"\"", Query/binary, "\"">>,
	MethodPart 		= 	<<"\"method\":", 	"\"", Method/binary, "\"">>,
	ParamsPart 		= 	<<"\"params\":", 	Params/binary>>,
	ResponsePart	= 	<<"\"response\":", 	Response/binary>>,

	_Result = <<"{", 
		UniquePart/binary, ",",
		TsPart/binary, ",",
		ActionPart/binary, ",",
		QueryPart/binary, ",",
		MethodPart/binary, ",",
		ParamsPart/binary, ",",
		ResponsePart/binary,
	"}">>.
	
encode_rack_response(Status, ResponseHeaders, ResponseBody) ->
	StatusValue = list_to_binary(integer_to_list(Status)),
	_ResponseHeadersValue = ResponseHeaders,
	ResponseBodyValue = ResponseBody,
	<<"{",
		"\"status\":",  StatusValue/binary, ",",
		% "\"headers\":\"",  ResponseHeadersValue/binary, "\",",
		"\"body\":",  ResponseBodyValue/binary,
	"}">>.

handle_reconnect(_State) ->
	<<"{\"action\": \"reconnect\"}">>.
