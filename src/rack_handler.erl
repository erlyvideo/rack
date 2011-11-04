-module(rack_handler).
-author('Max Lapshin <max@maxidoors.ru>').

-export([init/3, handle/2, terminate/2]).

-record(state, {
  path
}).

init({_Transport, http}, Req, Options) ->
  Path = proplists:get_value(path, Options, "./priv"),
  {ok, Req, #state{path = Path}}.


join(Tokens, <<"/">>) ->
  iolist_to_binary([[<<"/">>, PT] || PT <- Tokens, PT =/= <<"..">> andalso PT =/= <<"">>]);

join(Tokens, Sep) ->
  iolist_to_binary(string:join([binary_to_list(PT) || PT <- Tokens, PT =/= <<"..">> andalso PT =/= <<"">>], Sep)).

translate_headers(Headers) ->
  lists:foldl(fun
  ({'Host', _}, Acc) ->
    Acc;
  ({K,V}, Acc) when is_binary(K) ->
    Name = "HTTP_" ++ re:replace(string:to_upper(binary_to_list(K)), "\\-", "_"),
    [{list_to_binary(Name), V}|Acc];
  ({K,V}, Acc) when is_atom(K) ->
    Name = "HTTP_" ++ re:replace(string:to_upper(atom_to_list(K)), "\\-", "_"),
    [{list_to_binary(Name), V}|Acc]
  end, [], Headers).

handle(Req, #state{path = Path} = State) ->
  {RequestMethod, Req1} = cowboy_http_req:method(Req),
  {ScriptName, Req2} = cowboy_http_req:path(Req1),
  {PathInfo, Req3} = cowboy_http_req:path_info(Req2),
  {QueryString, Req4} = cowboy_http_req:raw_qs(Req3),
  {ServerName, Req5} = cowboy_http_req:host(Req4),
  {ServerPort, Req6} = cowboy_http_req:port(Req5),
  {RequestHeaders, _} = cowboy_http_req:headers(Req6),
  {ok, Body, Req7} = case RequestMethod of
    'POST' -> cowboy_http_req:body(Req6);
    _ -> {ok, <<"">>, Req6}
  end,
  
  RackSession = [
    {<<"REQUEST_METHOD">>, atom_to_binary(RequestMethod, latin1)},
    {<<"SCRIPT_NAME">>, join(lists:sublist(ScriptName, length(ScriptName) - length(PathInfo)), <<"/">>)},
    {<<"PATH_INFO">>, join(PathInfo, <<"/">>)},
    {<<"QUERY_STRING">>, QueryString},
    {<<"SERVER_NAME">>, join(ServerName, ".")},
    {<<"SERVER_PORT">>, list_to_binary(integer_to_list(ServerPort))},
    {<<"HTTP_HOST">>, <<(join(ServerName, "."))/binary, ":", (list_to_binary(integer_to_list(ServerPort)))/binary>>}
  ] ++ translate_headers(RequestHeaders),
  
  {ok, {Status, ReplyHeaders, ReplyBody}} = rack_worker:request(Path, RackSession, Body),  
  
  {ok, Req8} = cowboy_http_req:reply(Status, ReplyHeaders, ReplyBody, Req7),
  {ok, Req8, State}.


terminate(_,_) ->
  ok.
