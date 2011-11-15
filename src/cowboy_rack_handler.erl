%% Copyright (c) 2011, Max Lapshin <max@maxidoors.ru>
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

%% @doc Cowboy rack handler that intercepts requests.
-module(cowboy_rack_handler).
-author('Max Lapshin <max@maxidoors.ru>').

-export([init/3, handle/2, terminate/2]).
-export([handle/3]).
-include("log.hrl").

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

handle(Req, Env, Path) ->
  case file:read_file_info(filename:join(Path, "config.ru")) of
    {ok, _} ->
      % ?D({rack_call,Env}),
      {ok, {Status, ReplyHeaders, ReplyBody}, Req1} = handle(Req, Path),
      case proplists:get_value(<<"X-Accel-Redirect">>, ReplyHeaders) of
        undefined ->
          {ok, Req2} = cowboy_http_req:reply(Status, ReplyHeaders, ReplyBody, Req1),
          {ok, Req2};
        Redirect ->
          % ?D({rack_redirect,Redirect}),
          {unhandled, Req, lists:keystore(path, 1, Env, {path, Redirect})}
      end;
    {error, _} ->
      unhandled
  end.


handle(Req, #state{path = Path} = State) ->
  {ok, {Status, ReplyHeaders, ReplyBody}, Req1} = handle(Req, Path),
  {ok, Req2} = cowboy_http_req:reply(Status, ReplyHeaders, ReplyBody, Req1),
  {ok, Req2, State};

handle(Req, Path) when is_list(Path) ->
  handle(Req, list_to_binary(Path));

  
handle(Req, Path) when is_binary(Path) ->  
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
  
  % Trying to follow http://rack.rubyforge.org/doc/SPEC.html here
  
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
  {ok, {Status, ReplyHeaders, ReplyBody}, Req7}.
  

terminate(_,_) ->
  ok.
