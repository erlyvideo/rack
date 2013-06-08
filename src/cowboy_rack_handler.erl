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

-export([init/3, handle/2, terminate/3]).
-export([handle/3]).
-include("log.hrl").

-record(state, {
  path
}).

init({_Transport, http}, Req, Options) ->
  Path = proplists:get_value(path, Options, "./priv"),
  {ok, Req, #state{path = Path}}.


% join(Tokens, <<"/">>) ->
%   iolist_to_binary([[<<"/">>, PT] || PT <- Tokens, PT =/= <<"..">> andalso PT =/= <<"">>]);

% join(Tokens, Sep) ->
%   iolist_to_binary(string:join([binary_to_list(PT) || PT <- Tokens, PT =/= <<"..">> andalso PT =/= <<"">>], Sep)).

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
          {ok, Req2} = cowboy_req:reply(Status, ReplyHeaders, ReplyBody, Req1),
          {ok, Req2};
        Redirect ->
          % ?D({rack_redirect,Redirect}),
          {unhandled, Req, lists:keystore(path, 1, Env, {path, Redirect})}
      end;
    {error, _} ->
      unhandled
  end.

% handle(Req, State) ->
%   {Method, Req2} = cowboy_req:method(Req),
%   {Echo, Req3} = cowboy_req:qs_val(<<"echo">>, Req2),
%   {ok, Req4} = echo(Method, Echo, Req3),
%   {ok, Req4, State}.

% echo(<<"GET">>, undefined, Req) ->
%   cowboy_req:reply(400, [], <<"Missing echo parameter.">>, Req);
% echo(<<"GET">>, Echo, Req) ->
%   cowboy_req:reply(200,
%     [{<<"content-encoding">>, <<"utf-8">>}], Echo, Req);
% echo(_, _, Req) ->
%   %% Method not allowed.
%   cowboy_req:reply(405, Req).

handle(Req, #state{path = Path} = State) ->
  % {ok, Req1} = cowboy_req:reply(200, [{<<"content-encoding">>, <<"utf-8">>}], <<"Success">>, Req),
  % {ok, Req1, State}.
  {ok, {Status, ReplyHeaders, ReplyBody}, Req1} = handle(Req, Path),
  {ok, Req2} = cowboy_req:reply(Status, ReplyHeaders, ReplyBody, Req1),
  {ok, Req2, State};

handle(Req, Path) when is_list(Path) ->
  handle(Req, list_to_binary(Path));

  
handle(Req, Path) when is_binary(Path) ->  
  {RequestMethod, Req1} = cowboy_req:method(Req),
  {ScriptName, Req2} = cowboy_req:path(Req1),
  {_PathInfo, Req3} = cowboy_req:path_info(Req2),
  {QueryString, Req4} = cowboy_req:qs(Req3),
  {ServerName, Req5} = cowboy_req:host(Req4),
  {ServerPort, Req6} = cowboy_req:port(Req5),
  {RequestHeaders, _} = cowboy_req:headers(Req6),
  {ok, Body, Req7} = case RequestMethod of
    'POST' -> cowboy_req:body(Req6);
    _ -> {ok, <<"">>, Req6}
  end,
  
  % Trying to follow http://rack.rubyforge.org/doc/SPEC.html here
  
  RackSession = [
    {<<"REQUEST_METHOD">>, RequestMethod},%atom_to_binary(RequestMethod, latin1)},
    {<<"SCRIPT_NAME">>, <<"">>}, %join(lists:sublist(ScriptName, length(ScriptName) - length(PathInfo)), <<"/">>)},
    {<<"PATH_INFO">>, ScriptName}, %join(PathInfo, <<"/">>)},
    {<<"QUERY_STRING">>, QueryString},
    {<<"SERVER_NAME">>, ServerName}, %join(ServerName, ".")},
    {<<"SERVER_PORT">>, list_to_binary(integer_to_list(ServerPort))},
    {<<"HTTP_HOST">>, <<ServerName/binary, ":", (list_to_binary(integer_to_list(ServerPort)))/binary>>}%<<(join(ServerName, "."))/binary, ":", (list_to_binary(integer_to_list(ServerPort)))/binary>>}
  ] ++ translate_headers(RequestHeaders),
  
  io:format("************************~nRACK:~n~p~n************************~n", [RackSession]),

  case rack:request(Path, RackSession, Body) of
    {ok, {_Status, _ReplyHeaders, _ReplyBody} = Reply} ->
      % ?D({_Status, RequestMethod, join(PathInfo, <<"/">>), iolist_size(_ReplyBody)}),
      {ok, Reply, Req7};
    {error, busy} ->
      {ok, {503, [], <<"Backend overloaded\r\n">>}, Req7};
    {error, timeout} ->
      {ok, {504, [], <<"Backend timeout\r\n">>}, Req7};
    {error, Error} ->
      {ok, {500, [], iolist_to_binary(io_lib:format("Internal server error: ~p\r\n", [Error]))}, Req7}
  end.

terminate(_,_,_) ->
  ok.
