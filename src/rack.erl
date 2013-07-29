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
-module(rack).

-behaviour(application).
-include("log.hrl").

%% Application callbacks
-export([start/2, stop/1]).

-export([request/3, start_rack/1, start_rack/2]).
-export([sup_id/1, manager_id/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    rack_sup:start_link().

stop(_State) ->
    ok.

request(Path, Headers, Body) ->
  start_rack(Path),
  rack_worker:request(erlang:whereis(manager_id(Path)), Headers, Body).

  

start_rack(Path) ->
  case erlang:whereis(rack_sup) of
    undefined -> application:start(rack);
    _ -> ok
  end,
  case erlang:whereis(sup_id(Path)) of
    undefined ->
      start_rack(Path, [{workers, 4}]);
    Pid ->
      {ok, Pid}
  end.  

sup_id(Path) ->
  list_to_atom(lists:flatten(io_lib:format("rack_worker_~s_sup", [Path]))).

manager_id(Path) ->
  list_to_atom(lists:flatten(io_lib:format("rack_manager_~s", [Path]))).

worker_id(Path, N) ->
  list_to_atom(lists:flatten(io_lib:format("rack_worker_~s_~p", [Path, N]))).

start_rack(Path, Options) when is_binary(Path) ->
  start_rack(binary_to_list(Path), Options);

start_rack(Path, Options) when is_list(Path) ->
  SupId = sup_id(Path),
  Workers = proplists:get_value(workers, Options, 1),

  RackOptions = application:get_env(rack, rack_options), %[{rack_env, "development"}],

  supervisor:start_child(rack_sup, {
    SupId,
    {supervisor, start_link, [{local, SupId}, rack_sup, [rack_worker_pool_sup]]},
    permanent,
    infinity,
    supervisor,
    []
  }),
  lists:foreach(fun(N) ->
    SubId = worker_id(Path, N),
    supervisor:start_child(SupId, {
      SubId,
      {rack_worker, start_link, [[{path, Path}, {rack_options, RackOptions}]]},
      permanent,
      1000,
      worker,
      [rack_worker]
    })
  end, lists:seq(1,Workers)),
  Res = supervisor:start_child(SupId, {
    manager_id(Path),
    {rack_manager, start_link, [[{process_id, manager_id(Path)},{path,Path}]]},
    permanent,
    1000,
    worker,
    [rack_manager]
  }),
  io:format("START MANAGER:~n~p~n~n", [Res]).






