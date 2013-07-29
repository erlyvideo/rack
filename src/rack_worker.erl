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

-module(rack_worker).
-author('Max Lapshin <max@maxidoors.ru>').
-include("log.hrl").

-export([start_link/0, start_link/1, request/3]).

-export([init/1, handle_call/3, handle_info/2, terminate/2]).

start_link() ->
  start_link([]).

start_link(Options) ->
  gen_server:start_link(?MODULE, [Options], []).

% request(Path, Headers, Body) ->
%   io:format("~n~nWORKER:~nPath = ~p~nHeaders:~n~p~n~nBody = ~p~n~n~n", [Path, Headers, Body]),
%   {error, busy}.

request(Path, Headers, Body) when is_binary(Path) ->
  request(binary_to_list(Path), Headers, Body);

request(Path, Headers, Body) when is_list(Path) ->
  {ok, Pid} = rack:find_worker(Path),
  request(Pid, Headers, Body);

request(Pid, Headers, Body) when is_pid(Pid) ->
  try gen_server:call(Pid, {request, Headers, Body}, 60000) of
    Reply -> Reply
  catch
    error:timeout ->
      gen_server:call(Pid, {cancel_req, self()}),
      {error, timeout};
    _Class:Error ->
      gen_server:call(Pid, {cancel_req, self()}),
      {error, Error}
  end.


-record(state, {
  port,
  timeout,
  timer,
  from,
  options,
  path
}).

init([Options]) ->
  Path = proplists:get_value(path, Options, "./priv"),
  Timeout = proplists:get_value(timeout, Options, 60000),
  {ok, RackOptions} = proplists:get_value(rack_options, Options, []),

  State = #state{
    path = Path,
    timeout = Timeout,
    options = Options
  },

  {ok, start_worker(State, RackOptions)}.

start_worker(#state{path = Path} = State, RackOptions) ->
  WorkerPath = code:lib_dir(rack, priv),
  Cmd = WorkerPath++"/worker.rb "++Path,

  % RackEnv = proplists:get_value(rack_env, RackOptions, "production"), 
  
  % Env = [
  %   {"RACK_ENV", RackEnv}
  % ],
  Port = erlang:open_port({spawn, Cmd}, [
    nouse_stdio,
    binary,
    exit_status,
    {packet,4},
    {env, RackOptions}
  ]),
  io:format("Start Rack worker with '~s', PID(~p)~n~p~n~n", [Cmd, self(), RackOptions]),
  State#state{port = Port}.

handle_call({request, _H, _B} = Request, From, #state{from = undefined} = State) ->
  {noreply, start_request(Request, From, State)};

handle_call({request, _H, _B}, _From, State) ->
  {reply, {error, busy}, State}.


start_request({request, Headers, Body}, From, #state{port = Port, timeout = Timeout, from = undefined} = State) ->
  Packed = iolist_to_binary([<<(length(Headers)):32>>,[
    <<(size(Key)):32, Key/binary, (size(Value)):32, Value/binary>> || {Key, Value} <- Headers
  ], <<(size(Body)):32>>, Body]),
  port_command(Port, Packed),
  {ok, Timer} = timer:send_after(Timeout, kill_request),
  State#state{from = From, timer = Timer}.
  
ask_next_job(State, Manager) ->
  case rack_manager:next_job(Manager) of
    empty -> State;
    {ok, {Request, From}} -> 
      % ?D({worker_pickup,self()}),
      State1 = start_request(Request, From, State),
      State1
  end.
  

handle_info({Port, {data, Bin}}, #state{port = Port, from = From, timer = Timer, path = Path} = State) ->
  {ok, cancel} = timer:cancel(Timer),
  <<Status:32, HeadersCount:32, Rest/binary>> = Bin,
  {Headers, BodyType, RawBody} = extract_headers(Rest, HeadersCount, []),
  Body = case BodyType of
    file -> {ok, B} = file:read_file(binary_to_list(RawBody)), B;
    raw -> RawBody
  end,
  gen_server:reply(From, {ok, {Status, Headers, Body}}),
  State1 = ask_next_job(State#state{from = undefined, timer = undefined}, rack:manager_id(Path)),
  {noreply, State1};

handle_info({has_new_job, Manager}, #state{from = undefined} = State) ->
  {noreply, ask_next_job(State, Manager)};

handle_info({has_new_job, _}, #state{} = State) ->
  {noreply, State};

handle_info(kill_request, #state{from = undefined} = State) ->
  {noreply, State};

handle_info(kill_request, #state{from = From} = State) when From =/= undefined ->
  (catch gen_server:reply(From, {error, timeout})),
  ?D({timeout,self()}),
  {stop, normal, State};


handle_info({Port, {exit_status, _Status}}, #state{port = Port} = State) ->
  {stop, normal, State}.


extract_headers(<<BodyFlag, BodyLen:32, Body:BodyLen/binary>>, 0, Acc) ->
  BodyType = case BodyFlag of
    1 -> file;
    0 -> raw
  end,
  {lists:reverse(Acc), BodyType, Body};

extract_headers(<<KeyLen:32, Key:KeyLen/binary, ValueLen:32, Value:ValueLen/binary, Rest/binary>>, HeadersCount, Acc) ->
  extract_headers(Rest, HeadersCount - 1, [{Key, Value}|Acc]).

terminate(_, _) -> ok.
