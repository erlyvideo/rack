-module(rack_worker).
-author('Max Lapshin <max@maxidoors.ru>').

-export([start_link/0, start_link/1, request/3]).

-export([init/1, handle_call/3, handle_info/2, terminate/2]).

start_link() ->
  start_link([]).

start_link(Options) ->
  gen_server:start_link(?MODULE, [Options], []).

request(Path, Headers, Body) when is_binary(Path) ->
  request(binary_to_list(Path), Headers, Body);

request(Path, Headers, Body) when is_list(Path) ->
  {ok, Pid} = rack:find_worker(Path),
  request(Pid, Headers, Body);

request(Pid, Headers, Body) when is_pid(Pid) ->
  gen_server:call(Pid, {request, Headers, Body}, 30000).


-record(state, {
  queue,
  port,
  timeout,
  timer,
  options,
  max_len,
  path,
  reply
}).

init([Options]) ->
  Path = proplists:get_value(path, Options, "./priv"),
  Timeout = proplists:get_value(timeout, Options, 30000),

  State = #state{
    queue = queue:new(),
    path = Path,
    timeout = Timeout,
    max_len = proplists:get_value(max_len, Options, 1000),
    options = Options
  },

  {ok, restart_worker(State)}.

restart_worker(#state{port = OldPort, path = Path} = State) ->
  (catch erlang:port_close(OldPort)),
  WorkerPath = code:lib_dir(rack, priv),
  Port = erlang:open_port({spawn, WorkerPath++"/worker.rb "++Path}, [use_stdio,binary,exit_status,{packet,4}]),
  io:format("Start Rack worker at path ~s~n", [Path]),
  try_start_request(State#state{port = Port}).  
    

handle_call({request, _H, _} = Request, From, #state{queue = Queue, max_len = MaxLen} = State) ->
  case queue:len(Queue) of
    Len when Len >= MaxLen ->
      {reply, {error, busy}, State};
    _ ->
      State1 = try_start_request(State#state{queue = queue:in({Request, From}, Queue)}),
      {noreply, State1}
  end.

try_start_request(#state{reply = undefined, queue = Queue} = State) ->
  case queue:out(Queue) of
    {{value, Element}, Queue1} ->
      start_request(Element, State#state{queue = Queue1});
    {empty, _} ->
      State
  end;

try_start_request(#state{} = State) ->
  State.

start_request({{request, Headers, Body}, From}, #state{port = Port, timeout = Timeout} = State) ->
  Packed = iolist_to_binary([<<(length(Headers)):32>>,[
    <<(size(Key)):32, Key/binary, (size(Value)):32, Value/binary>> || {Key, Value} <- Headers
  ], <<(size(Body)):32>>, Body]),
  port_command(Port, Packed),
  Timer = timer:send_after(Timeout, kill_request),
  State#state{reply = From, timer = Timer}.



handle_info({Port, {data, Bin}}, #state{port = Port, reply = Reply, timer = Timer} = State) ->
  timer:cancel(Timer),
  <<Status:32, HeadersCount:32, Rest/binary>> = Bin,
  {Headers, BodyType, RawBody} = extract_headers(Rest, HeadersCount, []),
  Body = case BodyType of
    file -> {ok, B} = file:read_file(binary_to_list(RawBody)), B;
    raw -> RawBody
  end,
  gen_server:reply(Reply, {ok, {Status, Headers, Body}}),
  {noreply, try_start_request(State#state{reply = undefined, timer = undefined})};

handle_info(kill_request, #state{reply = Reply} = State) ->
  (catch gen_server:reply(Reply, {error, timeout})),
  {noreply, restart_worker(State#state{reply = undefined, timer = undefined})};

handle_info({Port, {exit_status, _Status}}, #state{port = Port} = State) ->
  {noreply, restart_worker(State)}.


extract_headers(<<BodyFlag, BodyLen:32, Body:BodyLen/binary>>, 0, Acc) ->
  BodyType = case BodyFlag of
    1 -> file;
    0 -> raw
  end,
  {lists:reverse(Acc), BodyType, Body};

extract_headers(<<KeyLen:32, Key:KeyLen/binary, ValueLen:32, Value:ValueLen/binary, Rest/binary>>, HeadersCount, Acc) ->
  extract_headers(Rest, HeadersCount - 1, [{Key, Value}|Acc]).


terminate(_, _) -> ok.
