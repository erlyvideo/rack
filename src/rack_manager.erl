-module(rack_manager).
-author('Max Lapshin <max@maxidoors.ru>').
-include("log.hrl").

-export([start_link/1, next_job/1]).
-export([init/1, handle_call/3, handle_info/2, terminate/2]).

start_link(Options) ->
  Name = proplists:get_value(process_id, Options),
  gen_server:start_link({local, Name}, ?MODULE, [Options], []).

-record(queue, {
  path,
  free,
  max_len,
  requests,
  working
}).

next_job(Pid) ->
  gen_server:call(Pid, next_job).

init([Options]) ->
  Path = proplists:get_value(path, Options),
  {ok, #queue{
    path = Path,
    requests = queue:new(),
    max_len = proplists:get_value(backlog, Options, 200),
    working = []
  }}.

pids(Path) ->
  [Pid || {_, Pid, _, _} <- supervisor:which_children(rack:sup_id(Path)), Pid =/= self()].
  
remove_pid(Pid, #queue{requests = Requests} = State) ->
  Req1 = queue:filter(fun
    ({_R, {Client,_}}) when Client == Pid -> true;
    (_) -> false
  end, Requests),
  case queue:len(Requests) of
    0 -> ok;
    _ -> ?D({remove_job,queue:len(Requests),queue:len(Req1)})
  end,
  State#queue{requests = Req1}.
  

handle_call({request, _H, _B} = Request, From, #queue{requests = Requests, path = Path, max_len = MaxLen} = State) ->
  case queue:len(Requests) of
    Len when Len >= MaxLen ->
      {reply, {error, busy}, State};
    _ ->  
      Pids = pids(Path),
      [Pid ! {has_new_job, self()} || Pid <- Pids],
      {Client, _} = From,
      erlang:monitor(process, Client),
      case queue:len(Requests) of
        Len when Len > 20 -> ?D({queue, queue:len(Requests) + 1});
        _ -> ok
      end,
      {noreply, State#queue{requests = queue:in({Request,From}, Requests)}}
  end;

handle_call({cancel_req, Pid}, _From, #queue{} = State) ->
  {reply, ok, remove_pid(Pid, State)};


handle_call(next_job, _From, #queue{requests = Requests} = State) ->
  case queue:out(Requests) of
    {{value, From}, Requests1} -> 
      {reply, {ok, From}, State#queue{requests = Requests1}};
    {empty, Requests} ->
      {reply, empty, State}
  end.

handle_info({'DOWN', _, _, Pid, _}, #queue{} = State) ->
  {noreply, remove_pid(Pid, State)};

handle_info(Msg, State) ->
  {stop, {unknown_info, Msg}, State}.

terminate(_,_) -> ok.
