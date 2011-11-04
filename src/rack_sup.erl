
-module(rack_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([rack_worker_pool_sup]) ->
  {ok, {{one_for_one, 5, 10}, []}};

init([]) ->
  Supervisors = [
  % {   rack_worker_sup,
  %     {rack_worker,start_link,[]},
  %     permanent,                               % Restart  = permanent | transient | temporary
  %     1000,                                % Shutdown = brutal_kill | int() >= 0 | infinity
  %     worker,                              % Type     = worker | supervisor
  %     [rack_worker]                                       % Modules  = [Module] | dynamic
  % }
  
  ],
  {ok, { {one_for_one, 5, 10}, Supervisors} }.

