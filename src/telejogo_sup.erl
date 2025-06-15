%% ------------------------------------------------------------------
%% src/telejogo_sup.erl
%% ------------------------------------------------------------------
-module(telejogo_sup).
-author("Matheus de Camargo Marques").
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

%% @doc Starts the supervisor and registers it locally with the module name.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @doc Supervisor callback that defines the supervision strategy and children.
init([]) ->
    % Supervision strategy: if a child dies, only that child is restarted.
    SupFlags = #{strategy => one_for_one, intensity => 1, period => 5},

    % List of child process specifications.
    ChildSpecs = [
        #{
            id => telejogo_game, % Unique ID for the child.
            start => {telejogo_game, start_link, []}, % How to start the child.
            restart => permanent, % Restart whenever it dies (unless a normal stop).
            shutdown => 2000, % Time to wait for the child to terminate (ms).
            type => worker, % Type of child.
            modules => [telejogo_game] % Associated modules.
        }
    ],

    {ok, {SupFlags, ChildSpecs}}.