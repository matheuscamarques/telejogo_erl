%% ------------------------------------------------------------------
%% src/telejogo_app.erl
%% ------------------------------------------------------------------
-module(telejogo_app).
-author("Matheus de Camargo Marques").
-behaviour(application).

-export([start/2, stop/1]).

%% @doc OTP application callback. Called when the application starts.
%% Its only task is to start the application's main supervisor.
%% @end
start(_StartType, _StartArgs) ->
    telejogo_sup:start_link().

%% @doc OTP application callback. Called when the application stops.
%% No specific cleanup action is needed here.
%% @end
stop(_State) ->
    ok.
