%% ------------------------------------------------------------------
%% src/telejogo.erl
%% ------------------------------------------------------------------
-module(telejogo).
-author("Matheus de Camargo Marques").
-export([main/1]).

%% @doc Main entry point for the executable (escript).
%% Its sole responsibility is to start the command-line interface.
%% @end
main(Args) ->
    telejogo_cli:main(Args).