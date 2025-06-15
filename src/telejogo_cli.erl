%% ------------------------------------------------------------------
%% src/telejogo_cli.erl
%% ------------------------------------------------------------------
-module(telejogo_cli).
-author("Matheus de Camargo Marques").
-export([main/1]).
-import(gen_server, [cast/2]).

%% @doc Command-Line Interface (CLI) entry point.
%% Manages the application lifecycle from the user's perspective
%% and captures keyboard input.
%% @end
main(_) ->
    io:format("Starting Telejogo...~n"),

    % Ensures the 'telejogo' application and its dependencies (kernel, stdlib)
    % are started. This will trigger `telejogo_app:start/2`, which in turn
    % starts the supervision tree.
    {ok, _} = application:ensure_all_started(telejogo),

    % Puts the Erlang shell in "raw" mode, which allows us to capture
    % single keystrokes (like 'w', 's') and escape sequences (like arrow keys)
    % without the user needing to press Enter.
    ok = shell:start_interactive({noshell, raw}),

    % The input loop is wrapped in a `try...after` block to ensure
    % the application is stopped cleanly when the loop ends
    % (either by pressing 'q' or due to an error).
    try
        input_loop(telejogo_game)
    after
        application:stop(telejogo)
    end.

%% @doc Recursive loop that waits for keyboard input.
%% @param GamePid The PID or registered name of the game process (`telejogo_game`).
%% @end
input_loop(GamePid) ->
    % `io:get_chars/2` waits for characters from the terminal.
    % We use 10 as a buffer size, as arrow key escape sequences
    % usually have 3 or 4 bytes (e.g., `\e[A`).
    case io:get_chars("", 10) of
        % Use pattern matching to identify the pressed key.
        % If the user presses 'q', send the `stop` message to the game.
        "q" ++ _ -> cast(GamePid, stop);

        % For movement keys, send a `{input, Action}` tuple
        % to the game process and call `input_loop` again
        % to wait for the next key.
        "w" ++ _ -> cast(GamePid, {input, p1_up}), input_loop(GamePid);
        "s" ++ _ -> cast(GamePid, {input, p1_down}), input_loop(GamePid);
        "o" ++ _ -> cast(GamePid, {input, p2_up}), input_loop(GamePid);
        "l" ++ _ -> cast(GamePid, {input, p2_down}), input_loop(GamePid);

        % Handle arrow keys.
        "\e[A" ++ _ -> cast(GamePid, {input, p2_up}), input_loop(GamePid);   % Up Arrow
        "\e[B" ++ _ -> cast(GamePid, {input, p2_down}), input_loop(GamePid); % Down Arrow

        % Any other input is ignored, and the loop continues.
        _ -> input_loop(GamePid)
    end.