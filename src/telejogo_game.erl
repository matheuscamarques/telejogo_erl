%% ------------------------------------------------------------------
%% src/telejogo_game.erl
%% ------------------------------------------------------------------
-module(telejogo_game).
-author("Matheus de Camargo Marques").
-behaviour(gen_server).

%% --- Public API & Callbacks ---
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% --- Game Constants ---
-define(WIDTH, 80).          % Screen width in columns
-define(HEIGHT, 24).         % Screen height in rows
-define(PADDLE_HEIGHT, 4).   % Height of the paddles
-define(TICK_INTERVAL, 100). % Game loop interval in milliseconds

%% @doc Defines the data structure for the game state.
-record(state, {
    ball_pos = {round(?WIDTH / 2), round(?HEIGHT / 2)}, % Ball's {X, Y} position
    ball_vel = {1, 1},                                   % Ball's {VX, VY} velocity
    p1_pos = round((?HEIGHT - ?PADDLE_HEIGHT) / 2),      % Player 1's paddle Y position
    p2_pos = round((?HEIGHT - ?PADDLE_HEIGHT) / 2),      % Player 2's paddle Y position
    score = {0, 0}                                       % {P1, P2} score
}).

%% ===================================================================
%% Public API
%% ===================================================================

%% @doc Starts the game gen_server and registers it locally with the module name.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% ===================================================================
%% GenServer Callbacks
%% ===================================================================

%% @doc Initializes the server's state.
init([]) ->
    % Seed the random number generator to ensure different serves.
    rand:seed(exsplus, os:timestamp()),
    % Schedule the first 'tick' message to start the game loop.
    erlang:send_after(?TICK_INTERVAL, self(), tick),
    {ok, #state{}}.

%% @doc Handles synchronous calls (not used in this game).
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%% @doc Handles asynchronous calls.
handle_cast({input, Move}, State) ->
    % Receives a player movement, updates the state, and continues.
    NewState = handle_input(Move, State),
    {noreply, NewState};
handle_cast(stop, State) ->
    % Receives the stop command, terminating the gen_server.
    {stop, normal, State}.

%% @doc Handles all other messages (mainly the 'tick').
handle_info(tick, State) ->
    % This is the heart of the game loop.
    % 1. Update game logic (move ball, check collisions).
    UpdatedState = update_game_state(State),
    % 2. Redraw the terminal screen.
    draw_state(UpdatedState),
    % 3. Schedule the next 'tick'.
    erlang:send_after(?TICK_INTERVAL, self(), tick),
    {noreply, UpdatedState};
handle_info(_Info, State) ->
    % Ignore any other messages.
    {noreply, State}.

%% @doc Executes cleanup code before the process terminates.
terminate(_Reason, _State) ->
    % Restore cursor visibility and clear the screen.
    io:put_chars("\e[?25h\e[2J\e[H"),
    ok.

%% @doc For hot code upgrades (not actively used).
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% ===================================================================
%% Game Logic
%% ===================================================================

%% @doc Updates the paddle position based on player input.
%% Uses guard clauses to prevent paddles from leaving the play area.
handle_input(p1_up, State = #state{p1_pos = Pos}) when Pos > 1 ->
    State#state{p1_pos = Pos - 1};
handle_input(p1_down, State = #state{p1_pos = Pos}) when Pos < ?HEIGHT - ?PADDLE_HEIGHT ->
    State#state{p1_pos = Pos + 1};
handle_input(p2_up, State = #state{p2_pos = Pos}) when Pos > 1 ->
    State#state{p2_pos = Pos - 1};
handle_input(p2_down, State = #state{p2_pos = Pos}) when Pos < ?HEIGHT - ?PADDLE_HEIGHT ->
    State#state{p2_pos = Pos + 1};
handle_input(_, State) -> State. % Ignore invalid moves.

%% @doc Orchestrates a single game frame update.
update_game_state(State) ->
    MovedState = move_ball(State),
    check_collisions(MovedState).

%% @doc Updates the ball's position based on its velocity.
move_ball(State = #state{ball_pos = {X, Y}, ball_vel = {VX, VY}}) ->
    State#state{ball_pos = {X + VX, Y + VY}}.

%% @doc Checks and handles all collisions.
check_collisions(State = #state{ball_pos = {_X, Y}, ball_vel = {VX, VY}, score = {S1, S2}}) ->
    % 1. Collision with top and bottom walls.
    NewVelY = if
        (Y =< 2 andalso VY < 0) orelse (Y >= ?HEIGHT - 1 andalso VY > 0) -> -VY;
        true -> VY
    end,

    % 2. Collision with paddles.
    {NewVelX, TempState} = check_paddle_collision(State#state{ball_vel = {VX, NewVelY}}),

    % 3. Check for scoring (ball went past a side wall).
    case TempState#state.ball_pos of
        % Point for Player 2
        {NewX, _} when NewX < 1 ->
            reset_ball(TempState#state{score = {S1, S2 + 1}, ball_vel = {-1, random_y_vel()}});
        % Point for Player 1
        {NewX, _} when NewX > ?WIDTH ->
            reset_ball(TempState#state{score = {S1 + 1, S2}, ball_vel = {1, random_y_vel()}});
        % No score, just update the ball's velocity.
        _ ->
            TempState#state{ball_vel = {NewVelX, NewVelY}}
    end.

%% @doc Checks for a collision between the ball and one of the paddles.
check_paddle_collision(State = #state{ball_pos={X, Y}, ball_vel={VX, _}, p1_pos=P1Y, p2_pos=P2Y}) ->
    IsOnP1 = (Y >= P1Y andalso Y =< P1Y + ?PADDLE_HEIGHT),
    IsOnP2 = (Y >= P2Y andalso Y =< P2Y + ?PADDLE_HEIGHT),

    Paddle1Hit = (X =< 3 andalso VX < 0 andalso IsOnP1),
    Paddle2Hit = (X >= ?WIDTH - 1 andalso VX > 0 andalso IsOnP2),

    if
        Paddle1Hit orelse Paddle2Hit ->
            {-VX, State}; % Reverse horizontal velocity on hit
        true ->
            {VX, State}  % Maintain velocity if no hit
    end.

%% @doc Resets the ball to the center of the screen after a point is scored.
reset_ball(State) ->
    State#state{ball_pos = {round(?WIDTH / 2), round(?HEIGHT / 2)}}.

%% @doc Generates a random vertical velocity (-1, 0, or 1) for the serve.
random_y_vel() ->
    case rand:uniform(3) of
        1 -> -1;
        2 -> 0;
        3 -> 1
    end.

%% ===================================================================
%% Drawing Logic
%% ===================================================================

%% @doc Main drawing function. Orchestrates the rendering of a single frame.
draw_state(#state{score = {S1, S2}} = State) ->
    % Use ANSI escape sequences to clear the screen and hide the cursor.
    io:put_chars("\e[2J\e[?25l"),

    % Draw the score at the top of the screen.
    ScoreStr = io_lib:format("Player 1: ~2.10.0w   |   Player 2: ~2.10.0w", [S1, S2]),
    io:format("\e[1;1H~ts", [center_text(ScoreStr)]),

    % Build the screen in a buffer before printing to prevent flickering.
    Screen = build_screen_buffer(State),
    % Print the buffer line by line.
    lists:foldl(
      fun(Line, LineNum) ->
        io:format("\e[~p;1H~ts", [LineNum + 1, Line]),
        LineNum + 1
      end,
      1,
      Screen).

%% @doc Builds a representation of the screen (a list of strings).
build_screen_buffer(#state{ball_pos = {BX, BY}, p1_pos = P1Y, p2_pos = P2Y}) ->
    Grid = [lists:duplicate(?WIDTH, $ ) || _ <- lists:seq(1, ?HEIGHT - 2)],
    Border = lists:duplicate(?WIDTH, $-),
    Screen = [Border] ++ Grid ++ [Border],

    WithP1 = draw_paddle(Screen, 2, P1Y),
    WithPaddles = draw_paddle(WithP1, ?WIDTH - 1, P2Y),
    draw_ball(WithPaddles, round(BX), round(BY)).

%% @doc Draws a paddle onto the screen buffer.
draw_paddle(Grid, X, Y) ->
    lists:foldl(
        fun(I, Acc) -> set_char(Acc, X, Y + I, $┃) end,
        Grid,
        lists:seq(0, ?PADDLE_HEIGHT - 1)
    ).

%% @doc Draws the ball onto the screen buffer.
draw_ball(Grid, X, Y) ->
    set_char(Grid, X, Y, $o).

%% @doc Utility function to change a character at a given [X, Y] coordinate in the buffer.
set_char(Grid, X, Y, Char) ->
    LineNum = Y - 1,
    if Y >= 1 andalso Y =< length(Grid) andalso X >= 1 andalso X =< ?WIDTH ->
        Line = lists:nth(Y, Grid),
        NewLine = lists:sublist(Line, X - 1) ++ [Char] ++ lists:nthtail(X, Line),
        lists:sublist(Grid, LineNum) ++ [NewLine] ++ lists:nthtail(Y, Grid);
    true ->
        Grid % Return the original Grid if coordinates are out of bounds
    end.

%% @doc Utility function to center a text string.
center_text(Text) ->
    Padding = max(0, round((?WIDTH - length(Text)) / 2)),
    lists:duplicate(Padding, $ ) ++ Text.