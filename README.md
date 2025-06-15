# Telejogo Game Documentation in Erlang
**Author:** Matheus de Camargo Marques
**Version:** 1.0.0
**Description:** This document outlines the architecture and implementation of "Telejogo", a clone of the classic Pong game developed using the Erlang/OTP platform.

---

## 1. Project Overview

Telejogo is a terminal-based application that recreates the classic two-player Pong experience. The project is built on Erlang/OTP principles, making it robust, concurrent, and fault-tolerant.

The architecture is based on a `gen_server` that manages the game state (ball position, paddles, scoreboard) and a supervisor to ensure that the game process restarts automatically in case of unexpected failures. User interaction occurs through a command-line interface (CLI) that captures keypresses and sends them as messages to the game process.

---

## 2. Structure and Architecture

The project is organized into several modules, each with a clear responsibility, following the OTP application pattern.

* **`rebar.config`**: Build configuration file for rebar3, defining dependencies and how to compile the executable (escript).
* **`telejogo.app.src`**: OTP application resource file defining metadata and modules.
* **`telejogo.erl`**: Entry point of the executable, delegates CLI responsibilities.
* **`telejogo_app.erl`**: Application callback module responsible for starting and stopping the supervision tree.
* **`telejogo_sup.erl`**: Main supervisor that starts and monitors the `telejogo_game` process.
* **`telejogo_game.erl`**: Core game logic implemented as a `gen_server`.
* **`telejogo_cli.erl`**: Manages user interaction and keyboard input from the terminal.

### Execution Flow

1. The user runs the `telejogo` script.
2. `telejogo:main/1` is called, which delegates to `telejogo_cli:main/1`.
3. `telejogo_cli` starts the OTP application using `application:ensure_all_started/1`.
4. This triggers `telejogo_app` to start the supervisor `telejogo_sup`.
5. `telejogo_sup` starts its only child, the `telejogo_game` process.
6. `telejogo_cli` enters an input loop (`input_loop/1`) to capture keypresses and sends them via `gen_server:cast/2` to `telejogo_game`.
7. `telejogo_game` updates the state and redraws the screen every 100ms.
8. When the user presses 'q', the application exits.

---

## 3. Module Breakdown

### `rebar.config`

Configuration for rebar3 build tool.

* `escript_name`: Output file name is `telejogo`.
* `escript_main_app`: Sets `telejogo` as the main application.
* `escript_incl_apps`: Includes the application code in the final executable.

### `telejogo.app.src`

Defines OTP application metadata.

* `{mod, {telejogo_app, []}}`: Specifies the callback module.
* `{modules, [...]}`: Lists all modules to be included in the final build.

### `telejogo.erl`

Minimal entry point for the escript.

```erlang
main(Args) ->
    telejogo_cli:main(Args).
```

### `telejogo_app.erl`

Implements the `application` behaviour.

* `start/2`: Starts the main supervisor via `telejogo_sup:start_link/0`.

### `telejogo_sup.erl`

Implements the `supervisor` behaviour.

* `init/1`:

  * `SupFlags`: Strategy `one_for_one`, restarting only `telejogo_game` on failure.
  * `ChildSpecs`: Defines a single child:

    * `id => telejogo_game`
    * `start => {telejogo_game, start_link, []}`
    * `restart => permanent`
    * `type => worker`

### `telejogo_cli.erl`

Handles terminal interaction.

* `main/1`:

  * Starts OTP application.
  * Uses `shell:start_interactive({noshell, raw})` for raw key input.
  * Enters the input loop and ensures graceful shutdown with `application:stop/1`.
* `input_loop/1`:

  * Uses `io:get_chars("", 10)` to non-blockingly read up to 10 bytes (arrow keys included).
  * Pattern matches keys: `w`, `s`, `o`, `l`, arrows, and `q`.
  * Sends async messages to `telejogo_game` with `gen_server:cast/2`.

### `telejogo_game.erl`

Core game logic using `gen_server`.

#### State (record):

* `ball_pos`: `{X, Y}` coordinates of the ball.
* `ball_vel`: `{VX, VY}` velocity.
* `p1_pos`, `p2_pos`: Top Y-coordinate of each player's paddle.
* `score`: `{S1, S2}` tuple with scores.

#### Key Functions:

* `init/1`: Initializes random seed and schedules first tick.
* `handle_cast({input, Move}, State)`: Receives user input, updates paddle positions via `handle_input/2`.
* `handle_info(tick, State)`:

  * Updates game state via `update_game_state/1`.
  * Redraws screen via `draw_state/1`.
  * Schedules next tick.
* `terminate/2`: Cleans up by restoring terminal cursor and clearing screen.

#### Game Logic:

* `handle_input/2`: Moves paddles with boundary checks.
* `update_game_state/1`: Handles ball movement and collision detection.
* `check_collisions/1`:

  * Wall collision: inverts `VY`.
  * Paddle collision via `check_paddle_collision/1`.
  * Score detection: updates score, resets ball via `reset_ball/1`.
* `check_paddle_collision/1`: Detects paddle collision, inverts `VX`.
* `reset_ball/1`: Resets ball position and assigns new vertical speed.

#### Rendering:

* `draw_state/1`: Handles screen drawing:

  * Clears screen and hides cursor.
  * Displays score.
  * Builds and prints screen buffer via `build_screen_buffer/1`.
* `build_screen_buffer/1`: Generates a 2D screen buffer, draws borders, paddles, and ball.
* `set_char/4`: Utility to update characters at specific coordinates.

---

## 4. How to Compile and Run

To build the executable, ensure you have `rebar3` installed. Then, in the project root directory, run:

```bash
rebar3 escriptize
```

This will create an executable named `telejogo` in `_build/default/bin/`.

To run the game:

```bash
_build/default/bin/telejogo
```

### Controls:

* **Player 1:** `W` (up), `S` (down)
* **Player 2:** `O` (up), `L` (down) or Arrow Keys
* **Quit:** `Q`


