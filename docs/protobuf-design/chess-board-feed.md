# Chess board in the agent-repl feed

An agent response must be able to show a chess game, a single position, or a
live engine session as an interactive board inside the webapp's response
bubble. The rebuilt agent-repl stack has no chess support at all: the daemon
serves no widget assets and reports no capability, the webapp draws no board,
and the `show-chess-game` skill probes an endpoint that no longer exists. The
feature is designed from scratch here; the pre-rebuild implementation is not a
reference.

## Context

Pending scoping (step 1).

## Core design principles

### agent-repl only renders the widget

- **Principle (user's terms):** the goal is to render the widget in the feed;
  agent-repl does nothing special beyond rendering it.
- **Consequences:** agent-repl parses, validates and analyzes no chess data.
  The feed row carries what the widget needs to mount and nothing derived from
  it; failures inside the game data are the widget's to show.
- **Reopens:** nothing landed yet.
- **Does not claim:** that the host has no work at all. Serving the widget
  bundle, supplying whatever the widget's host contract requires, and keeping
  the board mounted across feed redraws remain host work.

### Only CEE CLI sessions are eligible, and the skill owns finding them

- **Principle (user's terms):** the skill covers how to discover and specify
  chess game sessions, meaning CEE CLI sessions (`sdks/cli` in
  explanation-engine), which are the only things eligible for rendering.
- **Consequences:** no PGN or FEN board source exists in the contract; a board
  names a CEE CLI session. Discovering sessions and choosing one is the
  `show-chess-game` skill's job, not the daemon's or the webapp's.
- **Reopens:** the earlier scoping proposal's PGN / FEN / live-session trio.
- **Does not claim:** how the session's game reaches the widget (who calls the
  CEE resolver), which is still open.

## Landed changes
