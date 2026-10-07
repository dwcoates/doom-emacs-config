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

## Landed changes
