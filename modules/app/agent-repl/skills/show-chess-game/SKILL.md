---
name: show-chess-game
description: Show the reader an interactive chess board in the agent-repl feed for a game loaded in a CEE CLI session — the CEE CLI webapp's own widget, with its move list, engine lines and per-square engine answers. Finds the session and its live game id, then calls agent-repl's show_chess_board tool. Only CEE CLI sessions can be shown. Use when the user asks to show, render, or display a chess game, position, analysis, or CEE session on a board.
allowed-tools: Bash(<skill_base_dir>/run.sh:*), mcp__agent-repl__show_chess_board
lineage_root: user.dodge.skills.show-chess-game
---

**Requires the gns cee plugin.** If `gns cee` is unavailable, install it with `gns skills install cee-cli`, then retry.

## What This Skill Does

Shows a CEE CLI session's game as an interactive board in the agent-repl feed, at the point in the response where it is called. The board is the CEE CLI webapp's own widget. Everything after the call is handled downstream: the board appears in the feed and says what it is doing until it is ready, or why it cannot be shown.

## Arguments

| Argument | Behaviour |
|---|---|
| `<session-id>` | Show the game loaded in this CEE CLI session. |
| (none) | Show the game of the session this conversation created or used, else of the only live session. |

## Steps

1. Resolve the session to show.
  - a. If the user named a session, or this conversation created or used one, that is the session. Continue to step 2.
  - b. Otherwise call `<skill_base_dir>/run.sh --list-sessions`.
    - `EXIT CODE 0:` stdout is one session id per line. If it is exactly one, that is the session; continue to step 2. If it is several, ask the user which to show, and STOP until they answer.
    - `EXIT CODE 1:` no CEE CLI session exists. Tell the user a board needs a CEE CLI session with a game loaded, and STOP.
    - `EXIT CODE 2:` IMMEDIATELY terminate and surface the raw error.

2. Read the session's live game.
  - Call `<skill_base_dir>/run.sh --live-game <session-id>`.
    - `EXIT CODE 0:` stdout is the game id. Continue to step 3.
    - `EXIT CODE 1:` the session does not exist or holds no game. Surface the printed reason, and STOP.
    - `EXIT CODE 2:` IMMEDIATELY terminate and surface the raw error.

3. Show the board.
  - Call the `mcp__agent-repl__show_chess_board` tool with `session_id` set to the session id and `game_id` set to the game id from step 2.
  - Relay the tool's answer in one line. The board itself is in the feed; NEVER describe or re-draw the game in text.

## Notes

- **CRITICAL NOTE: Only CEE CLI sessions can be shown.** NEVER pass a PGN, a FEN, or anything but a session id and its game id.
- **CRITICAL NOTE: The game id is the session's LIVE one.** Always read it with step 2 immediately before calling the tool; NEVER reuse one remembered from earlier.
- **IMPORTANT NOTE: Build, start, or probe nothing.** The board's backend is readied downstream; a board that cannot be shown says why in the feed.
- **CRITICAL NOTE: Do not self-remediate a `run.sh` failure or read its internals.** React only to the documented exit codes.
