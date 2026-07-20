---
name: show-chess-game
description: Display an interactive chess board inside the agent-repl GUI response bubble. Use when the user asks to show, render, or display a chess game, position, or live analysis session on a board — from a PGN, a FEN, or a live engine session id. The response carries a one-line marker the GUI renders as the board.
lineage_root: user.dodge.skills.show-chess-game
---

# Show Chess Game

## What This Skill Does

Renders an interactive chess board inside the current agent-repl response bubble. The game payload (PGN, FEN, or a live engine-session id) is piped into `run.sh`, and the response carries only the one-line marker `run.sh` prints, which the GUI replaces with the board. Text before and after the marker keeps flowing around the board in the same bubble.

## Arguments

| Argument | Behaviour |
|---|---|
| (PGN source) | A PGN document — inline text, a file path, or a command that produces one — renders as a steppable, playable game board. |
| (FEN source) | A single FEN line renders as a position board. |
| (session id) | A live engine-session id renders as a board mirroring that session in real time. |

## Steps

1. Resolve the payload form from the request, then dispatch:
   - a. If the game is a PGN document:
     - i. When a command can produce it (a file on disk, an engine/API call), pipe that command's output DIRECTLY into `<skill_base_dir>/run.sh --write-game pgn` so the payload never transits the response.
     - ii. Otherwise pipe the PGN text you hold into `<skill_base_dir>/run.sh --write-game pgn`.
   - b. If the game is a single FEN position, pipe the FEN line into `<skill_base_dir>/run.sh --write-game fen`.
   - c. If the game is a LIVE engine session, call `<skill_base_dir>/run.sh --write-session <session-id>`.
2. React to the `run.sh` exit code:
   - `EXIT CODE 0:` stdout is the marker line. Continue to step 3.
   - `EXIT CODE 1:` usage error. IMMEDIATELY terminate and surface the raw error.
   - `EXIT CODE 2:` environment or input error. IMMEDIATELY terminate and surface the raw error.
   - `EXIT CODE 3:` the engine daemon's address was not discoverable. Ask the user for the backend URL, then re-run step 1c as `<skill_base_dir>/run.sh --write-session <session-id> <url>`.
   - `EXIT CODE 4:` the chess-widget capability is unavailable, so no board can render. stdout is actionable remediation, NOT a marker. Surface the printed remediation to the user VERBATIM as plain text, and NEVER emit a marker line.
3. Emit the marker line in the response:
   - a. Re-emit the printed marker line VERBATIM, on its own line, as plain text.
   - b. NEVER wrap the marker line in a code fence, inline code, or a blockquote — a fenced marker renders as literal text instead of a board.
   - c. Place any commentary before and/or after the marker line; both render around the board in the same bubble.

## Notes

- **CRITICAL NOTE: the marker line must be re-emitted verbatim and unfenced.** Any wrapping or edit breaks the board rendering.
- **CRITICAL NOTE: never paste large game payloads into the response.** The payload travels through `run.sh`; the response carries only the marker line.
- **IMPORTANT NOTE: do not self-remediate `run.sh` failures or read its internals.** React only to the documented exit codes.
- **IMPORTANT NOTE: the board requires the agent-repl daemon's chess capability.** When `run.sh` exits 4 it has already printed the remediation to surface, so NEVER hand-write your own capability guidance and NEVER emit a marker anyway.
