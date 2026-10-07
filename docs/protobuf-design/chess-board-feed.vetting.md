# Chess board in the agent-repl feed — vetting register

Investigations owed by decisions in `chess-board-feed.md` that rest on
unverified assumptions about the CEE CLI (explanation-engine `sdks/cli`).

## V1. The hand-encoded CEE field numbers match CEE's schema

- **Assumption:** `GetCeeWebWidgetRequest.session = 1`,
  `CeeSessionMetadata.session_id = 1`, `CeeSessionMetadata.game_id = 2`,
  `GetCeeWebWidgetResponse.widget = 1`, `GetSquareEventsRequest.session = 1`,
  `.game_point = 2` (int64), `.square = 3` (enum).
- **Affected:** the daemon's CEE client encoding; a mismatch makes every board
  unavailable or every square click fail.
- **How to verify:** read `sdks/cli/proto/chesscom/cee_webapp/v1/cee_web_widget_service.proto`,
  `cee_session.proto` and `square_events.proto` in the checkout the daemon
  resolves, and compare with the constants in the daemon's CEE client.
- **Status:** CONFIRMED against the checkout at
  `/Users/dodgecoates/workspace/ChessCom/explanation-engine` on 2026-10-07 by
  reading those three files. A later CEE change is not detected automatically.

## V2. `gns cee debug webapp` prints the singleton's URL as JSON

- **Assumption:** the command prints `{"url": "<base URL>"}` on success
  (`internal/daemon/debug_webapp.go` `DebugWebappResult`) and exits non-zero
  with a diagnosis otherwise.
- **Affected:** the daemon's start-up of cee-webapp.
- **How to verify:** run `gns cee debug webapp` with the plugin installed and
  read its stdout; confirm the gns envelope does not wrap the result.
- **Status:** CONFIRMED 2026-10-07: it printed `{"url":"http://127.0.0.1:60258"}`
  unwrapped, and a repeat call answered in about 80ms, reusing the live
  singleton.

## V3. The widget's npm build runs from a clean checkout

- **Assumption:** `npm ci` in `sdks/cli/web` followed by
  `npm run build -w @chesscom/cee-web-widget` produces
  `packages/cee-web-widget/dist/cee-web-widget.js` and `cee-web-widget.css`
  with the user's `~/.npmrc` registry auth, and needs nothing generated first
  (the `gen/` workspace is checked in).
- **Affected:** every board on a machine whose widget is not yet built.
- **How to verify:** run both commands in a checkout with no `node_modules`
  and list the `dist/` they leave.
- **Status:** OPEN.

## V4. An agent can learn a session's game id

- **Assumption:** `gns cee session poll` reports `game_id` for a session that
  holds a game (`internal/daemon/poll.go` `SessionPollResult`), so the skill
  can hand the tool both identifiers.
- **Affected:** the `show_chess_board` tool's input and the skill text.
- **How to verify:** create a session with a game, run the poll op against it,
  read `game_id`.
- **Status:** PARTLY CONFIRMED 2026-10-07: `gns cee session poll <id>` takes the
  id positionally and answers `active`, `has_game`, `root_gamepoint` (shown for
  a session that does not exist); `game_id` is declared `omitempty` in
  `internal/daemon/poll.go`, so it appears only for a session holding a game.
  A poll of a live session with a game is still owed.
