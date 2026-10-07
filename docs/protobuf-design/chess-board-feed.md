# Chess board in the agent-repl feed

An agent response must be able to show a chess game, a single position, or a
live engine session as an interactive board inside the webapp's response
bubble. The rebuilt agent-repl stack has no chess support at all: the daemon
serves no widget assets and reports no capability, the webapp draws no board,
and the `show-chess-game` skill probes an endpoint that no longer exists. The
feature is designed from scratch here; the pre-rebuild implementation is not a
reference.

## Context

Scoping (step 1) in progress.

### The widget is the one the CEE CLI webapp uses

- **What:** the board is `@chesscom/cee-web-widget`
  (`explanation-engine/sdks/cli/web/packages/cee-web-widget`), mounted with
  `mountCeeWebWidget(element, { widgetBytes, onPositionChange, onSquareSelect })`.
  Its input is a `chesscom.cee_webapp.v1.CeeWebWidget` in binary form, which
  the hosting page obtains from
  `chesscom.cee_webapp.v1.CeeWebWidgetService.GetCeeWebWidget` for a
  `chesscom.cee_webapp.v1.CeeSessionMetadata` (session id plus game id); a
  square click is answered by the host calling `GetSquareEvents` and handing
  the response bytes back through `showSquareEvents`.
- **Why:** the user wants the same widget the CEE CLI debug webapp
  (`gns cee debug webapp`) renders, which is also the one that fits the
  sessions-only principle.
- **Rejected:** `explanation-engine/apps/cee-web-widget/dist`, an older bundle
  with a PGN / FEN / session-id mount interface. It is not what the CEE CLI
  webapp uses.
- **Evidence (code tier):** `sdks/cli/web/packages/analyze-position-app/src/components/WidgetHost.vue`
  is the CEE webapp's only widget host; `sdks/cli/web/packages/cee-web-widget/src/types.ts`
  is the host contract; `sdks/cli/internal/webapp/services/ceewebwidget/ceewebwidget.go`
  resolves the widget shape and lives under `internal/`, so agent-repl cannot
  import it. The widget's `dist/` is not built in the checkout inspected on
  2026-10-07.

### cee-webapp is the widget backend, and it serves the bundle too

- **What (code tier):** `cee-webapp` (`sdks/cli/cmd/cee-webapp`) serves the
  widget bundle under `/v1/widget/` (`internal/webapp/static/static.go`
  `WidgetRoute`) and answers `CeeWebWidgetService` and `CeeSessionService`
  over the same loopback listener (`cmd/cee-webapp/mux.go`). It is a per-user
  singleton that binds a free loopback port and publishes the bound address to
  an address file (`internal/webapp/lifecycle`). The gns cee plugin installs
  it at `~/.gns/plugins/cee/cee-webapp` (`sdks/cli/justfile`), beside
  `cee-cli-daemon`.
- **Consequence:** agent-repl neither builds nor serves widget assets; the
  bundle comes from cee-webapp. The widget itself knows no backend: it renders
  the bytes and reports clicks, and its host does all calling.

### The agent-repl daemon is the widget's host-side caller

- **Decision:** the agent-repl daemon calls `GetCeeWebWidget` and
  `GetSquareEvents`. The webapp mounts the widget from what the daemon serves
  and relays square clicks; it never calls cee-webapp itself.
- **Why:** the server resolves and the client renders verbatim, the same rule
  every other feed component follows.

### agent-repl starts cee-webapp when a board needs it

- **Decision:** when a board must be resolved and no cee-webapp is running,
  agent-repl starts it, rather than requiring the user to have started it.

### A board whose session is gone shows as unavailable

- **Decision:** once the CEE session behind a board no longer exists (the CEE
  daemon sweeps idle sessions after an hour by default), the board shows as
  unavailable. The widget data is not saved with the response.
- **Why lost:** saving the widget data at response time would keep the board
  browsable, but square clicks would still fail against a missing session, so
  the board would work only partly.

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
