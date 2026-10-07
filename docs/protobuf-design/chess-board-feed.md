# Chess board in the agent-repl feed

An agent response must be able to show a chess game, a single position, or a
live engine session as an interactive board inside the webapp's response
bubble. The rebuilt agent-repl stack has no chess support at all: the daemon
serves no widget assets and reports no capability, the webapp draws no board,
and the `show-chess-game` skill probes an endpoint that no longer exists. The
feature is designed from scratch here; the pre-rebuild implementation is not a
reference.

## Context

Scoping closed on 2026-10-07. The owner then delegated every remaining design
decision ("just take it home, i dont want anymore input"), so the decisions
below from "The agent asks through an agent-repl MCP tool" onward were made by
the orchestrator without further review, and the iteration sequence was walked
without per-increment agreement: the agent's request, then `conversation.v1`,
then `frontend.v1`, then the square-click endpoint, then the skill text.

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
- **Consequence:** the widget itself knows no backend: it renders the bytes
  and reports clicks, and its host does all calling.
- **Retracted:** this entry first concluded that agent-repl neither builds nor
  serves widget assets. Both turned out wrong: cee-webapp serves the bundle
  from the checkout's unbuilt `dist/` (so the daemon builds it, below), and
  its route is cross-origin to the webview (so the daemon serves the bundle
  itself, below). Root cause: the conclusion was drawn from the route's
  existence before reading where the route reads its files from.

### The agent-repl daemon is the widget's host-side caller

- **Decision:** the agent-repl daemon calls `GetCeeWebWidget` and
  `GetSquareEvents`. The webapp mounts the widget from what the daemon serves
  and relays square clicks; it never calls cee-webapp itself.
- **Why:** the server resolves and the client renders verbatim, the same rule
  every other feed component follows.

### agent-repl starts cee-webapp when a board needs it

- **Decision:** when a board must be resolved and no cee-webapp is running,
  agent-repl starts it, rather than requiring the user to have started it.

### The daemon builds the widget backend itself; the skill builds nothing

- **Decision:** the agent-repl daemon builds the `cee-webapp` binary and the
  widget's `dist/` from the explanation-engine checkout when they are missing
  or older than their sources, then starts cee-webapp. Building, starting and
  calling cee-webapp have one owner.
- **Why:** the user judged the skill unnecessary for building once the daemon
  can do it; one owner also means no second party ever starts cee-webapp.
- **Evidence (code tier):** cee-webapp serves the widget from the checkout's
  `sdks/cli/web/packages/cee-web-widget/dist` (`internal/webapp/static/static.go`
  `DefaultDirs`, which resolves the checkout through `repopath.EngineDir()`),
  not from the plugin install, so "built" covers both the binary and that
  `dist/`. The widget package needs Node and its `sdks/cli/web` dependencies;
  cee-webapp needs Go and links no libcee (`sdks/cli/justfile`).
- **Consequences:** the board row carries the backend's state (building,
  build failed with its reason, session expired, ready), since nothing checks
  readiness before the board is asked for. The daemon must locate the
  explanation-engine checkout. The first board after a source change waits for
  a build.
- **Retracted:** a readiness request from the skill to the daemon, agreed one
  exchange earlier, under which the skill would build whatever the daemon
  reported missing. It became unnecessary once the daemon builds; the board
  row's own state replaces it.
- **The skill's remaining job:** finding CEE CLI sessions and asking for a
  board with one (session id and game id), per the sessions-only principle.

### A board whose session is gone shows as unavailable

- **Decision:** once the CEE session behind a board no longer exists (the CEE
  daemon sweeps idle sessions after an hour by default), the board shows as
  unavailable. The widget data is not saved with the response.
- **Why lost:** saving the widget data at response time would keep the board
  browsable, but square clicks would still fail against a missing session, so
  the board would work only partly.

### The agent asks through an agent-repl MCP tool, so a board is a conversation entry

- **Decision:** the shim hosts an in-process MCP server named `agent-repl`
  with one tool, `show_chess_board`, taking a CEE session id and game id. The
  call is converted like every modelled tool, into its own `AgentActivity`
  arm, so the board is part of the conversation record: it is durable, ordered
  where the agent asked for it, replayed on every page, and survives a daemon
  restart with no daemon-side storage.
- **Why the alternatives lost:** a daemon rpc the skill calls through
  `claude-repld call` would have needed a synthesized feed row (non-durable,
  like `FeedCommandPanel`) or a new durable-row store and an ordering rule for
  rows nobody's conversation contains. A marker line in the response text
  would carry an untyped payload inside prose.
- **Consequences:** the shim gains an SDK MCP server (`createSdkMcpServer`) and
  pre-allows its tool so the call never raises a permission prompt; the shim
  and the sidecar each gain a converter for the tool name
  `mcp__agent-repl__show_chess_board`; the mocked SDK must offer the tool.

### The board's widget data is carried as bytes

- **Decision:** `frontend.v1.FeedChessBoardWidget` carries the
  `chesscom.cee_webapp.v1.CeeWebWidget` as its binary encoding, and the
  square-click answer carries a `chesscom.cee_webapp.v1.GetSquareEventsResponse`
  the same way.
- **Why (the untyped-field exception):** the schema belongs to the CEE CLI in
  another repository, agent-repl relays the message without reading it (the
  only-renders principle), and the widget's own host contract takes exactly
  these bytes. Importing CEE's protos would make agent-repl's build depend on
  that repository's checkout and its `chesscom.*` dependency tree.
- **Accepted cost:** no compiler checks the relayed bytes; a CEE schema change
  reaches agent-repl only as whatever the widget does with them.

### The daemon speaks to cee-webapp over Connect's binary codec, by hand

- **Decision:** the daemon encodes the two tiny CEE requests
  (`GetCeeWebWidgetRequest`, `GetSquareEventsRequest`) with `protowire`, posts
  them as `application/proto`, keeps `GetSquareEventsResponse` whole, and
  takes field 1 of `GetCeeWebWidgetResponse` as the widget bytes.
- **Accepted cost:** the field numbers are a second spelling of CEE's
  schema; the vetting register records how to check them.

### The square click carries a typed echo token

- **Decision:** a ready board carries `FeedChessBoardSquareToken`, minted by
  the daemon from the board's session, and `InspectChessBoardSquare` takes it
  back with the displayed gamepoint and the clicked square. The daemon decodes
  the token; it keeps no table of boards.
- **Consequence:** the webapp tracks which gamepoint each board displays,
  from the widget's `onPositionChange`; that is the widget host contract's own
  requirement, not a derivation.

### The widget bundle is served by the agent-repl daemon from the built dist

- **Decision:** the daemon serves `cee-web-widget.js` and
  `cee-web-widget.css` from the checkout's widget `dist/` under its own HTTP
  listener, at a path stamped with the build's content stamp, and puts both
  URLs on the ready board.
- **Why:** the webview's page is the daemon's origin, so the module import is
  same-origin; cee-webapp's own `/v1/widget/` route would be a cross-origin
  module import it sends no CORS headers for. The stamp makes a rebuilt
  bundle a new URL, so a page never keeps a stale module.

### cee-webapp is reached through `gns cee debug webapp`

- **Decision:** the daemon ensures the cee-webapp singleton by running
  `gns cee debug webapp`, which spawns it when none is live and prints its
  base URL, with `CEE_AGENT_EXPLANATION_ENGINE_DIR` stated explicitly so the
  daemon and cee-webapp resolve the same checkout.
- **Why:** CEE's own lifecycle package owns the singleton (pid and address
  files, stale-state cleanup); the daemon adds no second discovery rule.

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
