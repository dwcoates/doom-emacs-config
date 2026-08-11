# Webapp

## Building

- **Finish every webapp change by running `bin/build-frontend.sh webapp`.** Not
  `npm run build`. The script builds the same artifact AND writes the two stamps
  beside it that nothing else writes: `dist/.built-sha` (the source revision,
  read by the deploy report) and `dist/.build-id` (the artifact's own identity,
  taken from the entry bundle's content hash).
- `dist/.build-id` is what the webview's URL carries as `&build=`. The URL is
  otherwise fixed per workspace, so it is a stable cache key: a client can go on
  answering out of its own cache with a bundle from an earlier build — including
  one whose file has since been deleted — and no amount of rebuilding reaches
  the screen. The build id makes every build a different address.
- A missing `dist/.build-id` is a hard error at webview-mount time, not a
  degraded mode. Building with `npm run build` alone leaves it stale or absent,
  which is exactly that failure.
- The daemon serves `index.html` with `Cache-Control: no-store` for the same
  reason. That governs new responses only; it cannot evict what a client already
  stored, which is why the identity lives in the URL as well.

## Workspace-state freshness

- An open WebSocket is transport reachability, not current session state.
  `WsClient` reports `awaiting_snapshot` until the first `StateSnapshot` is
  decoded and atomically adopted by `ConversationStore`.
- GUI streams receive a `StateSnapshot` every 15 seconds. The browser's
  45-second lease expires after three missed snapshots, invalidates all active
  state and progress projections, and forces a reconnect. No disconnected or
  freshness-expired UI may retain `submitting`, `thinking`, `permission`, or
  another active phase.
- `WorkspaceState.at_ms` is the browser's monotonic revision. A regressing
  revision, conflicting payload at an equal revision, or `already_complete`
  beside an active phase is an ingestion invariant violation. Validate the
  entire adapter-effect batch before mutating store state.
- Emacs owns sidebar membership and non-current row status. The revisioned
  `ConversationStore` state owns both the footer and the current row status, so
  later roster pushes cannot overwrite the current session's phase.

## Logging

- The webapp owns one canonical logging API with normal and verbose emission
  functions in `src/wslog.ts`. New or changed webapp code uses that API only.
- Canonical workspace records are persisted by the daemon at
  `<workspace>/.claude/emacs/webapp.log`. The webapp is always attached to a
  session and workspace, so it has no global durable log. Missing workspace
  association is an invariant violation.
- Every record is JSON-shaped and carries a stable `operation`, `connection_id`,
  and every known session, request, workspace directory, and workspace ID.
  Never create human-formatted durable records or legacy logging identities.
- Every new or materially changed nontrivial function logs its entry. Every
  meaningful branch that selects a different nontrivial block, call, state
  transition, or outcome logs its selection.
- The normal helper persists through the daemon and emits to the browser
  console. The verbose helper always persists and gates browser-console output
  through the webapp verbose setting.
- Each error is logged exactly once by its owning layer with session, workspace,
  connection or request, operation, resolved inputs, branch outcome, and cause.
  Error-path tests assert the canonical record and its context.
- Log every critical state transition and branch that selects a materially
  different outcome. Errors use explicit `error` severity.
- Frequent or hot diagnostics use the verbose helper. Do not bypass logging.
  Direct `console` calls or ad hoc logger aliases are forbidden except a
  documented pre-logger bootstrap failure or logger-sink emergency path.

- `npm run typecheck` type-checks and `npm run coverage` measures authored
  `src/**/*.ts`, including branch data, excluding declarations and generated
  sources.
- `modules/app/agent-repl/bin/test-all.sh` (from the repository root) runs
  every tracked suite across the module.
- Maintain at least 90% statement coverage. Never reduce the measured baseline,
  and add focused tests for every critical branch and every error path changed.
- `modules/app/agent-repl/bin/test-all.sh --record` records suite timings to
  the canonical `modules/app/agent-repl/test_time.csv` for spotting timing
  regressions.

## Logging-density audit

`modules/app/agent-repl/bin/report-logging-density.sh webapp` reports
source-line and canonical-call counts. This is a rough syntactic review aid,
not semantic logging coverage. Directly audit every critical branch and error
path even when the ratio rises.

## The async teal wash, and what it guarantees

Teal is keyed on TOOL KIND, not on asyncness. `ASYNC_TEAL_TOOLS` in
`src/render.ts` is the one list — `Skill` plus `SUBAGENT_TOOLS` (`Task`,
`Agent`) — and `test/styles.test.ts` pins it to the stylesheet's
`.tool-card.tool-skill, .tool-card.tool-agent, .tool-card.tool-task` selector
list. A card can be async without being teal (a background `Bash` watcher is
grey), and a teal card need not have detached anything.

A teal card's NESTED SECTION — the expanded area holding its constituent
sub-bubbles: its activity fold, its stream fold, the `AsyncBubble` its call
detached, and that bubble's own children — is not an aside to be opened. It is
the card's content, so it carries three guarantees:

- ALWAYS OPEN. It renders through `Fold`'s `fixed` arm: no
  `data-panel-toggle`, no chevron, no click target, and no open state
  consulted.
- THE SHARED CAP. `.fold-fixed > .agent-panel` joins the shared N-line cap rule
  and takes `--cap-lines: var(--feed-cap-lines)` — the very budget a response
  or prompt bubble body stops at, stated once in `:root`.
- SCROLLED, NOT CLIPPED. `overflow-y: auto` comes off that same shared rule,
  and `max-height` (never `height`) lets a short body shrink to fit.

Adding a kind to the wash means inheriting all three. `test/render.test.ts`
asserts them for every member of `ASYNC_TEAL_TOOLS`, so a kind that gains the
teal without the guarantees fails the suite rather than shipping a card that
looks teal and still folds. Grey (non-teal) cards fold exactly as they always
have; nothing here changes them.
