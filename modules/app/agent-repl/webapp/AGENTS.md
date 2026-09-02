# Webapp

The webview claude-repld serves: a TypeScript SPA that speaks generated Connect
clients to the daemon and renders server-resolved views. It derives nothing.
Where the daemon composed a sentence, the webapp draws the sentence.

## Layout

```
index.html                the SHELL: mount points by id, nothing else
src/main.ts               THE BOOT: builds the context, then mounts everything
src/shell.ts              resolves the shell's ids once, fails loudly by name
src/rpc/                  the contract layer, imported by every component
  transport | client        Connect over binary protobuf
  streams | unary           watchStream (standing streams) and callUnary
  strict | malformed        assertNoUnknownFields, requireCase, MalformedView
  context                   AppContext: client, workspace, ticker, failures
  refusal | refuse          the ONE refusal hook (see "Standing rules")
  guard                     guardMalformed: the ONE fire-and-forget click guard
  moved                     the page-wide "workspace moved" signal registry
  page-address | workspace-ref   ?workspace=<id>&dir=<dir>[&composer=1]
src/format.ts             the ONE client-side token formatter
src/clock.ts src/duration.ts   the shared ticker and its formatters
src/vocab.ts              typed accessors over proto/vocab/*.json
src/log.ts                the canonical logging API
src/link.ts               renderExternalLink / renderEditorLink
src/feed/                 the feed mechanism (feed, feed-view, bubble, rows)
  renderers.ts              THE SEAM, plus createRowRenderers: the registry
  cards/ asks/ merge/       the fifteen row renderers
src/footer/ src/topbar/ src/sidebar/ src/tray/ src/composer/ src/panels/
src/login/ src/lifecycle/ src/failure/      the remaining components
test/                     one test file per source module, mirroring src/
test/integration/         the whole app under jsdom against a fake daemon
```

## Mount order (src/main.ts, mirrored by test/integration/harness.ts)

1. `shellElements(document)` — a broken shell fails here, by id.
2. the page address, then the transport, the client and the failure overlay.
3. the logger, bound to this page's identity.
4. `adoptAtBoot(ctx)` — BEFORE any view stream. A joining daemon refuses every
   per-workspace rpc with `not_yet_adopted` until its rendezvous finishes, so
   adopting first turns a race into a wait. A terminal refusal throws
   `AdoptionFailed`, which the boot mints as `boot_failed`.
5. the mounts, in index.html's own top-to-bottom order, with two forced
   exceptions: the login overlay precedes the topbar (whose account control
   opens it), and the feed precedes the footer (whose jump rows reveal rows).
   sidebar, topbar, feed, hold tray, footer, composer (dev mode only), login
   overlay, lifecycle.

## The seams

- **`RowRenderers` (src/feed/renderers.ts).** The feed mechanism and the cards
  meet here and nowhere else. `createRowRenderers(ctx)` is the ONE place the
  fifteen keys are filled in; `main.ts` and the integration harness both call
  exactly it. A key added to the interface and not to the assembler does not
  compile.
- **The composer gate.** `footer.onStatus` drives `ComposerGate`: closed on
  `merging`, `closing` and `disconnected` (R7), and the reason shown is the
  FOOTER's own status word, never a second vocabulary for the same three states.
- **`workspaceMoved` (src/rpc/moved.ts).** `startLifecycle` registers the page's
  move handler; the refusal hook raises the signal by name. The registry lives
  in the rpc layer so a refusal can reach the mounted banner without the rpc
  layer importing a component.
- **The failure sink.** `mountFailureOverlay` IS the `FailureSink` every layer
  reports through.
- **The ticker.** One `Ticker` on the context; every clock subscribes to it.

## DOM hooks

The stable attributes the integration suite targets are specified in
`docs/overhaul/reports/webapp-briefs/WEBAPP-AGENT-PREAMBLE.md`, sections 5 and
5b. That list is the contract: use exactly those names, add nothing, rename
nothing. Values are generated oneof CASE names (lowerCamel) unless stated.

## Commands

```
npm test                   the unit suites (vitest + jsdom)
npm run typecheck          tsc over src/ AND test/, integration suite included
npm run build              typecheck plus vite build
npm run test:integration   the whole app against a loopback fake daemon
```

`bin/build-frontend.sh webapp` is what actually SHIPS a build: it writes
`dist/.built-sha` and `dist/.build-id` beside the artifact, and the build id is
what the webview URL carries as `&build=`, which is the only thing that defeats
a cached bundle. `npm run build` alone leaves those stamps stale, and a missing
`dist/.build-id` is a hard error at webview-mount time, not a degraded mode.

## Standing rules

- **STATELESS RENDERER.** No phase-to-word tables, no state-to-color mapping
  beyond a CSS class per arm, no counting rows to label chips, no token
  arithmetic, no ANSI parsing, no per-tool knowledge. Whole-view pushes replace
  their unit whole; feed rows upsert by `FeedId`; nothing accumulates across
  pushes.
- **TYPED ARMS, NO FALLBACKS.** Every oneof is switched exhaustively. An unset
  oneof, an unset non-optional message field, or an unknown arm is a
  `MalformedView` — never a default, never something else drawn instead. An
  absent `optional` field means draw nothing.
- **ONE REFUSAL HOOK.** `src/rpc/refuse.ts`. `refusalOf` for a call site that
  words its own refusal, `drawTypedRefusal` for one that lets the hook draw it,
  `crossCuttingSentence` for one that composes its own sentence — all three
  share a single implementation, and all three raise the page-wide move notice
  on `transferring_away`. The cross-cutting four are worded once in
  `src/rpc/refusal.ts`; never call `refusalSentence` from outside `src/rpc/`.
- **ONE CLICK GUARD.** `guardMalformed` (src/rpc/guard.ts). Every
  fire-and-forget click handler goes through it, so a `MalformedView` is logged
  once and filed as `frame_undecodable` instead of escaping as an unhandled
  rejection.
- **ONE TOKEN FORMATTER.** `formatTokens` (src/format.ts), mirroring the
  daemon's `format.go`: below 1000 unscaled; at or above it, k or M with
  exactly one fractional digit, a trailing ".0" trimmed, and the unit chosen by
  the RENDERED value (999950 reads "1M").
- **CLOCKS TICK CLIENT-SIDE.** The wire ships instants; subscribe to the shared
  ticker and format with `src/duration.ts`. Never a `setInterval` of your own.
- **STREAMS ARE STANDING.** A client ends a watch only by aborting it. A stream
  ending on its own is a transport failure: report it and reopen. Stopping
  anything is an `Interrupt` rpc, never a stream close. The webapp never
  redials a successor daemon.
- **EVERY CLICK IS AN RPC**, and its refusal renders AT the clicked control,
  never as pushed state. Domain outcomes (deny, nothing-running, empty) are
  SUCCESS arms.
- **THE FOUR IDENTIFIER SPACES** — `FeedId`, `TurnId`, `WorkspaceRef.id`,
  `FeedWatchToken` — are never interchangeable. Echo them verbatim.
- **LOGGING** goes through `src/log.ts` only (`log`, `logVerbose`). Every
  nontrivial function logs its entry at debug; every branch selecting a
  materially different outcome logs its selection; every error is logged
  exactly once by its owning layer with resolved inputs and cause. No direct
  `console.*` outside the documented pre-logger bootstrap path in `main.ts`.
- **SEMANTIC COLOR** comes from `proto/vocab/render-colors.json` and
  `paint-classes.json` through `src/vocab.ts`, and every consumer asserts its
  table row for row against the file, so a new arm without a color fails loudly.
- **CSS** is appended in a delimited section headed
  `/* ---- <component> (<file>) ---- */`. Existing classes are never renamed or
  restyled.
- **NEVER edit `proto/`.** The contract is frozen and the bindings are
  committed; a schema gap is reported, never patched locally.

## Tests

- One test file per source module, mirroring the directory: `src/feed/feed.ts`
  goes with `test/feed/feed.test.ts`. Table-driven, Arrange/Act/Assert, ONE edge
  case per test.
- Fixtures are built with `create(XSchema, {...})` from the generated code.
  Verbs are scripted with `createRouterTransport` from `@connectrpc/connect`.
- **NO REAL TIMERS.** `vi.useFakeTimers()`; never `await sleep(...)`.
- **NO NETWORK AND NO VENDOR CALLS.** The integration config sets
  `AGENT_REPL_FORBID_VENDOR_CALLS=1` as a standing tripwire; the only "real"
  server is the loopback fake daemon the suite starts itself.
- **NO REAL GIT.** Nothing here shells out to git.
- Every branch has a test: every arm rendered, every malformed input rejected
  (unset oneof, unset required field, unknown arm), every refusal arm drawn at
  its call site, every tick and every format.
