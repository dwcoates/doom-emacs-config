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
  page-address | workspace-ref   ?workspace=<id>&dir=<dir>&log_level=<level>[&composer=1]
src/format.ts             the ONE client-side token formatter
src/clock.ts src/duration.ts   the shared ticker and its formatters
src/vocab.ts              typed accessors over proto/vocab/*.json
src/log.ts                the canonical logging API
src/link.ts               renderExternalLink / renderEditorLink
src/bubble/               THE ONE BUBBLE: draw (drawBubble, the spec) and body (the pipeline)
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
2. the page address, then the transport, the client, the client-local
   failures (`createLocalFailures`) and the topbar's MOUNT — its warning chip
   lists those failures from here on, before any stream exists.
3. the logger, bound to this page's identity and configured from the page's
   `log_level` boot parameter.
4. `adoptAtBoot(ctx)` — BEFORE any view stream. A joining daemon refuses every
   per-workspace rpc with `not_yet_adopted` until its rendezvous finishes, so
   adopting first turns a race into a wait. A terminal refusal throws
   `AdoptionFailed`, which the boot mints as `boot_failed`.
5. the mounts, in index.html's own top-to-bottom order, with two forced
   exceptions: the login overlay precedes the topbar's `watch` (whose account
   control opens it), and the feed precedes the footer (whose jump rows reveal
   rows). sidebar, topbar `watch`, feed, hold tray, footer, composer (dev mode
   only), login overlay, lifecycle.

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
- **The failure sink.** `createLocalFailures` IS the `FailureSink` every layer
  reports through; the topbar's warning chip is its one drawer.
- **The ticker.** One `Ticker` on the context; every clock subscribes to it.

## DOM hooks

The stable attributes the integration suite targets are specified in
`docs/overhaul/reports/webapp-briefs/WEBAPP-AGENT-PREAMBLE.md`, sections 5 and
5b. That list is the contract: use exactly those names, add nothing, rename
nothing. Values are generated oneof CASE names (lowerCamel) unless stated.

Attributes added by a LANDING after that list was written are recorded here,
and are contract on the same terms:

| attribute | on | values | landing |
|---|---|---|---|
| `data-delivery` | the marker on the SENDER's agent-prompt row | `queuedToLive` \| `resumedRecipient` \| `refused` (absent when `FeedAgentPrompt.delivery` is unset — every recipient copy) | 10, `refused` 14 |
| `.refused` class + `.prompt-refusal-reason` | the same delivery marker, when the send was REFUSED; the reason element is absent when the producer gave no account | — | 14 |
| `data-permission-verdict` | the answered permission card's `.perm-verdict` | the `FeedPermissionAnswered.answer` case, `deniedUndecidable` included | 10 |
| `data-query-cause` | the line under a `queryDied` turn error | `unexpectedEof` \| `iteratorFailure` (absent when the cause is unset) | 10 |
| `data-attention` | the cold gate row the model picker routed to | `coldGate` | 10 |
| `data-footer-notice` | the footer notice drawn when no cold gate row is on the page | `coldGate` | 10 |
| `data-wave` | the `.bubble.user` of a prompt row whose turn is IN FLIGHT | `working` — present exactly while the row's daemon-stated `working` flag (`FeedUserPrompt.working`, `FeedAgentPrompt.working`) is set, drawn at the bubble's draw and moved in place when the daemon re-pushes the row at the turn's terminal; never inferred from final-answer marks or `turn_ended` rows | int-fix-bubble-wave, redrawn by owner ruling 2026-09-14; daemon-owned since fix/prompt-working-until-turn-ends |
| `.held-right` class | every held-prompt card in the hold tray | — (the rail: a held prompt hangs where the `.bubble.user` it will become hangs) | owner ruling 1, 2026-09-13 |
| `.ws.open` class | a sidebar row whose detail panel is showing | — (there is NO chevron: the row opens its own panel on hover after `HOVER_OPEN_DELAY_MS` and closes it `HOVER_CLOSE_GRACE_MS` after the pointer leaves BOTH the row and the panel; keyboard focus opens it at once) | owner ruling, 2026-09-14 |
| `data-client-verdict` | the `.pfooter` dock, while a client link verdict stands | the verdict's kind (`unary_transport`, `stream_ended`, `subscription_source_ended`, `unsubscribe_failed`, `client_log_failed`, `daemon_unreachable_card`, `frame_undecodable_card`, `feed_not_tailing`) — absent whenever the daemon's pushed view is the one drawn | webapp/footer-client-states |
| `data-cold-gate-progress` | the cold-gate card's progress slot, inside `.hibernation-actions` | — (empty and `hidden` unless an answer is in flight AND the footer view carries a `compaction` activity; its text is the daemon's own line, verbatim) | cold-gate feedback, 2026-09-14 |
| `data-status-wave` | the `.footer-status` cell whose arm means PROGRESS | `progress` (absent on every other status, and on the client's own composed disconnected strip); the word is then per-letter `.pfooter-wave-letter` spans inside one `.pfooter-status-word` | webapp/footer-status-wave |
| `.topbar-account-cell` class | the strip's first cell, wrapping the connectivity glyph and the account chip in that order | — (the pair is one element, and it is the session-line reveal's anchor) | owner ruling 3, 2026-09-13 |
| `data-reviving` + `.reviving` class | the sidebar `.ws` row (`data-reviving`) and its `.name` (`.reviving`), while the row carries `RosterRowReviving` | `true` — absent once the daemon drops the marker (the revival ended, success or failure). The name wears the subtle `ws-revive-shimmer` ripple, phase-continued across redraws by `REVIVE_SHIMMER_PERIOD_MS` (src/sidebar/reviving.ts), and stopped under reduced motion | owner ruling, 2026-09-19 |
| `data-role` / `data-variant` / `data-cap-lines` | every blue and purple `.bubble` (src/bubble/draw.ts) | role `prompt` \| `response`; variant `response` \| `thinking` \| `agentic` \| `compaction` \| `user` \| `agent` \| `peer` \| `held`; cap `feed` \| `2` \| `0` | one-bubble, 2026-09-23 |
| `.bubble-strip` class | every header-strip element of a bubble (a click on it toggles the bubble's scroll box) | — | one-bubble, 2026-09-23 |
| `data-local-arms` / `data-local` | the topbar's `.topbar-warnings` chip (`data-local-arms`), and each client-local row in its list (`data-local`, with `data-arm`) | the standing client-local `FailureKind` arm names, space-separated, first-filed first — absent when none stands; the `#failure-overlay` and its `[data-arm]` cards are GONE | owner ruling, 2026-09-23 |

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
- **NO BUSINESS LOGIC, EVER** (owner ruling, 2026-09-21). The daemon is the
  source of truth and this is a renderer of it. The webapp does not decide
  what is true: not a status, not a substatus, not a count, not whether
  something is live, not an ordering. It draws the arm it was pushed.
  In particular it NEVER repairs a cross-surface invariant — reconciling the
  footer's status with the sidebar's here, so the screen reads consistently,
  is forbidden, because that invariant is the daemon's to guarantee (root
  `AGENTS.md`, "Workspace status is a cross-surface invariant") and a fixup
  here hides the daemon's defect from Emacs and from every other consumer
  while leaving it in place. A surface that looks wrong is reported to the
  daemon and fixed there. When a view needs a fact it does not have, the fact
  gets PUBLISHED; it is never inferred locally.
- **THE ONE EXCEPTION TO IT: THE CLIENT'S LINK VERDICT** (owner ruling,
  2026-09-13, `docs/REALTEST-JUDGEMENT-CALLS.md`, "webapp-side failures reach
  the footer"). When a call to the daemon fails at THIS end, no daemon can push
  the fact — the footer is written over the link that just failed, and
  `FooterStatusDisconnected` describes the daemon→shim link, not this one. So
  the webapp draws it: every failing site reports
  `reportClientFailure(kind, context)` (`src/rpc/link.ts`), and while a verdict
  stands the footer composes its three status cells itself — status
  `disconnected`, the kind's substatus ("daemon unreachable"; "feed not
  tailing" for an `OpenFeed` the daemon ANSWERED and refused; "frame
  unreadable" for a frame that would not decode), and the reporting site's own
  ad-hoc line as the activity. The clock, tokens and chips stay the daemon's
  last pushed ones, the composer gate is NOT closed by a verdict (the retry is
  what lifts it), and a push does NOT lift one: only a unary the daemon
  answered, or a stream that reads again, does. Nothing else in the webapp
  composes a footer cell, and no new arm was added to the proto for it.
- **THE TOPBAR'S WARNING CHIP IS THE ONE PLACE AN ERROR IS SHOWN** (owner
  ruling, 2026-09-23). The chip and its dropdown (`src/topbar/warnings.ts`)
  are the canonical surface on which the webapp makes an error visible: no
  overlays, banners, toasts or corner cards. The daemon's pushed warnings are
  drawn there verbatim; the page's own client-local failures
  (`src/failure/local.ts`) merge in client-side, ahead of them and in the same
  count, and never wait on a push — the topbar mounts before adoption so the
  chip can list a failure that stops every stream. The chip is red (`--err`).
  Every error is ALSO logged: filing one writes `warning-chip.report`, clearing
  it `warning-chip.retract`.
- **TYPED ARMS, NO FALLBACKS.** Every oneof is switched exhaustively. An unset
  oneof, an unset non-optional message field, or an unknown arm is a
  `MalformedView` — never a default, never something else drawn instead. An
  absent `optional` field means draw nothing.
- **ONE VIEWED MODE.** `ViewedRegistry` (src/sidebar/viewed.ts) decides every
  row's FULL/PARTIAL display mode, and nothing else reads `RosterRowViewed`. In
  PARTIAL the row's NAME greys to `--muted` and NOTHING else changes — the
  status dot keeps its tone. It is the ONE deliberate piece of memory in the
  stateless renderer: it remembers the status arm it last drew per workspace,
  so that a row whose arm CHANGED is drawn FULL even while the wire still
  carries the marker. That rule is the daemon's own (it clears the marker on
  the same edge) and the Emacs tab-bar's; the three may never disagree, and the
  module-root AGENTS.md section "The viewed mode" owns the invariant. The pass
  is bracketed like the blink pass, because both groupings draw every row and
  the two copies must resolve identically.
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
- **ONE BUBBLE** (owner rulings, 2026-09-23). Every blue (prompt) and purple
  (response) bubble — a response, thinking, an agentic card, a compaction
  summary, a user or agent prompt, a peer message, a held prompt — is built by
  `drawBubble(spec, previous)` (src/bubble/draw.ts), and a kind's module only
  builds its spec: role, variant and state, the header strip (plus the
  response's usage corner), the content, the collapsed line count and, on a
  prompt, the working flag. Content goes through ONE body pipeline
  (src/bubble/body.ts): prose is a `markdownSlot`, painted by the body, so a
  metaprompt tree in any bubble wraps at that bubble's cap (`--bubble-max-width`)
  and a bubble below its cap never wraps; a paint that needs a width waits for
  the bubble to be laid out, and an unmeasurable width still fails loudly. A
  redraw given the row's previous draw updates it IN PLACE. The stylesheet has
  ONE rule set on `.bubble`: one size and leading, `[data-role]` sets only the
  side and `--bubble-bg`, a variant or state only the border, `[data-cap-lines]`
  the collapsed limit; one scroll box, one has-more measurer (bubble-more.ts)
  and one toggle (expand.ts, which also opens a bubble from its header strip).
  `test/bubble/consolidation.test.ts` fails any bubble, box, body, wrap, paint,
  has-more or toggle logic built anywhere else. Three rulings of 2026-09-23 ride
  it: "more below" is the FADE ONLY, never a chevron, and `has-more` means the
  BODY's rendered lines run past the cap (never the box's `scrollHeight`, which
  counts the usage corner's hit area); a prompt's border lands once it is
  received and stays (the user's purple independent of the wave, one amber
  `--agent-prompt-border` for agent-addressed prompts and peer messages, none
  on a held prompt); and a tree's column budget always takes off the expanded
  scrollbar's gutter (`--scrollbar-gutter-width`), so it never re-wraps on a
  click.
- **ONE VIEWPORT CLAMP.** `clampReveal` (src/topbar/clamp.ts). Any panel that
  must hang under an anchor and stay inside the window places itself through
  it — the topbar reveals and the sidebar row's detail panel both do — rather
  than growing a second set of edge rules.
- **ONE TOKEN FORMATTER.** `formatTokens` (src/format.ts), mirroring the
  daemon's `format.go`: below 1000 unscaled; at or above it, k or M with
  exactly one fractional digit, a trailing ".0" trimmed, and the unit chosen by
  the RENDERED value (999950 reads "1M").
- **CLOCKS TICK CLIENT-SIDE.** The wire ships instants; subscribe to the shared
  ticker and format with `src/duration.ts`. Never a `setInterval` of your own,
  and never `ctx.ticker.subscribe` directly either: go through `tick`
  (`src/feed/ticking.ts`), which is what makes the subscription FINDABLE by
  `stopTicking` from a replace site, a dispose, or the turn-end backstop.
- **A TIMER STOPS WHEN ITS UNIT SETTLES.** A card drawn with a terminal arm
  calls `stopTicking` on its own element and shows the span the MESSAGE
  reports, never a wall-clock reading. Whoever discards an element stops it
  first — every site that replaces a host's children goes through the ONE
  helper, `replaceTicking`, which stops what it drops and keeps what it merely
  moves. As a backstop, `feed-view` stops every remaining clock in a turn once
  that turn's `turn_ended` row lands, and records one DEBUG
  (`feed.turn-end-stopped-clocks`) naming the rows it had to stop — a card that
  needed the backstop failed the first rule, and that log is how it is found.
- **STREAMS ARE STANDING.** A client ends a watch only by aborting it. A stream
  ending on its own is a transport failure: report it and reopen. Stopping
  anything is an `Interrupt` rpc, never a stream close. The webapp never
  redials a successor daemon.
- **THE PAGE HOLDS ONE CONNECTION, AND NOTHING BUT THE MUX MAY OPEN ONE.**
  Every standing watch goes through `ctx.streams.watch(kind, request, signal)`
  (`src/rpc/page-streams.ts`), which multiplexes it onto the page's single
  `WatchPage` stream. NEVER call `client.watch*` for a server-streaming rpc from
  anywhere else; `test/rpc/page-streams.test.ts` reads the whole `src` tree and
  fails on any module that does.

  This is not tidiness. A webview negotiates **http/1.1** — the daemon serves
  h2c, but no browser negotiates cleartext HTTP/2 — and HTTP/1.1 caps a page at
  **six** connections per host, measured exactly on this daemon in the e2e
  sandbox: standing streams 1-6 reached it within 9ms and were logged as
  accepted; streams 7, 8 and 9 produced NO daemon record at all, and a plain
  same-origin `GET` taken while six were held timed out in the browser after 5s
  while the same `GET` with five held returned 200 in under a millisecond.

  The page used to open six dedicated watches — `WatchWorkspaceRoster`,
  `WatchWebWorkspace`, `WatchDaemon`, `WatchTopbar`, `WatchFooter`,
  `WatchDaemonHolds` — before its feed tail. `WatchFeed` was the seventh, and it
  did not fail: it QUEUED, forever, with no request on the wire and therefore no
  `daemon_unreachable` card, so the root feed never drew a row produced after
  the page loaded. Every expanded subagent bubble opens another feed tail, so no
  fixed budget could have contained the count — which is why the guarantee is
  "one stream exists" rather than "few enough streams exist".
- **THE USER OWNS THE SCROLL** (owner rule, 2026-09-23). `src/scroll.ts` is
  the ONE module that writes a scroll position; `test/scroll.test.ts` scans
  every other `src` module and fails on a `scrollTop`/`scrollLeft` assignment,
  a `scrollIntoView`/`scrollTo`/`scrollBy` call, or a park/place/shift call.
  A bubble's own scroll box (an expanded response, thinking, tool or async
  bubble) has NO implicit writer: it moves only on the reader's input. The
  feed moves implicitly only for the closed set `SCROLL_CAUSES` —
  `promptSent`, `selectionMoved`, `detachedWorkSelected`, `initialPlacement`,
  `replaceRestore`, `prependCompensation`, `collapseCompensation` (a thinking
  bubble wholly above the reader collapsing when the daemon marks it
  `superseded`; the view shifts by exactly the height it lost), `latestVisible`
  — each a named `TailFollow` cause, each move recorded at DEBUG as
  `scroll.feed-moved` with its cause. A follow starts from a parking cause, or
  (`latestVisible`, owner rule 2026-09-23) whenever the reader can SEE the feed
  column's latest entry (the last root row, or the hold tray's last card;
  `latestEntry` in src/feed/feed.ts, "can see" defined once by
  `latestEntryVisible`), evaluated on every scroll event, resize and row
  upsert. That latch moves nothing; later content then keeps the tail in view.
  An active reply selection holds it off until the selection clears, and a
  click on the feed OUTSIDE ANY BUBBLE (owner ruling 2026-09-23;
  `isFeedBackground` in src/feed/background-click.ts is the one hit test: the
  scroll box, its direct children, or a root row's `.feed-item` wrapper) is
  what clears it — by sending the daemon `SelectResponse` CLEAR, never locally;
  the daemon's cleared push parks through `selectionCleared`. A follow
  ends when the reader scrolls the latest entry away. The reader's own wheel
  redirect and
  collapse click are input, not causes, and are the only other writes.
  NOTHING MOVES A SCROLL BOX INDIRECTLY EITHER: a redraw never re-attaches an
  element already in place (`placeChildren`, src/dom.ts), a response re-push
  updates its bubble in place, a card holding a box the reader scrolled is
  morphed rather than replaced (src/feed/keep-scroll.ts), an unchanged re-push
  draws nothing, and anything repainted on a tick or toggled by a measurer
  holds a fixed footprint (the cost corner, the shell clocks, `has-more`).
- **A REDRAW NEVER UN-TOGGLES, WHATEVER ITS SHAPE** (owner ruling, 2026-09-18).
  The wire's fold is the INITIAL fold: an upsert carries the reader's open folds
  off the element it replaces (`carryExpanded`), and a full page replace —
  reconnect, reload, compaction replay — snapshots them by row `FeedId` across
  the teardown (`snapshotExpanded`/`retainRows` in src/expand.ts, spent in
  `feed-view.ts`), the expanded bubble's own 50vh scroll box included.
- **EVERY CLICK IS AN RPC**, and its refusal renders AT the clicked control,
  never as pushed state. Domain outcomes (deny, nothing-running, empty) are
  SUCCESS arms.
- **THE FOUR IDENTIFIER SPACES** — `FeedId`, `TurnId`, `WorkspaceRef.id`,
  `FeedWatchToken` — are never interchangeable. Echo them verbatim.
- **LOGGING** goes through the one logger in `src/log.ts`. Its public emission
  API is exactly one method per level: `log.debug`, `log.info`, `log.warn` and
  `log.error`. A call marks tracing-only evidence with
  `verbosity: "verbose"`; omitted verbosity is `normal`. Every forwarded
  `agentrepl.v1.ClientLogRecord` carries the client-side `timestamp` captured
  before throttling and the record's `verbose` class. Its `context` contains
  the call site's fields plus logger-bound connection and session identities,
  never a nested copy of the complete record.

  `AGENT_REPL_LOG_LEVEL` is the only threshold. The Emacs webview host reads it
  and carries its effective value in the page URL as `log_level`; the page
  validates `debug|info|warn|error` during boot. An absent parameter uses the
  contract's `info` default for ordinary browser development. An invalid
  present value aborts boot. There is no `localStorage` logging toggle and no
  second verbose-console switch.

  The daemon persists forwarded records to the workspace's canonical
  `.claude/emacs/webapp.log`. Every nontrivial function logs its entry at
  debug; every branch selecting a materially different outcome logs its
  selection; every error is logged exactly once by its owning layer with
  resolved inputs and cause. `npm run lint` forbids direct `console.*` outside
  `src/log.ts` and the documented pre-logger bootstrap path in `main.ts`.

  Read forwarded webapp records and harvest run windows through
  `../bin/logs.sh`; the full path, rotation, attribution, and level-switch
  table is in `../AGENTS.md`.
- **SEMANTIC COLOR** comes from `proto/vocab/render-colors.json` and
  `paint-classes.json` through `src/vocab.ts`, and every consumer asserts its
  table row for row against the file, so a new arm without a color fails loudly.
- **CSS** is appended in a delimited section headed
  `/* ---- <component> (<file>) ---- */`. Existing classes are never renamed or
  restyled.
- **NEVER edit `proto/`.** The contract is frozen and the bindings are
  committed; a schema gap is reported, never patched locally.

## Verification

```bash
npm run lint             # eslint, type-aware, over src/, test/ and the root configs
npm run typecheck        # tsc over src/ and test/
npm test                 # the unit suite (un-isolated; see vitest.config.ts)
npm run test:integration # the whole app under jsdom against the fake daemon
npm run coverage         # istanbul, per file, isolated (see vitest.config.ts)
npm run coverage:verify  # prove the per-file numbers are still a measurement
```

### Coverage is istanbul, and the per-file numbers moved when it became one

The package reads **98.47% of lines, 97.95% of statements, 94.82% of branches
and 98.31% of functions**, against 97.97 / 97.98 / 95.96 / 97.09 under the old
provider. The AVERAGE barely moved, which is exactly why the defect went
unnoticed for so long: the per-file numbers underneath it were wrong in both
directions and roughly cancelled.

`@vitest/coverage-v8@2.1.9` merges each test-file window's RAW V8 coverage with
`mergeProcessCovs` BEFORE remapping it through the source maps, so a module
compiled in more than one window keeps one contributor's ranges instead of the
sum. `src/scroll.ts` read 100% of its statements with only `test/scroll.test.ts`
running and 47.71% with `test/feed` added; `src/format.ts` 100% against 84.61%;
`src/markdown.ts` 100% against 59.52%. Which contributor survives depends on
which files shared a process, so the figures also moved run to run: two whole
suite runs differing only in test-FILE order disagreed on 66 of 100 files, some
of them about how many lines the same module even has. `--isolate`,
`--pool=forks` and `coverage.all: false` were each tried and none of them
touches it.

Under istanbul the files that were losing counts to the merge came back up
(`scroll.ts` 47.71 → 98.82, `markdown.ts` 59.52 → 100, `format.ts` 84.61 →
100), and files v8 had been flattering came down. The largest single fall was
`src/main.ts`, from a fictional 100% to 0%: it is the mount entry, and no unit
test loaded it -- its only exercise was `test/integration/` and the webapp
layer, neither of which counts here or can say which of its branches a
regression hit. `test/main.test.ts` now runs the real boot under jsdom, with
only the wire and the mounts substituted, and it reads 100% again -- honestly
this time.
Three type-only modules (`src/feed/cards/context.ts`, `src/topbar/context.ts`,
`src/tray/context.ts`) left the report entirely, because a file of `interface`
declarations has no statement to cover and v8 was scoring it 100% of nothing.

`npm run coverage:verify` (`bin/coverage-honesty.mjs` at the module root) is
what stops a later provider bump bringing any of that back: it runs this
package's own `npm run coverage` three times — once naturally ordered, twice
with the test FILES shuffled under fixed seeds — and fails if any file's
covered or total count moves, then checks that a probe module reads no LESS in
the full suite than it does with only its own test file running. Run it after
any change to the provider, its version, or the isolation the coverage script
buys. Under the v8 provider it fails on 66 of the 100 files.

`npm run lint` is TYPE-AWARE and is not a style pass: it reads the same program
`tsc` does, and the rules it adds on top are the ones that catch what `tsc`
cannot see — a floating promise, a `switch` with neither a missing arm's case
nor a default, an `any` that spreads through an object literal, a `||` that
substitutes a default for a legitimately empty string. Several of the standing
rules above are mechanized in it: logging goes through `src/log.ts` only, and
`no-console` now says so everywhere but the two documented bootstrap sites.

Its rule set and every deliberate omission are argued inline in
`eslint.config.js` — including the measurement behind the exhaustiveness rule's
setting. Disagree with a rule there, in one place, rather than with an inline
disable. An inline disable is legitimate when it carries a `--` reason a
reviewer would accept, and unused ones fail the run.

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

### Wait/timeout bounds

Every wait bound in both suites is set to roughly 3x the slowest healthy
duration actually observed, never left at a tool default. Measured against
97 unit files (3279 tests) and all 13 integration files (1601 assertions):

| bound | old (default) | new | observed healthy max | why |
|---|---|---|---|---|
| unit `testTimeout`/`hookTimeout` (`vitest.config.ts`) | 5000ms / 10000ms | 850ms / 850ms | 272.8ms (`test/feed/cards/shell.test.ts`, re-measured; see below) | no real I/O, everything fake-timered |
| integration `testTimeout`/`hookTimeout` (`vitest.integration.config.ts`) | 5000ms / 10000ms | 900ms / 900ms | 274.8ms (in `refusals.integration.test.ts`) | in-process loopback fake daemon, instant to start |
| `SETTLE_ROUND_CAP` (`test/integration/harness.ts`) | 60 rounds | 60 rounds (unchanged) | 24 rounds (also in `refusals.integration.test.ts`) | already a ~2.5x margin; the 3x rule would ask for 72, which is looser than the current cap, so it stays — a bound is never loosened to fit a formula |

No per-site exception was needed: nothing in either suite (xterm/login
terminal included) took long enough to need its own raised `timeout`. If a
future test genuinely needs more than these globals, give it its own
`{ timeout: ... }` with a one-line comment naming why, rather than raising
the shared bound.

**Unit `testTimeout` re-derivation (300ms proved too tight).** The 300ms unit
bound tripped three times under load on tests that pass alone —
`test/feed/asks/question.test.ts`, `test/feed/cards/shell.test.ts`, and
`test/feed/feed.test.ts` ("tails the token the reopen minted") — with no real
timer or heavy fixture in any of them. Re-measured with
`npx vitest run --reporter=json`, four passes: two quiet, two with the box
pinned on all 16 cores (`yes > /dev/null &` x4, killed after). All 3359 tests
passed every time; per-run slowest-test figures:

| run | slowest test | duration |
|---|---|---|
| quiet 1 | `shell.test.ts`: "the stop control interrupts the detached target by this row's own id" | 272.8ms |
| quiet 2 | `question.test.ts`: "answering sends the allowOnce arm from the allowOnce button" | 211.4ms |
| loaded 1 (`yes` x4) | `shell.test.ts`: "the stop control clears the outcome once it has been readable long enough" | 197.9ms |
| loaded 2 (`yes` x4) | `question.test.ts`: "the entry click is SelectWorkspace and nothing else (R8) echoes the ref the queue served, verbatim" | 174.7ms |

The old 88.9ms baseline no longer holds: the healthy max across these four
runs is 272.8ms, inside the old 300ms bound with essentially no margin —
that gap, not a slow test, is the flake. Ruling: (b) — the bound was too
tight for the suite's own variance, not any one test's arrangement.
`testTimeout`/`hookTimeout` are re-set to 850ms (~3x the 272.8ms measured
max).
