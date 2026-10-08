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
src/link.ts               renderExternalLink / renderEditorLink / renderMergeTestLogLink
src/bubble/               THE ONE BUBBLE: draw (drawBubble, the spec) and body (the pipeline)
src/feed/                 the feed mechanism (feed, feed-view, bubble, rows)
  renderers.ts              THE SEAM, plus createRowRenderers: the registry
  cards/ asks/ merge/       the fifteen row renderers
src/footer/ src/topbar/ src/sidebar/ src/tray/ src/composer/ src/panels/
src/login/ src/lifecycle/ src/failure/      the remaining components
src/news-digest/          the daily Claude news digest overlay over the feed
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
   only), login overlay, lifecycle. The news digest overlay is mounted with the
   lifecycle (just before it), because the lifecycle's `WatchDaemon` stream is
   what carries its `news_digest` standing.

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
| `data-query-cause` | RETIRED with the ended-turn bubble (owner ruling, 2026-10-06): a query death's cause is the outcome marker's expansion line `data-line="what-died"`, and what it threw `data-line="thrown"` | — | 10, retired 2026-10-06 |
| `.outcome-marker` + `data-outcome-marker` / `data-family` / `data-expanded` | THE OUTCOME MARKER (src/feed/marker.ts): a turn ending's (inside `.turn-ended`), a user denial's (inside `.perm-verdict`), a broken plan's (inside `.plan-failed`) and a failed compaction's (inside `.sep-compaction-failed`, with no divider) | the `FeedOutcomeMarker.family` case: `neutral` \| `vendorFault` \| `agentReplFault`; the root also wears that family's `.tone-<color>` (render-colors.json#feed_outcome_marker); `data-expanded` while a fault's expansion is open. Inside: `.outcome-marker-pill` (a control with `aria-expanded` on a fault, a plain span on a neutral marker), `.outcome-marker-glyph[data-glyph]` (`stop` \| `diamond` \| `cross`), `.outcome-marker-label`, `.outcome-marker-detail`, `.outcome-marker-chevron` (faults only), and `.outcome-marker-expansion` holding `.outcome-marker-line[data-line]` (`time` \| `error-type` \| `message` \| `retries` \| `retry` \| `model` \| `account` \| `what-died` \| `thrown` \| `restarted`) and `.outcome-marker-action[data-action]` (`sign-in` \| `resend`; a resend's refusal is a `.refusal[data-arm]` beside it, an accepted one wears `data-resent`) | owner ruling, 2026-10-06 |
| `data-attention` | the cold gate row the model picker routed to | `coldGate` | 10 |
| `data-footer-notice` | the footer notice drawn when no cold gate row is on the page | `coldGate` | 10 |
| `data-wave` | the `.bubble.user` of a prompt row whose turn is IN FLIGHT | `working` — present exactly while the row's daemon-stated `working` flag (`FeedUserPrompt.working`, `FeedAgentPrompt.working`) is set, drawn at the bubble's draw and moved in place when the daemon re-pushes the row at the turn's terminal; never inferred from final-answer marks or `turn_ended` rows | int-fix-bubble-wave, redrawn by owner ruling 2026-09-14; daemon-owned since fix/prompt-working-until-turn-ends |
| `.held-right` class | every held-prompt card in the hold tray | — (the rail: a held prompt hangs where the `.bubble.user` it will become hangs) | owner ruling 1, 2026-09-13 |
| `.ws.open` class | a sidebar row whose detail panel is showing | — (there is NO chevron: the row opens its own panel on hover after `HOVER_OPEN_DELAY_MS` and closes it `HOVER_CLOSE_GRACE_MS` after the pointer leaves BOTH the row and the panel; keyboard focus opens it at once) | owner ruling, 2026-09-14 |
| `data-client-verdict` | the `.pfooter` dock, while a client link verdict stands | the verdict's kind (`unary_transport`, `stream_ended`, `subscription_source_ended`, `unsubscribe_failed`, `client_log_failed`, `daemon_unreachable_card`, `frame_undecodable_card`, `feed_not_tailing`) — absent whenever the daemon's pushed view is the one drawn | webapp/footer-client-states |
| `data-cold-gate-progress` | the cold-gate card's progress slot, inside `.hibernation-actions` | — (empty and `hidden` unless an answer is in flight AND the footer view carries a `compaction` activity; its text is the daemon's own line, verbatim) | cold-gate feedback, 2026-09-14 |
| `data-status-wave` | the `.footer-status` cell whose arm means PROGRESS | `progress` (absent on every other status, and on the client's own composed disconnected strip); the word is then per-letter `.pfooter-wave-letter` spans inside one `.pfooter-status-word` | webapp/footer-status-wave |
| `.topbar-account-cell` class | the strip's first cell, wrapping the connectivity glyph and the account chip in that order | — (the pair is one element, and it is the session-line reveal's anchor) | owner ruling 3, 2026-09-13 |
| `data-reveal="agent-repl-session"` + `data-began` / `.topbar-session-duration` | the connectivity glyph's dropdown (src/topbar/session.ts), anchored on the `.topbar-connectivity` glyph itself (`data-reveal-anchor="agent-repl-session"`), whose click stops at the glyph so the account cell's options do not also open; its rows are `.topbar-session-row`s (`.topbar-session-label`, `.topbar-session-value`) sharing the token breakdown's row rules | `data-began`: `login` \| `editorStart`; the duration row's value is `formatTickedAge` of the reader's now against `started_at_ms`, ticked by the shared ticker; it is the dropdown's only row (the vendor traffic row was removed, owner ruling 2026-10-06). A connectivity indicator carrying NO session binds nothing: the glyph is unmarked and its click opens the account options, as before | session-traffic, owner ruling 2026-10-06 |
| `data-reviving` + `.reviving` class | the sidebar `.ws` row (`data-reviving`) and its `.name` (`.reviving`), while the row carries `RosterRowReviving` | `true` — absent once the daemon drops the marker (the revival ended, success or failure). The name wears the subtle `ws-revive-shimmer` ripple, phase-continued across redraws by `REVIVE_SHIMMER_PERIOD_MS` (src/sidebar/reviving.ts), and stopped under reduced motion | owner ruling, 2026-09-19 |
| `data-role` / `data-variant` / `data-cap-lines` | every blue and purple `.bubble` (src/bubble/draw.ts) | role `prompt` \| `response`; variant `response` \| `thinking` \| `agentic` \| `compaction` \| `user` \| `agent` \| `peer` \| `held` (`turn-ended` retired 2026-10-06: a turn's ending is its outcome marker); cap `feed` \| `1` \| `0` \| `none` (`none` is `BUBBLE_UNCAPPED`: every non-thinking `response`, at full height, whose box wears `.bubble-box` alone; `2` retired 2026-09-27) | one-bubble, 2026-09-23; `none` 2026-09-27 |
| `data-more` | every CAPPED `.bubble` (src/bubble/draw.ts; absent on an uncapped one) | `fade` (the shared has-more bottom fade) \| `ellipsis` (a one-line cap only: the collapsed body is clamped to its line, which the engine ends in `…` exactly when anything follows it, and the fade is hidden) | held-prompt-quiet-one-line, 2026-09-27 |
| `.bubble-strip` class | every header-strip element of a bubble (a click on it toggles the bubble's scroll box) | — | one-bubble, 2026-09-23 |
| `.async-work-id` class + `data-work-id` | the last element of a detached subagent head (`.subagent-head`) and of every shell head (`.shell-head`), drawn by `src/feed/work-id.ts` | the daemon's detached-work id, verbatim (absent on a synchronous spawn) | footer-rows-and-work-ids, 2026-09-23 |
| `data-work-id` / `data-jump` / `data-jump-unresolved` | every footer detached-work row (`.footer-row-jump`: agents, shells, monitors) | `data-work-id` is `FooterWorkId.value`; exactly one of `data-jump` (the entry's FeedId: a subagent's bubble, a shell's head, a monitor's Monitor tool-call card) and `data-jump-unresolved` (`notDrawn`; `noFeedEntry` is retired, owner ruling 2026-09-23) | footer-rows-and-work-ids, 2026-09-23 |
| `.bubble-expand-only` class | every element of a bubble's expand-only region (`expandOnly` in src/bubble/draw.ts), after the scroll box | — (hidden while the sibling `.bubble-scroll` is not `.expanded`; a held prompt's one `.queued-details` element — queued age, rationale or failure detail, actions — wears it) | held-prompt-compact-badges, 2026-09-23 |
| `data-held-status` + `.held-badge` class | every status badge in a held prompt's header strip (a `.badge`) | the status: a `classification` arm, a `hold` arm, or `accepted`; its tone class comes from `HELD_STATUS_BADGES` (src/tray/held-prompt.ts), the one table | held-prompt-compact-badges, 2026-09-23 |
| `data-held-action="release"` | the held card's release control | `release` — the wire verb and the hook; the control is LABELLED "Send now" (`SEND_NOW_LABEL`, src/tray/held-prompt.ts), which is also its accessible name, and its forbidding-arm tooltips (`NO_RELEASE_TITLES`) say "send", never "release" | send-now-label, 2026-09-23 |
| `data-held-action="edit"` / `data-editing` / `data-held-status="editing"` | the held card's Edit control (between Send now and Cancel), the card while its entry carries `HeldPrompt.editing`, and that entry's `editing` status badge in the header strip | `edit`; `true` — absent when the daemon states no edit; the badge's tone is `HELD_STATUS_BADGES.editing` | edit-held-prompt, 2026-09-23 |
| `data-held-action="fold"` + `.queued-action-fold` class | the held card's "fold above" control (`FOLD_ABOVE_LABEL`), last in the actions row; drawn only while the entry carries `HeldPrompt.fold_above` | `fold` — the hook; the click sends `FoldHeldPrompt` with the entry's own turn and the served `fold_above.above`, and a refusal is the row's `.queued-refusal[data-arm]`, as Send now's is | held-fold-above, 2026-09-30 |
| `.entry-selected` class (with `data-revealed` / `data-selected-row` (valued `response`, `prompt` or `bubble`) on the row) | the CARD of the selected feed entry — the row's first element child (a bubble, a tool card, a detached bubble's fold), never the full-width `.feed-item` | — (the row states WHICH act selected it: `data-revealed` while a footer detached-work jump's landing stands, `data-selected-row` while the reply-to-a-past-response selection names it; `syncSelectedEntry` (src/feed/selected-entry.ts) derives the class from those facts, and the chrome mirror re-derives it after every body draw so a replaced card inherits it. The stylesheet draws an inset `--selected-response` outline, and on a final response turns its own border blue instead; the old `.row-revealed` bar and `.response-selected` class are gone) | selected-mark-on-card, 2026-09-23 |
| `data-selection-governed` / `data-selectable` on a root-feed row | a root-feed prompt or response row whose bubble the selection owns (`stampSelection`, src/feed/bubble-selection.ts); `data-selectable` while the daemon publishes `FeedRow.selectable` | a click selects it through `SelectFeedRow` `bubble` (or `clear` on the selected one); its box opens only while it is the selection (`expandSelected`, feed-view.ts), neither a click nor the auto-collapse toggles it, and every selected bubble kind wears the blue border (`.bubble.entry-selected`) | bubble selection, 2026-10-01 |
| `data-phase` + `.footer-activity-update` class | the footer activity line drawing a deploy's salient `FooterStatusActivityUpdate`, or the finished deploy's transient `FooterActivityTransientUpdated` | the phase arm's case name (`building`, `installing`, `restartingServices`, `handingOver`, `waiting`), or `updated` for the transient; the waiting counts wear `data-datum="count"` | deploy-progress-in-footer, 2026-09-27; `updated` a transient since fa-webapp |
| `data-tier` | the footer's `.footer-activity` cell (src/footer/activity.ts) | `salient` \| `transient` \| `enduring`: the tier DRAWN — the daemon's salient line, or, in the unpinned tiers, the transient while the client clock is before its `expiry.expires_at_ms`, then the enduring line. The cell's `data-arm` is then the salient or transient kind's case name, or `enduring` | fa-webapp (footer activity tiers); `quiet` retired with the quiet tier, 2026-10-01 |
| `.footer-activity-transient` + `data-datum="agent"` | a transient raised by a subagent's work: the line's wrapper, and the `.footer-activity-agent` label span in front of it (identity blue, `activityDatumClass("agent")`) | the subagent's label, verbatim; absent for the main agent | fa-webapp |
| `.footer-activity-enduring` + `data-line` | the enduring line (it inherits the retired `.footer-activity-rate-limited` layout: figures elastic); only each allowance's percentage is colored, by the percent gradient, the `.footer-rate-separator` "|" between allowances is blue (`--footer-rate-separator`) and bold, with exactly three no-break spaces on each side (`FOOTER_RATE_SEPARATOR_GAP`, drawn by `drawFooterRateDivider`), and no reading age is drawn | `data-line`: `usage` \| `unobserved` (drawn empty) | fa-webapp; one line since the combined model |
| `.footer-next-attempt` + `[data-countdown]` / `data-overdue` | the salient `retrying` line's next-attempt span: "next try in 12s", ticking off `FooterStatusActivityRetrying.next_attempt`, then "next try overdue by 2m" once the instant passes | `data-overdue` present exactly while the promised attempt is past due, so a stalled vendor reads as a stall; the line also names "of N" when `max_attempt` is set | retry countdown, 2026-09-30 |
| `.footer-activity-api-restored` | the `api_restored` transient: "API answering again after 8 failed attempts" | — | retry countdown, 2026-09-30 |
| `data-edge` + `.footer-activity-network-resume` | the `network_resume` transient's line | the edge's case name: `waiting` \| `resumed` \| `gaveUp` \| `abandoned`; `waiting` carries a `.footer-gives-up[data-countdown]` span | fa-webapp |
| `.footer-chip-waiting` + `data-waiting-for-api` | the agents chip's waiting-for-the-API glyph holder, whose `.footer-chip-glyph[data-glyph="waitingForApi"]` is ⧗ | the daemon's waiting count; absent when no agent waits | fa-webapp |
| `data-state` / `.footer-row-state` | every agents-panel row (`data-state`), and the waiting row's "waiting for the API · gives up in …" span | `running` \| `waitingForApi` | fa-webapp |
| `data-datum` on the failed-deploy overlay | each line of a `deployFailed` warning's detail overlay (src/topbar/warnings.ts) | `step` \| `component` \| `rollback` \| `detail` \| `log` (absent when the build archived no log); the `detail` line also wears `.topbar-warning-whole`, which keeps its line breaks. The first push carrying a failed deploy is logged at ERROR as `topbar.deploy-failed` | deploy-failure-every-client, 2026-09-28 |
| `data-local-arms` / `data-local` | the topbar's `.topbar-warnings` chip (`data-local-arms`), and each client-local row in its list (`data-local`, with `data-arm`) | the standing client-local `FailureKind` arm names, space-separated, first-filed first — absent when none stands; the `#failure-overlay` and its `[data-arm]` cards are GONE | owner ruling, 2026-09-23 |
| `data-merge-test-log` + `.merge-test-log-link` | the merge bubble tests tab's log link (`renderMergeTestLogLink`, src/link.ts), inside `.merge-test-log` | — (drawn in the response bubble's link blue, `var(--accent)`; its text is `FeedMergeTestLogLabel.text`; a click sends `OpenInEditor` with `target.merge_test_log` = the served token, which never appears in the markup) | merge queue rework, 2026-09-30 |
| `.footer-columns` / `.footer-row-main` / `.footer-column-header` + `data-column` | the agents panel's header and rows (`.footer-columns`, each exactly four cells sharing the section's grid through `subgrid`), a row's first cell (`.footer-row-main`: glyph, label, description, any wait, any "not on screen"), and the header's column headers | `data-column`: `tokens` \| `duration`; the header's first cell is the "stop all" control (the "live agents" title is gone), and the row's tokens figure carries no "tok" (the daemon dropped it); widths are measured in test/webkit/footer-columns.webkit.test.ts | footer columns, 2026-10-01 |
| `data-step` + `.footer-activity-merge-step` | the salient `merge_step` line under `merging` and `merge failed` (src/footer/merge-step.ts) | the step's case name: `enqueued` \| `preprocessing` \| `rebasing` \| `conflictResolution` \| `testing` \| `fixing` \| `committing` \| `updatingMain` \| `postprocessing`; a testing line also wears `data-edge` (`started` \| `passed` in `tone-green` \| `failed` in `tone-red`), a rebasing line `data-line` (`running` \| `failed`), an updating-main line `data-update-step` (`fetching` \| `fastForwarding`) | merge queue rework, 2026-09-30 |
| `data-merge-progress` / `data-merge-attempt` / `data-update-step` | the merge bubble's rebasing tab progress ("3/7"), a fixes tab's attempt ("attempt 2/3"), and the updating main tab's step (src/feed/merge/step-tabs.ts) | `data-update-step`: `fetching` \| `fastForwarding`; the other two carry no value | merge queue rework, 2026-09-30 |
| `.merge-queue-header` / `.merge-queue-stage` / `.merge-queue-duration` + `data-queue-place="current"` | the merge bubble's queue tab, a COLUMN TABLE sharing the agents panel's `.footer-columns` subgrid, `.footer-column-header` and duration column (src/columns.ts, src/feed/merge/queue.ts) | the header's `data-column`: `workspace` \| `stage` \| `duration`; each row is exactly three cells (`.merge-queue-label`, `.merge-queue-stage`: the front's active tab label or "waiting", `.merge-queue-duration`: a `.footer-row-clock` ticking from the entry's `stage_entered_at_ms`); "you are here" is gone and this workspace's own row (`data-queue-place="current"`) is subtly highlighted; widths and row heights are measured in test/webkit/merge-queue-columns.webkit.test.ts | merge bubble durations, 2026-10-01 |
| `.merge-tab-duration` | every merge tab badge, between `.merge-tab-label` and `.merge-tab-glyph` (src/feed/merge/tab-strip.ts) | — (a live tab ticks from its state's `started_at_ms`, a settled one shows `ended_at_ms - started_at_ms`; muted, never wrapping, measured in test/webkit/merge-tab-duration.webkit.test.ts) | merge bubble durations, 2026-10-01 |
| `data-component="news-digest"` + `data-news-digest-close` / `data-kind` / `data-effective` / `data-outcome` | the news digest overlay's host (fixed on `#feed-scroll`'s rectangle, src/news-digest/news-digest.ts), its close control, each `.news-digest-section`, an item's effective-date pill, and each `.news-digest-source` row | `data-kind`: the `NewsDigestSectionKind` arm (`backend` \| `deprecation` \| `policy` \| `feature` \| `release` \| `incident`; `backend` is drawn in the warning red); `data-outcome`: `read` \| `failed`; `data-week` (`risks` \| `quiet`) is the "Since last week" section, drawn FIRST, its risk items each carrying a `data-risk-reason` element under the summary and its quiet sentence a `data-week-quiet` element; `data-sdk-version` (`known` \| `unknown`) is the header's middle "SDK Version: …" element; the close control and Escape send `DismissNewsDigest` with the served id, which never appears in the markup, and only the daemon's `none` push takes the overlay down | news digest, 2026-10-02 |

## Commands

```
npm test                   the unit suites (vitest + jsdom)
npm run typecheck          tsc over src/ AND test/, integration suite included
npm run build              typecheck plus vite build
npm run test:integration   the whole app against a loopback fake daemon
npm run test:webkit        real headless WebKit over the real stylesheet (test/webkit/)
```

`npm run test:webkit` (`vitest.webkit.config.ts`) is the suite for behavior
jsdom cannot lay out, run in Playwright's headless WebKit, the Emacs webview's
engine. `playwright-core` is a pinned devDependency (1.48.2, whose WebKit build
is `webkit-2083`); a machine without that build fetches it once with
`npx playwright-core install webkit`. Each test bundles a page entry
(`test/webkit/*-page.ts`) into one classic script with vite and loads it with
`src/styles.css` through `setContent`, with no server and no network. It is out
of `npm test` because it takes seconds. Today it holds one file:
`anchoring.webkit.test.ts`, which scrolls a fresh 400-row feed up 120 steps and
asserts that no PAINTED frame moves the content under the reader. It samples in
a ResizeObserver created after the feed's own, so it reads the layout the
feed's corrections left, not a between-frames state no frame ever shows.

The test and coverage scripts (and their `pre*` hooks) run through
`../bin/background.sh`, at background priority; every vitest config imports
`../bin/require-background.mjs`, so a bare `npx vitest` refuses to start
(prefix it: `../bin/background.sh npx vitest run ...`). `build`, `dev`, `lint`
and `typecheck` stay at normal priority.

`bin/build-frontend.sh webapp` is what actually SHIPS a build: it writes
`dist/.built-sha` and `dist/.build-id` beside the artifact, and the build id is
what the webview URL carries as `&build=`, which is the only thing that defeats
a cached bundle. `npm run build` alone leaves those stamps stale, and a missing
`dist/.build-id` is a hard error at webview-mount time, not a degraded mode.

### Dependencies come from a SELF-HEALING shared store

`node_modules` is a symlink into ONE shared store entry per lockfile hash,
`$AGENT_REPL_NODE_STORE` (default `~/.cache/agent-repl/node-store`)
`/<name>-<lockhash>/node_modules`, made by `bin/build-frontend.sh deps` (and
every build target). The `pre*` hook of every test, typecheck, lint and
coverage script runs `bin/ensure-deps.sh` first. Nobody repairs an entry by
hand any more:

- An entry is judged by whether it SATISFIES ITS OWN LOCKFILE (`npm ls
  --depth=0` inside the entry), never by whether its directory exists. The
  2026-09-23 outage was an entry that existed but had been EMPTIED, which the
  old existence check linked every checkout to.
- A broken entry (empty or partial) is REPAIRED IN PLACE, by both
  `link_node_modules` and `ensure-deps.sh`, through the one repair path in
  `bin/lib-node-store.sh`:
  - under an exclusive per-entry lock, `mkdir <store>/.<entry>.lock` (flock is
    not on macOS); a second repairer waits, re-checks, and skips the entry the
    first one fixed; a dead holder's lock is broken, loudly;
  - into a fresh tree `<entry>/.trees/<id>/`, swapped in by renaming
    `<entry>/node_modules` (a symlink to the tree) atomically, so a concurrent
    reader of the link never sees a half tree;
  - announced on stderr with the `[node-store]` prefix.
- NOTHING EVER RUNS `npm ci` THROUGH A SYMLINK: that is what emptied the entry.
- `ensure-deps.sh` keeps the link when the repair works. It removes the link
  (never the entry) and installs a private `node_modules` only when the repair
  FAILS or the entry is whole but keyed by another lockfile, and says so loudly.
- `bin/test-build-frontend.sh` pins every one of those cases over a fake `npm`.

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
- **FOOTER TEXT IS FOR A HUMAN USER, NOT A DEVELOPER** (owner ruling,
  2026-09-29). Every status, substatus and activity line the footer draws is
  short, plain words a person reads: "enqueued 3/5", "conflict resolution",
  never `merge_conflict`, `awaitingTurnEnd` or any other camelCase,
  snake_case or identifier spelling. It carries only what matters to a user
  who is not an agent-repl developer, never debug detail or an internal
  mechanism (what gates a surface, which component holds what). The code
  underneath may spell things however suits it; what reaches the footer
  follows this rule.
- **A ROW IS PLACED BY ITS KEY, NEVER BY ARRIVAL** (owner ruling,
  2026-09-27: a late row lands where it would have been had it not been late;
  plan `docs/investigations/2026-09-27-feed-row-order-plan.md`). Every
  `FeedRow` a page or a push carries holds `order` (`FeedRowOrder.key`), the
  daemon's opaque place for it. `feed-view.ts` inserts every NEW row, page or
  push, at the index a binary search over the held rows' keys finds
  (`positionOf`; keys compare as JS strings, code unit by code unit, and are
  never parsed); `order.splice(index, 0, id)` in `insertAt` is the one place a
  row enters the order, and a source scan in `test/feed/feed-view.test.ts`
  fails any arrival-order index. A pushed row whose key sorts before every held
  row while the walk's edge is `has_more` is unloaded history: it is not drawn,
  and load-more brings it in place; at `at_start` it goes on top. A re-push
  never moves a held row: a changed key is ERROR `feed.row-order-changed` and
  the row stays put, and a key another row holds is ERROR
  `feed.row-order-duplicate`. A row (a removal included) with no `order` or an
  empty key is a `MalformedView`, and a page holding one is refused whole
  before any row changes. Every pushed placement is INFO `feed.row-placed`
  (`key`, `outcome` `inserted` | `appended` | `unloadedHistory`, `position`);
  a page is one INFO `feed.page-placed` (row count and key span) with each row
  at DEBUG `feed.page-row-placed`. A new row landing above the viewport keeps
  the reader still through `prependCompensation` (`feed.insert-kept-place`),
  and a following reader stays at the tail. Every fixture row carries a key
  (`test/feed-order.ts`: minted per id on first build, reset before each test;
  a test about order states its keys with `withOrder`).
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
- **A PLANNED ENDING IS NOT A FAULT.** The daemon, roster and host streams
  carry an `ending` arm (`DaemonStreamEnding`) a daemon standing down in a
  planned exit sends as a stream's last frame. `watchStream`'s
  `plannedEnding` recognizer (wired by the sidebar and the lifecycle's
  `WatchDaemon`) consumes the frame before `onPush`, and a run that then ends
  cleanly is logged at info (`rpc.stream-ended-planned`) and reopened at once
  with NO `daemonUnreachable` filed. A clean end without it, or an error after
  it, is the unreachable failure it always was. The daemon sends the frame on
  its dedicated rpcs only; a page mux subscription is not given one (the
  `WatchPage` stream has no ending arm), so today the page's own end still
  reads as the failure.
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
- **ONE VIEWED MODE.** `viewedMode` (src/sidebar/viewed.ts) decides every
  row's FULL/PARTIAL display mode, and nothing else reads `RosterRowViewed`. In
  PARTIAL the row's NAME greys to `--muted` and NOTHING else changes — the
  status dot keeps its tone. It reads the wire's marker and nothing else: the
  daemon derives the marker from the workspace's read-result fact in the same
  render that resolves the status, so a status change never overrides it (a
  read turn end returning from `idle_async` arrives PARTIAL on that very push).
  The module-root AGENTS.md section "The viewed mode" owns the cross-surface
  invariant.
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
- **ONE CONTROL, AND IT IS NEVER A `<button>`** (owner ruling, 2026-10-02:
  WebKit starts no text selection inside a `<button>`, and all text must be
  selectable). Every clickable control is built by `createControl`
  (src/control.ts): an `<ar-button>` with the `button` role, `tabindex`, Enter
  (keydown) and Space (keyup) activation, and a `disabled` property reflected
  as `aria-disabled="true"` that drops it from the tab order and refuses every
  click (DEBUG `control.click-refused`). Stylesheet rules name `ar-button`,
  a type selector like `button` was, and `[aria-disabled="true"]` in place of
  `:disabled`; the user agent's button look is restated once under
  `:where(ar-button)`, and the controls macOS drew as native push buttons are
  drawn to that bezel's geometry. `test/control.test.ts` fails any `<button>`
  built in `src` or named in the stylesheet, and
  `test/webkit/control.webkit.test.ts` holds a control's box to a button's.
- **A CLICK THAT ENDS A DRAG-SELECT IS NOT A CLICK** (owner ruling,
  2026-10-02: all text everywhere is selectable, and selecting it must not
  break the click targets it lies on). `installSelectionClickGuard`
  (src/selection.ts), installed once by `main.ts`, swallows at document
  capture every MOUSE click whose press-drag-release changed the selection
  into one holding text (DEBUG `selection.click-swallowed`); a click beside an
  unchanged old highlight, and every keyboard click (`detail` 0), go through.
  The selection's text is read through `selectedText` alone.
  `test/webkit/selection.webkit.test.ts` drags the real mouse in WebKit.
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
  A kind's chrome the reader should see only once the bubble is open goes in
  the spec's `expandOnly`, which the same toggle reveals; there is no second
  fold. A capped bubble's spec also chooses its MORE SIGNAL (`data-more`,
  2026-09-27): the shared fade, or the ELLIPSIS, drawn at a one-line cap only
  (the spec types refuse it elsewhere) — the collapsed body is clamped to its
  line (`-webkit-line-clamp`), so the engine ends that line in `…` exactly when
  anything follows it (a wrapped over-long line, a further line, a further
  block), and the fade is hidden; the measurer still sets `has-more`, reading
  the clamped body's `scrollHeight`, and nothing keys on it. A HELD prompt
  (owner spec, 2026-09-23) is HALF the one width
  (`calc(var(--bubble-max-width) / 2)`, so its trees wrap at the half), shows
  collapsed only its ONE first line under the ellipsis (owner ruling,
  2026-09-27) and its status badges, and keeps the rest expand-only; its fill
  is 5% of the `--held-prompt-bg` tint over the feed's `--bg`; each status badge's color comes from ONE table,
  `HELD_STATUS_BADGES`, and its words are the ones the card already said for
  that arm (the proto carries no status text), a hold's standing sentence
  included. A THINKING bubble is a FIXED height (owner request, 2026-10-07):
  arriving or landed it is under the feed cap (`THINKING_CAP_LINES`,
  `responseCap`, src/feed/cards/response.ts) and says "more" with the fade, so
  its own text landing never collapses it. The earlier half-line thinking fade
  and the one-line landed collapse are gone.
  A RESPONSE IS NEVER ABBREVIATED (owner request, 2026-09-27): every
  non-thinking response (arriving, interim pear, final green) and the
  ended-turn bubble is drawn `BUBBLE_UNCAPPED` (`data-cap-lines="none"`), the
  one bubble's first-class uncapped mode. Its box wears `.bubble-box` (the
  structural rules every box shares) but never `.bubble-scroll`, which every
  cap, clip, gutter, zoom cursor, fade, click toggle, carried fold and
  auto-collapse keys on, so it cannot be abbreviated or opened by construction;
  the spec types forbid `expandOnly` on it, a box never switches mode in place
  (`drawBubble` builds a fresh bubble), and `toggleSection` refuses at ERROR
  (`expand.toggle-uncapped`) anything that is not a capped section. Thinking
  bubbles, prompts, held prompts, peers, agentic cards and compaction
  summaries stay capped.
- **ONLY A JUMP'S EXPANSION CLOSES ON LEAVING THE VIEW; THE READER'S CLOSE AT
  THE TAIL** (owner rulings, 2026-10-01, replacing the wheel-armed close of
  2026-09-30). EVERY JUMP to a feed entry (a footer detached-work row, a
  breadcrumb, a hook's gated-call link: `reveal` in `src/feed/feed.ts`)
  EXPANDS the entry (its sub-feed bubble, else every capped section its row
  owns), then CENTERS it on the expanded layout, then marks it. An entry the
  jump expanded is watched (`src/feed/jump-collapse.ts`) and collapsed once NO
  PART of it is visible; one already open when the jump landed, or one the
  reader toggles by hand afterwards, is the reader's and is never watched. An
  entry the READER expanded does NOT close when scrolled out of view: it
  closes when the reader scrolls back to the tail so the follow re-latches
  (`TailFollow.onTailReached`, the one re-latch in `latchIfLatestVisible`,
  fired only for the reader's own scroll after the latest entry was out of
  view), which closes EVERY expanded entry once (INFO
  `feed.tail-reached-collapse`) and drops the jump watches. "Left the view" is
  ONE detector, `createLeftViewWatch` (`src/feed/left-view.ts`: seen first,
  then wholly out, once; a detached row is no departure), shared by the jump
  watch and the selection's `left_view` (selection-visibility.ts). Every
  expansion is client-owned but one: `FeedMergeFold` is the merge bubble's
  fold at its first draw and again whenever a push CHANGES it (folded until
  the merge fails, open once it has; owner ruling, 2026-10-08), and there is
  no daemon fold verb. A bubble the daemon will not
  open for a jump is ERROR `feed.jump-expand-failed` and a
  `controlPlaneFailed` on the chip.
- **ONE HEAT RULE FOR EVERY TOKEN FIGURE AND PERCENTAGE** (owner rulings,
  2026-09-30). A footer percentage is painted by `pressurePercentColor`
  (`src/pressure-color.ts`): green below 40%, yellow by 70%, orange by 90%, red
  from 90%, a continuous gradient between the stops (`src/percent-gradient.ts`).
  The topbar's context figure is painted by the SAME helper over the daemon's
  `TopbarContextChip.window_fill` (owner, 2026-10-01), inline so no stylesheet
  rule can override it; `test/pressure-color-call-sites.test.ts` fails either
  surface that stops calling it. The
  response bubble's token stamp and the footer's token count share
  `tokenHeatColor` (`src/token-heat.ts`) over the daemon's
  `frontend.v1.TokenHeat` position; neither is re-derived locally.
  `test/bubble/consolidation.test.ts` fails any bubble, box, body, wrap, paint,
  has-more or toggle logic built anywhere else. Three rulings of 2026-09-23 ride
  it: "more below" is the FADE (or the ellipsis a spec chooses), never a chevron, and `has-more` means the
  BODY's rendered lines run past the cap (never the box's `scrollHeight`, which
  counts the usage corner's hit area); a prompt's border lands once it is
  received and stays (the user's purple independent of the wave, one amber
  `--agent-prompt-border` for agent-addressed prompts and peer messages, none
  on a held prompt); every scroll box wears the SYSTEM default bar (no
  `::-webkit-scrollbar` rule anywhere) and the bubble scroll box declares
  `scrollbar-gutter: stable`, so its gutter is the system bar's width in both
  states; and a tree's column budget takes off that gutter as MEASURED on the
  `.bubble-box` (`offsetWidth - clientWidth - borders`), so it never
  re-wraps on a click (an uncapped box reserves no gutter and measures 0).
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
  A bubble's own scroll box (an expanded thinking, prompt, tool or async
  bubble) has NO implicit writer: it moves only on the reader's input. The
  feed moves implicitly only for the closed set `SCROLL_CAUSES` —
  `promptSent`, `promptHeld` (a held prompt's card drawn in the tray for the
  FIRST time parks the feed at its tail and follows, as a sent prompt does; a
  re-push or a removal moves nothing), `selectionMoved`, `detachedWorkSelected`,
  `entryJumped` (every other jump: a breadcrumb, a hook's gated-call link,
  centered by the same `revealCenterDelta`), `itemExpanded`, `initialPlacement`,
  `replaceRestore`, `workspaceSelected` (the roster's `current` moved to this
  page's workspace, by any switch path: `src/sidebar/selection-edge.ts` parks
  the feed at its tail and follows; returning to Emacs from another
  application is not a switch), `prependCompensation`, `latestVisible`
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
  `detachedWorkSelected` CENTERS the picked card in the feed's viewport
  (`revealCenterDelta`: midpoint onto midpoint, a card taller than the
  viewport top-aligned, clamped at the feed's edges); the walk opens only the
  containers selecting the row requires, and the landing then expands the
  entry itself (owner request, 2026-10-01). `itemExpanded` (owner request,
  2026-09-30, widening the bubble-only ruling of 2026-09-29) puts the vertical
  middle of ANY feed item the reader expands on the feed viewport's vertical
  middle at once (`expandCenterDelta`: midpoint onto midpoint whatever the
  item's height, clamped at the feed's edges), through the same
  `TailFollow.centerReveal`. The item is the expanded element's nearest feed
  row. A capped section the click owner toggles centers through its callback;
  an item owning its own fold (a sub-feed bubble)
  dispatches `ITEM_EXPANDED_EVENT` (`announceItemExpanded`, src/expand.ts) on
  the reader's toggle only — a reveal opening bubbles never announces. A
  collapse moves nothing.
  THE EXPANDED-ITEM CEILING (owner request, 2026-09-30): an expanded item that
  is neither a response nor a prompt (a tool or skill card, a subagent's,
  detached work's or merge bubble, a detached shell, a hook card, a standalone
  title) is never taller than `--feed-item-max-h`, 80% of `#feed-scroll`'s
  visible height (`80cqh` on its size container), declared ONCE there; its
  header keeps its height and its inner section gives way and scrolls.
  test/webkit/expanded-ceiling.webkit.test.ts measures it in real WebKit. `prependCompensation` also covers
  a bubble whose sub-feed lies wholly above the viewport collapsing (a
  negative shift). THE FEED OWNS ITS SCROLL ANCHORING (2026-09-27): WebKit has
  no native CSS scroll anchoring, and every `.feed-item` is
  `content-visibility: auto`, so a row above the reader changes height when it
  is first laid out. `TailFollow` holds one anchor, the first root row that
  starts in view (`feedAnchorRows`), at its top in content coordinates. On every
  size change and scroll event it measures that anchor BEFORE re-taking it, and
  shifts by however far it moved (`prependCompensation`). The prepend,
  sub-feed-collapse and thinking-collapse compensations are the same pass
  (`compensate`), and a tail-following reader holds no anchor. Each correction
  is DEBUG `scroll.anchor-corrected` (cause, trigger, delta, anchor). A height
  change off the tail with no row to anchor on is ERROR `scroll.anchor-missing`.
  `npm run test:webkit` is its regression test. The expanded footer's section is capped at
  `EXPANDED_FOOTER_MAX_ROWS` (4) and scrolls on its own; its scroll is the
  reader's, and a push redraws the rows INSIDE the kept section so it is never
  detached or reset.
- **A DETACHED-WORK ROW'S CLICK HAS EXACTLY ONE OUTCOME** (owner ruling,
  2026-09-23). Every agent, shell and monitor row in the expanded footer
  carries the daemon's `FooterJump`: `entry` selects that FeedId through the
  feed's `selectDetachedWork`, the one jump (expand, center, close again once
  wholly out of view); `unresolved` (and an entry the reveal could not
  land, an unreadable answer, or a throw) draws "not on screen" at the row and
  writes `footer.expanded.jump-unreachable` with `work_id`, `kind`, `feed_id`,
  `jump` and `reason`. The notice is the footer mount's state
  (`JumpNotices`), painted by every draw — never a mark on the clicked
  element, which the next whole-view push throws away. A MONITOR's entry is
  its Monitor call's ordinary tool-call card (owner ruling, 2026-09-23): the
  webapp draws it as any tool card and holds no monitor knowledge; the click
  centers it (`detachedWorkSelected`) and rings it with `.entry-selected`.
- **A COLLAPSED BUBBLE HOLDS NOTHING** (owner ruling, 2026-09-23). Expanding a
  subagent or async bubble paints the sub-feed's newest `OpenFeed` page and
  tails it; collapsing disposes the child controller, its rows and the
  bubble's composer, and the next expansion starts from a fresh page.
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
- **AN OPEN SECTION CLOSES WHEN THE READER LEAVES IT** (2026-09-27; scroll
  triggers replaced 2026-10-01). The one `AutoCollapse` owner in
  `src/expand.ts` (one per document; every `installClickExpand` host registers
  with it) closes every open capped section through the same `collapseSection`
  a click uses, on: the reader's return to the tail (`tailReached`, called by
  the feed), the window's own `blur`, or `visibilitychange` to hidden. A wheel
  or a scrollbar grab no longer closes anything, and it never listens to
  `scroll`. Each close is a DEBUG `expand.auto-collapse` with `trigger` and
  `kind`.
  Known gap: a keyboard-only Emacs window or workspace switch made while the
  WKWebView still holds first responder fires no DOM signal at all.
  Its twin: A WHEEL INSIDE AN OPEN SECTION NEVER MOVES THE FEED. The open box
  wears `overscroll-behavior: contain`, and `installIntentScroll` (scroll.ts),
  told which section is open by `expandedSectionAt`, never redirects that
  wheel and consumes it (`preventDefault`) once no box up to the section can
  move further (`sectionTakesDelta`). A collapsed box's wheel is the feed's.
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
- **THE FOOTER'S ACTIVITY SECTION IS ALWAYS EXACTLY ONE LINE.** A line longer
  than the cell is truncated with an ellipsis (`.pfooter-cell` never wraps,
  `.pfooter-grow` hides its overflow behind `text-overflow: ellipsis`), and the
  footer, barring its expanded section, never grows in height for it. The
  cell's hover title carries the whole line. `test/styles.test.ts` pins both
  declarations.
- **THE FOOTER'S ACTIVITY CELL DRAWS THREE TIERS.** Salient, transient,
  enduring, under every status. The quiet tier (a line worded from the feed
  item that landed last, held until the next row was painted) is retired
  (owner ruling, 2026-10-01), with its hold and the feed's paint tracking.
  `test/footer/activity.test.ts`, "the unpinned tiers under working and
  background".
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
npm run test:webkit      # real headless WebKit: scroll anchoring (see Commands)
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
| `COLD_BOOT_TIMEOUT_MS` (`bootColdOnce`, `test/integration/harness.ts`) | 900ms (the hook bound) | 1800ms | 602ms at a load average of ~60 (971ms at 100-300) | a file's FIRST app boot compiles the whole app lazily, ~3-4x a warm boot; it is paid in a `beforeAll` so no test body carries it |
| `COLD_BOOT_TIMEOUT_MS` (`beforeAll`, `test/main.test.ts`) | 5000ms (the file's test bound, paid by its first test) | 15000ms | 3.15s over 20 runs at a load average of ~112; 5.59s once on the contended host | the first `import("../src/main.js")` in a worker fetches and compiles the whole graph, ~10x a warm import; paid once before any test |
| `BOOT_TIMEOUT_MS` (`test/main.test.ts`, each test and its `afterEach`) | 5000ms | 4500ms | 536ms at ~112; 1.45s on the contended host | each test re-evaluates the whole mocked graph (`vi.resetModules`), because `main.ts` boots at import time; `afterEach` awaits a boot still in flight so none outlives its test |
| webkit `hookTimeout` (`vitest.webkit.config.ts`) | 10000ms (default) | 90000ms | 29.4s (bundle, launch and the 120-step pass, all in `beforeAll`) | a real browser pass; the tests only read its result, so `testTimeout` stays 850ms |
| `SETTLE_ROUND_CAP` (`test/integration/harness.ts`) | 60 rounds | 60 rounds (unchanged) | 24 rounds (also in `refusals.integration.test.ts`) | already a ~2.5x margin; the 3x rule would ask for 72, which is looser than the current cap, so it stays — a bound is never loosened to fit a formula |

**Every integration file that boots the app calls `bootColdOnce()` at its top
level.** The first boot in an isolated file is its cold start, and under load
it alone crossed the 900ms `testTimeout`, failing exactly the first test of
each file. The helper pays it in a `beforeAll` under its own measured bound
(above); `harness.self.test.ts` fails any file that calls `startHarness`
without it.

Apart from those cold boots and `test/main.test.ts`'s per-test boots, no per-site exception was needed: nothing in either suite (xterm/login
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
