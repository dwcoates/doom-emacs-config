# Design: the consolidated progress footer

One footer element in the webapp GUI replaces every scattered in-flight
indicator. It is fed by a single new daemon-resolved frame, `ProgressView`,
which consolidates the progress facts that today are either scattered across
frames or stranded with no frontend representation at all.

## Decisions (settled with the user)

- **Nuke, don't evolve.** The existing indicator code is removed wholesale and
  the footer is built from scratch:
  - the `thinking…` / `working…` / `retrying…` tail rows and `pulseTarget`
    (`webapp/src/render.ts:283-346` and the render sites that consume them);
  - the duration and token widgets they orbit (exact file map produced at
    execution time, then recorded here).
- **One frame.** A single `frontend.ProgressView` message carries the footer's
  entire input: phase mirror, tickers, activity windows, counts. Extensible to
  tasks/workspace detail later. No constellation of per-signal frames.
- **No Emacs component.** The footer is webapp-only. Emacs keeps its existing
  tab-bar/sidebar render of `WorkspaceState` and gains nothing new here.
- **No output tokens in the footer.** The token cell shows the CURRENT
  TURN's cumulative input tokens (cached + uncached) while thinking/working,
  alongside the thinking ticker. The running output-token figure is
  explicitly unwanted, so the `message_delta` usage relay is out of the
  footer's scope entirely.
- **Ephemeral counters live in the footer; session-wide figures in the
  topbar.** The topbar's tasks/agents counter menus RELOCATE into the
  footer: tasks and subagents live and die within a session, so they are
  ephemeral state and belong beside the rest of it. Session-scoped token
  usage (the tokens chip and its per-model breakdown overlay) STAYS in the
  topbar.
- **Placement and shape (user constraints):** a raised, flat bubble docked at
  the bottom of the GUI window, visually akin to the sidebar; thin by default
  (tall enough for a line or two); sits below the streaming response cards; no
  wider than the widest response bubble (the green-bordered purple-background
  assistant bubble). UI design beyond these constraints is delegated (see
  §UI design).

## ProgressView (proto sketch)

Daemon-resolved, latest-wins per workspace, pushed on change and included in
`StateSnapshot`. Frontends render it; they never re-derive any of it.

```protobuf
message ProgressView {
  string workspace = 1;
  string session_id = 2;

  // Phase mirror: the SSM's resolved state, repeated here so the footer has
  // one self-sufficient input. Never re-derived frontend-side.
  RenderState state = 3;

  // Turn clock: 0 = no turn in flight.
  int64 turn_started_at_ms = 4;

  // Live tickers (latest-wins). Deliberately NO output-token ticker
  // (decision: input tokens only).
  int64 thinking_tokens = 5;   // data.ThinkingTokens.estimated_tokens
  int64 input_tokens = 6;      // this turn's cumulative cached+uncached input
  int64 ttft_ms = 7;           // first-token latency of the current message

  // Activity windows: open until cleared. Each is its own message so
  // window-specific detail has a home.
  message Window {
    bool active = 1;
    int64 since_ms = 2;
    string detail = 3;         // hook name, auth prompt line, ...
  }
  Window compacting = 8;        // data.StatusMessage status="compacting"/null
  Window retrying = 9;          // ApiErrorLine retry family
  Window authenticating = 10;   // data.AuthStatus
  Window hook = 11;             // data.HookStarted / HookResponse
  message RateLimitWindow {
    bool active = 1;
    int64 resets_at = 2;        // epoch seconds
    double utilization = 3;
    string status = 4;          // e.g. "allowed_warning"
  }
  RateLimitWindow rate_limited = 12;  // data.RateLimitEvent

  // Error state: persists until the next turn starts.
  string error_summary = 13;    // TurnEnded.is_error / ApiErrorLine summary

  // Counts:
  int64 pending_permissions = 14;
  int64 queue_depth = 15;
  int64 live_task_count = 16;   // richer task detail deferred, see §Deferred
}
```

Sourcing notes:

- The daemon's progress resolver is a sibling of the SSM: it folds the same
  store event stream (plus the ephemeral relays) into the latest `ProgressView`
  and pushes on change. High-frequency tickers coalesce daemon-side so the
  frame rate stays sane.
- `ThinkingTokens`, `StatusMessage`, `RateLimitEvent`, `AuthStatus`,
  `HookStarted`/`HookResponse` are today vendor-only events that die in
  `conversationItemsFromVendor` (`daemon/internal/frontend/translate.go`);
  the resolver is their first consumer.
- `ttft_ms` dies earlier — the shim's delta bypass forwards only
  content-bearing arms (`streamEventToContentDelta` returns null for
  structural frames), so the shim grows a minimal relay for it. The
  `message_delta` cumulative output usage stays dropped on purpose: the
  footer wants input tokens only.
- `input_tokens` is TURN-scoped, not session-scoped: the resolver sums each
  API request's input usage (`input_tokens` + `cache_read_input_tokens` +
  `cache_creation_input_tokens`) across the current turn and resets the sum
  at turn start. Session-wide and per-model figures are topbar territory and
  never appear here.

## UI design (delegated; honors the constraints above)

- **Dock:** a raised flat bubble centered on the feed column, bottom of the
  window, matching the feed's max bubble width. Rounded corners and the same
  elevation treatment as the sidebar so it reads as chrome, not content.
- **Default (collapsed) strip, one line, zones left→right:**
  1. *Phase*: colored glyph + word from `state` (thinking / done / permission /
     merging / degraded...), the footer's anchor.
  2. *Activity detail*: whichever window or tool activity is live — running
     tool name with elapsed (from `HeartbeatView`), `compacting…`,
     `retrying…`, `rate-limited until HH:MM`, hook name, auth prompt.
     Windows take an accent color each; empty when nothing is live.
  3. *Turn clock*: elapsed since `turn_started_at_ms`, ticking via slot PAINT
     (the `TaskTimer` discipline — never a full re-render per second).
  4. *Token cell*: the current turn's cached+uncached input tokens, with the
     thinking ticker beside it during reasoning. No output-token figure.
  5. *Counters cluster*: the tasks/agents rosters relocated from the topbar
     (chip + disclosure each), plus queue depth and pending permissions
     badges. Hidden at zero.
- **Error line:** when `error_summary` is set, a second line in red accent with
  the summary; persists until the next turn starts; click scrolls the feed to
  the corresponding `ErrorItem`.
- **Expansion:** clicking the strip (or a keyboard toggle) grows the bubble
  upward into a short detail sheet — per-window detail rows now; the task
  roster and per-model token breakdown when their deferred work lands.
- **Idle:** the strip stays visible with a quiet low-opacity summary of the
  last turn (duration + tokens), replacing the nuked bottom-right widgets'
  role.

## NOT to be implemented: task telemetry (reference note only)

Decision: the task-telemetry bundle below is NOT planned work. Do not
implement any of it, as part of the footer or otherwise — the note exists
purely for future reference. (Distinct from the counter RELOCATION decided
above, which IS in the footer's scope: the rosters move as-is; they do not
gain the telemetry below.)

- `core.TaskProgress` is stranded: `BuildTaskCatalog`
  (`daemon/internal/frontend/translate.go:407`) has no arm for it and
  `TaskEntry` has no progress fields (`bytes_observed` / `records_observed` /
  `last_progress_at_ms` to add).
- `data.TaskNotificationMsg.TaskUsage` (total_tokens / tool_uses /
  duration_ms at task end) is stranded — only status/summary survive into
  `TaskEntry`.
- `data.BackgroundTasksChanged` is persisted vendor-only with no twin
  (`shim src/proto/convert.ts:24` note) and dropped by the daemon. It is the
  only full-live-set snapshot, so it should be folded into the SSM and
  `BuildTaskCatalog` as an authoritative reconciliation input — the sweep for
  the `IMPOSSIBLE live_task_count=-N` ghost class.
- A per-task progress pulse on the relocated rosters (fed by the items
  above) would be the eventual render, were this ever revisited.

## Nuked (recorded at execution)

The precise inventory of removed code, with line ranges as they stood at
removal time (i.e. against the commit the nuke was applied to).

### `webapp/src/render.ts` (3600 → 3238 lines)

Line ranges below are against `fc38125d~1` (the commit the nuke was applied to).

| symbol | kind | lines at removal |
|---|---|---|
| `interruptingIndicatorHtml` | exported fn | 263–280 |
| `thinkingRowHtml` | exported fn | 282–301 |
| `workingRowHtml` | exported fn | 303–323 |
| `retryingRowHtml` | exported fn | 325–344 |
| `PulseTarget` | exported type | 1865–1886 |
| `pulseTarget` | exported fn | 1888–1974 |
| `isPulsed` | exported fn | 1976–1980 |
| `tailStatusRow` | exported fn | 1982–2013 |
| `turnStatsRowHtml` | exported fn | 2020–2047 |
| `tailLineHtml` | exported fn | 2049–2075 |
| `FeedRenderer.tailSlot` | private field | 2656–2663 |
| `FeedRenderer.tailHtml` | private field | 2664–2670 |
| `FeedRenderer.turnTimerLabel` | private field | 2689–2695 |
| `FeedRenderer.applyPulse` | private method | 3160–3178 |
| `FeedRenderer.renderTailLine` | private method | 3389–3403 |
| `FeedRenderer.paintTurnTimer` | public method | 3405–3417 |

Plus the call sites: the `tailSlot` constructor parameter, the two
`const pulse = pulseTarget(...)` bindings (`renderRestored` and `render`), the
two `this.renderTailLine(tailLineHtml(...))` calls, and the two
`this.applyPulse(...)` calls.

`formatTokenDelta` (2015–2018) was deleted with the block and RESTORED: the
settled response's corner (`resultMeta`) and the result chip still use it. Its
neighbours in that contiguous run were all footer-replaced; it was not.

KEPT deliberately: `showsMonitoringRow` and `FeedRenderer.isMonitoring` /
`monitoring`. They take booleans, never the pulse, and feed the SIDEBAR's amber
dot — a different surface the footer does not replace.

### `webapp/src/styles.css`

Removed rules: `.interrupting-pending`, `.interrupting-spinner`,
`.working-pending`, `.retrying-pending`, `.retrying-spinner`,
`.turn-stats-live` (+ its two `.info-*` children), `.tail-line`, `#tail-slot`,
`#tail-slot:empty`, `.bubble.pulsing`, `@keyframes bubble-breathe`, and the
reduced-motion overrides naming `.interrupting-spinner`, `.retrying-spinner`
and `.bubble.pulsing`.

Removed palette tokens (both themes): `--user-pulse`, `--assistant-pulse`, and
the `--pulse-to` bindings on `.bubble.user` / `.bubble.assistant`.

Narrowed rather than removed: the two grouped `.thinking-pending` rules (the
ellipsis-gap cancel and the left-rail flush) lost their `working` / `retrying`
/ `interrupting` siblings. `.thinking-pending` and `.thinking-spinner` SURVIVE
— `chess-game.ts` renders its own `processing…` row with them.

### `webapp/index.html`

`#tail-slot` replaced by `#progress-footer`, the footer's own slot in the same
position (flex sibling between `#feed` and the composer).

### `webapp/src/main.ts`

The `tailSlotEl` lookup and its constructor argument; the `TaskTimer` paint
callback repointed from `feed.paintTurnTimer` to `footer.paintTurnTimer`.

### Tests removed with their subjects

`webapp/test/render.test.ts` (6316 → 5368 lines): the `interruptingIndicatorHtml`,
`workingRowHtml`, `thinkingRowHtml`, `retryingRowHtml`, `turnStatsRowHtml`,
`isPulsed`, `tailStatusRow`, `tailLineHtml` describes; the three `pulseTarget`
describes; `FeedRenderer: the tail line pins to the bottom slot, not the feed`;
and `FeedRenderer: the working-frontier breath is a class, not HTML`.

`webapp/test/styles.test.ts`: `the bubble pulse`, `live turn-stats row`,
`bottom-pinned tail slot (#tail-slot)`, `combined tail line`,
`tail status rows flush the response column's left rail`,
`interrupting indicator`, `working indicator`, and the two
`describe.each` palette suites (`$role pulse palette`,
`$role breath is perceptible`). The `retrying indicator` describe was reduced
to its two surviving `--retry` palette assertions, which the footer's own
retry accent still depends on.

## Unfed seams (schema present, no live source)

Recorded so the gap is a known one rather than a silent zero.

- **`ttft_ms`** — genuinely unfed. `ResultMessage.ttft_ms` exists but arrives
  only at TURN END, which is useless to a live footer, and the mid-turn
  `StreamEvent.ttft_ms` is unreachable: `stream_event` is ephemeral, so
  `convert()` never runs on it, and the delta bypass returns null for every
  non-`content_block_delta` frame. Feeding it needs a minimal shim relay —
  OUT OF SCOPE here (the shim was a hard boundary for this work). The footer's
  sheet omits the row entirely rather than printing a zero.
- **`authenticating`** — WIRED but unwitnessed. `data.AuthStatus` is converted
  and would forward, but no corpus fixture exists and the CLI has not been
  observed emitting it. The resolver's arm is real, not a placeholder.

Everything else the doc's sketch named IS fed: `thinking_tokens`, the
`compacting` window, the `retrying` / `error_summary` split out of the
`ApiErrorLine` family, the `hook` window (`HookStarted` / `HookResponse`), and
the `rate_limited` window (`RateLimitEvent`) all reach the daemon today as
vendor events that previously died in `conversationItemsFromVendor`.

### Proto deltas from the sketch above

- `Window` and `RateLimitWindow` are TOP-LEVEL messages (`ProgressWindow`,
  `RateLimitWindow`) rather than nested, for cleaner generated names in both
  Go and TS. Field numbers and semantics are exactly as sketched.
- `error_item_uuid = 17` was added: the design's error row scrolls the feed to
  the corresponding item, which needs the item's address. `ErrorItem` /
  `RetryItem` grew a matching `uuid` in the webapp store.

### Retry sourcing after the SDK 0.3.220 upgrade

The retrying window now has TWO sources describing the same backoff on
different planes, with a strict precedence between them:

- **`data.ApiRetry`** (stream `system/api_retry`) is AUTHORITATIVE. It carries
  what the transcript twin cannot — the backoff delay and the HTTP status — so
  its detail reads `attempt 3/10 · next in 8s · 529`. A connection error that
  never got a response has no status to print (`error_status_set` false), so
  the detail names the typed error family instead (`server error`); a bare `0`
  would read as a status.
- **`data.ApiErrorLine`** (transcript `system/api_error`) is the FALLBACK for
  the window and remains the SOLE terminal error record. It opens the window
  only when no `api_retry` has spoken for it (`retryDetailRich`), because the
  disk twin generally lands second and would otherwise overwrite the richer
  live detail with its own poorer one. When retries are EXHAUSTED it still
  closes the window and sets `error_summary` — `api_retry` never reports a
  terminal failure, so that half is unchanged.

`retryDetailRich` clears whenever the window closes (terminal error, turn
start, turn end), so the fallback speaks again on the next turn and a session
whose plane or CLI version emits no `api_retry` at all is never left with a
shut window.

### `session_state_changed`: a window, deliberately NOT a phase

`data.SessionStateChanged` drives EXACTLY ONE thing: the new
`ProgressView.blocked` window (`requires_action` opens it, `idle`/`running`
close it, anything else is loud-logged and changes nothing).

It is deliberately NOT wired to `ProgressView.state`. The SSM remains THE
phase authority, and two independent phase sources is precisely the drift the
SSM exists to prevent — a test pins this: an `idle` report while the SSM has
resolved `THINKING` leaves the phase untouched.

What it genuinely adds is a fact the daemon cannot otherwise see. The daemon
counts the permission prompts IT parked (`pending_permissions`), but a session
can block on an interaction the daemon holds no count for, so that count alone
under-reports "waiting on you". `requires_action` is the session's own report
of exactly that.

In the footer's activity cell it sits ABOVE the retry/compaction/hook/tool band
("nothing will happen until you act" outranks any account of the agent being
busy) and BELOW auth and rate limits (the same statement with a specific
remedy attached).

### Per-model token usage: no new surface was needed

The tokens overlay's per-model sections rendered as dashes because the store
marked `resultUsage`/`modelUsage` as GAPs on the premise that `frontend.v1`
carries no `model_usage` map. That premise was wrong.

`translate.go`'s `resultItems` passes the typed `ResultMessage` through
UNCHANGED into the `result` conversation item, and `frontend-proto` adopts an
item's payload opaquely, so `modelUsage` has been arriving intact the whole
time — the webapp adapter simply never read it. The fix was to read it: no
proto field, no daemon change.

Both figures are session-CUMULATIVE snapshots the SDK recomputes per result,
so a landed result REPLACES rather than accumulates, and `turnUsage` clears
with the baseline so a request already folded into it is never counted twice.
An ABSENT map leaves the standing one alone (the SDK declined to itemize this
result); an EMPTY map is a real, different answer and is adopted.

Per-model figures stay TOPBAR territory by the settled decision — nothing
per-model appears in the footer.

### Counter relocation (executed)

The topbar's `sessionTopbarDatapoints` now returns empty `agents` / `tasks`,
so the header strip renders no roster chips at all; it keeps the tokens chip,
the model picker, and the remaining session datapoints. The rosters render in
the footer's counters cluster through the SAME `counter-menu` facade
(`agentsMenuHtml` / `tasksMenuHtml`) — the chips, rows, dots, and reveal
behavior are unchanged, and the only adaptation is CSS: `.pfooter-counters`
flips each overlay UPWARD (`bottom: calc(100% + 6px)`), since a dock at the
window's bottom has nowhere to drop.

The AGENT-scoped strip (`agentTopbarDatapoints`, rendered inside subagent
bubbles) keeps its own rosters: a bubble's direct children are about that
bubble, not about the session, so they never belonged in the relocation.

Disclosure ownership split accordingly: `main.ts` keeps a `tokensMenuOpen`
boolean for the header, the `ProgressFooter` owns its own roster + sheet
disclosure, and `FeedRenderer` still owns each bubble's. All three dismiss
together through one `closeAllMenus`.

### Turn-start reset signal

`input_tokens` / `thinking_tokens` / `ttft_ms` reset, and `error_summary`
clears, on the FIRST of either signal to arrive:

- `NoteTurnAccepted`, fired when the daemon accepts a prompt for immediate
  submission (`sessiondrv.Manager.SubmitPrompt`, non-queued path). This is the
  earliest turn start the daemon actually observes today, since live
  `TurnStarted` events do not currently reach it (a separate known defect).
- a `TurnStarted` store event, once those flow again.

The open is idempotent (guarded by the resolver's own `turnOpen`), so whichever
arrives second is a no-op and the accumulated figures survive it. A QUEUED
prompt deliberately does not fire it: the turn it would report is the one
already running.
