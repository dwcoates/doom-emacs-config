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

## Nuked (filled in at execution)

The precise inventory of removed code lands here with the removal commit, so
the doc records what the footer replaced.
