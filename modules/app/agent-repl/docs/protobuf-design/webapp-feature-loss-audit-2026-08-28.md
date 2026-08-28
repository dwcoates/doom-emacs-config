# Webapp feature-loss audit against `frontend.v1` — 2026-08-28

## Method

1. Read every `.proto` under `proto/src/frontend/v1/` in full (agents_panel,
   context_panel, daemon_hold, failure, feed, footer, help_panel, mcp_panel,
   sidebar, status_panel, todos_panel, topbar — 4,715 lines), plus every
   `agentrepl/v1` endpoint the webapp calls and the `service.proto` rpc list
   (39 rpcs), plus `conversation/v1/user.proto`, `api.proto` (ModelOption),
   and `workspace/v1/workspace.proto`.
2. Read `docs/protobuf-design/digests/webapp.md` in full (876 lines) and
   grep-verified every candidate against `figma-to-idl-redesign.md`,
   `.deferred.md` and `.vetting.md` before calling it loss.
3. Walked `webapp/src/` exhaustively — all 97 source modules, via three
   parallel read-only enumeration passes (render/bubble family; shell:
   sidebar/topbar/footer/commands/main; state/transport/pagers) plus direct
   reads of the modules whose behavior was load-bearing for a finding.

## Evidence tier

**Documentary.** Every finding cites a webapp `file:line` for the behavior
and a named absence in the new contract (no message, no field, no rpc).
Every finding was checked against the design record and the digest for a
recorded deletion or relocation; where the record *does* dispose of a
neighbouring concern, that is stated inside the finding. Confidence labels
distinguish "the contract provably has no home" (high) from "a home may exist
by generic reuse, but nothing in the record says so" (medium/low).

Scope caveat: this audit is about REPRESENTABILITY in the contract. Purely
client-side mechanics (scrolling, folding, copy chords, smoothing, coalescing)
are out of scope by the design's own "rendering is the webapp's" rule and are
listed in the closing coverage section.

---

## Findings

### 1. The system-failure card has no drawn home — `FailureKind` is referenced by nothing

- **Feature**: the feed's failure card and the client's own connectivity
  cards: "lost the connection to the daemon; reconnecting", "the daemon closed
  the connection", "this workspace no longer exists on the daemon", "a message
  from the daemon could not be read and was skipped", "this page cannot read
  the daemon's state… restart the view", "the interface could not start", and
  the daemon-side death cards (session deleted / superseded / shim died /
  resume failed) with their open → resolved → terminal lifecycle and
  reconnect-time retraction.
- **Evidence**: `webapp/src/local-failure.ts:111-250` (six client-minted
  cards), `webapp/src/failure-card.ts:50-166` (side→color, message + detail +
  `resolved at` stamp, connectivity cards vanish on reconnect rather than
  settling), `webapp/src/render.ts:2400-2495` (card drawn in the feed with
  typed resume/query-termination evidence).
- **What is missing**: `frontend/v1/failure.proto` declares `FailureKind` with
  all 17 arms including the six CLIENT-LOCAL ones, but **no component message
  and no endpoint anywhere references `FailureKind`, `FailureCardRef` or any
  lifecycle** — grep across `proto/src` returns hits only inside
  `failure.proto` itself. The old `FailureCardView` (message + detail +
  lifecycle) has no successor: `TopbarWarningStrip`'s `detail` oneof carries
  only accounting / unmodeled_tool / detached_unmodeled / session_fault /
  degraded_window, and `FooterStatus` is daemon-pushed, so it cannot draw the
  one class of failure that exists precisely when no push can arrive.
- **Confidence**: high.

### 2. The permission-mode picker and the ungated-session banner are unrepresentable

- **Feature**: the topbar's permission-mode `<select>` (default / acceptEdits /
  plan / auto / manual / dontAsk), the held pick that survives until the daemon
  reports the mode in force, and the permanent "NO PERMISSION GATE —
  bypassPermissions: every tool call runs immediately" banner.
- **Evidence**: `webapp/src/main.ts:842` (`mode-select`), `main.ts:2354-2361`
  (pick held, applied by the next prompt), `webapp/src/pending-mode.ts:16-37`,
  `webapp/src/ungated.ts:43,87-130` (banner + `.ungated` body class),
  `webapp/src/status.ts:75` (`/status` prints the mode).
- **What is missing**: `TopbarView` has title, session line, model selector,
  connectivity, warnings, token breakdown — no permission-mode element; the
  agentrepl service has `SetModel` but **no SetPermissionMode** (only
  `shim/v1/endpoint_set_session_permission_mode.proto`, which the webapp
  cannot call), and no view carries the mode in force. The record disposes of
  `/permissions` as a dropped PANEL and mentions permission mode only as a
  session-side value inside an echoed standing — never as a webapp control.
- **Confidence**: high.

### 3. Account identity and account switching leave the contract entirely

- **Feature**: the topbar account chip ("email (personal|work)", red "logged
  out"), and its menu: `re-auth <email>`, and `switch to <root> (<who>)` for
  every other `CLAUDE_CONFIG_DIR`, which migrates the transcript and bounces
  the shim under the new root keeping the session id.
- **Evidence**: `webapp/src/account.ts:15,51-64,104-129,140-197`,
  `webapp/src/main.ts:2401-2413,2522-2580`, `webapp/src/status.ts:66-75`.
- **What is missing**: nothing in `frontend.v1` carries an account, an email
  or a config root (`config_dir` exists only on
  `agentrepl.v1.HostVendorClaude`, inside Emacs's `WatchHostWorkspace`, which
  is explicitly not the webapp's stream), and there is no verb to switch or
  re-auth an account. Grep of the whole design record for "account switch",
  "email", "CLAUDE_CONFIG" returns nothing about this surface.
- **Confidence**: high.

### 4. The login terminal (pty) has no transport

- **Feature**: the full-screen xterm.js login overlay — the daemon owns a pty
  running the vendor's OAuth TUI, the webapp renders its bytes and forwards
  keystrokes; closing it kills the child. Deliberately never scraped.
- **Evidence**: `webapp/src/login.ts:1-23,26-47,64-100` (open/close,
  "login terminal open" / "login failed to open" / "login closed"),
  `webapp/src/login-terminal.ts:59-136` (raw byte in/out, resize),
  `webapp/src/main.ts:2465-2492`.
- **What is missing**: no bidirectional byte stream and no login verb anywhere
  in `agentrepl.v1`; `frontend.v1` has no terminal component. The record
  classifies `/login` only as a NO-PANEL "act/flow command"
  (`figma-to-idl-redesign.md:358-360`), which disposes of the *panel*, not of
  the existing pty surface.
- **Confidence**: high.

### 5. The webview cannot learn its composer is gated (merge lease / drain / restart / parked)

- **Feature**: the composer is blocked with an explanation *before* a prompt is
  spent — "this workspace is being merged — the merge is driving the session,
  so prompting is blocked until it finishes", with live merge progress appended
  to the notice and to the disabled Send tooltip.
- **Evidence**: `webapp/src/merge-gate.ts:14-19,37-61,72-117`,
  `webapp/src/main.ts:1183-1198,2303-2318` (draft preserved, notice re-asserted).
- **What is missing**: the composer gate exists only as
  `HostSessionLive.composer` (open | merging | draining | restarting |
  merge_parked) on **`WatchHostWorkspace`**, whose header says Emacs renders
  it; the webapp's own `WatchWebWorkspace` carries a single arm, `transferred
  { address }`, and its header states it is "NOT DRAWN". `SubmitPrompt`'s
  `merging` refusal arm tells the webapp only *after* the prompt is spent —
  exactly the failure mode `merge-gate.ts` exists to prevent.
- **Confidence**: high.

### 6. Incremental search over the feed (isearch) has no contract support

- **Feature**: `C-s`/`C-r`/`C-g`/`RET` isearch driven from the composer, with
  `I-search: q [3/12]` status line, case folding, matches revealed inside
  height-capped sections and `display:none` folds, "(3 unopened folds not
  searched)" honesty, and an Emacs host hook that echoes the status line.
- **Evidence**: `webapp/src/search.ts:16-31,151-181,355-376,543-562,823-835`;
  `webapp/src/lazy-item.ts:125-165` deliberately keeps a deferred item's text
  in its placeholder *so search still finds it*.
- **What is missing**: search across a feed is now inherently server-side —
  rows arrive by page (`GetFeedPage`, no cursor, no page size), subagent and
  merge bubbles are separate sub-feeds opened on expand, and settled bubbles
  transfer only a collapsed head. There is no search verb, no match address,
  and no way to reach text the client has never been sent. Nothing in the
  record mentions search.
- **Confidence**: high.

### 7. The sidebar's task-grouping management actions have no verbs

- **Feature**: in the Tasks view — create a task (`+ New task`), toggle a
  task's done checkbox, open the task's org notes, and add a workspace to a
  task; plus the per-section `(n)` counts and done styling.
- **Evidence**: `webapp/src/sidebar.ts:584-613,681-687,963-1021` (POSTs
  `task-create`, `task-toggle-done`, `task-open`, `task-add-workspace`),
  old wire arms `HostAction.taskCreate/taskToggleDone/taskOpen/taskAddWorkspace`
  (`webapp/src/frontend-proto.ts:1664`).
- **What is missing**: `frontend/v1/sidebar.proto` *draws* the task grouping
  (`RosterTaskSection`, `RosterTaskSectionHeader`, `RosterTaskDone`) but the
  agentrepl service has no task verb at all — the sidebar section is
  `CreateWorkspace / Open / Close / Merge / Restart / Kill / Nuke / Select`.
  The record deletes the daemon→Emacs command loop (`ReportHostAction` never
  exists) and moves fold/grouping preferences webview-local; it never
  disposes of task *management*, which was carried on that same loop.
- **Confidence**: high (drawn but unactionable is itself the evidence).

### 8. Version-skew detection loses its signal: no daemon build identity, no webapp reload push

- **Feature**: a bundle loaded once into a long-lived xwidget reloads itself
  when the daemon is redeployed underneath it, and rescues itself from the
  permanent "reconnecting" card via a stale-bundle path with a bounded reload
  ceiling and a loud card when reloading does not help.
- **Evidence**: `webapp/src/version-skew.ts:179,237-261,280` (compares
  `DaemonBuild{version, binaryMtimeMs}` from the connect snapshot),
  `webapp/src/main.ts:1528-1539`.
- **What is missing**: the old `DaemonView{bootId, protocolVersion,
  daemonBinaryMtimeMs, daemonVersion}` (`frontend-proto.ts:735`) has no
  successor — `DaemonHealth` returns only healthy/unhealthy+faults, and
  `WatchDaemon` pushes only `shutdown_announced{address}`. Emacs gets
  `HostWorkspaceReloadWebapp` on its stream; the webapp's `WatchWebWorkspace`
  has no reload arm. `FailureStaleBundle` exists in `failure.proto` but,
  per finding 1, nothing draws it and nothing supplies the evidence to mint it.
- **Confidence**: high.

### 9. Opening a link in the system browser has no verb

- **Feature**: every `http(s)` anchor in the feed is intercepted at click time
  and handed to the daemon, which launches the external browser — because a
  navigation inside the WKWebView replaces the conversation with a web page.
- **Evidence**: `webapp/src/external-link.ts:1-104`, installed at
  `webapp/src/main.ts:181-184`.
- **What is missing**: the new contract mandates "all URLs are clickable"
  (artifact URLs, tool-call input links, WebSearch link rows) but provides no
  verb to open one — there is no `OpenExternal`/browser rpc, and grep of the
  design record for "external browser" returns nothing. Under the new
  contract a click is either a same-webview navigation or dead.
- **Confidence**: high.

### 10. The unsupported-command escape hatch (build support for this command) is gone

- **Feature**: when the headless CLI refuses an interactive-only slash command,
  the feed renders the refusal with a button that asks the daemon to have Emacs
  open a workspace which builds the feature, with `Asking Emacs…` / `workspace
  requested` / `Asking failed: …` states.
- **Evidence**: `webapp/src/unsupported.ts:33-77` (`POST
  /sessions/<id>/add-support`), `webapp/src/render.ts:2005-2037`.
- **What is missing**: no verb, and no row kind for the refusal. The record
  drops `/doctor`, `/hooks`, `/release-notes`, `/export`, `/memory`,
  `/permissions` as panels — which *increases* the number of commands that
  will fall through as ordinary prompts — but never disposes of the
  build-the-missing-feature affordance that today catches them.
- **Confidence**: medium-high.

### 11. Interrupt no longer asks before killing live subagents

- **Feature**: an interrupt that would stop live detached work returns a typed
  challenge ("interrupt needs confirmation: N live tasks"), and the second
  keystroke sends `confirmAgents: true`.
- **Evidence**: `webapp/src/command-dispatch.ts:228-233,919-925`,
  `webapp/src/frontend-command.ts:73-76`,
  `webapp/src/frontend-proto.ts:1286` (`InterruptConfirmRequired{liveTasks}`).
- **What is missing**: `InterruptRequest` is `{ workspace; target turn |
  detached }` with no confirmation field, and `InterruptSuccess` has arms
  `interrupted_turn | interrupted_detached{count} | nothing_running` — no
  `confirmation_required` answer. `InterruptError` is empty-on-purpose
  ("arms DERIVED at the wave"), so this could land there, but nothing records
  the intent and a refusal arm is not the same shape as a confirm-and-retry.
- **Confidence**: medium-high.

### 12. The chess-board widget channel has no home

- **Feature**: a marker line in a response renders as an interactive chess
  board inside the bubble (PGN / FEN / live engine session), with prose
  flowing around it, board state surviving re-renders, keyboard stepping from
  Emacs, and an actionable error when the widget is not installed.
- **Evidence**: `webapp/src/chess-game.ts:23-59,155-156,203-218,317-434`
  (payload fetched from `/sessions/{id}/chess-game`).
- **What is missing**: `FeedResponse` carries `prose` markdown only; the new
  contract bans client-side derivation from prose ("the client DRAWS, never
  COMPARES"), has no embedded-widget block in the drawn-block vocabulary
  (`text | image | unsupported`), and no side-channel for the payload file.
  "chess" appears nowhere in the design record.
- **Confidence**: medium-high (a client could still string-match the marker,
  but the payload fetch has no route and the practice is contra the rules).

### 13. The gns-sockets bridge fold is unrepresentable, and it takes the green border with it

- **Feature**: a session with a live Slack-thread subscription respawns a
  `sockets-listener` bridge from its Stop hook; that upkeep is folded into the
  final response above it, which thereby regains the answering-response
  border it would otherwise lose to the respawn.
- **Evidence**: `webapp/src/gns.ts:1-42,122-215`,
  `webapp/src/render.ts:1664-1675` (`📡 gns-sockets bridge · N steps · …`).
- **What is missing**: no way to mark a row as plumbing, and no nesting
  mechanism for it — `parent` is presentation nesting the *daemon* assigns,
  and a subagent bubble always draws as a bubble on its parent feed. The
  border half is genuinely fixed by `FeedTurnEnded.concluded.answer` (a
  recorded improvement), but the *hiding* of bridge upkeep has no successor
  and "gns"/"sockets-listener" appear nowhere in the record.
- **Confidence**: medium.

### 14. Host-injected ("meta") spans will be drawn to the user

- **Feature**: Emacs prepends a metaprompt read-directive, an
  autonomous-execution preamble and a wrap-up gate to certain sends, bracketed
  with inert markers; the agent receives all of it, the reader sees only their
  own words.
- **Evidence**: `webapp/src/meta.ts:1-43`, `webapp/src/turn.ts:22-33`.
- **What is missing**: `FeedUserPrompt` is daemon-resolved blocks drawn
  verbatim, and `UserSaid` carries no provenance — tag 2 is RETIRED with
  "prompt provenance is deferred" (`conversation/v1/user.proto`). The deferral
  is recorded for provenance *metadata*; nothing records that the harness's
  injected text should be stripped before drawing, so under the new contract
  it lands in the bubble unless the daemon silently strips it.
- **Confidence**: medium.

### 15. The global drain banner loses its content

- **Feature**: a non-dismissible chrome banner above the central column:
  "Daemon bounce scheduled — <cause> · draining <elapsed>", the enumerated
  holds it is waiting on ("workspace — turn in flight, 2 live tasks"), and
  whether sessions survive the bounce.
- **Evidence**: `webapp/src/drain.ts:52-131`,
  `webapp/src/frontend-proto.ts:1802` (`ShutdownScheduleDraining{scheduleId,
  scheduledAtMs, cause, stopShims, holds[]}`).
- **What is missing**: `UpdateShutdownSchedule` is inbound-only and its header
  states the consequences arrive on existing surfaces (tray + footer). Those
  surfaces show a hold only *after* the user submits a prompt and only for
  their own workspace; the standing daemon-wide schedule, its cause, its
  deadline, its hold list and `stop_shims` have no field anywhere.
- **Confidence**: low-medium (an explicit relocation is recorded; the
  relocation is lossy rather than absent).

### 16. Per-bubble age and per-response duration are no longer shippable

- **Feature**: a bubble's hover timestamp ("5m 30s ago", live) and the turn
  stats stamp in the bubble corner ("5s · 63.5k in").
- **Evidence**: `webapp/src/render.ts:414-433,2219-2244`.
- **What is missing**: no `FeedRow` and no row kind carries a creation instant
  — the only instants are `FeedTurnEnded.ended_at_ms`, bubble
  `started_at_ms`/`ended_at_ms` on subagent/shell, and answer-time stamps on
  cards. `FeedResponseUsageStamp` restores the token half; the age and
  duration halves have no field, and the clock convention ("clocks tick
  client-side from shipped instants") cannot help without an instant.
- **Confidence**: medium.

---

## Checked and NOT findings

Verified covered by the new contract, or deliberately deleted/relocated by the
design record (so the review can trust coverage):

**Recorded deletions / relocations (digest §3, §4, §6, §10)** — hibernation and
the revival gate (`hibernation.ts` whole, teal, `FooterStatusAsleep`,
`RosterRowStatusHibernated`); tasks in the feed (footer ☑ chip + checklist
only); thinking in the feed (footer tokens panel only); unmodeled tools in the
feed (topbar warning dropdown); the unmodeled footer chip; workflows
(deferred wholesale); fences, `revision`/`boot_id`, the rosterFromFrame
staleness check; the when-column precedence code; client-side activity
precedence over ProgressView windows; `asyncShape`/`classifyAsyncSource`; the
3-tier identity ladder and offset-append machinery; the optimistic-row and
pending-ack map; `finalResponses` border assignment (now
`FeedTurnEnded.concluded.answer`); the separate `BubbleTyping` line; shell
spool paging; keepalive frames (retracted convention); `highlight.ts` and its
twenty grammars (moved daemon-side, with the grammar-width risk recorded).

**Covered by the new contract** — feed paging and load-more (`OpenFeed` /
`GetFeedPage` `has_more | at_start`, positionless by design); subagent and
merge bubbles as sub-feeds (`OpenFeed(FeedId)` → `WatchFeed`); the per-agent
composer inside a bubble (`SubmitPrompt.feed`); the Stop button on detached
work (`Interrupt.target.detached`); footer jump targets for agents and shells
(`FeedId target`); the model picker with per-option descriptions
(`TopbarModelSelector` + `conversation.v1.ModelOption`); the token breakdown
menu (nested in `TopbarView`, always populated); the turn tokens cell, alarm
and accounting verdict with evidence lines; rate-limit rungs
(`FooterAllowance` session + weekly); the wakeup countdown (footer
`waiting · wakeup`); monitors and crons (footer chips + panels); permission
cards incl. always-allow, policy denial and abandonment; question cards incl.
expiry and the always-drawn free-text escape; the cold-context gate (its own
row, data-not-prose by ruling); clear/compaction dividers with before/after
tokens, summary fold and cold-read notice; worktree dividers; skill cards with
SKILL.md and allowances; artifact, plan (with the ✎ host-raised edit) and
findings bubbles with host-raised locations; hook failures; merge phases as a
tabbed sub-feed with the ahead/current/behind queue snapshot; merge
pause/resume/evict (`UpdateMergeQueue`); the merge-dequeue offer
(`HeldOfferMergeDequeue`, daemon-composed sentence); held prompts with
classification, rationale, accept and the four hold arms
(`DaemonHoldTray`); `/status`, `/todos`, `/agents`, `/mcp`, `/context`,
`/help` panels (`SubmitPrompt.command_panel`); daemon-intercepted commands
(recognition is the daemon's, transparently); client log forwarding
(`ClientLog`); the roster's five-color dot vocabulary, merge glyph statuses,
inactive `?`, closed/receded rows, nesting, when-column, detail lines and the
attention blink cadence; graceful daemon handover (`WatchWebWorkspace` +
`AdoptWebWorkspace`), which subsumes `restart-window.ts`'s purpose.

**Client-local by the design's own rule (no contract needed)** — edge-gated
scrolling and tail-following; scroll-position preservation across rebuilds;
click-to-expand capped sections; folds and label caps; `C-c`/`y` copy chords;
`C-S-j/k` bubble navigation; smooth type-out pacing; per-frame render
coalescing; lazy placeholder items; breathing/animation; markdown rendering,
tables, task lists; metaprompt tree layout; the offline prompt-queue buffer
(now backed by `SubmitPrompt.idempotency_key`); background recovery, the
recovery probe, reconnect backoff and the resync machinery (the old seq/fence
protocol they serve is nuked, not lost); `catalogue.html` (a developer
gallery, not a product surface); Emacs host hooks (text scale, park-at-tail,
close-menus, recover-now).
