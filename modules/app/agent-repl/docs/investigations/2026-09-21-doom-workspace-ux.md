# Investigation notes: the `doom` workspace session of 2026-09-21 (13:50–14:42 EDT)

Status: investigation only. Nothing here is fixed. Each item states what is
CONFIRMED (read in code or logs), what is INFERRED, and what is OPEN.

Evidence base: `bin/logs.sh --workspace doom`, `--central`, `--all`; the code at
master `54a53fa1e`; `docs/overhaul/daemon.md`; the one-shot agent's transcript.

---

## 1. A deploy kills every running turn

### What the design says (`docs/overhaul/daemon.md`, item 9 and item 10)

- Item 9, ruled 2026-08-28: "teardown NEVER interrupts the vendor — graceful
  shutdown means waiting for freeness".
- Item 10, the rollout controller: a daemon change is a BLUE-GREEN HANDOVER.
  - The old daemon spawns the new one in joining mode.
  - Each workspace transfers at FREENESS (no in-flight turn, no live detached
    work); the shim keeps running and is adopted, never killed.
  - A never-free workspace waits forever, with a periodic warning.
  - Intake is held during the transfer and replayed in order on the new daemon.
- A shim change is a per-workspace prelaunch + relaunch, also at freeness.
- A webapp change is a `reload_webapp` push against the same daemon.
- Store and sidecar are deliberately unhandled (a user-initiated full restart).

### What the code does

- CONFIRMED: the handover is implemented — `daemon/internal/rollout`
  (`controller.Handover`, `adopt.go`, `relaunch.go`, `manifest.go`).
- CONFIRMED: its ONLY production trigger is `rollout.Trigger`, called from
  `daemon/internal/merge/terminal.go:516` — i.e. only when a change lands
  through the daemon's own merge orchestrator on the self repository.
- CONFIRMED: `bin/deploy-all.sh` never reaches it. Step 5 evals
  `(agent-repl-runtime-restart-await)`, which calls
  `agent-repl-frontend-daemon-stop` (`lisp/daemon.el:1749`), which sends
  `UpdateShutdownSchedule{now}`.
- CONFIRMED: `now` is `drain.controller.ShutdownNow`
  (`daemon/internal/drain/controller.go:286`). Its own doc comment: "takes no
  lease and waits for no freeness … THIS IS THE ONE PLACE THE DRAIN FORCES".
  It force-stands-down every session (`KillSession` forced), sweeps in-flight
  spawns, and exits. The announcement carries no successor address, so every
  client reads a plain bounce.
- CONFIRMED in logs: three such restarts at 14:18:28, 14:29:11, 14:39:14, each
  preceded by `agent-repl.backend-restart-beginning`, each ending with
  `daemon.shimclient.kill force=true` and the footer going
  interrupted → disconnected → idle.
- CONFIRMED: `deploy-all.sh` also kickstarts the store and boots out the
  sidecar UNDER running shims (14:18:26, 14:29:08: `connect ENOENT store.sock`,
  absorbed by the shim's read retry — but a running turn rides through a store
  outage).

### The gap, stated once

The lead's landing flow (git merge on master + `deploy-all.sh`) is not the flow
the rollout controller was built for (the merge orchestrator's self-reload). So
every lead deploy takes the forced `now` path, and the freeness-gated handover
never runs. The design is faithful in code; it is simply never invoked by the
path we actually use.

### Consequences observed downstream (items 6, 8 and part of 3 hang off this)

- Turn `fe105fa390ef44d3` and turn `c517a2df6bd74c05` concluded as "interrupted
  by host shutdown".
- The detached subagent `a32bd327461d99385` (`fix/replay-and-terminals`) was
  stopped at 14:18:28 and recorded as "a person stopped the detached run".
- Emacs posted "Agent ready: doom" after each kill, as if the turn finished.

### OPEN

- Whether `deploy-all.sh` should call into the rollout controller (a new
  "roll out what is on disk" trigger that is not a merge), or schedule a
  freeness-gated drain (`UpdateShutdownSchedule{schedule}` exists and DOES wait)
  and restart after it fires.
- Store/sidecar bounces have no freeness story at all, by ruling. A deploy that
  touches them currently cannot be imperceptible.

---

## 2. Every turn replays a 200-entry page; so does every detached-agent reopen

- CONFIRMED: `daemon/internal/workspace/sender.go:92` builds `StartTurnRequest`
  with `PageSize: turnPageSize` (200) and NO `KnownThrough`.
- CONFIRMED: the shim (`engine/turn.ts:374`, `openingPage`) answers
  `readFirstPage(agent, pageSize, knownThrough)` — with no pointer that is the
  newest 200 entries of the book, every turn.
- CONFIRMED: the watcher (`sessionwatcher/watcher.go`, `OnTurnOpened`) feeds the
  whole page through `routeOpeningPageLocked(w.main, page)` — the same path as a
  watch's catch-up page. The feed re-resolves all 200 entries.
- CONFIRMED in logs: every `shim.engine.turn … page_entries:200` is followed in
  the same millisecond by re-fired history effects: `clear_confirmed`,
  `delivery_bound_moved` (the 13:52, 13:57, 13:58 moves are the SAME divider
  re-applied), `subagent_without_start` for the same unit, ~6–35
  `detached_unknown_unit`, and 10–15 webapp `draw-response` + `draw-turn-ended`.
- CONFIRMED: the watcher already tracks a per-agent pointer
  (`w.known[mainWatchKey]`, used by `WatchAgent` at `watcher.go:933`). StartTurn
  simply does not carry it.
- CONFIRMED second source: a live-work change re-opens a detached agent's
  `WatchAgent` from nothing. At 14:17:21 the footer's live-work set "added"
  `agent:toolu_01Fe8…` (already known since 14:00), the sub-feed re-placed 133
  rows, retired 12, and the expanded bubble redrew 223 responses.

Owner rule to hold the fix to: history is replayed ONLY when a workspace is
opened or a transcript is selected (`SPC j c`), and only the first page.

### OPEN

- StartTurn's page exists for R15 ("a fresh session's opening page holds exactly
  the prompt row"). With a correct `KnownThrough` the page is the prompt row
  alone; whether the page should exist at all is a contract question.
- Why the live-work reconciliation drops and re-adds an agent it already
  watches (the "dropped shell / added agent" churn in `live_work_taken`).

---

## 3. A tool card can say "running…" when nothing is running

### How the state is derived (CONFIRMED)

- A tool card's outcome is a pure function of THE LAST FRAME OF ITS OWN UNIT
  (`resolve/feed/toolcall.go`: `Start`/`Progress` → `runningOutcome`,
  `Success`/`Failure` → `returnedOutcome`).
- The turn's terminal (`turnended.go`, `drawTerminal`) settles the TURN row only.
  It does not visit the turn's units.
- So "unit is running" and "turn is over" are two independent facts with no
  invariant joining them, in the daemon or the shim. The webapp draws what it is
  given; it is not the source of the disagreement.

### Who is supposed to close the units (CONFIRMED)

- The shim, and only on ONE path: `convert/fold.ts:284–292`. When the vendor's
  `result` is a user stop (`aborted_streaming` / `aborted_tools`),
  `cutOpenCalls` writes a terminal for every open call ahead of the turn's
  terminal. The comment there states the invariant exactly: "a unit left on its
  running arm draws a live tool inside a turn that has ended".

### The paths that skip it (CONFIRMED)

- `engine/session.ts:3220` `writeHostShutdownTerminal` — a turn concluded by
  `KillSession` (every deploy, item 1) writes the turn terminal alone. No
  `cutOpenCalls`. Only detached SHELL RUNS are closed ("closed the shell runs
  this teardown stopped").
- Any end with no vendor `result` at all: shim force-killed, vendor child death,
  the 1000 ms "vendor message loop did not end within its budget" stand-down at
  14:39:14.
- Any non-stop terminal (`api_error`, `max_turns`, execution error) that lands
  while a call is open: `isUserStop` is false, so nothing is cut.

### The 308 "unknown unit" records are a different thing (CONFIRMED)

- `daemon.feed.detached_unknown_unit` (232) and
  `sessionwatcher.detached_kind_unknown` (76) name the same ~20 units over and
  over, in lockstep with each 200-entry replay.
- They are window artifacts of item 2: the detach announcement falls inside the
  200-entry window and the unit's own call falls outside it. They draw nothing
  and leave no stuck card. Fixing item 2 removes them. `detached_kind_unknown`
  is logged at ERROR for it.

### OPEN

- Where the invariant belongs. Candidates: (a) the shim cuts open calls on EVERY
  turn conclusion, whatever ended it; (b) the daemon's feed settles every open
  unit of a turn when it draws that turn's terminal — which also covers a shim
  that died before writing anything. (b) is the only one that holds when the
  shim is SIGKILLed; (a) keeps the record itself truthful. Probably both, with
  (b) the structural guarantee.
- The interrupted footer substatus (`FooterStatusIdle.interrupted`, proto
  already committed on `fix/replay-and-terminals`) belongs to the same fix.

---

## 4. A queued prompt interrupted the running turn; the owner never asked for that

- CONFIRMED: every prompt submitted while a turn runs is held AND judged
  (`promptqueue/classify.go`): `judge` asks `deps.Judge.Judge(runningText,
  heldText)`; on `verdict.Interject` it calls `interject`.
- CONFIRMED: `interject` sets the footer to waiting/interrupting and sends
  `KillTurn(running, force=false)`. Delivery of the interrupting prompt then
  waits for the killed turn's real end.
- 14:01:53 — the verdict was `interject` (4.7 s after the hold). The shim refused:
  "turn f957… spawned 1 live item(s); pass force to end them" (a background
  subagent). `stripJump` stamped the held prompt `classification_error`, the
  webapp warned `tray.held-prompt.classification-error`, and the prompt waited
  ~2.5 min for the turn to end naturally.
- 14:28:35 — a second verdict of `interject` SUCCEEDED: turn `9fcab85d18a64c9d`
  was killed (`aborted_tools`) 3 s after the owner's next prompt was submitted.
  That killed turn was the lead's own investigation of the "running…" bubbles.
- CONFIRMED logging gap: the verdict and its REASON are recorded at DEBUG
  (`opClassify … "recorded the verdict"`), and durable sinks are INFO. Why the
  classifier chose to interject is not recoverable from disk.

### Owner's stated intent

- Sending a prompt while a turn runs QUEUES it. It does not interrupt.
- An explicit interrupt = interrupt the turn, then send the interrupting prompt
  to the shim immediately. How the prior turn and the new prompt relate is the
  agent's/SDK's business, not ours.

### OPEN

- Whether the classifier has any remaining role (it is
  `prompts/queue-routing-classifier.md` + `internal/classifier`). Under the
  stated intent its `interject` arm has none.
- An explicit interrupt against a turn with live detached work: `force=false`
  is refused today. The owner's rule implies the interrupt ends the TURN and
  leaves detached work alone — which is a third thing the shim's KillTurn does
  not offer (it is all-or-refuse).
- `classification_error` is the wrong arm for "the interrupt was refused".

---

## 5. The "conversation may be missing" card at 14:00:26

- CONFIRMED provenance: the daemon sends a `FeedTextScale` frame on EVERY feed
  watch the instant its tail is accepted, sub-feeds included
  (`server/feed.go`: "THE FEED TEXT ZOOM RIDES EVERY FEED'S WATCH … the topic
  replays its latest value").
- CONFIRMED: the root feed routes the non-row frames first
  (`webapp/src/feed/feed.ts:212–220`). The sub-feed handler does not:
  `webapp/src/feed/bubble.ts:382` is
  `child?.upsert(requireMessage(response.row, "WatchFeedResponse.row"))`.
- So every time an expanded bubble opens its watch, the first frame is a
  well-formed text-scale push that the bubble rejects as "a non-optional message
  field is unset" → `rpc.stream-frame-undecodable` → the `frameUndecodable`
  failure card.
- Sequence in logs: bubble expanded 14:00:08 (`kind: detachedSubagent`); its
  page painted 14:00:26.495–.499; the watch opened and the card filed at
  14:00:26.505–.510.
- What pre-empted it: `clearClientFailure("frame_undecodable_card")` runs on the
  next frame ANY stream decodes (`webapp/src/rpc/streams.ts`, "A FRAME DECODED …
  disproves the undecodable verdict"). That was 14:00:27.474.
- No conversation was actually lost. The card is false, and it will fire on
  every bubble expand. The bubble also never applies the zoom.
- Logging gap: the undecodable record names neither the feed nor the frame's
  populated arm.

---

## 6. No webapp records after 14:35:50 — INCONCLUSIVE

- CONFIRMED: `doom` was the selected workspace 14:36:00–14:38:44 and again from
  14:38:44. The 14:38:52 turn placed 4 rows; the 14:39 restart replayed 167; the
  webview was repointed at 14:39:18.602 (three `load-changed`).
- CONFIRMED: the page DID run after the 14:39 restart — the daemon logged its
  `AdoptWebWorkspace` call at 14:39:18.693.
- Yet there is no `main.boot`, no mount, no draw, from ANY workspace's webapp
  after 14:35:50 except eight `forward-skew` lines.
- So the logs cannot separate "the page was blank/frozen" from "the webapp's log
  forwarding stopped". Both the rendering and the evidence of rendering travel
  the same browser → daemon path, and the daemon records nothing at INFO about
  accepting or dropping forwarded webapp records, nor about `OpenFeed` /
  `WatchFeed` being opened.
- This is a logging defect to fix before the question can be answered:
  daemon-side INFO records for page attach, `OpenFeed`, `WatchFeed` open/close,
  and webapp-log intake.

---

## 7. `/compact` left no divider

- CONFIRMED: the shim holds the compaction boundary until "the very next
  assistant prose", which it takes to BE the summary
  (`convert/fold.ts:401–436`, `settleCompaction`).
- CONFIRMED against the vendor transcript: the summary rides a USER record
  (`type:"user"`, `isCompactSummary:true` — 19 such records in this
  conversation), not an assistant message. The sidecar reads it correctly:
  `context-compacted trigger="manual" tokens 508470->9428 coalesced with its
  summary (29542 characters)` at 14:37:43.
- So the shim's release condition never matches the real shape. After the
  compaction, the next turn's assistant messages were tool calls with no prose —
  four `the cut is still held` warnings at 14:38:59–14:39:03 — and the shim was
  killed at 14:39:14 with the cut still pending. It was lost with the process.
- Worse than missing: had one of those assistant messages carried prose, the
  shim would have filed the agent's ordinary reply as the compaction summary.
- CONFIRMED: no `separation|context_cut:compact` row was placed live, and none
  in the 167-row replay after the restart — so the sidecar's store row did not
  become a feed divider either.

### OPEN

- Whether the SDK streams the `isCompactSummary` user message to the shim at
  all, or only `compact_boundary`. If it does not, the summary is only available
  from the transcript (the sidecar's plane).
- Why the sidecar's `context-compacted` record did not surface as a divider on
  replay (agent id on that record is the vendor session `6a1b0e3a…`; the
  workspace's book is `9a632c97…`).

---

## 8. `/model fable` refused, shown as an outage — NOT a bounce artifact

- CONFIRMED: the shim refused `SetSessionModel`: `"fable" is not in this
  session's model catalog` (the session's catalog held 5 models; vendor binary
  2.1.220).
- CONFIRMED: the daemon has the typed arm in mind but not on the wire:
  `daemon.refusal.unlanded_arm — intended arm: SubmitPromptError
  .model_not_in_catalog`. With no proto arm it went out as HTTP 400
  `failed_precondition`.
- CONFIRMED: Emacs therefore read a domain refusal as a TRANSPORT failure
  (`elisp.input.transport-failure`) and held the prompt as `kind=:outage`
  (`elisp.prompt-queue.held … depth=1`). The user saw nothing happen, and a held
  "outage" prompt now sits in the queue waiting for a link that was never down.
- The restart 53 s earlier is coincidental. The defect is the unlanded arm, plus
  Emacs treating every non-2xx as an outage.

---

## 9. `mutationProgress` "forward-compat skew" — NOT skew, and harmless

- I reported this as "the webapp build was older than the daemon". That was
  wrong.
- CONFIRMED: `mutationProgress` appears nowhere in `webapp/src`. It is a
  `WatchDaemon` push meant for Emacs (mutation progress for create/open/bind).
  The webapp has no drawer for it by design, and `streams.ts` labels ANY arm it
  has no drawer for as "a newer arm … this build cannot draw (forward-compat
  skew)".
- No UX consequence. The message is misleading; an arm the webapp deliberately
  ignores should be on an explicit ignore list, so that real skew stays
  distinguishable.

---

## 10. The `glimmer-intensity-boost` one-shot never merged

- Design (CONFIRMED, `workspace/oneshot.go`, ruling 2026-09-12): "THERE IS NO
  FINISH ACTION" — the daemon never merges a one-shot. The agent carries out
  `prompts/oneshot-completion-directive.md` itself by invoking the
  `/create-or-update-workspace` merge skill.
- CONFIRMED: the agent did. It committed `ff168d5fa`, then at 13:55:26 ran
  `~/.claude/skills/create-or-update-workspace/run.sh --emit-commands` with
  `{"type":"merge","workspace":"glimmer-intensity-boost","project_dir":"…/doom-worktrees/glimmer-intensity-boost"}`.
  Exit 0.
- CONFIRMED: the daemon refused it 2 s later and QUARANTINED the file
  (`~/.claude-emacs/output/quarantine/workspace_commands_AB6118DB-….json`):
  `daemon.commandfile.entry — unknown_workspace: no workspace
  "glimmer-intensity-boost" is registered`.
- CONFIRMED root cause — a contract mismatch between the skill and the ingress:
  - `commandfile/ingress.go:317` `target` passes `entry.Workspace` as
    `WorkspaceRef.Id`. The skill writes the workspace NAME there. The real id is
    `db6c528bba4043b4`.
  - `commandfile/entry.go` reads the directory from `dir`. The skill writes
    `project_dir`, which the ingress ignores.
  - Had the entry carried `dir` and no `workspace`, `WorkspaceByDir` would have
    resolved it.
- CONFIRMED: nothing surfaced it. The skill exited 0 before the daemon judged
  the file; the refusal is a central-log WARN only; no footer, tray or Emacs
  notice. When the owner asked at 14:02 "did we merge the workspace?", the agent
  answered "dispatched, not yet performed" — true from where it stood.
- Nothing reached the merge queue (`daemon.merge.recover entries:0` at every
  later boot), so the three restarts did not lose a merge; there was none.
- State now: branch `glimmer-intensity-boost` at `ff168d5fa`, one commit ahead
  of where it forked, worktree present, workspace open. Not merged, not closed.
- Note: 33 files sit in that quarantine directory. This has likely been failing
  silently for every skill-dispatched merge/close since the ingress moved to ids.

### OPEN

- Which side moves: the ingress accepting a name (and `project_dir`), or the
  skill emitting `dir`/id. The skill lives outside this repo
  (`~/.claude/skills/`), so touching it needs the owner's per-use permission.
- A quarantined command needs a visible surface (the dispatching workspace's
  footer/tray at minimum).

---

## Cross-cutting: what the logs could not tell us

- Draw records carry row ids only for `draw-response`.
- Nothing marks a row placement as replay vs live (the `plane` field exists on
  `row_placed`; the webapp side has no equivalent).
- Tool-card state transitions are not logged on either side.
- Classifier verdicts and reasons are DEBUG (item 4).
- The undecodable-frame record names no feed and no arm (item 5).
- Daemon-side page attach / feed open / webapp-log intake are not at INFO
  (item 6).
- `daemon.shimclient.watch_bash` logs an ERROR for a refusal its own next line
  calls `expected:true`, and the store logs ~20 WARNs per occurrence.

## How the items cluster

- A. Deploy path bypasses the rollout controller → items 1, 6 (likely), part of 3.
- B. Replay is unbounded and repeated → item 2, the 308 unknown-unit records,
  most feed churn.
- C. No invariant ties unit liveness to turn liveness → item 3.
- D. Queue semantics differ from the owner's intent → item 4.
- E. Independent defects: 5 (sub-feed frame routing), 7 (compaction release
  condition), 8 (unlanded refusal arm), 9 (mislabel), 10 (command-file contract).

---

## Addendum: the two SDK questions that decided direction (answered 2026-09-21)

### Can a turn be interrupted while its detached work keeps running? YES.

- The SDK's `Query.interrupt()` aborts the running turn and nothing else;
  `Query.stopTask(taskId)` ends one background task. They are independent calls.
- The shim's `killTurn` (`engine/turn.ts:636`) calls `interrupt()` and then
  loops `stopTask` over everything the turn spawned. The `live` refusal when
  `force` is unset is the SHIM'S OWN POLICY, not an SDK limit.
- `TurnKilledAgentOnly` already exists as an outcome arm. A "turn only" kill is
  therefore `interrupt()` without the `stopTask` loop — structurally available.

### Does the shim have a reliable source for the compaction summary? YES.

- The SDK streams `compact_boundary` (trigger, pre/post tokens) with no summary.
- The SDK offers a `PostCompact` hook whose input carries `compact_summary`
  (`sdk.d.ts`, `PostCompactHookInput`). The shim registers no hooks today.
- So the fix is: write the divider AT the boundary, and attach the summary from
  the `PostCompact` hook. The "next assistant prose" release condition goes.

---

## Fix 1 LANDED (2026-09-21): a deploy rolls out and never ends a turn

- `RollOutBuild` + `deploy-all.sh` rolling out by default; see the changelog
  line `deploy-rolls-out-never-restarts` and AGENTS.md "A deploy ROLLS OUT".
- Proven live at 16:04:13: six workspaces handed to a successor daemon in 2 s,
  every shim ADOPTED, none killed. e2e proves the invariant against a real
  daemon: a rollout during a running turn leaves the turn running
  (`e2e/rollout_e2e_test.go`).

### What the first live handover showed (all belong to fixes already planned)

- 270 `ClientLog` calls were refused `transferring_away` in the handover's one
  second, and the refusal has no proto arm (fix 5). Log forwarders keep posting
  to the daemon they were started against. THIS IS THE LIKELY ANSWER TO ITEM 6:
  webapp records vanish when their forwarder is pointed at a daemon that no
  longer serves the workspace. To confirm when fix 5 is done.
- The held `/model fable` prompt re-submitted itself on reconnect and was
  refused again (item 8): an "outage"-held prompt replays forever.
- The successor REPLAYED HISTORY on adoption (`detached_unknown_unit`,
  `row_without_identity`, `subagent_without_start` at 16:04:14). Adoption is
  neither an open nor a bind, so under the owner's replay rule it must not
  replay (fix 2).
- Transfers and shim relaunches run SERIALLY (`completeHandover`,
  `relaunchFleet`): a busy workspace delays the free ones queued behind it.
  They stay served by the old daemon, so nothing is harmed, but the design says
  "per workspace, independently". A separate defect, not yet scheduled.

