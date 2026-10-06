# Footer activity tiers — design record (combined with the working-status and quiet-stretch plan)

The footer's activity cell (`frontend.v1.FooterStatus`, each status arm's
`activity`) rarely changes during a turn, and when it does change the line
often stays up for a very long time. Today a line clears only when its own
clearing event arrives, and a newer event of lower rank can never replace an
older one of higher rank. The owner wants the cell to be very active,
giving continual feedback that the session is doing something. Stale lines
should give way to newer events, and when nothing is happening the cell
should fall back to always-true facts such as how close the account is to
its usage limits.

The owner told me to design this contract autonomously, following the
create-or-update-protobufs conventions, without per-increment sign-off
(2026-09-28). The selection gates were therefore skipped. Every decision
below is recorded here, so the owner can review each one and reverse it.

## THE PLAN (owner-approved 2026-09-30; READ THIS WHOLE FILE AFTER ANY COMPACTION)

This record now covers the COMBINED footer: this branch's tiers
(`footer-activity-updates`) merged with the other workspace's plan that
landed on `master` on 2026-09-29
(`docs/protobuf-design/footer-working-status-and-lull-activity.md`: the
`working` status, its step substatuses, and the quiet-stretch line). The
combined model is "The combined model" below, and it governs wherever an
older entry in this file disagrees with it.

### Steps, in order

1. **Rebase `footer-activity-updates` onto LOCAL `master`**, resolving every
   conflict along the way. The branch also carries four fixes that were
   consolidated onto it (commits titled "a real prompt interrupts the
   keep-alive it waits behind", "a mid-turn vendor failure is warned once, by
   the session watcher that owns it", its session-watcher test, and "the
   store probe asks GetLiveWork a scoped question"). Conflicts in the footer
   are resolved toward the combined model: `master`'s `working` rename and
   step substatuses win outright; the activity cell takes this branch's tier
   structure, with `master`'s quiet-stretch line becoming the quiet tier.
2. **Implement the combined model end to end, UNINTERRUPTED.** The owner said:
   no questions, no pauses, including for protobuf changes: follow the
   existing principles (the create-or-update-protobufs conventions, figma to
   idl, legality by construction, `optional` for absence, every element a
   message, every field documented, no process history in comments) with best
   judgment, and record every contract decision in "Landed changes" below as
   it is made.
   - Proto (`frontend/v1/footer.proto`), regenerate bindings (`proto/`
     `make all`, `make lint`, `make check-generated`).
   - Daemon footer resolver and its sources, with unit tests (one edge case
     per test, AAA, table-driven where it fits, canonical `dlog` logging,
     every error path tested).
   - Webapp footer, with Vitest, typecheck, the integration suite and
     `test:webkit`.
   - Daemon integration tests and any e2e scenario the change touches.
   - Comments that cite the old section title "salient, transient, or
     enduring" (`footer.proto` status-family header, `activity.go`, and the
     regenerated bindings) are updated to the new title.
3. **Docs:** `AGENTS.md` "Footer activity lines are salient, transient,
   quiet, or enduring" (already written 2026-09-30; keep it matching what is
   built); the webapp `AGENTS.md` one-line activity rule (from `master`);
   one line per landed fix in `docs/REMEDIATION-CHANGELOG.md`.
4. **Green everywhere:** daemon `make test` and `make integration`; shim
   `npm run typecheck`, `lint`, `npm test`, `test:integration`; webapp
   `typecheck`, `lint`, `npm test`, `test:integration`, `test:webkit`; proto
   `make validate`. Every failure in a tracked test is this work's to fix.
5. **Consolidation sweep** over the whole diff against `master`; extractions
   land as their own behavior-preserving commits with their own tests.
6. **Cherry-pick every commit of the branch onto LOCAL `master`** (NOT the
   merge queue, which is suspended; NOT a merge commit).
7. **Bounce all systems** so the owner's Emacs runs the new build: rebuild
   and restart the daemon, shim and webapp (`bin/build-frontend.sh`), and
   rebuild and kickstart `shim-store` and `shim-claude-sidecar` (see
   `AGENTS.md` for the deploy path); verify with
   `bin/readiness-report.sh` and `scripts/agent-shim-doctor.sh`.

### How the work runs

- Do the work MYSELF, in-session. Subagents only when very useful for rote
  work.
- Commit granularly (atomic units), tests passing before each commit.
- The held-queue fix is IN SCOPE (owner, 2026-09-30); see "Held-queue:
  prompts that skip classification queue like any other" below.
- The detached-work liveness fix is IN SCOPE (owner, 2026-09-30); see
  "Detached-work liveness" below. It is its own unit of work: its own proto
  decisions (recorded there), its own commits, and it runs after the footer
  implementation (step 2) and before the docs and the green-everywhere pass
  (steps 3 and 4), so both land in the same cherry-pick.

## Detached-work liveness (added to the plan 2026-09-30)

### The defect

- On 2026-09-28 two background shells of a subagent
  (`toolu_01AS9Dzaxys8Ho2FWQfbQewq`, `toolu_01Ur4sMrYb9NZPiQZA1F11Jh`) were
  killed at 22:00 (their spool files end in `[killed]`), yet the daemon still
  counts them live (`daemon.footer.live_work_taken` `shells: 2`). A merge of
  this workspace waited on them in `daemon.merge.await_free` indefinitely, and
  the footer still shows them.
- Nothing could retire them:
  - The sidecar never read their spools. It claims a spool only from the
    transcript sentence matched by `backgroundSentence`
    (`agent-shim/claude/shim-sidecar/internal/convert/launch.go:83`,
    `Command running in background with ID: X.`); a foreground Bash call the
    vendor moved to the background on its timeout is worded `Command did not
    complete within its 600s timeout and was moved to the background (ID: X).`
    and carries no structured field (`toolUseResult` is null), so the spool
    stayed held and unread (`held.go`), and its `[killed]` terminal was never
    seen. The shim also mislabels it "a person backgrounded running work by
    hand" (`shim.convert.detached`).
  - The daemon's `WatchBash` for each was refused `not_found` ("the store holds
    no rows for shell run") at launch, recorded as expected, and never
    retried, so no daemon-side watch existed to see a terminal.
  - The vendor sent no `task_notification` for them: the subagent that owned
    them had already concluded (`unit_terminal` 21:31:48).
- Evidence: the workspace daemon, shim and central sidecar logs for
  2026-09-28 20:40–22:10, the subagent transcript
  `.../subagents/agent-a6debe22e912b308d.jsonl` line 180, and the spool files
  under the session's `tasks/` directory.

### The fix (structural)

1. **The claim comes from the SDK's structured facts, not transcript prose.**
   The shim already receives `task_started` with `task_id`, `tool_use_id` and
   `task_type` (it logs them at `shim.engine.detached`). It writes the claim
   (the run's handle, its vendor task id, the owning agent, and the spool path
   when known) to the store through its writer; the sidecar reads claims from
   the store for every held spool and claims it, whatever wording the vendor
   used. The prose matcher may remain only as long as it is needed for runs
   the shim never saw; if it stays, it also matches the timeout wording.
2. **`WatchBash` on an announced run with no rows yet WAITS** for the run's
   first row instead of refusing `not_found`, and streams from there; the
   daemon holds that watch until the run's terminal row. A run is retired from
   the live set only by an observed end (its terminal, the vendor's
   notification, the shim's teardown conclusion), never left live with no
   watch able to observe it.
3. **The shim's `convert.detached` names the cause it actually observed** (the
   vendor's timeout moved it) rather than "a person backgrounded it by hand".
4. Tests: the sidecar claims a timeout-backgrounded spool from a store claim
   and reads its `[killed]` terminal; `WatchBash` before the first row waits
   and then streams; the daemon retires the run on the terminal; the
   end-to-end scenario of a subagent's backgrounded shell outliving the
   subagent and being killed retires it. Every error path logs through the
   component's canonical logger.
5. Record the contract changes (the store claim write, `WatchBash`'s changed
   `not_found` semantics) under "Landed changes" as they are made.

After it deploys, the two phantom shells retire on their own (their spools
already hold `[killed]`), and this workspace's merge and restart stop waiting.

## Held-queue: prompts that skip classification queue like any other (added to the plan 2026-09-30)

### The defect (observed 2026-09-30)

- The owner sent `/compact` while a turn ran. It correctly did NOT interrupt
  the turn, but it did NOT appear in the webapp's queue as a held prompt
  waiting for the turn to end, although it was held in some sense. Investigate
  where it went (the prompt queue's session-act path, `acts.go` /
  `runContextCut` / the held-prompt tray push) before changing anything.

### The rule (owner, 2026-09-30)

1. A prompt that skips classification (`/compact`, `/clear`, and every other
   session act or prompt kind that is never classified) is ENQUEUED exactly
   like a classified prompt: it is a held prompt, it surfaces in the webapp's
   queue as queued and waiting for the turn to end, and it runs when what is
   ahead of it ends. The ONLY difference is that it is never classified.
2. The queue is FIFO across both kinds: a prompt submitted after such an act
   is queued BEHIND it.
3. Classification (may this prompt interrupt?) is judged only against the item
   immediately ahead of the new prompt:
   - ahead is a non-classifiable act: the new prompt is NOT classified; it
     waits behind the act;
   - ahead is a classifiable prompt (even one that itself went unclassified
     because of what preceded it): the new prompt IS classified against it,
     and interrupts it when the classifier rules so.
4. "Never interrupted" means never interrupted AUTOMATICALLY by a prompt that
   comes after it. It can still be interrupted EXPLICITLY like any other
   prompt: `C-c C-k`, the webapp's stop, and every other explicit interrupt
   work on it unchanged.
5. **A classifiable prompt whose item ahead is a classifiable prompt still
   QUEUED (not yet running) is classified against it, and an "interrupt"
   verdict COALESCES the two** (owner, 2026-09-30): nothing has started, so
   the new prompt is folded into the queued one ahead of it. The queued prompt
   keeps its place and takes the new prompt's content folded in after its own;
   the new prompt disappears from the held-prompt drawer as its own entry; and
   the drawer shows the merged entry as `coalesced` (a user-visible result the
   owner asked for). A "no interrupt" verdict queues the new prompt behind it
   as usual. When the item ahead is RUNNING, an "interrupt" verdict interrupts
   it, as today.
6. The owner's worked example:
   - "hey update the doc" runs;
   - `/compact` is sent: queued, unclassified, visible as queued;
   - "read doc file and continue" is sent during the compaction: queued behind
     `/compact`, unclassified (what precedes it is an act), runs after it;
   - "actually also do X as well" is sent while "read doc file and continue"
     runs: classified against it (it is a classifiable prompt) and, the
     classifier ruling so, interrupts it immediately.
7. Tests: an act submitted during a turn is a held prompt on the tray; a
   prompt behind an act is held behind it unclassified; a prompt behind a
   classifiable prompt is classified; an explicit interrupt still stops an
   act; an "interrupt" verdict against a queued prompt coalesces the two (one
   drawer entry, marked `coalesced`, content folded in order); the worked
   example end to end in the daemon integration suite.
8. It is its own unit of work, done after the detached-work liveness fix and
   before the docs and green-everywhere pass, so it lands in the same
   cherry-pick. Update `AGENTS.md` wherever the prompt queue's act handling is
   described.

## The combined model (settled 2026-09-30)

### Four tiers, defined by what ends a line

| tier | ends when | kinds |
| --- | --- | --- |
| salient | the condition it describes stops being true; never a timer | every existing status-bound kind (fault under `disconnected` and `blocked`, start failed, a compaction RUNNING, retrying until a response lands, wakeup, gated call, question lead, cold gate cost, interrupting, blocked on user, authenticating, merging commit, close blocked, deploy update); the dead-query line (ends at the next prompt); the agent's push notification (ends at the next prompt); the context-budget warning and "compaction failed — ..." (end when a cut shrinks the context) |
| transient | its 10 s window lapses, or a newer transient replaces it | tool-call starts (`Bash: npm test`); task-tracker moves (`write tests · 3/7`); `submitting`, which states the delivery as it happens (held behind a turn, the held queue's size, classifying, interjecting, delivered); a concluded compaction; non-escalating faults; daemon Warn and Error records; session changes; a finished deploy; network-resume edges; a detached run finishing while the status is `background` |
| quiet | the next feed item surfaces | the quiet-stretch line (`✅ Bash finished — handling result...`, `❌ Read failed — handling failure...`, `✅ Prompt delivered — awaiting response...`), legal under `working` and `background` |
| enduring | never | ONE line: `usage`, or `unobserved` before any figure is read |

### Precedence: salient, then transient, then quiet, then enduring, always

- A transient covers only a quiet or enduring line, never a salient one. When
  it lapses, the line beneath shows again (or for the first time).
- Within salient: the kind that explains the standing step first, then a
  fault, then a deploy's progress, then the push notification, then the
  context-budget warning. (A rate-limit event ranked between the deploy and
  the notification until the owner dropped its line on 2026-10-01.)
- Within transient: the newest wins, so submitting a prompt replaces any
  standing transient.
- The 10 s window is never changed by what arrives next: the next event may
  never come, or come much later. A newer transient replacing an older one is
  a separate, orthogonal end.

### The enduring line: usage only

- Owner ruling, 2026-09-30: the context window's enduring line is dropped
  ("it's not useful"), and with it the 80% rule that chose between it and
  the usage. `context_window` (tag 2) is reserved.
- The line draws the 5-hour and weekly allowances; before any figure is
  read it is `unobserved`, so the element is never empty.
- Only each `<number>%` is colored, by the unchanged percent gradient.
- No enduring line draws a reading age: it is enduring, so when it was drawn
  does not matter. `figures_read_at_ms` (tag 5) is reserved.

### Taken from the other workspace's plan (on `master`), unchanged

- `FooterStatus.working` (was `thinking`) and its step substatuses
  (`executing`, `reading`, `writing`, `searching`, `fetching`, `delegating`,
  `thinking`; `submitting`, `clearing`, `compacting` as before).
- The quiet-stretch line, its wording rules (✅ or ❌, what landed, what comes
  next, never "agent"), its ERROR on a quiet stretch with no line while a turn
  runs, and its background lines.
- The one-line activity cell rule.

### Taken from this branch

- The tier structure of the contract, client-applied transient expiry, the
  transient kinds above, the network-resume visibility (chip glyph, waiting
  row, transients), the daemon Warn/Error tee, `session_change`,
  `compaction_concluded`, `updated`.

### Dropped

- Live reasoning and response text tails (`thinking` and `response`
  transients): they would conflict with the existing updates.
- Rotating the enduring lines, then the 80% rule that replaced it: the
  enduring line is the usage alone.
- `master`'s standing notification, rate-limit and budget lines: replaced by
  the salient kinds and the enduring rule above.

### Rulings of 2026-09-30, and the entries they supersede (kept visible)

- **Salient outranks transient** (confirmed after being briefly reversed in
  conversation): a transient must never cover a permission prompt or any
  other salient line.
- **A salient line clears when its own condition ends.** A proposal to clear
  every salient line at the next prompt was retracted by the owner; the next
  prompt clears only the lines whose condition it is (dead query, push
  notification).
- **Push notifications are salient.** SUPERSEDES "Push notifications are
  transient" (2026-09-28) and an intermediate "enduring" ruling.
- **The context-budget warning is salient.** SUPERSEDES its transient
  placement in landed change 1.
- ~~**Rate limiting is salient** as the vendor's rate-limit event; the usage
  percentages stay enduring.~~ **SUPERSEDED 2026-10-01 by the owner:** the
  salient rate-limit line (`FooterStatusActivityRateLimit`) is dropped, because
  the enduring usage line already carries the 5-hour and weekly figures. The
  vendor's rate-limit events still feed those figures and their verdicts; the
  `rate_limit` arm's tag is reserved in every salient oneof.
- **The quiet tier is new**, its own tier between transient and enduring.
  The owner first said "lowest priority"; ranked below enduring it could never
  show (an enduring line always exists), and the owner confirmed the ranking
  above transient-covered, above enduring.
- **`submitting` stays a transient and carries the delivery's progress**
  (held queue size, classification, interjection).
- **Task-tracker moves stay transients.**
- **"Only a condition that blocks the turn or the user is salient"** is
  widened: salient now also holds the push notification and the context
  budget warning, which the owner ruled salient.

## Core design principles

### Activity lines are salient, transient, or enduring

- **SUPERSEDED 2026-09-30** by four tiers: salient, transient, quiet,
  enduring. See "The combined model".

- **Principle (owner, 2026-09-28):** every activity line belongs to exactly
  one tier, and the tier is defined by what ends the line.
  - Salient lines end when the system state they describe stops being true.
  - Transient lines end when a newer transient replaces them or their expiry
    passes. The window starts at 10 s.
  - Enduring lines never end.
  - The owner's canonical statement is in `AGENTS.md`, in the section
    "Footer activity lines are salient, transient, or enduring".
- **Consequences for the contract:**
  - Each status arm's activity cell becomes a `oneof tier`, holding either
    that arm's salient line or the shared transient-over-enduring pair.
  - Salient kinds are declared per status arm.
  - Transient and enduring kinds are shared across all status arms.
- **Decisions it reopens:**
  - Every per-arm activity oneof.
  - The single-cell precedence (fault > update > notification > status kinds
    > rate_limited > context_budget).
  - The status-independent `notification`, `rate_limited` and
    `context_budget` kinds.
  - The status-independent `fault` kind.
  - The momentary `updated` deploy phase.
- **What it does not claim:**
  - It does not retire the momentary statuses (`interrupted`, `loading`).
    Those are statuses, not activity lines, and they keep the resolver's
    existing timed successor push.

### Only a condition that blocks the turn or the user is salient

- **Principle (owner, 2026-09-28):** non-blocking errors and warnings are
  transient by definition.
- **Consequences:**
  - Non-escalating faults become the transient `fault` kind.
  - Escalating faults stay salient, and only under the two statuses they
    claim (`disconnected`, `blocked`).
- **What it does not claim:**
  - Escalating faults are unchanged. They still decide the status per THE
    FAULT PARTITION.

### A timer may end only a transient

- **Principle (owner, 2026-09-28):** when a salient line never clears, the
  fix is to add the missing end signal, not to add an expiry.

### The daemon decides the expiry, and the client applies it

- **Principle (owner, 2026-09-28):**
  - Each push carries the transient with its expiry instant, plus the
    enduring line beneath it.
  - The client switches between them using its own clock.
  - The daemon runs no expiry timer and pushes nothing when a transient
    lapses.
- **Consequences:**
  - Nothing races between a timer firing and a new event landing, because
    the view drawn depends only on the last push and the client's clock.
- **What it does not claim:**
  - The client decides nothing beyond that single clock comparison.

### A prompt that could not be answered is salient

- **Principle (owner, 2026-09-28):** a dead vendor query means the prompt
  the turn carried could not be answered, so its line is salient, not
  transient.
- **Consequences:**
  - `query_died` is a salient kind of the idle cell, which the `turn_failed`
    and `degraded` statuses share.
  - It stands until the next turn opens.
  - `frontend.v1.FooterStatusBlockedSalient` reserves the tag its
    `query_died` kind held, because master made a dead query a failed turn,
    never a block.
- **Retraction:** an earlier revision of this design made `query_died` a
  transient.
  - Root cause: I read "the workspace is usable" as "nothing is blocked".
    But the unanswered prompt blocks the user until they prompt again.

### The network-resume change is visibility only

- **Principle (owner, 2026-09-28):** surfacing a background subagent's
  network-resume wait changes only what the footer shows.
  - How background subagents work today is fine, and nothing about it
    changes.
- **Consequences:**
  - No daemon rule reads the waits: not the `background` status, not the
    close-quiet check, not the deploy's drain.
  - Only the footer's agents chip glyph, the agents panel row and the
    `network_resume` transient consume them.
- **What it does not claim:** it does not stop the footer resolver from
  keeping a waiting agent's row data. That data exists only to draw the
  row.

### Push notifications are transient

- **SUPERSEDED 2026-09-30:** push notifications are salient (ends at the next
  prompt). See "Rulings of 2026-09-30".

- **Principle (owner, 2026-09-28):** the agent's `PushNotification` message
  is a transient line.
  - This is the `conversation.v1.AgentPushNotificationStart.message` field.
  - This replaces today's `notification` line, which is never cleared.

## Iteration sequence

Because the owner asked for autonomous design, the work is one increment
covering one concern: the activity cell of the footer's status family, in
`proto/src/frontend/v1/footer.proto`. The increment was walked top-down by
containment:

1. The status-family header rules.
2. The per-arm activity cells.
3. The salient leaves.
4. The transient tier.
5. The enduring tier.

No transport or endpoint stage applies. The cell is part of the existing
`WatchFooter` view, whose whole-view, event-driven push convention is
unchanged.

## Landed changes

### 1. The status family's activity cell is tiered

- **What changed** (`frontend.v1`, `footer.proto`):
  - Every status arm's `activity` field is now always set.
  - The field's type is a per-arm container holding `oneof tier { salient;
    unpinned }`.
  - `salient` is a per-arm `FooterStatus<Arm>Salient`, carrying `at` and the
    salient kinds legal under that arm.
  - `unpinned` is the shared `frontend.v1.FooterActivityTransientOverEnduring`,
    carrying an optional `frontend.v1.FooterActivityTransient` and an
    always-set `frontend.v1.FooterActivityEnduring`.
  - `frontend.v1.FooterStatusWaitingActivity` has no unpinned branch. A
    waiting session is always parked on a blocking condition, so its
    activity is always salient.
- **Salient kinds per arm:**

  | status arm | salient kinds |
  | --- | --- |
  | idle, turn_failed, degraded (they share `frontend.v1.FooterStatusIdleActivity`) | update, query_died |
  | thinking | compaction, retrying, update |
  | waiting | wakeup, gated_call, question_lead, blocked_on_user, cold_gate_cost, interrupting, update |
  | interrupted | update |
  | merging, merge_conflict, merge_failed, merged (they share `frontend.v1.FooterStatusMergingActivity`) | merging_commit, update |
  | background | update |
  | blocked | authenticating, fault, update |
  | disconnected | start_failed, fault, update |
  | closing | close_blocked, update |
  | loading | update |

  - The status arms that share a container were added on master by the
    commits "the footer gains merge_conflict, merge_failed and merged status
    arms" (`2fe6d812a`) and "the footer's turn_failed and degraded status
    arms" (`f3c38fb21`).
    - This design keeps their sharing, and makes each shared `activity`
      field always set like every other arm's.

- **Transient kinds** (status-independent):
  - `thinking`, with the text visible or withheld.
  - `response`.
  - `tool_call`.
  - `task`.
  - `submitting`.
  - `hook`.
  - `context_injected`.
  - `notification`.
  - `context_budget`.
  - `fault`, for non-escalating faults only.
  - `daemon_warning` and `daemon_error`.
  - `session_change`.
  - `updated`.
  - `compaction_concluded`.
    - A compaction's outcome is an event: the salient `compaction` line
      stands only while the compaction runs.
    - This replaces the momentary dwell that retired the concluded
      progress line on a timer, which a salient line may never have.
      (The daemon agent raised this; decided 2026-09-28.)
    - A failed compaction stays the transient `context_budget`.
  - Each transient also carries an `expiry`, and an optional `agent` that
    labels a subagent.
- **Enduring line:**
  - Optional `usage`, the renamed rate-limit report.
  - Optional `context_window`: used tokens, window size, and the fill
    resolved by the daemon.
    - Retired 2026-09-30 by the owner; see "The enduring line: usage only".
- **Moved or renamed:**
  - `FooterStatusActivityNotification` became
    `FooterActivityTransientNotification`.
  - `FooterStatusActivityContextBudget` became
    `FooterActivityTransientContextBudget`.
  - `FooterStatusActivityHook` became `FooterActivityTransientHook`.
  - `FooterStatusActivityContextInjected` became
    `FooterActivityTransientContextInjected`.
  - `FooterStatusActivityRateLimited` became `FooterActivityEnduringUsage`.
    Its allowances are now `optional`.
  - `FooterStatusActivityUpdate` lost its `updated` arm; tag 6 is reserved.
    `FooterStatusActivityUpdateUpdated` was deleted, and the finished deploy
    is now the transient `updated` kind, carrying the deploy notes.
  - `FooterAllowance` and its verdict arms gained the documentation they
    lacked. Their shape is unchanged.
- **Why:**
  - The owner's tier ruling (see Core design principles).
  - Lines stayed up too long because every kind was a standing fact ranked
    in one cell. Lines rarely changed because ordinary turn work never
    reached the cell at all.
- **Why the alternatives lost:**
  - *One flat oneof with a tier tag on each kind:* salient legality would no
    longer be per-status, so a merge commit under idle, for example, would
    become representable again.
  - *Salient and transient both shipped, client picks:* salient always
    outranks transient, so shipping both only adds a client-side decision.
    The rule that the client decides nothing forbids that, and the resolver
    already knows which one applies.
  - *Daemon-side expiry timer:* rejected by the owner's ruling. A timer can
    fire at the same moment a new event lands, and every lapse would cost an
    extra push.
  - *Keeping `rate_limited` as the name:* the line is now always drawn, not
    only when a limit is close. The old name would lead implementers to keep
    the ≥80% gate.
  - *Separate per-arm transient oneofs:* a transient can outlive the status
    it was raised under, such as the last response line lingering into
    `idle · done`. Making transients status-bound would drop exactly that
    carry-over.
- **Consequences, including accepted costs:**
  - **Daemon resolver (`daemon/internal/resolve/footer/`):**
    - `activity.go` is rewritten. Each per-status function returns the tier
      container.
    - `rateLine()` (`activity.go:74`) loses its newsworthy gate, because the
      enduring usage is always drawn.
    - The state's lifetimes change:
      - `notification` (`chips.go:404`) becomes a transient.
      - `contextBudget` (set at `resolver.go:542` and `resolver.go:980`)
        becomes a transient.
      - `retrying` must clear on the first successful response, not only at
        the next turn open (`resolver.go:363`).
      - `hook` and `injected` become transients.
    - The resolver must hold "the newest transient" as a single slot, filled
      from every event source, with `expires_at = at + window`. The window is
      a new injectable option, defaulting to 10 s.
  - **New daemon event sources feeding the transient slot:**
    - `conversation.v1.AgentThinking` and `conversation.v1.AgentResponse`
      deltas.
      - The resolver keeps the tail of the current unit's text.
      - Handling starts in `OnActivity`, `chips.go:18`. Today it ignores
        `Thinking` and handles `Response` only for latency.
    - Tool-call starts for every tool arm.
    - `TaskAct` moves.
    - `OnTurnOpened`/`SetTurn` produce `submitting`. The prompt's first line
      must reach the footer, which `TurnStarted` does not carry today (see
      `api.go`).
    - `SessionUpdate` model, permission-mode and MCP arms produce
      `session_change`. Today they are handled but ignored for the activity
      cell (`resolver.go` `OnSessionUpdate`).
    - Workspace-scoped Warn and Error records produce `daemon_warning` and
      `daemon_error`.
      - These must be copied to the footer at one place in
        `claude-repld.internal.dlog`, where workspace loggers emit, and never
        per call site.
      - The same rule is how faults reach the footer through
        `health.ObserveFaults`.
      - Records the footer resolver itself writes must be excluded, or the
        copy would feed back into itself.
  - **Deploy completion:** `update.go:77` currently arms a momentary dwell
    for the `updated` phase. That dwell is deleted: the successor raises the
    transient `updated` instead, and no timer retires it.
  - **Accepted cost: push volume.**
    - Thinking and response deltas arrive many times a second, and each
      change republishes the whole `frontend.v1.FooterView`.
    - The topic must keep only the latest value per subscriber, and the
      resolver should rate-limit tail changes before publishing (for
      example, one tail per new line).
    - This is an implementation obligation, not a contract change.
  - **Webapp (`webapp/src/footer/strip.ts`, `footer.ts`, `tones.ts`):**
    - The activity cell renders the tier container.
    - For `unpinned`, it compares the client clock to `expires_at_ms`,
      schedules a single re-render at that instant, then draws the enduring
      line.
    - This is the one clock decision the client is allowed.
  - **Elisp is unaffected.**
    - No `lisp/*.el` source decodes `frontend.v1.FooterView`.
    - The only mentions are prose comments in `lisp/host.el:30` and
      `lisp/roster.el:852`.
    - The check was a grep of `lisp/` for `FooterStatus` and `FooterView`.
  - **Carried forward unchanged:** `merging_commit` and `authenticating`
    remain salient kinds whose daemon state is never set.
    - `mergingCommit` is declared at `state.go:507` and `authLine` at
      `state.go:496`, and neither is ever assigned.
    - Deleting or feeding them is a separate question, left for the owner.

### 2. A background subagent waiting to be resumed after a network outage is visible

- **Requested by** the project lead (owner-approved, 2026-09-28).
  - Today the shim tracks the wait only in memory and in
    `shim.engine.network_resume` records (outcomes: waiting, resumed,
    not_resumed, abandoned, gave_up), so the user learns of it only from
    logs.
  - The lead left the shape to this design.
- **What changed, on the wire** (`conversation.v1`, `session.proto`):
  - `conversation.v1.SessionUpdate.network_resume_waits` (tag 31) is the
    STANDING SET of waits, restated whole on every change and once on every
    WatchSession open.
  - Each wait is a `conversation.v1.SessionNetworkResumeWait` carrying:
    - `work`, the failed run's `conversation.v1.DetachedWorkId`.
    - `failed_at_ms`.
    - `gives_up_at_ms`.
    - `resumes_delivered`.
  - `conversation.v1.SessionUpdate.network_resume_outcome` (tag 32) is one
    event per ended wait, with arms `resumed`, `gave_up` and `abandoned`
    (which carries the reason).
  - `not_resumed` has no arm. A failure the API answered is never waited on,
    and the agent's own failure terminal already says everything about it.
  - The `conversation.v1.SessionUpdate` header now says the session stream
    carries work the session does about a unit whose own stream has ended.
    - The header's stale clause that shim health is pulled was dropped,
      since `diagnostics` is pushed.
- **What changed, in the footer** (`frontend.v1`, `footer.proto`):
  - `frontend.v1.FooterAgentRow` gains `oneof state { running;
    waiting_for_api }`.
    - The waiting arm carries `failed_at_ms`, `gives_up_at_ms` and
      `resumes_delivered`, and is drawn as "waiting for the API · gives up
      in 24m".
    - A waiting agent keeps its row until the wait ends.
  - `frontend.v1.FooterChipAgents` gains an optional `waiting_for_api` glyph
    with a count, so the stall stays visible on the strip without opening
    the panel.
  - The chip's `count` includes waiting rows.
  - A new transient kind, `network_resume`, announces each edge of a wait:
    - `waiting`, with the give-up deadline.
    - `resumed`.
    - `gave_up`.
    - `abandoned`.
    - The transient's `agent` field names the subagent.
- **Why this shape:**
  - The wait blocks neither the turn nor the user, so under the tier
    principle it cannot be salient.
  - The standing state therefore lives where standing non-blocking work
    already lives: the live-work chip and its panel row.
  - The edges of the wait reach the activity cell as transients.
  - The give-up deadline is shipped as an instant, so the client ticks the
    countdown.
- **Why the alternatives lost:**
  - *Salient line while any agent waits:* this contradicts the owner's
    principle that only blocking conditions are salient. An outage lasting
    up to 30 minutes would pin the cell and hide all live feedback.
  - *Hold back the agent's failure terminal until the wait ends:* the
    failure would then look like a live agent to the chips with no new
    arms. But `conversation.v1` is what the producer saw, and the vendor did
    report the failure. Holding it back would also redesign the landed
    resume path, which adopts the resumed run as the vendor's own
    continuation.
  - *Events only, no standing set:* a consumer that (re)connects mid-wait
    would never learn the wait exists.
  - *Standing set only, no outcome event:* a wait leaving the set cannot say
    whether it was resumed or given up, and the user wants to know which.
- **Consequences:**
  - **Shim** (`agent-shim/claude/shim/src/engine/network-resume.ts`):
    - `NetworkResume` needs an emit seam for the set and the outcomes, fed
      at the same sites that write the `waiting`, `resumed`, `gave_up` and
      `abandoned` records.
      - The `waiting` record is at `network-resume.ts:426`; `gave_up`,
        `resumed` and `abandoned` are the other `LOGGER` outcome sites.
    - The work handle must come from the run's `tool_use_id`, which is the
      key of `NetworkResume.runs` and equals
      `conversation.v1.DetachedWorkId.value` (`convert/detached.ts:352`).
      Today `KnownRun` stores only `taskId` and `description`.
    - The WatchSession opener must state the current set before live frames.
  - **Daemon:**
    - The session watcher routes both arms to the footer resolver.
    - The footer resolver must KEEP a failed agent's row data (label,
      description, tokens, jump) while a wait for its work stands. Today the
      row leaves at the terminal (`OnSubagent`, `chips.go`), so the row
      would otherwise have nothing to draw.
    - Per the owner's visibility-only ruling (see Core design principles),
      a standing wait counts toward NO daemon rule: not the `background`
      status, not the close-quiet check, not the deploy's drain.
  - **Webapp** (`webapp/src/footer/`): renders the row state, the chip
    glyph and the transient.

### 3. The combined model (2026-09-30)

- **Decided by** this design under the owner's standing no-questions ruling of
  2026-09-30, following "The combined model" above.
- **What changed, on the wire** (`frontend.v1`, `footer.proto`):
  - The quiet tier is its own message,
    `frontend.v1.FooterActivityQuietStretch` (`text`, and `at`, the instant
    the stretch began, so the client draws its age).
    - It is carried only by
      `frontend.v1.FooterActivityTransientOverQuietOverEnduring`
      (`transient`, `quiet_stretch`, `enduring`), the unpinned tier of the
      `working` and `background` arms.
    - Every other arm keeps
      `frontend.v1.FooterActivityTransientOverEnduring`, so a quiet line is
      unrepresentable where no turn runs.
  - The shared salient kinds ride every arm's salient `kind` oneof under the
    same field names:
    - `rate_limit` (`frontend.v1.FooterStatusActivityRateLimit`: `window`,
      `verdict` of `allowed_warning` or `rejected`, `utilization`,
      `resets_at_s`). DROPPED 2026-10-01 (owner): its tag is reserved in
      every salient oneof, and the vendor's events feed the enduring usage
      line only.
    - `notification` (`frontend.v1.FooterStatusActivityNotification`).
    - `context_budget` (`frontend.v1.FooterStatusActivityContextBudget`),
      which also carries "compaction failed — ...".
    - Among the shared kinds: update, then rate limit, then notification,
      then context budget.
  - `frontend.v1.FooterActivityEnduring` is `oneof line { usage;
    unobserved }`.
    - `context_window` (tag 2) was retired on 2026-09-30; see "The enduring
      line: usage only".
    - `unobserved` (`frontend.v1.FooterActivityEnduringUnobserved`) states
      that neither figure has been read yet, so the element is never empty.
  - `frontend.v1.FooterActivityTransientSubmitting` gains `oneof stage`:
    - `held` (`position`, `queued`).
    - `classifying`.
    - `interjecting`.
    - `coalesced` (emitted by the held-queue fix).
    - `delivered`.
  - Transient tags 4, 5, 11 and 12 are reserved: the reasoning and response
    tails and the transient notification and budget kinds are gone.
- **What changed, in the daemon:**
  - Each shared salient kind has its own end signal
    (`internal/resolve/footer/salient.go`):
    - The notification ends at the next prompt.
    - The context budget ends at a successful cut, `/clear`, a concluded
      compaction, or a session switch (a new vendor session id).
    - A rate-limit line ends at an `allowed` event for its window (the line
      was dropped on 2026-10-01).
  - The cold gate's answer carries the compaction's `Progress`, so a gate
    that ran a compaction announces it concluded; the gate retires before its
    answer clears, so no intermediate status flashes.
  - The prompt queue reports each submitting stage (`reportHeld`,
    `OnSubmission`).
- **Owner requests of 2026-09-30, landed with it** (webapp and Lisp, no wire
  change):
  - The composer is 20% shorter (`agent-repl-input-height-fraction` 0.184).
  - The compaction boundary's "▸ summary" toggle is twice as large and white.
  - Expanding any feed item centers its row in the feed (`itemExpanded`).
    - This supersedes the 2026-09-23 rule that an expansion never moves the
      feed; a collapse or a reveal still never moves it.
  - An expanded non-prompt, non-response item never exceeds
    `--feed-item-max-h: 80cqh` on the `#feed-scroll` size container.

### 4. Detached-work liveness (2026-09-30)

- **Decided by** this design, implementing "Detached-work liveness" above.
- **What changed, on the wire** (`store.v1`):
  - `store.v1.EntryBatch.shell_run_claims` (tag 5) carries
    `store.v1.ShellRunClaim{vendor_task_id, run}`.
    - The shim writes one at every detachment fact the task stream states
      for a shell: `task_started` in the background, and a
      `task_updated` patch that moves it.
    - The store keeps one row per (task id, run) in the in-place table
      `shell_run_claim`, so the owner's database is not rebuilt.
  - `store.v1.ShimStore.GetShellRunClaims` answers the claims for a set of
    task ids.
    - Each answer carries `owner`, the book holding the run's own
      `activity:` row, unset while no producer has written it.
    - The claim carries no book because the shim often does not know it: a
      backgrounded subagent's calls never reach its stream.
  - `store.v1.WatchBashRunRequest.await_first_row`: when set, a run with no
    stored row is waited on instead of refused.
    - The shim sets it exactly for a run in its own live set, so the wait is
      bounded by the run's life.
- **What changed, on the wire** (`conversation.v1`):
  - `conversation.v1.DetachedWorkDetached.cause` gains `vendor_moved`
    (`conversation.v1.DetachedCauseVendorMoved`).
    - A `task_updated` patch states that work moved to the background, never
      why. The shim had called every such shell "backgrounded by hand"; the
      incident's shells were moved by their own timeout.
    - A shell is now announced `vendor_moved` at the patch, and its own tool
      result restates the row with the real cause when it reaches the shim.
    - An agent moved by a patch keeps `by_user`, unchanged.
- **What changed, in the systems:**
  - The sidecar asks the store for the claims of its held shell spools once
    per rescan, and claims each one whose owning book's transcript it has
    attributed, through the same `TaskSpawned` observation a launch uses.
  - The sidecar's prose matcher also reads the timeout sentence ("did not
    complete within its 600s timeout and was moved to the background (ID:
    X)"), with its limit, for runs the shim never saw.
  - The daemon needed no change: it holds a shell watch's pending open with
    no deadline and retires the run at its terminal. A daemon integration
    test pins that for a watch that opens with no `start`.
  - `shim.v1.WatchBashResponse`'s doc says a run claimed only by task id has
    no `start` and opens on its output.

### 5. The retry schedule and the restored API (2026-09-30)

- **Decided by** the owner after the outage of 2026-09-30.
  - The retrying line stood for 17 minutes with no sign of when the next
    try was due.
  - The vendor's own records show it promised a retry in 32 seconds after
    its eighth failure and never made one: the vendor stalled, and nothing
    cleared the line because nothing recovered.
- **What changed, on the wire:**
  - `conversation.v1.ApiRequestFailed.retry` carries
    `conversation.v1.ApiRetry{attempt, max_retries, next_attempt_at_ms}`,
    the vendor's `retryAttempt`, `maxRetries` and the failure's instant
    plus its `retryInMs`.
  - `frontend.v1.FooterStatusActivityRetrying` gains `next_attempt` (an
    instant) and `max_attempt`, both unset when the vendor stated no
    schedule.
  - `frontend.v1.FooterActivityTransient.api_restored`
    (`frontend.v1.FooterActivityTransientApiRestored{failed_attempts}`).
- **What changed, in the systems:**
  - The sidecar reads the schedule from every recorded `api_error`.
  - The daemon counts attempts as the vendor does when it states a schedule.
    - Counting failures itself had run one ahead of the vendor's count.
  - The retried call's first response ends the line and raises
    `api_restored`.
  - The webapp draws "retry #9 of 11 · next try in 12s · <status>", the
    countdown ticking on the shared clock.
    - Once the promised instant passes it reads "next try overdue by 2m",
      so a stalled vendor is visible as a stall.
  - The webapp draws the transient as "API answering again after 8 failed
    attempts".
- **Left open:** nothing acts on an overdue retry. The line only shows it.

### 6. The verdict split (2026-09-30)

- **Decided by** the owner: most prompts sent during a turn add to its work,
  and interrupting for them made the agent read the addition as a rejection.
  - A live probe (SDK 0.3.280) settled the mechanism: a message pushed while
    a tool call runs is folded into the running turn after the tool result.
  - With no tool call in flight it becomes the vendor's next turn instead.
- **What changed, on the wire:**
  - `shim.v1.StartTurnRequest.join_running_turn` sends a prompt into the
    running turn with no interrupt.
  - `conversation.v1.AgentPrompt.folded_into` names the turn a prompt was
    folded into.
  - `shim.v1.StartTurnRequest.vendor_note` carries words for the agent alone.
  - `frontend.v1.HeldPrompt.after_tool_call` and
    `frontend.v1.FooterActivityTransientSubmitting.after_tool_call`.
- **What changed, in the systems:**
  - The classifier routes `queue`, `after_tool_call` or `interrupt`, and
    answers `after_tool_call` when unsure.
  - The queue sends an `after_tool_call` prompt to join the running turn, one
    at a time; against a still-queued prompt either non-queue route coalesces.
  - The shim holds the join until the vendor's echo decides its fate.
    - Folded: its row names the running turn, and the daemon closes its own
      turn as `wsm.CloseFolded`, drawing its bubble under the running turn.
    - Not folded: it takes the slot the moment the running turn leaves it,
      and the daemon's watcher stands it in flight in that turn's place.
  - A prompt whose verdict interrupted the turn is delivered with a note
    telling the agent the work was cut because this message changes it.
  - The tray draws the new verdict with a green badge; the footer reads
    "after this tool call".

### 7. The usage line is the account's, and it is never empty (2026-10-06)

- **Decided by** the owner: the activity cell always shows something, and with
  nothing salient or transient standing it shows the account's usage.
  - Live, two workspaces on the work account drew an empty cell after a daemon
    restart while one on the personal account drew its figures.
  - The work account is an enterprise seat: the vendor's usage service answers
    for it with no five-hour or weekly window, which the line had no arm for.
- **What changed, on the wire:**
  - `FooterActivityEnduring.no_allowance` (tag 4,
    `FooterActivityEnduringNoAllowance`) is the account whose usage service
    reports no session allowance.
  - `FooterActivityEnduring.unobserved` is now drawn as words: "usage not yet
    seen for this account".
  - `FooterAllowance.resets_at_s` now states that a passed reset draws the
    allowance as "<label> reset since last seen" with no percentage.
- **What changed, in the systems:**
  - The daemon keeps the usage per account root, shared by every workspace on
    it, and persists the last figures per account in its state store.
  - The webapp words every enduring arm, and lapses an allowance on its own
    clock.

### 8. The usage line is chosen by the account's billing mode (2026-10-06)

- **Decided by** the owner: the enduring usage line follows the account's
  billing mode, never which account it is.
  - A subscription (Pro, Max) draws its five-hour and weekly windows, as before.
  - A per-seat Enterprise account draws its month-to-date spend against the
    seat's allotment: "$223.88 of $12,000 this month".
  - "no session or weekly allowance on this account" is gone.
- **Evidence** (the vendor reports the spend):
  - The SDK's get_usage reply declares `rate_limits.extra_usage {monthly_limit,
    used_credits, currency}` in minor units, and `subscription_type`
    ('pro' | 'max' | 'team' | 'enterprise').
  - The work account's usage answer (the vendor CLI's own cache) carries every
    window null and `extra_usage.monthly_limit` set.
  - The seat tier itself (`enterprise_usage_based`) is not exposed by the SDK.
- **The per-seat signal** (owner ruling): the subscription type is enterprise
  AND the vendor reports a monthly limit with no five-hour window. Anything
  else that reports windows is a subscription. The shim decides and logs the
  decision with the raw subscription type at INFO.
- **What changed, on the wire:**
  - `SessionAccountUsage.outcome` gains `seat_spend` (tag 5,
    `SessionAccountUsageSeatSpend {allotment, optional spent}`), with money as
    `SessionMoney {amount_minor, currency}`.
  - `SessionAccountUsage.subscription_type` is typed: tag 2 (the free string)
    is reserved, and tag 6 is `optional SessionSubscriptionType` with arms
    pro, max, team and enterprise. A plan this revision does not name is the
    set message with its oneof unassigned; no Unknown arm.
  - `FooterActivityEnduring.no_allowance` (tag 4) is RETIRED and reserved.
  - `FooterActivityEnduring.seat_spend` (tag 5,
    `FooterActivityEnduringSeatSpend {allotment, optional spent, optional
    utilization}`), with money as `FooterMoney {amount_minor, currency}`.
- **Who formats:** the webapp, where every other usage-line figure is formatted
  today. The drawn spend wears the same percent gradient as the subscription
  line's percentages, driven by `utilization` (one gradient path).
- **No reset date:** the vendor states none.
- **What changed, in the systems:**
  - The shim reads `extra_usage` and the plan, and picks the arm.
  - The daemon keeps one billing mode per account root (a seat-spend sample
    clears the windows; a windows sample clears the seat spend), persisted in
    wsm layout 25 (`account_usage` gains the seat columns; `no_allowance` is
    no longer read and is written 0, to be dropped by a later breaking step).
  - A row stored with `no_allowance=1` reloads as unobserved and re-resolves
    on the account's next sample.

## Sweep

- Nothing in `footer.proto` is left unreferenced after the change.
  - The check was a grep of every message name against `proto/src`.
- Nothing outside `footer.proto` referenced any moved or renamed type.
