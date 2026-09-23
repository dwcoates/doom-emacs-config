# Performance assertions in the cross-system e2e suite — specification

Scope: what to BUILD. Every budget in section C was **PROVISIONAL** when this
document was written, and had to be replaced by a measurement before the
assertion it belongs to was allowed to fail a run.

> **PHASE 1 IS BUILT AND MEASURED (2026-09-04).** Ten assertions — the Go and
> webapp halves of rows 1a/1b, 2, 3a, 5, 8a, 11a/11c, 14 and 17b — are live in
> `e2e/perf_*_test.go` and `webapp/test/webapp-layer/perf.layer.test.ts`, with
> their measurements, their final budgets and the defects the measurement found
> in **section I**. The Emacs-layer halves (rows 1a, 2a/2b, 8b, 11a/11b, 14b,
> 17a — every row whose stamps are `float-time` inside Emacs, §A3) are phase 2.
> Section C's per-row budgets are left as written: they are the provisional
> figures the ratchet in §I was applied to, and rewriting them would erase the
> record of what was proposed before anything was run.

The standing instruction this document was written under, and the reason it
is shorter than the proposal that produced it:

> **A row that names a behavior the system does not have is STRUCK with a
> one-line reason, never built.**

Eleven of the twenty-four proposed rows named something the system does not
do. They are struck or restated below, each with the file that settles it. The
proposal was a proposal, not a feature list, and this document does not treat
a budget as evidence that the hop it budgets exists.

### The strike ledger

Every claim removed, with the one line that settles it. Full reasoning in §C.

| row | what was struck | why |
|---|---|---|
| 5 | interrupt **keypress in Emacs** | no `Interrupt` client exists in elisp; `rpc.el` has no such verb and no command calls one |
| 8 | ask reaching an **Emacs modeline** | the module's only modeline segment is `agent-repl-link-drain-segment`, which reports the daemon link |
| 8, 9 | Emacs **answering** an ask | `AnswerQuestion` / `AnswerPermission` / `AnswerColdGate` have no elisp client |
| 12 | webview **first paint** | nothing in the suite can observe rendering inside an xwidget |
| 13, 15 | **tabs in the webapp** | the webapp is one page per workspace; the tab bar is Emacs's |
| 20 | a **500-row** page | `resolve/feed/api.go:200 DefaultPageSize = 50` |
| 21 | roster cost **flat in N** | `sidebar.ts` rebuilds the whole rail per push — O(N) by construction |
| 3b | token → **visible** text as a latency | `smooth.ts` paces reveal at 200 cps on a 0.3 s constant, by design |
| 1, 4, 11, 13, 14 | any **single "both clients"** number | no layer can read two clients at once (§B) |
| all | anything on **fast mode** | owner ruling 1: no frontend surface, and none is coming |
| 23b, 24 | CPU and memory sampling | blocked, not struck — `harness.Daemon`'s process handle is unexported (§G P3) |

---

## A. The clocks that already exist

Nothing in section C proposes a new production timestamp. Everything it
measures is measured with one of these five, all of which are already
written on every run.

### A1. One timestamp contract, five runtimes, microsecond resolution

`proto/vocab/log-timestamp.json` is the seam; each runtime answers it in its
own language and each asserts against the seam in its own tests.

| runtime | helper | resolution |
|---|---|---|
| daemon, store, sidecar (Go) | `agent-shim/logging/go/timestamp.go:Timestamp`, layout `2006-01-02T15:04:05.000000-07:00` | microsecond field, `time.Now()` precision |
| shim, webapp (TypeScript) | `agent-shim/logging/ts/timestamp.ts:logTimestamp` | **millisecond** — the helper's own comment: "JavaScript resolves instants only to milliseconds, so the last three microsecond digits are always zero" |
| Emacs (elisp) | `lisp/core.el:797 agent-repl--log-rfc3339-timestamp`, format `agent-repl--log-timestamp-format` = `"%FT%T.%6N%:z"` | microsecond |

Every record also carries `runtime`, `pid`, `level`, a **stable `operation`
name**, and a free-form `context` map. `daemon/integration/harness/logs.go:22
LogRecord` already decodes all of it, and
`harness.AwaitLogRecord` already waits on it. This is the suite's primary
instrument and it needs no addition.

**The one caveat that governs section C**: these are LOCAL WALL CLOCKS. A
delta between two records is a latency only when both were written by
processes on the same host with the same clock, which is true of every
process in a `World` and of the sandboxed Emacs. It is NOT true of the
webapp under the vitest layer — see A4.

### A2. The Go test's own clock

`time.Now()` in the Go test process, around a Connect call and a watch-stream
frame receipt. The most honest instrument the suite has: one clock, one
process, no correlation problem. It measures **rpc issued → frame delivered to
a Connect client**, which is what the daemon owes; it does not measure any
client's drawing.

Existing precedent: `emacs_test.go:940 AwaitEvalFor` already stamps `started
:= time.Now()` and records the satisfied wait as a phase.

### A3. Emacs: `float-time` stamped INSIDE Emacs, read back afterwards

The naive instrument — a Go `AwaitEval` loop — cannot measure anything in
this document's range. It polls at `world_test.go:85 pollInterval = 20ms`
and each probe is an `emacsclient` round trip whose observed healthy maximum
is 414 ms (`EMACS-LAYER-SPEC.md`, "The bounds, measured"). An instrument with
20 ms granularity and a 414 ms tail cannot adjudicate a 15 ms budget.

The instrument that works is already precedented in this suite:
`emacs_composer_e2e_test.go:armSubmissionObserver` advises the module's own
public outbound verb (`agent-repl-rpc-submit-prompt`) and pushes what it saw
onto an elisp variable the Go test reads later. The same shape, stamping
`(float-time)` at each edge, gives an **in-Emacs** delta at microsecond
resolution with the emacsclient round trip entirely outside the measurement.

- The observers advise PUBLIC contract boundaries (an rpc verb, a hook on
  `agent-repl-roster-update-functions`) — never a private helper. Reaching
  past a command into an internal is already a defect in this layer
  (`EMACS-LAYER-SPEC.md`, "Commands are invoked as commands").
- The samples accumulate in one elisp variable and are read back ONCE at the
  end of the run, so the readback cost is paid once, not per sample.

### A4. The webapp: two clocks, and they are not the same

`WEBAPP-LAYER-SPEC.md` §G finding 7 already records this; it is decisive
here. `webapp/test/integration/harness.ts:mountApp` runs
`vi.useFakeTimers({ shouldAdvanceTime: true, now: HARNESS_EPOCH_MS })` with
`HARNESS_EPOCH_MS = 10_000`.

Consequences, all of them binding:

1. `Date.now()` on the page reads ~1970. `logTimestamp()` therefore stamps
   webapp-layer records in 1970. **A webapp-layer log timestamp can never be
   subtracted from a daemon log timestamp.** Only durations computed
   page-side are meaningful.
2. `shouldAdvanceTime: true` makes the fake clock track real time, so a
   page-side `Date.now()` DELTA is approximately real — but only across a
   window in which no test called `app.tick(ms)`, which jumps the clock.
3. `performance.now()` appears nowhere in `webapp/src`. The only mention is a
   docstring in `webapp/src/smooth.ts:61`; the reveal actually injects
   `rc.ctx.ticker.now()`, and `webapp/src/clock.ts:createTicker.now` returns
   `Date.now()`.
4. `settle()` (`harness.ts` ~551) returns only after
   `SETTLE_STABLE_ROUNDS = 4` consecutive quiet drain rounds. A measurement
   taken after `settle()` returns over-counts by at least four rounds. **No
   latency assertion may be gated on `settle()`.**

The instrument that works: capture the real clock BEFORE the fake timers are
installed, exactly as `mountApp` already captures the real `setImmediate` for
`yieldToIo` ("the real one is taken now"), and stamp with that. This is a
change to `webapp/test/**`, not to `webapp/src/**` — no production touch.

### A5. The apply chokepoint the webapp already has

`webapp/src/rpc/streams.ts:watchStream` → its inner `consume(response)`
closure is the ONE place every frame from all seven server-streaming rpcs
passes through:

```
assertNoUnknownFields(...) → ctx.notePush?.() → opts.onPush(response)
```

`opts.onPush` writes the DOM **synchronously** and returns only once it has.
There is no virtual DOM, no diff, no batching, no microtask coalescing and no
rAF scheduler anywhere in the apply path. So `consume` is simultaneously the
receive stamp and the apply-complete stamp: **wrapping one function yields
frame→DOM latency for every surface.** `ctx.notePush` (`webapp/src/rpc/context.ts`)
already exists as a page-wide push observer, but it fires BEFORE the draw and
so cannot close the interval on its own.

Per-surface apply sites are named in section C. Each already logs (`feed.row-appended`,
`feed.row-replaced`, `feed.apply-page`, …) via `webapp/src/log.ts:log`, but
records pass through `webapp/src/clientlog-throttle.ts` (`DEFAULT_INTERVAL_MS
= 2000`), so a naive per-token log would be throttled away. Perf samples
travel as ONE aggregated record at the end of a scenario, never one per event.

### A6. Correlation ids

| clock pair | correlated by |
|---|---|
| Emacs edge → Emacs edge | none needed — one process, one variable |
| Go rpc → Go frame | the `TurnId` / `WorkspaceRef.id` in the response and the frame |
| daemon log → daemon log | the promoted `workspace_id` field, plus turn identity **inside** `context` |
| daemon log → Emacs log | `workspace_id` (both runtimes bind workspace identity into every record: `lisp/core.el:1081 agent-repl--log-add-workspace-identity`) |
| webapp page → Go test | the `ClientLog` rpc, `operation` matched verbatim, payload in `context` — the rendezvous `webapplayer_e2e_test.go`'s `wlPageMountedOperation` already uses |

**Three correlation hazards, each of which will silently produce zero matches
if a reader assumes otherwise. All three are grounded in
`daemon/internal/dlog/record.go`.**

1. **There is no promoted `turn_id` record field.** Turn identity travels as
   an ordinary key inside `context`, and **the spelling differs by package**:
   `context.turn` in `promptqueue`, `workspace` and `resolve/feed`;
   `context.turn_id` in `sessionwatcher`. A perf reader must accept both.
2. **`workspace_id` is promoted, but several packages also write a raw
   `context.workspace` key** (`wsm`, `workspace/verbs.go`, `rollout`,
   `handover`) that does NOT land in the promoted field. A reader keyed only
   on the promoted field misses those records entirely.
3. **`request_id`, `agent_repl_session_id` and `claude_session_id` are
   declared in the record and populated by nothing in the daemon.** They look
   like correlation ids and are always empty. Reported in §H.

---

## B. Which layer may host which assertion

| layer | file pattern | what it can time | what it cannot |
|---|---|---|---|
| Go | `e2e/*_e2e_test.go` | rpc→frame (A2), any pair of structured-log records (A1) | any client's drawing |
| webapp | `webapp/test/webapp-layer/*.layer.test.ts`, driven by `webapplayer_e2e_test.go` | frame→DOM, click→DOM, page-side durations only (A4) | anything correlated to a daemon instant |
| Emacs | `e2e/emacs_*_e2e_test.go` + in-Emacs observers (A3) | any pair of in-Emacs edges; Emacs edge→daemon log via `workspace_id` | anything inside the xwidget webview |

**No single layer can host an assertion whose two endpoints are in different
clients.** The Emacs layer does run the real webapp — the proof-of-life test
confirms a live WKWebView serving the real bundle from the daemon's origin —
but nothing in the suite can read the DOM inside an xwidget. Every proposed
"…in both clients" row is therefore SPLIT into one assertion per layer, and
the cross-client claim is retired as unmeasurable rather than approximated.

**N = 20 samples per assertion. Percentile rule: p50 = sample 10 of the
sorted 20 (the lower median, never an interpolated mean); p95 = sample 19 of
the sorted 20 (`ceil(0.95 × 20) = 19`).** No interpolation anywhere: with
N=20 an interpolated p95 is a fiction between two real observations, and a
budget must be violated by a real sample. Stated once here, and by reference
in every row.

---

## C. The rows

Verdict key: **BUILD** — chain exists and is timeable. **SPLIT** — the row is
two assertions in different layers or on different chains. **RESTATE** — the
behavior exists but not as worded. **STRUCK** — the behavior does not exist.

### C0. Owner rulings that bind this section

**Ruling 1 — fast mode has no frontend surface and will not get one.** Any row
or hop that would terminate in a fast-mode indicator is STRUCK, permanently,
not deferred. None of the twenty-four proposed rows does, so nothing is lost
here; the ruling is recorded because the proto invites the mistake. The
conversation protos carry `SessionUpdate.fast_mode` / `SessionFastMode`
(`proto/gen/go/conversation/v1/session.pb.go:907`) and
`ModelCapabilities.supports_fast_mode`, and a future reader will find them and
assume a surface exists. It does not: the only mention anywhere in a client is
`webapp/src/topbar/model.ts:195`, which appends a static `fast` **capability**
tag to a model offer, and never reflects fast-mode STATE. There is no elisp
mention at all. No perf assertion may be written against fast mode.

**Ruling 2 — the sidecar's 50 ms spool poll is an accepted inherent floor.**
Row 19 keeps its 60/150 ms budget and is marked **inherent-floor**: the poll is
the design, not a defect candidate, and no assertion may be written that treats
closing it as an improvement. §E's structural assertions therefore assert the
cadence is PRESENT and unchanged, never that it is absent.

**Ruling 3 — Emacs draws no feed.** The webapp draws the feed; Emacs draws the
tab bar, the modeline segment and the composer. This is why row 1 splits, why
row 2 is "composer cleared + roster arm flips in Emacs" and nothing about
bubbles, and why no row in this document asserts a feed row in Emacs. An
earlier draft of the proposal assumed otherwise; every trace of that assumption
is removed.

### 1. RET in Emacs composer → prompt bubble drawn in webapp — SPLIT

**Chain, fully grounded:**

| hop | site |
|---|---|
| origin | `lisp/input.el:931` `(map! :map agent-repl-input-mode-map :ni "RET" #'agent-repl-send)` |
| command | `lisp/input.el:855 agent-repl-send` → `:813 agent-repl--send` (origin `:user-sent`) → `agent-repl--input-submit` (rpc call at `input.el:650`) |
| rpc | `agent-repl-rpc-submit-prompt` (`lisp/rpc.el:283`, `defverb` macro) → `SubmitPrompt` (`proto/src/agentrepl/v1/service.proto:68`, `endpoint_submit_prompt.proto`) |
| daemon | `daemon/internal/server/prompt.go:(*server).SubmitPrompt` → `prompthandler/handler.go:(*handler).Submit` → `promptqueue/submit.go:(*queue).Submit`; operations `daemon.server.submit_prompt`, `daemon.prompthandler.submit`, `daemon.promptqueue.submit`, `.deliver`, `.accept` |
| push | `WatchFeed`, via the ONE feed write path: `promptqueue/deliver.go:(*queue).mirrorAccepted` → `resolve/feed/resolver.go:(*resolver).upsert` → `resolve/feed/tail.go:(*tailSub).enqueue` → `server/feed.go:(*server).WatchFeed`. Also `WatchFooter` (`daemon.footer.set_turn`) and `WatchWorkspaceRoster` (`daemon.sidebar.set_turn`) |
| webapp receive | `webapp/src/rpc/streams.ts:watchStream.consume` |
| webapp handler | `webapp/src/feed/feed.ts:mountFeed.openWatch` `onPush` |
| webapp apply | `feed-view.ts:upsert` (logs `feed.row-appended`) → `adopt` → `announce` → `feed/rows/user-prompt.ts:drawFeedUserPrompt` |
| DOM | `article.feed-item[data-row-kind="userPrompt"][data-mine="true"] .bubble.user > .bubble-body` |

**Why SPLIT:** the origin is in Emacs and the terminus is in a webapp DOM no
layer can read from where the origin lives (§B). The prompt bubble is **not
optimistic** — `webapp/src/composer/composer.ts:send1` only remembers the
`TurnId` via `own-turns.ts:rememberOwnTurn`; the bubble arrives on the server
round trip — so the webapp half is a genuine round-trip measurement in its
own right.

- **1a (Emacs layer, N=20):** `float-time` at the `agent-repl-send` advice
  entry → the daemon's `daemon.prompthandler.submit` record for the same
  `workspace_id`. Clocks A3 + A1, same host. Budget PROVISIONAL 20/50 ms.
- **1b (webapp layer, N=20):** page-side real clock at `composer.send1` →
  the `consume` return for the frame carrying the matching `TurnId`. Clocks
  A4 + A5. Budget PROVISIONAL 20/50 ms.

The proposed row 1 as a single number is retired: it was two round trips
added together with no instrument spanning them.

### 2. RET → composer cleared + roster arm flips in Emacs — SPLIT

These are **two different chains**, and the proposal treated them as one.

- **2a — composer cleared. BUILD.** Clearing rides the UNARY ACK, not a push:
  `lisp/input.el:628 agent-repl--input-on-success` → `:595
  agent-repl--input-accepted`, which calls `(erase-buffer)` (guarded by
  `from-buffer`, which is `(null prompt)` at `input.el:846`) and logs
  `elisp.input.accepted`. Emacs layer, both stamps in Emacs (A3): advice on
  `agent-repl-send` → advice on `agent-repl--input-accepted`. Budget
  PROVISIONAL 15/40 ms. This is the single cleanest row in the document: one
  process, one clock, microsecond resolution, no correlation.
- **2b — roster arm flips. BUILD, different budget.** The arm is
  `lisp/roster.el:108 agent-repl-roster--status-by-id`, written by
  `roster.el:557 agent-repl-roster--record-statuses` inside `roster.el:566
  agent-repl-roster-apply`, reached from `roster.el:592
  agent-repl-roster-on-push` on the `WatchWorkspaceRoster` stream. It does
  not ride the ack and cannot share 2a's budget: it is a full server round
  trip plus a roster recomputation. Emacs layer, stamps at the `send` advice
  and on `agent-repl-roster-update-functions` (the public hook
  `roster.el:566` runs last). Budget PROVISIONAL 30/80 ms, matching the other
  push-carried rows rather than the ack-carried one.

There is **no roster buffer** in Emacs; the roster surface is the tab bar
(`lisp/status.el:1647 agent-repl--tabline-advice`, redraw
`status.el:903 agent-repl--force-tab-bar-redraw`). 2b asserts on
`agent-repl-roster--status-by-id`, per the readback table's rule "never
scrape human text where a variable exists".

### 3a. First streamed token → visible in webapp — RESTATE

The chain exists: `consume` → `feed.ts` `onPush` → `feed-view.ts:drawRowBody`
case `"activity"` → `drawActivity` case `"response"` →
`webapp/src/feed/cards/response.ts:drawFeedResponse` → `drawFeedResponseUpdate`.
DOM: `[data-row-kind="activity"][data-unit="response"]` with body
`div.bubble.assistant.md[data-state="update"]` and `span.response-arriving`.

**"Visible" must be restated, because visibility is deliberately paced.**
`response.ts:animate` reveals characters through
`webapp/src/smooth.ts:SmoothReveal` with `DEFAULT_REVEAL_OPTIONS = { minCps:
200, catchupSeconds: 0.3 }` — a designed type-out, not a latency. The
assertion is therefore on **the first response element entering the DOM**
(which `animate` does synchronously via its initial `paint(resumed)` before
scheduling any frame), not on the token's characters being legible.

BUILD, webapp layer, N=20, budget PROVISIONAL 20/50 ms.

### 3b. Token frame → DOM apply, steady over 200 tokens — RESTATE

The hop is real and is the cleanest thing in the webapp: apply is synchronous
inside `consume`, with no batching and no scheduler (A5). What is NOT real is
any claim about the reveal: `animate` self-schedules on
`requestAnimationFrame` and is the only rAF in `webapp/src`. The two are
decoupled by design.

- Assert: `consume` entry → `consume` return, per response frame, over 200
  frames. This measures decode + `replaceChildren` and nothing else.
- Do NOT assert on revealed character count; that is `smooth.ts`'s tuning and
  belongs to `webapp/test/` unit coverage, which already owns it.
- **The instrument is marginal at this budget.** Even on the captured real
  clock, jsdom under vitest gives millisecond granularity in practice; a 5 ms
  p50 sits at ~5 quanta. The budget must be re-derived from the measurement
  before it is allowed to fail a run, and if the observed spread is under
  ~3 quanta the row is retired rather than given a number it cannot support.

**"Steady" is the load-bearing word, and there is a grounded reason to doubt
it.** The daemon does not send a token; it re-sends the WHOLE accumulated row
on every token. `daemon/internal/resolve/feed/response.go` folds
(`fold.markdown += state.Update.GetNewMarkdown()`), `proto.Clone`s the result,
and `resolver.upsert` pushes it (deduping only byte-identical rows,
`daemon.feed.row_unchanged`). The webapp then rebuilds the row body whole.
Both halves are therefore **O(row length) per token, O(n²) over a turn** — so
frame 200 is expected to be dearer than frame 1 by construction, and a flat
p50/p95 over 200 frames would be the surprising result.

The assertion is written to say what is true rather than to hide it:

- an absolute p50/p95 over all 200 frames, AND
- a growth check: the p50 of frames 150-200 is within a stated factor of the
  p50 of frames 1-50. The factor is derived from the first measurement; it is
  not asserted to be 1.

There is also a **daemon-side clock for the same hop**: `daemon.feed.activity`
is emitted per token at DEBUG (`resolve/feed/sink.go`), and DEBUG records
persist to the workspace `daemon.log` regardless of the verbose flag. So the
daemon half of the quadratic can be measured in the Go layer, independently of
jsdom, at microsecond resolution — a better instrument than the page's. Build
that as the companion assertion.

BUILD with those caveats, webapp layer (plus a Go-layer companion), N = 200
frames in one scenario (this row's N is frames, not scenarios — the one stated
exception to N=20). Budget PROVISIONAL 5/15 ms.

### 4. Turn end → bubble finalized, arm idle in both clients — SPLIT

- **4a webapp.** `feed/rows/turn-ended.ts:drawFeedTurnEnded` (and
  `…Concluded`/`…Errored`/`…Interrupted`); DOM `[data-row-kind="turnEnded"]`,
  `.turn-ended[data-arm=…]`, settled bubble `[data-state="success"]`. Footer
  arm via `footer/strip.ts:drawFooterStatus`, `[data-arm]`. BUILD, webapp
  layer, budget PROVISIONAL 30/80 ms.
- **4b Emacs.** "Arm idle" is `agent-repl-roster--status-by-id` returning to
  a settled arm, on the same roster-push chain as 2b. BUILD, Emacs layer,
  same budget.
- **Defect found, not fixed here:** `FINAL_ANSWER_CLASS = "final-response"`
  (`webapp/src/feed/cards/response.ts:64`) is exported and styled
  (`webapp/src/styles.css:1362`) but **applied by no TypeScript in
  `webapp/src`**. Any 4a assertion written against the final-answer border
  would assert a class that never appears. Reported to the owner; 4a asserts
  on `[data-state="success"]` instead.

### 5. Interrupt keypress → arm interrupted in roster + footer — RESTATE (Emacs half STRUCK)

**STRUCK, Emacs half:** there is no `Interrupt` client in elisp. `rpc
Interrupt` is declared (`service.proto:88`) but no `agent-repl-rpc-interrupt`
verb exists in `lisp/rpc.el` and no elisp command calls it; the only elisp
occurrence of the word is the decoded roster arm `:interrupted`
(`lisp/wire-roster.el:213`). The closest elisp behavior is
`agent-repl-rpc-restart-workspace` (`lisp/verbs.el:433`), which is a
different operation. **There is no interrupt keypress in Emacs to time.**

**RESTATE, webapp:** the origin is a click on
`webapp/src/footer/stop.ts:interruptControl` → `rpc/unary.ts:callUnary("Interrupt")`
→ daemon `daemon.server.interrupt` / `daemon.workspace.interrupt` → roster and
footer pushes → `sidebar/sidebar.ts:mountSidebar` `onPush` →
`sidebar/roster.ts:drawWorkspaceRoster` → `sidebar/row.ts:drawRosterRow`, and
`footer/footer.ts:mountFooter` `onPush` → `footer/strip.ts:drawFooterStrip`.
DOM: `[data-roster-row=<wsid>][data-arm=…]`, `.st.st-<arm>`, footer
`[data-arm]`, local outcome `[data-stop-outcome]`
(`footer/stop.ts:drawInterruptSuccess`).

**The footer's interrupted arm is MOMENTARY, and the assertion must catch it
before it retires.** `daemon/internal/resolve/footer/resolver.go:253` schedules
`retireMomentary` through `clock.AfterFunc` at
`resolve/footer/api.go:192 DefaultMomentaryDwell = 1500ms`, which covers both
`interrupting` and `loading`. An assertion that polls or settles its way to the
arm has a 1.5 s window and will pass by luck; one written against the frame
that carries the arm is deterministic. Assert on the frame.

Note also that `daemon.footer.set_interrupting` fires when the interrupt
REGISTERS, not at real turn end — so this row measures acknowledgement, and the
real end arrives later on `WatchFeed` (that is row 4, not this one).

BUILD, webapp layer, N=20, budget PROVISIONAL 30/80 ms.

### 6. Cancel detached shell / subagent → bubble stopped — BUILD

Apply sites: `webapp/src/feed/cards/shell.ts:drawFeedShell` and
`feed/rows/subagent.ts:drawFeedSubagent` / `drawFeedDetachedSubagent`. The
subagent head is drawn as a bubble head (`feed.ts:bubbleFor` →
`feed/bubble.ts:mountBubble`), so it applies through
`feed-view.ts:drawBody`'s `state.bubble.update(row)` path, **not**
`drawRowBody` — an assertion written against `drawRowBody` would never fire.
DOM: `.shell-bubble[data-state=<outcome>]`, `[data-exit-code]`,
`.shell-stop-outcome[data-outcome]`; `.subagent-head[data-state=…]`,
`.subagent-stop-outcome[data-outcome]`, control `[data-interrupt=<rowid>]`.

Webapp layer, N=20, budget PROVISIONAL 30/80 ms.

### 7. Finish edge → deferred prompt's bubble — BUILD (Go layer)

Daemon operations: `daemon.promptqueue.hold` → `daemon.promptqueue.release` →
`daemon.promptqueue.deliver`. The elisp side has its own finish-edge runner
(`lisp/roster.el:496 agent-repl-roster--run-finish-edges`) but the deferral
itself is daemon-owned, so the assertion belongs where both endpoints are
daemon records (A1) or an rpc/frame pair (A2).

Go layer, N=20, budget PROVISIONAL 30/80 ms.

### 8. Ask raised → card in webapp + marker/modeline in Emacs — SPLIT, modeline STRUCK

- **8a webapp. BUILD.** `feed-view.ts:drawRowBody` case `"question"` /
  `"permission"` → `feed/asks/question.ts:drawFeedQuestion`,
  `feed/asks/permission.ts:drawFeedPermission`. DOM
  `[data-row-kind="question"]`, card `[data-state=<arm>]`. Daemon side:
  `daemon.feed.question` / `daemon.feed.permission`,
  `daemon.sessionwatcher.question` / `.permission`. Budget PROVISIONAL
  30/80 ms.
- **8b Emacs marker. RESTATE.** There is no ask-specific indicator. What
  exists is the generic, **daemon-owned attention marker** delivered on the
  roster: variable `lisp/status.el:731 agent-repl-status--marker-on`, written
  by `status.el:742 agent-repl-status--set-marker` from
  `agent-repl-status-sync-attention` (registered on
  `agent-repl-roster-update-functions` at `status.el:818`), read by
  `status.el:747 agent-repl-status-attention-visible-p`. The assertion is on
  that marker, and says so. Emacs layer, budget PROVISIONAL 30/80 ms.
- **8c Emacs modeline. STRUCK.** The only modeline segment the module owns is
  `agent-repl-link-drain-segment` (`lisp/daemon-link.el:706`/`:723`), which
  reports the daemon LINK, not asks. No ask ever reaches a modeline.

### 9. Ask answered → card retired + next token — BUILD (webapp only)

Retirement arrives as a **feed push replacing the row**, not a local
mutation — so this is a genuine round trip, and asserting a local optimistic
update would assert something that does not happen. DOM: the same
`[data-row-kind="question"]` element transitioning `[data-state="standing"]`
→ answered.

The Emacs half is STRUCK: `AnswerQuestion` / `AnswerPermission`
(`service.proto:92-96`) have no elisp client at all.

**There is no daemon-side origin stamp for this row.** `AnswerQuestion`,
`AnswerPermission` and `AnswerColdGate` in `daemon/internal/server/answers.go`
log **nothing on the success path** — only the verb-level
`daemon.workspace.answer_question` / `.answer_permission` exist, one layer
down. The origin stamp is therefore the page's own click (A4), not a daemon
record, and this row cannot be cross-checked against daemon time. Stated so
that a later reader does not go looking for the record that would close it.

Webapp layer, N=20, budget PROVISIONAL 30/80 ms.

### 10. Cold gate offer drawn; answer → live — BUILD (Go layer)

Apply site exists (`feed/asks/cold-gate.ts:drawFeedColdGate` →
`drawFeedColdGateStanding` / `drawFeedColdGateResolved`, DOM
`[data-row-kind="coldGate"]`, `[data-state="standing"]` →
`[data-state=<choice>][data-arm=<choice>]`), but
`WEBAPP-LAYER-SPEC.md` §G finding 4 records that **no webapp-layer scenario
drives `cold_gate` today**. Building a perf assertion would mean first
building the functional scenario, which is not this document's business.

Host it where the functional coverage already is: `e2e/coldgate_e2e_test.go`,
Go layer (A2), against `coldGateChainTimeout`'s own world. N=20, budget
PROVISIONAL 50/150 ms.

### 11. Select in Emacs → daemon ack → roster/header in both clients — SPLIT

**Grounding correction: the roster update does NOT ride the ack.** The ack's
only landing site is one variable.

| hop | site |
|---|---|
| origin | `lisp/host.el:273 agent-repl-host-select`, driven by `host.el:319 agent-repl-host--on-workspace-activated` — a tab switch IS the select |
| rpc | `agent-repl-rpc-select-workspace` (`lisp/rpc.el:212`) → `SelectWorkspace` (`service.proto:201`) |
| daemon | `server/workspaces.go:(*server).SelectWorkspace` → `workspace/select.go:(*verbs).Select`; operation `daemon.workspace.select` **only** — there is no `daemon.server.select_workspace` |
| ack | `SelectWorkspaceResponse`, `:success` arm → `(setq agent-repl-host-last-selected-id …)` + log `elisp.host.selected`; `:error` → `host.el:251 agent-repl-host--on-refused` |
| push | `workspace/verbs.go:(*verbs).republishRegistry` → `resolve/sidebar/resolver.go:(*resolver).SetRegistry` (`daemon.sidebar.set_registry`) then `SetSelected` (`daemon.sidebar.set_selected`) → `publish.Topic.Publish` — the WHOLE roster is republished, on `WatchWorkspaceRoster` |
| Emacs apply | `roster.el:592 agent-repl-roster-on-push`; attention via `agent-repl-status-sync-attention` |
| webapp apply | `sidebar/sidebar.ts:mountSidebar` `onPush` → `sidebar/roster.ts:drawWorkspaceRoster`; header separately `topbar/topbar.ts:mountTopbar` `onPush` → `drawTopbarView` |

- **11a. BUILD.** Command → ack landing. Emacs layer, both stamps in Emacs
  (A3): advice on `agent-repl-host-select`, advice on the `:on-response`
  path. Budget PROVISIONAL 20/50 ms.
- **11b. BUILD.** Command → roster push applied in Emacs. Emacs layer, second
  stamp on `agent-repl-roster-update-functions`. Separate budget, because it
  is a different chain; PROVISIONAL 30/80 ms.
- **11c. BUILD.** `SelectWorkspace` unary → roster row `[data-current="true"]`
  in the webapp. Webapp layer. Budget PROVISIONAL 20/50 ms.

The "both clients" single assertion is retired (§B).

### 12. Open panel → webview live + first paint — RESTATE, "first paint" STRUCK

**The panel open calls no RPC.** `lisp/panels.el:1112 agent-repl` (`SPC o C`)
and `:1123 agent-repl-simple` (`SPC o c`) dispatch through
`panels.el:1026 agent-repl--toggle` →
`panels.el:995 agent-repl--panels-ensure-host-subscription` (which subscribes
`WatchHostWorkspace` via `agent-repl-rpc-watch-host-workspace`,
`lisp/rpc.el:322`) → the frontend registry's `:open-fn`, which is
`lisp/frontend.el:560 agent-repl--gui-open`. That builds a URL with
`agent-repl-frontend-webview-url` and calls
`frontend.el:372 agent-repl--frontend-ensure-webview-buffer` →
`:169 agent-repl--frontend-make-webview-buffer` (an xwidget-webkit buffer) →
`:472 agent-repl--frontend-display-webview`. `agent-repl-rpc-open-workspace`
(`rpc.el:238`) is a different verb, called only from `lisp/verbs.el:416`.

- **STRUCK: "first paint."** Nothing in the suite can observe rendering
  inside an xwidget. The proof-of-life test's strongest available claim is
  that the live WKWebView answers with the daemon's origin — an existence
  fact, not a paint instant.
- **BUILD, restated:** command invocation → webview buffer exists and the
  `WatchHostWorkspace` subscription is established. Emacs layer, A3 stamps at
  `agent-repl--toggle` and `agent-repl--frontend-display-webview`. Budget
  PROVISIONAL 300/600 ms, which is generous for a local buffer creation and
  should be expected to fall sharply once measured.

### 13. Create/register → roster row in both clients; close → tab gone — SPLIT, webapp-tab half STRUCK

**STRUCK: "tab gone" in the webapp.** There are no workspace tabs in the
webapp. It is one page per workspace (`webapp/src/rpc/page-address.ts`); the
tab bar is Emacs's, and the webapp's own comments say so
(`webapp/src/sidebar/attention.ts:8`, `webapp/src/vocab.ts:9`). The only tabs
in the webapp are **merge bubble tabs** (`feed/merge/merge-body.ts`,
`feed/merge/tab-strip.ts`), which are a different feature.

- **13a. BUILD.** `CreateWorkspace` / `RegisterWorkspace` → roster row drawn.
  Webapp: `sidebar/roster.ts:drawWorkspaceRoster`, DOM
  `[data-roster-row=<wsid>]`. Daemon operations `daemon.workspace.create`,
  `daemon.workspace.register`, `daemon.sidebar.row`. Webapp layer, budget
  PROVISIONAL 50/150 ms.
- **13b. BUILD.** `CloseWorkspace` (`daemon.workspace.close`) → the tab leaves
  `lisp/roster.el:113 agent-repl-roster--tab-order`, via
  `roster.el:382 agent-repl-roster-reconcile`. Emacs layer, same budget.

### 14. Daemon loss → footer disconnected + Emacs modeline — SPLIT, both BUILD

- **14a webapp.** Link death is detected in
  `webapp/src/rpc/streams.ts:watchStream` → `reportUnreachable` →
  `webapp/src/failure/sink.ts:daemonUnreachable`; the footer's own
  `disconnected` arm arrives on its stream. DOM: footer
  `[data-arm="disconnected"]`, warning chip
  `#topbar .topbar-warnings[data-local-arms~="daemonUnreachable"]`.
  **There is no daemon-side "the daemon is gone" push, and there cannot be** —
  a dead daemon pushes nothing. The disconnected state is drawn entirely
  client-side from the stream dying. What the daemon does publish is the
  shim link state (`resolve/footer/resolver.go:(*resolver).OnLink`,
  `daemon.footer.on_link`) and participant liveness (`SetParticipants`,
  `daemon.footer.set_participants`, fed from
  `server/streams.go:(*server).holdParticipant`) — different facts that the
  footer also draws as not-connected. **The assertion must kill the daemon and
  time the client's own detection**, not wait for a push that will never come.
  **Structural note governing the budget:** the reconnect backoff in
  `streams.ts:waitBackoff` is 250 ms doubling to 5000 ms, so nothing on this
  path can be faster than the stream error itself surfaces. Webapp layer,
  budget PROVISIONAL 100/300 ms.
- **14b Emacs.** `lisp/daemon-link.el:723 agent-repl-link--refresh-indicator`
  sets `agent-repl-link-drain-segment` (computed by `:706
  agent-repl-link--compute-drain-segment`) and calls
  `force-mode-line-update t`; installed into `global-mode-string` by
  `daemon-link.el:730 agent-repl-link-install-indicator`. This is the
  sanctioned "the subject IS the rendered string" exception in
  `EMACS-LAYER-SPEC.md`. Emacs layer, budget PROVISIONAL 100/300 ms.
  **Structural note:** `agent-repl-link--schedule-reconnect`
  (`daemon-link.el:415`) backs off 1.0 s → 5.0 s, so detection, not
  reconnection, is what 14b bounds.

### 15. Daemon back → link up, roster rehydrated, no duplicate tabs — RESTATE

- **"No duplicate tabs" in the webapp is STRUCK** — no tabs (see 13). The
  real anti-duplication mechanism is that
  `webapp/src/feed/feed.ts:openAndTail` re-issues `OpenFeed` on **every**
  reopen and calls `feed-view.ts:applyPage(page, "replace")`, which runs
  `clearRows()` before re-adopting. The assertion that matters is that the
  row set is REPLACED, not appended — a correctness invariant, and it belongs
  in the functional suite, not in a latency budget.
- **The feed is NOT republished on adoption**, and an assertion that waits for
  it to be will hang. `daemon/internal/handover/handover.go:(*Views).PublishViews`
  (`daemon.handover.publish_views`) republishes topbar, footer, holds and
  sidebar only — via `publish/topic.go:(*Topic).Republish`, which deliberately
  bypasses `Publish`'s dedupe. Feed rehydration is **client-driven**: the page
  calls `OpenFeed` again for a fresh page and a fresh tail token. So this row
  times a client action, not a server push.
- **BUILD, restated:** daemon back → `consume`'s retraction block
  (`ctx.failures.retract("daemonUnreachable")` + `opts.onReconnected?.()`) →
  feed rehydrated. Webapp layer. Emacs equivalent: link up at
  `daemon-link.el:339 agent-repl-link--accept-primary` (the one place the
  link becomes up; runs `agent-repl-link-up-functions`, where
  `agent-repl-roster-on-link-up` resubscribes). Emacs layer, separate
  assertion. Budget PROVISIONAL 300/800 ms each, and both are dominated by
  the backoff cadences named in 14 — see §E.

### 16. Handover announce → adopted → promoted; refused-send gap — BUILD (Go layer)

Operations, and note the prefix: **the `rollout` package does not prefix
`daemon.`** — its operations are bare `rollout.handover`, `rollout.transfer`,
`rollout.adopt`, `rollout.adopt_host`, `rollout.adopt_web`,
`rollout.adoption_window` (`rollout/controller.go:15-30`), while the rest of
the chain does prefix it: `daemon.handover.quiesce`,
`daemon.handover.drain_intake`, `daemon.handover.publish_views`,
`daemon.wsm.promote`, `daemon.shimclient.adopt`, `daemon.boot.adopt`,
`daemon.server.adopt_host`, `daemon.server.adopt_web`,
`daemon.server.transferred`. A reader matching on a `daemon.` prefix silently
loses the announce half of the chain.

The whole chain is daemon records on one host, so A1 alone times it.

**The refused-send gap is a real, named interval with two arms.**
`rollout/handover.go:(*controller).transfer` calls `Quiesce` before detaching,
taking a `wsm.PolicyHold` under `handover.QuiesceHolder = wsm.HolderRestart`.
While the gap stands the OUTGOING daemon refuses with arm `transferring_away`
and the INCOMING one with `not_yet_adopted` (both `daemon/internal/server/refuse.go`,
arms declared `workspace/refusal.go:28,30`), logged at WARN under
`daemon.refusal.unlanded_arm.standing`. Prompts past resolve are HELD by the
lease, not lost; the gap closes at `handover/handover.go:(*Intake).DrainIntake`
(`daemon.handover.drain_intake`). **Time the gap as
`daemon.handover.quiesce` → `daemon.handover.drain_intake`** — two records,
one runtime — rather than by probing for refusals, which samples the gap
instead of measuring it.

**Structural floor, and the reason this budget cannot be tightened
arbitrarily:** `daemon/internal/rollout/handover.go:22 adoptionPoll = 25ms`
and `daemon/internal/rollout/adopt.go:53 manifestPoll = 25ms`, plus
`rollout/spawn.go:134 ProcessSpawner.Poll = 50ms`. Three polled edges put a
~100 ms floor under the chain before any work happens.

Go layer, N=20, budget PROVISIONAL 500/1500 ms, reusing
`HandoverChainTimeout`'s world shape.

### 17. Cold start — BUILD, and 17a is nearly free

- **17a Emacs launch → daemon link up, warm build.** Chain:
  `config.el:386 (add-hook 'emacs-startup-hook #'agent-repl-daemon-schedule-ensure)`
  → `lisp/daemon.el:1008 agent-repl-daemon-schedule-ensure` (idle timer,
  `agent-repl-daemon-startup-idle-seconds` default **0**, `daemon.el:130`) →
  `daemon.el:1023 agent-repl-daemon-ensure` → `:985 agent-repl-daemon--begin`
  → probe/adopt or `agent-repl-daemon--build-and-start` →
  `daemon-link.el:297 agent-repl-link-connect` → `:320
  agent-repl-link--open-primary` → `:339 agent-repl-link--accept-primary`.
  Log operations along it: `elisp.daemon.ensure-scheduled`,
  `elisp.daemon.addr-absent`, `elisp.daemon.linking`, `elisp.link.open-pending`,
  `elisp.roster.subscribed`.

  **This row already has a measurement and a harness.** `emacs_test.go`'s
  `Emacs.record` / `reportPhases` time each boot phase on every run, and
  `EMACS-LAYER-SPEC.md`'s bounds table already reports `daemonLinkBound`
  observed max **265 ms** (spread 263-265 ms) and `doomBootBound` observed max
  **1.162 s** (spread 1.041-1.162 s). The proposed 800/2000 ms budget is
  consistent with those and should be replaced by the p50/p95 of 20 recorded
  `daemon-link` phases rather than re-derived. Emacs layer; the only new work
  is the percentile computation over `phases`.

  **Structural note:** `daemon.el:789 agent-repl-daemon--boot-tick` polls at
  `agent-repl-daemon-boot-poll-interval-seconds = 0.25s` (bounded by a 30 s
  timeout), so cold start has a 250 ms quantum in it by construction.

- **17b daemon start → first roster push. BUILD, but expect a near-zero
  number, and know why.** The roster is published BEFORE anything is served:
  `daemon/cmd/claude-repld/graph.go`'s `Prime` hook calls
  `workspace/register.go:(*verbs).PublishRegistry` ("published the opening
  roster") ahead of the listener, and `publish.Topic.Subscribe` **replays the
  latest value to a new subscriber**. So a client subscribing after boot gets
  the roster immediately with no wait, and this row degenerates to subscribe
  latency. That is worth asserting — a regression here would mean the prime
  ordering broke — but the budget must be set from the measurement, and the
  proposed 200/500 ms is almost certainly two orders too generous. Operations:
  `daemon.boot.started` → `daemon.workspace.register` →
  `daemon.sidebar.set_registry` → `daemon.boot.run`. Go layer, A2.

### 18. Subagent spawn → bubble; nested rows land while parent streams — BUILD

Nested rows arrive on a **per-bubble** `WatchFeed`:
`webapp/src/feed/bubble.ts:mountBubble.openWatch` `onPush` →
`child.upsert(...)`, laid out by `feed/renderers.ts:defaultBubbleBody` →
`arrangeSubfeedRows` (plus `nestSlot` for intra-feed `parent` nesting). DOM:
outer `[data-row-kind="detachedSubagent"]` or `[data-unit="subagent"]`,
`[data-expanded]` mirrored onto the `<article>` by `feed-view.ts:mirrorState`,
child host `[data-feed=<bubbleFeedId>]`, nesting `[data-nest]`.

The "while parent streams" half is a CONCURRENCY claim, not a latency one:
assert that the nested row's apply latency does not degrade while the parent
feed is applying response frames — i.e. the same p50/p95 with and without a
streaming parent. Webapp layer, N=20 each arm, budget PROVISIONAL 30/80 ms.

### 19. Detached spool output → bubble update; TaskOutput poll — BUILD, inherent-floor

**Owner ruling 2 applies: the 50 ms spool poll is an ACCEPTED INHERENT FLOOR.**
The budget stands at 60/150 ms and the poll is not a defect candidate. What
follows is the grounding the budget rests on, recorded so the number stays
legible — not an argument against the ruling.

The 50 ms figure is the cadence this suite runs the sidecar at:
`e2e/world_test.go:810-811` passes `--poll-interval 50ms --rescan-interval
200ms`. The sidecar's compiled-in defaults are different
(`shim-sidecar/main.go:70-71`: `DefaultPollInterval = time.Second`,
`DefaultRescanInterval = 30 * time.Second`, and nothing in launchd overrides
them), and `shim-sidecar/cycle.go:83 recoverTick = 50ms` is a third, unrelated
store-recovery cadence. Three different 50 ms-adjacent numbers sit near this
row, so the assertion's doc comment must name **which** one it was measured
under: `--poll-interval`, from `world_test.go:810`.

**The poll actually on this path is not only the sidecar's.** The shim
re-asks the store for a detached run's rows on its own cadence:
`agent-shim/claude/shim/src/store/reader.ts:262 BASH_ROW_RECHECK_MS = 25ms`
for shell runs and `:294 AGENT_ROW_RECHECK_MS = 250ms` for agents, with
`:281 BASH_CONCLUDED_WINDOW_MS = 500ms` as the backstop for a run that retires
without a terminal row. **The 250 ms agent recheck dominates a 60/150 ms
budget outright**, so this row is built for the SHELL case (25 ms recheck) and
the agent case gets its own, larger, separately-derived budget rather than
being folded in and quietly failing.

Go layer, N=20, budget PROVISIONAL 60/150 ms for the detached-shell case, at
`--poll-interval 50ms` — inherent-floor, per ruling 2.

### 20. Feed replay on open, 500 rows → full page — RESTATE

**"500 rows" is STRUCK: the page size is 50**, set daemon-side at
`daemon/internal/resolve/feed/api.go:200 DefaultPageSize = 50`. Nothing in
`webapp/src` names a page size at all. A 500-row page is not a thing the
system produces.

BUILD, restated as: `OpenFeed` → `feed-view.ts:applyPage(page, "replace")`
complete for one 50-row page, and separately one `GetFeedPage` →
`applyPage(page, "prepend")` via `feed-view.ts:loadOlder`. `applyPage` loops
`adopt()` per row and then runs ONE `announce()` / `arrangeSubfeedRows` pass,
with scroll anchoring through `scroll.ts:captureFeedAnchor` /
`restoreFeedAnchor`. DOM: `[data-feed="root"] [data-feed-row]` count, walk
control `button.feed-load-more[data-load-more]`.

Webapp layer, N=20, budget PROVISIONAL 200/500 ms for a 50-row page.

### 21. Roster of 10+, one status change propagates, cost flat in N — RESTATE

**"Cost flat in N" is STRUCK: the implementation is O(N) by construction.**
`webapp/src/sidebar/sidebar.ts:mountSidebar`'s `onPush` calls
`sidebar/roster.ts:drawWorkspaceRoster` and then `body.replaceChildren(drawn)`
— **the whole rail is rebuilt on every push**, and the topbar does the same
(`topbar/topbar.ts:drawTopbarView` + `strip.replaceChildren(...)`). A row
asserting flat cost would fail against correct code.

BUILD, restated as two honest claims:

- an ABSOLUTE bound on apply latency at N=10 workspaces, and
- a LINEARITY check: apply latency at N=20 is under ~2.5x the latency at
  N=10 (i.e. linear with a small constant, not quadratic). Quadratic growth
  would be a real defect; linear growth is the design.

Webapp layer, N=20 samples per roster size, budget PROVISIONAL 20/50 ms at
N=10 workspaces.

### 22. Ten prompts in 2 s all accepted, ordered, per-prompt stable — BUILD (Go layer)

Daemon operations: `daemon.promptqueue.accept`, `.classify`, `.hold`,
`.interject`, `.deliver`, `.drop`. Acceptance and ordering are functional
claims that belong in the functional suite; the PERF claim is that the
per-prompt latency distribution under a 10-in-2 s burst is not worse than
row 1a's single-prompt distribution by more than the budget.

Go layer, N=20 bursts (200 prompts total), budget: per-prompt p95 within
2x row 1a's measured p95 — a RELATIVE budget, deliberately, because an
absolute one would just restate row 1.

### 23. Idle CPU, daemon + shim + sidecar, one open workspace — SPLIT

- **23a STRUCTURAL. BUILD, and this is the one that actually catches the
  defect.** The thing that makes an idle process burn CPU is a timer, and
  every timer in the stack is enumerated in §E. Assert, from the structured
  logs of a world left idle for a fixed window, that no operation fires more
  often than its declared cadence and that no unnamed periodic operation
  appears at all. This needs no new instrument and no process metrics.
- **23b MEASURED. BLOCKED on a proposal.** `harness.Daemon` keeps its
  `*exec.Cmd` unexported (`daemon/integration/harness/daemon.go:209 cmd
  *exec.Cmd`); there is no exported pid or `ProcessState` accessor, so
  nothing in `e2e/` can sample a process's CPU. See §G proposal P3. Not
  built until the owner rules.

### 24. Memory growth over 200 turns — BLOCKED on the same proposal

Same obstruction as 23b: no exported handle on any spawned process. Also
platform-bound — the Emacs layer runs in a Linux container where `/proc` is
available (`emacs_sandbox_test.go:196`, `emacs_coldstart_e2e_test.go:72`
already read it), but the Go layer runs on the developer's macOS host too, so
a `/proc`-based sampler would silently skip on half the runs. Not built until
the owner rules on P3.

---

## D. The harness

### D1. The perf recorder

One type, in a new `e2e/perf_test.go`, modeled directly on the existing
`emacs_test.go` `phase` / `record` / `reportPhases` trio rather than invented:
that trio already implements this document's core discipline (measure on
every run, print on a PASSING run, derive the bound from the print).

```
type PerfRecorder struct  // name, samples []time.Duration
  Record(d time.Duration)
  P50() time.Duration     // sorted[9]  of 20 — the lower median
  P95() time.Duration     // sorted[18] of 20 — ceil(0.95*20)=19, 1-indexed
  Report(t *testing.T)    // ALWAYS logs n, min, p50, p95, max — pass or fail
  Assert(t *testing.T, p50Budget, p95Budget time.Duration)
```

Rules, each with its reason:

- **`Report` runs on every outcome.** A budget must be a stated multiple of
  an observed healthy maximum, and the only run that produces that
  observation is a run that passed. This is verbatim the reason
  `emacs_test.go:reportPhases` gives.
- **No interpolation.** With N=20 an interpolated percentile is a value no
  sample took. A budget must be violated by a real observation.
- **A sample is never discarded.** No trimming, no outlier rejection, no
  "best of". The p95 exists precisely to hold the tail; removing the tail
  removes the assertion.
- **N=20 is fixed** and asserted: a recorder finishing with fewer than 20
  samples FAILS rather than reporting a percentile over a short sample. The
  one exception is row 3b, whose N is 200 frames and which says so.
- **The webapp-layer recorder is a TypeScript twin** in
  `webapp/test/webapp-layer/perf.ts`, with the identical percentile rule, and
  it ships its finished p50/p95 to Go as ONE `ClientLog` record (A6) so the
  Go failure output names the number. One aggregated record, never one per
  sample — `clientlog-throttle.ts` would drop the latter.

### D2. The calibration guard

A latency budget on a saturated box measures the box. The guard runs ONCE per
perf phase, before any assertion, and produces one of three outcomes.

- **The loopback probe.** 100 sequential `DaemonHealth`
  (`endpoint_daemon_health.proto`) unary calls against the world's own
  daemon. This is the cheapest real rpc in the service and exercises the
  exact transport every measured hop rides.
- **The fixed CPU probe.** A deterministic, allocation-free integer loop with
  a fixed iteration count, timed. It is fixed so that its result is
  comparable across machines and across runs of the same machine, which a
  wall-clock-only probe is not.
- **The verdict.** Both probes have their own recorded baselines (D3). If
  either exceeds its baseline by more than the calibration factor, the phase
  reports **`DECLINED`**.

`DECLINED` is neither pass nor fail:

- it does NOT call `t.Fatalf` and it does NOT call `t.Skip` silently;
- it logs, at the top of the phase's output, the word `DECLINED`, both probe
  numbers, both baselines, and the ratio that tripped it;
- every assertion in the phase then reports its samples (D1) and asserts
  nothing;
- the run's summary carries a `DECLINED` line so a green run that measured
  nothing cannot be mistaken for a green run that measured something.

The reason it is not a skip: a skip is a thing a reader's eye passes over,
and the failure mode this guard exists to prevent — a perf suite that quietly
stopped asserting — looks exactly like a skip.

**AN AREA WHOSE SAMPLES COME FROM A CHILD DECLINES BEFORE IT STARTS THE CHILD.**
The three bullets above are written for a measurement already in hand: report
it, assert nothing. `TestPerfWebappLayer` has no measurement at calibration
time — it has a vitest child that drives 80 real chain traversals under
`WebappLayerPerfTimeout`. MEASURED, on a box saturated enough to decline: the
guard DECLINED, the area started the child anyway, the child did not finish
inside the 60 s bound, and the area FAILED on `child.WaitFor` — a red reported
by a phase that had already decided it would assert nothing. A decline that can
produce a failure is not a decline. So `perfDeclineArea` (`perf_harness_test.go`)
ends such an area at the calibration, before the child is started: it records
one `DECLINED` summary row per assertion the area owns — so the phase summary
says exactly what it would have said had the child run — and skips with the
probe numbers in the reason. Covered by `perf_decline_test.go`.

### D3. Baselines and the regression check

- One file per assertion, under `e2e/testdata/perf/<assertion>.json`, holding
  the recorded p50 and p95, the host's core count, and the date. Committed.
- Each assertion checks its measurement against BOTH:
  1. its absolute budget (section C), and
  2. its baseline, failing on a **>20 % regression** in p50 or p95.
- The absolute budget catches "this was always too slow"; the baseline check
  catches "this got slower", which is the one a suite of absolute budgets
  with headroom will never catch.
- A baseline is only ever rewritten deliberately, by a named make target
  (`make -C e2e perf-baseline`), never automatically by a failing run. A
  baseline a red run can rewrite is not a baseline.
- Baselines are recorded per host core count, because §C's numbers were taken
  on a 16-core host (`SPEC.md`'s own measurements say so) and a 4-core CI box
  is a different machine. A baseline whose core count does not match the
  running host is reported and NOT enforced — the calibration guard is what
  covers that case.

### D4. The serial perf phase

**Nothing in this document runs under `-parallel 8`.** `SPEC.md`'s own
measurements settle it: per-test wall time inflates with concurrency, and at
`-parallel 12` a run went 20.7 s → 38.4 s and lost a test to a bound. A
latency measurement taken while seven other worlds are running measures the
scheduler.

- Every perf test is in one file per layer, guarded by a build tag `perf`, and
  **declares no `t.Parallel()`** — which is already how the Emacs-layer files
  behave.
- Invocation, from `modules/app/agent-repl/e2e`, added to the Makefile beside
  `test` and `coverage`:

  ```
  perf:
  	TMPDIR=/tmp go test ./... -tags perf -run 'TestPerf' -count=1 \
  		-timeout $(GOTEST_TIMEOUT) -parallel 1 -v
  ```

  `-parallel 1` is stated explicitly rather than inherited, so that a future
  edit to `GOTEST_PARALLEL` cannot silently parallelize the perf phase.
- It runs **after** the functional suite, never interleaved: a red functional
  run makes every perf number meaningless, and measuring first wastes the
  time it takes to find that out.
- `-v` is not optional here: `Report` (D1) writes through `t.Logf`, and the
  whole discipline depends on a passing run printing its numbers.
- **Durations land in the run's duration table the same way the Emacs layer's
  already do**: `Report` emits one `t.Logf` line per assertion in the
  established `emacs phase <name> took <d>` shape, so the existing reader —
  `go test -v` — surfaces them with no new tooling. The perf phase adds a
  final summary block listing every assertion, its n/p50/p95, its budget, its
  baseline, and the calibration verdict.

---

## E. Structural assertions — where the latency is a poll, not a computation

For several rows the honest cause of latency is a cadence, not work. A
percentile over a polled edge measures the poll. These assertions are
therefore STRUCTURAL: they assert the cadence is what it is declared to be,
and they are cheap, deterministic, and immune to machine load.

### E1. The cadence table (grounded; the proposal's figures corrected)

| system | site | constant | value |
|---|---|---|---|
| daemon | `daemon/internal/commandfile/api.go:74` | `DefaultInterval` | **250 ms** — poll cadence AND settling window; `ingress.go:115` only ingests a file whose mtime is ≥ `Interval` old, so command-file ingress is structurally 250-500 ms |
| daemon | `daemon/internal/rollout/handover.go:22` | `adoptionPoll` | 25 ms |
| daemon | `daemon/internal/rollout/adopt.go:53` | `manifestPoll` | 25 ms |
| daemon | `daemon/internal/rollout/spawn.go:134` | `ProcessSpawner.Poll` | 50 ms |
| daemon | `daemon/internal/dlog/surfaces.go:16` | `scanInterval` | 30 s (log cap scan) |
| daemon | `daemon/internal/resolve/footer/api.go:192` | `DefaultMomentaryDwell` | **1500 ms** — how long the footer's `interrupting` / `loading` statuses stand before `retireMomentary`; governs row 5 |
| daemon | `daemon/internal/shimclient/transport.go:45` | `defaultBackoff` | 100 ms ×2 → 5 s (daemon→shim redial) |
| daemon | `daemon/internal/drain/controller.go:44` | `DefaultSweepEvery` | 5 min (idle sweep) |
| daemon | `daemon/internal/server/api.go:504` | `announcementFlush` | 2 s |
| sidecar | `shim-sidecar/main.go:70` | `DefaultPollInterval` | **1 s compiled-in default** (launchd ships it unchanged); `e2e/world_test.go:810` overrides to 50 ms — the **accepted inherent floor** per ruling 2 |
| sidecar | `shim-sidecar/main.go:71` | `DefaultRescanInterval` | **30 s compiled-in default**; `e2e/world_test.go:811` overrides to 200 ms |
| sidecar | `shim-sidecar/cycle.go:83` | `recoverTick` | 50 ms — store-recovery heartbeat, reads no files |
| shim (TS) | `shim/src/store/reader.ts:262` | `BASH_ROW_RECHECK_MS` | **25 ms** — governs row 19's shell case |
| shim (TS) | `shim/src/store/reader.ts:294` | `AGENT_ROW_RECHECK_MS` | **250 ms** — dominates any sub-250 ms budget on the detached-agent path |
| shim (TS) | `shim/src/store/reader.ts:281` | `BASH_CONCLUDED_WINDOW_MS` | 500 ms backstop |
| webapp | `webapp/src/clock.ts` | `DEFAULT_TICK_MS` | 1000 ms — ONE shared page ticker; `sidebar/row.ts:19`, `lifecycle/lifecycle.ts:37`, `feed/ticking.ts:7`, `rpc/context.ts:36` all declare the invariant that components subscribe to it and never call `setInterval` |
| webapp | `webapp/src/rpc/streams.ts:210` | reconnect backoff | 250 ms → 5000 ms |
| webapp | `webapp/src/lifecycle/lifecycle.ts:568` | adopt retry | 250 ms → 5000 ms, 60 s budget |
| webapp | `webapp/src/sidebar/attention.ts:144` | `ATTENTION_PHASE_MS` | 500 ms per blink phase |
| webapp | `webapp/src/clientlog-throttle.ts:70` | `DEFAULT_INTERVAL_MS` | 2000 ms log flush |
| Emacs | `lisp/daemon.el:789` | `agent-repl-daemon-boot-poll-interval-seconds` | 250 ms (boot poll, 30 s cap) |
| Emacs | `lisp/daemon-link.el:415` | reconnect backoff | 1.0 s → 5.0 s |
| Emacs | `lisp/status.el:2313` | `agent-repl-ready-view-fade-delay` | 2.0 s (the repeating state-poll timer; comments calling it a "1 Hz heartbeat" are wrong about the period) |
| Emacs | `lisp/status.el:774` | `agent-repl-status-blink-schedule` | 0.0/0.5/1.0/1.5/2.0 s |
| Emacs | `lisp/autosave.el:120` | autosave | 300 s |
| Emacs | `lisp/host.el:265` | `agent-repl-host-handover-retry-delay` | 200 ms |

### E2. The structural assertions

1. **No poll on the prompt happy path.** The chain in row 1 must contain no
   entry from E1. Assert it by shape: the daemon's records between
   `daemon.prompthandler.submit` and the feed push carry no operation with a
   declared cadence.
2. **Command-file ingress is the 250 ms it declares.** Assert
   `daemon.commandfile.run`'s logged `interval_ms`
   (`commandfile/ingress.go:57` already logs it) equals `DefaultInterval`, and
   that no row in §C routes through it.
3. **One page ticker.** Assert `setInterval` appears in `webapp/src` only in
   `clock.ts` — the invariant four files already declare in prose, never
   enforced. This is the assertion that keeps row 23a true over time.
4. **Every declared timer is registered.** Emacs routes keyed timers through
   `lisp/core.el:67 agent-repl--register-timer` into `agent-repl--timers`,
   with `agent-repl--required-timer-keys` pinning the required set. Assert the
   live set equals the declared set — an unregistered repeating timer is
   exactly the idle-CPU defect row 23 was aimed at.
5. **The sidecar cadence a perf budget was measured under is stated.** Assert
   the running sidecar's `--poll-interval` matches what row 19's doc comment
   claims, so the budget can never drift away from its premise. Per ruling 2
   this asserts the floor is PRESENT and unchanged — it is never framed as a
   cost to remove.
6. **No `time.Sleep` on any measured path.** `time.Sleep` appears zero times
   in non-test `daemon/internal/**` today, and the command-file ingress is a
   deliberate poll rather than fsnotify (`commandfile/ingress.go:46-51`).
   Assert the zero holds: a sleep introduced onto a happy path is exactly the
   defect a latency budget would report as an unexplained regression, and the
   structural check names it directly instead.

---

## F. The first eight to implement

Rows 1, 2, 3a, 5, 8, 11, 14, 17 — restated per §C, with the exact assertion
shape. Every budget below is PROVISIONAL and every one is replaced by the
measurement on first green run.

| # | assertion | layer | origin stamp | terminal stamp | clock | budget p50/p95 |
|---|---|---|---|---|---|---|
| F1 | `agent-repl-send` → daemon accepted the prompt | Emacs | advice on `agent-repl-send` | daemon record `daemon.prompthandler.submit`, same `workspace_id` | A3 + A1 | 20/50 ms |
| F2 | `agent-repl-send` → composer erased | Emacs | advice on `agent-repl-send` | advice on `agent-repl--input-accepted` | A3 only | 15/40 ms |
| F3 | response frame received → response bubble in DOM | webapp | captured real clock at `consume` entry | `consume` return, first `[data-unit="response"]` present | A4 + A5 | 20/50 ms |
| F5 | interrupt click → footer arm `interrupted` | webapp | real clock at `interruptControl`'s click handler | `consume` return for the footer frame carrying the arm | A4 + A5 | 30/80 ms |
| F8 | ask raised → question card `[data-state]` drawn | webapp | daemon `daemon.feed.question` is the ORIGIN in daemon time; the page measures `consume`→DOM only | `consume` return, `[data-row-kind="question"]` present | A4 + A5 | 30/80 ms |
| F11 | `agent-repl-host-select` → `SelectWorkspace` ack landed | Emacs | advice on `agent-repl-host-select` | advice on the ack path setting `agent-repl-host-last-selected-id` | A3 only | 20/50 ms |
| F14 | daemon killed → `agent-repl-link-drain-segment` reports disconnected | Emacs | Go `time.Now()` at the kill, plus the elisp stamp at the segment refresh | advice on `agent-repl-link--refresh-indicator` | A3 + A2 | 100/300 ms |
| F17 | Emacs launch → daemon link up | Emacs | the existing `daemon-link` phase in `Emacs.record` | same | already recorded | derive from 20 recorded phases; observed max to date 265 ms |

Why these eight first:

- **F2, F11 and F17 need no new instrument at all.** F2 and F11 are two
  in-Emacs stamps on public boundaries, using the advice idiom
  `emacs_composer_e2e_test.go` already established. F17's numbers are already
  being recorded and printed on every run; only the percentile computation is
  new. They are the proof that the recorder and the reporting work, before
  anything harder depends on them.
- **F1 and F14 are the first cross-runtime pairs**, and they exercise the
  A1 timestamp contract across two runtimes on one host, which every
  remaining Go-layer row depends on.
- **F3, F5 and F8 are the first three webapp rows**, and they share one
  instrument: the `consume` wrapper (A5) and the captured real clock (A4).
  Building the three together is what makes the wrapper worth writing; each
  alone would not be.
- **Not in the first eight, deliberately:** 3b (the instrument's granularity
  is marginal at a 5 ms budget — build it once F3 has produced a real
  distribution to size it against), 23 and 24 (blocked on P3), 10 (needs a
  functional cold-gate webapp scenario that does not exist).

### F-shape, worked once

The Emacs shape, in full, because every Emacs row is this shape:

- Arrange: install a stamping observer with `advice-add … :before` on the two
  public boundaries, each pushing `(float-time)` onto one list variable.
  Both advices are removed at teardown.
- Act: drive the command through `call-interactively`, 20 times, with the
  scenario reset between samples.
- Assert: read the two lists back in ONE `emacsclient` eval; pair them by
  index; `PerfRecorder.Record` each delta; `Report`; `Assert` against budget
  and baseline.

The webapp shape:

- Arrange: capture the real clock before `mountApp` fakes timers; wrap
  `consume` through the layer's own harness seam.
- Act: drive 20 samples in one vitest file.
- Assert: compute p50/p95 in `perf.ts`, assert page-side, and ship the
  numbers to Go in ONE `ClientLog` record so the Go failure output names them.

---

## G. Proposals flagged for the owner — NOT built

Each of these would need a production or harness change. None is made here.

**P1 — a client `sent_at_ms` on `SubmitPrompt`.** Would let row 1's full
Emacs-to-webapp chain be measured end to end with one number instead of two.
Cost: a proto field carried for a test's benefit, and a client clock the
daemon would have to decide whether to trust. Recommendation: **decline**.
Rows 1a and 1b measure the two halves honestly and their sum is the answer.

**P2 — a real-clock escape in the webapp integration harness.** Capture
`Date.now` (or `performance.now`) before `vi.useFakeTimers` and expose it on
`MountedApp`, exactly as `yieldToIo` already captures the real
`setImmediate`. This is `webapp/test/**` only, no production touch, and F3/F5/F8
depend on it. Recommendation: **accept** — it is the same seam, for the same
reason, in the same function.

**P3 — an exported process handle on `harness.Daemon`.** Rows 23b and 24 are
unbuildable without one: `daemon.go:209 cmd *exec.Cmd` is unexported and there
is no pid or `ProcessState` accessor. Additive (`func (d *Daemon) PID() int`),
zero behavior change, and every existing caller is unaffected. Also needed:
the same for `Store` and `Sidecar`. Note the platform split — `/proc` exists
in the Emacs layer's Linux container but not on a macOS host — so a sampler
would need a portable path or a stated, loud skip. Recommendation: **owner's
call**; the structural row 23a covers the defect 23b was aimed at, at no cost.

**P4 — a perf marker operation in the webapp.** Rows measured page-side ship
their result through `ClientLog`, which already exists. No new operation is
needed, but the operation NAME must be registered wherever the suite's
operation vocabulary is checked. Recommendation: **accept**, as part of F3.

---

## H. Findings this work surfaced, reported not fixed

1. `webapp/src/feed/cards/response.ts:64 FINAL_ANSWER_CLASS` is exported and
   styled (`webapp/src/styles.css:1362`) but applied by no TypeScript in
   `webapp/src` — the final-answer border does not exist at runtime.

2. `e2e/webapplayer_e2e_test.go`'s `WebappLayerTimeout` is
   `300 * time.Second` while the doc comment above it, and
   `WEBAPP-LAYER-SPEC.md` §E, both say 10 s. The constant and its stated basis
   disagree; one of them is wrong.

3. `lisp/status.el`'s comments describe the state-poll timer as a "1 Hz
   heartbeat"; the actual period is `agent-repl-ready-view-fade-delay` = 2.0 s
   (`status.el:2313`).

4. The proposal's premise that "the sidecar polls spools at 50 ms" describes
   this suite's harness override, not the sidecar. Production is 1 s / 30 s
   (`shim-sidecar/main.go:70-71`).

5. `proto/src/agentrepl/v1/service.proto` declares `Interrupt`,
   `AnswerQuestion`, `AnswerPermission` and `AnswerColdGate`, and elisp has a
   client for none of them. That is a scope fact, not necessarily a defect,
   but four rows of the proposal assumed otherwise.

6. **Standing streams break the operation-naming convention.**
   `daemon/internal/server/streams.go:serveTopicWith` logs
   `log.Debug(rpc, "accepted a standing stream", nil)`, so the operation
   string is literally `"WatchFeed"` / `"WatchFooter"` /
   `"WatchWorkspaceRoster"` rather than `daemon.<pkg>.<verb>`. Every
   log-reading tool, this document's harness included, must special-case it.

7. **Three declared correlation fields are never populated.** `request_id`,
   `agent_repl_session_id` and `claude_session_id` are reserved and promoted
   in `daemon/internal/dlog/record.go:41-46` and written by nothing in the
   daemon. A reader correlating on them gets silence, not an error.

8. **Turn identity has two spellings and no promoted field.** `context.turn`
   in `promptqueue` / `workspace` / `resolve/feed`, `context.turn_id` in
   `sessionwatcher`. Likewise workspace identity appears both as the promoted
   `workspace_id` and as a raw `context.workspace` key in `wsm`,
   `workspace/verbs.go`, `rollout` and `handover`.

9. **The response fold is quadratic over a turn.** The daemon re-sends the
   whole accumulated row per token (`resolve/feed/response.go`,
   `fold.markdown += …` then `proto.Clone`), and the webapp rebuilds the row
   body whole. Not a defect claim — it may well be the right trade at real
   row sizes — but it is the mechanism row 3b's growth check exists to watch,
   and it should be a deliberate choice rather than an unnoticed one.

10. `e2e` is in neither `bin/test-all.sh`'s `ALL_SUITES` nor
   `daemon/internal/workspace/merge/suiteselect.go`'s path map
   (`EMACS-LAYER-SPEC.md`, "Registration"). A perf phase added to a suite
   outside the merge gate is a perf phase that stops being true; whatever
   ruling covers the functional suite covers this one.

---

## I. Phase 1, as built and measured (2026-09-04)

Everything in this section is a measurement or a consequence of one. Nothing in
it is an estimate.

### I1. The host, and what a number here means

A 16-core macOS host, shared with sibling agent suites — which is the whole
reason §D2's calibration guard exists, and, as I5 records, the reason it is not
yet strong enough. Every figure below is from **three full serial runs**
(`make -C e2e perf-only` at `-count=3`) taken at load average 6.1-8.3 with the
calibration guard passing on all three.

### I2. The calibration thresholds, measured

| probe | measured | baseline set | gate (x1.5) |
|---|---|---|---|
| loopback: the **p50** of 100 `DaemonHealth` round trips | 199.5, 199.5, 210.6, 315.3 µs across four probes at load 8.5 | **230 µs** | 345 µs |
| cpu: the fixed 20M-iteration integer loop | **42.0 ms**, the minimum of 25 samples at load 3.9 | **42 ms** | 63 ms |

**The loopback probe is a p50, not the mean §D2 implies, and that cost a whole
baseline run to learn.** A mean over a hundred calls is one stall away from
anything: a single probe read 600 µs because one of its hundred calls took tens
of milliseconds, declined the phase, and — being sticky at the time — took the
next two and a half minutes of a `-count=3` run with it, every assertion of
which came back at its quiet figures. The percentile rule with no interpolation
is what §D1 already requires of every other number in this phase; the probe had
no business being the exception.

Two more things this cost, both worth recording:

- **The CPU baseline was first GUESSED at 12 ms and was wrong by 3.5x**, so
  every run DECLINED. A threshold in this suite is a measurement; the guess
  produced a guard that asserted nothing and looked like it was working.
- **The factor is 1.5, not the 2.5 an earlier draft carried.** The CPU probe is
  barely sensitive to load on a 16-core host: 42.0-43.0 ms at load 3.9, and
  42.0 ms at load 10.6. A single-threaded loop does not slow down while free
  cores remain, so a 2.5x gate on it is a gate that never closes.

### I3. The ten assertions

The budget rule, applied once and stated once:

    budget = min(the spec's provisional budget, 3 x the measured worst of three runs)

The `min` is a **one-way ratchet**: three times an observed healthy maximum is
this suite's standing multiple and it TIGHTENS a budget with too much headroom,
but it may never widen one, because a budget widened to fit its measurement
asserts nothing about that measurement.

**No assertion exceeded its provisional budget, so phase 1 raised no production
finding of the "this hop is too slow" kind.** Seven of the ten tightened, four
of them by more than tenfold.

| # | assertion | layer | measured p50 (3 runs) | measured p95 (3 runs) | provisional | final budget | verdict |
|---|---|---|---|---|---|---|---|
| 1a/2a | `submit-prompt-ack` | Go | 7.92 / 8.48 / 8.87 ms | 9.13 / 9.94 / 10.32 ms | 20/50 ms | 20/32 ms | pass |
| 2b | `submit-prompt-roster-arm` | Go | 8.15 / 8.30 / 8.63 ms | 9.13 / 9.95 / 10.59 ms | 30/80 ms | 26/32 ms | pass |
| 11a | `select-workspace-ack` | Go | 479 / 495 / 497 µs | 669 / 829 / 1170 µs | 20/50 ms | 1.5/3.5 ms | pass |
| 14 | `footer-flip-participant-loss` | Go | 182 / 191 / 183 µs | 224 / 232 / 251 µs | 100/300 ms | 600/800 µs | pass |
| 17b | `roster-subscribe-replay` | Go | 247 / 236 / 229 µs | 663 / 691 / 611 µs | (200/500 ms proposed) | 800 µs/2.1 ms | pass |
| 1b | `perf-prompt-bubble` | webapp | 4.31 / 4.23 / 4.53 ms | 6.49 / 6.06 / 6.44 ms | 20/50 ms | 14/20 ms | pass |
| 3a | `perf-response-bubble` | webapp | 959 / 991 / 921 µs | 1.66 / 1.48 / 1.67 ms | 20/50 ms | 3/5 ms | pass |
| 5 | `perf-interrupt-footer` | webapp | 5.50 / 5.42 / 5.44 ms | 6.07 / 6.37 / 6.18 ms | 30/80 ms | 17/20 ms | pass |
| 8a | `perf-question-card` | webapp | 3.63 / 3.39 / 3.46 ms | 4.50 / 4.74 / 4.05 ms | 30/80 ms | 11/15 ms | pass |
| 11c | `perf-sidebar-selected` | webapp | 1.26 / 1.26 / 1.28 ms | 1.53 / 1.41 / 1.53 ms | 20/50 ms | 4/5 ms | pass |

Four of those numbers say something the document did not know when it was
written:

1. **Row 17b's prediction held, and the margin is the one it named.** §C said
   "the proposed 200/500 ms is almost certainly two orders too generous". It is
   nearer three: 236 µs and 691 µs, or 800x and 700x under. The prime ordering
   plus `publish.Topic.Subscribe`'s replay really does make a fresh subscribe
   free.

2. **Row 2's split did not bear out.** §C row 2b gave the push-carried roster
   arm a larger budget than 2a's ack "because it is a full server round trip
   plus a roster recomputation rather than an ack". Measured, the two are within
   a millisecond of each other (p50 8.63 vs 8.87 ms, p95 10.59 vs 10.32 ms). The
   split is still right as a matter of chains — they are two different paths —
   but the cost difference the larger budget was justified by does not exist
   today.

3. **Row 14's provisional 100/300 ms was sizing a different hop.** That figure
   was built around `streams.ts:waitBackoff`'s 250 ms reconnect cadence, which
   bounds a CLIENT'S OWN DETECTION of a dead daemon. What the Go layer can
   honestly measure is the daemon's publish path for a connectivity change, and
   no backoff sits on it: 191 µs and 251 µs.

4. **Row 11c answers the owner's question.** "Is a workspace switch reflected in
   the webapp sidebar more-or-less instantly?" — the selected marker moves
   **1.3 ms** after the roster frame lands, and that includes the whole rail
   being rebuilt (§C row 21: `sidebar.ts` `replaceChildren`s the body per push).
   The Emacs-originated half of the switch is phase 2.

### I4. The restatements phase 1 had to make, and why

The Emacs layer is phase 2, so five rows §F places there were either deferred
or restated onto a layer that can host them today. Each is named in its own
test's doc comment; collected here so no reader has to hunt.

| §F row | phase 1 built | deferred to phase 2 |
|---|---|---|
| F1 | the daemon's ack, timed in Go | the `float-time` stamp at the `agent-repl-send` advice |
| F2 | — (2a rides the same ack as F1) | the stamp at `agent-repl--input-accepted` |
| F11 | the `SelectWorkspace` ack (11a) and the webapp sidebar's marker (11c) | 11b, the roster push applied in Emacs |
| F14 | the daemon's publish path for a participant loss | 14b, `agent-repl-link-drain-segment` |
| F17 | 17b, the roster replay | 17a, the recorded `daemon-link` boot phase |

**Row 11c's rpc is issued from the page's own client, not from the Go driver.**
The owner asked for the Go world to issue `SelectWorkspace` so the hop matches
what Emacs's tab switch sends. It is the same rpc against the same real daemon
either way, and the issuer is **outside the measured interval by construction**:
the origin stamp is the roster frame's arrival, not the call. Issuing it from
the page spares the area a twenty-round cross-process rendezvous for a term the
measurement does not contain. If the owner wants the call to originate in Go
regardless, the rendezvous shape from `TestWebappLayerRestartHandover` is the
one to reuse.

### I5. Findings — added to §H

11. **`e2e/webapplayer_e2e_test.go`'s `WebappLayerTimeout` was 300 s — §H
    finding 2 — and is now 10 s. FIXED.** The 300 s existed because one area
    needs longer: the restart handover drives two whole process lifecycles. Each
    area now passes its own bound to `wlChild.WaitFor`
    (`WebappLayerHandoverTimeout` = 60 s, `WebappLayerPerfTimeout` = 60 s), and
    `TestWebappLayerParticipantHoldOutlivesTheWaitBound` pins one bound per area
    instead of one bound sized for the slowest.

12. **`daemon/integration/harness/daemon.go` already exports `func (d *Daemon)
    PID() int`.** §G proposal P3 says it does not and blocks rows 23b and 24 on
    adding one. Half of P3 is therefore already granted; what is still missing is
    the same accessor on `Store` and `Sidecar`, and the platform question §G
    raises is untouched. Not built here (23b/24 are not phase 1), but the
    proposal should be re-read before it is ruled on.

13. **The calibration guard as specified does not catch the condition it exists
    for, on this host.** A `-count=3` baseline run taken while sibling suites
    saturated the box produced measurements 2-4x the quiet figures — `submit-
    prompt-ack` p95 29.5 ms against 10.3 ms, `perf-sidebar-selected` p95 13.4 ms
    against 1.5 ms — and **both probes read normal throughout** (loopback 216 µs,
    cpu 42.5 ms). Two causes, one mitigated here and one open:

    - *Mitigated, and it is a DELIBERATE DEVIATION FROM §D2's "once per phase".*
      A reading taken once at the start cannot see load that arrives during the
      phase, and on a shared box that is the normal case. But a phase-wide
      STICKY verdict is worse: one spike then declines every assertion after it,
      observed twice. The loopback probe is therefore re-taken before each
      assertion (~20 ms each) and DECLINES THAT ASSERTION; the phase summary
      still carries a DECLINED line if any assertion was declined, so §D2's
      actual requirement — a green run that measured nothing cannot pass for a
      green run that measured something — is met.
    - *Open, for the owner:* the CPU probe does not discriminate at all on a
      16-core host (single-threaded, so it does not degrade while free cores
      remain), and the loopback probe, while it does move with load, still read
      normal through runs whose measured hops were 2x their quiet figures. A
      probe that would work is the host's own load average against its core
      count, which is what a human checks and what this work checked by hand
      before every measurement run. That is a change to §D2's design, so it is
      proposed rather than made.

14. **A red repetition could rewrite the baselines. FIXED.** §D3's rule is that
    "a baseline a red run can rewrite is not a baseline", and the first
    implementation wrote unconditionally, so the saturated run in finding 13
    recorded the numbers of assertions it had just failed. A repetition now
    records only when its budgets held, and a skipped one says so.
    `make -C e2e perf-baseline` also runs `-count=3` and records the HIGH-WATER
    p50/p95 across the repetitions: run-to-run spread is real and unequal
    between rows — `submit-prompt-ack`'s p95 varied 6% across three runs while
    `select-workspace-ack`'s varied 75% — so a baseline from a single repetition
    would put the 20% regression check inside the noise for the noisy rows.

15. **Three measurement faults in the assertions themselves, all fixed, all
    worth knowing before the phase-2 rows are written.**

    - **A footer with no seen shim link never leaves `idle`.**
      `daemon/internal/resolve/footer/status.go`'s `disconnected()` returns nil
      while `!s.linkSeen`, so a workspace that has never run a turn draws `idle`
      however many participant streams leave. Row 14's first draft waited out its
      bound against a footer that was correct. One warm-up turn establishes the
      link.
    - **The first prompt a workspace ever receives pays for the shim session's
      own process spawn**: ~355 ms in the Go layer and ~370 ms page-side, against
      under 12 ms for the other nineteen samples. That cost is cold start (§C row
      17a) and not these rows, so every prompt row drives one warm-up turn as
      ARRANGEMENT. No sample is discarded — §D1 forbids that — and the warm-up is
      never recorded.
    - **The footer's interrupt control has no confirmation step in the common
      case.** `footer/stop.ts:interruptControl`'s own listener calls `Interrupt`
      on the first click; the second, confirming button is drawn only when the
      daemon refuses with `confirmRequired` (live agents the stop would also
      end), which `!hold` has none of. Row 5's origin is the first click, and a
      confirm appearing now fails the sample rather than being clicked through —
      it would mean the sample timed a refusal round trip.

16. **`ctx.notePush` is a sufficient origin stamp, and a microtask queued from
    inside it is a sufficient terminal stamp.** §A5 says `notePush` "fires BEFORE
    the draw and so cannot close the interval on its own", which is true, and
    proposes wrapping `consume` instead — which would be a production touch.
    Because `opts.onPush` writes the DOM synchronously with no batching and no
    rAF, a microtask queued at `notePush` time runs after that apply has
    returned, which IS `consume`'s return. The instrument is therefore entirely
    test-side (`webapp/test/webapp-layer/perf.ts`), per frame rather than per
    MutationObserver batch, and §G proposal P2's real-clock escape is the only
    harness change it needed — taken exactly where `mountApp` already takes it
    for `yieldToIo`.

17. **§D3's flat >20% regression check is inside this host's own noise for
    three rows, so it REPORTS rather than fails by default.** The check is built
    exactly as §D3 specifies — one committed file per assertion under
    `e2e/perf-baselines/`, p50 and p95 and the tip they were measured at, a
    core-count guard, and rewriting only by `make -C e2e perf-baseline` — and
    the evidence that it cannot yet be enforced here is three measurements:

    - a baseline recorded over THREE runs put `perf-response-bubble`'s p95 at
      1.539 ms; the next run measured 2.036 ms (32% over) against a BUDGET of
      5 ms it never came near;
    - widened to FIVE runs, the next run put `perf-prompt-bubble`'s p95 60% over
      its baseline, again nowhere near its 20 ms budget;
    - the row's honest spread across every run taken here is 5.5-17.3 ms, about
      3x, on a host shared with sibling agent suites.

    No observation window short enough to be a make target captures a spread
    that wide, and widening the tolerance until it fits would leave a check that
    detects nothing. So the drift is REPORTED — loudly, naming the percentile,
    the baseline, the drift and this finding — while the ABSOLUTE budgets fail
    hard on every run as normal. `make -C e2e perf-enforce` (or
    `AGENT_REPL_E2E_PERF_BASELINE=enforce`) turns the failure on, and is the
    right invocation on a quiet or dedicated host.

    **This is not a fallback the phase chose for itself; it is finding 13 in a
    different costume.** A regression check can only be as tight as the
    calibration guard is strong, because "the box was busy" and "the code got
    slower" look identical to it. Ruling on 13's proposed load-average probe is
    what unblocks enforcing 17.
