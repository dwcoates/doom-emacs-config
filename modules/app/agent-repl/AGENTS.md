# agent-repl/

The Claude REPL: an Emacs frontend (`lisp/*.el`), a resident Go daemon (`daemon/`), a
per-session TypeScript shim driving the Claude SDK (`agent-shim/claude/shim/`), a
browser GUI (`webapp/`), and two OS-managed services carrying the file plane
(`agent-shim/shim-store/`, `agent-shim/claude/shim-sidecar/`).

The repo-wide rules in the top-level `AGENTS.md` apply here in full; this file
covers deploying and running THIS module, plus the color vocabulary every one
of its surfaces shares.

## Elisp layout

Every elisp source and every ERT suite lives in `lisp/`. Exactly three files
stay at the module root, because Doom's module loader resolves them by exact
path and would not find them anywhere else:

- `config.el` — the module loader Doom loads for an enabled module. It
  `load!`s each source as `lisp/<name>`.
- `packages.el` — read by Doom's package manager at the module root.
- `doctor.el` — loaded by `doom doctor`; it loads `lisp/install.el`,
  `lisp/codex.el` and `lisp/daemon.el` for their check functions.

Sources resolve module-root siblings (`prompts/`, `images/`, `hooks/`, `bin/`,
`skills/`, `metaprompt.md`, `webapp/`) by climbing one level out of `lisp/`.

The canonical suite invocation, from `modules/app/agent-repl/`:

```bash
emacs -batch -Q -l ert -l lisp/test-agent-repl.el -f ert-run-tests-batch-and-exit   # everything
emacs -batch -Q -l ert -l lisp/test-<module>.el   -f ert-run-tests-batch-and-exit   # one suite
```

### Test wait/timeout bounds are measured, not guessed

Every synchronization wait in `test-integration-*.el` funnels through
`agent-repl-itest--wait-until` (`lisp/test-integration-helpers.el`), which
polls via `accept-process-output` — never `sleep-for`/`sit-for`, which would
block the very process I/O a predicate is waiting on. Its bounds were sized
by running all seven `test-integration-*.el` suites plus the full
`test-agent-repl.el` unit run once, recording each test's own ERT-reported
duration, and setting each bound to roughly 3x the slowest HEALTHY case it
has to cover (never to fix a flake — a bound that only just barely passes on
a slow run is a race, not a bound, and gets a code fix instead of a bigger
number).

| bound | site | old | new | basis |
|---|---|---|---|---|
| `agent-repl-itest-default-timeout` | shared default for every unadorned `wait-until`/`await-*` call | 15s | 14s | slowest healthy wait observed anywhere in the suite run was the link suite's own handover/reconnect scenarios (~4.7s); 3x ≈ 14s |
| boot-timeout logged/surfaced (3 sites, `test-integration-daemon.el`) | `agent-repl-itest--wait-until` after `agent-repl-daemon-boot-timeout-seconds` fires | 5s | 4s | those scenarios' own boot-timeout fixtures run 1.0–2.0s; observed wait tops out at ~1.4s; 3x ≈ 4.2s |
| restart's own ensure build/start-ran (4 sites, `test-integration-daemon.el`) | file-flag checks after a restart's cold-start ensure | 5s | 3s | the flag file is touched synchronously once `agent-repl-daemon-ensure` proceeds; no observed case needs more than a fraction of a second |
| restart-abandonment message (`test-integration-daemon.el`) | echo-area message after a refused stop | 5s | 3s | same shape as the build/start-ran checks above |
| indicator-names-the-cause loop (`test-integration-link.el`) | per-case wait inside a `dolist` whose whole multi-case test runs in ~0.2s | 2s | 1s | still >3x any single case's real share of that 0.2s |
| fake-daemon exit on teardown (`test-integration-helpers.el`) | `agent-repl-itest--stop-daemon`, runs after EVERY scenario in every suite | 5s | 5s (unchanged) | genuinely needs longer: this reclaims a REAL OS process via its own graceful-shutdown path after every single test, and a slow CI host is exactly the case a bound exists to tolerate — a spurious failure here still falls through to `delete-process` |
| fake daemon's own HTTP graceful-shutdown grace (`lisp/testsupport/fakedaemon/main.go`) | `context.WithTimeout` around `httpServer.Shutdown` | 2s | 2s (never reached) | the exit path now aborts every standing stream first (`abortAllStreams`), so `Shutdown` returns on its own rather than waiting this out; the bound survives as the backstop it was always meant to be |

Re-measure before loosening any of these: `git log -p` on this section names
the run that produced each number, and a bound that creeps back up without a
new measurement behind it is exactly the kind of unexamined slack this table
exists to prevent.

### What a scenario is allowed to spend time on

The bounds above are ceilings on failure. These are the rules about what a
PASSING scenario costs, which is a different question and the one that decides
what the suite costs to run.

- **A wait samples at 2ms, and only when it has to.**
  `agent-repl-itest--wait-until` passes `nil` to `accept-process-output`, which
  returns the moment ANY process output arrives — so a predicate over Emacs's
  own state is already woken by the event, and the interval is the floor under
  a predicate whose fact lives on the DAEMON (a subscriber count, a recorded
  call) and so cannot announce itself. It was 20ms, and at 20ms nearly every
  wait in the suite slept a whole slot: 454 waits, 10.0s of an 11.5s host run.

- **The control plane never spawns a process.**
  `agent-repl-itest--control` speaks HTTP/1.1 over `make-network-process` to
  127.0.0.1. It is called several times by every scenario — twice by the reset
  alone, then once per turn of every `--await-*` poll — so a `curl` child per
  call cost 471 spawns and 3.3s of a 9.4s roster run. Production's transport
  still spawns `curl`, through `agent-repl-connect--spawn-curl`; that is the
  one boundary these suites run for real on purpose, and it is now the largest
  remaining per-scenario cost (~270 spawns, ~3.7s, in a host run).

- **A duration a scenario WRITES is a fixture, not a contract.**
  An announced `expected_outage_ms`, a rebound
  `agent-repl-daemon-boot-timeout-seconds`, a rebound
  `agent-repl-daemon-boot-poll-interval-seconds`: none of these is what any
  scenario asserts. Size them by the tolerance the assertion actually allows
  itself — an order of magnitude over it — never by what production ships.
  Three link scenarios were spending 1.5s each and three daemon scenarios a
  second each proving only that the client honors the window at all.

- **A push needs a SUBSCRIBER, not a ref.**
  `agent-repl-host-ref` is minted when `RegisterWorkspace` answers, strictly
  before the `WatchHostWorkspace` subscription behind it is registered
  daemon-side. A push in that window reaches nobody and the scenario waits out
  its whole deadline for a state delivered to no one. Every push after a
  subscribe goes through `agent-repl-itest--await-subscriber` first. Three
  scenarios were relying on the old 20ms poll to hide the window.

What those rules bought, measured by running each suite at the branch point
and at the tip alternately on one host, so both sides met the same load
(ERT's own reported suite time, seconds):

| suite | before | after |
|---|---|---|
| `test-agent-repl.el` (everything, 3695 -> 3701 tests) | 209.7 | 58.8 |
| `test-integration-link.el` | 64.3 | 5.7 |
| `test-integration-host.el` | 33.7 | 7.9 |
| `test-integration-composer.el` | 30.4 | 9.7 |
| `test-integration-verbs.el` | 26.7 | 5.5 |
| `test-integration-daemon.el` | 10.6 | 4.6 |
| `test-integration-roster.el` | 9.8 | 3.4 |

- **A sentinel outlives the scenario that armed it.**
  Emacs runs a sentinel from the event loop, never at the moment the process
  dies, so a stub build script's exit is delivered after `cl-letf` has put the
  external-boundary guards back — and the continuation behind it reaches them
  and errors out of a sentinel, aborting the whole batch run and naming
  whichever test happened to be running.
  `agent-repl-itest--reset-cold-start` therefore drops the sentinel whenever
  the process OBJECT exists, not only while it is live, and cancels the
  departure wait alongside the boot wait.

### Integration suites restore a REGISTERED boundary, by name and per scenario

The batch harness replaces every entry of
`agent-repl--external-boundary-functions` with a guard that errors, and that
stays true for `test-integration-*.el` too — with one sanctioned exception.
An integration scenario exists precisely to drive one external boundary
against a real, harmless, test-owned target: the transport's
`agent-repl-connect--spawn-curl` against a fake daemon on loopback, and cold
start's `agent-repl--frontend-run-build-script` /
`agent-repl--frontend-spawn-daemon` / `agent-repl--frontend-artifact-exists-p`
against stub scripts in the scenario's own temp dir. Those are restored
through the harness's own restore path
(`agent-repl-itest--real-boundary`, which signals unless the symbol is in
`agent-repl--external-boundary-functions`), BY NAME and PER SCENARIO, while
every other guard stays armed. This is the sanctioned way to write an
integration scenario, not a bypass of the guard: an unregistered boundary
cannot be restored at all, and a scenario that reaches for one fails loudly.

### ONE fake daemon serves a whole suite run

`agent-repl-itest--with-fake-daemon` hands every scenario the SAME fake-daemon
process — started lazily on the first scenario, reaped on `kill-emacs-hook`.
A process boot costs seconds and the integration suites run hundreds of
scenarios, so a per-test spawn is the single largest cost in the run.

The saving is only allowed to exist because the cleaning is TOTAL, and it
happens on the way IN (`agent-repl-itest--begin-scenario`), never on the way
out, so a scenario that dies mid-way cannot poison its successor:
`/_fake/reset` clears the recording, the scripted table, the snapshots and
every armed gate and ends every standing stream; the shared state root is
swept back to nothing but `daemon.addr`; that address is re-published so a
scenario which pointed it at a stub daemon cannot misdirect the next one; and
a second reset after the subscribers drain closes the window in which the
previous scenario's dying transport children can still land a request.

A scenario may still stop the shared daemon — cold start's absent-address
cases must — and the accessor respawns into the same state root next time.
A scenario that needs TWO live daemons (a handover) still spawns its own
successor through `agent-repl-itest--with-second-daemon`, which re-publishes
the primary's address once the successor is reaped. Those two are the only
sanctioned reasons to pay a spawn; `test-integration-fixture.el` pins the
isolation guarantee that makes the sharing safe.

### A fixture workspace directory is process-private and swept per scenario

Every `:project-dir` an integration scenario registers comes from
`agent-repl-itest--fixture-dir`, under one pid-keyed root. Never write a
literal `/tmp/itest-...` path into a suite again, and never let two scenarios
inherit one directory's contents.

Both rules exist because a REGISTERED WORKSPACE'S RECORDS ARE REACHABLE ONLY
THROUGH THAT DIRECTORY'S CANONICAL `.claude/emacs/emacs.log` SYMLINK, and
`agent-repl--workspace-emacs-log-target` re-points that link at its own
runtime-owned target whenever it finds it naming someone else's:

- **Across processes**, two suite runs sharing a fixed directory are two such
  runtimes, each stealing the link back from the other, so every `--await-log`
  reads a sink holding the other run's records and waits out its whole
  deadline for a line that was written, findably, somewhere else.
- **Across scenarios**, the log-target registry is scratch-bound per scenario,
  so each scenario mints a fresh target — but the link still names the
  previous scenario's target until this scenario writes its first
  workspace-owned record. In that window `--await-log` is satisfied instantly
  by the previous scenario's record for the same operation, and the assertion
  behind it reads THAT record's arguments.

`agent-repl-itest--begin-scenario` therefore sweeps the fixture root exactly
as it sweeps the state root; production recreates the `.claude` tree the
moment it routes a record.

### A fixture that shortens the reconnect interval must cap its ceiling too

`agent-repl-link--reconnect-tick` DOUBLES its interval on every poll that
finds no daemon, up to `agent-repl-link-reconnect-max-interval-seconds`. A
fixture that binds only `agent-repl-link-reconnect-interval-seconds` still
pays the 5s production ceiling, so a scenario that stops a daemon and waits
for a successor spends its deadline on backoff that has nothing to do with
what it asserts. Bind both, at every site that binds either.

## Runtime investigations go through one skill

For any current or historical agent-repl behavior, use the complete controller
at
`<current-repository-root>/modules/app/agent-repl/skills/debug-emacs-agent-repl/SKILL.md`
through `/debug-emacs-agent-repl`. The skill
derives the relevant evidence playbooks and owns operational procedures for
health, readiness, identity correlation, structured logs, SSM and store SQL,
testing and coverage, and observability-gap reporting. Keep implementation
mandates in the scoped `AGENTS.md` files and keep diagnostic recipes in the
skill.

## Purple means the vendor, blue means the local environment, teal means nothing is wrong

Every surface that carries color here — the Emacs tab-bar, the sidebar dots,
the feed bubbles, the failure cards — splits the same way, and a new element
picks its hue from that split before it picks a shade:

- **Purple: the llm/agent vendor.** The vendor's api, the account, and the
  model's own work. `vendor_blocked` and `ERROR_CLASS_API` (auth, a usage
  limit, a persistent 4xx/5xx), the assistant text bubble, the tool-card titles
  and the subagent chip (work the agent itself issued), the wash behind a
  compaction summary, and the arc drawn while a failed api request is being
  auto-retried.
- **Blue: the local environment, BROKEN.** Everything on the
  Emacs→daemon→shim→store route and the machine it runs on, when there is
  EVIDENCE something failed. `starting`, `severed` (a bring-up that could not
  be completed, or a session controller that died on a terminal protocol
  error), `dead`, `degraded` and `ERROR_CLASS_INTERNAL` (shim down, store
  outage, a refused command), the backfill-failed gate, and the user's own
  prompt bubble.
- **Teal: nothing is wired, and nothing is wrong.** `hibernated` alone — a
  session we SIGTERMed on purpose to reclaim its ~500MB, or a workspace nothing
  has ever been wired to.

  It is the correction to a conflation that cost blue its meaning. A single
  `dormant` state used to say both "asleep by choice" and "the substrate is
  broken", so the most routine event in the system — the idle sweeper reaping a
  workspace nobody touched for an hour — painted a tab exactly like a dead shim
  did. A user who watches every workspace go blue after an ordinary daemon
  bounce learns to ignore blue, and then misses the one that is really severed.

  Teal's PRECEDENCE is still the blue band's, not green's (rank 15, directly
  below `starting` at 14 and above purple's 20): a teal workspace cannot be
  interacted with until a bring-up is paid for, which is exactly the claim green
  exists to deny. Only the reason is benign. Consequently a teal tab over a live
  turn is unreachable by construction — `hibernate()` refuses a workspace that
  is not settled — and anywhere it is detectable it is logged as an invariant
  violation, never as expected.

`proto/vocab/render-colors.json` is where the split is executable: a failure
card takes its class's color from the same table the workspace dot takes, so a
purple workspace can never be explained by a blue card or the reverse. Reach
for a NEW hue only once you are sure the thing is neither side's — the tree
carried three answers about one api failure before this rule existed, and teal
was added only because one existing color was answering two incompatible
questions.

Within a hue the shade still carries meaning:

- The magenta-leaning `--blocked` (`#a21caf`,
  `agent-repl--color-vendor-blocked-purple`) is reserved for stopped at the
  vendor, needing a human. The violets (`--retry`, `--info-agents`,
  `--tool-title`) are the vendor working, and a retry mistaken for a dead
  session is the misread the two leans exist to prevent.
- Blue is deliberately one color for every local fault. Which part of the route
  broke matters to whoever debugs it, not to the user reading a tab, so the
  failure cards carry that distinction instead. What blue does NOT cover is the
  absence of a fault, which is the teal split above.

The merge lifecycle is outside the split by design: merge states wear glyphs
rather than colors so they never spend one of the six, and the Recently Merged
disc borrows the `--info-agents` violet as a section tint, not as a claim about
the vendor. Rows inside Recently Merged render glyphless whatever status they
carry: the section is settled history, and a question mark or a recycle mark
there reads as an alarm about work that is already done.

## The "expanded footer" is what the progress footer's detail section is called

The progress footer's expandable detail section — the `FooterDisclosure.expanded`
surface `webapp/src/progress-footer.ts` draws — is the **expanded footer**. Use
that name in code, comments, tests and prose; "sheet", "expansion", and "detail
panel" are the older names it replaces, and one surface answering to four is how
a change lands on the wrong one.

It carries the AGENT AND TASK ROSTER, and nothing else. Session status — the
rate-limit allowances and when they reset, the open compaction/hook/retry/blocked
windows, the merge's account, first-token latency — belongs in the strip's own
center cells (`activityDetail`, the phase word, the counters cluster) and NEVER
in the expanded footer. A fact said in both places gives the reader two homes for
one answer, and the two wordings drift apart the first time either is reworded.

It is also the ONLY surface that carries the session's subagent roster. The
agents chip opens and closes it rather than dropping a roster of its own, and
the per-bubble agent strips inside feed cards are a different thing entirely:
they are scoped to one bubble's own call.

## Hibernation is the memory knob, and it is gated on real elapsed quiet

A live session costs a node+CLI process pair of roughly 500MB, and dozens of
workspaces will exhaust a machine. `-idle-timeout` is the mitigation: after a
workspace has been left alone for that long, the sweeper SIGTERMs its shim and
leaves the registry record rehydratable, so the next act pays one bring-up and
gets everything back. It defaults to the keep-alive policy's own idle cutoff,
**6 hours**, and `0` disables hibernation entirely.

IT IS FLOORED AT THAT CUTOFF AND CANNOT GO BELOW IT. `-idle-timeout` and
`AGENT_REPL_HIBERNATE_IDLE_CUTOFF_MS` answer the same question — how long has
nobody touched this workspace — and while they disagreed, the shorter one reaped
sessions the longer one was still keeping alive, with the longer one's own
`idle_cutoff` cause attached to a threshold that was not its. Configuring a
shorter value now raises it, loudly, at daemon construction.

IT IS MEASURED ON THE ENGAGEMENT CLOCK, WHICH IS NOT THE CACHE CLOCK. Two
durable instants live on the registry record and they answer different
questions:

- `last_turn_end_ms` is the CACHE clock. It moves on EVERY turn end, keep-alive
  pings included, because a ping really does refresh the prompt cache and the
  next one really is due a cache lifetime after it. The ping and warm-compaction
  schedules measure from it.
- `last_engagement_ms` is the ENGAGEMENT clock. It moves only for a turn somebody
  asked for. The 6h idle cutoff measures from it and from nothing else.

ONE FIELD ANSWERING BOTH IS A BUG IN BOTH DIRECTIONS. While the cutoff measured
the cache clock, every successful ping reset it — so a session pinged every
fifty-five minutes never reached six hours and NEVER HIBERNATED AT ALL, the
exact inverse of the cold-cache defect below. The generic idle sweep had the same
contamination through `ssm.LastActivityMs`, since a ping's own turn boundaries
append `workspace_state` rows exactly as a real turn's do; it reads the
engagement clock now, and the state log only to date a record that has none.

WHICH TURNS COUNT IS DECLARED AT THE SUBMIT, NEVER RECOGNIZED AFTERWARDS.
`submitter.engagement()` answers it at `forwardPrompt` — the one funnel every
prompt path reaches — and the fact travels with the turn id to that turn's own
end. Nothing reconstructs it from prompt text, duration, cost, or timing. The
default is TRUE, so a submitter added later and forgotten delays a teardown
rather than making real work invisible to the cutoff.

THE IDLE CUTOFF IS THE ONLY TIME-BASED ROUTE INTO A SLEEP. Nothing hibernates
before it. A prompt cache that goes cold first — an overslept window, a missed
ping, a bounce — stops being PINGED (`keepalive.ActionLetCacheCool`, and the
cold-ping verdict in `keepalivecold.go`) and is not slept: a dead cache is a
reason to stop spending on it, not a reason to tear the session down. Both of
those used to hibernate at roughly the one-hour cache TTL, which is why
hibernation looked far more frequent than the six-hour rule permits. The
`cache_expired` cause is still READ — records written by earlier daemons carry
it and must still be revivable — and is written by nothing.

For a record with NO engagement instant at all — one written before the second
clock, or one the daemon has never seen a turn under — the window falls back to
the newest row on the workspace's own state log (`ssm.LastActivityMs`), which is
already an activity record: every row is appended by something that actually
happened, and nothing appends on a timer. So a turn ending STARTS the clock
rather than arming an immediate sweep. It is a FALLBACK and only that: the state
log cannot tell a keep-alive ping's turn boundaries from a person's, which is
why the engagement clock outranks it wherever one exists.

AND THE SWEEPER IS NO LONGER THE ONLY GUARD.
`sessioncontroller.hibernate()` itself refuses any workspace whose resolved
state is not SETTLED — the red band (a turn in flight, either context cut) and
purple (a vendor block the user has not seen through yet) — with the typed
`sessioncontroller.ErrNotSettled`. The rule used to be "never call Hibernate
mid-turn", left to each caller, so it held only for the callers that remembered
it; inside the shared teardown it is mechanical, and the frontend command and
every future caller hit it too.

That also protects the vocabulary. `hibernated` is ranked in the blue band
precisely so a stale agent row cannot mask it, which means a teal tab over a live
turn would look exactly like a teal tab over a settled one — the user sees
"asleep" while the agent works, with no color anywhere to correct it. The guard
is what makes that combination unreachable by construction, and the resolver logs
it as an INVARIANT VIOLATION wherever it can still detect it.

Terminal lifecycle operations use `StopSession`, not hibernation. A delete or
supersession may terminate an active turn because the exact registry record is
already terminal and must not retain a process or turn claim. `StopSession`
never publishes `HIBERNATED`; that benign state is reserved for a settled
workspace admitted through the hibernation lease.

Intentional process replacement uses `StopSessionForReplacement`. A controller
generation holds an SSM-owned registration reservation from before shim
startup through its durable operational edge, and hibernation cannot begin
while that reservation exists. Replacement releases the reservation only after
the exact process stop completes, then brings the same durable session back up.

Both gates in `Server.sweepable` are load-bearing, and neither is redundant:

- `!turn_active` alone is satisfied the instant a turn ends, so a sweeper gated
  on it hibernated healthy sessions within one tick of them finishing work.
  That was roughly every seven minutes in practice, since the tick is
  `idleTimeout/4` and the timeout was never applied as a threshold at all.
- Every unknown answers NO. A workspace with no resolved state, or none the log
  can date, is one the sweeper knows nothing about, and reaping on absent
  evidence is how a bring-up still in flight got hibernated before its first
  event landed.

Raise `-idle-timeout` when a machine has headroom and bring-up latency is the
annoyance; lower it when memory is the constraint — though it cannot go below the
policy's idle cutoff.

A KNOWN DEFECT, STILL OPEN: `Manager.Hibernate` is also the teardown a merged
workspace, a daemon bounce in stop-shims mode, a scheduled drain and an account
switch all take, and it publishes `RENDER_STATE_HIBERNATED` for every one of
them. So a bounced workspace SHOWS the user a sleep that never happened, and with
this backend bouncing often that is very likely why hibernation looked far more
frequent than the six-hour rule permits. The state row's cause kind now names the
real initiator (`hibernated:merged_teardown`, `hibernated:idle_sweep`, …) so the
two are countable apart in the log; the render state is unchanged, because fixing
it needs a third connectivity token beside `hibernated` and `severed` — "stood
down, and neither asleep nor broken" — which is a proto and webapp change.

## One canonical token shape, and the daemon owns every judgment taken from it

Every cost decision in this module — the compaction cold-read tripwire, the
cold-ping verdict, the progress footer's expensive-turn alert, token
accounting, any future budget gate — reads ONE representation, and it is the
one that states the economics rather than the vendor's field names.

```proto
message TokenUsage {
  TokenCacheHits   input_hits    = 1; // served from the prompt cache — the cheap bucket
  TokenCacheMisses input_misses  = 2; // processed fresh — the expensive buckets
  uint64           output_tokens = 3; // there is no output cache; plain total
}
message TokenCacheHits  { uint64 read = 1; }
message TokenCacheMisses {
  uint64 written   = 1; // entered the cache as it was processed (vendor cache_creation, 1.25x)
  uint64 unwritten = 2; // never entered the cache at all (vendor input_tokens, 1x)
}
```

**The expensive sum is structural, not an addition anyone has to remember.**
`input_misses` IS what the request paid for at uncached rates, because both of
its fields missed the cache. That is the entire reason for the nesting. The
vendor's three counters are disjoint (`@anthropic-ai/sdk`
`resources/messages/messages.d.ts`: "Total input tokens in a request is the
summation of `input_tokens`, `cache_creation_input_tokens`, and
`cache_read_input_tokens`"), and their names describe WHERE the tokens went,
not what they cost:

- `cache_read_input_tokens` → `input_hits.read`. Served from the prompt cache;
  the ONLY cheap bucket (~0.1x the input rate).
- `cache_creation_input_tokens` → `input_misses.written`. Processed fresh at
  full price PLUS the cache-write premium (~1.25x). "Cache" in the vendor name
  means the tokens were being written INTO the cache, not served from it.
- `input_tokens` → `input_misses.unwritten`. Processed fresh and never written
  to the cache at all. Uncached in every economic sense; it carries no cache
  label only because it never entered the cache.

Reading either miss ALONE misses the case the whole apparatus exists to catch:
the CLI marks nearly all input cacheable, so a cold prompt — a full context
re-ingest, the most expensive thing that can happen — surfaces almost entirely
as `written` while `unwritten` stays near zero, and a deliberately uncacheable
prefix surfaces the other way round.

**Rates are DERIVED, never stored.** The cache-hit / cache-write / fresh-input
partition is three quotients over these same counters. It is computed at the
point of use (`daemon/internal/tokenusage.DeriveRates`) and never persisted
beside the counters it comes from. The three sum to 1, one per disjoint bucket,
so the fresh-input rate is a SHARE and the expensive share is fresh + write.

### Who does what

- **The shim is a faithful translator and nothing more.** It converts vendor SDK
  usage into canonical counters at the boundary and does NO token-based
  processing, gating, flagging, or derived-figure computation. Its usage log
  carries the raw vendor buckets, verbatim; it derives no sum, no total, no
  rate, and raises no threshold warning about tokens. (The one warning it does
  raise is about a usage key the typed contract cannot express, which is a
  TRANSLATION defect and therefore its own business.)
- **The daemon is the sole owner of token judgment.** One boundary conversion
  (`daemon/internal/tokenusage`) produces the canonical shape, and one accessor
  answers each question: `ExpensiveInput` (both misses), `ContextInput` (misses
  plus the hit), `DeriveRates`. A second independent derivation anywhere is a
  defect — that is precisely how two subsystems come to disagree about whether
  one turn was cold.
- **The webapp renders what the daemon resolved.** `ProgressView.input_tokens`
  and the session / per-subagent canonical totals are daemon figures, rendered
  verbatim, and their absence is a loud failure rather than a cue to re-derive.
- **Durable evidence stays vendor-faithful, and that is where the mapping
  happens.** The statedb `token_utilization` and `turn_accounting` rows persist
  `VendorTokenUsage` and `TokenUsageTotals` as binary protobuf, and a replayed
  durable stream must reproduce a persisted row BYTE FOR BYTE (`proto.Equal`
  in `statedb`). Those shapes are therefore FROZEN: adding a populated field,
  removing one, or changing what one holds breaks the replay of every row an
  earlier build wrote. The canonical shape is produced FROM them at read time —
  it is never stored beside them, and the database is not migrated.
  - `TokenCacheRates` is the one surviving stored rate, kept and kept populated
    for exactly that reason, and read by no judgment. New code must not read it.

## `conversation.v1` is what a producer saw, and the daemon only ever adds its own bookkeeping

`conversation.v1` is designed to be rendered DIRECTLY by a frontend. A
conversation record reaches the GUI as itself: `claude-repld` never synthesizes
one, and never re-encodes one on its way out. The only thing the daemon
contributes is its own bookkeeping — facts it worked out that no producer ever
observed.

**The daemon never synthesizes a conversation record.** A `conversation.v1`
record states something a PRODUCER observed, and there are exactly two
producers: `claude-shim`, per session, watching the vendor SDK live — the
stream plane — and `shim-claude-sidecar`, reading the vendor's on-disk
transcripts — the file plane. `claude-repld` produces nothing here; it consumes
what those two wrote. A daemon-minted `conversation.v1` record asserts an
observation nobody made.

**The daemon never re-encodes one either.** `frontend.v1` CARRIES a
conversation record and stamps its own bookkeeping alongside it; it adds
nothing to the record. A `frontend.v1` message that restates a `conversation.v1`
fact in its own words is a re-spelling, and the translation layer that produces
it is work that should not exist.

**The test for which side a fact belongs on is who came by it.** Did a producer
OBSERVE it, or did the daemon WORK IT OUT?

- Observed → `conversation.v1`, and it reaches the frontend unchanged.
- Worked out → `frontend.v1`, where it is bookkeeping riding alongside.

The worked example is a background shell, because it separates cleanly. Its
EXIT CODE is observed — a producer watched the process exit 137 — so the code
is a `conversation.v1` fact. The OUTCOME resolved from that code is the
daemon's, because a killed process also exits nonzero, and reading the code as
"it failed" reports a user's own interrupt back to them as an error. So the
code rides in `conversation.v1`, the verdict rides in `frontend.v1`, and
neither restates the other.

THIS IS A TARGET, NOT A DESCRIPTION OF THE TREE. The first half holds today:
`claude-repld` constructs `conversation.v1` messages at exactly three sites,
all inside `claude-repld.internal.tokenusage.fromCounters()`, and both of its
callers are converting the daemon's OWN durable records
(`state.v1.VendorTokenUsage`, `state.v1.TokenUsageTotals`) into the canonical
shape for display — the daemon reading its own bookkeeping, per the section
above, not minting conversation. The second half does NOT hold: the webapp
imports `conversation.v1` in exactly two files (`webapp/src/tokens.ts` and
`webapp/src/agent-emission.ts`), and everything else arrives as `frontend.v1`
re-encodings the daemon built from conversation records.
`frontend.v1.Message`'s payload oneof has no arm that can carry a
`conversation.v1.MessageEntry` at all, so today the re-encoding is forced by
the schema rather than chosen.

## The shim-store database is nuked, not migrated

`shim-store`'s SQLite database is DISPOSABLE and is to be regarded as EMPTY. A
schema change deletes the file and recreates it; there are no migrations and no
backfills, and existing rows go away with the database.

THIS IS A DEVELOPMENT-STAGE POSTURE, not a permanent property of the store.
Nobody currently cares what is in that database, so migrating it is complexity
bought for nothing. That stops being true the moment its contents matter to
someone, and this section is what has to change first when that happens — it is
not licence to treat stored data as expendable forever. This is the same posture
the frozen durable shapes above take from the other end — `state.v1` replay is
protected by never changing those messages, not by migrating what was written
under them.

**The absence of a migration is a DECISION, not an oversight to be helpfully
corrected.** Two things go wrong when it is left unwritten:

- An agent that assumes the database must be preserved invents migration and
  backfill work nobody wants and nobody will review. It is worse than wasted
  effort: migration code asserts a compatibility guarantee this module has never
  made, and the next reader believes it.
- A stale database is more expensive than an empty one. Rows written under a
  retired schema, sitting on disk while new code reads them, produce
  PLAUSIBLE-LOOKING wrong data rather than a clean failure — nothing announces
  that the rows are stale, so everyone reading them reasons from data that was
  never valid under the current contract.

**Say this explicitly in the instructions of any agent that touches the store's
schema.** An agent working from the code alone sees a `schema_meta(version
INTEGER)` table and reasonably infers that migrations are expected. Nothing in
the tree corrects that inference.

The pending case is `shim-store`'s `entry` table gaining a `parent_message_id`
column, extracted at ingest exactly as `top_level_message_id` already is, plus an
index `entry(session_id, parent_message_id, seq)` mirroring the existing
`entry_message_owner`. It exists so `frontend.v1.PageScopeInside` can page a
nested container in one indexed pass instead of a scan:
`conversation.v1.MessageEntry.parent` currently lives inside the opaque
`payload` BLOB, and `top_level_message_id` cannot substitute because a subagent
and a subagent inside IT share one value. That change ships with no migration and
no backfill.

## Committing to master means bouncing what you changed

Every component here is a built artifact, and every running process keeps
serving the binary it started with — so a commit deploys nothing on its own. A
change is finished when the process serving the user is running it, and your
report says what you rebuilt and what you restarted.

A merged-but-undeployed fix looks exactly like a fix that does not work, except
the correct code sitting in `git log` makes it harder to diagnose.

The two deploy paths have OPPOSITE bounce policies. Follow each as written
rather than re-deciding per change.

**1. `bin/build-frontend.sh` — shim, webapp bundle, daemon. ALWAYS bounce the
daemon afterwards.** Build-if-stale, so it is cheap to run unconditionally:

```sh
modules/app/agent-repl/bin/build-frontend.sh
```

Then bounce claude-repld — every time, without asking and without weighing
whether this particular change merits it. Rebuilding the binary is not deploying
it, and the bounce is also what remounts webviews, which is how a webapp rebuild
reaches the user. The top-level "Daemon bounce policy (claude-repld)" section
governs HOW (never mid-turn; prefer the Emacs restart path) — never whether.

**2. Hand-deployed launchd services — shim-store, shim-claude-sidecar. Leave
these IN FLIGHT; bounce only when the user asks.** They carry the file plane for
every live session, so restarting them is disruptive in a way a daemon bounce is
not. After landing an important change to either, ASK the user whether to bounce
— an unbounced service means they never see the change, so silence is not the
safe option either.

`build-frontend.sh` does NOT touch them. They run out of
`~/.cache/agent-repl/bin/` under `com.agentrepl.shim-store` and
`com.agentrepl.shim-claude-sidecar` (plists in `launchd/`), so changes under
`agent-shim/shim-store/` or `agent-shim/claude/shim-sidecar/` deploy nothing
until:

```sh
cd modules/app/agent-repl/agent-shim/shim-store
go build -o ~/.cache/agent-repl/bin/shim-store .
cd ../claude/shim-sidecar
go build -o ~/.cache/agent-repl/bin/shim-claude-sidecar .

launchctl kickstart -k gui/$(id -u)/com.agentrepl.shim-store
# WAIT for ~/.cache/agent-repl/sock/store.sock before the next line
launchctl kickstart -k gui/$(id -u)/com.agentrepl.shim-claude-sidecar
```

**That ordering is mandatory.** Restarting both at once makes the sidecar's
cursor recovery fail against a socket not yet listening; it then starts cold and
silently re-reads every watched transcript from offset zero (observed
2026-07-25, thousands of files re-ingested).

## Verify the deploy rather than assuming it

`KeepAlive` restarts a failing service forever, so a broken deploy presents as a
service that is "running" while doing nothing. Read the tail of
`~/.cache/agent-repl/log/shim-{store,claude-sidecar}.log` and confirm the steady
state — for the sidecar, `store link UP`, not a repeating `store link DOWN`.

## No real Claude/Anthropic calls from tests

`AGENT_REPL_FORBID_VENDOR_CALLS`, set to any non-empty value, makes every
vendor entry point refuse loudly: `daemon/internal/vendorguard` returns an
error at the queue classifier's `claude -p` exec and at the login pty, and
`agent-shim/claude/shim/src/vendor-guard.ts` throws at the one chokepoint that
can import the real SDK. The harnesses set it for you — `TestMain` in
`daemon/e2e` and `daemon/internal/sessioncontroller`, and the shim's vitest
setup — and children inherit it, so a new test needs no opt-in. Production must
never set it.
