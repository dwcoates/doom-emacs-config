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
gets everything back. It defaults to **1 hour**, and `0` disables hibernation
entirely.

The window is measured from the newest row on the workspace's own state log
(`ssm.LastActivityMs`), which is already an activity record: every row is
appended by something that actually happened, and nothing appends on a timer.
So a turn ending STARTS the clock rather than arming an immediate sweep.

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
annoyance; lower it when memory is the constraint.

## One canonical token shape, and the daemon owns every judgment taken from it

Every cost decision in this module — the compaction cold-read tripwire, the
cold-ping hibernation, the progress footer's expensive-turn alert, token
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
