# agent-repl/

The Claude REPL: an Emacs frontend (`*.el`), a resident Go daemon (`daemon/`), a
per-session TypeScript shim driving the Claude SDK (`agent-shim/claude/shim/`), a
browser GUI (`webapp/`), and two OS-managed services carrying the file plane
(`agent-shim/shim-store/`, `agent-shim/claude/shim-sidecar/`).

The repo-wide rules in the top-level `AGENTS.md` apply here in full; this file
covers deploying and running THIS module, plus the color vocabulary every one
of its surfaces shares.

## Purple means the vendor, blue means the local environment

Every surface that carries color here — the Emacs tab-bar, the sidebar dots,
the feed bubbles, the failure cards — splits the same way, and a new element
picks its hue from that split before it picks a shade:

- **Purple: the llm/agent vendor.** The vendor's api, the account, and the
  model's own work. `vendor_blocked` and `ERROR_CLASS_API` (auth, a usage
  limit, a persistent 4xx/5xx), the assistant text bubble, the tool-card titles
  and the subagent chip (work the agent itself issued), the wash behind a
  compaction summary, and the arc drawn while a failed api request is being
  auto-retried.
- **Blue: the local environment.** Everything on the Emacs→daemon→shim→store
  route and the machine it runs on. `starting`, `dormant` (no session is wired
  to the workspace), `dead`, `degraded` and `ERROR_CLASS_INTERNAL` (shim down,
  store outage, a refused command), the backfill-failed gate, and the user's
  own prompt bubble.

`proto/vocab/render-colors.json` is where the split is executable: a failure
card takes its class's color from the same table the workspace dot takes, so a
purple workspace can never be explained by a blue card or the reverse. Reach
for a third hue only once you are sure the thing is neither side's — the tree
carried three answers about one api failure before this rule existed.

Within a hue the shade still carries meaning:

- The magenta-leaning `--blocked` (`#a21caf`,
  `agent-repl--color-vendor-blocked-purple`) is reserved for stopped at the
  vendor, needing a human. The violets (`--retry`, `--info-agents`,
  `--tool-title`) are the vendor working, and a retry mistaken for a dead
  session is the misread the two leans exist to prevent.
- Blue is deliberately one color for every local fault. Which part of the route
  broke matters to whoever debugs it, not to the user reading a tab, so the
  failure cards carry that distinction instead.

The merge lifecycle is outside the split by design: merge states wear glyphs
rather than colors so they never spend one of the five, and the Recently Merged
disc borrows the `--info-agents` violet as a section tint, not as a claim about
the vendor.

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
`daemon/e2e` and `daemon/internal/sessiondrv`, and the shim's vitest setup — and
children inherit it, so a new test needs no opt-in. Production must never set
it.
