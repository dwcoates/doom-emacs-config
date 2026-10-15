# Which test cases are redundant: the measurement and its pilot (2026-10-14)

STATUS: draft, paused by owner decision on 2026-10-15 before the pilot
rerun and the baseline run. The pilot and rollout sections are not written
yet. The first pilot run's numbers are under "Pilot".

Option A of the test-slimming effort (parent workspace
`agent-repl-test-slimming`): build the measurement that says which test
cases may go without losing coverage, judged by MUTANTS KILLED, pilot it on
`daemon/internal/workspace/sessions_test.go`, and propose its rollout.

The tool is `testrun redundancy` (AGENTS.md, "Which tests are redundant").

## What the tool measures

Per case, each leaf test measured alone:

- the production blocks it reaches, unioned over `-runs` runs (default 3) so
  a path only a race reaches is not called uncovered;
- its runtime, from its `=== RUN` line to the line that ends it (Go's own
  `(0.00s)` is too coarse: most unit cases here run in microseconds);
- the mutants it kills.

### Mutants: one schema, not one compile per mutant

`internal/redundancy/gomut` rewrites a package's production files into a
mutant SCHEMA: every mutant is compiled into the one binary behind
`AGENT_REPL_MUTANT`, reaching the compiler through `go -overlay`, so no source
file is ever written. Measured: 794 mutants of `sessions.go` built in one 14s
compile, where one compile per mutant would have cost 794.

- Operators: `<`/`<=`/`>`/`>=` boundary and negation, `==`/`!=` swap, `&&`/`||`
  swap, dropping `!`, `true`/`false` flips, `+ - * / %` swaps, removing a call
  statement, `return ..., err` to `return ..., nil`.
- Every operand is evaluated once and in its original order (generic helpers
  take the operands as arguments), and short circuits stay short (`&&`/`||`
  go through closures), so the inactive schema behaves exactly as the
  original. A baseline run with no mutant active must pass every case, or the
  measurement refuses.
- A site that does not type-check (a string `+`, a named bool) is dropped by
  the compile error that names it, and reported.
- gremlins and go-mutesting were not used: neither records WHICH test killed
  a mutant, and the minimization needs exactly that.

### Kills, and what is not a kill

- Each mutant runs only the cases that reach its block, in one process; a
  case that process never reported (a panic, a hang) reruns alone.
- A hang is a kill only when it reproduces at `-hang-bound` (11s, just over
  twice the longest real wait the package's tests allow themselves). The
  first pilot counted 1689 kills by hang across 81 mutants, most of them a
  removed `mu.Unlock()` (a genuine deadlock); but a mutant can merely SLOW a
  case, and a false kill makes another case look dominated, the unsafe
  direction.
- A failing subtest fails its parent; that parent failure is not credited to
  a sibling (a bug the tests caught).
- An interrupt ends the measurement: a process the interrupt cut short would
  otherwise read as a hang, and a hang as a kill.

### What always stays

- A case asserting a canonical log record: its body, or a test-file helper
  it reaches by name, calls `-pin-calls` (default `FindRecord,Records`).
- A case named with `-pin`.

## The minimization

Weighted set cover over the universe (default kills-only, owner ruling
2026-10-14: a case may go when the kept cases kill every mutant it kills;
`strict` also keeps every reached block):

- reductions to a fixpoint (pinned and essential cases forced, dominated
  cases dropped), then an exact branch and bound, held to a brute force over
  every subset in its tests; past its node budget the best cover stands and
  the report says it is not proven cheapest;
- each removable case is listed with what catches its bugs instead: a single
  dominating kept case when one exists, else the kept cases that cover it;
- the report also lists the cases each universe removes and the other keeps.

### The coverage budget

Owner ruling, 2026-10-14: the 5 points of statement coverage per system are
ONE budget across every workspace, shared out by the parent workspace; each
workspace reports at 1 point (`-max-coverage-drop`, default 1).

- A system is its Go module, by the longest module path prefixing a block.
- The estimate errs low: a statement only removed cases reach counts lost,
  even if a test outside the measurement reaches it.
- Over budget, removed cases go back, the most lost statements per unit of
  runtime first.
- The reference is `testrun/coverage-baseline.json` when `-baseline` names
  it: each system's all-tier coverage at one master commit, produced by
  `bin/test-all.sh --coverage` (its new last unit, `coverage:baseline`).
  Building that run found two gaps, now closed: the programs the integration
  and e2e tiers spawn were uninstrumented, and the daemon's build-tagged
  integration pass got no spawned-coverage directory at all.

## Cross-tier: verifying an audit's deletion set

Owner ruling via the parent, 2026-10-15, option (c). The tier audits (B, D)
propose DELETE rows "already asserted at a lower tier". A whole kill matrix
across tiers costs hundreds of hours (a daemon integration case reaches
thousands of mutants at 2.4s a run), so `-delete FILE` proves a deletion set D
against the kept rest K, mutant by mutant, each step stopping at the first
kill:

1. kept unit-tier cases reaching the mutant, together, one process per
   package: a kill settles it;
2. else the deleted cases reaching it, cheapest first: none kills it, and
   nothing is lost;
3. else the kept integration and e2e cases, cheapest first: none kills it,
   and the cheapest deleted killer goes back and stays.

Running D before the kept tiers in step 2 is deliberate: proving "no kept
case kills it" needs every kept case reaching it, so a mutant nothing kills
(39% of reached mutants in the pilot) would pay all of them and then D too.
`-estimate` stops after step 1 and reports |R|, the unsettled mutants, with
the rest's cost bounded.

## Pilot: `daemon/internal/workspace/sessions_test.go`

First run (before hang confirmation; its hangs over-count kills, see
above): 240 leaf cases, 29 pinned; 117 removable under kills-only (106
under strict), proven cheapest; case runtime 141ms total, 54ms of it saved.
Mutants: 1445 in the reached files of internal/workspace, 496 killed, 311
survived, 638 unreached, 0 dropped (61.5% over reached). The measurement
cost 1h of wall and the schema compiled once.

The second run (hang confirmation, coverage budget) ran under host loads of
85-196 and was discarded; the tool now waits for load at most 32 (load.go).
A rerun is queued behind the pause.

## Findings

- A Go unit case costs microseconds to a millisecond: all 1243 of
  internal/workspace's cases total 0.5s, while daemon integration's 703 cost
  1984s and e2e's 351 cost 1497s (the scheduler's history). Removing unit
  cases buys case count, not time; the runtime is in the integration and
  e2e tiers, which is why the cross-tier verification exists.

## Rollout

Not yet written (paused).
