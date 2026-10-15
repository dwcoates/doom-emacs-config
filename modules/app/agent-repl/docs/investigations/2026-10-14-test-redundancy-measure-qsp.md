# Which test cases are redundant: the measurement and its pilot (2026-10-14)

STATUS: the pilot and the baseline are measured; the cross-tier
verification of the audits' deletion sets is running.

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
- A vitest case that reads a source Stryker mutates. Stryker mutates in
  place, because its sandbox copy breaks every import out of the package
  (the shim's `vitest.config.ts` imports `../../../bin/require-background.mjs`).
  A case that reads a mutated file's text, such as the shim's one-shape
  guards over `engine/session.ts`, would read the mutant switches instead.
  The traced runs record every file a case reads through `node:fs`, and
  such a case is pinned with that file as its reason. Stryker's runs skip it
  (`testrun/vitest/redundancy-skip.mjs`).

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

Measured 2026-10-15 at master 9e2f7e4874 (merged into this branch), host
load under 32 throughout, against the pinned baseline; the full report is
[pilot-sessions_test-report.md](2026-10-14-test-redundancy-measure-qsp/pilot-sessions_test-report.md).

| | |
|---|---|
| Leaf cases | 240 (173 top-level tests, their subtests expanded) |
| Pinned (log-record assertions) | 29 |
| Removable, kills-only | 117 (proven cheapest), keeping 123 |
| Removable, strict | 106; kills-only removes 12 more for 2.7ms |
| Case runtime | 110ms total, 37ms of it saved |
| Coverage, agent-repld | 87.85% to 87.82% estimated (0.04 points) |
| Mutants | 1445: 494 killed, 313 survived, 638 unreached (61.2% over reached) |
| Kills by hang | 66 mutants' hangs confirmed at 11s; 2 only slowed a bound-measuring test and were not counted |
| Measurement cost | 31m23s wall, 4m00s CPU, 13287 processes; builds 7s, mutant runs 31m |

- The first run (before hang confirmation, under heavy load) found the
  same 117; its kill sets over-counted by hangs. The second run under loads
  of 85-196 was discarded.
- Nearly half the cases go, but the time saved is noise: the package's
  whole file runs in 0.11s against a 4s compile.

## Findings

- A Go unit case costs microseconds to a millisecond: all 1243 of
  internal/workspace's cases total 0.5s, while daemon integration's 703 cost
  1984s and e2e's 351 cost 1497s (the scheduler's history). Removing unit
  cases buys case count, not time; the runtime is in the integration and
  e2e tiers, which is why the cross-tier verification exists.

## Rollout

Scope as narrowed by the parent on 2026-10-15. Each tier audit deletes the
rows whose cover it confirmed by reading both bodies, checking coverage
only. This tool proves the rest, the rows whose evidence is weaker.

1. **Daemon and store integration rows the audit could not confirm by
   reading.** These are `prover-daemon.txt` (262 rows) and
   `prover-store.txt` (106 rows), from test-tier-audit-integration at master
   `3bf667fcc0`. Each is proved together with the audit's confirmed rows
   (`confirmed-daemon.txt`, 164; `confirmed-store.txt`, 20). No kill a
   to-be-deleted case gives can then vouch for a prover row.
   - `-estimate` first. It reports |R|, the mutants no kept unit case
     settles, and the cost bounds of steps 2 and 3.
   - A projected cost under 12 hours of serial CPU goes straight to the
     proving batch. A larger one goes to the parent first.
   - Every put-back is named in the report with the mutant that needs it.
     The proven set lands as its own merge, and the put-backs stay.
2. **The sidecar's rows**, once its audit sends them, the same way.
3. **The shim and webapp sets, with the vitest adapter.** Their baselines
   in `testrun/coverage-baseline.json` stay provisional until the adapter
   re-measures each package all-tier. No shim or webapp case is removed
   before then.
4. **ERT**, after vitest: an in-process mutator that re-evaluates a
   mutated defun, since Emacs Lisp has no compile step to overlay.
5. **The 45 `TestEmacs*` e2e rows**, last. They need a Docker sandbox to run
   under the measurement, and they stay out of every deletion until then.

Measuring conditions, for every batch:
- AC power only.
- Nothing starts while the 1-minute load is over 32, and a hang seen above
  it is invalid.
- One batch at a time, through `bin/background.sh`.
- `GOCACHE=/tmp/agent-repl-slimming-gocache`.

The coverage budget is one point of cumulative loss per workspace. The
parent arbitrates the shared five.
