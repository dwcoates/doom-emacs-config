# Test suite runtime: findings and remediation plan (2026-10-14)

Goal: the full suite (unit + integration + e2e + e2e-emacs) under 5 minutes,
green every run, with no host-wide test locks (owner ruling 2026-10-14).

## Measurements

| Run | Slots | Unit I/O tier | Scope | Wall | Unit-seconds | CPU | Failures |
|---|---|---|---|---|---|---|---|
| before (flock, suite slot) | 8 | utility | full, e2e-emacs declined | 260s | 2040 | 1032 | 0 |
| locks removed | 13 | utility | e2e,daemon (4 runs) | 124-132s | 1605-1658 | 723-735 | 2-6 per run |
| locks removed | 13 | default | e2e,daemon (3 runs) | 51-55s | 659-707 | 484-500 | 1 per run |
| locks removed | 13 | default | full, e2e-emacs RAN | 152s | 1834 | 1115 | 0 (bar the tier pin) |

- The e2e prebuild alone, idle host: 4.7s at the default tier, 11.8s at
  `throttle` (bin/background.sh), 23.0s at `utility` (testrun units). The
  difference is Go reading GOCACHE and linking; CPU is the same.
- The live store during the default-tier runs (16:32-16:37Z): no warn or
  error, WAL checkpoints 2-33ms. The 2026-10-06 starvation came from test
  SQLite on the SSD; test state now lives on the run's RAM disk.
- One e2e world (store, sidecar, daemon) boots in ~95ms and tears down in
  ~15ms warm (1.2s cold, once per process). 155 world-creating call sites
  cost ~17s of e2e's 219 unit-seconds: restarts are NOT where e2e's time goes.
- The webapp layer already runs one warm vitest per test binary
  (e2e/webapplayer_server_test.go).
- Long poles at the default tier: webapp[integration] 140s and
  shim[integration] 132s (each ONE chunk), e2e-emacs 110s, shim 102s, ert
  100s (one chunk), build-frontend-harness 82s.
- The planner predicted 650s for a 152s run: test-history.json is full of
  throttled-tier samples, so it under-chunks the long poles.
- Build and vet: 163 units, 384s wall, 107s CPU at the default tier.

## Landed on branch test-suite-runtime

1. The daemon-suite host flock removed (it serialized one run's own six
   daemon-harness processes).
2. bin/suite-slot.sh retired; the merge gate no longer sets
   AGENT_REPL_SUITE_PRIORITY.
3. testrun sizes a run to NumCPU-3 slots.
4. The exit's stream-end waits share the exit's grace as one deadline.
5. shimclient.Died reads the shim's process state (P_WEXIT/SZOMB,
   PF_EXITING/Z): no DeathVerdictBudget.
6. cachelapse records a cadence call the shim's death cut at INFO.
7. The perf-tagged e2e files compile again; e2e is vetted under perf and
   realtest (roster VetTags).
8. protohelpers/oneofarm replaces three hand-written WhichOneof readers.
9. The perf baseline writer runs no git (AGENT_REPL_E2E_PERF_TIP).
10. Failure artifacts live beside the run log (<run>/artifacts/), so a run
    no longer evicts its own.
11. harness.TimedStep: every prebuild and TestMain setup step states its
    duration.
12. AGENTS.md: integration suites run their own system alone; each test
    directory names its mocks.

## Failure catalogue

Product defects (fix at the source):

- P1 `opentabs.Coordinator.readOwed` records ERROR and a fault when the
  open's caller left (`context canceled`). Fix: a cancelled context is the
  caller leaving, recorded at INFO with no fault, as shimclient's
  callerAbandoned does. Seen: TestARosterRowIsOpeningWhileItsOpenIsInFlight.
- P2 `conversation.v1.AgentArtifactSuccess` does not stand alone: the
  heading's favicon comes from a start frame this process may never have
  drawn (logs: two Success draws, no Start). Fix: restate the act on the
  success (`oneof act { AgentArtifactPublish publish; AgentArtifactList
  list; }`, as AgentArtifactFailure does under SETTLES_STAND_ALONE), fill it
  in the shim and sidecar converters, draw from it. Seen:
  TestArtifactHeadingKeepsTheFavicon.
- P3 `daemon.merge.chain` ERROR "the chain does not stand at the front of
  its queue in queue order". Not yet diagnosed: its only occurrence predates
  artifact retention. Reproduce under the new retention, then fix.

Test defects:

- T1 TestAHandoverWithANewerShimBuildBouncesTheAdoptedShimCleanly waits for
  the incumbent's build-report judgement but not for its boot sweep
  (`judged every adopted shim ... why=fresh_boot`), which can land after the
  newer build is installed and bounce the shim itself. Fix: also await the
  fresh_boot judgement before installing. Seen twice at the default tier.

Throttled-I/O artifacts (absent in all default-tier runs; verify after R1):

- landing deploy waits (TestALandedMerges*, TestAnAsyncLanding*,
  TestAMergedUpstreamSource*), the reaper sweep (registration's four fake-git
  calls took 2.8s), TestDetachedLostSweptUp, webapp-layer vitest timeouts.

## Remediation, in order

- R1 Units run at the default I/O tier on the RAM disk
  (testrun/internal/ramdisk.UnitIOTier), with CPU still at nice 19.
  bin/background.sh's `throttle` stays for test processes outside testrun.
  Update ramdisk.go's rationale and TestUnitPrefixRunsUnitsAtTheUtilityTier.
- R2 Fix P1, P2 (proto change carried through every producer and consumer),
  T1; reproduce and fix P3.
- R3 Two consecutive green full runs at the default tier; any new failure is
  catalogued and fixed at its source.
- R4 Start the timing history afresh for the new tier (a new measure name, or
  a reset of test-history.json), so the planner chunks the long poles
  (webapp[integration], shim[integration], ert).
- R5 Suite selection (daemon/internal/merge/suiteselect.go):
  - a system change also selects the cross-system suites it reaches:
    daemon/, agent-shim/* and webapp/ select e2e; lisp/ selects e2e-emacs
    (today daemon/ selects only daemon, and lisp/ only ert);
  - Go suites' cross-module inputs come from `go list -deps`, not
    hand-written rules;
  - a source-scan test fails any integration directory that references
    another system's binary or tree (the mock rule, enforced).
- R6 Build each program once per run: one prebuild unit per binary
  (agent-repld, fakeshim, fake git, shim-store, shim-claude-sidecar,
  shim-lock, the shim bundle) that every suite consumes; today the
  daemon-integration and e2e prebuilds both build agent-repld, fakeshim and
  git, and the sidecar suite builds the store again.
- R7 The sidecar integration suite stops running the real store:
  - cursor-only tests move to the fake store as they are;
  - tests asserting books assert the sidecar's own written batches on the
    fake store (the books are the sidecar's lines);
  - tests whose subject is store behaviour (heal through the store, store
    bounce durability) move to the store's own integration suite with
    sidecar-shaped batches, or to e2e.
- R8 realtest git: gitclient's one exec site (internal/gitclient/run.go)
  takes its binary from a link-time variable (`-ldflags -X`), defaulting to
  "git". Open: realtest drives the owner's own daemon on the owner's real
  repositories, so a fake-git daemon there breaks the owner's workspaces for
  the sweep (see Questions).
- R9 Shared e2e worlds: dropped (owner, 2026-10-14) on the measurement
  (~17s of 219 unit-seconds); per-test worlds stay isolated.

## Owner rulings (2026-10-14)

- R1 approved: units at the default I/O tier.
- R8: the daemon takes its git binary from an override, and every test,
  realtest included, points it at the mocked git.
- R9 dropped.
- Coverage may fall by at most 1% (ideally far less), and no change a user
  would notice.
