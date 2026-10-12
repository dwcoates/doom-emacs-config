# Test run speed investigation (2026-10-11)

Goal: cut full `bin/test-all.sh` wall time a lot, with no loss of coverage.
Evidence comes from instrumented runs on branch `test-speed-evidence` (never
merged): per-unit start/end/CPU and per-item timings, plus targeted probes.
Fixes land on branch `test-speed`.

## Baseline (8 slots, the default)

- Wall span 479s, 339 units, 1,236 CPU-seconds reaped.
- Slots held 97% of the span; CPU per held slot-second 33%.
- Total slot-seconds 3,731 / 8 slots = 466s, so the run is CAPACITY-bound.
  - Consequence: splitting a long unit does not shorten the run at 8 slots.
  - Every unit-second removed anywhere saves ~1/8 s of wall.

| suite | slot-s | share | CPU / held slot |
|---|---|---|---|
| daemon | 1198 | 32% | 34% |
| e2e | 646 | 17% | 35% |
| e2e-emacs (4 wide, VM CPU unseen) | 318 | 9% | n/a |
| webapp | 288 | 8% | 41% |
| shim | 280 | 8% | 65% |
| sidecar | 222 | 6% | 34% |

## 16-slot comparison

- Wall span 367s (23% faster), but every unit got 1.7-3x slower at the same CPU.
- `top` at the time: 53% busy, 24% of it system time; host not saturated.
- Timing bounds tripped in node suites (a 900ms vitest timeout took 6.6s).
- Open question: what shared resource serializes units (finding 3).

## Landed so far (branches `test-speed`, `test-speed-pb`; not yet merged)

| commit | change | measured |
|---|---|---|
| dd7e74fe0 | sidecar mock scenario table in 8 top-level shards | 131s serial -> 18.5s overlapped |
| 8f8427d7f | testrun honors `AGENT_REPL_TEST_CPU_BUDGET` | interface for the host-wide budget |
| 7cd377a9a | webapp integration un-isolated + per-file hygiene | 231s -> 72-102s, green under shuffle seeds 1-3 |
| 1a49fad74 | PRODUCTION: shim redial after 10ms, not 100ms | removes ~90ms from every shim spawn (103ms of a 205ms fixture boot) |
| 9f3e0f4c4 | stray sweep reads the process table in-process | 131ms -> ~20ms per teardown (`ps` 29ms vs 3.7ms per listing) |
| d998fd7cc | ingestion test completes its file atomically, as producers do | removes a load-dependent race of the test's own making |
| 75b62133f | wait-bound integration packages overlap 4 tests per chunk | those chunks' wall 1.5-2.6x shorter at ~1 core each |
| 284ff0c42 (pb) | PRODUCTION: protobuf-es registry guard at build | shim start CPU 0.16s -> 0.10s; webapp proto boot 0.20s -> 0.13s; descriptors identical |

First full run with the first four: wall 479s -> 409s, slot-s 3,731 -> 3,169.

## Second round (after the 2026-10-12 landing on master, 2e8f69982)

| commit | change | measured |
|---|---|---|
| ac5097c75 | fake git compiled (bin/fake-git.go), harnesses install the binary | build-frontend 219s -> 69s in a full run (17s alone), readiness 167s -> 10s; 53-call differential identical |
| cc3761a44 | doctor.el loaded as a library runs no check | test-config.el 0.37s; no host probe from a unit test |
| df9f409ed | wait-bound packages overlap 8 tests per chunk (was 4) | full run 261s -> 231s, slot-s 2,056 -> 1,798 |
| b6561e41e, 907f4db5b | a branch recount that raced its workspace's teardown is not a fault | removes a load-exposed e2e failure |
| (operational) | e2e sandbox image rebuilt: its baked shim node_modules was stale, so every e2e-emacs run did `npm ci` | e2e-emacs 291 -> 257 slot-s |

Full-run trajectory (8 slots): 479s / 3,731 slot-s / 1,236 CPU-s -> 231s / 1,798 / 1,115 (best, all3); 246s / 1,914 / 1,179 on a busier host (all5).

Tried and not kept:
- 12 slots: 210s wall but slot-s +40%, CPU +12%, a load-only e2e failure; host ~11 of 16 cores.
- Masking clock-only mutations ahead of `quietMarkup` in the webapp integration harness: no change (70s).
- shim unit `isolate: false`: blocked on module-level logger state across 143 files.
- NODE_COMPILE_CACHE for the shim: ~13% of a 130ms start.

Remaining candidates:
- Daemon self-hash cache (31% of daemon CPU): needs the owner's call (finding 17).
- e2e-emacs per-test fixed cost ~1s (doom boot ~330ms, daemon link ~270ms, exit ~140ms) over 70 tests.
- ERT: 275 tests over 100ms carry 134s; first-use loads (org, gnus) are real dependencies.
- Load balancing (owner, 2026-10-12): even out the run's parallel parts so the tail is not one long unit.

## Third round (2026-10-12, after a5170948c)

- Full run, 8 slots: 229s (all7) -> 207s (all9, with the suite-slot signal fix: its harness 61s -> 5s); 1,420 slot-s, 1,082 CPU-s.
- Full run, 12 slots (all8): 187s, 1,963 slot-s, 1,179 CPU-s -- 18% faster at the same CPU. Pending the owner's ruling on run sizing.
- Coverage: no test removed. The green runs execute 13,540 tests against the baseline's 13,160; of the 12 baseline names missing, TestMockScenarios became TestMockScenariosShard0-7 and the other 11 were retired by other sessions' feature commits.
- Load-only failures found and fixed: the e2e shim killer matched a forked-not-exec'd daemon child; linux ESRCH on a mid-read exit; darwin half-written procargs2.
- Latency probe (pipe ping-pong): idle p99 0.5ms; during the daemon suite 6-9ms; during a full run 20-25ms, identical at nice 0 and nice 19, so priority is not the cause. Wait-bound tests inflate 5-7x in a full run (ERT integration files 3.4s alone -> 28s; e2e TestASuccessorThatFinishedJoiningAcceptsADeploy 1.7s -> 10.5s, past its 5s bound).
- Tried and dropped: skipping the stray sweep's argv reads of processes older than the test process. Daemon suite 535 -> ~500 slot-s, but 4 of 6 runs then failed in four different session-lifecycle tests against 0 of 5 without it; the faster teardown exposes timing races that are not yet understood.
- Open: TestSessionHealthAfterTheShimExitsReportsShimDied reports link_severed instead of shim_died under load (seen twice).

## Fourth round (2026-10-12): the RAM disk's I/O tier is the slowdown

- Wait-bound tests run 5-7x slower inside a full run than alone (ERT integration files 3.4s -> 28s, build-frontend-harness 17s -> 130s). Ruled out: CPU priority (a pipe ping-pong is identical at nice 0 and 19), process spawns (halved, no change), the stray sweep's argv reads, fseventsd's log (`.fseventsd/no_log` changed nothing).
- Cause, measured with a 4KB write+fsync probe: units run at `taskpolicy -d utility` (testrun/internal/ramdisk UnitIOTier). On the run's RAM disk a utility-tier fsync takes p50 0.1ms but p90 166ms, p99 328ms during a run; at the default tier it takes p90 0.5ms. On a scratch RAM disk, utility-tier I/O is 0.2ms alone and 86ms per fsync while any default-tier I/O touches the same volume; default-tier I/O on the SSD does not affect it. The SSD shows no such effect at either tier.
- Experiment, daemon suite only, units at the default tier: 495 -> 310 slot-seconds (-37%), 62s -> 57s wall, RAM-disk fsync p90 175ms -> 0.5ms. Pending the owner's ruling (tests run at background priority; CPU stays at nice 19).
- A faster suite exposes timing races: the opened fixture's control-socket dial lost to a 50ms idle cutoff's hibernate (fixed on test-speed: OpenedShim, not landed until a full run passes); a merge drain over its 2s bound; the fake git answering no default branch; a handover await past its deadline.
- Landed: e2e-emacs holds 2 slots (264 -> 164 slot-s); the incremental log reader (daemon suite CPU -30s).
- Incident: the experiments' scratch RAM disks wedged diskarbitrationd (`diskutil list` and `hdiutil attach` time out), which blocks every test run on the host until `sudo killall diskarbitrationd` or a reboot.

## Targets (owner, 2026-10-11)

- Minimize BOTH wall-clock time and CPU, aiming at 90%; coverage loss at most 2-3%.

## Content-addressed result reuse (design note)

- A unit whose inputs are byte-identical to a passed run's reuses that result: zero coverage loss, since the code under test is identical.
- Sound for pure Go unit packages: inputs are exactly `go list -deps -test` package files, embeds, testdata, toolchain and env.
- NOT yet sound for integration/e2e/vitest/ERT units: they exec built binaries and scripts and read files outside their packages, so a hand-declared input set that misses one yields a false pass.
- Status: deferred until those suites' inputs can be discovered mechanically rather than declared.

## Findings

1. Sidecar `TestMockScenarios`: 133 rows serialized by `-test.parallel=1`.
   - Serial 131s; 8 overlapping 28.6s.
   - FIXED on `test-speed` (dd7e74fe0): 8 top-level shards; all rows 18.5s, one shard 9.6s.
   - Full-run effect at 8 slots: ~110 slot-s, ~14s wall.

2. Per-test boot.
   - CORRECTION: the first probe's 1,163ms e2e boot average was one cold world building its binaries (19.4s); testrun prebuilds those, so a run never pays it.
   - e2e world, warm, phase-timed: ~125ms boot (store ~22ms, daemon ~82ms, build report ~20ms) + ~66ms teardown.
     - 337 tests x ~0.19s = ~64s of 608s (~10%).
   - daemon fixture (`newOpened`), warm: daemon ~110ms (fixtures 7, addr 42, serving 61) + register/open/watch ~175ms + teardown ~170ms.
     - 653 tests x ~0.46s = ~300s of 642s (~47%).
   - Boot is a CPU BURST: 8 boots cost 5.0 CPU-s (43% system), ~1.3 cores per booting fixture.
     - 8 concurrent boots: 308ms each (no slowdown); 16 concurrent: 1,016ms each (3.3x). This is the 16-slot slowdown (finding 3).
   - The test process itself is cheap: 250ms CPU over 8 boots; log polling is not the burst at an 82KB / 283-record boot log.
   - Next: profile the daemon side of register/open and teardown; evaluate fixtures shared across tests.

3. Concurrency slowdown (blocks the slot fix).
   - Process launch ruled out: empty binary 1.5ms solo, 3.0ms at 16 concurrent.
   - EXPLAINED: units are wait-bound on average (33%) but boot in CPU bursts (~1.3 cores per daemon fixture boot). At 16 slots the bursts overlap and saturate the host; see finding 2's 8-vs-16 measurement.
   - Consequence for the slot fix: an average-CPU width under-reserves bursty units. Either reserve by burst, or cut the burst (cheaper boots, fewer boots), or both.

4. Fake git (`bin/fake-git.sh`), timed per call inside the script plus the 6.9ms bash startup.
   - build-frontend: 1,078 calls, 25.3s, 23.5ms avg.
   - readiness-report: 851 calls, 18.0s, 21.1ms avg.
   - lib-worktrees: 24 calls, 0.4s.
   - Floor for a compiled fake: ~1.5ms launch, ~2.5ms with work.
   - Waste vs that floor: ~38.8 slot-s per full run (~1%, ~5s wall at 8 slots). A Go rewrite of the fake is worth it only after the larger items.

10. Daemon fixture boot timeline (one fixture's daemon log, 283 records over 208ms).
   - 103ms of it is one gap: shim spawned at 97ms, session_live at 200ms.
   - Cause: the first dial of every spawn fails (the shim has not bound its socket), then the redial backoff waited its 100ms first delay.
   - FIXED on `test-speed` (1a49fad74): first redial delay 10ms, doubling to the same 5s cap. Applies to every spawn, production included. Re-measure queued.

12. e2e-emacs's 4-slot width is EARNED: host busy 34% before (other sessions and apps, ~5.5 cores) vs 64% during its 84s run, so it uses ~4.75 cores in its VM. Not a lever.

13. shim[integration]: 412 tests, Duration 111s of which tests 100s; ~110 CPU-s, so CPU-bound, not wait-bound.
   - Every test spawns a real node shim (`dist/main.js`, 2.3MB bundle) plus a fake store.
   - The bundle's load to argv refusal is ~130ms wall / 150ms user; `NODE_COMPILE_CACHE` only trims it to ~115ms (not worth it alone).
   - The lever would be fewer spawns (shared shims within a file), which changes per-test isolation; not taken.

14. Daemon-integration bodies: harness waits are 5-20ms polls (no hidden fixed sleeps), except `ExpectNoPush`'s 500ms `ProbeWindow` at 44 call sites (~22s, 3.4% of the suite). An event-driven replacement needs a per-stream marker round trip (a proto change); deferred.

15. PRODUCTION: protobuf-es descriptor boot is quadratic in dependency depth (shim startup).
   - `node --cpu-prof dist/main.js`: 142ms total, ~55ms in `nestedTypes`/`add`/`addFile`/`addFields`.
   - Cause: each generated file's `fileDesc` builds a fresh registry and `recurseDeps` re-adds every already-booted dependency with `addFile(dep, false, true)`, which re-walks that dependency's whole closure with no visited check. Still so in upstream 2.16.0.
   - A guard that skips a file already added with types and deps cuts shim startup CPU from ~0.16s to ~0.10s (-37%), every spawn (production sessions, e2e shims, shim integration, sidecar scenarios: ~900 spawns per run, ~54 CPU-s).
   - Landing needs a dependency-patch mechanism (none in the repo today; `node_modules` is a read-only symlink in the e2e sandbox), or an upstream fix. The webapp loads the same library; its share not yet measured.

18. shim unit suite isolation.
   - At one worker: isolated 256s (collect 146s, tests 38s); in a full run the unit chunk's Duration was 84s with collect 39s and tests 23s.
   - `--no-isolate` fails 143 files at load: `test/log-setup.ts` calls `configureLog` per file and the logger refuses a second configuration ("shim logger has already been configured"); the setup's `vi.mock("node:fs")` would also need re-checking for cross-file leakage.
   - Candidate: a test-only logger reset plus a shuffle-seed guard like the webapp's. Worth ~40 slot-s (~5s wall at 8 slots). Not yet done.

17. Daemon lifetime profile over a run of roster/footer/feed/topbar/prompt/turn integration tests (rotating 300ms CPU-profile chunks, 450ms sampled): `buildid.File` (the self-hash) is 31% of all daemon CPU, debug logging (`dlog` emit to `RotatingFile.Write`) 11%; nothing else above a few percent. The daemon process is a small share of a fixture's CPU overall.
   - OWNER DECISION NEEDED for the self-hash cache: `internal/buildid`'s package doc records the owner design "staleness is decided by a content hash per component, never by 'did this build change the file'". A cache keyed by (dev, inode, size, mtime, ctime) still answers the content hash, but trusts unchanged metadata to mean unchanged bytes.

16. PRODUCTION: the daemon hashes its own 49MB binary at every boot (`buildid.File(selfExe)` for `deploy.Deps.DaemonBuild`): 54% of a booting daemon's sampled CPU, ~25ms. It must be taken at boot (a deploy replaces the file at the same path), so not lazily; a cache keyed by the file's identity would avoid it.

11. Compile units: 140 build units hold 497 slot-s for 130 CPU-s; vet 100 slot-s for 17; typecheck 105 for 18.
   - Median build 0.8s; a few big packages took 25-36s wall for 2-7s CPU while the host was busy.
   - 136 of the 140 builds start after 300s: the planner runs small packages last, so they make the tail.

5. Long single units.
   - webapp[integration]: 20 files, 211s, one chunk at 8 slots (222s, 105 CPU-s).
   - ERT: 63 files, 135s, one chunk.
   - build-frontend harness: 8 groups, one chunk.
   - Splitting matters only once the run stops being capacity-bound.

8. Vitest overhead dominates the node integration time.
   - webapp[integration] (one vitest, 20 files, 2,115 tests): Duration 218s = tests 74s + environment 94s + collect 31s + setup 8s + transform 6s.
     - The config runs isolated: a fresh jsdom and a fresh module graph per file.
     - The unit config already runs `isolate: false` (13.5s to 5.5s), guarded by shuffle seeds.
     - Integration mounts once per file in `beforeAll`, so it needs per-FILE hygiene, not the unit suite's per-test wipe.
     - MEASURED at one worker: isolated 231s (environment 75s, collect 53s); `--no-isolate` 78s (environment 0.4s, collect 6s); 2,115/2,115 passing both ways.
     - Change on `test-speed`: `isolate: false` plus `test/setup-shared-integration.ts` (real clock and empty page once per file); shuffled-seed verification queued.
     - Expected full-run effect: ~150 slot-s, ~19s wall at 8 slots, and the run's longest unit drops from 222s to ~80s.
   - e2e TestWebappLayer* (13 tests, 194s): each spawns its own vitest child.
     - 12 children: Duration 169s = transform 39s + collect 105s + environment 32s + tests 22s.
     - About 85% is vitest standing up and importing the app, not testing.
     - Each area test boots its own world and names exactly one layer file for its child, so every child re-transforms and re-imports the app.
     - Proposal (design needed): one long-lived vitest driver per e2e run that runs each area's file against the world the Go test hands it, or a pre-built app bundle the children import instead of TS sources.
     - CONSTRAINT: `e2e/WEBAPP-LAYER-SPEC.md` ruling 4 (project lead, 2026-09-03): one Go test per area, its own world, exactly one vitest file. Batching every area into one child breaks it; a warm server that runs one file per request keeps it. Deferred behind larger items; the owner may also choose to relax the ruling.
     - Ceiling: ~144 slot-s of transform+collect, ~18s wall at 8 slots.

9. Log-file waits re-read and re-parse the whole JSONL log on every 5ms poll (`harness.awaitLogRecordAfter`).
   - The daemon runs at debug level, so the log grows through a test and each poll costs more.
   - Every daemon boot awaits its serving record this way; 95 daemon-integration and 22 e2e call sites use it.
   - Next: CPU profile of the probe's test process; log size per test.

6. `ps`-summed %CPU undercounts host CPU by about a third versus `top`; use host ticks.

7. Unexplained SIGTERM (exit 143) of `webapp#00` and a `make` in `proto` at the same second (baseline run); first occurrence in any recorded run; sender unknown.
