# agent-repl/

The Claude REPL: an Emacs frontend (`lisp/*.el`), a resident Go daemon (`daemon/`), a
per-session TypeScript shim driving the Claude SDK (`agent-shim/claude/shim/`), a
browser GUI (`webapp/`), and two OS-managed services carrying the file plane
(`agent-shim/shim-store/`, `agent-shim/claude/shim-sidecar/`).

The repo-wide rules in the top-level `AGENTS.md` apply here in full; this file
covers deploying and running THIS module, plus the color vocabulary every one
of its surfaces shares.

## Tests NEVER run real git or a real vendor

Owner rule, reaffirmed 2026-10-06: no test of any form — unit, integration, e2e, e2e-emacs, harness, hook test — runs real `git` or reaches a real vendor (the Claude API, the Agent SDK against Anthropic, any network vendor). Git is the fake git executable (`bin/fake-git.sh`, `daemon/integration/fakegit`); the vendor is the mocked SDK. A test that needs either and cannot use the fake is a gap to report, never an exception to take.

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
bin/background.sh emacs -batch -Q -l ert -l lisp/test-agent-repl.el -f ert-run-tests-batch-and-exit   # everything
bin/background.sh emacs -batch -Q -l ert -l lisp/test-<module>.el   -f ert-run-tests-batch-and-exit   # one suite
```

`bin/test-all.sh` runs without coverage by default; pass `--coverage` only
when coverage is the question being asked, never for routine verification.

### Every test runs at background priority: `bin/background.sh`

Test load must never starve the owner's live runtime (shim, daemon, store,
sidecar, Emacs, webview). `bin/background.sh <cmd>` runs a command and its
whole process tree at the host's background priority, and it is the ONE
helper every test entry point goes through.

- macOS and Linux: `nice -n 19` (owner ruling 2026-09-23, "very low").
  - The run keeps the performance cores but always yields the CPU to the live
    runtime; disk I/O is not demoted.
  - `taskpolicy -b` was measured and rejected: it pinned runs to the
    efficiency cores and the webapp integration suite passed 56-58 of 1765.
  - Linux is reached only inside the e2e sandbox container.
- Any other platform: REFUSED with exit 78, never run at normal priority.
- It is idempotent: a process already at niceness 19 or above (read with
  `getpriority`, not from the environment) runs its command straight through.
- It exports `AGENT_REPL_BACKGROUND_PRIORITY`, and only it sets it.

How each kind of entry point routes through it:

- Every bash test entry point (`test-*.sh`, `test-all.sh`, `suite-slot.sh`,
  the coverage and repeat runners, the `.claude/` test hooks) carries this
  prologue as its FIRST executable line:
  `[[ -n ${AGENT_REPL_BACKGROUND_PRIORITY:-} ]] || exec "$(dirname "${BASH_SOURCE[0]}")/background.sh" bash "${BASH_SOURCE[0]}" "$@"`
- Every Makefile test recipe runs under `$(BACKGROUND)`, defined with the
  standard `BACKGROUND := $(abspath $(dir $(lastword $(MAKEFILE_LIST)))<rel>/bin/background.sh)`.
- Every npm `test*`, `coverage*` and `smoke` script and its `pre*` hook starts
  with the helper; a compound one is wrapped whole as `<helper> sh -c '...'`.
- The runners that cannot be wrapped from outside refuse to start without the
  marker: every vitest config imports `bin/require-background.mjs`,
  `lisp/test-helpers.el` signals in batch, and the Go integration harness's
  `WithRunRoot` (daemon integration and e2e) returns 1.
  - So a bare `npx vitest`, `emacs -batch -l lisp/test-*.el` or
    `go test -tags integration` fails loudly; prefix it with `bin/background.sh`.
- The e2e sandbox entrypoint execs every command through the helper.
- The live runtime and the deploy builds (the daemon's deploy, `build-frontend.sh`,
  `launchd/`, the non-test `lisp/*.el`, npm `build`/`dev`) are NEVER demoted.

`bin/test-background.sh` (the `background-harness` suite) scans the repository
and fails on any entry point that bypasses the helper, and on any live-runtime
or deploy path that references it.

### CPU load only when the owner asks, and only through `bin/with-cpu-load.sh`

The owner does not want the machine under load (2026-09-30). Reproducing a
flake under CPU load is done ONLY when the owner asks for it, and then ONLY
through `bin/with-cpu-load.sh <nloops> <command...>`. Never hand-write busy
loops: in one evening a `( while :; do :; done ) &` generator outlived its
author twice (zsh does not word-split `kill $PIDS`, so nothing was killed and a
`wait` hung on the loops; and a `trap` cleanup never runs when its shell is
killed outright).

- Each of the helper's loops polls the helper's pid and exits the moment it is
  gone, so no ending of the helper -- an exit, a signal, a SIGKILL -- leaves a
  loop spinning.
- The command runs as its own process group and is waited on, so a TERM stops
  it, everything it started, and the loops at once.
- `bin/test-with-cpu-load.sh` (the `cpu-load-harness` suite) holds all of this.

### One scheduled run at a time: `bin/test-all.sh` and `bin/suite-slot.sh`

`bin/test-all.sh` builds `testrun`, which turns the whole roster into one DAG
and schedules it across `runtime.NumCPU()-2` slots. There is no special case
for smaller hosts. The planner chooses each suite's chunk count by simulating
the run against EWMA unit timings in
`~/.cache/agent-repl/test-history.json`, then assigns the longest remaining
dependency chain first. Parallelism belongs to this scheduler:

- every Go package is compiled once and split by top-level test; the go
  command carries `GOFLAGS=-p=1`, every process carries `GOMAXPROCS=2`, and
  chunks carry `-test.parallel=1`;
- ERT is split by authored test file;
- vitest is split by test file and pinned to one worker;
- e2e and integration packages build their shared binaries once in a prebuild
  unit, then every test chunk consumes those exact binaries;
- slow shell harnesses expose `--list` / `--only` groups and per-group timing.
- a unit that cannot be pinned to one core holds a WIDTH of slots and caps
  itself to exactly that many cores. `e2e-emacs` is four slots wide (the VM its
  Emacs parallelism bound was measured on); testrun hands the width over in
  `AGENT_REPL_UNIT_SLOTS`, and the sandbox applies it as `docker run --cpus`
  and `GOMAXPROCS` inside the container. A vitest suite's typecheck is two
  slots wide, because `tsc` measured 1.5-1.75 cores and cannot be pinned.
  The run's "not pinned to its width" line judges reaped CPU against the
  width. It cannot see CPU spent in a VM or by an unreaped descendant, and
  the sandbox VM is exactly such a case, so a unit like that must be capped
  by its own command line. A unit starts only when its whole
  width is free, nothing narrower overtakes it while it waits, and a plan with
  a unit wider than the host's slot count is refused before anything runs.

`--suites a,b` selects a roster subset and refuses an unknown name.
`--coverage` explicitly adds Go and vitest instrumentation and report units;
ordinary and merge-gate runs omit them. `--record` is reserved for the
canonical post-merge timing run on master.

The WHOLE scheduled run holds `bin/suite-slot.sh`. A second run does not go
twice as fast: it competes with an already host-filling schedule and turns
ordinary bounds into noise.

That is not hypothetical. Several agents each running their own suites at once
took this box to a load average of 253, and the Emacs layer then failed 37 of
45 scenarios on Doom's boot bound alone — a whole run's evidence thrown away,
with nothing wrong with the product. So:

```bash
bin/suite-slot.sh npm test          # from the package dir; wraps, never relocates
bin/suite-slot.sh go test ./... -count=1
```

`bin/suite-slot.sh` re-execs itself through `bin/background.sh`, so a command
it runs is at background priority too.

The gate counts what is actually running, claiming slots as directories via
`mkdir` (atomic, fails if the name exists), and reclaims a slot whose recorded
pid is gone. It is the same mechanism as the sandbox's container gate in
`e2e/sandbox/bin/e2e-sandbox.sh`, for the same reason.

**IT NESTS.** A slot is held by a process TREE, so wrapping a script that
wraps its own children is safe: the holder exports
`AGENT_REPL_SUITE_SLOT_HELD`, and an inner invocation that sees it runs
straight through with one line saying so. Without that, `bin/suite-slot.sh
bin/e2e-repeat.sh ...` deadlocked — every per-run acquisition queued behind the
outer holder that was waiting for those very runs to finish, 6000s of "still
waiting" with nothing running.

Do NOT gate on the load average instead. It is a decaying mean, so it keeps
climbing for a minute after the work stops, and every waiter reads the same
number and starts at the same instant — a thundering herd that recreates the
overload. This was tried; it is what produced the 253.

Two standing rules follow from the same measurement for direct, focused suite
runs outside `testrun`:

- The vitest configs cap `maxWorkers` at 50%. A suite may not claim every CPU
  even when it does hold the slot.
- A test that starts a child process group must kill the GROUP. `npm run` is a
  wrapper: signalling the npm pid alone leaves vitest's worker pool reparented
  to init and burning CPU for the rest of the run.

### No test writes to the user's temp directory

Every temp file a test makes lives under a root its runner owns and removes.

- `testrun` (`bin/test-all.sh`) moves its own `TMPDIR` to `/tmp/tr-XXXXXX`
  (planning's `go list`/`vitest list` included), and `run.OSExec` hands every
  unit a fresh `TMPDIR` root under it, removed when the unit exits. A root that
  cannot be removed fails the unit.
- Batch ERT (`lisp/test-helpers.el`) makes one private root per process,
  points `temporary-file-directory` and `TMPDIR` at it, and removes it on
  `kill-emacs-hook`, so a direct `emacs -batch` run leaks nothing either.
- The shim's vitest runs keep their own `/tmp/sv-*` root
  (`agent-shim/claude/shim/test/run-tmp-root.ts`).
- MACOS'S `mktemp` IGNORES `TMPDIR` without a template (and with `-t`): it
  writes to the user temp directory whatever the root says. Every script names
  its parent, `mktemp -d "${TMPDIR:-/tmp}/name.XXXXXX"`, and
  `testrun/internal/run/mktemp_scan_test.go` fails a bare one.
- Why: the suites' leaks grew the user temp directory to 856,127 entries, and
  creates there stalled for seconds and timed tests out.
- Roots live under `/tmp`, not `os.TempDir()`: a unix socket path is capped at
  104 bytes on macOS and the user temp directory's own path spends 49.
- There is NO before/after check of the user temp directory. The whole host
  writes there (other agents' runs, the owner's Emacs on every bounce), so a
  listing cannot tell this run's entry from theirs; it failed two runs that
  leaked nothing. A `sandbox-exec` write denial would attribute exactly, but it
  also refuses every setuid binary, `/bin/ps` included, which the harnesses'
  stray reaping needs.

### Suite timings: what a row measures

`test_time.csv` is the canonical per-suite timing history (recording rules:
the repository root `AGENTS.md`, "Canonical test timing history"). Its
`measure` column names what each row's `duration_seconds` is:

| measure | written by | what it is |
|---|---|---|
| `serial-wall` | the retired serial `bin/test-all.sh` (rows up to 2026-08-04), never again | `time` of the suite run ALONE on the host, its `go test -count=1 -cover`, vitest-with-coverage or Emacs processes free to use every core |
| `unit-wall-sum` | `testrun finish-record` (staged by `testrun run --record`) | the sum of the suite's own units' wall times, each unit on one core slot, prebuild/compile units included |

The regression report compares a suite only with prior rows of its own
measure on its own branch, and says how many rows of another measure it set
aside. An unknown measure, a run whose rows disagree on their measure, and a
row of the wrong width are errors, never skipped rows.
`TestCanonicalTimingFileParses` holds the committed file to that schema.

Why `unit-wall-sum` and not the alternatives:

- The span, from a suite's first unit start to its last unit end, is a property
  of the schedule.
  - It includes every stretch in which the suite's next unit waited for a slot another suite held.
  - The first `--record` under testrun recorded `store` at 89.7s against a 0.8s history while the whole run halved.
- CPU time misses everything a test waits on, such as timers, sockets and I/O.
  - It also misses every process the unit does not reap, such as the Docker VM behind `e2e-emacs` and daemonized children.
  - A test that starts waiting 5s longer would never register.
- Summed unit wall time is the slot time the suite itself occupies.
  - It is what the planner budgets, and it moves when the suite's own tests get slower.
  - It does not grow while its units queue behind other suites.

What still moves `unit-wall-sum` without the suite changing:

- The planner's chunk count for a Go or vitest suite: each extra chunk pays its process start (and `TestMain`) once more.
- Contention from the units sharing the host at the same moment.

So a regression is a lead to investigate, not a verdict. Read the unit lines
(`unit <id> [<suite>] ok, <wall>s wall, <cpu>s cpu`) and the plan line's chunk
counts before attributing it to the suite.

`--record` refuses `--coverage`: instrumentation and the report units would
inflate every Go and vitest suite's figure. A future change to what a run
measures (a different figure, or pinning) gets a NEW measure name, never a
reuse of an old one, so its first rows start a fresh baseline.

### Every Go module pins a shared third-party dependency at one version

The store, daemon and e2e modules once each pinned `modernc.org/sqlite`
independently, and one driver bug (a statement leaked on context cancel,
pinning the SQLite WAL) sat in two systems at once. `bin/check-go-deps.sh`
reads every `go.mod` under this module (direct and `// indirect` requires, block
and single-line forms) and fails, naming the dependency, each version and the
go.mod files pinning it, whenever a third-party module path is required at more
than one version.

- Our own modules replaced by a local path (`replace ... => ./` or `../`, such as
  `agentrepl/proto` and `agentrepl/logging`) are exempt, detected from each
  go.mod's own `replace` directives.
- `go.mod` files under `node_modules`, `testdata`, `fixture` and `fixtures` are
  skipped.
- `bin/test-check-go-deps.sh` (the `go-deps-harness` suite) covers the check on
  fixture trees and runs it against the real tree as the gate.
- An upgrade of a shared dependency lands in EVERY module that requires it, in
  the same commit, followed by each affected module's tests.

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

### The scenario coverage matrix is generated, never hand-edited

`e2e/SCENARIO-MATRIX.md` is the inventory of which mocked-vendor scenarios
have e2e coverage, and people plan work from it. It was hand-maintained until
it was caught lying in both directions on one day — twelve `!api-*` rows
reading `uncovered` while a single file drove every one of them, twenty
session and tool rows reading `uncovered` with strong tests already behind
nineteen, its own summary counts disagreeing with its own table, and section
(c) disagreeing with section (a). Two agents nearly wrote duplicate tests off
it.

So it is now derived and enforced by `e2e/scenariomatrix_test.go`
(`TestScenarioMatrixMatchesReality`), which runs with the e2e package, spawns
nothing and drives no scenario:

```bash
AGENT_REPL_MATRIX_WRITE=1 go test ./e2e -run TestScenarioMatrixMatchesReality
```

- The canonical scenario list comes from `src/fake/scenarios/*.ts`,
  cross-checked against the registry-generated prompt table in the shim's
  AGENTS.md. Add or rename a scenario and the check fails until the matrix
  follows.
- Which test drives which scenario comes from the tests, by the vendor's own
  `selectScenario` rule over their string literals plus the bare-name drive
  helpers. An argument shape the extractor cannot read FAILS rather than
  counting as zero.
- A `!name` literal in a counted layer that names no registered scenario is a
  dead trigger and fails the check.
- The counts and the uncovered/weak lists are computed from the table.

What it deliberately does not decide: whether an assertion is STRONG or WEAK.
That is a reading of the test body, so the `Grounded?` and
`Strongest assertion` columns and the covered-vs-weak choice stay
hand-written, and a newly-derived row defaults to `weak` with a TODO for a
human to raise.

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

- **Nothing here spawns a process to speak HTTP, production included.**
  `agent-repl-itest--control` speaks HTTP/1.1 over `make-network-process` to
  127.0.0.1. It is called several times by every scenario — twice by the reset
  alone, then once per turn of every `--await-*` poll — so a `curl` child per
  call cost 471 spawns and 3.3s of a 9.4s roster run. Production's transport
  dials the same way now, through `agent-repl-connect--open-socket`; that is
  the one boundary these suites run for real on purpose, and it was the
  largest remaining per-scenario cost (~270 spawns, ~3.7s, in a host run)
  until it stopped being a spawn at all. Measured on one box, alternating
  branch point and tip: a unary round trip 60.1ms -> 3.5ms, composer
  3.1/3.1s -> 2.1/2.2s, host 3.8/3.9s -> 3.0/3.1s, verbs 3.8/4.3s ->
  2.8/2.8s, connect 1.5/1.5s -> 0.90/0.90s.

  ITS CONNECT BLOCKS, AND THAT IS NOT A TUNING CHOICE. The peer is a
  loopback listener that answers in a fraction of a millisecond or refuses
  on the spot. A `:nowait` dial would have to wait for the connection
  before it could write, and every way of waiting -- an explicit
  `accept-process-output`, or the 20ms retry Emacs performs on the write's
  own EAGAIN -- runs the event loop. These dials happen INSIDE PROCESS
  FILTERS (daemon-link attaches a successor from the `WatchDaemon` filter),
  and running the event loop from inside a filter cost the e2e handover its
  `transferred` pushes outright: sockets open, daemon pushing, Emacs
  delivering nothing, no workspace adopted, no promotion.

  THE RESPONSE DECODING IS SHARED, NOT COPIED. `agent-repl-connect--reader` —
  status line, then a body under a `Content-Length`, `Transfer-Encoding:
  chunked`, or the close — is production's, and the harness calls it rather
  than keeping a second decoder of its own. curl used to do that decoding on
  the transport's behalf; when it went, the harness's copy became the only
  other one, and two HTTP readers for one daemon is exactly the drift this
  rule exists to prevent.

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
`agent-repl-connect--open-socket` against a fake daemon on loopback, and cold
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

## The daemon is a RESIDENT SERVICE, and it outlives Emacs

Emacs owns the daemon's cold start and nothing after it: a daemon that
ANSWERS is adopted, never killed. That contract only holds if a daemon can
still be answering after the editor that started it has gone, and for a
long time it could not — the daemon was a plain `make-process' child, and
Emacs SIGHUPs every child from `kill-emacs`, so quitting the editor took
the daemon with it. The adopt-on-restart path was unreachable in practice
and unmeasurable in a realtest: restarting Emacs always respawned.

The spawn now goes through a detacher, `agent-repl-daemon--spawn-argv`
(`lisp/daemon.el`): a `/bin/sh` that sets SIGHUP to ignore, redirects the
child's stdout and stderr to `<state>/logs/daemon.stdio.log`, and then
EXECs the daemon.

- The ignored SIGHUP survives the exec, and Go leaves an
  initially-ignored signal ignored — the daemon asks for SIGINT, SIGTERM
  and SIGQUIT and no others.
- The redirection matters just as much as the trap. On Emacs's own pipe,
  the daemon's first log line after the editor exits would meet a closed
  read end and die of SIGPIPE: a detach that only held until the daemon
  next spoke. `agent-repl--frontend-stdio-log-tail` reads that file back,
  and the boot wait's "exited before it published its address" record
  carries both it and the run-log tail.
- It is an `exec`, so the pid Emacs holds IS the daemon's. The sentinel,
  the boot wait's exit branch and `agent-repl-daemon--spawned-here-p` all
  keep watching the real process.

STOPPING IT IS STILL DELIBERATE AND STILL EMACS'S TO DO. The stop verb is
`UpdateShutdownSchedule{now}` over the link
(`agent-repl-frontend-daemon-stop`), and the restart verb sequences that
stop ahead of a fresh ensure. Neither ever signalled the process object,
so neither lost anything to the detach. Only the implicit death-on-exit is
gone.

## SIGUSR2 to a shim resets its keep-alives (a backdoor, not a wire verb)

Each shim (`agent-shim/claude/shim/src/main.ts`) answers SIGTERM (graceful
shutdown) and SIGINT (refused, to protect a live turn under an attached
terminal). It also answers **SIGUSR2**: sent to a live shim process, it
collapses every outstanding keep-alive turn for that shim's session back to
the last real record, reclaiming the context and the store bookkeeping those
turns hold — precisely the rollback the shim's own per-cycle keep-alive
rewind already performs before every beat and every real prompt (the shared
`resetKeepalives` subroutine in `agent-shim/claude/shim/src/engine/session.ts`;
see `engine/keepalive.ts` for the cadence and the rewind it reuses). It never
rewrites the vendor's transcript file. A shim with nothing outstanding, or no
session bound yet, treats the signal as a safe no-op and logs that it did.

There is no rpc for this — it is a deliberate backdoor for an operator at a
shell, not a daemon-issued verb. To send it: find the shim's pid for the
workspace's socket, then signal it.

```
ps -eo pid,command | grep 'shim/dist/main.js .*<workspace-id>.sock'
kill -SIGUSR2 <pid>
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

## Runtime operations go through one door and one skill

Building, bouncing, hard bouncing, deploying, hot-reloading, health checks,
logs and one-off daemon requests all go through `bin/agent-repl-runtime <verb>`
(`help` lists the verbs; `call METHOD` reaches any unary AgentRepl rpc through
`claude-repld call`), and agents operate it through `/manage-agent-repl-runtime`
(`skills/manage-agent-repl-runtime/`), whose terminology table fixes what
"hard bounce", "bounce", "deploy", "workspace restart" and "hot reload" mean.
The scripts behind the verbs stay where their other callers reach them, but a
person or an agent uses the door.

**Any change to build, deploy, bounce or daemon-request infrastructure updates
`bin/agent-repl-runtime`, its harness (`bin/test-agent-repl-runtime.sh`), the
`/manage-agent-repl-runtime` skill and this section in the same commit.** Only
the lead session runs `bounce`, `bounce --hard` or `deploy`; subagents never do.

## A finished branch lands on master at once, with no question asked

Owner policy, standing (2026-09-27). A branch whose work is done — its
agent has reported and its tests are green — is merged into master
IMMEDIATELY by whoever receives the report, as a `--no-ff` merge commit.

- Nobody asks the owner for permission to merge, and nobody holds a finished
  branch "pending a go-ahead." The owner's earlier approval of the work IS the
  approval to land it.
- Open questions a report raises (a proto ruling, an edge case left for later)
  do not hold the merge. They are surfaced to the owner AFTER the branch has
  landed, and are answered by follow-up work on a new branch.
- The only thing that holds a merge is a red suite. A red suite is fixed on
  the branch, and the merge follows the moment it is green.
- Once merged, the branch is deleted and its worktree removed in the same
  breath. No finished branch or worktree is left behind.
- The merge is followed by the ONE deploy the next section requires.

## ONE deploy per complete change landing on master, and the daemon runs it

Owner design, standing (2026-09-23; supersedes the 2026-09-21 "deploy
immediately with bin/deploy-all.sh" ruling). THERE IS NO DEPLOY SCRIPT. The
daemon owns deploys (`daemon/internal/deploy`, `Deploy{force}` in
`proto/src/agentrepl/v1/endpoint_deploy.proto`): it builds, decides what is
out of date, and restarts what is, WHEN it may.

- A COMPLETE CHANGE landing on master — a merge commit — gets exactly ONE
  deploy, never one per commit.
  - A landing through the daemon's own merge (the workspace-merge flow) runs it
    automatically: the merge tells the deploy once, with every commit it
    landed, and never waits on the build.
  - A change merged onto master by hand is followed by ONE deploy, in the same
    breath and never behind a question:
    `modules/app/agent-repl/daemon/bin/claude-repld deploy` (or
    `M-x agent-repl-deploy` in Emacs).
- A change whose tests are red is not landed, so it is not deployed.
- The deploy's answer is its DECISIONS, one per component; what follows
  arrives on the surfaces that already carry it. A build failure deploys
  NOTHING and is the answer, loudly (`build_failed` naming the step, the tail
  of its output and the archived log under `<state>/deploy/logs/`).

## The `wsm schema version` goes up only for a breaking table change

The `wsm schema version` is the version of the daemon database's schema
(`wsm.db`, today's `wsm.LayoutVersion` in `daemon/internal/wsm/open.go`; older
docs call it the "state layout"). Owner ruling, 2026-10-01:

- It goes up ONLY for a BREAKING table change, one the running daemon cannot
  work beside: a table or column removed, renamed or repurposed.
- A new daemon with a different `wsm schema version` cannot take over from a
  running one, because it must migrate the database first and only one daemon
  may write it; such a deploy drains every workspace and stops then starts.
- An ADDITIVE change (a new table or column) does not raise it: the new daemon
  applies it once it is the only writer and does not touch the new tables
  before then, and the old daemon ignores what it does not know.
- The same rule holds for protobuf package versions (`proto/AGENTS.md`).

## A deploy never ends a turn unless it is FORCED

How the daemon puts each component into service:

1. It BUILDS every component into a staging directory
   (`bin/build-frontend.sh --out <staging> <target>` per target, after
   `make -C proto all`). A failure installs nothing and restarts nothing.
2. It judges STALENESS BY CONTENT HASH: each running process's reported build
   against the fresh one. Every process reports its build when it connects, as
   a REQUIRED field — Emacs on `WatchDaemon` (`elisp_build`), a webview on
   `WatchWebWorkspace` (`webapp_build`), a shim on every `SessionDiagnostics`
   frame (`shim_build`), and the store and sidecar in their build report
   (`agent-shim/logging/go/buildreport`).
3. It INSTALLS the staged artifacts atomically, then:
   - the STORE and SIDECAR restart at once when stale, in the recorded safe
     order: bootout the sidecar, kickstart the store, await `store.sock`,
     bootstrap the sidecar;
   - a stale DAEMON is replaced by the blue-green handover, and its successor
     bounces the stale shims it adopts through its own bounce registry;
   - each stale SHIM goes to the prompt queue's BOUNCE REGISTRY: bounced now
     when it has no turn and no detached work, otherwise registered and
     bounced on its freeness edge (a turn or the last detached item ending).
     A shim that DIES under a registered bounce is itself that edge: its work
     ended with it, so the bounce relaunches it at once, or is unregistered
     when this daemon ended the session, the workspace is closed, or a newer
     shim already serves it — never waiting for a revival.
     Queued prompts never block a bounce; the workspace drains and they are
     delivered to the new shim. Monitors, background shells and background
     subagents DO block it, because they die with the shim's vendor child.
     AN UNFORCED SHIM REPLACEMENT NEVER ENDS LIVE WORK: freeness is a reading
     the vendor can overtake (it starts a turn on its own the moment a
     subagent concludes), but the shim's `KillSession{force:false}` refusal is
     atomic and authoritative. A `live` refusal, or a stand-down the shim never
     answered and did not leave inside the window, is never forced: the old
     shim keeps serving untouched, the prelaunch is retired, the hold released,
     and the bounce is re-registered behind that work (`bounce.ErrDeferred`)
     and runs at the next freeness. Only a FORCED bounce, or a stand-down the
     shim answered without a `live` refusal and then did not leave (a hung
     shim, not live work), is force-killed at the window's end.
     A shim replacement and a handover transfer asked of one workspace
     coalesce into TWO stages, replacement then transfer, and never one in
     place of the other (daemon/AGENTS.md, "A coalesced bounce runs every
     kind it was asked");
   - a stale Emacs is pushed `reload_elisp` (the whole module set in
     `config.el` load order, then the heartbeat and timer re-arm check), and a
     stale webview `reload_webapp`.

EVERY PHASE IS ON THE FOOTER (owner request, 2026-09-27). The deploy states
each phase through ONE entry point (`internal/deployprogress.Sink`, the footer
resolver) and the line stands on every workspace's strip, below a fault and
above everything else: building, installing, restarting services, handing over,
a busy workspace's `waiting` (its own turn and background counts), and a
momentary `updated` the daemon that stays says, or the successor says after a
handover (it reads `Manifest.deploy`). A deploy's handover draws no webapp
banner; a successor that will not start is the `successor_spawn_failed` fault.
A build, install or service restart that FAILS takes the line down and stands
as the daemon-scoped `deploy_failed` fault (non-escalating: the running build
keeps serving), its line naming the step and the last line of its output. It
stands until a deploy gets through that step, and a daemon that boots owning
its state closes the ones an earlier daemon left.

A FAILED DEPLOY ROLLS BACK (owner ruling, 2026-09-28): any failure after the
install began puts every replaced artifact back and restarts every service
the deploy restarted onto the restored build, so the host is left exactly on
the build it ran before; the fault line says `rolled back`. A rollback that
fails is its own `deploy_failed` fault (the `rollback` step). Both stand in
the TOPBAR's warning strip as well as on every footer, and the next deploy
that gets through takes them down from both.

`force` does not wait: every stale shim is bounced at once and a stale daemon
hands every workspace over at once, ENDING RUNNING TURNS. Emacs asks before a
forced deploy; the CLI's `-force` says so in its help.
`UpdateShutdownSchedule{now}` is still the operator's emergency stop, and no
deploy calls it.

A DEPLOY IN FLIGHT IS NEVER STARTED AGAIN (`already_deploying`), a handover
in flight refuses a second (`already_rolling_out`, naming the holdouts), and
a successor still joining refuses (`joining`). A handover owns ONE successor
slot: one that fails after its spawn stops that successor (confirmed by the
reap) before it frees the slot, and one whose successor will not stop stays in
flight, so two successors never coexist. Every deploy decision is
recorded through the daemon's logger under `daemon.deploy.*`.

A running shim rides a store restart out on its retry buffer, which holds every
row until it lands and never drops one (its vendor stream pauses instead while
the backlog is high); the sidecar re-reads its files from its cursor.


## Every landed remediation gets a changelog line

`docs/REMEDIATION-CHANGELOG.md` carries one brief line per landed remediation,
newest first. Append to it in the same commit or merge that lands the change.
Owner instruction, 2026-09-12, standing for every realtest section.

Its purpose is REGRESSION WATCH across a long remediation effort. A performance
win or an invariant established while fixing one realtest must not be quietly
undone while fixing a later one, and this file is the only artifact that spans
the whole effort. Before landing anything, scan it for a line the change would
reverse, and say so if it would.

It is read back into context at every compaction, so it must stay cheap to
carry. One sentence per entry, in the shape of a classic changelog. No rationale
and no narrative: the commit message holds the reasoning and
`docs/REALTEST-JUDGEMENT-CALLS.md` holds the decisions.

It also carries a short "standing measurements to protect" section. Update those
numbers when a measurement genuinely improves; never when it regresses.

## Every optimization carries a comment saying it is one

Any code shaped by performance — a cache, a skipped check, a batched or reordered call, a narrowed watch set — gets a comment at the site naming the cost it avoids, with the measurement if there is one. A decision that looks strange because it is an optimization is never removed or "simplified" without asking the owner first.

## A regression fix leaves a breadcrumb at the fix site

Owner instruction, 2026-09-14, standing. When a change fixes a REGRESSION — a
behavior that once worked and broke — leave a brief comment at the implementation
site naming the regression it fixes (what broke, and the shape of the fix). One
or two lines is enough; the commit message and the changelog hold the detail.

Its purpose is FORWARD regression watch. The comment is not a lock: it does not
forbid ever changing or rolling back this code. It is a flag for the next person
who touches this spot that this exact behavior has regressed before, so a change
here is a place to watch for the same regression returning — and, when the risk
looks real, to surface to the user rather than land silently.

## A repository's one-shot policy is its own

Owner ruling, 2026-09-12. A repository states its one-shot and merge policy in
`.agent-repl/prompts/` at its main checkout root. This module's own `prompts/`
corpus is the policy of exactly ONE repository — the one this checkout lives in
— and is never a fallback for another. A repository that states none has its
one-shot creates REFUSED by the daemon (`CreateWorkspaceError`'s
`one_shot_policy_missing`), which Emacs draws as a warning; Emacs never reads
the filesystem to decide it.

`docs/ONE-SHOT-POLICY.md` is the reference for a repository's authors.

## A repository joins the roster from any path inside it, with its main worktree

`SPC j .` (`agent-repl-register-repository`) registers a REPOSITORY **and the
repository's main worktree as a workspace**. It is not `SPC TAB C-n`, and the
two are easy to confuse:

| binding | command | rpc | what enters the roster |
|---|---|---|---|
| `SPC TAB C-n` | `agent-repl-add-project-workspace` ("Add project directory") | `RegisterWorkspace` | a WORKSPACE at the DIRECTORY you name, whose repository row is minted as a side effect |
| `SPC j .` | `agent-repl-register-repository` ("Register repository from file") | `RegisterRepository` | a REPOSITORY resolved from any path inside it, PLUS a workspace at its main worktree |

It exists because a repository had exactly one way in — that side effect — so
`agent-repl-verbs--read-repository` (the static create's picker, `SPC TAB N`)
could offer only repositories some workspace had already minted, and a checkout
nobody had worked in yet could not be named at all.

THE GESTURE IS PICKING A FILE, not naming a repository root: `read-file-name`
defaults to the buffer's own file, and the daemon resolves the repository's
main worktree from ANY path inside it. Registering one the registry already
holds is SUCCESS and says so (`already_known`), never a refusal.

THE MAIN WORKTREE IS REGISTERED AS A WORKSPACE TOO (owner ruling, 2026-09-14:
"I expect the main repo to be added as an actual workspace as well"). The first
landing stopped at the repository row, and a repository with no workspace is
NOT SELECTABLE as a repository row: a repository with no workspace has no row
`SPC p p` (`agent-repl-switch-to-project`) could offer, so registering the
repository you were standing in still left you unable to switch to it.

That registration is the SAME ONE `RegisterWorkspace` runs — `internal/workspace`'s
unexported `register`, which both rpc bodies call — so the row gets the same
mint, the same git-derived naming, the same roster row, the same session
revival and the same refusals. Never write a second near-copy of it; one
directory must not have two registration behaviors depending on which rpc
announced it. The shared body raises its refusals with NO rpc name and each
verb stamps its own through `namedRefusal`, so a `RegisterRepository` refusal
never reaches the client naming `RegisterWorkspace`.

The success carries both halves and both already-known bools
(`workspace`/`workspace_already_known` beside `repository`/`already_known`),
which are INDEPENDENT: a repository registered before this ruling landed is
already known while its workspace is minted now. The ack says both.

IT BECOMES SELECTABLE WHEN THE ROSTER PUSH LANDS, not when the command returns.
The editor's registry is built by `agent-repl-roster-reconcile`, and the daemon
republishes the roster as the LAST act of the registration — before it answers —
so the push is on the wire ahead of the ack rather than waiting for a later one.

A repository whose section is drawn with NO rows is still an ordinary state
(every workspace under it closed), so neither the daemon's roster resolver nor
the webapp's rail may drop an empty section.

## `SPC p p` offers EVERY KNOWN workspace, not only the live ones

Owner ruling, 2026-09-20. `agent-repl-switch-to-project` with no argument is
the one switcher, and its candidate list is built from THE DAEMON'S ROSTER
(`agent-repl-verbs--all-rows`), which is the source of what exists. Emacs's
own `agent-repl--workspaces` table is not: it holds the workspaces that have
a PERSPECTIVE STANDING IN THIS EMACS, which excluded three real kinds of
workspace the user could not otherwise reach — one that was closed or killed,
one the daemon knows that no roster push has been reconciled into the table
yet (a just-registered repo whose landing is still pending), and one with no
local perspective for any other reason.

A live local workspace the roster does not carry is still offered, because
the push and the registry reconcile asynchronously and the switcher must
never drop the workspace the user is standing in. persp-mode's own
perspectives (`main`, `none`) are never candidates; `agent-repl--live-ws-names`
excludes them at its source.

Picking one is TWO behaviors and exactly two. A workspace whose tab is
standing is an editor-local `agent-repl--ws-switch` and costs no round trip.
Anything else goes through `agent-repl-verb-open` — the same `OpenWorkspace`
verb `SPC TAB O` runs, with the same `mutation-progress.el` stage reporting —
and the landing is registered with `agent-repl-verbs-select-minted` under
`agent-repl-verbs--land-on-tab`, so it fires when the roster push brings the
tab and lands BY IDENTITY rather than by directory. Never add a second open
path here: a switcher that opened workspaces its own way is a second
mechanism to disagree with the first.

The candidate strings are plain text and must stay that way: an open
workspace carries no affix, a closed one ` (closed)`, one the daemon knows
but this Emacs has no tab for ` (not open here)`. A name that collides with
another candidate's is qualified by its directory, because `completing-read`
answers with the string and two identical strings make the second
unreachable.

## Implementers do not judge proto design

An implementation agent lands a proto shape that is already settled, or it lands
nothing and describes the question. It never decides whether an addition belongs,
what shape it should take, or which oneof it fits. Owner instruction, 2026-09-12.

Contract shape is the owner's call, and this repo already holds that the proto's
framing wins any dispute. A shape chosen mid-task buries a contract decision
inside an implementation commit, where nobody reviews it as one.

So a brief either carries the exact text to add, or marks the item
description-only. When a modelling question surfaces mid-task, that item stops:
leave it unlanded and write up the options.

Deriving an arm from how the daemon already opens the fault, and matching the
shape of the arms beside it, is reading settled behavior and is in bounds.
Choosing between plausible shapes is not.

## No look-and-feel changes during bug remediation

Standing owner instruction, 2026-09-12. Remediation fixes defects. It does not
restyle the editor.

Do not change colors, faces, fonts, spacing, borders, padding, icons, glyphs,
window or panel proportions, tab-bar appearance, or the wording and phrasing of
anything the owner reads on screen, unless the change IS the bug being fixed or
the owner asked for it by name.

What remains in bounds:
- Behavior a defect report names, such as a panel that should open and does not,
  or a command that needs two presses instead of one.
- A visual rule the owner specified, implemented exactly as specified and no
  further.
- Logging, which the owner does not see on screen.

When a fix seems to call for a look-and-feel change, do the narrowest thing that
resolves the defect, then say what you would have changed and why, and leave it
for the owner to rule on. Restyling that arrives attached to a bug fix is hard
to review and hard to reverse, which is why it waits.

## User controls land with the user guide

Any new or changed user control (a key, a button, a click, a command the
user drives) lands in the same change as its entry in `docs/USER-GUIDE.md`.
That covers what the control does, its confirmation if any, and its limits.
The guide is how the owner learns a control exists, and a control missing
from it goes unused. Controls that predate the guide are not backfilled.

## The editor popup is the one place a file opens

Emacs has ONE path that shows a file or directory the system points at, named
**the editor popup**: `agent-repl-popup-open PATH [LINE]` in `lisp/popup.el`. It
is a Doom popup on the right at 40% of the frame's width, focused on open and
closed (buffer killed) by `q` in command mode; a file opens at LINE (1-indexed,
the top when absent) and a directory opens in dired. A path that does not exist
is refused, never created. It shares its display with `agent-repl-popup-show`
(a buffer rather than a path), so every agent-repl popup has one geometry.

- **Every agent-repl file open goes through it.** Its callers are the host
  stream's `open_in_editor` push (`agent-repl-host--open-in-editor`), the plan
  bubble's edit button and every findings-row location jump (both ride that
  push), the worktree divider's paths and `commands.el`'s link-code, and
  `notes.el`'s notes file. A call site never calls `find-file` or
  `display-buffer` for such a window itself, and a per-caller variant is the
  defect this exists to prevent.
- **Feed links in bubbles open through it too.** The webapp sends
  `OpenInEditor{feed_link}` and the DAEMON resolves the href, in this order:
  an absolute path; a path relative to the worktree root; a bare name as
  `<worktree>/modules/app/agent-repl/<name>`, then `<git root>/<name>`. A
  resolved link is relayed to Emacs as the same `open_in_editor` push a
  `workspace_file` target produces. An unresolved link draws the footer's
  transient "unknown file <name>" line (no status change) and queues a
  non-interrupting prompt to the agent quoting the source bubble and asking
  which file was meant (`PROMPT_ORIGIN_LINK_UNRESOLVED`). Emacs does no
  resolution of its own.
- **Deliberate exceptions, not file-opening for a reference:** switching to a
  project opens that project's most recent file in the ordinary editor window
  (`commands.el`, `agent-repl--switch-to-project`), and a new worktree's
  initial buffers are added to its perspective without being shown
  (`worktree.el`). Neither is a popup.

## UI changes require an explicit specification

Owner ruling, standing. UI/visual changes — layout, colors, sizes, borders,
animations, copy, keybinding-visible behavior, and the presentation of the
minibuffer, footer, tab-bar, or sidebar — are NEVER made without an explicit
specification from the owner.

Never infer a UI change: not from a bug report, not from a nearby edit, not
from a judgment that something would look better. The only latitude is a very
small refinement strictly necessary to REALIZE an explicit prescription, never
to extend or improve on it.

Any question about whether a change is covered by an existing specification
is asked before the change is made, not resolved by guessing. "Owner" means
the user; for a subagent, it means the parent agent that dispatched it — and a
parent agent that receives such a question does not rule on it itself, it
recurses the question up to the user in turn.

This sits beside "No look-and-feel changes during bug remediation" above: that
section rules out restyling attached to a bug fix specifically, and this one
rules out inferring a UI change under any circumstance. Remediation fixes
defects; it never restyles.

## Workspace status is a cross-surface invariant

Owner ruling, standing. A workspace's status renders on three surfaces: the
Emacs tab-bar, the webapp sidebar, and the webapp footer.

These three are STRUCTURAL invariants, not a convention kept in sync by hand:
none may ever disagree with either of the other two. All three derive from the
same daemon-published source of truth — the roster/status stream — and never
from a locally inferred or cached state that can drift out from under it.

Whether background tasks exist for a workspace is part of that status and
follows the same rule: the footer's background-task accounting must be
invariant with respect to the daemon's source of truth, never a
locally-kept count.

A disagreement between surfaces is a defect in whichever surface departs from
the daemon-published truth. Fix it at the source — the publisher or consumer
of that truth — never by patching the surface that looked wrong.

Any status change, from any origin (a user action, a backend event, or
anything else), restores a workspace's rendering to full (non-demoted)
display mode on every surface. (Forward reference: a sibling section covers
the viewed/partial display modes and demotion this restores from; read the two
together.)

## The webapp holds NO business logic; the daemon is the source of truth

Owner ruling, standing (2026-09-21). The webapp is a RENDERER of what the
daemon publishes, and as near as is reasonably possible it decides nothing.
Every fact it draws — a status, a substatus, a count, a label, an ordering, a
badge, whether a thing is live — is resolved by the daemon and pushed; the
webapp turns the pushed arm into pixels and stops there.

So a cross-surface invariant is NEVER repaired in the client. Reconciling the
footer's status against the sidebar's in the webapp, so the two read the same,
is forbidden even when it would make the screen look right: the invariant
belongs to the daemon (see "Workspace status is a cross-surface invariant"),
and a client-side fixup converts a daemon defect into a hidden one that every
other consumer — Emacs, a second webview, a test — still has.

The same rule governs anything that smells like a decision: inferring a state
the daemon did not state, deriving one field from another, counting rows to
label something, holding a shadow copy of daemon state to smooth a push over.
When a surface needs a fact, the fact gets published.

A surface that disagrees with another is a defect in whichever one departed
from the published truth. It is fixed at the publisher, or at the consumer's
reading of the publisher, and never by a correction layered on top.

THE ONE STANDING EXCEPTION is the client's own link verdict: when the call to
the daemon is what failed, no daemon can push that fact, so the webapp says it
itself. It is written down in `webapp/AGENTS.md`, it is the only one, and a
second exception is an owner ruling rather than a judgement call.

## The topbar's warning chip is the webapp's one error surface

Owner ruling, standing (2026-09-23). The topbar's warning chip and its
dropdown are the ONE canonical place the webapp makes an error visible to the
user: no overlays, banners, toasts or corner cards. The daemon's pushed
warnings are drawn there verbatim, and the page's own client-local failures
(the ones no daemon can push, because it may be what is unreachable) are
listed there too, with or without a topbar push. The chip is red (`--err`).
Every error is also always logged; the chip never stands in for the record.
The details live in `webapp/AGENTS.md`.

## An invisible action is a logging defect, not a test problem

When a test, an investigation, or a person cannot tell from the logs what the
software just did, the logs are what is wrong. This holds whenever it comes up,
not only when a test happens to assert on it.

The reflex to resist is treating the gap as the observer's problem: relaxing the
assertion, reading process state instead, or concluding the behavior is fine
because it visibly worked. A workspace switch that selects the right workspace
and underlines the right tab while writing nothing to any sink is still a defect,
because afterwards nobody can say it happened, in what order, or why.

The fix is to emit the missing record at the site that performs the action, once,
naming the action and the subject it acted on.

IF YOU CANNOT SEE IT IN THE LOGS, THE FIX IS ALWAYS THE LOGS. This covers every
diagnosis, not only tests, and it covers STATES as well as actions. A condition
that persists (a pinned WAL, a stuck queue, a held lock, a slow edge) is a
defect in the logs if nothing above verbose says it is happening. Probing a
live process, reading raw SQLite files, or running a one-off experiment may
show where the record belongs. None of them is the answer. The investigation
is finished when the record that would have shown the fault has landed, with
enough structured context to diagnose that fault from the log alone, and has a
test. On 2026-09-28 the store's checkpoints copied nothing for four hours while
the WAL passed 137 MB, and the only record of it was verbose. Finding it took
reading the `-shm` read marks by hand. The fix was the driver bug and ALSO the
`store.db.wal-pin` warning that would have named it.

CHATTINESS IS NOT A REASON TO STAY SILENT; IT IS A REASON TO PICK THE RIGHT
LEVEL. Do not skip a record because the log would get noisy, and do not promote
one to WARN so it is easier to find. Both are how a log stops being readable.

- ERROR and WARN are for conditions somebody must act on. The realtest harvest
  fails a run on every one of them, with no allowlist, so a routine event logged
  at WARN breaks the gate for everyone.
- INFO is for actions a person took and lifecycle a person would ask about.
- DEBUG is for the per-item detail that explains an INFO record when someone is
  already looking. High-frequency and loop-body records belong here.

A record that fires on a routine action almost always belongs at DEBUG or INFO.
Use the level the existing operations in the same file already use.

Follow the module's logging contract for shape and routing: one log function per
codebase, the established operation naming, and per-workspace routing so the
record reaches that workspace's sink. See `logging-contract.md` and
`docs/LOGGING.md`.

## Logs

`bin/logs.sh` is the one reader for persisted agent-repl records. It resolves a
workspace by daemon ID, absolute directory, or daemon display name; reads the
current file plus rotation generations `.1` (newest) through `.5` (oldest);
merges every selected runtime by timestamp; and fails with the source path and
line number when any selected JSONL line is malformed. Its default output is a
compact local-time line. Use `--json` when another program will consume the
records. An absent or unreadable sink and a workspace sink that is not a
symlink are findings: harvest includes attributed finding rows, other modes
summarize them on stderr, and the reader fails only when none of the selected
sinks can be read.

| evidence | path, including retained generations | writer process | format | select a run window | attribution | level switch |
|---|---|---|---|---|---|---|
| Emacs echo-area history | `*Messages*`; this is a live buffer, not a log and has no rotation files | Emacs | Emacs buffer text | read the live buffer, then delimit the relevant timestamps manually | buffer and message text only | none; this is not the durable logger |
| workspace Emacs | `<workspace>/.claude/emacs/emacs.log`; canonical symlink to its runtime-owned target, with `.1` through `.5` beside that target | Emacs | contract JSONL | `bin/logs.sh --workspace <id\|dir\|name> --runtime emacs --since <RFC3339\|duration> --until <RFC3339>` | `workspace_id`, `workspace_dir`, session and request fields when known | `AGENT_REPL_LOG_LEVEL` |
| workspace daemon | `<workspace>/.claude/emacs/daemon.log`; canonical symlink to its daemon-owned target, with target `.1` through `.5` | `claude-repld` | contract JSONL | the workspace recipe above with `--runtime daemon` | `workspace_id`, `workspace_dir`, session and request fields when known | `AGENT_REPL_LOG_LEVEL` |
| workspace shim | `<workspace>/.claude/emacs/shim.log`; canonical symlink to the daemon-owned target written through inherited file descriptor `3`, with target `.1` through `.5` | `claude-shim` | contract JSONL | the workspace recipe above with `--runtime shim` | `workspace_id`, `workspace_dir`, `agent_repl_session_id`, `claude_session_id`, `pid`, and `request_id` when known | `AGENT_REPL_LOG_LEVEL` |
| workspace webapp | `<workspace>/.claude/emacs/webapp.log`; canonical symlink to its daemon-owned target, with target `.1` through `.5` | browser forwards; `claude-repld` persists | contract JSONL | the workspace recipe above with `--runtime webapp` | `workspace_id`, `workspace_dir`, `connection_id`, and session/request fields when known | `AGENT_REPL_LOG_LEVEL` |
| workspace sidecar | `<workspace>/.claude/emacs/sidecar.log`; canonical symlink to its daemon-owned target, with target `.1` through `.5` | `shim-claude-sidecar` forwards; `claude-repld` persists | contract JSONL | the workspace recipe above with `--runtime sidecar` | `workspace_id`, `workspace_dir`, `claude_session_id`, `pid`, and file context | `AGENT_REPL_LOG_LEVEL` |
| central Emacs | `~/.claude-emacs/logs/emacs.central.log` and `.1` through `.5`; `AGENT_REPL_EMACS_GLOBAL_LOG` names a customized live path | Emacs | contract JSONL | `bin/logs.sh --central --runtime emacs --since <RFC3339\|duration> --until <RFC3339>` | genuine global records have no workspace; `pid` and known session/request fields remain | `AGENT_REPL_LOG_LEVEL` |
| central daemon run | `~/.claude-emacs/logs/daemon.run.log` and `.1` through `.5`; `$AGENT_REPL_STATE_DIR/logs/` replaces the default root | `claude-repld` | contract JSONL | the central recipe above with `--runtime daemon` | genuine global records have no workspace; `pid` and `request_id` remain when known | `AGENT_REPL_LOG_LEVEL` |
| central store | `~/.cache/agent-repl/log/shim-store.log` and `.1` through `.5`; `$XDG_CACHE_HOME/agent-repl/log/` replaces the default root | `shim-store` | contract JSONL | the central recipe above with `--runtime store` | `pid`, `request_id`, `agent_id`, and book keys when known | `AGENT_REPL_LOG_LEVEL` |
| central sidecar | `~/.cache/agent-repl/log/shim-claude-sidecar.log` and `.1` through `.5`; `$XDG_CACHE_HOME/agent-repl/log/` replaces the default root | `shim-claude-sidecar` | contract JSONL | the central recipe above with `--runtime sidecar` | genuine global records have no workspace; `pid`, agent, and file keys remain when known | `AGENT_REPL_LOG_LEVEL` |

Read `*Messages*` from the GUI Emacs through its application binary and the
server socket under `$TMPDIR/emacs501/`:

```sh
/Applications/Emacs.app/Contents/MacOS/bin/emacsclient \
  --socket-name "${TMPDIR}emacs501/server" \
  --eval '(with-current-buffer "*Messages*" (buffer-substring-no-properties (point-min) (point-max)))'
```

Build stamps are evidence, not logs. `~/.cache/agent-repl/bin/` contains the
installed `shim-store`, `shim-claude-sidecar`, and `shim-lock` binaries plus
their `.<name>.built-sha` and `.<name>.source-tree` one-line stamps, written by
`bin/build-frontend.sh` and installed beside each artifact by the daemon's
deploy. The store's and the sidecar's own build reports (pid plus content
hash, under `~/.cache/agent-repl/run/`) are what a deploy judges them by. None
of these have timestamps, rotations, workspace attribution, severity, or
`AGENT_REPL_LOG_LEVEL` behavior; use `bin/readiness-report.sh` to compare them
with the source tree and running artifacts.

### Resetting the record store

`bin/store-reset.sh` throws `events.db` away and brings the store and the
sidecar back up. The store holds nothing that is not re-derivable, and during
development it needs no retention (owner ruling 2026-09-13), so this is the
answer to a database that has outgrown its host — not pruning, and not a
migration.

```sh
AGENT_REPL_STORE_RESET=1 modules/app/agent-repl/bin/store-reset.sh
```

It refuses unless `AGENT_REPL_STORE_RESET` is exactly `1`. `--keep-down`
removes the files and leaves both services stopped. The sidecar stops first and
starts last (its cursors live in the file being removed), and the store's socket
is waited on in between — the same recorded safe order the daemon's deploy uses.
The rest of the rules live in `agent-shim/shim-store/AGENTS.md`.

AFTER A RESET THE SIDECAR RE-READS THE WHOLE CORPUS from offset zero, which is
hours of ingestion and a database that grows straight back. That is the cost of
the reset, not a defect of it.

To harvest the realtest remediation window, record RFC3339 instants immediately
before and after the run, then ask for every warning and error across every
daemon-known workspace and central sink:

```sh
from="2026-09-10T14:00:00-04:00"
to="2026-09-10T14:30:00-04:00"
modules/app/agent-repl/bin/logs.sh --harvest "$from" "$to"
```

The harvest table groups by workspace ID and directory, level, runtime,
operation, and message, with a count. Genuine central records are labelled
`central` with directory `-`. An empty window still prints the header and exits
zero. For exploratory reading use `--all`, `--level`, comma-separated
`--runtime`, `--follow`, and `--json`; `bin/logs.sh --help` is authoritative.

### Querying compactly — read this before dumping raw records

Every record is already structured JSONL, and `--json` dumps it verbatim, but
`--json`/no-flag output is expensive for a limited context budget: a session
that reads a window by eyeballing raw records burns tens of thousands of
tokens on records that answer nothing. `bin/logs.sh` answers the four
questions that come up over and over as COMPACT, first-class modes instead.
**Prefer `--tally` first to see what happened; then drill in with
`--sample`/`--fields`/`--timeline`; reach for `--json` only when a whole
record's every field is genuinely needed.** All four compose with the
existing selectors (`--workspace`/`--central`/`--all`, `--since`/`--until`,
`--level`, comma-separated `--runtime`) exactly like `--json` and the default
format do; `--sample` composes with `--tally`, and `--follow` works with
`--timeline`/`--fields` but not with `--tally`/`--sample` (an aggregate table
has nothing incremental to append to).

1. "What warn/error operations occurred in this window, and how many of
   each?" — `--tally`, one `count level runtime operation` line per group,
   sorted by count descending:
   ```sh
   modules/app/agent-repl/bin/logs.sh --all --level warn --since 30m --tally
   ```
2. "Show me ONE representative record per operation." — add `--sample N`
   (default a small N such as 1); with `--tally` it follows the count table,
   used alone it prints just the sample lines:
   ```sh
   modules/app/agent-repl/bin/logs.sh --all --level warn --since 30m --tally --sample 1
   ```
3. "What is the timeline of the interesting records for one
   process/workspace?" — `--timeline`, one `time level operation message`
   line per record in time order, message truncated to `--width` (default
   120):
   ```sh
   modules/app/agent-repl/bin/logs.sh --workspace <id|dir|name> --runtime shim --timeline
   ```
4. "Give me the full message and context for exactly one operation." —
   narrow with `--runtime`/`--level` (and shell `grep` on the operation name
   if needed), then `--fields` to print exactly the named top-level or
   `context` fields, nothing else:
   ```sh
   modules/app/agent-repl/bin/logs.sh --workspace <id|dir|name> --runtime shim \
     --fields operation,message,cause
   ```

A service's `.err.log` (raw stderr — `shim-store.err.log`,
`shim-claude-sidecar.err.log`) and a captured Emacs `*Messages*` snapshot
passed with `--messages FILE` are text, not JSONL: the contract permits the
former only when the canonical structured sink could not record a process's
own failure, and the latter has never carried structured fields at all.
Everything above is still queryable ONE way — `--tally`, `--sample`,
`--timeline`, `--fields`, the default format, and `--json` all include these
two sources whenever the selection covers them, by synthesizing the fields
they lack rather than dropping the line:
- `operation` is a stable synthetic tag: `stderr` for every `.err.log` line,
  `messages` for every scraped `*Messages*` line.
- `runtime` names the emitting service (`store` or `sidecar`) for `.err.log`,
  or `emacs` for a `--messages` snapshot.
- `level` is inferred: an `.err.log` line is `warn` by default and `error`
  when it plainly names one; a `*Messages*` line is included at all only when
  it matches one of the severity shapes `e2e/realtest/messages.go` looks for
  (the module's own `WARNING:`/`ERROR:` rungs, `display-warning`, an escaped
  lisp signal, the debugger opening, a load failure), each classified `warn`
  or `error` the same way — every other buffer line is prose and is skipped.
- `message` is the raw line, and `context` is empty; there is no workspace
  attribution for either source in this reader, so both read as `central`.
  `--messages` has no effect unless `--runtime` selects (or omits) `emacs`,
  the same composition rule every other synthetic and structured source
  follows.
`--harvest` keeps its established scope and does not gain these two sources.

**EVERY WORKSPACE IS SHOWN BY ITS NAME.** `bin/logs.sh` hands the reader the
daemon's ID-to-name table (`wsm.db`'s `workspaces`), and the reader stamps a
synthetic `workspace_name` on every record whose `workspace_id` it names:
the default format prints `workspace=<name>` in place of `workspace_id=` and
`workspace_dir=`, `--json` appends `"workspace_name"` as the record's last
key, and `--fields` projects it. The ID stays the records' join key, because
it is stable across a rename and never reused; the name is the daemon's
CURRENT name for it. A record whose ID the daemon no longer knows keeps its
ID. A daemon workspace with no name is refused. `--harvest` is unchanged.

`bin/logs.sh --help` documents every compact flag; the fixtures in
`bin/test-logs.sh` are worked examples of each mode, including a stderr and a
Messages fixture line.

The realtest harvest manifest's sibling `HARVEST-FULL.jsonl` (one JSON object
per finding, in the run directory — see `docs/LOGGING.md` and
`e2e/REALTEST-SPEC.md` "The harvest is collapsed, never filtered") is read the
same compact way instead of being dumped whole, since it is exactly this kind
of large flat JSONL:

```sh
jq -s 'group_by([.level, .source, .operation])
  | map({count: length, level: .[0].level, source: .[0].source, operation: .[0].operation})
  | sort_by(-.count)' HARVEST-FULL.jsonl
```

## The topbar's schema and organization are FIXED

Owner ruling, 2026-09-13, no exceptions. **The strip has one shape and only
one.** Every cell `frontend.v1 TopbarView` names is present on every
publication and in the same slot. There is no whole-view state that replaces,
suppresses or rearranges the strip, and none may be added — two such states,
`hibernated` and `cold_gate`, were landed and reversed the same day, and their
tags are reserved in `topbar.proto` against a revival.

A cell whose SESSION fact is unknown — hibernated, standing at the cold gate,
or simply not started yet — states that IN ITS OWN SLOT:

- `model_selector`, `effort_selector`, `permission_mode_picker` and
  `fast_mode` are ABSENT. Absence is how "no session has stated this" is said,
  and the webapp draws a dash in the slot rather than omitting the cell.
  `effort_selector`'s current level is the VENDOR'S OWN, pushed by the shim
  as `SessionUpdate.effort_changed` (`Query.getSettings()`'s
  `applied.effort`; owner ruling 2026-10-02). Before the session's first push
  the confirmed pick stands, and before that the config root's
  `settings.json` under `CLAUDE_CODE_EFFORT_LEVEL`. With none of them stating
  a level the selector is absent: a guessed level is never drawn.
- `context` is ALWAYS SET. A session-less workspace's chip states the context
  the session HELD — a hibernated conversation's size, or what a cold read
  would re-read — and 0 when none is known, never a blank (a blank reads as
  "loading", the one thing it is not). The reason the figure is not live is
  the FIRST section of the chip's hover breakdown.
- `warnings` is ALWAYS SET, and it carries the state as one warning LINE:
  "hibernated since 14:03", "cold context, awaiting your answer". That line is
  the one warning with no overlay behind it — it is a statement, not a
  control, and a cold gate is answered in the feed's gate card and nowhere
  else. An EMPTY list still means nothing is wrong and still draws no chip.

**The readiness gate is the workspace facts and nothing else** — the naming
(WSM's) and the account (the config root's). No session fact gates the
publication; they fill in as they arrive. Gating on one is what left the strip
BLANK for as long as a session-less state stood.

**The title is the workspace SUMMARY when the vendor has one.** The vendor
writes an `ai-title` line into the session transcript; the shim reads it
(`convert/session-title.ts`, `engine/title.ts`) and states it as
`conversation.v1 SessionUpdate.title`, and the resolver composes
`TopbarTitle.text` from it in preference to the workspace name. The branch
suffix is a different fact and its rule is unchanged.

## Tab-bar vocabulary: full, partial, and the extent rule

A workspace's tab is `[N] <workspace-name>`, and the status color reaches it
with one of two EXTENTS. The words are the owner's (ruling 5, 2026-09-13); use
them in code, comments, tests and prose.

- **full** — the whole `[N] <workspace-name>` entry carries the status color.
- **partial** — only `[N]` carries it; the name region falls back to the bar's
  own ground (`agent-repl-tab-unarmed`).

**THE RULE IS AN IF AND ONLY IF, PLUS THE VIEWED MODE.** A tab is partial
whenever that workspace's agent-repl panels — the webapp panel AND the input
window — are closed, and full when they are open AND the user has not already
seen what the workspace is showing. Nothing else may decide it: not selection,
not the arm. Ruling 5 (2026-09-13) had "panels open" as the whole rule and
struck the ready-view dwell that fought it; the owner's 2026-09-15 ruling put a
dwell back, GENERALIZED — see the next section, which owns it.

Where it is executable: `agent-repl--ws-agent-open-p` (status.el) is the
panels-open fact, `agent-repl--ws-display-state` applies the rule and returns
nil for partial, `agent-repl--ws-bracket-state` answers the orthogonal question
of what color `[N]` carries either way, and `agent-repl--tab-spec-bracket-only`
builds the partial appearance. A flip between the two extents is recorded once,
at DEBUG, by `agent-repl--note-tab-background-mode`, with the reason.

The three axes on a tab are independent, and each says one thing:

| axis | what it says | how it is drawn |
|---|---|---|
| color | the connection/lifecycle state | the arm's hue |
| extent | are the panels open | full vs partial |
| selection | which workspace the user is standing in | an underline under the NAME alone |

## The viewed mode: PARTIAL means "you have already seen this"

The same two words, `full` and `partial`, name a SECOND thing a workspace
carries, and it is drawn on the Emacs tab-bar and in the webapp sidebar alike.
A workspace whose panels the user has stood in front of for its DWELL is
demoted to **partial**: the tab's name
falls back to the bar's ground (`[N]` keeps the status colour) and the sidebar
row's name greys to `--muted`. **The status itself never recedes** — the
bracket and the sidebar dot keep their colour in either mode. The mode says
what the user has SEEN; the colour says what the workspace is DOING.

**THE DAEMON IS THE SINGLE SOURCE OF THIS MODE.** A roster row carrying
`RosterRowViewed` is **partial**; a row without it is **full**. Both the Emacs
tab-bar and the webapp sidebar RENDER the mode from that marker and nothing
else, so they cannot disagree.

**THE MARKER IS DERIVED FROM A READ FACT** (owner ruling, 2026-09-27). A turn
that completes or is interrupted leaves its result UNREAD;
`MarkWorkspaceViewed` on the row while it shows that turn-end arm (`done` or
`interrupted`) READS it; the next turn resets it. The daemon draws the marker
exactly when the row stands on a turn-end arm and the result is read, resolved
in the same render as the status. An UNREAD turn end outranks `idle_async`, so
a turn that finishes while detached work runs shows its green `done` (or
`interrupted`) until the user has viewed it; once viewed the row shows
`idle_async`, full; when the work ends the row returns to its turn-end arm
PARTIAL, never a fresh full one claiming an unread result.

**Emacs DETECTS and REPORTS; it does not decide.** The dwell is measured in
Emacs, because only Emacs knows what the user is standing in front of, and its
threshold is chosen in ONE place, `agent-repl--tab-dwell-seconds` (status.el;
owner ruling, 2026-10-02):

| case | dwell |
|---|---|
| a `done` lands while the user is viewing the workspace | 1 s (`agent-repl-tab-dwell-fast-demote-seconds`) |
| the user walks into a `done` whose row carries `RosterRowDetachedLive` (background work runs; once read it shows yellow `idle_async`) | 1 s |
| the user walks into any other turn end that already stood; any interrupted, failed or vendor-blocked row | 5 s (`agent-repl-tab-dwell-demote-seconds`) |
| a `/clear` or compaction completes | none: the daemon reads the result in `SetTurnEnded` and the push that ends the cut already carries the marker |

There is ONE pending dwell (`agent-repl--tab-dwell-pending`), and its timer
carries the generation it was armed under: a timer that fires after a re-arm,
a drop or its own demotion finds a different generation and does nothing, so
a superseded timer cannot demote anything. The webapp runs no timer at all.
When it is satisfied, `agent-repl--tab-view-partial` (status.el) reports
the workspace (`agent-repl-host-mark-viewed` -> `MarkWorkspaceViewed`) and
repaints, and latches nothing locally. The tab turns partial when the next
roster push carries the marker: `agent-repl--tab-dwell-demoted-p` reads only
`agent-repl-roster-viewed-for-ws` (roster.el). That one roster round trip of
visible latency is ruled acceptable.

| | applies PARTIAL | restores FULL |
|---|---|---|
| daemon (the source) | `sidebar.Resolver.SetViewed` reads the result | `wsState.viewedOn` derives the marker; `startTurn`/`SetTurnEnded` reset the fact (daemon/internal/resolve/sidebar/state.go, resolver.go) |
| Emacs (renders the marker) | reports via `agent-repl--tab-view-partial` (status.el) | `agent-repl--tab-view-restore-full` re-arms the dwell (status.el) |
| webapp (renders the marker) | `viewedMode` (webapp/src/sidebar/viewed.ts) | the same function, from the wire alone |

Emacs's restore reports NOTHING: the daemon originated the clear. It hangs off
ONE hook, `agent-repl-roster-viewed-cleared-functions` (roster.el), which fires
per workspace on the marker's present->absent edge (a restated marker is not a
clear, and neither is a first sighting without it). The reaction only re-arms
the dwell clock, since the tab already draws full from the row.

## "WatchDaemon", "the daemon watching in Emacs" and "the editor stream" are one thing

All three names mean Emacs's `WatchDaemon` subscription
(`agentrepl.v1.WatchDaemon` with the `WatchDaemonEmacs` client arm): the one
long-lived daemon stream an Emacs process holds, carrying the roster, the
elisp reload, persistent Wi-Fi, the daemon's standing faults and the startup's
`DaemonStartupEvent`s. Use any of the three; never use them for the per-workspace
`WatchHostWorkspace` stream, which is "the host stream".

- **`WatchDaemonEmacs.instance` is required.** Emacs mints one `EditorInstance`
  per process (`agent-repl-editor-instance`), and the daemon refuses a watch
  without it.
- **A NEW instance runs the editor's startup; a reconnecting one does not.** The
  daemon brings every registered workspace up (hibernated ones too) and emits,
  on that stream only and never replayed, `opening`, each workspace's steps,
  one `workspace_open` go-ahead per workspace strictly in registry order, and
  `finished`. Emacs pre-creates every workspace's input buffer and webview,
  opens a tab only once its go-ahead arrived and its page drew
  (`lisp/startup.el`), and echoes each step as one minibuffer line.
- **A new instance also re-stands today's dismissed news digest** without a new
  run.

## A standing gate hides the input window and docks its banner

Owner, 2026-10-02. A gate is a choice whose answer replaces the composer;
only the cold gate is one today. While it stands the daemon states it twice
from the one cold-gate standing: `HostWorkspace.gate` on the host stream and
the root feed's standing `FeedColdGate` row. Emacs reads the first
(`agent-repl-input-hidden-p`, `lisp/window.el`) and lays the workspace out
with no input window; the webapp reads the second (`src/feed/gate-dock.ts`)
and moves the banner into `#gate-dock`, a full-width row at the page's very
bottom spanning the sidebar's column too (the sidebar keeps its height),
fit to its content and at most the input window's height, drawing no inline
gate. Its buttons sit on one line, equal width, each choice's explanation a
hover tooltip. Answering
or retracting the gate restores both. Never gate either side on a signal the
other cannot see.

## What each status color means

Owner ruling, 2026-09-28. A workspace's status color answers ONE question on
every surface — the Emacs tab bar, the webapp sidebar dot and the footer
strip: what state is this workspace in, and can I use it? `proto/vocab/render-colors.json`
is where the assignment is executable (Go, TypeScript and elisp each assert
against it), and this table is what it means.

| Color | Meaning | Usable? | Statuses |
|---|---|---|---|
| Red | The agent is working. | Yes: a prompt is held or interjected. | submitting, thinking, clearing, compacting; footer `working`, `loading` |
| Yellow | The main thread is idle while detached work (background subagents, shells) runs. | Yes | `idle_async`; footer `background` |
| Green | Ready for you: idle, or waiting on your input. | Yes | ready, done, interrupted, permission; a merge that landed (`merged`); footer `idle`, `waiting`, `interrupted`; a Stop hook's deliberate stop and a deferred tool read as done |
| Purple | A merge is in progress; the daemon holds the workspace. | No: the composer is closed. | `merge_queued`, `merging` |
| Turquoise | Something unexpected went wrong and wants your attention, but the workspace is usable. | Yes | roster `turn_failed` (a failed turn restored from the durable record only; live, a failed turn stands as its fault, below); `merge_failed`; `degraded`; every VENDOR FAULT: roster `vendor_fault` / footer `vendor_fault · vendor_retry`, `vendor_rejection`, `vendor_failed` (the vendor will not start), roster `vendor_blocked` / footer `vendor_fault · auth`, `usage_limit`, `billing`, `vendor_error` (a vendor or account block, and every turn the vendor ended or refused, until the next turn starts), roster and footer `api_retrying` (the vendor is retrying the turn's failed API call) |
| Blue | The workspace is unusable right now. | No: the composer is closed (except under `turn_died`, declared in `composer_open_substatuses`). | every AGENT-REPL FAULT: roster `init`, `severed`, `dead`, `start_failed`, `turn_died` / footer `agent_repl_fault · starting`, `degraded`, `severed`, `dead`, `start_failed`, `daemon_impaired`, `turn_died` (the last turn's vendor query or agent process died, until the next turn starts); every NETWORK FAULT: roster `network_fault` / footer `network_fault · offline`; footer `closing` |
| Uncolored | There is no lifecycle to report. | — | `none` (never had a session), `inactive` (no open perspective, drawn `?`) |

### The three fault domains

Owner ruling, 2026-10-02. Every fault a workspace can stand in belongs to
exactly ONE of three domains, named for whose services are failing.

| Domain | Whose services fail | Color | Composer | Roster arms | Footer status |
|---|---|---|---|---|---|
| `agent_repl_fault` | agent-repl's own: the daemon, the shim, the store, the sidecar, the link between them | Blue | Closed | `init`, `severed`, `dead`, `start_failed` | `agent_repl_fault` |
| `network_fault` | this machine's network: the vendor cannot be reached at all | Blue | Closed | `network_fault` | `network_fault · offline` |
| `vendor_fault` | the vendor's or the account's: it will not start, refuses, blocks or retries | Turquoise | Open | `vendor_fault`, `vendor_blocked`, `api_retrying` | `vendor_fault` |

- **Precedence is `agent_repl_fault` > `network_fault` > `vendor_fault`**, on
  the footer, the roster, the health wire and the ladder alike
  (`daemon/internal/resolve/ladder`: `AgentReplFault` above `NetworkFault`
  above `VendorFault`). A vendor that cannot be reached because the machine is
  offline is a network fault, and a network that cannot be judged because the
  shim is down is an agent-repl fault.
- **The shim is the one judge of network versus vendor.** One classifier
  (`shim/src/engine/failures.ts#classifyAgentFailure`, its `network` verdict)
  labels every vendor-start failure: `StartSessionVendorStartRetryable.cause`
  is always set (`network` or `vendor`), and the shim opens the
  `network_unreachable` session fault on its diagnostics push while it cannot
  reach the network and resolves it on the first message that proves it can.
  The daemon opens `KindNetworkUnreachable` from that push and never re-judges.
- **A network-caused start failure does not spend the vendor's ten-minute
  window**: it closes the vendor retrying fault, stands as `network_fault`, and
  retries on its own backoff until the network returns.
- **A vendor fault leaves the composer open, and every prompt sent under it
  is held "after reconnect"** (owner ruling, 2026-10-06). A prompt sent while
  the vendor will not start is held under the reconnect hold and delivered
  when the session comes up. A prompt sent during a MID-SESSION vendor block
  (`auth`, `usage_limit`, `billing`, `vendor_error`, `api_retrying`;
  `footer.Resolver.VendorBlock`) is held on the same hold at once, UNCLASSIFIED
  and never sent; nothing is classified while it sits there. When the block
  stops standing (the footer's vendor-serves edge: a session start, a
  rate-limit verdict that is not rejected, a new turn, the retried call
  answered, the retried turn ending) the held prompts are popped in order and
  classified THEN, each against what is ahead of it, and delivered per the
  verdict (`daemon/internal/promptqueue/vendorblock.go`).
- **`network_unreachable` is recorded at INFO**, in the shim and the daemon:
  the machine being offline is the environment, not an agent-repl defect.

The rules that keep this true:

- **The footer's color is the sidebar's and the tab bar's color, always**
  (owner ruling, 2026-10-01). If the footer is blue, the sidebar dot and the
  tab are blue, and the same for every color. Every footer status that claims
  a ladder rung has a roster arm on the same rung, both resolvers are fed the
  same facts, and `TestTheFooterAndTheRosterAlwaysMakeTheSameCoarseClaim`
  (`daemon/internal/resolve/sidebar/onestatus_test.go`) holds the two
  together; a fact only one resolver sees is a defect to close by feeding the
  other.
- **A turn whose API call the vendor is retrying is a vendor fault,
  turquoise** (owner ruling, 2026-10-02, superseding 2026-10-01's blue):
  footer `vendor_fault · api_retrying` with the retry line, roster
  `api_retrying`. It stands until the retried agent is answered
  (`ladder.RetryAnswered`), the turn ends, or a new turn opens. A prompt sent
  during the retry is held after reconnect, unclassified (owner ruling,
  2026-10-06, superseding 2026-10-01's interrupt-the-wait), and is classified
  against the running turn when the retry ends.
- **Blue is only "unusable", and only an agent-repl or a network fault is
  blue.** Something that went wrong while the workspace stays usable is
  turquoise, never blue; a vendor fault is always turquoise. An expected state that awaits you (a
  permission ask) is green, never blue. A merge never parks: one that gives up
  is `merge_failed`, turquoise, and the workspace is back with you.
- **One classifier decides a failure's color.** `ladder.ClassifyFailure` sorts
  every turn-ending agent failure into a vendor or account block, a failure the
  vendor ended or refused, the query dying, or an expected stop, and
  `ladder.ResolveTurnFault` turns the turn's close and that class into the
  TURN FAULT both the footer and the roster raise from the same close (owner
  ruling, 2026-10-06): a vendor fault (turquoise; footer `vendor_fault ·
  vendor_error` or the block's own step, roster `vendor_blocked`), an
  agent-repl fault for a query or agent-process death (blue; footer
  `agent_repl_fault · turn_died`, roster `turn_died`, composer left open), or
  none for an interrupt or an expected stop. It stands until the next turn
  starts, and the footer's activity line is the daemon's per-cause sentence
  (`resolve/turnfault`), the one the feed's turn-end row carries. The feed
  draws the event as an OUTCOME MARKER (`frontend.v1.FeedOutcomeMarker`), never
  a bubble.
- **The status ladder ranks every unusable rung above every usable one**
  (`daemon/internal/resolve/ladder`), so a blue claim is never hidden under a
  turquoise one.
- **A repository's fold is the daemon's** (`agentrepl.v1.FoldRepository`,
  `frontend.v1.RosterRepoSection.fold`): the sidebar draws it (header grey:
  very light expanded, darker collapsed) and the Emacs tab bar hides a
  collapsed repository's workspaces, numbering and navigating only the drawn
  tabs (`agent-repl-roster-drawn-tab-order`). Task and merged folds stay
  webview-local.
- **The webapp composer is closed exactly when the footer is blue**
  (`render-colors.json#composer_closed_colors`, read by
  `webapp/src/vocab.ts#composerClosedFor`). The gate is derived from the color,
  never from a list of arm names, with one kind of exception: a substatus
  DECLARED in `render-colors.json#composer_open_substatuses` keeps it open
  under a closing color (`agent_repl_fault · turn_died` alone: its fault ends
  at the next turn, so a closed composer would make it permanent;
  `api_retrying` is a vendor fault and turquoise, open by its color). A merge in flight (purple) leaves it open:
  what is submitted is held until the merge ends, and a prompt held so keeps
  the workspace open past the landing. Emacs's composer is gated by the
  daemon's host composer arm instead (a drain, a restart; a holding merge
  lease answers `open`), and while the
  workspace is unusable it still takes prompts and holds them durably (the held
  ingress), per the ruling that held prompts survive outages.
- **Whether a status draws FULL (unread) or PARTIAL (viewed) is independent of
  its color.** Nothing about color changes the viewed mode.
- **Each renderer keeps its own shade, never its own assignment.** A surface
  that must diverge declares it in `render-colors.json#surface_overrides`; none
  does today. The sidebar's disc fill and merge-glyph ink are the arm's tone
  (`#ws-sidebar .st.tone-*`), and no later rule may recolor a mark.

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

## The working status names the main agent's step, and no line is composed from a landed feed item

The `working` status's step says what the main agent is doing now, sync only —
`thinking` (an inference call, no call running), `executing`, `reading`,
`writing`, `searching`, `fetching`, `delegating`, beside `submitting`,
`clearing` and `compacting`. The latest of the main agent's running calls names
it; a subagent's calls and detached work never do. The daemon's footer resolver
reads it (`daemon/internal/resolve/footer/workstep.go`).

The activity cell never carries a line composed from the feed item that landed
last (`✅ Bash finished — handling result...`, `✅ Prompt delivered — awaiting
response...`). That was the QUIET tier, and it is RETIRED (owner ruling,
2026-10-01: too chatty, next to no information). Between two feed items the
cell draws whatever the three tiers below resolve to.

## "Fresh input" is the one token quantity every spend figure counts

**Fresh input** is every input token of an API response that was NOT a cache
hit: the vendor's `input_tokens` plus `cache_creation_input_tokens`, which the
wire carries as `conversation.v1.TokenCacheMisses` (unwritten plus written).
Cache reads are not fresh; output is not fresh (it comes back as input on the
next request, and is counted there). Cache writes ARE fresh on purpose: they
are the expensive part of a turn, so a cold cache re-writing the whole prefix
must show up as a big figure. The daemon computes it in exactly one place,
`daemon/internal/freshinput`, and a test fails any other site that sums the two
misses by hand.

Every figure is ONE agent's fresh input, never its subagents':

- the footer's tokens cell (`frontend.v1.FooterTokensCell`) is the MAIN agent's
  fresh input for the in-flight turn, colored on a green → yellow → orange →
  red gradient with stops at 0, 30k, 50k and 100k;
- a response bubble's cost corner (`frontend.v1.FeedResponseUsageStamp`) is
  the fresh input its agent added since that agent's previous bubble landed,
  frozen when it lands, so a turn's bubbles partition the footer's figure; the
  green final-answer bubble carries the turn's whole figure;
- a subagent card's figure (`frontend.v1.FeedSubagentTokens`) is that
  subagent's fresh input over its whole lifetime.

The topbar's context chip is a different fact — the context window's size —
and none of the above is derived from it. With a warm cache the footer stays
below the chip; after a cold cache it can exceed it.

## Every daemon fault kind reaches the footer, and this is where each lands

The owner's ruling of 2026-09-13. Before it, three of the nineteen fault kinds
reached the strip, by bespoke paths beside the fault rather than derived from
it; the other sixteen rode only `WatchHostWorkspace`, which Emacs subscribes to
and the webapp does not.

**THE MAPPING IS DECIDED IN ONE PLACE**, `daemon/internal/health/footer.go`, and
stated normatively in `proto/src/frontend/v1/footer.proto`'s `FooterStatus`
header. The resolver TAKES that verdict and draws it; it derives no mapping of
its own, and a new fault kind added to the vocabulary without a row in that
table draws nothing at all.

**THE FOOTER LEARNS FROM THE ONE PLACE FAULTS ARE WRITTEN.**
`health.ObserveFaults` decorates the state client every raise site shares, so a
fault opened anywhere lands on the strip. Do NOT add a footer call beside a
raise: that is exactly how three kinds came to have a path and sixteen did not.

| status | substatus | fault kinds |
| --- | --- | --- |
| `agent_repl_fault` | `start_failed` | `shim_start_failed`, `resume_failed`, `relaunch_resume_failed`, `adoption_window_expired` (session scope), `cold_gate_reopen_failed` |
| `agent_repl_fault` | `dead` | `shim_died`, `bounce_died`, `session_absent` |
| `agent_repl_fault` | `severed` | `link_severed`, `watch_open_refused` |
| `agent_repl_fault` | `daemon_impaired` | `prompts_dir_missing`, `wsm_read_only`, `log_sink_poisoned`, `successor_spawn_failed`, `daemon_state_unreadable`, `adoption_window_expired` (daemon scope) |
| `network_fault` | `offline` | `network_unreachable` |
| `vendor_fault` | `vendor_retry` | `vendor_start_retrying` |
| `vendor_fault` | `vendor_rejection` | `vendor_start_rejected` |
| `vendor_fault` | `vendor_failed` | `vendor_start_failed` |
| unchanged | unchanged | `shim_reported`, `classifier_failed`, `bounce_unknown`, `conversation_abandoned`, `deploy_failed` (daemon scope) — NON-ESCALATING |

The three statuses are the three fault domains ("The three fault domains",
above): `agent_repl_fault` and `network_fault` are blue, `vendor_fault` is
turquoise, ranked in that order when several stand.

The activity cell is `FooterStatusActivityFault{kind, detail}` in every case but
two. `shim_start_failed` keeps `FooterStatusActivityStartFailed`, which now
carries only the cause: a failed bring-up never drops held prompts any more.
The three vendor-start kinds use `FooterStatusActivityVendorStart{text}`, a line
the daemon composes whole and the clients draw verbatim ("Claude SDK did not
start (attempt N): <cause> · retrying", "Claude SDK refused to start: <cause> ·
restart: SPC o C-c", "Claude SDK failed to start · restart: SPC o C-c").

**A VENDOR START THAT FAILS IS RETRIED, AND THE FOOTER SAYS WHICH OF THREE
THINGS STANDS.** `vendor_retry` is a retryable failure on the daemon's capped
backoff (x1.5 from 200ms, capped at 5s) for ten minutes of wall time from the
first failure of the run; `vendor_rejection` is a failure the shim labeled
non-retryable and is never retried; `vendor_failed` is the ten minutes
exhausted. All three are vendor faults, turquoise, with the composer open: the
workspace is usable and waits on the vendor. A start that failed because this
machine is offline is not among them: the shim labels it `network`, and it
stands as `network_fault · offline` instead. A prompt submitted while the
session is down is held under the reconnect hold
(`HeldPromptReconnectHold`, badge "after reconnect") and delivered when a
session next comes up; it is never drawn and then lost.

**`SPC o C-c` (`RestartWorkspace`) HAS ONE MODE: IMMEDIATE.** It interrupts the
running turn and stops all detached work with bounded calls, bounces the
workspace's shim (rebuilt first when stale, session resumed, forced teardown
hard-killing what did not stop), releases reconnect holds and reloads the
workspace's webapp page. It never touches the daemon, the store or the sidecar.
`RestartWorkspaceRequest.force` and the `no_session` error arm are retired; Emacs
sends the workspace alone and the webapp menu has one "Restart" entry. It is
for a stuck workspace (a turn that never ends, a vendor that failed to start, a
stale shim, a page out of sync), not a routine action: a fresh shim or page may
speak an API the older running daemon does not. The user-facing account is in
`docs/USER-GUIDE.md`, "Restarting a stuck workspace".

**THE FOUR NON-ESCALATING KINDS LEAVE THE STATUS ALONE** and take the activity
cell only. The shim ANSWERED in every one of them: a shim that pushed a
diagnostic is alive, a classifier run is a headless side errand, an undetermined
bounce disposition is an accounting question for a human, and an abandoned
conversation is what a SUCCESSFUL fresh bring-up left behind. It is not a
wording question — `agent_repl_fault` closes the webapp's composer
(`webapp/src/main.ts`), so escalating any of them would lock the user out of a
session that is serving perfectly.

**A DAEMON-SCOPED FAULT STANDS ON EVERY WORKSPACE'S STRIP**, because it is every
workspace that is owed the service the daemon cannot give.

**THE LINK STATE STILL OUTRANKS A FAULT** for the agent-repl fault step: the link
state is the live truth about the link, and a fault is the standing record
beside it.

### Every fault kind declares when it ends

The owner-approved plan of 2026-09-28
(`docs/investigations/2026-09-27-footer-fault-lifetimes-plan.md`). A fault
line stands until something closes its record, and before the plan several
kinds had no closer at all: `bounce_unknown` stood on a strip for over 30
minutes on a workspace that was serving.

**THE LIFETIME IS DECLARED IN ONE TABLE**, `daemon/internal/health/lifetime.go`,
beside the footer partition. Each kind is either STANDING until one of its
named recovery edges (a healthy attach, a started session, the next turn, a
serving successor, a deploy step that got through, a boot, ...) or MOMENTARY
(closed as it is recorded, or never recorded at all).

**A RECOVERY EDGE CLOSES THROUGH ONE DOOR**, `health.CloseOnEdge` (or
`health.CloseFaultOn` for the one fault a resolver tracks). It closes every
standing fault in scope whose kind declares that edge, and records each close
at INFO under `daemon.health.close_on_edge` with the kind, the fault, the edge
and how long it stood. Do NOT list kinds at a call site: add the kind's row to
the table. `lifetime_test.go` fails a kind with no lifetime and an edge with no
production caller.

## Footer activity lines are salient, transient, or enduring

The owner's rulings of 2026-09-28 through 2026-10-01 (the design records are
`docs/protobuf-design/footer-activity-tiers.md` and, for the quiet tier's
retirement, `docs/protobuf-design/2026-10-02-ui-lifecycle-wave.md` decision 1). The strip's activity cell is
meant to be ACTIVE: continual feedback that the session is doing something,
never a line that stands for an hour because nothing arrived to clear it, and
never an empty cell. It is always exactly ONE line, cut off with an ellipsis
when it would overflow. Every activity kind belongs to exactly ONE of three
tiers, and the tier is defined by WHAT ENDS the line:

| tier | ends when | examples |
| --- | --- | --- |
| **salient** | the condition it describes stops being true — never a timer | escalating faults (a severed link, a failed bring-up, an impaired daemon); anything waiting on the user (a gated call, a question batch, the cold gate, the agent's `PushNotification` message, which ends at the next prompt); an act in progress with its own end signal (a compaction running, an interrupt, a refused close, a deploy, a pending wakeup, a retry until the response lands); the context-budget warning (ends when a cut shrinks the context); the dead-query line (ends at the next prompt) |
| **transient** | its 10 s display window lapses, or a newer transient replaces it | tool-call starts; task-tracker moves; the `submitting` line with the held-prompt queue and classification progress; a concluded compaction; every non-blocking error or warning (non-escalating faults, daemon Warn/Error records); session changes; a finished deploy; network-resume edges; a detached run finishing while the session is `background` |
| **enduring** | never — it is always true, so the cell is never empty | the 5-hour and weekly usage (`unobserved` until a figure is read), fed by account-usage samples and by the vendor's rate-limit events, which stand no salient line of their own (owner ruling, 2026-10-01) |

**PRECEDENCE IS BY TIER, ALWAYS:** salient, then transient, then enduring. A
transient never covers a salient line; it covers only the enduring line, and
when it lapses the enduring line shows again. Within the
salient tier the contract's precedence decides (the kind that explains the
standing step first, then a fault, then a deploy's progress, and so on);
within the transient tier the NEWEST wins, so submitting a prompt (which
raises the `submitting` transient) replaces whatever transient stood.

**NO LINE IS COMPOSED FROM A LANDED FEED ITEM.** A fourth, QUIET tier once
stood a line worded from the feed item that landed last until the next one
surfaced. It is retired (owner ruling, 2026-10-01), end to end: the contract
has no shape for it, the daemon composes nothing of the kind, and the webapp
holds nothing back for it.

**THE ENDURING LINE IS THE USAGE, AND ONLY ITS PERCENTAGES ARE COLORED.**
The line draws the 5-hour and weekly allowances (and overage when present),
or reads `unobserved` before any figure is read. Only each `<number>%` wears
the percent gradient; labels and reset countdowns stay the line's color. The
line draws no reading age: it is enduring, so when it was read does not
matter. The context window's fill is not an enduring line (owner ruling,
2026-09-30); the topbar's context chip carries it.

**A TIMER MAY END ONLY A TRANSIENT.** A salient line with no clearing
event is a missing end signal, and the fix is the end signal, not an expiry.
The transient window is a presentation choice, which is why it alone is timed,
and it is never shortened or lengthened by what arrives next: the next event
may never come.

**THE DAEMON DECIDES THE EXPIRY; THE CLIENT APPLIES IT.** Every push carries
the standing transient WITH its expiry instant, AND the enduring line beneath
it. The client draws the transient until its clock passes the expiry, then the
enduring line — the same
client-side ticking from a shipped instant the turn clock already does. The
daemon runs no expiry timer and pushes nothing when a transient lapses, so
what is drawn is a pure function of the last push and the client's clock.

**NON-BLOCKING ERRORS AND WARNINGS ARE TRANSIENT.** Only a condition the user
must see until it ends is salient; a non-escalating fault announces as a
transient, and its standing record lives elsewhere (the topbar's warning
strip, the status panel), never as a line pinned over the session's feedback.

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

## Residue shape catalog

The sidecar persists NO residue — `vendor_specific` of any kind, `unknown`, and
`unparsed` bytes are classified and withheld. That takes the bytes out of the
store and, with them, the only evidence the vendor emits that line at all, so
the SHAPE is catalogued in their place (owner ruling 2026-09-13,
`docs/REALTEST-JUDGEMENT-CALLS.md`, "the unmodelled-line shape catalog").

WHAT IT IS: one row per distinct recursive key structure, in the store's
`residue_shapes` table — the hash, the canonical rendering, the residue kind,
the first example verbatim, first/last seen, and a count. Key names and scalar
TYPES only; values never reach it, except the one example per shape that makes
the row readable.

WHY IT IS BOUNDED: the row count grows with the number of shapes a vendor emits,
not with traffic. Object keys that look like generated ids collapse to a single
wildcard `*` so an id-keyed map cannot mint a shape per line — the exact rule,
clause by clause, is in `agent-shim/claude/shim-sidecar/AGENTS.md` under "the
residue shape catalog".

HOW TO QUERY IT:

```
make -C agent-shim/shim-store shapes
make -C agent-shim/shim-store shapes ARGS="--kind unparsed --example --limit 20"
```

It calls the store's `ListResidueShapes` rpc over the running store's socket.
Nothing in the running system reads the catalog; it exists so a human can ask
what the vendor is emitting that this system does not model.

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

## Committing to master means deploying what you changed

Every component here is a built artifact, and every running process keeps
serving the build it started with — so a commit deploys nothing on its own. A
change is finished when the process serving the user is running it, and your
report says what the deploy decided.

A merged-but-undeployed fix looks exactly like a fix that does not work, except
the correct code sitting in `git log` makes it harder to diagnose.

THE DEPLOY IS THE DAEMON'S, for every component alike — shim, webapp, daemon,
store, sidecar and elisp (see "A deploy never ends a turn unless it is
FORCED" above). Nobody hand-builds into `~/.cache/agent-repl/bin`, hand-
kickstarts a service or hand-loads elisp to deploy a change: those paths skip
the staleness judgement and the recorded safe order, and they are how a
service ends up running a binary no build report accounts for.

```sh
modules/app/agent-repl/daemon/bin/claude-repld deploy          # decisions, one line per component
modules/app/agent-repl/daemon/bin/claude-repld deploy -force   # ENDS RUNNING TURNS
```

`bin/build-frontend.sh` (no `--out`) is still how Emacs's cold start builds a
daemon before any daemon exists, and how a developer builds in place; it
deploys nothing.

**The store-then-sidecar ordering is mandatory, and the daemon holds it.**
Restarting both at once makes the sidecar's cursor recovery fail against a
socket not yet listening; it then starts cold and silently re-reads every
watched transcript from offset zero (observed 2026-07-25, thousands of files
re-ingested).

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

## Realtest orchestration (owner rulings, 2026-09-11)

- **No subagent ever launches, evals into, kills, or drives ANY Emacs process
  on this machine (Emacs.app, `emacs` interactive, emacsclient, probe
  instances included); only the lead does, and only through
  `bin/realtest.sh` or a deliberate owner-facing action.**
  - Batch ERT suites (`bin/background.sh emacs -batch -Q -l ert -l lisp/test-*.el ...`) remain
    allowed because they open no frame and no server.
  - A realtest starts Emacs once per test with `open -g -a Emacs` only.
  - Reason (owner, 2026-09-11): an agent's repeated `Emacs -Q` probes stole
    the owner's focus over and over and launched the wrong Emacs.
- Realtests run against the real Emacs.app on the host: real keystrokes, no
  sandbox, no focus grab, no pictures.
  - A realtest is remediated only when the harvest of ALL logs in the run
    window is clean.
- A sweep is `bin/realtest.sh <numbers or names>`, and the script sequences it:
  one `go test` per realtest, with the world each one's precondition demands
  established between them (`e2e/REALTEST-SPEC.md`, "The entry point").
  - `AGENT_REPL_REALTEST_TAKEOVER=1` covers every editor quit in the run;
    stopping the daemon for realtest 3 needs `AGENT_REPL_REALTEST_STOP_DAEMON=1`
    on top of it, and without that consent realtest 3 is skipped.
  - Exit 78 means some realtests never ran; 0 means everything asked for ran
    and passed. Never read a skip as a pass.
  - A new realtest needs a row in the script's world table, and
    `bin/test-realtest.sh` fails until it has one.
- `docs/REALTEST-PLAN.md` sections 1 and 2 (startup, workspaces; tests 1
  through 8) are run and remediated by the lead autonomously.
  - Sections 3 onward are owner-rules-first: run, surface, owner rules,
    remediate, owner confirms.
- The lead asks the owner nothing unless truly blocked.
  - Every judgement call is recorded in `docs/REALTEST-JUDGEMENT-CALLS.md`.
- Model policy for dispatched fixes:
  - Sonnet: docs, ledgers, shell and harness edits, tests from a settled
    table, log-shape fixes, read-only investigations, single-site fixes
    with a stated cause.
  - Opus-medium: multi-file production logic.
  - The lead never edits files itself.
- Logging coverage only grows.
  - Fixes never demote or delete a warn/error site.
- Proto files are never edited by agents.
  - Contract questions are surfaced to the lead.
  - The proto's framing wins.
- Workspace id contract: the daemon's 16-hex id is the id on every record
  and sink across all runtimes.
  - A workspace whose directory no longer exists is closed automatically
    by the daemon.

## No warning or error anywhere is neglected

A realtest's harvest reads only its own run window. That is not the whole
truth: warnings and errors also land between runs (deploy restarts, boot
catch-up, the owner's own use, leftovers a run failed to clean), and the
owner sees them in the live Emacs. Every WARN/ERROR/non-record in ANY
agent-repl log source, and every `*Warnings*` entry the module raises, is a
finding the moment it exists, whether or not a test was running. Before a
sweep, the gap since the previous sweep is scanned and its findings recorded
in the run's manifest; a run leaves the owner's state exactly as it found
it, and a leftover it cannot clean fails the run by name. Never allowlist,
never dismiss, never let a popup stand unexplained.

## Zero warnings, zero errors, in every system's logs — standing order

The owner wants NO warnings and NO errors in the logs of ANY agent-repl
system: daemon, shim, store, sidecar, webapp, Emacs. Not "acceptable",
not "known", not "benign". A warning is remediated in exactly one of two
ways: FIX the condition that raised it, or, ONLY when it is genuinely
miscategorized (an expected, ordinary outcome that carries no defect),
re-level it to info or debug with the reasoning in the commit. Fixing is
the strong default; re-leveling is the exception that has to be argued.

"PRE-EXISTING" IS NOT A CATEGORY. Never describe a warning or error as
pre-existing, historical, inherited, out of scope, or unrelated to the
change at hand. If it is in a log, it is a defect that needs fixing now,
whoever introduced it and whenever. Sweeping all logs for warnings and
errors and remediating every one of them is ordinary work, not an
extra.

## The webapp's integration suite is a gate, not an option

Every change under webapp/ runs `npm run test:integration` beside
`typecheck`, `lint` and `npm test` before it is reported. On 2026-09-13
three topbar landings ran only the unit suite and left the integration
suite red with 25 failures that a later agent then called "pre-existing".
Neither the omission nor the label is acceptable.
