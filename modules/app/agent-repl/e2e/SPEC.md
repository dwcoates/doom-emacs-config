# Cross-system e2e suite — specification

Scope of this document: what to BUILD, not what to run. No test in this
suite has been executed to produce this spec; every fact below comes from
reading the contract docs, the proto changes, the two audit reports, the
already-landed harnesses (`daemon/integration/harness`,
`agent-shim/claude/shim-sidecar/integration`), and the fake-SDK source. Where
the contract is silent or the audit reports disagree with what the code
looks like it does, that goes in section F, not into an assumption here.

---

## A. Module placement decision

**Decision: `modules/app/agent-repl/e2e` gets its OWN `go.mod`
(`module agentrepl/e2e`), importing `claude-repld/integration/harness` by
`replace`.** Not a package inside `daemon/`.

### Why

The daemon module (`daemon/go.mod`, `module claude-repld`) and the store
module (`agent-shim/shim-store/go.mod`, `module agentrepl/shim-store`) are
the only two Go modules under `modules/app/agent-repl` today; the sidecar
(`agent-shim/claude/shim-sidecar/go.mod`, `module
agentrepl/shim-claude-sidecar`) is a third. `daemon/integration/harness` is
`package harness`, importable as `claude-repld/integration/harness` — it
lives inside the `claude-repld` module but NOT under `internal/`, so nothing
about Go's visibility rules stops another module from importing it.

Putting the e2e suite as a package under `daemon/` (the way the deleted
`daemon/e2e` was, before the cutover: `git show 23cc6a672^:modules/app/agent-repl/daemon/e2e/e2e_test.go`)
would make every e2e test file literally live in the daemon's own module
tree. That directly conflicts with `feedback_proto_foundation_protos_only` /
the fanout discipline this overhaul runs under: the daemon lead's tree is
the daemon lead's, and cross-system test authorship is not a daemon-tree
change. It also conflicts with the task's own instruction that the e2e
directory is `modules/app/agent-repl/e2e`, a sibling of `daemon/`, not a
subdirectory of it.

A sibling module importing an exported, non-`internal` package from another
module by `replace` is the normal Go pattern for this (it is exactly how
`agent-shim/shim-store` and `agent-shim/claude/shim-sidecar` already import
`agentrepl/proto` and `agentrepl/logging` from sibling directories). The one
wrinkle: Go replace directives are NOT transitive. `daemon/go.mod` replaces
`agentrepl/proto => ../proto/gen/go` and `agentrepl/logging =>
../agent-shim/logging/go`, but those replaces apply only when `claude-repld`
is the main module. When `agentrepl/e2e` is the main module and it imports
`claude-repld/integration/harness`, which itself imports
`agentrepl/proto/...` and `agentrepl/logging`, the e2e module's own `go.mod`
must repeat those two replaces itself, or the build fails to resolve
`agentrepl/proto`/`agentrepl/logging` at all (there is no such module
published anywhere — they only exist as local paths).

### `modules/app/agent-repl/e2e/go.mod`

```
module agentrepl/e2e

go 1.23.0

require (
	claude-repld v0.0.0-00010101000000-000000000000
	agentrepl/proto v0.0.0
	agentrepl/logging v0.0.0
	connectrpc.com/connect v1.17.0
	google.golang.org/protobuf v1.36.11
	golang.org/x/net v0.43.0
)

replace claude-repld => ../daemon

replace agentrepl/proto => ../proto/gen/go

replace agentrepl/logging => ../agent-shim/logging/go
```

The store module (`agent-shim/shim-store`) and the sidecar module
(`agent-shim/claude/shim-sidecar`) are NOT Go dependencies of `agentrepl/e2e`
— the suite never imports their packages. It builds their binaries with
`exec.Command("go", "build", ...)` run with `cmd.Dir` set to each module's
own directory, exactly as the deleted `daemon/e2e`'s `buildShimStore` did and
as `shim-sidecar/integration/helpers_test.go` does for the store today. A
child `go build` inside a different module directory needs no `replace` in
the parent — it resolves its own `go.mod` on its own.

### The one daemon-tree change: an additive harness seam

`daemon/integration/harness` was built to run a real daemon against FAKES of
every neighbor (its own package doc, verbatim: "runs a REAL claude-repld
process against FAKES of every neighbor"). Two things in it are hardwired to
those fakes in a way the e2e suite cannot reuse as-is. Both are additive:
zero-value behavior is byte-for-byte what every existing `daemon/integration`
test already gets, so no existing test's behavior changes.

**Seam 1 — `harness.MainAt(m *testing.M, moduleRoot string) int`, new exported func in `daemon/integration/harness/harness.go`.**

`harness.Main` builds `claude-repld`, the fake shim, and the fake `git`
by locating the daemon module root via `moduleRoot()`, which walks UP from
`os.Getwd()` looking for the nearest `go.mod`. That walk is safe only when
the calling test binary's module IS `claude-repld` (`daemon/integration`'s
own suites). Called from `agentrepl/e2e`'s `TestMain`, `os.Getwd()` is
somewhere under `modules/app/agent-repl/e2e`, which has its OWN `go.mod` —
the walk stops there and `go build ./cmd/claude-repld` runs in the wrong
module and fails.

Fix by refactoring `Main` into a thin wrapper over a new exported entry
point that takes the module root explicitly:

```go
// MainAt is Main, except the daemon module root is given explicitly
// instead of discovered by walking up from the working directory. A
// suite outside the claude-repld module (agentrepl/e2e) cannot use the
// cwd-walk — its own go.mod would be found first — so it resolves its
// own relative path to daemon/ and calls this instead of Main.
func MainAt(m *testing.M, moduleRoot string) int {
	// ...exact body Main has today, minus the moduleRoot() call...
}

func Main(m *testing.M) int {
	module, err := moduleRoot()
	if err != nil {
		fmt.Fprintln(os.Stderr, "harness:", err)
		return 1
	}
	return MainAt(m, module)
}
```

`Main`'s behavior is unchanged (same two lines of error handling, same
delegate). `daemon/integration`'s own `TestMain` (`func TestMain(m
*testing.M) { os.Exit(harness.Main(m)) }`) is untouched. `agentrepl/e2e`'s
`TestMain` computes the daemon module directory relative to its own source
file (the same pattern the deleted suite's `repoRoot()` used:
`filepath.Abs(filepath.Join("..", "daemon"))` from a file at
`modules/app/agent-repl/e2e/...`) and calls `harness.MainAt(m, thatPath)`.

**Seam 2 — three new `Opts` fields, in the same file, `daemon/integration/harness/daemon.go`.**

`StartDaemon` today hardcodes three things to the fakes:
`"--node", FakeShimBinary(t)` (the built fake-shim binary),
`"--shim-main", mainJS` (a one-line placeholder file it writes itself), and
`d.StoreSocket` (a fresh temp path nothing ever listens on, always minted
internally, never settable).

```go
// Opts gains:

	// ShimNode overrides --node (default: the built fake shim). The e2e
	// suite passes a real `node` binary here so the daemon spawns the
	// real TypeScript shim instead of the fake-shim Go stand-in.
	ShimNode string
	// ShimMain overrides --shim-main (default: a one-line placeholder
	// module). The e2e suite passes the real shim bundle, built from
	// source with `--fake` support baked in (see B).
	ShimMain string
	// StoreSocket overrides the store socket path threaded through
	// --store-socket to every shim this daemon spawns (default: a fresh
	// path nothing listens on, as today). The e2e suite passes the
	// socket of a REAL running shim-store, so shims spawned by this
	// daemon persist and read real events.
	StoreSocket string
```

`StartDaemon` uses each override when non-empty and falls back to today's
exact default when empty:

```go
node := opts.ShimNode
if node == "" {
	node = FakeShimBinary(t)
}
shimMain := mainJS
if opts.ShimMain != "" {
	shimMain = opts.ShimMain
}
// d.StoreSocket set from opts.StoreSocket if non-empty, else the existing
// filepath.Join(sockRoot, "store.sock") mint, BEFORE the args slice and
// AGENT_REPL_STORE_SOCKET env line are built (both already read d.StoreSocket).
```

Nothing else changes: `--fake` still gets passed by default (`opts.NoFake`
false, unchanged), `AGENT_REPL_CLAUDE_BIN` still points at the fake `claude`
(the e2e suite never exercises the login pty or the classifier — those stay
out of scope, see B), the scripted fake `git` STAYS on `PATH` exactly as in
`daemon/integration` (ruling 4 REVERSED: this suite mocks every external
dependency, git included), and
`AGENT_REPL_FORBID_VENDOR_CALLS=1` is already set unconditionally. `--fake`
on the REAL shim selects its `src/fake` mocked vendor by prompt-text
scenario name (`fake/registry.ts`) — this is the intended, documented way to
drive the real shim without touching the vendor, not a harness invention.

**Seam 3 — WITHDRAWN (`SkipFakeGit`).** An earlier ruling had this suite use
real git; the USER REVERSED that. Every external dependency is mocked here,
git included, so the scripted fake git stays installed exactly as
`daemon/integration` installs it. `Opts.SkipFakeGit` must be REMOVED from the
harness; if removing it is more churn than leaving it, it may remain but
NOTHING may ever set it. `NewRealRepo` / `RealRepo` / any hermetic-git
environment are removed from the e2e harness.

This is the ONLY daemon-tree change this spec asks for. It is additive,
covered by the "does not change `daemon/integration`'s own behavior" rule
because every existing call site leaves all three new fields at their zero
value.

---

## B. Harness design

### Building (once per `go test` run, shared across every test)

| Binary | Built by | Source | Skip condition |
|---|---|---|---|---|
| `claude-repld` | `harness.MainAt` (via seam 1) | `daemon/cmd/claude-repld` | `go` on PATH (else the whole run fails hard — the daemon is not optional) |
| fake `git` | `harness.MainAt` | `daemon/integration/fakegit/git` | same |
| real shim bundle | new `e2e` build helper, modeled on the deleted `daemon/e2e`'s `buildShim` | `agent-shim/claude/shim`, `node build.mjs` | `node` on PATH; `agent-shim/claude/shim/node_modules` present — **loud `requireDependency` FAILURE, never an implicit `npm ci`** (a skip only under `AGENT_REPL_E2E_ALLOW_MISSING_DEPS=1`; see `precondition_test.go`) |
| `shim-store` | new `e2e` build helper, modeled on the deleted `daemon/e2e`'s `buildShimStore` and on `shim-sidecar/integration/helpers_test.go`'s `storeBinary` | `agent-shim/shim-store` (`go build -o <bin> .`) | `go` on PATH (already required) |
| `shim-sidecar` | same pattern | `agent-shim/claude/shim-sidecar` (`go build -o <bin> .`) | same |

The shim bundle is built from source on every run, never from a
gitignored, hand-rebuilt `dist/`, for the same reason the deleted suite's
`buildShim` gave: a stale checked-out bundle would let the suite silently
stop covering the source it exists to cover. The shim's build identity is
NOT baked into the bundle: the daemon hashes the bundle it spawns
(`--shim-main`) and hands that content hash to each shim as
`SHIM_BUILD_SHA` in its spawn environment, so the freshly built bundle is its
own identity and nothing has to be pinned or passed for it (section B, "A
deploy judges what really runs").

**Layout invariant — the staged bundle keeps production's siblings.** The
shim build deliberately leaves `@anthropic-ai/claude-agent-sdk` EXTERNAL
(`build.mjs`), so the bundle resolves it at RUNTIME by walking up from its own
URL — `build-identity.ts`'s `sdkPackageDir()` does that on the `main` path,
before anything else runs. `bin/build-frontend.sh` satisfies that by writing
`shim/dist/main.js` beside `shim/node_modules` and `shim/package.json`; the
staged e2e bundle MUST reproduce the same siblings, or every shim dies in its
first millisecond with `Cannot find module '@anthropic-ai/claude-agent-sdk'`
and the whole suite reads as a wall of timeouts rather than one build error.
`buildShimBundle` therefore stages a `node_modules` symlink (to the repo
shim's real one, whose presence `requireShimBundle` has already made a loud
skip) and a copy of `package.json` at BOTH levels the bundle can reach — its
own directory and the one above it, the latter because `src/main.ts` reads
its version through a literal `require("../package.json")`. The build then
self-tests the invariant by stat-ing
`<outDir>/node_modules/@anthropic-ai/claude-agent-sdk/package.json` and fails
the build, loudly and by name, if it is not there.

All five binaries are built into one `t.TempDir()`-rooted directory per
`TestMain`/`m.Run()` invocation and referenced by every test via exported
accessors, exactly as `harness.DaemonBinary(t)` / `harness.FakeShimBinary(t)`
work today.

### Per test

One daemon + one store + one sidecar + the real shim (spawned per-session BY
the daemon, never directly by the test):

- `store` — `startRealStoreAt`-shaped helper (same idea as
  `shim-sidecar/integration/helpers_test.go`'s `startRealStoreAt`): a fresh
  temp socket + a fresh temp `.db` file per test, `--socket`, `--db`,
  `--log`. Exposes a `Stop()` / `StartSameDB()` pair so a test can provoke a
  real outage (see the degraded-state control below) and so the bounce
  helper (also below) can restart the SAME durable database.
- `sidecar` — spawned with `--store-socket <the store's socket>`,
  `--config-roots <the daemon's per-account CLAUDE_CONFIG_DIR roots,
  comma-joined>`, `--spool-root <a fresh temp dir>`, `--log <a fresh temp
  file>`, plus the four interval/window flags below. `AGENT_REPL_STORE_SOCKET`
  and `AGENT_REPL_FORBID_VENDOR_CALLS=1` in its environment (mirrors
  `startSidecar` in `shim-sidecar/integration/helpers_test.go` verbatim).
- `daemon` — `harness.StartDaemon(t, harness.Opts{ShimNode: node, ShimMain:
  shimBundle, StoreSocket: store.Socket, ...})`. `d.DefaultConfigDir` / `d.MultiRepoConfigDir` — ALREADY
  produced by the existing harness's `NewConfigRoot` — ARE the per-account
  `CLAUDE_CONFIG_DIR` roots: `main.ts` requires `CLAUDE_CONFIG_DIR` in the
  spawned shim's environment (`shim.md` process-level obligations; `main.ts`:
  "EVERY REQUIRED VARIABLE IS A REFUSAL... `CLAUDE_CONFIG_DIR` ... names the
  account this session runs as"), and the daemon is the one that sets it per
  spawn from its own `--default-config-dir`/`--multi-repo-config-dir` roots.
  No new harness plumbing is needed for this half — it is already there. The
  sidecar's `--config-roots` is simply told to watch the SAME two directories.
- Real shim — never started directly by a test. It comes up when the daemon
  spawns a session against a workspace, using `ShimNode`/`ShimMain`.
  `--fake` on it selects the mocked vendor by the submitted prompt's `!name`
  prefix (`fake/registry.ts`); a plain prompt with no `!name` prefix falls
  through to the default prose scenario, unchanged.

Per-test isolation: fresh `t.TempDir()` state root, fresh sockets (via the
existing harness's `shortTempDir`/`shortSocketPath` pattern — UDS path
budget is 103 bytes and this suite's test names are long), fresh spool
root, fresh `.claude.json` account roots (routing through
`d.MultiRepoRoot` reuses the existing harness field verbatim for the
multi-repo tests in this suite).

`AGENT_REPL_FORBID_VENDOR_CALLS=1` in EVERY spawned process's environment —
daemon, sidecar, store, and (inherited) shim — with no exception. This is
already how `daemon/integration` behaves; nothing new.

### Running the suite — parallelism, and every bound it rests on

**The invocation, from `modules/app/agent-repl/e2e`:**

```
go test -count=1 -timeout 45m -parallel 8 ./...
```

`-parallel 8` is not decoration. **Every test in this package declares
`t.Parallel()`**, so without a bound `go test` would use `GOMAXPROCS` and
stand that many full worlds — a daemon, a store, a sidecar and a node shim
each — at once.

#### Why parallel is safe here

Isolation was already structural before the declaration was added; nothing
had to be made safe for it:

| Shared thing | Why concurrent worlds do not collide |
|---|---|
| daemon address | an ephemeral loopback port the daemon picks and publishes into its own state root's `daemon.addr` |
| store socket | `shortSocketPath` — a random 4-byte suffix directly under `$TMPDIR` |
| shim sockets | under the world's own short state root (`shortStateRoot`, `os.MkdirTemp`) |
| state root, config roots, lock dir, prompts, webapp dist | per-daemon, minted by `harness.StartDaemon` under its own `t.TempDir()` |
| spool root | one `t.TempDir()` per world, handed to the shims AND the sidecar (`assertOneSpoolRoot`) |
| fake git state | a per-daemon state file, named to each daemon through `fakegit.EnvStateFile` |
| built binaries | `sync.Once` per binary, one shared temp bin dir per `go test` process; `Once` blocks the racers rather than duplicating the build |
| stray-reaping exemptions | a package-level map keyed by pid, mutex-guarded, entries removed at each owner's cleanup |
| process environment | nothing in this package calls `t.Setenv`, `os.Setenv` or `os.Chdir`; every lever travels through `Opts.ExtraEnv` into one daemon's own environment |

The Emacs-layer files are the one part of the suite that does not declare
`t.Parallel()`; they are owned separately and skip on a host with no sandbox
image.

#### Why 8, and not more

Measured on a 16-core host, back to back on the same tree:

| Setting | Wall | Verdicts |
|---|---|---|
| `-parallel 1` | 65.6s | 118 pass / 2 known fail / 47 skip |
| `-parallel 8` | 15.4s | identical |

Per-test wall time inflates with concurrency, because a world is four real
processes and every wait in this suite is bounded against an ordinary,
lightly-loaded machine (`DefaultTimeout`'s own sizing note). Past 8 that
inflation starts costing correctness rather than buying time: at
`-parallel 12` a run went 20.7s → 38.4s **and** lost a test to a bound. 8 is
the widest setting measured green.

#### The webapp layer gets a second, tighter bound

`-parallel` counts worlds, and a webapp-layer area is not a world: it is a
world PLUS a whole vitest child (its own node process, an esbuild transform
of the app's sources, a jsdom document), and there are ten areas. One number
cannot size two loads that far apart, so `wlMaxConcurrentAreas` (currently
**3**, `webapplayer_e2e_test.go`) caps how many areas run at once,
independently of `-parallel`.

- Left uncapped at `-parallel 8`, the feed-families area went 7.4s → 29.1s
  and four unrelated Go tests failed on bounds sized for an unloaded box.
- The slot is taken BEFORE the area builds its world and released at test
  cleanup. A world's context starts ticking at `harness.StartDaemon`, so an
  area that built its world and then queued would burn its own budget, and
  its four processes, while waiting in line.
- 3 was measured against 2 and 4: 2 → 31.2s, 3 → 20.7s (both at
  `-parallel 8`, on a box under other load), and 4 at `-parallel 12` lost a
  test. 3 is the fastest setting measured green.

#### Bounds this suite touched, and their measured basis

No wait bound was widened, and none was tightened below what was measured.

| Bound | Value | Basis |
|---|---|---|
| `wlMaxConcurrentAreas` | 3 | the measurement above: 2 → 31.2s, 3 → 20.7s, 4 → a lost test |
| `-parallel` | 8 | 1 → 65.6s, 8 → 15.4s, both green; 12 → 38.4s and a lost test |

The wall figures above were taken on a workstation shared with other running
suites, so they are noisy in absolute terms (the same serial run measured
65.6s idle and 157s under heavy neighbour load). Every comparison quoted here
is between settings measured back to back under the same conditions.

Every other bound (`DefaultTimeout`, `HandoverChainTimeout`,
`AdoptionChainTimeout`, `StoreOutageWindow`, `unownedSpoolWindow`,
`WebappLayerTimeout`, `coldGateChainTimeout`, `reapGrace`) is unchanged from
the sizing its own doc comment records.

#### Stability is proved, never assumed

A parallel run that is green once has proved nothing. The pass set must be
IDENTICAL across `go test -count=3 -parallel 8`; a test that moves between
runs is a defect to root-cause at its source, never a run to repeat. One such
defect was found and fixed this way: the refused-vendor-start area's health
fault lands asynchronously behind the rpc that causes it, so whether the
cleanup sweep saw it depended on machine load — the fix is that the area
states the fault its own arrangement causes (`refusals_e2e_test.go`'s
`rfVendorStartFaultWarnings`), not that the run is retried.

### Loud skips

Exactly the deleted `daemon/e2e`'s three: no `node` on `PATH` → skip; no
`agent-shim/claude/shim/node_modules` → skip naming the exact `npm ci`
command and directory; no `go` on `PATH` → skip (this one is unlikely to
ever fire since building `claude-repld` itself already needs `go`, so it is
really the same precondition, checked defensively at the shim-store/sidecar
build sites too). **The harness never runs `npm ci` or `go mod download`
itself.**

### Lifecycle

- stderr (and stdout, where the process does not separate them — the store
  and sidecar do via `--log`, matching their existing integration suites;
  the daemon writes to a `daemon.stderr.log` file exactly as
  `daemon/integration/harness/daemon.go` does today, never a pipe, for the
  documented reason: a piped `Wait` blocks on every inheriting child, which
  bites the moment a rollout/handover test chains a second process onto the
  same harness) is captured to a file for every one of: daemon, store,
  sidecar. The shim's stderr is whatever the daemon's own shim-spawn
  plumbing already captures (unchanged — this suite adds no new capture
  path for it).
- A process that exits before its test's `t.Cleanup` runs fails that test
  loudly (mirrors `daemon/integration/harness`'s existing exit tracking on
  `Daemon`, extended to `store` and `sidecar` structs new to this package).
- Deterministic teardown order, registered via `t.Cleanup` in START order so
  cleanup runs in REVERSE: shim (daemon-owned, dies with its session or the
  daemon) → daemon → sidecar → store. Tearing the store down before the
  sidecar/daemon would race a live writer against a closed socket and turn
  an unrelated test failure into a spurious one.

### Failure artifacts

Every real log sink the daemon and its shims write — the restart-scoped
`daemon.run.log` and each per-workspace `daemon`/`shim`/`webapp` sink — is a
file under the state root's `logs/` directory (`daemon/internal/dlog/sink.go`
`createTarget`); the `<workspace>/.claude/emacs/<sink>.log` paths are symlinks
into it. The state root is a per-test temp dir the testing package deletes on
the way out, so a failure used to leave nothing behind and the next diagnosis
had to re-run the test with instrumentation added.

`preserveLogsOnFailure` (registered in `NewWorld` immediately after the daemon
starts, so LIFO cleanup runs it AFTER the daemon and its shims have exited and
flushed) collects that one directory, and only when `t.Failed()`:

- `AGENT_REPL_E2E_ARTIFACTS=<dir>` set → every sink is copied whole into
  `<dir>/<test name with '/' flattened>/`, and the destination is named in the
  test output.
- unset (the default) → each sink's last 64 KiB, cut forward to a record
  boundary, goes into `t.Log` output. Bounded, but never nothing.

The store's and the sidecar's own log files are routed into that same
`logs/` directory by `NewWorld` (which mints the state root itself, before the
store starts, and passes it to `harness.StartDaemon` as `Opts.StateDir`).
They previously landed in anonymous `t.TempDir()`s, so a failing run
preserved the daemon and shim sinks but not the store or sidecar log — the
two processes whose misbehavior a store-outage or spool failure most needs.
One directory now holds everything, and the single sweep collects it.

A passing test writes no artifacts and logs nothing.

### Store stop/restart control (ruling 2 — real degraded-state outages)

The store helper exposes:

```go
func (s *e2eStore) Stop()               // SIGTERM, waits for exit
func (s *e2eStore) StartSameDB(t *testing.T) // relaunches on the SAME socket + db path
```

modeled directly on `shim-sidecar/integration/helpers_test.go`'s existing
`startRealStoreAt`/bounce pattern (already used there to prove sidecar
durability across a store bounce) and the deleted `daemon/e2e`'s
`shimStoreProc.launch`/bounce pattire. A degraded-state test calls `Stop()`
while a session is live, asserts the daemon renders the shim-reported
degraded-state fact (`AgentUpdate`/`SessionUpdate` shim-degraded arm — see
`daemon.md`'s "Failure classification" section: "upstream silence is stated
by the daemon as a fact (shim-degraded arms)"), then `StartSameDB()` and
asserts recovery. No fabricated `Event_DegradedState` is ever written by a
test — the whole point of ruling 2.

### Bounce-with-real-rows helper (ruling 3)

```go
func driveScenarioToCompletion(t *testing.T, d *Daemon, workspace, scenario string) TurnId
```

Submits a real prompt (`"!"+scenario`, or the scenario's documented prompt
form from `fake/registry.ts`, e.g. `"!compact [summary]"`) through the
daemon's real `SubmitPrompt`, waits (frame-driven, see Waits below) for the
turn's terminal frame, and returns once the sidecar has durably written the
resulting facts to the real store (waited for via the store's own read path
or a sidecar cursor-advance signal — never a sleep). A test that needs
pre-restart durable rows (the old `durableresync_e2e_test.go` /
`conversationpage_e2e_test.go` / `promptreceiptreplay_e2e_test.go` family)
calls this BEFORE simulating the daemon bounce, then restarts the daemon
against the same state root/store socket and asserts on replay. This
retires every `storedAssistantEvent`-seeded pre-restart fixture named in
`E2E-EVENT-INVENTORY.md`'s ruling 3.

### Frontends: none

No webapp, no WebSocket, no Emacs. Every test dials the daemon's Connect
API directly (`agentreplv1connect.AgentReplClient`, same client type
`harness.Daemon.client` already wraps) and asserts on watch-stream frames
(`WatchFeed`, `WatchAgentSession` equivalents, `WatchDaemonHolds`, footer/
topbar/sidebar resolver output) — the same surface the webapp itself would
consume, one layer lower.

### Waits

Event-driven only, gated on: a daemon watch-stream frame arrival, a
`LogRecord` in the daemon's, store's, or sidecar's own structured log (the
existing `AwaitLogRecord` pattern), or a store read returning the expected
row. **No `time.Sleep` for synchronization anywhere in this suite**, per the
project's own Go testing rule and per every one of these harnesses' own
stated discipline ("Nothing in the harness sleeps to synchronize").

Default bound: **5 seconds**, matching both `daemon/integration/harness`'s
`DefaultTimeout` (measured off a 443-test run: median 0.2s, observed max
1.7s, ~3x headroom) and the deleted `daemon/e2e`'s own independently-derived
`frameTimeout` (measured at ~0.65s per await dominated by one real node-shim
spawn plus a turn through a real store, "roughly an 8x margin"). Two
independently measured real-process baselines agreeing on 5s is strong
grounding for reusing it as-is rather than inventing a third number.

**That bound is ONE WAIT'S, never the whole run's.** `World.Ctx()` is the
`harness.Daemon`'s context, which is the run budget (`DefaultTimeout *
runBudgetWaits`, 30s). Handing it straight to a single rpc gives that call
whatever the run happened to have left — every test here boots a real daemon,
a store, a sidecar and a real Node shim before it asserts anything, so the
LAST call in a test would answer `deadline_exceeded` for time the earlier ones
spent. That is exactly how `TestHostRequestedStopLeavesNoProcessBehind` failed
~1 in 20 at ~5.09s on a stop measured at 9ms p50 / 15ms max over 104 runs. A
one-shot rpc whose latency is the subject takes a `DefaultTimeout` child of
`w.Ctx()`.

Per-site overrides get the SAME discipline `HandoverChainTimeout` uses in
`daemon/integration/harness`: a NAMED constant with a one-line reason,
never an ad hoc duration at the call site. This suite needs at minimum:

- `HibernationChainTimeout` (idle-cutoff tests: the daemon's own
  `IdleCutoffMS` compresses the cutoff, but the test still chains a real
  shim spawn + a real hibernation decision + a real revival before its
  budget starts, structurally two lifecycle events like the existing
  `HandoverChainTimeout` — reuse that constant directly if the shape
  matches, rather than mint a same-value twin).
- `AdoptionChainTimeout` (restart/adoption tests chain a SECOND real
  `claude-repld` boot onto the same context, exactly the case
  `HandoverChainTimeout`'s doc comment describes — reuse it).
- A store-outage window bound for degraded-state tests: the sidecar's own
  integration suite already picked `UnownedSpoolWindow: 200ms` as "a subject
  that is not ABOUT the hold would simply time out inside it, subjects that
  ARE about the hold set their own window" — the degraded-state test IS
  about the hold/outage, so it gets its own explicit bound derived the same
  way, not the 5s default.

### Every external dependency is MOCKED (USER RULING, reverses ruling 4)

Only the systems WE OWN run for real in this suite: `claude-repld`, the
shim, `shim-store`, `shim-sidecar`. Everything else is mocked, here exactly
as everywhere else in the repo:

- **git** is the SCRIPTED FAKE GIT the daemon harness already installs. No
  real `git`, no real repositories, no hermetic git environment, no
  `NewRealRepo`. `Opts.SkipFakeGit` is never set.
- **the vendor** is the fake SDK. `AGENT_REPL_FORBID_VENDOR_CALLS=1`.
- **the network** is not used at all. `web-fetch` / `web-search` goldens are
  driven as fake-SDK scenarios like every other scenario.
- no real vendor binaries anywhere.

The merge-queue area asserts merge behavior against scripted fake-git
FIXTURES, exactly the way `daemon/integration`'s own merge tests do.

**The git-invocation surface is NOT this suite's.** The git facts —
two-parent `--no-ff` merge commit, landed range, conflicted index and
MERGE_HEAD, revert, worktree prune, porcelain markers, `GIT_DIR` precedence,
version compatibility — are covered by the GIT-CLIENT LEAF's own tests. No
e2e test asserts them.

**TESTS MUST BE FAST.** Anything that would take real wall-clock time
because a real external tool executes is a SPEC ERROR, not a tradeoff.

### Harness helpers landed by overhaul/integration (a091a9b98)

`overhaul/integration` added two helpers to `daemon/integration/harness`.
They are merged into this branch; writers must know which one applies here.

- **`(*Daemon) DisplacedTurnCount() int`** (`wsmdb.go`) — USABLE AS-IS. It
  reads the daemon's own database (`SELECT count(*) FROM turns WHERE
  displaced = 1`) through `WithDB`, so it is independent of git entirely. The
  merge-queue area (§C, displaced turns) SHOULD use it rather than
  re-deriving the count.
- **`(*Repo) SetDirty(worktreeDir string, dirty bool)`** (`repo.go`) —
  USABLE, and the CORRECT way to make a worktree dirty here. It mutates the
  scripted `fakegit.State`, which is precisely what this suite runs against
  now that the real-git ruling is reversed. (An earlier revision of this
  document marked it NOT USABLE; that applied only under the withdrawn
  real-git ruling.)

### Two harness invariants, self-checked before any test runs

Both are silent when broken — neither fails as itself, both fail as a whole
suite of timeouts — so each is checked or resolved ONCE, up front, in one
place.

- **A deploy judges what really runs, and installs nowhere real.** The
  daemon owns deploys (the `Deploy` rpc, and the one deploy every landing on
  its own checkout runs): it builds into staging, judges each running
  process's build against the fresh one BY CONTENT HASH, and installs into
  its checkout (`AGENT_REPL_CHECKOUT`). A shim's build is the content hash of
  the bundle it was spawned from (`--shim-main`), so the real bundle the
  suite builds is its own identity and nothing has to be pinned for it. Three
  things keep every deploy a world runs honest:
  - **The checkout is the harness's.** A Go world names no checkout, so
    `harness.StartDaemon`'s own throwaway one stands (`checkoutEnv` in
    `main_test.go`): a deploy's install lands there, never over this
    worktree's `daemon/bin`, and its fresh elisp build is the one every
    harness `WatchDaemon` stream reports. Only the Emacs layer names the
    module root, because its real Emacs loads that elisp and the sandbox's
    working copy is a throwaway.
  - **The build is fake and stages what runs.** `AGENT_REPL_DEPLOY_BUILDER`
    names the harness's `DeployBuilder`, which stages the running bundle, the
    served dist and the harness daemon binary (`harness.DeployCurrent`), so a
    deploy decides everything up to date unless a test stages another build
    (`StageDeployBuild(harness.DeployStaleDaemon)` for a handover).
    `AGENT_REPL_LAUNCHCTL` is a recording fake, so no deploy reaches launchd.
  - **The real services report where the deploy reads.** The store and the
    sidecar write their own build reports into the daemon's lock dir
    (`harness.LockDirFor`), and the fake build stages copies of those very
    binaries (`Opts.ServiceBinaries`, `World.ServiceBinaries` for a daemon
    started by hand over the same state root), so a deploy that should touch
    no service restarts none. A mismatch looks like: a deploy answering
    `restarted` for the store, a fake launchctl invocation, and a store
    restart that waits out its window on `store.sock`.
- **One string per config root.** The sidecar records cursors under the path
  it walked, which is symlink-resolved (`/tmp/... -> /private/tmp/...` on
  macOS), while every cursor poll here prefix-matches a project directory
  derived from the daemon's account roots. `NewWorld` resolves
  `DefaultConfigDir` and `MultiRepoConfigDir` (`resolveConfigRoots`) before
  the roots are handed to the sidecar or read by any test, so the recorded
  path and the polled prefix are the same string. Unresolved, no cursor is
  ever seen to advance and `driveScenarioToCompletion` times out on turns
  that in fact completed and were durably written.

Both helpers are unit-tested in `harness_selftest_test.go`.

### Grep gate

A `TestMain`-time check (before `m.Run()`) that fails the whole run if any
`_test.go` file under `modules/app/agent-repl/e2e` contains:

- a call naming a store write verb directly (`WriteBatch`, or any
  `storev1connect` client method whose name is not a `Watch`/`Get`/`Open`
  read) — this suite proves behavior by driving the shim and sidecar, never
  by writing store facts by hand, which is the exact defect
  `E2E-SCENARIO-COVERAGE.md` found in 100% of the deleted suite's rows.
- a literal path write under `os.MkdirAll`/`os.WriteFile`/`os.Create` whose
  path string contains `"CLAUDE_CONFIG_DIR"`, `"/projects/"`, or the
  per-test config-root/spool-root variable names this harness exports — the
  suite's OWN vendor-transcript writes must all come from `--fake` running
  inside the real shim (`src/fake/vendor-files.ts`), never from a test
  hand-authoring a `.jsonl` transcript, which is the exact defect ruling 5
  retired.

Implemented as a source-grep over the package's own `.go` files (a small
`go/ast` or plain regex scan run from `TestMain`), not a build-tag trick —
it must fail loudly and specifically, naming the offending file:line, the
way `AGENTS.md`-style "structural gate" checks do elsewhere in this repo.

---

## C. Test list, by area

Each entry: **name** — scenario(s) driven — contract citation — frames
asserted. "Frames" names the `agentreplv1`/`frontendv1` shapes the daemon
renders on `WatchFeed`/`WatchAgentSession`/`WatchDaemonHolds`/footer-topbar
streams, per `PROTO-CHANGES.md`'s landing ledger and the per-system contract
docs. File-per-area grouping is given in section E.

### Turn lifecycle (`turnlifecycle_e2e_test.go`)

1. **TurnStartToCompletion** — `prose-streamed` — `daemon.md` §"Package map" /
   `shim.md` §"What the shim's streams owe consumers" — `FeedResponse`
   streaming deltas → turn terminal `success.completed`.
2. **TurnStopMaxTurns** — `turn-stop-max-turns` — `shim.md` failure-arm table
   (§"Where the sixteen failure arms actually live") — turn terminal
   `failure.maxTurns`.
3. **TurnStopMaxBudgetUsd** — `turn-stop-max-budget-usd` — same — terminal
   `failure.budgetExhausted`.
4. **TurnStopMaxStructuredOutputRetries** — `turn-stop-max-structured-output-retries`
   — same (DECLARED-ONLY per the shim manifest: capture ended
   `success.completed`, so this test asserts the SHAPE reaches the daemon
   unmodified, not a failure terminal — cite the manifest's own
   DECLARED-ONLY mark so a future reader does not "fix" the assertion).
5. **TurnStopErrorDuringExecution** — `turn-stop-error-during-execution` —
   same (also DECLARED-ONLY: capture ended `success.interrupted`).
6. **TurnStopHookStop** — `turn-stop-hook-stop` — same (DECLARED-ONLY:
   capture ended `success.completed`, residue
   `vendor_specific/system/notification`).
7. **ModelChanged** — `model-changed` — `daemon.md`/`shim.md` model-plumbing
   sections — `SessionUpdate`/model-catalog frame reflecting the new model.
8. **FastMode** — `fast-mode` — `shim.md` §"What the shim IS" — turn terminal
   `success.completed`, fast-mode marker on the turn's record.
9. **MaxTokens** — `max-tokens` — same family as #2-3 — terminal shape for a
   token-ceiling stop.
10. **ProseStreamedFourBlockShape** — `prose-streamed` — pins the specific
    four-block (withheld-thinking + visible-thinking + two text blocks)
    shape the coverage report calls out as never actually reproduced by the
    deleted suite's generic `!md` turns.

### Permissions (`permission_e2e_test.go`)

11. **PermissionAllowOnce** — `permission-allow-once` — `shim.md` §"The
    permission gate" — `AgentPermission.start` → `success` (allow-once
    echo token) → the gated unit's ordinary activity frames.
12. **PermissionAllowStanding** — `permission-allow-standing` — same —
    `success` with the standing echo token, and `SessionUpdate`'s
    authoritative permission-mode restatement (`set_mode`).
13. **PermissionDeniedByUser** — `permission-denied-by-user` — same —
    `AgentPermission` settles denied; the gated tool unit settles
    `failure` with content UNSET (the project-lead final ruling, 2026-09-01,
    cited verbatim in `shim.md`) joined by the shared `AgentActivityId`; the
    turn does NOT end (deny-and-continue, `daemon.md` §"Queue, holds, leases
    — contract facts").
14. **PermissionDeniedByPolicy** — `permission-denied-by-policy` — same —
    `denied.by_policy` (no open ask; the vendor's own system
    permission-denied message).
15. **PermissionUndecidableParked** — `permission-undecidable-parked` —
    `daemon.md` merge/hold family + `shim.md` gate section — the ask stays
    open with no terminal; daemon-side this is the parked-prompt flow.
16. **HeldTurnGate** — `held-turn-gate` — `daemon.md` §"Queue, holds, leases"
    — a turn HELD while another runs on the same agent; `WatchDaemonHolds`
    shows the `HeldPrompt`; `UpdateHeldPrompt` delivers it once the gate
    clears.
17. **PermissionModeChangedMidSession** — `permission-mode-changed` —
    `shim.md` gate section, `set_mode` — `SessionUpdate` reflects the new
    mode without a client-initiated change.

### Interrupts (`interrupt_e2e_test.go`)

18. **InterruptAfterTextDelta** — `interrupt` — `daemon.md`/`shim.md`
    interrupt handling — turn terminal `success.interrupted`, streamed
    prose truncated exactly after the observed `text_delta`.
19. **BashInterruptedByTimeout** — `bash-interrupted-by-timeout` — same
    family, Bash-tool-own timeout (not a vendor-level interrupt) — the
    tool unit's own terminal, distinct from #18's turn-level interrupt.

### /clear + identity rotation (`identityrotation_e2e_test.go`)

20. **ClearRotatesIdentity** — `identity-rotation-clear` — `daemon.md`
    §"Identity, as the daemon lives it" — `SessionIdentityRotated` +
    `AgentUpdate.context_cut(ContextCleared)`; three turn terminals per the
    manifest row.
21. **SecondRotateUnderRotatedIdentity** — `identity-rotation-clear` driven
    TWICE on the same session — `E2E-EVENT-INVENTORY.md` remediation item 3
    ("For the rotation-family tests that need a clear 'under the rotated
    identity,' drive a SECOND `!rotate`... instead of fabricating") — proves
    the second rotation composes on the daemon's already-rotated state, not
    a fabricated intermediate.

### Compaction + rotation (`compaction_e2e_test.go`)

22. **CompactionDirected** — `compaction-directed` — `daemon.md`/`sidecar.md`
    conversion sections — `status{compacting}` → `compact_boundary`
    (trigger/tokens/duration) → `status{compact_result}`; three turn
    terminals per the manifest row.
23. **CompactionDirectedWithSummaryOverride** — `!compact [summary]` (the
    landed summary-override option, `session.ts`) — `PROTO-CHANGES.md`
    landing ledger has no field for this (it is a shim-internal
    parameterization, not a wire change) — `ContextCompacted.Summary`
    carries the test's own distinctive string instead of the fixed
    `"Conversation compacted"`.
24. **CompactionAuto** — `!compact-auto` — same family — the auto-triggered
    variant of #22's boundary shape.
25. **CompactionFailed** — `!compact-failed` — same family — a compaction
    that fails; `HibernateError.kind.compaction_failed{error}` if triggered
    via a hibernate path, or the equivalent compaction-failure terminal on
    the plain turn path (whichever the scenario actually drives — resolve by
    reading `session.ts`'s `COMPACT_FAILED` scenario body when writing this
    test, not by guessing here).
26. **ContextBudgetWarning** — `!context-budget-warning` — `PROTO-CHANGES.md`
    Landing 4 (`AgentUpdate.context_budget_warning = 7`) — marked
    UNGROUNDED/INVENTED in the shim's own manifest, ruling 6 ("provisionally
    settled... pending a grounding capture... LANDING 5 IS NOT OVERTURNED").
    This test asserts the WIRE SHAPE lands on `AgentUpdate`, not that it
    matches a real vendor recording — say so in the test's own header
    comment, mirroring the manifest's own caveat.

### Subagents — sync / detached / nested (`subagents_e2e_test.go`)

27. **SubagentSyncNestedActivity** — `subagent-sync-nested-activity` —
    `daemon.md`/`conversation.v1` `AgentId` minting rule (`PROTO-CHANGES.md`
    Landing 3: "subagent = the spawning call's tool_use_id") — nested
    `AgentFrame`s attributed correctly by id, routed to the subagent's own
    sub-feed.
28. **SubagentDetached** — `subagent-detached` — `daemon.md` §"Resolvers and
    push duties" (merge bubble is the SAME plumbing as subagent bubbles) —
    async-launch ack, eventual completion frame on the detached work's own
    feed, not the top-level feed.
29. **SubagentDetachedUtteranceStaysOffTopLevel** — `!subagent-detached-utterance`
    (landed addition, `subagents.ts`) — `E2E-EVENT-INVENTORY.md` remediation
    item 8 — one mid-flight sidechain assistant line with NO completion;
    asserts the router keeps it out of the top-level feed while the unit is
    still live.
30. **NestedSubagentHistoricalUsage** — `!usage-historical` (landed addition)
    — `E2E-EVENT-INVENTORY.md` remediation item 9 — file-plane-only usage
    record (`cache_creation` 5m/1h split, `server_tool_use` counts,
    `service_tier`, `speed`, `inference_geo`) attributed to a nested
    (spawnDepth 2) subagent id; UNGROUNDED per the shim manifest — say so in
    the test header.

### Detached bash (`detachedbash_e2e_test.go`)

31. **BashDetachedStartAndComplete** — `bash-detached` — `daemon.md` detached
    work / `AgentBashOutput` — backgrounding, incremental spool growth,
    completion notification on the detached unit's own frame.
32. **BashDetachExplicitPoll** — `!bash-detach-poll` (landed addition,
    `shell.ts`) — `E2E-EVENT-INVENTORY.md` remediation item 6 — an explicit
    poll tool_use/tool_result pair reporting RUNNING-with-growing-output,
    then a terminal exit code/status; note in the test header that this
    shape is a fake-SDK invention (no vendor capture ever calls the poll
    tool — see the shim manifest's own note) mirrored from the deleted
    suite's `bashTaskOutcome`, not a golden-grounded assertion.
33. **REMOVED — owner ruling, 2026-09-04** (`docs/overhaul/PROTO-CHANGES.md`,
    "Landing 8"): detach of in-flight foreground work (formerly named for
    the Ctrl-B key chord; no client verb today) is dropped from the
    inventory and may be added back later. The golden this item drove,
    `ctrl-b-detach-of-foreground-work`, stays reachable-only with no e2e
    test; see `testdata/captures/MANIFEST.md`.
34. **REMOVED — project-lead ruling, 2026-09-04**: `TestVendorBackgroundedSubagent`
    was a perpetual skip (its drivable half ran, then it always skipped —
    `DetachForeground` has no `agentrepl.v1` caller and no fake-SDK scenario
    backgrounds a subagent) and is deleted rather than kept as noise. The
    golden this item drove, `ctrl-b-detach-of-foreground-subagent`, stays
    reachable-only with no e2e test. `DetachForeground`'s confirm/refusal
    arms (`unknownUnit`, `alreadyConcluded`, `unsupported`, success) are
    covered at the unit level only, generically over `AgentActivityId`
    (not subagent-specific): `agent-shim/claude/shim/test/engine/turn.test.ts`
    (`describe("DetachForeground", ...)` and
    `describe("DetachForeground on a live foreground unit", ...)`) and
    `agent-shim/claude/shim/test/engine/session.test.ts`. See
    SCENARIO-MATRIX.md's "Arms without an e2e lever" section.
35. **BashForegroundCompleted** — `bash-foreground-completed` — ordinary,
    non-detached Bash round-trip, for contrast with #31.
36. **BashNonzeroExit** — `bash-nonzero-exit` — the tool unit's failure/exit
    code path.
37. **BashImageOutput** — `bash-image-output` — an image-bearing tool result
    reaches the feed as such (this scenario has no fake support in the OLD
    `fake-query.ts`, but IS in the landed `src/fake/scenarios/shell.ts` per
    the manifest row — confirm the exact emitted `FeedToolCallReturned.form`
    when writing).
38. **BashPartialOutputWithSpill** — `bash-partial-output-with-spill` — spool
    spill path; the manifest flags this capture's spool at 241MB, so this
    test reads back only the store-durable summary/spool-pointer facts, not
    the whole spill file.

### Merge queue, including displaced turns (`mergequeue_e2e_test.go`)

39. **MergeLeaseRefusesSubmit** — plain prose, no named golden (merge is
    daemon-synthesized, `daemon.md` §"Merge (daemon-synthesized)": "the
    vendor knows nothing about" it) — a prompt arriving after a merge began
    is REFUSED, never held (`SubmitPromptError`), per `daemon.md` §"Queue,
    holds, leases": "A prompt arriving after a merge began is refused
    (never held)".
40. **MergeBubbleCoalescesIntoOneFeedRow** — drives a real merge (self-repo
    and non-self-repo methods both, per `daemon.md`'s "two methods keyed by
    self-repo-or-not") — everything produced during the merge lands on ONE
    feed bubble (own `FeedId`, `OpenFeed`/`WatchFeed`).
41. **MergeParkedRecognizedFromLeaseState** — no content classifier; the
    conversational parked flow is the only resume path (`daemon.md`) —
    asserts a parked merge resumes ONLY via the conversational flow, never
    a hand-resolution verb (there is none).
42. **DisplacedTurnCapturedEndedThenResubmittedExactlyOnce** — the commit at
    the top of this branch (`8ad7e279c docs(overhaul): daemon.md —
    displaced turn is ENDED after durable capture, before the exactly-once
    resubmit`) — read the exact ordering this commit settles in `daemon.md`
    before writing this test; it is the single most recently changed
    contract point in this doc set and the test must match it verbatim, not
    a memory of an earlier draft.
43. **FanWideCancel** — `fan-wide-cancel` — `E2E-EVENT-INVENTORY.md`
    coverage-report item 6: a genuinely fan-wide (multi-agent) cancel, not
    `detachedcancel_e2e_test.go`'s one-at-a-time cancel. Confirm the shim
    manifest's separately-flagged agent-spool `EXIT=` terminator defect (a
    PRODUCTION defect, not a spec ambiguity) is either fixed on this branch
    or file it as an open question (see F) rather than writing an assertion
    that depends on unfixed production behavior.

### Hibernation / keepalive (`hibernation_e2e_test.go`)

44. **HibernateOnIdleCutoff** — plain session, `IdleCutoffMS` compressed —
    `daemon.md` — session hibernates after the compressed idle window;
    `HibernateError.kind{turn_in_flight, compaction_failed{error},
    no_session}` arms exercised where reachable for real (turn_in_flight by
    hibernating mid-turn; compaction_failed only if a real compaction
    failure can be provoked — else note as unreachable in F).
45. **KeepAliveNeverAppearsOnWire** — `shim.md` §"Keep-alives (entirely
    shim-internal)" — assert by ABSENCE: no `PromptOrigin` value, no rpc, no
    control-plane signal ever names a keep-alive; a keep-alive turn is
    NEVER-SERVED (excluded from every page, `store.md`/`shim.md`) — read
    back a page spanning a keep-alive turn and assert it is missing, not
    merely unasserted.
46. **RevivalAfterHibernate** — the yield obligation (`shim.md`): a real
    prompt after hibernation rolls context back to just after the last real
    prompt, discarding trailing keep-alive turns before delivery.

### Adoption on restart (`adoption_e2e_test.go`)

47. **ColdBootReadsReplayFromStore** — uses the bounce-with-real-rows helper
    (B) — `store.md` cursor/replay semantics — a cold-started daemon replays
    a workspace's history correctly from durable store rows written by a
    prior REAL session.
48. **SessionStartedReAnnouncedOnEveryNewWatch** — `PROTO-CHANGES.md` Landing
    7: `WatchSessionResponse` `oneof frame {update; session_started}` —
    "the original SessionStarted re-announced ONCE per watch, right after
    the opening diagnostics, on EVERY new watch, so an adopting daemon
    (crash boot, handover) attaches purely" — assert the exact ONE-per-watch
    cardinality, not merely presence.

    CARVE-OUT (2026-09-03): "every new watch" excludes the watch that opens
    the session. `reannounceStart` returns `undefined` while `announcedStart`
    is unset — "UNSET before StartSession, which is the one state with
    nothing to re-state" (`agent-shim/claude/shim/src/engine/session.ts`:
    2263-2267) — and the ORIGINAL daemon's watch is the one established AT
    StartSession. So the original daemon's "took the session facts" count is
    **0**, and the cold-booted successor's fresh watch is the first with an
    announced start to re-state, making the cumulative count **1**, not 2.
    The one-per-watch rule still holds; it simply has no prior announcement
    to apply to on the opening watch.
49. **HandoverTransfersAtFreeness** — `daemon.md` §"Rollout / handover" —
    blue-green: new daemon boots joining, workspaces transfer one by one
    ONLY at freeness (no in-flight turn, no live detached work); uses
    `HandoverChainTimeout`/reuse per B.
50. **RefusalOrderingDuringHandover** — same section — new daemon refuses
    `not_yet_adopted` for unowned workspaces; old daemon refuses
    `transferring_away{address}`; a lagging client self-heals.

### Refusal arms (`refusals_e2e_test.go`)

51. **BubbleRefusedNotDeliverable** — `PROTO-CHANGES.md` Landing 7:
    `SubmitPromptError.bubble_refused{kind: not_deliverable}` — a
    bubble-addressed prompt the shim cannot deliver.
52. **BubbleRefusedAgentBusy** — same, `kind: agent_busy` — `shim.v1
    UpdateAgentFailure.agent_busy` (Landing 7) relayed as
    `bubble_refused{agent_busy}` — prompt to a subagent whose own turn is
    already running.
53. **UnknownAgentOnUpdateAgent** — `shim.v1 UpdateAgentFailure.kind ==
    unknown_agent` (Landing 1, OWN ACCORD arm list) — an `UpdateAgent`
    naming an agent id the session does not recognize.
54. **StartSessionVendorStartFailed** — `shim.v1
    StartSessionFailure.cause.vendor_start_failed` — provoked for real only
    if the fake SDK has a scripted failure-start path; otherwise this is an
    unreachable-without-fabrication arm — flag in F if so, do not fabricate
    the failure to force it.
55. **REMOVED — project-lead ruling, 2026-09-04**: `TestKillTurnNotTheOpenTurn`
    was a perpetual skip (`KillTurnFailure.cause.not_the_open_turn` is an
    internal daemon/shim race with no client-observable trigger and no
    documented scenario or env lever) and is deleted rather than kept as
    noise. The arm is covered at the unit level only:
    `agent-shim/claude/shim/test/service/failures.test.ts`
    (`describe("killTurnFailure", ...)`, the `notTheOpenTurn` case),
    `agent-shim/claude/shim/test/engine/turn.test.ts` (asserts
    `failureKind(response)` is `"notTheOpenTurn"`), and
    `agent-shim/claude/shim/test/integration/turn.test.ts` ("a TurnId that
    is not the open turn is refused not_the_open_turn"). See
    SCENARIO-MATRIX.md's "Arms without an e2e lever" section.

### Degraded state (`degradedstate_e2e_test.go`)

56. **DegradedDuringRealStoreOutage** — ruling 2 — the store-stop/restart
    control (B) provokes a REAL outage; `daemon.md` §"Failure classification"
    shim-degraded arm on `AgentUpdate`/`SessionUpdate`.
57. **RecoveryAfterStoreRestart** — same test family, second half — the
    degraded fact clears once the store is back and the shim reconnects.

### Usage / accounting (`accounting_e2e_test.go`)

58. **AccountUsage** — `account-usage` — `accountInfo`/`usage_EXPERIMENTAL`
    controls, per the coverage report's explicit note that
    `usageaccounting_e2e_test.go`'s `!usage-subagent` is a DIFFERENT,
    narrower fixture and does not cover this golden.
59. **ContextUsage** — `context-usage` — context-usage push, per
    `shim.md` §"Simple reads the shim now PUSHES on the session stream
    (context usage, diagnostics) — no pull rpcs remain."

### Slash commands (`slashcommands_e2e_test.go`)

60. **VendorAnsweredSlashCommand** — `vendor-answered-slash-commands` — the
    VENDOR answering a slash command (distinct from #61/#62's daemon-side
    bookkeeping, per the coverage report's own distinction).
61. **SlashShapeANamed** — `!slash-shape-a [command]` (landed addition,
    `session.ts`) — `E2E-EVENT-INVENTORY.md` remediation item 1 — the CLI's
    own slash-command bookkeeping as a raw `user`-typed transcript record,
    UNGROUNDED/INVENTED per the shim manifest (built from
    `machinery_e2e_test.go`'s constants, not a capture) — say so in the test
    header.
62. **SlashShapeAUnnamed** — `!slash-shape-a-unnamed` — the negative: a
    record naming no command.
63. **SlashShapeBViaSlash** — `!slash` (the EXISTING `SLASH_LOCAL` scenario,
    remediation item 2 — "No new scenario code needed") — the `system`/
    `local_command` isMeta record.

### Skills (`skills_e2e_test.go`)

64. **SkillInvocation** — `skill-invocation` — tool_use → ack → isMeta
    document triple.
65. **SkillNamedAndArgsParameterized** — `!skill [skill-name] [args]`
    (landed addition) — `E2E-EVENT-INVENTORY.md` remediation item 5 — an
    ARBITRARY skill name/args/document body (e.g.
    `create-or-update-workspace merge`), replacing the fixed `"fake-skill"`.

### File tools (`filetools_e2e_test.go`)

66. **Edit** — `edit` — Edit-tool round trip, zero support in the old
    `fake-query.ts` per the coverage report; confirm the landed
    `src/fake/scenarios/files.ts` shape when writing.
67. **Glob** — `glob`.
68. **GrepContentFilesCount** — `grep-content-files-count`.
69. **ReadWholeHeadRange** — `read-whole-head-range`.
70. **WriteCreatedAndUpdated** — `write-created-and-updated`.
71. **IdeDiagnosticsAfterEdit** — `ide-diagnostics-after-edit`.

### Everything else — SPLIT INTO FOUR FILES (project-lead ruling 5)

The original single `misc_e2e_test.go` was 28 tests spanning unrelated
families; it is split four ways for four writers:

- `hooks_e2e_test.go` — #77-80
- `questions_e2e_test.go` — #88-92
- `mcpmonitors_e2e_test.go` — #81-84
- `remainder_e2e_test.go` — #72-76, #85-87, #93-99

One test per remaining golden, as below.

72. **ArtifactPublishAndList** — `artifact-publish-and-list`.
73. **ContextInjectedMemory** — `context-injected-memory`.
74. **ContextInjectedSkills** — `context-injected-skills`.
75. **CronCreateListDelete** — `cron-create-list-delete`.
76. **Diagnostics** — `diagnostics`.
77. **HookSucceeded** — `hook-succeeded`.
78. **HookBlocked** — `hook-blocked`.
79. **HookFailed** — `hook-failed`.
80. **HookCancelled** — `hook-cancelled`.
81. **McpServerHealths** — `mcp-server-healths`.
82. **McpUnmodeledTool** — `mcp-unmodeled-tool` — asserts the topbar warning
    dropdown, per `daemon.md`: "Unmodeled tools are NOT failures and never
    feed rows — their home is the topbar's warning dropdown, one warning per
    distinct name."
83. **MonitorDeadline** — `monitor-deadline`.
84. **MonitorPersistent** — `monitor-persistent`.
85. **PlanModeEnterExit** — `plan-mode-enter-exit`.
86. **PushNotificationSent** — `push-notification-sent`.
87. **PushNotificationNotSent** — `push-notification-not-sent`.
88. **QuestionFreeText** — `question-free-text` — `shim.md` §"The permission
    gate": "AskUserQuestion is a tool riding the same [canUseTool] gate" —
    `AgentQuestion` has its OWN id space (not the gated unit's).
89. **QuestionSingleSelect** — `question-single-select`.
90. **QuestionMultiSelect** — `question-multi-select`.
91. **QuestionMultipleInOneBatch** — `question-multiple-in-one-batch`.
92. **QuestionUnanswered** — `question-unanswered`.
93. **ReportFindings** — `report-findings`.
94. **ScheduleWakeupScheduleAndStop** — `schedule-wakeup-schedule-and-stop`.
95. **SendMessageQueuedAndResumed** — `send-message-queued-and-resumed`.
96. **TaskActsCreateChangeReject** — `task-acts-create-change-reject`.
97. **WebFetch** — `web-fetch`.
98. **WebSearch** — `web-search`.
99. **WorktreeEnterExitKeptAndRemoved** — `worktree-enter-exit-kept-and-removed`.

99 tests named above against 69 goldens (several goldens get more than one
test where the contract has more than one assertable fact — e.g.
compaction, permissions, subagents); every one of the 69 has at least one.
Section D is the authoritative row-by-row mapping.

---

## C2. Coverage — what this suite measures in the systems it spawns

`go test -cover` on this package would measure nothing worth having: every
system under test is a SEPARATE PROCESS. Coverage therefore comes from
instrumented BUILDS plus per-process output directories, and one knob turns
all of it on: `AGENT_REPL_E2E_COVERAGE=<dir>`.

**The invocation**, from `modules/app/agent-repl`:

```
make -C e2e coverage                       # or: bin/report-nonlisp-coverage.sh e2e
AGENT_REPL_E2E_COVERAGE_DIR=/tmp/cov make -C e2e coverage   # keep the profiles
```

It runs the suite at the documented `-parallel 8`, then merges and reports.
`e2e` is an OPT-IN component of `bin/report-nonlisp-coverage.sh`: it is never
part of that script's default sweep.

**How each system is measured**

| System | Instrumented by | Output |
|---|---|---|
| `claude-repld` | `go build -cover` (harness `MainAt`) | `GOCOVERDIR=<dir>/claude-repld`, set by `harness.StartDaemon` |
| `shim-store` | `go build -cover` (`goBuildCovered`) | `GOCOVERDIR=<dir>/shim-store`, set at both spawn sites |
| `shim-claude-sidecar` | `go build -cover` (`goBuildCovered`) | `GOCOVERDIR=<dir>/shim-claude-sidecar` |
| the TypeScript shim | `NODE_V8_COVERAGE`, set on the DAEMON and inherited by every shim it spawns (`shimclient.spawnEnv` copies the daemon's environment forward verbatim outside its fixed override set) | `<dir>/shim`, remapped through the bundle's source map |

Reporting: `go tool covdata textfmt` merges each binary's counters into a
profile, `go tool cover -func` (run from that binary's own module directory,
which is what lets it resolve the packages) reports it; the shim's v8
profiles are rendered by `c8` and summed over `agent-shim/**` sources by
`bin/e2e-shim-coverage-summary.mjs`.

**COUNTERS ONLY LAND ON A GRACEFUL EXIT.** The Go runtime writes an
instrumented binary's counters as it leaves through `main`; a SIGKILLed
process writes nothing, and neither does a SIGKILLed `node`. All four systems
install SIGTERM handlers and exit through their own `main`, so:

- the store and the sidecar already leave through SIGTERM (`Store.Stop`,
  `Sidecar.Stop`);
- on a coverage run only, `harness.StartDaemon` registers a teardown that
  SIGTERMs the daemon and then its shims before the usual kill. It is
  registered AHEAD of the warning sweep, so `t.Cleanup`'s
  last-registered-first unwind runs it AFTER that sweep: the sweep reads the
  same log content with coverage as without, and the pass set does not move.
  (Registered the other way round, the graceful shutdown's own error records
  failed two merge-queue tests that pass on every ordinary run.)
- a process a test deliberately SIGKILLs (the cold-gate crash simulation)
  contributes nothing, by design.

**Two things stay unmeasured.**

- `shim-lock` — the SHIM spawns it, so nothing in the harness can hand it a
  `GOCOVERDIR`, and an instrumented Go binary started without one writes a
  warning onto a stderr the shim reads. It is built uninstrumented on purpose.
- the webapp — `webapplayer_e2e_test.go` drives it through its own npm script;
  its coverage belongs to `bin/report-nonlisp-coverage.sh webapp`.

**One production touch, stated plainly:** `agent-shim/claude/shim/build.mjs`
emits an external source map under `SHIM_BUILD_SOURCEMAP=1`, which ONLY the
coverage build sets. Every other build — the deploy path included — is
byte-for-byte what it was, so the bundle's build identity is unchanged.

### Baseline — measured 2026-09-04, `make -C e2e coverage`, 16-core host

| System | Statement coverage |
|---|---|
| `claude-repld` | **54.5%** |
| `shim-store` | **60.2%** |
| `shim-claude-sidecar` | **63.7%** |
| shim TypeScript (`agent-shim/**` sources, 94 files) | **82.6%** (24926/30182) |

The run: 138 `PASS` records, 59 `SKIP` (Emacs scenarios skip on a host with
no batch Emacs; the webapp-layer tests skip without `webapp/node_modules`),
`TestColdGate/Clear` failing as it is known to.

A CONTROL run of the same suite WITHOUT coverage recorded the identical
138/59 pass/skip counts and the same known `TestColdGate/Clear` failure, so
coverage does not move the pass set. Each of the two full runs also carried
one DIFFERENT extra failure under full-suite load —
`TestHibernateOnIdleCutoff` (coverage run) and
`TestRefusalOrderingDuringHandover` (control run) — each of which passes
repeatedly in isolation both with and without coverage. That is pre-existing
load-dependent flakiness in the suite, not a coverage effect, and it is
reported as such rather than papered over.

---

## D. Scenario mapping table — all 69 goldens

| # | Golden scenario | Registered scenario(s) | Test(s) (§C) | New fake-SDK scenario needed? |
|---|---|---|---|---|
| 1 | account-usage | `!usage-full` | #58 | no |
| 2 | artifact-publish-and-list | `!artifact-publish` + `!artifact-list` | #72 | no |
| 3 | bash-detached | `!bash-detach` | #31 | no |
| 4 | bash-foreground-completed | `!bash` | #35 | no |
| 5 | bash-image-output | `!bash-image` | #37 | no |
| 6 | bash-interrupted-by-timeout | `!bash-timeout` | #19 | no |
| 7 | bash-nonzero-exit | `!bash-fail` | #36 | no |
| 8 | bash-partial-output-with-spill | `!bash-spill` | #38 | no |
| 9 | compaction-directed | `!compact` | #22, #23 | no |
| 10 | context-budget-warning | `!context-budget-warning` (UNGROUNDED) | #26 | no (landed, UNGROUNDED per manifest — see F) |
| 11 | context-injected-memory | `!memory` | #73 | no |
| 12 | context-injected-skills | `!skills-injected` | #74 | no |
| 13 | context-usage | `!context-usage-drift` | #59 | no |
| 14 | cron-create-list-delete | `!cron` | #75 | no |
| 15 | ctrl-b-detach-of-foreground-subagent | `!subagent` (reachable-only) | #34 REMOVED (project-lead ruling, 2026-09-04) | no |
| 16 | ctrl-b-detach-of-foreground-work | `!vendor-backgrounded` (reachable-only) | #33 REMOVED (owner ruling, 2026-09-04) | no |
| 17 | diagnostics | (none; default `""` scenario) | #76 | no |
| 18 | edit | `!edit` | #66 | no |
| 19 | fan-wide-cancel | `!cancel-all` | #43 | no (see F: possible production spool defect) |
| 20 | fast-mode | `!fast-on` | #8 | no |
| 21 | glob | `!glob` | #67 | no |
| 22 | grep-content-files-count | `!grep-content` + `!grep-files` + `!grep-count` | #68 | no |
| 23 | held-turn-gate | `!hold` | #16 | no |
| 24 | hook-blocked | `!hook-blocked` | #78 | no |
| 25 | hook-cancelled | `!hook-cancelled` | #80 | no |
| 26 | hook-failed | `!hook-failed` | #79 | no |
| 27 | hook-succeeded | `!hook-success` | #77 | no |
| 28 | ide-diagnostics-after-edit | `!ide-diagnostics` | #71 | no |
| 29 | identity-rotation-clear | `!rotate` | #20, #21 | no |
| 30 | interrupt | `!interrupt` | #18 | no |
| 31 | max-tokens | `!max-tokens` | #9 | no |
| 32 | mcp-server-healths | `!mcp-all` | #81 | no |
| 33 | mcp-unmodeled-tool | `!unmodeled` | #82 | no |
| 34 | model-changed | `!model-fallback` | #7 | no |
| 35 | monitor-deadline | `!monitor-deadline` | #83 | no |
| 36 | monitor-persistent | `!monitor-persistent` | #84 | no |
| 37 | permission-allow-once | `!perm-allow-once` | #11 | no |
| 38 | permission-allow-standing | `!perm-allow-standing` | #12 | no |
| 39 | permission-denied-by-policy | `!perm-deny-policy` | #14 | no |
| 40 | permission-denied-by-user | `!perm-deny-user` | #13 | no |
| 41 | permission-mode-changed | `!perm-allow-standing-mode` | #17 | no |
| 42 | permission-undecidable-parked | `!perm-hold` | #15 | no |
| 43 | plan-mode-enter-exit | `!plan` | #85 | no |
| 44 | prose-streamed | default `""` scenario | #1, #10 | no |
| 45 | push-notification-not-sent | `!push-config-off` + `!push-user-present` + `!push-no-transport` | #87 | no |
| 46 | push-notification-sent | `!push-sent` | #86 | no |
| 47 | question-free-text | `!ask-free` | #88 | no |
| 48 | question-multi-select | `!ask-multi` | #90 | no |
| 49 | question-multiple-in-one-batch | `!ask-multi` | #91 | no |
| 50 | question-single-select | `!ask-single` | #89 | no |
| 51 | question-unanswered | `!ask-unanswered` | #92 | no |
| 52 | read-whole-head-range | `!read` + `!read-head` + `!read-range` | #69 | no |
| 53 | report-findings | `!findings` | #93 | no |
| 54 | schedule-wakeup-schedule-and-stop | `!wakeup-schedule` + `!wakeup-stop` | #94 | no |
| 55 | send-message-queued-and-resumed | `!send-message` + `!send-message-resumed` | #95 | no |
| 56 | skill-invocation | `!skill` | #64 | no |
| 57 | subagent-detached | `!subagent-detached` | #28 | no |
| 58 | subagent-sync-nested-activity | `!subagent` | #27 | no |
| 59 | task-acts-create-change-reject | `!task-create` + `!task-change` + `!task-reject` | #96 | no |
| 60 | turn-stop-error-during-execution | `!fail-execution` (DECLARED-ONLY) | #5 | no (DECLARED-ONLY, see manifest) |
| 61 | turn-stop-hook-stop | `!fail-stop-hook` (DECLARED-ONLY) | #6 | no (DECLARED-ONLY) |
| 62 | turn-stop-max-budget-usd | `!fail-budget` | #3 | no |
| 63 | turn-stop-max-structured-output-retries | `!fail-structured-output` (DECLARED-ONLY) | #4 | no (DECLARED-ONLY) |
| 64 | turn-stop-max-turns | `!fail-max-turns` | #2 | no |
| 65 | vendor-answered-slash-commands | `!slash` | #60 | no |
| 66 | web-fetch | `!web-fetch` | #97 | no |
| 67 | web-search | `!web-search` | #98 | no |
| 68 | worktree-enter-exit-kept-and-removed | `!worktree-keep` + `!worktree-remove` | #99 | no |
| 69 | write-created-and-updated | `!write-create` + `!write-update` | #70 | no |

**0 of 69 goldens need a new fake-SDK scenario.** Every scenario, and every
extra option `E2E-EVENT-INVENTORY.md`'s remediation list asked for (10
numbered items, all under "PROJECT-LEAD RULINGS... ruling 0: the fake SDK
arrives with the five-way merge"), is already present in
`agent-shim/claude/shim/src/fake/scenarios/*.ts` and documented in
`agent-shim/claude/shim/testdata/captures/MANIFEST.md`'s "`e2ecleanup/
fakesdk-ext` additions" section, confirmed by grep in this worktree, not
merely planned:

- `!slash-shape-a` / `!slash-shape-a-unnamed` — `session.ts`
- `!compact [summary]` override — `session.ts`
- `!context-budget-warning` — `session.ts`
- `!skill [skill-name] [args]` — `skills.ts`
- `!bash-detach-poll` — `shell.ts`
- `!subagent-detached-utterance` — `subagents.ts`
- `!usage-historical` — `subagents.ts`

Three of these carry an explicit UNGROUNDED/INVENTED or DECLARED-ONLY mark
in the shim's own manifest (`context-budget-warning`, `bash-detach-poll`,
`subagent-detached-utterance`, `usage-historical`, and the three
turn-stop-* DECLARED-ONLY arms). Tests against them (§C #4, #5, #6, #26,
#30, #32, #61, #62) MUST say so in their own header comments, the way the
manifest itself does, so a future reader does not mistake "compiles and
passes" for "grounded in a real vendor recording."

**"Registered scenario(s)" naming drift — RECONCILED.** This table's own
column above states, per golden, which registered `!name` selects it —
often a different name than the golden's own (`hook-succeeded` selects
`!hook-success`, and so on), and sometimes several names in combination
(`+`-joined rows). This was a known drift, recorded in
`docs/overhaul/reports/E2E-SCENARIO-COVERAGE.md`'s "Manifest/registry naming
drift" section; that section, and `agent-shim/claude/shim/testdata/captures/
MANIFEST.md`'s own `Scenarios:` column and reconciliation section, now carry
the FULL mapping in both directions, and `src/fake/registry.ts` grew an
`ALIASES` map so every golden with a 1:1 mapping can also be driven by ITS
OWN NAME (docs-only from this file's side; the alias mechanism and its test
guard live in the shim package). The tests in this suite are UNCHANGED —
they already drove the correct registered names — this is purely a
naming-drift cleanup on the shim side.

---

## E. Fanout plan

One file per area (§C's groupings), one writer per file, so writers never
touch the same file. Shared-file overlap is limited to two files every
writer reads but none writes:

- `harness_e2e.go` (or `main_test.go` + a small `internal/e2eharness`-style
  helper file) — the per-test daemon+store+sidecar+shim bringup, the store
  stop/restart control, the bounce-with-real-rows helper, the grep gate,
  and `TestMain`. Written FIRST, by one writer, before any area file, since
  every area file imports it. Nothing else touches it.
- `SPEC.md` (this file) — read-only reference for every writer.

Area files, each independent (zero overlap once `harness_e2e.go` exists):

1. `turnlifecycle_e2e_test.go` — §C #1-10
2. `permission_e2e_test.go` — §C #11-17
3. `interrupt_e2e_test.go` — §C #18-19
4. `identityrotation_e2e_test.go` — §C #20-21
5. `compaction_e2e_test.go` — §C #22-26
6. `subagents_e2e_test.go` — §C #27-30
7. `detachedbash_e2e_test.go` — §C #31-38
8. `mergequeue_e2e_test.go` — §C #39-43
9. `hibernation_e2e_test.go` — §C #44-46
10. `adoption_e2e_test.go` — §C #47-50
11. `refusals_e2e_test.go` — §C #51-55
12. `degradedstate_e2e_test.go` — §C #56-57
13. `accounting_e2e_test.go` — §C #58-59
14. `slashcommands_e2e_test.go` — §C #60-63
15. `skills_e2e_test.go` — §C #64-65
16. `filetools_e2e_test.go` — §C #66-71
17. `hooks_e2e_test.go` — §C #77-80
18. `questions_e2e_test.go` — §C #88-92
19. `mcpmonitors_e2e_test.go` — §C #81-84
20. `remainder_e2e_test.go` — §C #72-76, #85-87, #93-99

20 independent writer groups plus the one harness writer who goes first
(21 total dispatches, one dependency edge: everyone else waits on the
harness file's exported surface being named, not built — the harness
writer can hand out the intended function signatures before finishing the
implementation, same as any other fanout in this project).

---

## F. Open questions — ALL RULED (project lead, 2026-09-02)

SPEC APPROVED. Dispositions, binding on every writer:

1. **Fan-wide-cancel `EXIT=` terminator — CLOSED, not a defect.** Already
   fixed; the regression test pinning it is
   `agent-shim/claude/shim/test/fake/scenarios/subagents.test.ts:299`
   ("writes NO EXIT line into a stopped AGENT's spool"). Test #43 is a
   normal green test.

2. **Hook scenario shapes — no ruling needed.** A writer finding the landed
   shape differs from what #77-80 need reports it; it is not a spec defect
   and is never fixed by changing production.

3. **`vendor_start_failed` — NOT DROPPED, and NO new scenario needed.**
   The project lead ruled the arm must be covered. Investigation then
   established from the CONTRACT that a prompt-selected scenario for it is
   structurally impossible, and that no new shim code is required either:

   - `proto/src/shim/v1/endpoint_start_session.proto`
     (`StartSessionRequest` / `StartSessionFresh` / `StartSessionResume`)
     carries NO prompt text — only `model`+`permission_mode` (fresh) or
     `vendor_session_id`+`cold_remediation` (resume). `StartSession`
     resolves BEFORE any prompt exists, so there is no text off which a
     scenario could ever be selected. A prompt-selected
     `!vendor-start-failed` would require a new selector field on
     `StartSessionFresh`/`StartSessionResume` — a proto/production change,
     out of scope and unnecessary.
   - The fake SDK ALREADY has the documented lever: the whole-process env
     var `AGENT_REPL_FAKE_REFUSE=start` (`src/fake/index.ts` lines 124-142,
     which state outright that these arms are "answers to CONTROL CALLS,
     not to a turn", so no scenario prompt can reach them). A `start-once`
     variant refuses only the FIRST StartSession, for retry recovery.
     Existing shim coverage:
     `agent-shim/claude/shim/test/integration/session.test.ts:1530-1536`.
   - A whole-process lever is SAFE here because section B brings up one
     daemon + store + sidecar PER TEST: the blast radius is one test's
     world.

   Therefore: test #54 sets `AGENT_REPL_FAKE_REFUSE=start` on its world's
   shim environment via the harness's extra-shim-env option, and the
   retry-recovery test uses `start-once`. Branch
   `overhaul/shim-e2e-startfail` was opened for a scenario addition and
   then abandoned unused; NO shim change was made.

4. **Compaction-failure terminal shape — deferred to the area writer**, who
   reads `session.ts`'s `COMPACT_FAILED` scenario body rather than guessing.
   Not a contract ambiguity.

5. **Real git — RULED, THEN REVERSED BY THE USER: this suite does NOT use
   real git.** Only the systems we own run for real; every external
   dependency is mocked, git included. See B, "Every external dependency is
   MOCKED". The `SkipFakeGit` seam is withdrawn and nothing may set it; the
   git facts belong to the git-client leaf's own tests.

6. **`context-budget-warning` grounding — future housekeeping**, unchanged.

The original text of these questions is retained below for provenance.

---

## F (original). Open questions as first drafted

1. **`fan-wide-cancel`'s agent-spool `EXIT=` terminator.** The shim
   coverage report (`E2E-SCENARIO-COVERAGE.md`, ranked gap #6) flags this as
   "a real fake-writer defect... blocking this even after scenario support
   lands." I did not read shim production source to check whether it is
   fixed on this branch (out of scope for a spec-only pass). Test #43
   (`FanWideCancel`) may be unwritable as a PASSING test until this is
   confirmed fixed. Please confirm whether it is fixed, or whether #43
   should be written expecting failure / marked with a citation to this
   open item instead of asserted as green.

2. **`turn-stop-hook-stop` / hook family scenarios' relationship to "no PreToolUse/PostToolUse hook simulation."** The OLD coverage report says
   `fake-query.ts` had zero hook simulation. The MANIFEST confirms
   `hook-succeeded`/`hook-blocked`/`hook-failed`/`hook-cancelled` are named
   captures with real goldens, and `src/fake` (the NEW, landed fake SDK) is
   a different, richer implementation than `fake-query.ts` — but I did not
   verify each hook scenario's exact tool_use/tool_result shape by reading
   `hooks.ts` line-by-line (a compile-gate-only spec pass reads for
   existence and naming, not full behavioral verification). If a
   hook-scenario writer finds the landed shape does not match what tests
   #77-80 need, that is a normal implementation-time finding, not a spec
   defect — flagging here only so the project lead knows this file's
   contents were not independently walked.

3. **`StartSessionFailure.cause.vendor_start_failed` (test #54) may be
   structurally unreachable without a scripted failure-start scenario.**
   I did not find a named `!`-prefixed scenario in the manifest whose sole
   purpose is failing session START itself (as opposed to failing a TURN).
   If none exists, should this arm's e2e coverage be dropped (documented as
   "no real end-to-end path provokes this, per ruling 1's disposition
   menu"), or does the fake SDK need a new, narrower addition scoped
   outside this spec's "0 new scenarios" finding? This is exactly the shape
   of question ruling 1 in `E2E-EVENT-INVENTORY.md` already answered for a
   different case (`acceptOnceShim`) — I am flagging the same category of
   question for a case that ruling did not explicitly cover.

4. **Compaction-failure reachability (test #25, `CompactionFailed`).** I
   confirmed `!compact-failed` exists as a named prompt in `session.ts` but
   did not trace what daemon-visible terminal it produces (a
   `HibernateError.compaction_failed{error}`, a plain turn failure, or
   something else) — the test-list entry above says "resolve by reading
   `session.ts`'s `COMPACT_FAILED` scenario body when writing this test,
   not by guessing here" precisely because I did not read it. Not an
   ambiguity in the CONTRACT, just an implementation detail deferred to the
   area writer; noting it here so the project lead does not read #25's
   entry as under-specified by oversight.

5. **Whether real `git` should replace the fake `git` for this suite.**
   Section A's seam design leaves `installFakeGit`/`World(t)` untouched
   (unrelated to the vendor-call ban; git is not the vendor). Nothing in
   the six contract docs or `PROTO-CHANGES.md` calls out git-facing
   behavior as in scope for cross-system e2e (the merge-queue tests in §C
   exercise the DAEMON's merge orchestration against the harness's existing
   scripted `git` fixture world, same as `daemon/integration` does). If the
   project lead wants merge tests against a REAL git repository instead
   (e.g. to catch a real git-invocation regression), that is a scope
   increase this spec does not currently include — flagging rather than
   deciding it myself.

6. **`context-budget-warning`'s eventual grounding.** Ruling 6 marks this
   scenario "provisionally settled... pending a grounding capture." Test
   #26 asserts the wire shape lands correctly today; if/when a real capture
   grounds it, the test's header-comment caveat becomes stale and should be
   removed, but that is a future housekeeping item, not something this spec
   resolves now.

No place was found where the CONTRACT (the six planning docs +
`PROTO-CHANGES.md`) and PRODUCTION appear to disagree — this pass never read
production source for behavior (only for module/build layout facts needed
for section A), so it is not positioned to detect such a disagreement in
the first place. Items 1-4 above are the closest candidates and are phrased
as reachability/verification questions rather than contract-vs-production
conflicts, per the binding instruction not to read production code looking
for bugs.

## G. Open items for the project lead

- **A response-level failure does not reach the feed as `FeedResponse.error`.**
  (Raised 2026-09-03, observed against the real shim.) For `!max-tokens` the
  shim's own converter settles the block as a failure
  (`shim.convert.stream`: "settling a prose block ... settled=failure") and
  `daemon/internal/resolve/feed/response.go`'s `AgentResponse_Failure` case
  builds `FeedResponse.error`, yet the row the feed finally publishes for
  that block does not carry the error arm. `TestMaxTokens` therefore asserts
  the response-level fact the contract states in words — "the text is kept,
  the answer is incomplete" (`agent-shim/claude/shim/AGENTS.md:335`) — by
  the KEPT PROSE, and accepts either settled arm. Whether the last upsert of
  the block restates it as settled-success, or the failure never reaches the
  resolver, is the daemon's to determine.

- **An activity row published after its turn's terminal carries no turn id.**
  (Same investigation.) `stampTurn` (`daemon/internal/resolve/feed/sink.go`)
  stamps from the turn IN FLIGHT, so a block whose last upsert lands after
  the terminal loses the stamp its earlier upserts had. Any test matching an
  activity row by turn id is therefore racing the upsert order; `TestMaxTokens`
  matches on content instead. Whether a row should be able to lose a stamp it
  once had is a daemon question.

- **`HibernateError.kind.turn_in_flight` is effectively dead.** (Raised
  2026-09-03 by the e2e triage.) The idle sweep short-circuits on its OWN
  freeness pre-check — `if !c.deps.Freeness.Free(ws.ID) { ... "the idle
  session is not free; deferring its hibernation" ... continue }`
  (`daemon/internal/drain/sweep.go:64-66`) — BEFORE it ever calls
  `hibernate`, so a session with a turn in flight never receives a Hibernate
  directive and the shim never gets the chance to refuse with
  `turn_in_flight`. No path in the daemon reaches that arm today.
  `TestHibernateOnIdleCutoff` therefore asserts the pre-check's own record.
  Either the daemon should stop pre-checking and let the shim arbitrate (the
  structural answer: one arbiter, not two), or the arm should be retired.

- **The e2e harness must launch the daemon with EXACTLY the argv the Emacs
  launcher builds.** (Raised 2026-09-02 by the elisp daemon-argv landing; NOT
  implemented here.) The launcher now appends `--default-config-dir` and
  `--multi-repo-config-dir` (both expanded) and exports `MULTI_REPO_ROOT`,
  because the daemon's account resolver refuses to build without the two roots
  and exits 2 — which is precisely the defect that reached the user's live
  logs. An e2e harness that builds its own argv is a SECOND spelling of the
  launch contract and can drift from the one that ships; the e2e daemon start
  should derive its argv from the elisp launcher's own builder
  (`agent-repl-daemon--argv` in `lisp/daemon.el`, and the account roots in
  `daemon/integration/harness/daemon.go`) rather than restating it. Until that
  is settled, the guard against a missing root lives in
  `daemon/integration/boot_test.go` (`TestBootRefusesWithoutAnAccountRoot`) and
  in `lisp/test-daemon.el`.
