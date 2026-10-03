package e2e

import (
	"bytes"
	"context"
	"crypto/rand"
	"encoding/hex"
	"errors"
	"fmt"
	"io/fs"
	"net"
	"net/http"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"syscall"
	"testing"
	"time"

	"agentrepl/logging/buildreport"
	agentreplv1 "agentrepl/proto/agentrepl/v1"
	"agentrepl/proto/agentrepl/v1/agentreplv1connect"
	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
	storev1 "agentrepl/proto/store/v1"
	"agentrepl/proto/store/v1/storev1connect"
	workspacev1 "agentrepl/proto/workspace/v1"

	"connectrpc.com/connect"

	"claude-repld/integration/harness"
)

// ===========================================================================
// Waits (SPEC.md section B, "Waits"). Event-driven only: a daemon watch-
// stream frame, a structured-log record, or a bounded store-read poll. No
// time.Sleep for synchronization anywhere in this package.
// ===========================================================================

// DefaultTimeout bounds an ordinary per-test wait. Reuses
// harness.DefaultTimeout verbatim rather than minting a same-value twin — see
// that constant's own sizing rationale (a 443-test daemon/integration run:
// median 0.2s, observed max 1.7s), independently corroborated by the deleted
// daemon/e2e's own ~0.65s-per-await baseline for a real node-shim spawn plus
// a turn through a real store ("roughly an 8x margin"). Two independently
// measured real-process baselines agreeing on 5s is strong grounding for
// reusing it as-is.
const DefaultTimeout = harness.DefaultTimeout

// HandoverChainTimeout bounds the restart/adoption/handover tests that chain
// a SECOND real process boot onto the same context (adoption_e2e_test.go).
// Reused verbatim from harness — see its doc comment; the shape (two real
// process lifecycles on one budget) matches exactly.
const HandoverChainTimeout = harness.HandoverChainTimeout

// AdoptionChainTimeout is HandoverChainTimeout under the name the adoption
// area uses at its call sites: a restart/adoption test chains a second real
// claude-repld boot onto the same context, exactly the shape
// HandoverChainTimeout's doc comment describes. Kept as a distinct name (not
// a re-mint of the value) so an adoption test reads self-documenting at the
// call site without the reader having to know the handover family shares its
// budget.
const AdoptionChainTimeout = HandoverChainTimeout

// StoreOutageWindow bounds the degraded-state tests (degradedstate_e2e_test.go)
// that provoke a REAL store outage with Store.Stop/StartSameDB. Derived the
// same way the sidecar's own integration suite derived
// UnownedSpoolWindow=200ms ("a subject that is not ABOUT the hold would
// simply time out inside it; subjects that ARE about the hold set their own
// window") — this suite's degraded-state test IS about the outage, so it
// gets its own window rather than DefaultTimeout. The sidecar's own
// UnownedSpoolWindow constant is reused directly as the sidecar's outage
// tolerance (see Sidecar's defaultSidecarOpts below); this constant is the
// TEST's own budget for observing the daemon's shim-degraded fact and its
// clearing, one order of magnitude above the sidecar's own window so the
// test's wait is never the tighter of the two bounds.
const StoreOutageWindow = 2 * time.Second

// unownedSpoolWindow is this suite's own copy of the sidecar's real
// production default divisor, at a value matched to
// shim-sidecar/integration/helpers_test.go's own defaultSidecarOptions
// (UnownedSpoolWindow: 200ms) for the identical reason given there.
const unownedSpoolWindow = 200 * time.Millisecond

// pollInterval is how often a bounded store-read poll re-checks. It is a poll
// of a durable surface, never a sleep standing in for a signal.
const pollInterval = 20 * time.Millisecond

// ===========================================================================
// World: one test's full stack — a real store, a real sidecar, and a real
// daemon spawning the real shim per session. Every area test file builds one
// with NewWorld and drives it purely through the embedded *harness.Daemon's
// Connect client and watch streams (WatchFeed, WatchTopbar, WatchDaemonHolds,
// ...) plus this file's SubmitPrompt/driveScenarioToCompletion helpers.
//
// World embeds *harness.Daemon, so every exported harness.Daemon method
// (Client, WatchFeed, WatchTopbar, WatchDaemonHolds, AwaitLogRecord, ...) is
// callable directly as a method of World; harness.Register(t, w.Daemon, dir)
// still works by naming the embedded field explicitly where a function
// (rather than a method) is required.
// ===========================================================================

// World is one test's daemon + store + sidecar + (session-spawned) real shim.
type World struct {
	*harness.Daemon
	Store   *Store
	Sidecar *Sidecar
}

// WorldOpts configures one World. DaemonOpts is forwarded to
// harness.StartDaemon verbatim EXCEPT for the three fields NewWorld always
// sets itself (ShimNode, ShimMain, StoreSocket) — a caller that sets those is
// overridden, since this suite's whole point is exercising the real shim and
// real store against the daemon's scripted fake git, exactly as
// daemon/integration does (SPEC.md section B, "Every external dependency is
// MOCKED").
//
// DaemonOpts.ExtraEnv is the seam for a world-wide lever the real shim reads
// from its OWN spawn environment. It reaches every shim this daemon spawns
// unchanged: StartDaemon appends it onto the daemon process's own env, and
// the daemon's shimclient.spawnEnv copies the daemon's os.Environ() forward
// to each spawned shim verbatim except for a fixed, small override set
// (CLAUDE_CONFIG_DIR, SHIM_BUILD_SHA, state dir, session id,
// forbid-vendor-calls) — never an allowlist. Known consumer: the fake SDK's
// `AGENT_REPL_FAKE_REFUSE` (src/fake/index.ts) — "start" refuses every
// StartSession the whole process makes; "start-once" refuses only the
// FIRST one, for retry-recovery coverage. This is how the refusals area
// (test #54, StartSessionVendorStartFailed) provokes
// StartSessionFailure.cause.vendor_start_failed for real, since
// StartSession resolves before any prompt exists and so cannot be reached
// by a scenario prompt.
type WorldOpts struct {
	DaemonOpts harness.Opts

	// SidecarStaleness tightens the sidecar's OWN LOST-policy windows for a
	// test that is ABOUT one of them. UNSET (the zero value) leaves every
	// window at the sidecar's production default, which is what every test
	// that is not about the policy wants: a 30s grace and a 30m shell
	// silence simply cannot be reached inside this suite's budget, so an
	// ordinary world never concludes a run LOST by accident. Set it through
	// NewWorldWithSidecarStaleness rather than by hand, so the reason a
	// world runs with a short window is stated at its construction site.
	SidecarStaleness SidecarStaleness
}

// SidecarStaleness is the sidecar's LOST-policy window set, as this suite
// hands it over (`--stale-grace`, `--stale-shell-silence`,
// `--stale-agent-silence`, `--stale-workflow-silence`,
// `--unowned-spool-window`; agent-shim/claude/shim-sidecar/AGENTS.md, "The
// LOST policy"). A ZERO FIELD IS "LEAVE IT ALONE": the flag is not passed at
// all, and the sidecar keeps its own production default — never a zero
// window, which the sidecar refuses at bootstrap anyway.
//
// SHORTEN ONLY THE WINDOW A TEST IS ABOUT. The other windows are what stop
// some OTHER arm reaching a conclusion first and stealing the subject, which
// is the same rule the sidecar's own integration suite states for its
// `lostOptions` helper (shim-sidecar/integration/lost_policy_test.go).
type SidecarStaleness struct {
	// Grace is how long a VANISHED file is given before its disappearance is
	// concluded rather than treated as a rename race (file_vanished).
	Grace time.Duration
	// ShellSilence is how long a present-but-unchanged SHELL SPOOL may stay
	// quiet before it is concluded went_silent.
	ShellSilence time.Duration
	// AgentSilence is the same window for an agent-kind file.
	AgentSilence time.Duration
	// WorkflowSilence is the same window for a workflow journal.
	WorkflowSilence time.Duration
	// UnownedSpool is the hold an unclaimed spool sits in before it is tailed
	// at all. Zero leaves this suite's own default (unownedSpoolWindow),
	// which NewWorld already applies to every world.
	UnownedSpool time.Duration
}

// NewWorldWithSidecarStaleness builds a world whose sidecar runs with the
// given LOST-policy windows. It is the ONLY intended way to shorten them:
// the sidecar's production windows (30s grace, 30m shell silence) cannot
// elapse inside this suite's budget, so the three DetachedLost arms are
// unreachable without it — and a test that reaches them must say, at its
// construction site, which window it is buying.
func NewWorldWithSidecarStaleness(t *testing.T, opts WorldOpts, staleness SidecarStaleness) *World {
	t.Helper()
	opts.SidecarStaleness = staleness
	return NewWorld(t, opts)
}

// worldDaemonEnv is a world daemon's environment: the caller's ExtraEnv,
// then this suite's own statements, appended LAST so they win (the shim's env
// is scanned front-to-back and the last assignment of a name is the effective
// one). It names no checkout: harness.StartDaemon pins its own (see
// checkoutEnv).
func worldDaemonEnv(extra []string, spoolRoot, lockBin string) []string {
	return append(append([]string{}, extra...),
		"AGENT_REPL_FAKE_SPOOL_ROOT="+spoolRoot,
		// The shim inherits this from the daemon and spawns it for every
		// kernel claim; without it no session starts at all.
		"AGENT_REPL_SHIM_LOCK_BIN="+lockBin)
}

// NewWorld builds one test's stack: the real store, the real sidecar, and a
// real daemon spawning the real (built-from-source, `--fake`-mode) shim.
// Every process is killed and every socket/temp root is removed at test
// cleanup, in reverse start order (shim [daemon-owned] -> daemon -> sidecar
// -> store), so tearing the store down never races a live writer.
//
// GUARANTEE: a store or sidecar process that exits before the test's own
// cleanup runs — for any reason OTHER than a deliberate Store.Stop /
// Store.StartSameDB cycle — fails the test. This is automatic and requires
// no action from the test; there is no opt-out. It mirrors
// harness.Daemon's own unconditional warning sweep for the same reason: a
// check a test must remember to ask for is a check the tests that need it
// most (a store or sidecar that silently died) will forget to ask for.
//
// Loud skips (never a fail): no `node` on PATH, the shim's deps not
// installed (names the exact `npm ci` command — never runs it), no `go` on
// PATH. A store or sidecar build FAILURE (not a missing precondition) fails
// the test outright: that is a real defect this suite exists to catch.
func NewWorld(t *testing.T, opts WorldOpts) *World {
	t.Helper()
	node := requireNode(t)
	shimMain := requireShimBundle(t)
	sidecarBin := requireSidecarBinary(t)
	lockBin := requireLockBinary(t)

	// START ORDER: store, then sidecar, then daemon (which spawns the shim
	// per session). Cleanups are registered in this same order, so
	// t.Cleanup's LIFO unwind tears down daemon -> sidecar -> store — daemon
	// (and its shims) first, so nothing is left writing to a store or
	// sidecar that already went away.
	// ONE directory holds every log this world produces. The daemon's own
	// sinks already live under the state root's logs/ directory, so routing
	// the store's and the sidecar's logs there too means
	// preserveLogsOnFailure's single-directory sweep collects all of them
	// with no extra bookkeeping (SPEC.md section B, "Failure artifacts").
	// The state root is minted HERE rather than by harness.StartDaemon
	// because the store starts first and needs the logs directory already.
	stateRoot := opts.DaemonOpts.StateDir
	if stateRoot == "" {
		stateRoot = shortStateRoot(t)
	}
	logsDir := filepath.Join(stateRoot, "logs")
	if err := os.MkdirAll(logsDir, 0o755); err != nil {
		t.Fatalf("e2e: mkdir %s: %v", logsDir, err)
	}

	// The daemon's lock dir, settled before any process starts: the store and
	// the sidecar write their build reports into it, and the daemon's deploy
	// reads them from it.
	lockDir := harness.LockDirFor(stateRoot)
	storeDB := filepath.Join(t.TempDir(), "store.db")
	store := startStore(t, shortSocketPath(t, "store"), storeDB, filepath.Join(logsDir, "store.log"), lockDir)

	// ONE spool root for the whole world. The fake SDK inside every shim
	// this daemon spawns writes its task spools here (via
	// AGENT_REPL_FAKE_SPOOL_ROOT, src/fake/index.ts), and the sidecar globs
	// exactly this tree (--spool-root). Two roots would mean the sidecar
	// never sees a spool the fake wrote; worse, the fake's own default is
	// the REAL vendor location /tmp/claude-<uid>, so leaving it unset does
	// not merely break the pairing, it writes into the developer's live
	// vendor spool directory.
	spoolRoot := t.TempDir()

	daemonOpts := opts.DaemonOpts
	daemonOpts.ShimNode = node
	daemonOpts.ShimMain = shimMain
	daemonOpts.StoreSocket = store.Socket
	daemonOpts.StateDir = stateRoot
	daemonOpts.ExtraEnv = worldDaemonEnv(daemonOpts.ExtraEnv, spoolRoot, lockBin)
	// THE REAL SERVICES ARE WHAT A DEPLOY JUDGES. They report their own
	// builds into the daemon's lock dir (startStore/startSidecar are pointed
	// at it), and the deploy's fake build stages copies of these very
	// binaries, so a deploy that should touch no service restarts none.
	daemonOpts.ServiceBinaries = harness.ServiceBinaries{Store: store.bin, Sidecar: sidecarBin}

	// REGISTERED BEFORE THE DAEMON EXISTS, SO IT RUNS LAST OF ALL. t.Cleanup
	// unwinds last-registered-first, and the daemon registers its own
	// cleanups from inside StartDaemon: the stop (which flushes the last
	// records) and, ahead of it, the WARNING SWEEP. Registered after
	// StartDaemon, this ran FIRST — before the sweep had failed the test —
	// so every sweep failure preserved nothing at all, which is exactly the
	// class of failure whose logs are hardest to get a second time. Closing
	// over the variable rather than the value is what lets it be armed before
	// there is a daemon to name.
	var d *harness.Daemon
	preserveLogsOnFailure(t, &d, storeDB)
	d = harness.StartDaemon(t, daemonOpts)
	resolveConfigRoots(t, d)

	sidecar := startSidecar(t, sidecarBin, sidecarOpts{
		StoreSocket: store.Socket,
		ConfigRoots: []string{d.DefaultConfigDir, d.MultiRepoConfigDir},
		SpoolRoot:   spoolRoot,
		LogPath:     filepath.Join(logsDir, "sidecar.log"),
		StateDir:    stateRoot,
		Staleness:   opts.SidecarStaleness,
		LockDir:     lockDir,
	})
	if d.LockDir != lockDir {
		t.Fatalf("e2e: the daemon's lock dir is %s but the services report into %s; its deploy would restart them", d.LockDir, lockDir)
	}
	// A deploy reads the sidecar as current only once it has reported, so the
	// world is not handed over before it has.
	awaitBuildReport(t, d.Ctx(), lockDir, buildreport.ServiceSidecar, sidecar.cmd.Process.Pid)

	assertOneSpoolRoot(t, daemonOpts.ExtraEnv, sidecar)

	w := &World{Daemon: d, Store: store, Sidecar: sidecar}

	// Registered LAST, so t.Cleanup's LIFO unwind runs this FIRST — before
	// the store/sidecar/daemon Stop cleanups above ever touch the
	// processes. It therefore observes exactly the state ruling this
	// GUARANTEE cares about: what died on its OWN, before the test ended,
	// as opposed to what this world's own teardown is about to kill on
	// purpose. Store.Stop (and the deliberate restart in
	// Store.StartSameDB) records the exit as INTENTIONAL by setting
	// Store.stopped; this assertion is the only reader of that bit that
	// treats it as authoritative.
	t.Cleanup(func() {
		if store.Exited() && !store.stopped {
			t.Errorf("e2e: the store exited before test cleanup (%s), and was never stopped by the test:\n%s",
				store.exit.status(), tailStoreLog(t, store))
		}
		if sidecar.Exited() && !sidecar.stopped {
			t.Errorf("e2e: the sidecar exited before test cleanup (%s), and was never stopped by the test",
				sidecar.exit.status())
		}
	})

	return w
}

// ===========================================================================
// Footer waits (participant-gated).
// ===========================================================================

// FooterWatch is a footer stream plus the two participant streams whose
// liveness the footer's connectivity status is drawn from. Its embedded
// *harness.Stream is the footer stream itself, so a wait reads
// footer.Stream; Close tears all three down together.
type FooterWatch struct {
	*harness.Stream[*frontendv1.FooterView]

	host *harness.Stream[*agentreplv1.WatchHostWorkspaceResponse]
	web  *harness.Stream[*agentreplv1.WatchWebWorkspaceResponse]
}

// Close ends the footer stream and the participant pair held for it.
func (f *FooterWatch) Close() {
	f.Stream.Close()
	f.host.Close()
	f.web.Close()
}

// WatchFooter opens the workspace's footer stream WITH the two participant
// streams the footer's connectivity truth is driven by, shadowing the
// embedded harness.Daemon method of the same name so every footer wait in
// this suite is participant-gated by construction.
//
// The footer resolver only ever learns that a host or web participant is
// present from server.holdParticipant (daemon/internal/server/streams.go:378),
// which fires on the open/close edges of WatchHostWorkspace and
// WatchWebWorkspace and states the pair to footer.SetParticipants. A test
// that opened only the footer stream held NEITHER hop, so the footer
// correctly drew its disconnected/severed status forever — the status every
// one of this suite's footer waits was implicitly waiting past.
//
// The participant streams are opened BEFORE the footer stream, so the
// footer's first view is already drawn against a live pair, and Close tears
// them down with it: the hold lasts exactly the footer's lifetime.
func (w *World) WatchFooter(ws *workspacev1.WorkspaceRef) *FooterWatch {
	host := w.Daemon.WatchHost(ws)
	web := w.Daemon.WatchWeb(ws)
	return &FooterWatch{Stream: w.Daemon.WatchFooter(ws), host: host, web: web}
}

// ===========================================================================
// Failure artifacts (SPEC.md section B, "Failure artifacts").
// ===========================================================================

// ArtifactsEnv names the directory a failing test copies its structured logs
// into. Unset (the default) means the logs are emitted as bounded tails into
// the test's own t.Log output instead — never nothing.
const ArtifactsEnv = "AGENT_REPL_E2E_ARTIFACTS"

// artifactTailBytes bounds how much of one log a failing test prints when no
// artifacts directory is configured. Sized to hold the whole of an ordinary
// single-turn run log (the largest observed sink in this suite is a few tens
// of kilobytes) while refusing to dump an unbounded file into a CI transcript.
const artifactTailBytes = 64 << 10

// preserveLogsOnFailure keeps the whole world's structured logs — the
// daemon's, every shim's, the store's and the sidecar's — alive past the
// world's teardown, but only when the test FAILED.
//
// Every real sink file — the restart-scoped run log and each per-workspace
// daemon/shim/webapp sink — is created under the state root's logs directory
// (daemon internal/dlog/sink.go createTarget; the paths inside a workspace's
// .claude/emacs/ are symlinks INTO that directory). The state root is a
// per-test temp dir the testing package deletes on the way out, so a failure
// today leaves nothing to read and the next diagnosis has to re-run the test
// with instrumentation bolted on. Collecting the one directory therefore
// collects everything, with no per-test bookkeeping of which workspaces were
// registered.
//
// NewWorld routes the store's and the sidecar's own log files into that same
// directory (they otherwise landed in anonymous t.TempDir()s that vanished
// with the test, leaving a failed run with no store or sidecar log at all),
// so the one sweep below collects them alongside the daemon's sinks.
//
// WITH AN ARTIFACTS DIRECTORY THE STORE'S DATABASE IS KEPT TOO (storeDB and
// its WAL files): the logs say what each writer did, but only the rows say
// what a book held when a page was read from it. It is copied while the store
// still runs -- this cleanup is registered after the store's, so it runs
// first -- which is safe once the test has stopped writing.
func preserveLogsOnFailure(t *testing.T, daemon **harness.Daemon, storeDB string) {
	t.Helper()
	t.Cleanup(func() {
		if !t.Failed() {
			return
		}
		d := *daemon
		if d == nil {
			t.Log("e2e artifacts: the daemon never started, so this world produced no logs to preserve")
			return
		}
		logsDir := filepath.Join(d.StateDir, "logs")
		entries, err := os.ReadDir(logsDir)
		if err != nil {
			t.Logf("e2e artifacts: no logs to preserve from %s: %v", logsDir, err)
			return
		}
		dest := ""
		if root := os.Getenv(ArtifactsEnv); root != "" {
			dest = filepath.Join(root, artifactDirName(t.Name()))
			if err := os.MkdirAll(dest, 0o755); err != nil {
				t.Logf("e2e artifacts: cannot create %s (%v); falling back to log tails", dest, err)
				dest = ""
			}
		}
		for _, entry := range entries {
			if entry.IsDir() || filepath.Ext(entry.Name()) != ".log" {
				continue
			}
			src := filepath.Join(logsDir, entry.Name())
			body, err := os.ReadFile(src)
			if err != nil {
				t.Logf("e2e artifacts: read %s: %v", src, err)
				continue
			}
			if dest != "" {
				if err := os.WriteFile(filepath.Join(dest, entry.Name()), body, 0o644); err != nil {
					t.Logf("e2e artifacts: write %s: %v", filepath.Join(dest, entry.Name()), err)
				}
				continue
			}
			t.Logf("e2e artifacts: %s (last %d bytes of %d):\n%s", entry.Name(), min(len(body), artifactTailBytes), len(body), tailBytes(body, artifactTailBytes))
		}
		if dest != "" {
			for _, suffix := range []string{"", "-wal", "-shm"} {
				src := storeDB + suffix
				body, err := os.ReadFile(src)
				if errors.Is(err, fs.ErrNotExist) {
					continue
				}
				if err != nil {
					t.Logf("e2e artifacts: read %s: %v", src, err)
					continue
				}
				if err := os.WriteFile(filepath.Join(dest, filepath.Base(src)), body, 0o644); err != nil {
					t.Logf("e2e artifacts: write %s: %v", filepath.Join(dest, filepath.Base(src)), err)
				}
			}
			t.Logf("e2e artifacts: structured logs and the store database preserved under %s", dest)
		}
	})
}

// tailBytes answers the last limit bytes of body, cut forward to the next
// newline so the tail never opens mid-record and read as a broken JSONL line.
func tailBytes(body []byte, limit int) string {
	if len(body) <= limit {
		return string(body)
	}
	tail := body[len(body)-limit:]
	if i := bytes.IndexByte(tail, '\n'); i >= 0 {
		tail = tail[i+1:]
	}
	return string(tail)
}

// artifactDirName turns a Go test name into one path segment: subtests are
// separated by '/', which would otherwise silently nest (or, for a name with
// no subtest, collide with a sibling's directory).
func artifactDirName(name string) string {
	return strings.NewReplacer("/", "_", string(filepath.Separator), "_").Replace(name)
}

// resolveConfigRoots rewrites the daemon's two account roots to their
// symlink-resolved form, ONCE, before anything reads them.
//
// The sidecar records every cursor under the path IT walked, which is the
// resolved one (on macOS a temp root under /tmp/... is a symlink to
// /private/tmp/...). Every poll in this file compares those recorded paths
// against a project directory derived from these fields with a prefix match,
// so an unresolved root here means the prefix never matches, no cursor is
// ever seen to advance, and driveScenarioToCompletion times out on a turn
// that in fact completed and was durably written.
//
// Rewriting the fields rather than resolving at each use site is deliberate:
// the roots are handed to the sidecar (ConfigRoots below) and read by tests
// through the embedded Daemon, and there is exactly one string only if the
// single source is resolved.
func resolveConfigRoots(t *testing.T, d *harness.Daemon) {
	t.Helper()
	for _, root := range []*string{&d.DefaultConfigDir, &d.MultiRepoConfigDir} {
		resolved, err := resolvedPath(*root)
		if err != nil {
			t.Fatalf("e2e: resolve the daemon's config root %s: %v", *root, err)
		}
		*root = resolved
	}
}

// resolvedPath answers path with every symlink resolved. A path that does not
// exist is a LOUD error rather than the path itself: the only caller resolves
// roots the daemon has already created, so a missing one is a defect, not a
// case to paper over.
func resolvedPath(path string) (string, error) {
	real, err := filepath.EvalSymlinks(path)
	if err != nil {
		return "", err
	}
	return real, nil
}

// tailStoreLog reads back the store's own log file for the failure message
// above, so a test that trips this assertion does not have to go find the
// log file itself to learn why the store died.
func tailStoreLog(t *testing.T, s *Store) string {
	t.Helper()
	body, err := os.ReadFile(s.LogPath)
	if err != nil {
		return "(no store log: " + err.Error() + ")"
	}
	return string(body)
}

// shortSocketPath mints a short-enough UDS path directly under the OS temp
// dir. t.TempDir() encodes the whole test name, which blows the ~103-byte
// sockaddr_un budget for this suite's longer test names.
func shortSocketPath(t *testing.T, tag string) string {
	t.Helper()
	buf := make([]byte, 4)
	if _, err := rand.Read(buf); err != nil {
		t.Fatalf("e2e: random socket suffix: %v", err)
	}
	p := filepath.Join(os.TempDir(), fmt.Sprintf("are2e-%s-%s.sock", tag, hex.EncodeToString(buf)))
	t.Cleanup(func() { _ = os.Remove(p) })
	return p
}

// shortStateRoot mints a state root short enough to hold a shim's unix socket
// (the 103-byte path cap), the same way harness.StartDaemon mints its own —
// t.TempDir() encodes the whole test name and blows that budget for this
// suite's longer names.
//
// THE ROOT IS A CHILD OF THIS WORLD'S OWN DIRECTORY, NEVER /tmp ITSELF, and
// that is the whole point of the extra level. harness.StartDaemon derives the
// kernel-lock directory as a SIBLING of the state root
// (`filepath.Join(filepath.Dir(d.StateDir), "locks")`) precisely so every
// daemon over one state root probes one set of locks and a successor finds the
// lock its predecessor's surviving shim still holds. Handing it a bare `/tmp/<short>` as the state root
// made that sibling `/tmp/locks` — ONE directory shared by every world in the
// package, by every concurrent `go test` run, and by every other checkout on
// the box, none of which the harness ever cleans: an observed 7676 lock files,
// 3871 of them `workspace-<8 hex>.lock` in a 32-bit key space, accumulated in
// about an hour of runs. Nesting the root one level down makes the lock
// directory `<world>/locks` — per-world, still shared across the
// restarts that reuse the root (which is the invariant it exists for), and
// removed with the root at cleanup.
func shortStateRoot(t *testing.T) string {
	t.Helper()
	// Under the harness's owner-locked run root: removed at cleanup, and
	// reclaimed by the next run if this one dies before cleanup runs.
	world := harness.ShortTempDir(t)
	dir := filepath.Join(world, "state")
	if err := os.MkdirAll(dir, 0o755); err != nil {
		t.Fatalf("e2e: mkdir a short state root: %v", err)
	}
	return dir
}

// ===========================================================================
// Child-process exit tracking, shared by Store and Sidecar.
// ===========================================================================

// reapGrace bounds the wait for the kernel to reap a process that has already
// been sent SIGKILL. SIGKILL cannot be caught, blocked or ignored, so this is
// not a shutdown budget at all — it covers only the scheduling of an already
// doomed process, which every observed run completes in single-digit
// milliseconds. It is deliberately far below DefaultTimeout: a process still
// unreaped after this is a fault to REPORT, never something to keep waiting
// on, because the unbounded wait it replaces is what turned one failing
// subtest into a 45-minute suite timeout.
const reapGrace = 2 * time.Second

// processExit records one child process's exit exactly once and lets any
// number of observers ask about it.
//
// Readiness is a CLOSED channel plus a stored error, never a value delivered
// down a buffered channel. That distinction is the whole point: a
// `done chan error` of capacity one is DRAINED by the first receive, so the
// non-disturbing `exited()` probe would consume the very exit a later Stop
// needs to see, and that Stop would then wait on an empty channel forever. A
// closed channel answers every observer, in any order, any number of times.
type processExit struct {
	done chan struct{}
	err  error // written once, before done is closed; read only after it is
}

// watchProcess reaps cmd in the background and answers its exit through the
// returned processExit. Exactly one cmd.Wait is ever issued per process.
func watchProcess(cmd *exec.Cmd) *processExit {
	p := &processExit{done: make(chan struct{})}
	go func() {
		p.err = cmd.Wait()
		close(p.done)
	}()
	return p
}

// exited reports whether the process has already left, without disturbing it
// and without consuming the answer.
func (p *processExit) exited() bool {
	select {
	case <-p.done:
		return true
	default:
		return false
	}
}

// status answers how the process left, for a failure message that would
// otherwise say only THAT it is gone. A process killed by a signal writes no
// exit record of its own, so this is the only evidence such a death leaves.
func (p *processExit) status() string {
	if !p.exited() {
		return "still running"
	}
	if p.err == nil {
		return "exit status 0"
	}
	return p.err.Error()
}

// awaitWithin waits at most budget for the process to leave, and reports
// whether it did. Every teardown wait in this file goes through here, so no
// teardown path can block a test run indefinitely.
func (p *processExit) awaitWithin(budget time.Duration) bool {
	select {
	case <-p.done:
		return true
	case <-time.After(budget):
		return false
	}
}

// stopProcess is the one orderly stop every e2e-owned child process gets:
// SIGTERM, a bounded wait, then SIGKILL and a SECOND BOUNDED wait. Neither
// wait is unbounded, so a child that ignores both signals fails its test
// rather than hanging the whole suite until the go-test alarm.
//
// Every Signal and Kill error is surfaced. os.ErrProcessDone is the one
// benign case — the process left on its own between the probe and the signal
// — and even then the reap is still confirmed rather than assumed; any other
// error is a real fault and fails the test.
//
// A STOP THAT FAILS CARRIES THE CHILD'S OWN LOG TAIL (2026-10-03): the store
// twice missed its SIGTERM bound in one full-suite run, and the failure said
// only that -- the log that timestamps its drain, checkpoint and close was in
// a temp directory deleted with the test.
func stopProcess(t *testing.T, name string, cmd *exec.Cmd, exit *processExit, logPath string) {
	t.Helper()
	if cmd.Process == nil || exit.exited() {
		return
	}
	if err := cmd.Process.Signal(syscall.SIGTERM); err != nil && !errors.Is(err, os.ErrProcessDone) {
		t.Errorf("e2e: SIGTERM %s: %v", name, err)
	}
	if exit.awaitWithin(DefaultTimeout) {
		return
	}
	if err := cmd.Process.Kill(); err != nil && !errors.Is(err, os.ErrProcessDone) {
		t.Errorf("e2e: SIGKILL %s: %v", name, err)
	}
	if !exit.awaitWithin(reapGrace) {
		t.Errorf("e2e: %s was still unreaped %s after SIGKILL, itself %s after SIGTERM; its log ends:\n%s", name, reapGrace, DefaultTimeout, logTailOf(logPath))
		return
	}
	t.Errorf("e2e: %s did not exit within %s of SIGTERM; its log ends:\n%s", name, DefaultTimeout, logTailOf(logPath))
}

// stopLogTailBytes bounds the log a failed stop prints: enough for the last
// few dozen records, which span the stop.
const stopLogTailBytes = 8 << 10

// logTailOf answers the end of a child's log file, or why it cannot.
func logTailOf(path string) string {
	body, err := os.ReadFile(path)
	if err != nil {
		return fmt.Sprintf("(cannot read %s: %v)", path, err)
	}
	return tailBytes(body, stopLogTailBytes)
}

// ===========================================================================
// Store: the real shim-store, restartable on the same socket + database, for
// the degraded-state family's real outage window (ruling 2).
// ===========================================================================

// Store is one running real shim-store.
type Store struct {
	t       *testing.T
	bin     string
	Socket  string
	DBPath  string
	LogPath string
	// lockDir is AGENT_REPL_LOCK_DIR for this store, kept so StartSameDB
	// restarts the same process onto the same private dir rather than
	// re-deriving one.
	lockDir string
	Client  storev1connect.ShimStoreClient

	cmd     *exec.Cmd
	exit    *processExit
	stopped bool
}

// startStore launches the store on a named socket + database and blocks
// until a real rpc against it succeeds — readiness is the store's own
// statement, never an elapsed duration. lockDir is its AGENT_REPL_LOCK_DIR:
// the daemon's own lock dir, so the build report the store writes at boot is
// the one the daemon's deploy reads.
func startStore(t *testing.T, socket, dbPath, logPath, lockDir string) *Store {
	t.Helper()
	bin := requireStoreBinary(t)
	if err := os.MkdirAll(filepath.Dir(dbPath), 0o755); err != nil {
		t.Fatalf("e2e: mkdir %s: %v", filepath.Dir(dbPath), err)
	}
	if lockDir == "" {
		t.Fatal("e2e: the store needs the world's lock dir; an empty one would write its build report over the owner's own in ~/.cache/agent-repl/run")
	}
	cmd := exec.Command(bin, "--socket", socket, "--db", dbPath, "--log", logPath)
	cmd.Env = append(os.Environ(),
		"AGENT_REPL_STORE_SOCKET="+socket,
		"AGENT_REPL_FORBID_VENDOR_CALLS=1",
		// The world's lock dir keeps the boot's build-report write
		// (agentrepl/logging/buildreport) out of the owner's real
		// ~/.cache/agent-repl/run, and in the one the daemon's deploy reads.
		"AGENT_REPL_LOCK_DIR="+lockDir,
	)
	cmd.Env = append(cmd.Env, coverageEnv(t, "shim-store")...)
	cmd.Stdout = os.Stderr
	cmd.Stderr = os.Stderr
	if err := cmd.Start(); err != nil {
		t.Fatalf("e2e: start store: %v", err)
	}
	// The store's own --log path lives under the daemon's state root (logsDir),
	// so this process's argv NAMES the state directory — the exact key
	// harness.Daemon.ReapStrays uses to find the shims a dead daemon leaked.
	// The store is not a stray: this test starts it, stops it, and asserts it
	// was still running at the end. Declaring it spares it from every daemon's
	// reap, including a cold-booted successor's on the same state root.
	harness.SpareFromStrayReaping(t, cmd.Process.Pid)
	s := &Store{
		t:       t,
		bin:     bin,
		Socket:  socket,
		DBPath:  dbPath,
		LogPath: logPath,
		lockDir: lockDir,
		Client:  storeClient(socket),
		cmd:     cmd,
		exit:    watchProcess(cmd),
	}
	t.Cleanup(func() {
		if !s.Exited() {
			s.Stop()
		}
	})
	s.awaitReady()
	return s
}

func storeClient(socket string) storev1connect.ShimStoreClient {
	return storev1connect.NewShimStoreClient(udsHTTPClient(socket), "http://store")
}

func udsHTTPClient(socket string) *http.Client {
	return &http.Client{
		Transport: &http.Transport{
			DialContext: func(ctx context.Context, _, _ string) (net.Conn, error) {
				return (&net.Dialer{}).DialContext(ctx, "unix", socket)
			},
		},
	}
}

func (s *Store) awaitReady() {
	s.t.Helper()
	ctx, cancel := context.WithTimeout(context.Background(), DefaultTimeout)
	defer cancel()
	ticker := time.NewTicker(pollInterval)
	defer ticker.Stop()
	for {
		if _, err := s.Client.GetSidecarCursors(ctx, connect.NewRequest(&storev1.GetSidecarCursorsRequest{})); err == nil {
			return
		}
		select {
		case <-s.exit.done:
			s.t.Fatalf("e2e: store exited before readiness (%s):\n%s", s.exit.status(), tailStoreLog(s.t, s))
		case <-ctx.Done():
			s.t.Fatalf("e2e: store never answered GetSidecarCursors within %s; process is %s:\n%s",
				DefaultTimeout, s.exit.status(), tailStoreLog(s.t, s))
		case <-ticker.C:
		}
	}
}

// Exited reports whether the store process has already left, without
// disturbing it.
func (s *Store) Exited() bool { return s.exit.exited() }

// Stop sends SIGTERM and waits for the store to leave. It is ruling 2's real
// degraded-state control: a test calls this while a session is live, asserts
// the daemon states the shim-degraded fact, then calls StartSameDB to prove
// recovery. No test ever fabricates a degraded-state fact — this is the only
// legitimate way to produce a real one.
func (s *Store) Stop() {
	s.t.Helper()
	if s.stopped {
		return
	}
	s.stopped = true
	stopProcess(s.t, "store", s.cmd, s.exit, s.LogPath)
	if err := os.Remove(s.Socket); err != nil && !errors.Is(err, os.ErrNotExist) {
		s.t.Errorf("e2e: remove store socket %s: %v", s.Socket, err)
	}
}

// StartSameDB relaunches the store on the SAME socket and database path this
// Store was minted with, and waits for it to become ready again. Pairs with
// Stop for ruling 2's real outage window.
func (s *Store) StartSameDB(t *testing.T) {
	t.Helper()
	cmd := exec.Command(s.bin, "--socket", s.Socket, "--db", s.DBPath, "--log", s.LogPath)
	cmd.Env = append(os.Environ(),
		"AGENT_REPL_STORE_SOCKET="+s.Socket,
		"AGENT_REPL_FORBID_VENDOR_CALLS=1",
		"AGENT_REPL_LOCK_DIR="+s.lockDir,
	)
	cmd.Env = append(cmd.Env, coverageEnv(t, "shim-store")...)
	cmd.Stdout = os.Stderr
	cmd.Stderr = os.Stderr
	if err := cmd.Start(); err != nil {
		t.Fatalf("e2e: restart store: %v", err)
	}
	// A restart is a NEW pid; the exemption is per-pid.
	harness.SpareFromStrayReaping(t, cmd.Process.Pid)
	s.cmd = cmd
	s.stopped = false
	s.exit = watchProcess(cmd)
	t.Cleanup(func() {
		if !s.Exited() {
			s.Stop()
		}
	})
	s.awaitReady()
}

// Cursors answers the store's current durable sidecar cursors.
func (s *Store) Cursors(t *testing.T, ctx context.Context) []*storev1.CursorState {
	t.Helper()
	resp, err := s.Client.GetSidecarCursors(ctx, connect.NewRequest(&storev1.GetSidecarCursorsRequest{}))
	if err != nil {
		t.Fatalf("e2e: GetSidecarCursors: %v", err)
	}
	if f := resp.Msg.GetFailure(); f != nil {
		t.Fatalf("e2e: GetSidecarCursors failed: %v", f)
	}
	return resp.Msg.GetSuccess().GetCursors()
}

// ===========================================================================
// Sidecar: the real shim-claude-sidecar, watching the daemon's two account
// roots for the vendor-shaped files the real (--fake) shim writes.
// ===========================================================================

// Sidecar is one running real shim-claude-sidecar.
type Sidecar struct {
	t       *testing.T
	LogPath string
	// SpoolRoot is the tree this sidecar was told to glob. Kept on the
	// struct purely so NewWorld's self-check can compare it against what the
	// daemon hands its shims, rather than trusting the two call sites to
	// keep quoting the same variable.
	SpoolRoot string
	// bin and args are exactly what this sidecar was launched with, kept so a
	// Restart relaunches the same process rather than a re-derived one.
	bin string
	// lockDir is AGENT_REPL_LOCK_DIR, kept so a Restart reuses the same
	// private dir rather than re-deriving one.
	lockDir string
	args    []string
	cmd     *exec.Cmd
	exit    *processExit
	stopped bool
}

// assertOneSpoolRoot fails the world's construction unless the spool root
// shimEnv (the daemon's ExtraEnv, which reaches every shim it spawns)
// exports is EXACTLY the tree the sidecar
// globs. This is a self-check of the harness, not of any subject: getting it
// wrong makes every spool-dependent test fail in the same mystifying way (a
// turn that completed but whose task output the sidecar never files), and
// makes an unset value silently target the developer's real
// /tmp/claude-<uid>.
func assertOneSpoolRoot(t *testing.T, shimEnv []string, s *Sidecar) {
	t.Helper()
	const key = "AGENT_REPL_FAKE_SPOOL_ROOT="
	var exported string
	found := false
	for _, kv := range shimEnv {
		if strings.HasPrefix(kv, key) {
			exported, found = strings.TrimPrefix(kv, key), true
		}
	}
	if !found {
		t.Fatalf("e2e: the daemon exports no %s to its shims, so the fake would spool into the real vendor location", strings.TrimSuffix(key, "="))
	}
	if exported != s.SpoolRoot {
		t.Fatalf("e2e: the shims spool into %s but the sidecar globs %s; they must be one tree", exported, s.SpoolRoot)
	}
}

type sidecarOpts struct {
	StoreSocket string
	ConfigRoots []string
	SpoolRoot   string
	LogPath     string
	// StateDir is the state root the DAEMON runs on, and therefore the one it
	// exports into every shim it spawns. The shim's identity records live under
	// it, and they are the only link between a vendor session id a `/clear`
	// rotated and the conversation's original one; a sidecar pointed at any
	// other root would book a rotated transcript under a second id, which the
	// store refuses.
	StateDir string
	// Staleness is the LOST-policy window set. Zero fields are omitted from
	// the argv entirely, leaving the sidecar's own production defaults.
	Staleness SidecarStaleness
	// LockDir is the sidecar's AGENT_REPL_LOCK_DIR: where it writes its build
	// report. It is the daemon's own lock dir, so the report is the one the
	// daemon's deploy reads. Empty is refused, because the default is the
	// owner's real ~/.cache/agent-repl/run.
	LockDir string
}

// startSidecar launches the sidecar against the daemon's own two account
// roots (d.DefaultConfigDir, d.MultiRepoConfigDir), which is ALREADY the
// per-account CLAUDE_CONFIG_DIR the daemon hands each spawned shim — no new
// harness plumbing is needed to make the sidecar watch the right trees.
func startSidecar(t *testing.T, bin string, opts sidecarOpts) *Sidecar {
	t.Helper()
	if opts.StateDir == "" {
		// An empty --state-dir resolves to $AGENT_REPL_STATE_DIR and then to
		// $HOME/.claude-emacs: a test sidecar would read the DEVELOPER'S live
		// identity records. Refused rather than defaulted.
		t.Fatal("e2e: the sidecar needs the world's state root; an empty --state-dir would point it at the developer's own ~/.claude-emacs")
	}
	if opts.LockDir == "" {
		t.Fatal("e2e: the sidecar needs the world's lock dir; an empty one would write its build report over the owner's own in ~/.cache/agent-repl/run")
	}
	args := []string{
		"--store-socket", opts.StoreSocket,
		"--config-roots", strings.Join(opts.ConfigRoots, ","),
		"--spool-root", opts.SpoolRoot,
		"--state-dir", opts.StateDir,
		"--log", opts.LogPath,
		"--poll-interval", "50ms",
		"--rescan-interval", "200ms",
		// The unclaimed-spool hold defaults to 60s in production, which is the
		// whole budget of this suite: a subject that is not ABOUT the hold
		// would simply time out inside it. The degraded-state area sets its
		// own window (StoreOutageWindow) at the daemon-wait layer; this
		// shrinks the sidecar's OWN internal hold to match the sidecar
		// integration suite's own precedent (UnownedSpoolWindow: 200ms).
		"--unowned-spool-window", unownedSpoolWindow.String(),
	}
	// The LOST-policy windows, appended LAST so a test that bought one wins
	// over the default above (the sidecar's own flag parse takes the last
	// occurrence). A ZERO field appends nothing at all: the sidecar refuses a
	// zero window at bootstrap, and "unset" here means "keep the production
	// default", never "pass a zero".
	for _, w := range []struct {
		flag  string
		value time.Duration
	}{
		{"--stale-grace", opts.Staleness.Grace},
		{"--stale-shell-silence", opts.Staleness.ShellSilence},
		{"--stale-agent-silence", opts.Staleness.AgentSilence},
		{"--stale-workflow-silence", opts.Staleness.WorkflowSilence},
		{"--unowned-spool-window", opts.Staleness.UnownedSpool},
	} {
		if w.value > 0 {
			args = append(args, w.flag, w.value.String())
		}
	}
	lockDir := opts.LockDir
	cmd := exec.Command(bin, args...)
	cmd.Env = append(os.Environ(),
		"AGENT_REPL_STORE_SOCKET="+opts.StoreSocket,
		"AGENT_REPL_FORBID_VENDOR_CALLS=1",
		// The world's lock dir keeps the boot's build-report write
		// (agentrepl/logging/buildreport) out of the owner's real
		// ~/.cache/agent-repl/run, and in the one the daemon's deploy reads.
		"AGENT_REPL_LOCK_DIR="+lockDir,
	)
	cmd.Env = append(cmd.Env, coverageEnv(t, "shim-claude-sidecar")...)
	cmd.Stdout = os.Stderr
	cmd.Stderr = os.Stderr
	if err := cmd.Start(); err != nil {
		t.Fatalf("e2e: start sidecar: %v", err)
	}
	// Same reasoning as startStore's: --log puts the state root in this
	// process's argv, and the sidecar is the test's own, never a daemon stray.
	harness.SpareFromStrayReaping(t, cmd.Process.Pid)
	s := &Sidecar{t: t, bin: bin, args: args, lockDir: lockDir, LogPath: opts.LogPath, SpoolRoot: opts.SpoolRoot, cmd: cmd, exit: watchProcess(cmd)}
	t.Cleanup(s.Stop)
	return s
}

// ServiceBinaries names this world's real store and sidecar binaries, for a
// daemon started over the same state root by hand (a cold boot, a relaunch):
// its deploy stages copies of them and lets their own build reports stand, as
// the world's first daemon does.
func (w *World) ServiceBinaries() harness.ServiceBinaries {
	return harness.ServiceBinaries{Store: w.Store.bin, Sidecar: w.Sidecar.bin}
}

// SuccessorOpts is the harness.Opts for a daemon started BY HAND over this
// world's roots — a crash restart, a cold boot, a relaunch — restating every
// root and every piece of environment the world's own daemon handed its shims.
// A caller adds only what is its own (a timeout, a self repo, account roots).
//
// ONE DEFINITION, BECAUSE EVERY HAND-ROLLED COPY DRIFTED. A successor inherits
// none of NewWorld's environment, and each site that restated it by hand
// forgot a different piece: without AGENT_REPL_SHIM_LOCK_BIN the successor's
// shims resolve the lock holder under an e2e $HOME that has none, the kernel
// claim dies `spawn ENOENT`, and the start is refused `lock_holder_unavailable`
// (once `conversation_owned`, for a conversation nobody holds); without the fake spool root they spool where
// the sidecar never globs.
func (w *World) SuccessorOpts(t *testing.T) harness.Opts {
	t.Helper()
	return harness.Opts{
		StateDir:        w.StateDir,
		ShimNode:        requireNode(t),
		ShimMain:        requireShimBundle(t),
		StoreSocket:     w.Store.Socket,
		ServiceBinaries: w.ServiceBinaries(),
		ExtraEnv:        worldDaemonEnv([]string{"AGENT_REPL_LOCK_DIR=" + w.LockDir}, w.Sidecar.SpoolRoot, requireLockBinary(t)),
	}
}

// awaitBuildReport waits until service's build report in lockDir names pid —
// the process's own statement of its build, written at boot — bounded by
// ctx, never a fixed sleep.
func awaitBuildReport(t *testing.T, ctx context.Context, lockDir, service string, pid int) {
	t.Helper()
	ticker := time.NewTicker(5 * time.Millisecond)
	defer ticker.Stop()
	for {
		report, found, err := buildreport.Read(lockDir, service)
		if err != nil {
			t.Fatalf("e2e: read the %s build report: %v", service, err)
		}
		if found && report.PID == pid {
			return
		}
		select {
		case <-ticker.C:
		case <-ctx.Done():
			t.Fatalf("e2e: %s (pid %d) never reported its build into %s: %v", service, pid, lockDir, ctx.Err())
		}
	}
}

// Restart stops this sidecar and launches a new one on EXACTLY the argv the
// world built — same store socket, same config roots, same spool root, same
// windows. It is the lever for the sidecar's BOOT sweep (cycle.go's
// `bootSwept` gate runs `Tracker.BootSweep` once per PROCESS), which is the
// only way an e2e test can reach the `swept_up` arm: a leftover spool on disk
// is re-discovered by the new process and judged against the machine's boot
// time.
//
// Cursor recovery makes this safe to do mid-test: the new process recovers
// its read positions from the store before it reads a byte (cycle.go's
// beginCycle), so nothing already ingested is ingested twice.
func (s *Sidecar) Restart(t *testing.T) {
	t.Helper()
	s.Stop()
	cmd := exec.Command(s.bin, s.args...)
	cmd.Env = append(os.Environ(),
		"AGENT_REPL_STORE_SOCKET="+storeSocketOf(s.args),
		"AGENT_REPL_FORBID_VENDOR_CALLS=1",
		"AGENT_REPL_LOCK_DIR="+s.lockDir,
	)
	cmd.Env = append(cmd.Env, coverageEnv(t, "shim-claude-sidecar")...)
	cmd.Stdout = os.Stderr
	cmd.Stderr = os.Stderr
	if err := cmd.Start(); err != nil {
		t.Fatalf("e2e: restart sidecar: %v", err)
	}
	// A restart is a NEW pid; the exemption is per-pid.
	harness.SpareFromStrayReaping(t, cmd.Process.Pid)
	s.cmd = cmd
	// The new process is the one the end-of-test guarantee now judges, and it
	// has not been stopped.
	s.stopped = false
	s.exit = watchProcess(cmd)
}

// storeSocketOf reads the socket back out of a sidecar's own argv, so a
// restart cannot drift from the arguments the world actually launched with.
func storeSocketOf(args []string) string {
	for i, a := range args {
		if a == "--store-socket" && i+1 < len(args) {
			return args[i+1]
		}
	}
	return ""
}

// Stop sends SIGTERM and waits for the sidecar to leave.
func (s *Sidecar) Stop() {
	s.t.Helper()
	if s.stopped {
		return
	}
	s.stopped = true
	stopProcess(s.t, "sidecar", s.cmd, s.exit, s.LogPath)
}

// Log reads every structured-log record the sidecar has written so far.
func (s *Sidecar) Log(t *testing.T) []harness.LogRecord {
	t.Helper()
	return harness.ReadLog(t, s.LogPath)
}

// Exited reports whether the sidecar process has already left, without
// disturbing it.
func (s *Sidecar) Exited() bool { return s.exit.exited() }

// ===========================================================================
// A process that exits before test cleanup fails the test loudly (SPEC.md
// section B, "Lifecycle"). harness.Daemon already tracks this for the
// daemon; NewWorld registers the same check for Store and Sidecar
// AUTOMATICALLY, with no action required from a test — see NewWorld's own
// doc comment for the exact guarantee and how a deliberate Store.Stop is
// told apart from an unexpected exit (the Store.stopped / Sidecar.stopped
// bit each Stop method sets before it signals the process).
// ===========================================================================

// RequireNoUnexpectedExit asserts, RIGHT NOW rather than at test cleanup,
// that the store and the sidecar are both still running. It exists for a
// test that wants to pin "still alive" at some specific mid-test point
// (for example: immediately before provoking the very outage the
// degraded-state family's test IS about, to prove the outage is what
// causes the failure ordering, not something that already happened). It is
// a convenience on top of the automatic end-of-test guarantee NewWorld
// always registers, never a substitute for it — that one requires no call
// and has no opt-out.
func (w *World) RequireNoUnexpectedExit(t *testing.T) {
	t.Helper()
	if w.Store.Exited() && !w.Store.stopped {
		t.Errorf("e2e: the store has already exited, and was never stopped by the test")
	}
	if w.Sidecar.Exited() && !w.Sidecar.stopped {
		t.Errorf("e2e: the sidecar has already exited, and was never stopped by the test")
	}
}

// ===========================================================================
// Submitting a real prompt and awaiting its terminal, durably.
// ===========================================================================

// e2ePromptOrigin is the PromptOrigin every prompt this suite submits
// carries. UNSPECIFIED is refused at once (endpoint_submit_prompt.proto), and
// this suite's prompts are not attributable to any one particular Emacs send
// site, so the plain "user sent" arm is the correct choice for all of them.
const e2ePromptOrigin = conversationv1.PromptOrigin_PROMPT_ORIGIN_USER_SENT

// SubmitPrompt submits a real prompt through the daemon's SubmitPrompt rpc
// and answers the minted turn id. It fails the test if the submission was
// refused or did not mint a turn (a command-panel/command-acted/
// command-refused arm) — callers that expect one of those other arms should
// call the client's SubmitPrompt directly instead.
func SubmitPrompt(t *testing.T, w *World, ws *workspacev1.WorkspaceRef, text string) *conversationv1.TurnId {
	t.Helper()
	resp, err := w.Client().SubmitPrompt(w.Ctx(), connect.NewRequest(&agentreplv1.SubmitPromptRequest{
		Workspace: ws,
		Said: &conversationv1.UserSaid{Content: &conversationv1.UserContent{Blocks: []*conversationv1.UserContentBlock{
			{Block: &conversationv1.UserContentBlock_Text{Text: &conversationv1.TextBlock{Text: text}}},
		}}},
		IdempotencyKey: newIdempotencyKey(t),
		Origin:         e2ePromptOrigin,
	}))
	if err != nil {
		t.Fatalf("SubmitPrompt(%q): %v", text, err)
	}
	turn := resp.Msg.GetSuccess().GetTurn().GetTurn()
	if turn.GetValue() == "" {
		t.Fatalf("SubmitPrompt(%q) = %v, want a minted turn", text, resp.Msg)
	}
	return turn
}

func newIdempotencyKey(t *testing.T) string {
	t.Helper()
	buf := make([]byte, 16)
	if _, err := rand.Read(buf); err != nil {
		t.Fatalf("e2e: random idempotency key: %v", err)
	}
	return hex.EncodeToString(buf)
}

// AwaitTurnEnded opens the workspace's root feed, watches it until the given
// turn's FeedTurnEnded row arrives, and answers that row.
func AwaitTurnEnded(t *testing.T, w *World, ws *workspacev1.WorkspaceRef, turn *conversationv1.TurnId) *frontendv1.FeedRow {
	t.Helper()
	return awaitFeedRowOn(t, w, w.Client(), ws, "turn "+turn.GetValue()+" to end", endsTurn(turn))
}

// endsTurn matches TURN's FeedTurnEnded row.
func endsTurn(turn *conversationv1.TurnId) func(*frontendv1.FeedRow) bool {
	return func(row *frontendv1.FeedRow) bool {
		return row.GetTurn().GetValue() == turn.GetValue() && row.GetTurnEnded() != nil
	}
}

// turnEndedRow finds the given turn's terminal row in a feed snapshot, or
// fails the test loudly — every hook test needs this to confirm hook
// activity never froze or misclassified the turn's own outcome.
func turnEndedRow(t *testing.T, rows []*frontendv1.FeedRow, turn *conversationv1.TurnId) *frontendv1.FeedTurnEnded {
	t.Helper()
	ends := endsTurn(turn)
	for _, row := range rows {
		if ends(row) {
			return row.GetTurnEnded()
		}
	}
	t.Fatalf("no FeedTurnEnded row for turn %s in %d rows", turn.GetValue(), len(rows))
	return nil
}

// awaitFeedRow opens ws's root feed and answers the first row satisfying
// pred (awaitFeedRowOn, against the world's own daemon).
func awaitFeedRow(t *testing.T, w *World, ws *workspacev1.WorkspaceRef, what string, pred func(*frontendv1.FeedRow) bool) *frontendv1.FeedRow {
	t.Helper()
	return awaitFeedRowOn(t, w, w.Client(), ws, what, pred)
}

// awaitFeedRowOn opens ws's root feed on CLIENT and answers the first row
// satisfying pred: it checks the already-materialized page first (a settled
// row from an earlier driveScenarioToCompletion/AwaitTurnEnded call on this
// workspace is normally already in the page by the time a caller reaches
// here), falling back to watching the tail otherwise. CLIENT is the world's
// own daemon, or a successor a handover moved the workspace to. Bound by the
// watch stream's own DefaultTimeout via harness.AwaitView; never sleeps.
func awaitFeedRowOn(t *testing.T, w *World, client agentreplv1connect.AgentReplClient, ws *workspacev1.WorkspaceRef, what string, pred func(*frontendv1.FeedRow) bool) *frontendv1.FeedRow {
	t.Helper()
	opened, err := client.OpenFeed(w.Ctx(), connect.NewRequest(&agentreplv1.OpenFeedRequest{Workspace: ws}))
	if err != nil {
		t.Fatalf("OpenFeed: %v", err)
	}
	success := opened.Msg.GetSuccess()
	if success == nil {
		t.Fatalf("OpenFeed = %v, want success", opened.Msg)
	}
	for _, row := range success.GetPage().GetSuccess().GetRows() {
		if pred(row) {
			return row
		}
	}
	stream := w.WatchFeedOn(client, success.GetWatch())
	defer stream.Close()
	return harness.AwaitView(t, w.Ctx(), stream, what, pred)
}

// driveScenarioToCompletion submits a real "!"+scenario prompt (or a
// caller-supplied full prompt string for scenarios with their own documented
// form, e.g. "!compact [summary]"), waits for the turn's terminal
// FeedTurnEnded row, and returns once the sidecar has DURABLY written the
// resulting facts to the real store — proven by the store's own
// GetSidecarCursors read verb showing a cursor whose path is under this
// workspace's project directory advance past its pre-prompt baseline, never
// by a sleep.
//
// A test that needs pre-restart durable rows (the ColdBootReadsReplayFromStore
// family) calls this BEFORE simulating the daemon bounce, then restarts the
// daemon against the same state root/store socket and asserts on replay.
//
// configDir is the account root the workspace routes through
// (w.DefaultConfigDir or w.MultiRepoConfigDir) — the caller names it because
// only the caller knows which account root registered the workspace.
func driveScenarioToCompletion(t *testing.T, w *World, ws *workspacev1.WorkspaceRef, configDir, scenario string) *conversationv1.TurnId {
	t.Helper()
	ctx, cancel := context.WithTimeout(w.Ctx(), DefaultTimeout)
	defer cancel()
	before := w.Store.Cursors(t, ctx)
	baseline := cursorOffsetsUnder(before, harness.ProjectDir(configDir, ws.GetDir()))

	turn := SubmitPrompt(t, w, ws, "!"+scenario)
	AwaitTurnEnded(t, w, ws, turn)

	awaitCursorAdvance(t, w, harness.ProjectDir(configDir, ws.GetDir()), baseline)
	return turn
}

// driveDocumentedPrompt is driveScenarioToCompletion for the scenarios whose
// documented invocation is not a bare "!"+scenario (e.g. "!compact [summary]").
func driveDocumentedPrompt(t *testing.T, w *World, ws *workspacev1.WorkspaceRef, configDir, prompt string) *conversationv1.TurnId {
	t.Helper()
	ctx, cancel := context.WithTimeout(w.Ctx(), DefaultTimeout)
	defer cancel()
	before := w.Store.Cursors(t, ctx)
	baseline := cursorOffsetsUnder(before, harness.ProjectDir(configDir, ws.GetDir()))

	turn := SubmitPrompt(t, w, ws, prompt)
	AwaitTurnEnded(t, w, ws, turn)

	awaitCursorAdvance(t, w, harness.ProjectDir(configDir, ws.GetDir()), baseline)
	return turn
}

func cursorOffsetsUnder(cursors []*storev1.CursorState, projectDir string) map[string]int64 {
	out := map[string]int64{}
	for _, c := range cursors {
		if strings.HasPrefix(c.GetPath(), projectDir) {
			out[c.GetPath()] = c.GetOffset()
		}
	}
	return out
}

// awaitCursorAdvance polls GetSidecarCursors until some cursor under
// projectDir has advanced past its baseline offset (or a cursor under it now
// exists that did not before) — the store's own durable statement that the
// sidecar has committed the turn's resulting facts.
func awaitCursorAdvance(t *testing.T, w *World, projectDir string, baseline map[string]int64) {
	t.Helper()
	awaitCursorAdvanceWithin(t, w, projectDir, baseline, DefaultTimeout)
}

// awaitCursorAdvanceWithin is awaitCursorAdvance on a caller-supplied bound,
// for the one wait whose budget is its OWN measured window rather than this
// suite's ordinary one (hibernation_e2e_test.go's keepAliveObservationWindow).
// It exists because that constant used to bound only the BASELINE read beside
// this call, leaving the wait it documents running on DefaultTimeout — a bound
// stated at a site that was not the bound in force.
func awaitCursorAdvanceWithin(t *testing.T, w *World, projectDir string, baseline map[string]int64, bound time.Duration) {
	t.Helper()
	ctx, cancel := context.WithTimeout(w.Ctx(), bound)
	defer cancel()
	ticker := time.NewTicker(pollInterval)
	defer ticker.Stop()
	for {
		now := cursorOffsetsUnder(w.Store.Cursors(t, ctx), projectDir)
		for path, offset := range now {
			if offset > baseline[path] {
				return
			}
		}
		select {
		case <-ticker.C:
		case <-ctx.Done():
			t.Fatalf("e2e: waiting for the sidecar to durably advance a cursor under %s: %v", projectDir, ctx.Err())
		}
	}
}

// Git is the daemon's own scripted fake (harness.GitWorld, installed on the
// daemon's PATH exactly as daemon/integration installs it — SPEC.md section
// B, "Every external dependency is MOCKED"). This suite never runs a real
// `git` process: every git fact the daemon reads comes from the fixture the
// harness's World(t) mints, reachable off the embedded *harness.Daemon as
// w.Git.
