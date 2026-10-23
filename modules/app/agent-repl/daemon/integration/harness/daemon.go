package harness

import (
	"context"
	"crypto/tls"
	"errors"
	"fmt"
	"maps"
	"net"
	"net/http"
	"os"
	"os/exec"
	"path/filepath"
	"regexp"
	"runtime"
	"slices"
	"strconv"
	"strings"
	"sync"
	"syscall"
	"testing"
	"time"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	"agentrepl/proto/agentrepl/v1/agentreplv1connect"
	workspacev1 "agentrepl/proto/workspace/v1"

	"agent-repld/integration/fakegit"
	"agent-repld/integration/grantedclock"
	"agent-repld/internal/chessboard"
	"agent-repld/internal/cpugov"
	"agent-repld/internal/daemonaddr"
	"agent-repld/internal/newsdigest"
	"agent-repld/internal/procgroup"
	"agent-repld/internal/stateroot"
	"agent-repld/internal/tempdirs"
	"agent-repld/internal/workspace"
	"agent-repld/internal/wsm"

	"connectrpc.com/connect"
	"golang.org/x/net/http2"
)

// DefaultTimeout bounds every wait the harness performs, from one Daemon's
// process start to the last wait a test makes against it. It is a failure
// bound, never a synchronization device.
//
// Sized off teamlead run 8 (443 passing tests: median 0.2s, max 1.7s wall
// time per test, including ordinary boots, restarts on the same state root,
// and hibernation cutoffs): ~3x the observed max leaves headroom for a
// slower machine without letting one red test burn 30s. A wait that is
// structurally different from an ordinary single-daemon test — because it
// chains a SECOND real process boot onto the same context, rather than
// getting its own fresh one — gets its own longer, justified override
// through Opts.Timeout (see HandoverChainTimeout) instead of a bigger
// default for everyone.
//
// IT BOUNDS ONE WAIT, NOT A RUN. The Daemon's own context gets
// runBudgetWaits times this, because a run is a SEQUENCE of waits and a
// budget that only fits one of them makes the last call in a test answer for
// the time every earlier call spent. Every wait the harness makes takes a
// fresh child of this size off that budget (Daemon.waitCtx), so a wait
// bounded by it never means the whole test.
//
// EVERY BOUND HERE IS GRANTED TIME, NEVER WALL TIME (granted.go): a wait is
// charged only the time this binary's processes were not starved of CPU, so
// the measured basis above holds on a saturated host too.
const DefaultTimeout = 5 * time.Second

// runBudgetWaits is how many DefaultTimeout-sized waits a Daemon's own context
// is sized to hold.
//
// DefaultTimeout BOUNDS ONE WAIT; THE DAEMON'S CONTEXT IS A WHOLE-RUN BUDGET,
// and the two were the same number, which made every test's whole run as short
// as its single longest permitted wait. A test that boots a daemon, opens a
// workspace, drives a turn and then asks the daemon to stop makes FOUR waits on
// one context, and the last of them was answering `deadline_exceeded` for
// budget the first three had spent — reporting the stop as the slow step when
// the stop measured 9ms p50 and 15ms max across 104 e2e runs.
//
// Sized off what a whole run actually contains: five DefaultTimeout waits (25s)
// plus one drain.DefaultStandBound stand-down (6s) is 31s, and a run that
// exceeds that is wedged rather than slow. It is a MULTIPLE rather than its own
// constant so an Opts.Timeout override, which widens the per-wait bound for a
// structurally longer run, widens that run's budget in the same proportion —
// which is why the multiple is 7 (35s) rather than 6.2: the stand-down grew
// when it was re-derived as a sum of the shim teardown and the graceful kill it
// contains, and a budget that no longer covers one whole stand-down would make
// the LAST call of a run answer deadline_exceeded for budget the stop spent.
const runBudgetWaits = 7

// HandoverChainTimeout bounds the few tests whose single Daemon context must
// span an entire self-reload handover: a merge landing, the rollout trigger,
// a SECOND real agent-repld's full boot and workspace adoption, and the
// incumbent's own orderly exit — all chained on the ONE budget that started
// ticking at the incumbent's own process start, never a fresh context of its
// own. That is structurally two real process lifecycles, not one, so it gets
// 3x DefaultTimeout rather than sharing the ordinary bound: run 8's observed
// max (1.7s) already covers this chain today, so 15s leaves ample headroom
// for exactly the case that is most likely to grow (a slower deploy chain or
// a slower successor boot) without reintroducing a blanket 30s bound.
const HandoverChainTimeout = 3 * DefaultTimeout

// MergeChainTimeout bounds the tests whose single wait spans a WHOLE merge:
// from the enqueue (or the fixing turn's end) to the terminal row, through
// the rebase, the test gate, the commit and the teardown. That wait is not one
// daemon action but a sequential chain of 36-52 subprocesses (the fake git,
// once per git call, plus the gate's script), so its cost scales with how
// slowly the host spawns a process, and every one of them is on the critical
// path.
//
// MEASURED (2026-10-06, daemon logs of the five merge tests that timed out in
// full runs, `-test.parallel=1 GOMAXPROCS=2` chunks as testrun runs them):
// unloaded, the chain is ~0.1s at ~4ms a git call. Under 24 chunks plus
// `with-cpu-load.sh 24` its max was 2.82s; under 48 chunks plus
// `with-cpu-load.sh 32` (load average ~170 on 16 cores) the passing max was
// 4.22s, and 16 first-iteration runs of
// TestATargetThatMovedBeforeCommittingStartsTheMergeOver were cut by the 5s
// DefaultTimeout while still advancing: every step logged, no gap between
// records over 1.54s, the target fast-forwarded at +3.9-4.95s, the terminal
// row due a few git calls later. That is a bound shorter than the healthy
// chain, not a stall. 3x DefaultTimeout is ~3.5x that 4.22s passing max.
const MergeChainTimeout = 3 * DefaultTimeout

// pollInterval is how often a file-existence wait re-checks. Nothing in the
// harness sleeps to let another party make progress.
const pollInterval = 5 * time.Millisecond

// FakeShimDefaultBuildSHA is the runtime shim_build_sha the fake shim reports
// when FAKESHIM_BUILD_SHA is unset. It duplicates fakeshim's own
// DefaultBuildSHA because the fake is a program, not an importable package;
// the two constants are documented on each other and move together.
const FakeShimDefaultBuildSHA = "fake"

// TemporaryRegistrationEnv is the environment that lets a suite's daemon
// register the directories the suite makes, every one of which lies under the
// run root in /tmp -- a temporary root the registry otherwise refuses (owner
// ruling, 2026-10-06; internal/tempdirs).
//
//   - tempdirs.EnvTestRoot names the RUN ROOT as the one exempt directory. It
//     is honored only because AGENT_REPL_FORBID_VENDOR_CALLS is set too; every
//     other temporary folder is still refused, which the suite can assert.
//   - TMPDIR moves the daemon's own temporary directory one level down, into
//     the run root. The guard counts the process's temporary directory as a
//     root, and an exemption may not BE a root: left at the run root itself
//     (the harness points TMPDIR there), the daemon would refuse to boot.
//     Everything the daemon and its children write as temporary still lands
//     under the run root, so a dead run's reclaim still finds it.
func TemporaryRegistrationEnv(t testing.TB) []string {
	t.Helper()
	if runRoot == "" {
		t.Fatal("harness: no run root; the suite's TestMain must run through harness.MainAt")
	}
	daemonTmp := filepath.Join(runRoot, "daemon-tmp")
	if err := os.MkdirAll(daemonTmp, 0o755); err != nil {
		t.Fatalf("harness: make the daemon's temporary directory %s: %v", daemonTmp, err)
	}
	return []string{"TMPDIR=" + daemonTmp, TemporaryRegistrationRootEnv(runRoot)}
}

// TemporaryRegistrationRootEnv is the ONE spelling of the seam's environment
// entry, for a launcher whose registered directories live under a root of its
// own rather than under the run root -- the Emacs layer's per-test sandbox
// scratch. root must lie strictly beneath a temporary root and must not be
// the daemon's own TMPDIR (tempdirs.New refuses both); a launcher whose root
// IS its TMPDIR moves TMPDIR as TemporaryRegistrationEnv does.
func TemporaryRegistrationRootEnv(root string) string {
	return tempdirs.EnvTestRoot + "=" + root
}

// BuildIdentityEnv is the build-identity environment EVERY daemon this suite
// starts must carry, and the only place the three variables are named. The
// three answer to ONE string, FakeShimDefaultBuildSHA, so the build the
// daemon exports to each shim, the build the fake shim reports back, and the
// build the daemon reads as DEPLOYED cannot disagree:
//
//   - AGENT_REPL_CHECKOUT pins the checkout the daemon resolves its stamps
//     from to a harness-owned tree that carries neither stamp file. Left
//     unpinned, the daemon walks up from its temp binary to the compiled-in
//     source root and reads the HOST's agent-shim/claude/shim/dist/.built-sha
//     — a real git sha written by whatever frontend build ran last — as the
//     shim build it exports, because that file beats SHIM_BUILD_SHA.
//   - SHIM_BUILD_SHA is the identity every shim spawn is stamped with. With
//     no stamp under the pinned checkout it is the only answer.
//   - AGENT_REPL_DEPLOY_STAMP is the deployed build the rollout staleness
//     check compares the session's reported build against. It beats
//     daemon/bin/.built-sha unconditionally.
//
// Disagreement does not fail visibly: the daemon judges every freshly spawned
// fake shim stale, relaunches it, stands the first one down, and every test
// waiting on the fake's control socket times out.
//
// A test that WANTS a mismatch overrides either half through ExtraEnv, which
// StartDaemon appends after this.
func BuildIdentityEnv(checkout string) []string {
	return []string{
		"AGENT_REPL_CHECKOUT=" + checkout,
		"SHIM_BUILD_SHA=" + FakeShimDefaultBuildSHA,
		"AGENT_REPL_DEPLOY_STAMP=" + FakeShimDefaultBuildSHA,
	}
}

// HostNeutralNamingEnv clears the host's workspace-name prefix for a test
// daemon. The prefix is read from the daemon's own environment
// (workspace.PrefixEnv, else workspace.LegacyPrefixEnv) and makes every
// branch "<prefix>/<slug>"; inherited from a developer's shell (e.g.
// CLAUDE_WORKSPACE_PREFIX=ABC) it renames every test branch, so a scenario
// keyed on the bare branch (a scripted rebase conflict) never fires. Both
// spellings are stated EMPTY, which workspace.Prefix reads as unset, and
// os/exec keeps the last value of a repeated key, so this wins over the
// inherited one. A test that wants a prefix sets it through ExtraEnv, which
// StartDaemon appends after this.
func HostNeutralNamingEnv() []string {
	return statedEmpty(workspace.PrefixEnv, workspace.LegacyPrefixEnv)
}

// HostNeutralCheckoutEnv clears the host's explanation-engine checkout for a
// test daemon. A daemon that resolves a checkout (chessboard.EngineDirEnv,
// else chessboard.MultiRepoRootEnv's explanation-engine) builds and starts the
// CEE CLI's widget backend from it with real npm, go and gns; inherited from a
// developer's shell, a chess board drawn in a test would do exactly that.
// EngineDirEnv is stated EMPTY, which the daemon reads as unset, and os/exec
// keeps the last value of a repeated key, so this wins over the inherited
// one. MultiRepoRootEnv needs no clearing: StartDaemon states it as the
// world's own temporary tree, which holds no explanation-engine, so every test
// board resolves to "no checkout" deterministically.
func HostNeutralCheckoutEnv() []string {
	return statedEmpty(chessboard.EngineDirEnv)
}

// HostNeutralPowerEnv pins the power source a test daemon's gate governor
// reads to AC (cpugov.FixedPowerEnv). The power source decides how many merges
// at the front of a queue are worked on -- the top two on AC, the head alone
// on battery -- and a suite must not inherit whether its machine happens to
// be plugged in. A test of the battery window sets it through ExtraEnv, which
// StartDaemon appends after this.
func HostNeutralPowerEnv() []string {
	return []string{cpugov.FixedPowerEnv + "=" + cpugov.PowerAC.String()}
}

// NeutralNewsDigestStartDelay is the news digest's start delay every test
// daemon is given: far past any suite's timeout, so a scheduled run never
// fires inside a test process and no test daemon reads a real source.
const NeutralNewsDigestStartDelay = 24 * time.Hour

// HostNeutralNewsDigestEnv keeps a test daemon's news digest schedule from
// ever running (newsdigest.EnvStartDelay). Left at the production two
// minutes, any test daemon that lives longer -- the e2e suite's shared one --
// fetches the real sources, which the vendor guard refuses, and logs that no
// source could be read. A test of the digest drives it through
// RefreshNewsDigest and sets its own knobs through ExtraEnv, which StartDaemon
// appends after this.
func HostNeutralNewsDigestEnv() []string {
	return []string{newsdigest.EnvStartDelay + "=" + NeutralNewsDigestStartDelay.String()}
}

// statedEmpty answers each key stated EMPTY, the one form every host-neutral
// env takes: a daemon reads an empty variable as unset, and os/exec keeps the
// last value of a repeated key, so a stated-empty key appended after
// os.Environ() wins over the host's.
func statedEmpty(keys ...string) []string {
	out := make([]string, 0, len(keys))
	for _, key := range keys {
		out = append(out, key+"=")
	}
	return out
}

// ServiceBinaries are the real launchd services a world runs beside its
// daemon (Opts.ServiceBinaries). Both or neither.
type ServiceBinaries struct {
	Store, Sidecar string
}

// LockDirFor is the kernel-lock and build-report directory StartDaemon
// redirects a daemon over stateDir to (Daemon.LockDir). A world that starts
// real services before its daemon points their AGENT_REPL_LOCK_DIR here, so
// the build reports they write are the ones the daemon's deploy reads.
func LockDirFor(stateDir string) string {
	return filepath.Join(filepath.Dir(stateDir), "locks")
}

// Opts configures one daemon process.
type Opts struct {
	// StateDir overrides the state root; empty mints a fresh temp one. A root
	// given here keeps its state in wsm.db, and a root whose first daemon ran
	// on in-memory state is refused (see DurableState).
	StateDir string
	// DurableState keeps a freshly minted state root's database in wsm.db.
	//
	// WITHOUT IT A FRESH ROOT'S STATE LIVES IN MEMORY (wsm.EnvTestStateInMemory,
	// owner ruling 2026-10-12): most tests never start a second daemon on their
	// root, and the file was ~350KB of writes per daemon. A test whose state
	// must outlive the process -- a successor or restart on the same root, a
	// read of wsm.db through DBPath, WithDB, CorruptRow, CountRows or
	// DisplacedTurnCount -- sets it on the root's FIRST daemon, and the harness
	// refuses loudly at the reuse or the read when it is missing, so a test
	// can never pass against state that was never written.
	DurableState bool
	// FreshStateDir says StateDir was minted for this daemon by its caller
	// (the e2e world mints the root because its store starts first), so the
	// root is treated exactly as one StartDaemon minted: in memory unless
	// DurableState.
	FreshStateDir bool
	// ProfileDir overrides where the fake shim reads its per-workspace startup
	// profiles from; empty mints a fresh one under this start's own root.
	//
	// IT EXISTS FOR THE RELAUNCH. A daemon BRINGS ITS OPEN WORKSPACES' SESSIONS
	// UP AT BOOT, so a profile the successor's own shims must read has to be on
	// disk before the successor starts — and a profile dir minted by
	// StartDaemon cannot be, because the caller does not know its path until
	// the daemon is already running. Handing the predecessor's dir over is what
	// makes "the same daemon, restarted" true of the shim fixtures as well as
	// of the state root.
	ProfileDir string
	// Joining, when set, starts the daemon in joining mode against the address.
	Joining string
	// IdleCutoff sets the hibernation idle cutoff via --idle-cutoff.
	IdleCutoff time.Duration
	// IdleCutoffMS compresses the same cutoff in milliseconds, for the
	// hibernation tests
	// use so the cutoff can be a handful of milliseconds.
	IdleCutoffMS int
	// FooterMomentaryDwell compresses the footer's momentary-status dwell via
	// --footer-momentary-dwell. It exists for the two tests whose subject IS
	// the retirement: the product window is 1.5s because a person has to read
	// the status, and a test that only needs to see the successor push arrive
	// has no reason to sit through a window sized for human eyes.
	FooterMomentaryDwell time.Duration
	// Pprof sets the profiling listener address; empty leaves it off.
	Pprof string
	// ServiceBinaries names the real shim-store and shim-claude-sidecar
	// binaries this daemon's world runs, when it runs them. Those processes
	// report their own builds into LockDirFor(StateDir) — the world points
	// their AGENT_REPL_LOCK_DIR there — and the deploy's fake build stages
	// copies of these binaries, so a deploy judges the running services up to
	// date and never restarts them. Empty: the world runs no real services,
	// and the harness states their reports itself.
	ServiceBinaries ServiceBinaries
	// MultiRepoRoot is the tree whose workspaces use the multi-repo account.
	MultiRepoRoot string
	// DefaultAccountEmail is written into the default config root's
	// .claude.json; empty leaves the root logged out.
	DefaultAccountEmail string
	// DefaultSettings, when set, is written as the default config root's
	// settings.json: the file the daemon reads the effort selector's starting
	// level from.
	DefaultSettings string
	// MultiRepoAccountEmail is written into the multi-repo config root.
	MultiRepoAccountEmail string
	// NoFake starts the daemon WITHOUT `--fake` and without
	// AGENT_REPL_CLAUDE_BIN, so every vendor call site (the classifier's
	// headless run, the login pty) reaches its real implementation and is
	// refused by the vendor guard. It is the only way to exercise the
	// guard's refusal sites; the fake shim is unaffected, since `--node`
	// still names it.
	NoFake bool
	// WithoutFakeShimsHook withholds AGENT_REPL_FAKE_SHIMS from a NoFake
	// daemon, leaving the VENDOR GUARD as the only thing that can make a shim
	// spawn fake.
	//
	// It exists for exactly one subject: the rule that the guard IMPLIES fake
	// shims. A daemon started this way asks its supervisor for a real-vendor
	// shim, and the run proves the supervisor forces the fake instead of
	// refusing the spawn -- which is what once made a workspace impossible to
	// create under the guard. Meaningless without NoFake, since a `--fake`
	// daemon asks for a fake shim on its own.
	WithoutFakeShimsHook bool
	// JSONCodec dials the daemon with the JSON codec instead of binary.
	JSONCodec bool
	// KeepStaleAddr leaves a predecessor's daemon.addr in place instead of
	// removing it, so the DAEMON's own handling of a stale advertisement is
	// the subject of the test. The harness then waits for the file's contents
	// to CHANGE rather than merely to exist, so the address it reports is
	// always this daemon's own.
	KeepStaleAddr bool
	// ExpectEarlyExit stops the harness from failing when the daemon exits on
	// its own, for the tests whose subject is a refusal to boot.
	ExpectEarlyExit bool
	// ExtraArgs and ExtraEnv are appended verbatim.
	ExtraArgs []string
	ExtraEnv  []string
	// OmitArgs names flags to REMOVE from the argv the harness would
	// otherwise build, together with each one's value. It exists for the
	// tests whose subject is a flag the daemon REQUIRES: the harness's own
	// argv is the reference for a correct launch, so the only honest way to
	// ask what happens without a flag is to take it back out of that argv
	// rather than to hand-roll a second one that could drift from it.
	OmitArgs []string
	// Timeout overrides DefaultTimeout for this one Daemon's context. Zero
	// means DefaultTimeout. Set it ONLY to a named, documented constant (for
	// example HandoverChainTimeout) with a one-line reason at the call site —
	// never to an ad hoc duration.
	Timeout time.Duration
	// ShimNode overrides --node (default: the built fake shim). The e2e
	// suite passes a real `node` binary here so the daemon spawns the real
	// TypeScript shim instead of the fake-shim Go stand-in.
	ShimNode string
	// ShimMain overrides --shim-main (default: a one-line placeholder
	// module). The e2e suite passes the real shim bundle, built from source
	// with `--fake` support baked in.
	ShimMain string
	// StoreSocket overrides the store socket path threaded through
	// --store-socket to every shim this daemon spawns (default: a fresh path
	// nothing listens on, as today). The e2e suite passes the socket of a
	// REAL running shim-store, so shims spawned by this daemon persist and
	// read real events.
	StoreSocket string
}

// Daemon is one running agent-repld process and the client dialed to it.
type Daemon struct {
	// StateDir is the daemon's state root.
	StateDir string
	// Addr is the loopback address it serves on, read from daemon.addr.
	Addr string
	// ProfileDir holds the fake shim's per-workspace startup profiles.
	ProfileDir string
	// staleAddr is the predecessor's daemon.addr content a KeepStaleAddr start
	// left standing, which awaitFile must not mistake for this daemon's.
	staleAddr string
	// LockDir is the redirected kernel-lock directory.
	LockDir string
	// Browser, Deploy and Launchctl record what the daemon invoked. Deploy is
	// the deploy's fake build, which stages what runs (DeployCurrent) unless
	// a test stages another: a harness never builds.
	Browser   *Recorder
	Deploy    *DeployBuilder
	Launchctl *Recorder
	// Notifier records every desktop banner the daemon posted
	// (AGENT_REPL_NOTIFIER_CMD), so no test ever raises a real one.
	Notifier *Recorder
	// Chime records every banner chime the daemon rang
	// (AGENT_REPL_CHIME_CMD), so no test ever plays a real sound.
	Chime *Recorder
	// HostToolsDir holds the fake persistent-wifi host tools
	// (AGENT_REPL_PERSISTENT_WIFI_TOOLS_DIR), so no test ever reads or changes
	// the real machine's power or network settings.
	HostToolsDir string
	// PromptsDir is the copy of prompts/ the daemon reads its briefs from.
	PromptsDir string
	// WebappDir is the served dist.
	WebappDir string
	// DefaultConfigDir and MultiRepoConfigDir are the two account roots.
	DefaultConfigDir   string
	MultiRepoConfigDir string
	// MultiRepoRoot is the tree the daemon was given as $MULTI_REPO_ROOT: a
	// workspace UNDER it routes to MultiRepoConfigDir, anything else to
	// DefaultConfigDir. A test that cares about the routing puts its repository
	// here with NewRepoAt.
	MultiRepoRoot string
	// StoreSocket is the store path nothing listens on.
	StoreSocket string
	// Git is the fake git world every scripted `git` answers from.
	Git *GitWorld

	t   testing.TB
	ctx context.Context
	// waitBound is ONE wait's failure bound; ctx above is the whole run's.
	waitBound time.Duration
	expected  map[string]bool
	// required are the expected operations a test also REQUIRES to have been
	// produced (RequireWarnings): the sweep fails on one that never was.
	required map[string]bool
	// expectedFor are the operations ExpectWarningsFor licensed for one
	// workspace's records only, keyed by the workspace's directory forms.
	expectedFor map[string]map[string]bool
	// view is whether this Daemon is a View of a process another fixture
	// owns; claims are the workspace directory forms the view answers for.
	view   bool
	claims map[string]bool

	// daemonProc is the PROCESS's state, as opposed to one test's: its
	// command, its client, its exit and the locks that order them. Every
	// Daemon over one process shares the one daemonProc.
	*daemonProc
}

// daemonProc is the state of one daemon PROCESS, shared by everything that
// names it. Its mu also guards the per-test expectation maps on Daemon, so one
// lock orders every read and write the sweep makes.
type daemonProc struct {
	cmd        *exec.Cmd
	stderrPath string
	// afterGroupStopped, when set, runs inside Kill between the group's
	// SIGSTOP and the leader's SIGKILL: the instant at which the harness's own
	// tests can undo the stop, as the kernel does for a member inside execve.
	afterGroupStopped func()
	// afterLeaderExit, when set, runs inside Kill between the kernel's report
	// that the leader exited and the group's SIGKILL: the one instant at which
	// the harness's own tests can read a group whose leader is dead and whose
	// members this harness has not yet signaled to die.
	afterLeaderExit func()
	// afterStraysFrozen, when set, runs inside each ReapStrays round that lists
	// a stray, once every listed stray is confirmed stopped and before any is
	// killed: the instant at which the harness's own tests can kill one stray
	// first, as a racing sweep would, or undo a stop, as exec does.
	afterStraysFrozen func()
	client            agentreplv1connect.AgentReplClient
	http              *http.Client

	mu     sync.Mutex
	exited bool
	// sigMu makes every signal to the daemon's pid or group, and the reap
	// that frees them, ONE OWNER'S. The reap takes it once the process has
	// exited and marks reapBegun before cmd.Wait; every signal takes it and
	// checks reapBegun first. A reap still running on a goroutine an earlier
	// bounded wait left behind (Stop giving up on SIGTERM and falling through
	// to Kill) therefore can never free the pid under a signal, nor between
	// Kill's leader SIGKILL and its group SIGKILL.
	sigMu     sync.Mutex
	reapBegun bool
	exitErr   error
	waitOnce  sync.Once
	shims     map[string]*ShimControl
	// inMemoryState is whether this daemon's state lives in memory (see
	// Opts.DurableState).
	inMemoryState bool
	// claimed maps every workspace directory form a view claimed to that
	// view's test, so the owner's sweep leaves those records to the view.
	claimed map[string]string
	// leftClaims are the claims of views whose tests have ended, for the
	// owner's final sweep of what their workspaces logged afterwards.
	leftClaims []claim
}

// InstallFakeGit installs the scripted `git` into dir, the directory that
// leads a daemon's PATH, as a HARD LINK to the suite's one built binary.
//
// WHY A LINK, NOT A COPY. Every daemon a test starts gets its own fakebin,
// and a 3.3MB copy for each of the suite's ~1,000 daemon starts was ~3GB of
// writes per daemon suite, the largest single writer in the run. A link
// writes no data, and what runs is the same file: the stray sweep matches a
// process by its argv, which names the link's path inside the test's own
// tree exactly as the copy's did. The suite's binaries and every fakebin sit
// under the one run root, so the link never crosses a file system; one that
// would is a failed test, never a silent copy.
func InstallFakeGit(t testing.TB, dir string) {
	t.Helper()
	if err := os.MkdirAll(dir, 0o755); err != nil {
		t.Fatalf("harness: mkdir %s: %v", dir, err)
	}
	if err := os.Link(FakeGitBinary(t), filepath.Join(dir, "git")); err != nil {
		t.Fatalf("harness: install the fake git: %v", err)
	}
}

// gitEnvKeys are the repository-selecting variables that must never be
// inherited by a git child: a hook-leaked GIT_DIR is a real, previously
// observed source of bogus work-tree errors.
// inMemoryRoots names every state root this suite process started on
// in-memory state, so a later daemon on the same root is refused rather than
// booted onto an empty database.
var inMemoryRoots sync.Map

// rootIsFresh reports whether this daemon's state root is new: minted by
// StartDaemon, or minted by its caller and declared so (Opts.FreshStateDir).
func rootIsFresh(opts Opts) bool {
	return opts.StateDir == "" || opts.FreshStateDir
}

// freshRootInMemory decides whether a freshly minted state root keeps its
// state in memory: unless the test asked for the file, and never for a joining
// daemon, which reads its incumbent's file.
func freshRootInMemory(opts Opts) bool {
	return !opts.DurableState && opts.Joining == ""
}

// refuseInMemoryReuse refuses a daemon on a state root whose first daemon ran
// on in-memory state: nothing of that state outlived its process.
func refuseInMemoryReuse(stateDir string) error {
	if _, wasMemory := inMemoryRoots.Load(stateDir); wasMemory {
		return fmt.Errorf("harness: %s was started on in-memory state, so nothing of it outlived its daemon; start that root's first daemon with harness.Opts{DurableState: true}", stateDir)
	}
	return nil
}

var gitEnvKeys = []string{
	"GIT_DIR", "GIT_WORK_TREE", "GIT_INDEX_FILE", "GIT_COMMON_DIR",
	"GIT_PREFIX", "GIT_OBJECT_DIRECTORY", "GIT_ALTERNATE_OBJECT_DIRECTORIES",
}

// omitFlags returns args without each named flag and the value following it.
// A flag that is not present is not an error: a test names what must be
// missing, not what the harness happened to add.
func omitFlags(args, omit []string) []string {
	if len(omit) == 0 {
		return args
	}
	drop := make(map[string]bool, len(omit))
	for _, flag := range omit {
		drop[flag] = true
	}
	kept := make([]string, 0, len(args))
	for i := 0; i < len(args); i++ {
		if drop[args[i]] {
			i++ // the flag's value goes with it
			continue
		}
		kept = append(kept, args[i])
	}
	return kept
}

func cleanGitEnv(env []string) []string {
	out := make([]string, 0, len(env))
	for _, kv := range env {
		key, _, _ := strings.Cut(kv, "=")
		drop := false
		for _, bad := range gitEnvKeys {
			if key == bad {
				drop = true
				break
			}
		}
		if !drop {
			out = append(out, kv)
		}
	}
	return out
}

// StartDaemon lays out a hermetic environment, starts the daemon, waits for
// daemon.addr, and dials it. Every process it starts is killed on cleanup.
func StartDaemon(t testing.TB, opts Opts) *Daemon {
	t.Helper()
	// The suite's live-daemon cap, held for this top-level test's whole run.
	// See slots.go: DefaultTimeout's measured basis is eight concurrent
	// daemons, and `-parallel` alone does not bound that.
	acquireDaemonSlot(t)
	binary := DaemonBinary(t)
	root := t.TempDir()
	// Unix-domain socket paths are capped at 103 bytes, and t.TempDir() encodes
	// the whole test name, which blows that budget for the longer names in this
	// suite. Everything the daemon opens a SOCKET under lives beneath a short
	// root of its own; everything else stays under t.TempDir().
	sockRoot := ShortTempDir(t)

	d := &Daemon{
		StateDir:           opts.StateDir,
		ProfileDir:         opts.ProfileDir,
		PromptsDir:         CopyPrompts(t, filepath.Join(root, "prompts")),
		WebappDir:          NewFakeWebappDist(t, filepath.Join(root, "dist")),
		DefaultConfigDir:   NewConfigRoot(t, filepath.Join(root, "config-default"), accountEmail(opts.DefaultAccountEmail, "default@example.invalid")),
		MultiRepoConfigDir: NewConfigRoot(t, filepath.Join(root, "config-multi"), accountEmail(opts.MultiRepoAccountEmail, "multi@example.invalid")),
		StoreSocket:        opts.StoreSocket,
		Browser:            NewFakeBrowser(t, filepath.Join(root, "bin")),
		Launchctl:          NewFakeLaunchctl(t, filepath.Join(root, "bin")),
		Notifier:           NewFakeNotifier(t, filepath.Join(root, "bin")),
		Chime:              NewFakeChime(t, filepath.Join(root, "bin")),
		HostToolsDir:       NewFakeHostTools(t, filepath.Join(root, "host-tools")),
		t:                  t,
		expected:           map[string]bool{},
		required:           map[string]bool{},
		daemonProc:         &daemonProc{shims: map[string]*ShimControl{}},
	}
	if opts.DefaultSettings != "" {
		writeFile(t, filepath.Join(d.DefaultConfigDir, "settings.json"), opts.DefaultSettings)
	}
	if d.ProfileDir == "" {
		d.ProfileDir = filepath.Join(root, "shim-profiles")
	}
	fresh := rootIsFresh(opts)
	if d.StateDir == "" {
		d.StateDir = filepath.Join(sockRoot, "state")
		if err := os.MkdirAll(d.StateDir, 0o755); err != nil {
			t.Fatalf("harness: mkdir state root: %v", err)
		}
	}
	if err := refuseInMemoryReuse(d.StateDir); err != nil {
		t.Fatal(err)
	}
	if fresh {
		d.inMemoryState = freshRootInMemory(opts)
		if d.inMemoryState {
			inMemoryRoots.Store(d.StateDir, true)
		}
	}
	if d.StoreSocket == "" {
		d.StoreSocket = filepath.Join(sockRoot, "store.sock")
	}
	// THE COVERAGE TEARDOWN IS REGISTERED FIRST, SO IT RUNS LAST. A coverage
	// run has to let the daemon leave through SIGTERM — a SIGKILLed process
	// writes no counters — and a graceful shutdown emits records an abruptly
	// killed one never wrote. Registered here, ahead of the warning sweep, it
	// runs AFTER that sweep (t.Cleanup unwinds last-registered-first), so the
	// sweep reads exactly the log content it reads without coverage and the
	// suite's pass set is unchanged. The ordinary teardown below stands down
	// while this one is armed, so the process is still killed exactly once.
	if CoverageEnabled() {
		t.Cleanup(func() {
			d.gracefulStopForCoverage()
			d.Kill()
			d.standDownStraysForCoverage()
			d.ReapStrays()
		})
	}
	// The warning sweep is UNCONDITIONAL: every daemon sweeps its logs at test
	// end with an empty expected set, so a test that never calls ExpectWarnings
	// still gets the assertion. ExpectWarnings only widens this set.
	registerLogChecks(t, func() { keepFailureLogs(t, d.StateDir, d.stderrPath) }, d.assertNoUnexpectedWarnings, d.assertNoUnproducedRequiredWarnings)
	// THE LOCK DIRECTORY IS A CROSS-DAEMON RENDEZVOUS, not a per-start temp
	// dir, so it is keyed to the STATE ROOT and settled only now that the root
	// is known.
	//
	// A shim outlives the daemon that spawned it and holds its workspace lock
	// in the directory THAT daemon named. A successor given a fresh directory
	// therefore probes a file nobody could ever hold, reads it free, and
	// spawns a second shim onto a conversation the survivor is still serving
	// — the very failure the lock exists to prevent, manufactured by the
	// harness. Sharing a state root is how this suite spells "the same daemon,
	// restarted", so every daemon over one state root probes one set of locks.
	// It is a SIBLING of the state root rather than a child: the boot's own
	// refusal tests hand the daemon an unwritable state root, and a lock
	// directory beneath it could not be created at all.
	d.LockDir = LockDirFor(d.StateDir)
	requireSocketPathBudget(t, d.StateDir)
	for _, dir := range []string{d.ProfileDir, d.LockDir} {
		if err := os.MkdirAll(dir, 0o755); err != nil {
			t.Fatalf("harness: mkdir %s: %v", dir, err)
		}
	}
	mainJS := filepath.Join(root, "main.js")
	writeFile(t, mainJS, "// placeholder shim module\n")
	if opts.ShimMain != "" {
		mainJS = opts.ShimMain
	}
	fakeBin := filepath.Join(root, "bin")
	d.Deploy = NewFakeDeployBuilder(t, fakeBin, DeploySources{
		ShimMain: mainJS, WebappDist: d.WebappDir,
		Store: opts.ServiceBinaries.Store, Sidecar: opts.ServiceBinaries.Sidecar,
	})
	fakeClaude := NewFakeClaude(t, fakeBin)
	// The scripted `git` goes first on the daemon's PATH, so every git fact the
	// daemon reads comes out of this test's fixture file and the real binary is
	// never reached.
	d.Git = World(t)
	d.Git.tagPinnedCheckout(PinnedCheckout(t))
	InstallFakeGit(t, fakeBin)

	multiRoot := opts.MultiRepoRoot
	if multiRoot == "" {
		multiRoot = filepath.Join(root, "multi")
		if err := os.MkdirAll(multiRoot, 0o755); err != nil {
			t.Fatalf("harness: mkdir multi root: %v", err)
		}
	}

	d.MultiRepoRoot = multiRoot

	timeout := opts.Timeout
	if timeout <= 0 {
		timeout = DefaultTimeout
	}
	d.waitBound = timeout
	// THE RUN BUDGET, NOT ONE WAIT'S BOUND. See runBudgetWaits: every wait the
	// harness makes carves its own `timeout`-sized child off this, so the two
	// promises stay separate. Both are kept on the granted clock
	// (grantedBound), so a host with no CPU to give this daemon never reads
	// as the daemon's failure.
	requireGrantedClock(t)
	ctx, cancel := grantedclock.WithBound(context.Background(), timeout*runBudgetWaits)
	t.Cleanup(cancel)
	d.ctx = ctx

	node := opts.ShimNode
	if node == "" {
		node = FakeShimBinary(t)
	}
	args := []string{
		"--state-dir", d.StateDir,
		"--node", node,
		"--shim-main", mainJS,
		"--webapp-dist", d.WebappDir,
		"--store-socket", d.StoreSocket,
		"--prompts-dir", d.PromptsDir,
		"--default-config-dir", d.DefaultConfigDir,
		"--multi-repo-config-dir", d.MultiRepoConfigDir,
	}
	if !opts.NoFake {
		args = append(args, "--fake")
	}
	if opts.Joining != "" {
		args = append(args, "--joining", opts.Joining)
	}
	// THE CUTOFF HAS ONE KNOB. The daemon reads `--idle-cutoff` and nothing
	// else, so the millisecond spelling is the same flag with a smaller value.
	switch {
	case opts.IdleCutoff > 0:
		args = append(args, "--idle-cutoff", opts.IdleCutoff.String())
	case opts.IdleCutoffMS > 0:
		args = append(args, "--idle-cutoff", (time.Duration(opts.IdleCutoffMS) * time.Millisecond).String())
	}
	if opts.FooterMomentaryDwell > 0 {
		args = append(args, "--footer-momentary-dwell", opts.FooterMomentaryDwell.String())
	}
	if opts.Pprof != "" {
		args = append(args, "--pprof", opts.Pprof)
	}
	args = append(args, opts.ExtraArgs...)
	args = omitFlags(args, opts.OmitArgs)

	env := append(os.Environ(),
		"AGENT_REPL_STATE_DIR="+d.StateDir,
		// The integration suite asserts debug request and transition records.
		// Production's empty setting is info; the suite states debug explicitly,
		// and, debug being a window rather than a standing level
		// (proto/vocab/log-level-window.json), for the longest window there is.
		"AGENT_REPL_LOG_LEVEL=debug",
		fmt.Sprintf("AGENT_REPL_LOG_LEVEL_UNTIL=%d", time.Now().Add(5*time.Minute).Unix()),
		"AGENT_REPL_FORBID_VENDOR_CALLS=1",
		// The state database is thrown away with the test: no forced flushes.
		wsm.EnvTestUnsyncedWrites+"=1",
		"AGENT_REPL_LOCK_DIR="+d.LockDir,
		"AGENT_REPL_STORE_SOCKET="+filepath.Join(sockRoot, "unused-store.sock"),
		"MULTI_REPO_ROOT="+multiRoot,
		"AGENT_REPL_BROWSER_CMD="+d.Browser.Path,
		"AGENT_REPL_DEPLOY_BUILDER="+d.Deploy.Path,
		"AGENT_REPL_LAUNCHCTL="+d.Launchctl.Path,
		"AGENT_REPL_NOTIFIER_CMD="+d.Notifier.Path,
		"AGENT_REPL_CHIME_CMD="+d.Chime.Path,
		"AGENT_REPL_PERSISTENT_WIFI_TOOLS_DIR="+d.HostToolsDir,
		"AGENT_REPL_PERSISTENT_WIFI_HOTSPOT="+FakeHotspot,
		"AGENT_REPL_LAUNCH_AGENTS_DIR="+filepath.Join(root, "LaunchAgents"),
		fakegit.EnvStateFile+"="+d.Git.StateFile,
		"FAKESHIM_PROFILE_DIR="+d.ProfileDir,
		// HOME and PATH aside, the build identity is stated in one place only.
		"HOME="+root,
		"PATH="+fakeBin+string(os.PathListSeparator)+os.Getenv("PATH"),
	)
	if d.inMemoryState {
		env = append(env, wsm.EnvTestStateInMemory+"=1")
	}
	env = append(env, BuildIdentityEnv(PinnedCheckout(t))...)
	env = append(env, HostNeutralNamingEnv()...)
	env = append(env, HostNeutralCheckoutEnv()...)
	env = append(env, HostNeutralPowerEnv()...)
	env = append(env, HostNeutralNewsDigestEnv()...)
	env = append(env, TemporaryRegistrationEnv(t)...)
	// THE VENDOR BINARY IS THE GUARD'S OTHER HALF. Left unset, the login pty
	// falls back to the default `claude` and the guard refuses it; naming the
	// fake claude is what makes the ordinary tests spawn something harmless.
	// NoFake therefore withholds it deliberately.
	if !opts.NoFake {
		env = append(env, "AGENT_REPL_CLAUDE_BIN="+fakeClaude)
	} else if !opts.WithoutFakeShimsHook {
		// The SHIMS stay fake even with the whole stack's fake mode off:
		// --node names the fake shim, but the daemon cannot know that. The
		// vendor guard would force the fake by itself; the hook is stated
		// anyway so the ordinary NoFake test does not depend on that rule
		// while exercising something else. WithoutFakeShimsHook withholds it
		// for the one test whose subject IS that rule.
		env = append(env, "AGENT_REPL_FAKE_SHIMS=1")
	}
	// COVERAGE, WHEN THE RUN ASKED FOR IT. GOCOVERDIR is this daemon's own
	// counter directory; NODE_V8_COVERAGE is inherited by every shim the
	// daemon spawns (shimclient.spawnEnv copies the daemon's environment
	// forward verbatim outside its fixed override set). Both are absent
	// entirely on an ordinary run.
	if root := CoverageRoot(); root != "" {
		daemonCov, err := CoverageEnv(root, "agent-repld")
		if err != nil {
			t.Fatalf("harness: %v", err)
		}
		nodeCov, err := NodeCoverageEnv(root)
		if err != nil {
			t.Fatalf("harness: %v", err)
		}
		env = append(env, daemonCov...)
		env = append(env, nodeCov...)
	}
	env = append(env, opts.ExtraEnv...)

	// A NON-JOINING START THAT EXPECTS TO SERVE OWNS daemon.addr. A crash-restart test reuses a state
	// root whose previous daemon was SIGKILLed, so the file it never removed is
	// still there with the dead daemon's port: read as this daemon's address it
	// dials a closed socket. The daemon is about to rewrite it, so removing it
	// first makes AwaitAddrFile unambiguous. A JOINING successor shares the root
	// with a live incumbent that owns the file, so it is left alone — and so is
	// a start the test expects to be REFUSED, which is a second daemon against
	// a live incumbent whose file must survive its refusal untouched.
	//
	// A test whose SUBJECT is that handling passes KeepStaleAddr, and the
	// harness then reads the standing address so it can wait for a different
	// one instead of accepting the dead port as this daemon's.
	if opts.Joining == "" && !opts.ExpectEarlyExit {
		if opts.KeepStaleAddr {
			if body, err := os.ReadFile(filepath.Join(d.StateDir, "daemon.addr")); err == nil {
				d.staleAddr = string(body)
			}
		} else if err := os.Remove(filepath.Join(d.StateDir, "daemon.addr")); err != nil && !os.IsNotExist(err) {
			t.Fatalf("harness: remove the stale daemon.addr: %v", err)
		}
	}

	cmd := exec.Command(binary, args...)
	cmd.Dir = root
	cmd.Env = cleanGitEnv(env)
	// THE PROCESS WRITES TO A FILE, NEVER TO A PIPE. exec gives an io.Writer a
	// pipe and makes Wait block until every writer of it closes — and a
	// HANDOVER's successor inherits this daemon's stderr, so a piped harness
	// waits for the successor to die before it will admit the incumbent
	// exited. A file has no such reader to drain.
	stderrPath := filepath.Join(root, "daemon.stderr.log")
	stderrFile, err := os.Create(stderrPath)
	if err != nil {
		t.Fatalf("harness: create the daemon's stderr file: %v", err)
	}
	t.Cleanup(func() { _ = stderrFile.Close() })
	d.stderrPath = stderrPath
	cmd.Stderr = stderrFile
	cmd.Stdout = stderrFile
	cmd.SysProcAttr = &syscall.SysProcAttr{Setpgid: true}
	if err := cmd.Start(); err != nil {
		t.Fatalf("harness: start daemon: %v", err)
	}
	d.cmd = cmd
	// EVERY DEPLOY THIS DAEMON RUNS IS A NO-OP unless the test stages another:
	// the builds it states are the ones running.
	d.StageDeployBuild(DeployCurrent)
	t.Cleanup(func() {
		// On a coverage run the teardown registered ahead of the warning
		// sweep owns this and runs it after that sweep instead.
		if CoverageEnabled() {
			return
		}
		d.Kill()
		// The daemon's group is gone; its shims are in groups of their own and
		// would otherwise outlive the test.
		d.ReapStrays()
	})

	if opts.ExpectEarlyExit {
		return d
	}
	if opts.Joining == "" {
		d.Addr = d.AwaitAddrFile()
		d.dial(opts.JSONCodec)
		d.awaitServing()
	}
	return d
}

// awaitServing blocks until the daemon is actually SERVING, not merely
// advertising. `daemon.addr` is written at boot step 6 and the state client is
// opened at step 7, so a test that acts the instant the address appears can
// reach a daemon whose wsm.db has no schema yet — which is how
// `no such table: layout` surfaces from a WithDB read after an immediate Stop.
// The run log's own serving record is the first moment every boot step is done.
func (d *Daemon) awaitServing() {
	d.t.Helper()
	d.AwaitLogRecord(d.RunLogPath(), "the daemon's serving record", func(r LogRecord) bool {
		return r.Operation == "daemon.cmd.serve" && strings.Contains(r.Message, "serving")
	})
}

// ShortTempDir mints a directory directly under /tmp, short enough that a
// state root beneath it still fits a unix-domain socket path, and removes it
// on cleanup.
//
// t.TempDir() CANNOT HOLD ANYTHING A SOCKET HANGS OFF. It encodes the whole
// test name under the platform's temp root, and on macOS that root is itself
// the per-user `/var/folders/<hash>/T` — 49 bytes before the test name is even
// spelled. A shim socket lives at `<state root>/sock/<name>`, so a state root
// named after a test like TestBootRefusesAnUnwritableStateRoot blows the
// 103-byte sun_path budget and the daemon refuses to boot. A suite that only
// passes under a TMPDIR override is a broken suite, so every state root in it
// — the harness's own default and the ones tests build for themselves — hangs
// off this instead, whose length is fixed and independent of the test's name.
//
// Non-socket paths (config roots, fake dists, prompt copies, repos) have no
// such budget and stay under t.TempDir(), where a failed run's leftovers are
// named after the test that left them.
func ShortTempDir(t testing.TB) string {
	t.Helper()
	// Under the run root (runroot.go), so an abnormally ended run's state
	// roots are reclaimed with it. It is itself under /tmp and short.
	base := runRoot
	if base == "" {
		t.Fatal("harness: the suite's TestMain must call harness.Main")
	}
	dir, err := os.MkdirTemp(base, "ar")
	if err != nil {
		t.Fatalf("harness: mkdir a short temp root: %v", err)
	}
	t.Cleanup(func() { _ = os.RemoveAll(dir) })
	return dir
}

// requireSocketPathBudget fails the test before the daemon is launched when
// the state root cannot hold a shim socket. The daemon refuses such a root at
// boot; without this the failure surfaces as a DefaultTimeout wait for
// daemon.addr instead.
func requireSocketPathBudget(t testing.TB, stateDir string) {
	t.Helper()
	layout, err := stateroot.Root(stateDir, "")
	if err != nil {
		t.Fatalf("harness: resolve the state root %q: %v", stateDir, err)
	}
	if err := layout.CheckSocketPathBudget(); err != nil {
		t.Fatalf("harness: %v", err)
	}
}

func accountEmail(given, fallback string) string {
	if given == LoggedOut {
		return ""
	}
	if given != "" {
		return given
	}
	return fallback
}

// LoggedOut asks StartDaemon for an account root with no .claude.json.
const LoggedOut = "\x00logged-out"

// AddrFile is the state root's daemon.addr path.
func (d *Daemon) AddrFile() string { return filepath.Join(d.StateDir, "daemon.addr") }

// AwaitAddrFile waits for daemon.addr to appear and answers its address. The
// payload is the bare address on the first line and this daemon's `pid=<n>`
// on the second; the address is asserted to be the contracted loopback shape
// and the pid line to name this daemon's live process.
func (d *Daemon) AwaitAddrFile() string {
	d.t.Helper()
	raw := d.awaitFile(d.AddrFile())
	if !strings.HasSuffix(raw, "\n") {
		d.t.Fatalf("daemon.addr = %q, want a trailing newline", raw)
	}
	adv := AddrAdvertisement(raw)
	host, _, err := net.SplitHostPort(adv.Address)
	if err != nil {
		d.t.Fatalf("daemon.addr = %q, want 127.0.0.1:<port>: %v", raw, err)
	}
	if host != "127.0.0.1" {
		d.t.Fatalf("daemon.addr host = %q, want the loopback address", host)
	}
	if !adv.PIDKnown || adv.PID <= 0 {
		d.t.Fatalf("daemon.addr = %q, want a pid=<n> line naming the advertiser", raw)
	}
	return adv.Address
}

// AddrAdvertisement parses a daemon.addr payload into its address and pid,
// mirroring the daemon's own reader for the integration and e2e suites.
func AddrAdvertisement(raw string) daemonaddr.Advertisement {
	return daemonaddr.ParseAdvertisement(raw)
}

// AddrLine is the bare address a daemon.addr payload advertises, for tests
// that only need the address the file names.
func AddrLine(raw string) string {
	return daemonaddr.ParseAdvertisement(raw).Address
}

// staleFor is the content awaitFile must NOT accept for a path: the
// predecessor's advertisement, when the test asked for it to be kept. Empty
// for every other path, which no file's content can equal.
func (d *Daemon) staleFor(path string) string {
	if path == d.AddrFile() {
		return d.staleAddr
	}
	return ""
}

// awaitFile polls for a file, bounded by ONE wait's bound off the run budget.
func (d *Daemon) awaitFile(path string) string {
	d.t.Helper()
	wait, cancelWait := d.waitCtx()
	defer cancelWait()
	ticker := time.NewTicker(pollInterval)
	defer ticker.Stop()
	for {
		if body, err := os.ReadFile(path); err == nil && len(body) > 0 && string(body) != d.staleFor(path) {
			return string(body)
		}
		if d.Exited() && !d.expectedExit() {
			d.t.Fatalf("daemon exited before writing %s (exit %v)\nstderr:\n%s", filepath.Base(path), d.exitErr, d.Stderr())
		}
		select {
		case <-ticker.C:
		case <-wait.Done():
			// A DAEMON THAT IS ALIVE AND NOT DOING WHAT IT WAS WAITED FOR IS
			// ASKED WHERE IT IS, before the failure is written. Three processes
			// starting at one instant (two stores and this boot) once each
			// logged their first record and then nothing for 5s (2026-10-06,
			// a full-suite run), where a quiet boot measures 50-160ms; only a
			// stack names a stall like that.
			dump := d.dumpOnStall(reapGrace)
			d.t.Fatalf("waiting for %s: %v (%s)\nstderr:\n%s", path, grantedclock.Explain(wait), dump, d.Stderr())
		}
	}
}

// dumpOnStall asks a live daemon to dump every goroutine and exit -- its
// SIGQUIT handler records daemon.cmd.sigquit with the whole dump, which its
// stderr mirror carries -- and waits up to bound for it to go. It answers a
// line for the failure message saying what happened.
func (d *Daemon) dumpOnStall(bound time.Duration) string {
	d.sigMu.Lock()
	if d.reapBegun {
		d.sigMu.Unlock()
		return "the daemon was already reaped; no goroutine dump"
	}
	pid := d.cmd.Process.Pid
	err := syscall.Kill(pid, syscall.SIGQUIT)
	d.sigMu.Unlock()
	if errors.Is(err, syscall.ESRCH) {
		return "the daemon was already gone; no goroutine dump"
	}
	if err != nil {
		return fmt.Sprintf("could not ask the daemon for a goroutine dump: %v", err)
	}
	ctx, cancel := grantedclock.WithBound(context.Background(), bound)
	defer cancel()
	if err := WaitProcessExit(ctx, pid); err != nil {
		return fmt.Sprintf("SIGQUIT sent for a goroutine dump, but the daemon did not exit within %s granted: %v", bound, grantedclock.Explain(ctx))
	}
	return "SIGQUIT sent; the daemon.cmd.sigquit record below holds its goroutine dump"
}

// AwaitFileGone waits for a path to disappear.
func (d *Daemon) AwaitFileGone(path string) {
	d.t.Helper()
	wait, cancelWait := d.waitCtx()
	defer cancelWait()
	ticker := time.NewTicker(pollInterval)
	defer ticker.Stop()
	for {
		if _, err := os.Stat(path); errors.Is(err, os.ErrNotExist) {
			return
		}
		select {
		case <-ticker.C:
		case <-wait.Done():
			d.t.Fatalf("waiting for %s to be removed: %v", path, grantedclock.Explain(wait))
		}
	}
}

// AwaitFileExists waits for a path to appear, bounded by ONE wait's bound.
func (d *Daemon) AwaitFileExists(path string) {
	d.t.Helper()
	wait, cancelWait := d.waitCtx()
	defer cancelWait()
	ticker := time.NewTicker(pollInterval)
	defer ticker.Stop()
	for {
		if _, err := os.Stat(path); err == nil {
			return
		}
		select {
		case <-ticker.C:
		case <-wait.Done():
			d.t.Fatalf("waiting for %s to appear: %v", path, grantedclock.Explain(wait))
		}
	}
}

func (d *Daemon) expectedExit() bool {
	d.mu.Lock()
	defer d.mu.Unlock()
	return d.exited && d.exitErr == nil
}

// dial builds the Connect client. The daemon serves HTTP/1.1 and h2c on one
// origin; the harness uses h2c so a server stream is not head-of-line blocked.
func (d *Daemon) dial(jsonCodec bool) {
	d.t.Helper()
	d.http = &http.Client{
		Transport: &http2.Transport{
			AllowHTTP: true,
			DialTLSContext: func(ctx context.Context, network, addr string, _ *tls.Config) (net.Conn, error) {
				var dialer net.Dialer
				return dialer.DialContext(ctx, network, addr)
			},
		},
	}
	var opts []connect.ClientOption
	if jsonCodec {
		opts = append(opts, connect.WithProtoJSON())
	}
	d.client = agentreplv1connect.NewAgentReplClient(d.http, "http://"+d.Addr, opts...)
}

// Client is the Connect client dialed to this daemon.
func (d *Daemon) Client() agentreplv1connect.AgentReplClient {
	d.t.Helper()
	if d.client == nil {
		d.t.Fatal("harness: the daemon has no client (joining mode, or an expected early exit)")
	}
	return d.client
}

// Dial builds a second, independent client connection, for the tests whose
// subject is per-connection state (a feed page walk).
func (d *Daemon) Dial() agentreplv1connect.AgentReplClient {
	d.t.Helper()
	client := &http.Client{
		Transport: &http2.Transport{
			AllowHTTP: true,
			DialTLSContext: func(ctx context.Context, network, addr string, _ *tls.Config) (net.Conn, error) {
				var dialer net.Dialer
				return dialer.DialContext(ctx, network, addr)
			},
		},
	}
	return agentreplv1connect.NewAgentReplClient(client, "http://"+d.Addr)
}

// HTTP is a plain client against the daemon's asset origin.
func (d *Daemon) HTTP() *http.Client { return &http.Client{} }

// Ctx is the context every call in the test is bounded by.
func (d *Daemon) Ctx() context.Context { return d.ctx }

// waitCtx bounds ONE wait, off the daemon's whole-run budget.
//
// A wait is the harness's own failure detector, and its bound is
// Opts.Timeout (DefaultTimeout by default). Reading d.ctx directly instead
// would hand a wait whatever the run had left — everything from the full
// budget down to nothing, depending only on what ran before it — so a slow
// early step would surface as a timeout on a later, blameless one.
func (d *Daemon) waitCtx() (context.Context, context.CancelFunc) {
	return grantedclock.WithBound(d.ctx, d.waitBound)
}

// WaitCtx is waitCtx for the TESTS' own waits, which need the same bound for
// the same reason.
//
// The suite's shared await helpers (awaitRow, awaitFooter, awaitTopbar,
// awaitRoster, awaitHostFault) handed harness.AwaitView the daemon's whole-run
// context, which is precisely what waitCtx's doc says not to do. The cost is
// not theoretical: every detached-shell red in the 2026-09-18 run burned the
// FULL 35s run budget on one wait that was never going to be satisfied,
// turning a handful of reds into minutes of waiting for nothing new.
func (d *Daemon) WaitCtx() (context.Context, context.CancelFunc) { return d.waitCtx() }

// PID is the daemon process's id.
func (d *Daemon) PID() int { return d.cmd.Process.Pid }

// reapGrace is how long the scheduling of an already-decided signal ordinarily
// takes, which every observed run completes in single-digit milliseconds. It
// bounds the waits whose outcome is NOT yet decided (the freeze confirmation,
// the reap after a refused signal) and is the threshold above which Kill notes
// a slow reap. Kill's own wait after an accepted SIGKILL is not bounded by it:
// that exit is decided, and only its scheduling is left.
const reapGrace = 2 * time.Second

// Stop sends SIGTERM and waits, BOUNDED, for the process to leave. A daemon
// that ignores SIGTERM is escalated to SIGKILL and reported, never waited on
// indefinitely.
func (d *Daemon) Stop() {
	d.t.Helper()
	d.refuseOnView("Stop")
	d.signal(syscall.SIGTERM)
	if d.awaitReapWithin(DefaultTimeout) {
		return
	}
	d.t.Errorf("harness: the daemon did not exit within %s granted of SIGTERM; killing it", DefaultTimeout)
	d.Kill()
}

// gracefulStopForCoverage sends SIGTERM and waits, bounded, for the daemon to
// leave — but ONLY on a coverage run, and it reports nothing: it is a
// best-effort flush of the daemon's coverage counters ahead of the cleanup
// kill, not a shutdown assertion. Daemon.Stop remains the assertion.
func (d *Daemon) gracefulStopForCoverage() {
	if !CoverageEnabled() || d.cmd == nil || d.cmd.Process == nil || d.reaped() {
		return
	}
	d.sigMu.Lock()
	sent := !d.reapBegun && d.cmd.Process.Signal(syscall.SIGTERM) == nil
	d.sigMu.Unlock()
	if !sent {
		return
	}
	d.awaitReapWithin(DefaultTimeout)
}

// standDownStraysForCoverage SIGTERMs every process still naming this run's
// state directory and waits, bounded, for them to leave — ONLY on a coverage
// run, and reporting nothing. ReapStrays still runs underneath, so a process
// that ignores the signal is killed exactly as it always was.
func (d *Daemon) standDownStraysForCoverage() {
	if !CoverageEnabled() {
		return
	}
	pids := d.strayPIDs()
	if len(pids) == 0 {
		return
	}
	for _, pid := range pids {
		_ = syscall.Kill(pid, syscall.SIGTERM)
	}
	deadline := time.After(DefaultTimeout)
	ticker := time.NewTicker(20 * time.Millisecond)
	defer ticker.Stop()
	for {
		select {
		case <-deadline:
			return
		case <-ticker.C:
			if len(d.strayPIDs()) == 0 {
				return
			}
		}
	}
}

// Kill ends the process group without warning, for crash simulation and for
// the cleanup every test gets.
//
// THE LEADER HAS EXITED BEFORE ANY MEMBER IS SIGNALED TO DIE, so the daemon can
// never observe a death this harness caused. Every git the daemon runs, and
// every shim in the instant between its fork and its own setpgid, is a member
// of the daemon's group, and kill(-pgid, SIGKILL) is not one atomic event: the
// kernel walks the group, newest member first, and the walk can be preempted
// between members. Under load the daemon was still running when its git or its
// just-forked shim had already died, and it recorded exactly that at ERROR —
// "git was killed by a signal", "shim died during bring-up" — before its own
// SIGKILL landed; the warning sweep then failed the test on records its own
// teardown manufactured. A process that has exited runs no instruction, and
// exiting is irrevocable, so once the kernel reports the leader's exit,
// whatever order the group kill reaches the members in, nothing is left to
// observe it.
//
// THE ORDERING RESTS ON THE LEADER'S EXIT, NEVER ON A STOP, because a stop is
// revocable. On Darwin a SIGSTOP that reaches a process inside execve is
// DISCARDED when the exec completes: the kernel reports the process stopped
// while its thread is still in the kernel finishing the exec, then the new
// image runs, with no signal pending. MEASURED under 16 CPU loads: a member
// SIGSTOPped just after its fork read stopped and then running again, as the
// exec'd program, in 453 of 3000 runs; one already past its exec, in 0 of
// 3000. No state the kernel exports tells that transient stop from a real
// one, so no stop, and no confirmation of one, can guarantee a frozen group.
// The leader's stop was the old ordering's premise, and a leader killed in
// the instant after its Start is exactly such a process.
//
// The group is still stopped first, as containment: a member that is not
// inside execve, such as a shim between its fork and its setpgid, is held in
// the group while the leader dies rather than leaving it. The leader's exit
// orphans the group, and the kernel then sends its stopped members SIGHUP and
// SIGCONT, so the group kill follows the leader's exit at once.
func (d *Daemon) Kill() {
	d.t.Helper()
	d.refuseOnView("Kill")
	if d.cmd == nil || d.cmd.Process == nil {
		return
	}
	if !d.killGroup() {
		return
	}
	// THE REAP IS AWAITED ON THE REAP ITSELF, NOT RACED AGAINST A CLOCK. The
	// leader has already exited, so what a wall-clock bound measured here was
	// only how soon the reap got scheduled; on a saturated host that is
	// seconds. TestReselectingAWorkspaceProducesNoDuplicatePush failed on
	// "still unreaped 2s after SIGKILL" for a kill that had done exactly its
	// job.
	began := time.Now()
	d.waitOnce.Do(d.wait)
	if took := time.Since(began); took > reapGrace {
		d.t.Logf("harness: the SIGKILLed daemon took %s to be reaped; the host was starving it", took)
	}
}

// killGroup runs Kill's signals as sigMu's owner and reports whether the
// group was killed and is left to reap.
//
// A REAPED PROCESS IS NEVER SIGNALED. Once the reap has begun, the kernel may
// have freed the pid and the group id that shares it: -pid names no group of
// ours any more, and signaling it can only reach whatever process the pid was
// recycled into. This is the ordinary state of every test that waits for its
// daemon to leave on its own (a refused second daemon, a joining daemon)
// before the cleanup kill runs. While killGroup holds sigMu no reap can
// begin, so the leader is ours and unreaped — a zombie at worst — and every
// signal names our process and our group exactly.
func (d *Daemon) killGroup() bool {
	d.t.Helper()
	d.sigMu.Lock()
	defer d.sigMu.Unlock()
	if d.reapBegun {
		return false
	}
	pgid := d.cmd.Process.Pid
	if !d.signalGroup(pgid, syscall.SIGSTOP) {
		return false
	}
	if d.afterGroupStopped != nil {
		d.afterGroupStopped()
	}
	// EVERY MEMBER'S EXIT EVENT IS TAKEN WHILE THE GROUP IS STOPPED, BEFORE
	// ANY OF THEM IS KILLED. A stopped process is not exiting, so the kernel
	// grants the registration; once the kill is under way it refuses it, and
	// refuses the dying member to the group listing too, while the member
	// still holds its files (2026-10-16:
	// TestKillReturnsOnceEveryMemberHasReleasedItsFiles read a member's FIFO
	// still open after Kill returned). The group wait below awaits every held
	// event whatever the listing shows.
	held, err := holdGroupExits(pgid, procgroup.UnendedMembers, takeExitEvent)
	if err != nil {
		d.t.Errorf("harness: hold the exit events of the daemon's group %d before killing it: %v", pgid, err)
	}
	defer held.close()
	if !d.killLeader() {
		return false
	}
	// THE LEADER'S EXIT IS AWAITED ON THE KERNEL'S EXIT EVENT, NOT A CLOCK.
	// SIGKILL cannot be caught, blocked or ignored, kill(2) has accepted it,
	// and killLeader continued the leader the group stop held, so the leader
	// WILL exit; only its scheduling is left, and a leader that never exits
	// after that is a kernel fault the test binary's own -timeout reports
	// with every stack. The event does not reap
	// it, so the group id stays ours for the group kill below. A wait that
	// fails is REPORTED and the kill still goes ahead: a group left running is
	// worse than one whose leader might have seen a member go.
	//
	// A LEADER STILL ALIVE PAST leaderExitReportAfter IS REPORTED, WITH ITS
	// STATE, WHILE IT IS STILL STUCK (2026-10-03): one did, in a full-suite
	// run, for fifteen minutes -- sleeping, not stopped, its SIGQUIT pending
	// unread, two fake-launchctl children stopped by the group SIGSTOP -- and
	// nothing said so until an operator dumped the test binary. The wait goes
	// on unbounded after the report; the report says what was awaited and
	// what the kernel showed.
	if _, err := awaitKilledLeader(pgid, leaderExitReportAfter, awaitExitEvent, d.leaderStalled); err != nil {
		d.t.Errorf("harness: await the daemon's exit before killing its group: %v", err)
	}
	if d.afterLeaderExit != nil {
		d.afterLeaderExit()
	}
	// THE GROUP IS KILLED AND CONTINUED, AS THE LEADER WAS (killStopped): an
	// accepted SIGKILL never resumes a member the group stop suspended. Every
	// member has the SIGKILL pending, so its next instant is its exit.
	if err := killStoppedGroup(pgid); err != nil {
		d.t.Errorf("harness: kill the daemon's group %d: %v", pgid, err)
		return false
	}
	// KILL RETURNS ONCE EVERY MEMBER HAS EXITED, NOT ONLY THE LEADER. A
	// member holds its open files until its exit, and one the daemon had
	// forked but not yet exec'd holds the daemon's own. That once included
	// its boot claim and its serving lock, as flocks: a fresh daemon started
	// the instant Kill returned read the dying member as its incumbent
	// serving and refused to boot (2026-10-15, the killed daemon forking its
	// desktop notifier), and a child that had left the group before its exec
	// did the same past this wait (2026-10-22). Those two are record locks
	// now, which no child holds (daemonaddr/recordlock.go); the check after
	// this wait says so for every Kill.
	if err := awaitGroupEnd(pgid, held.pids(), leaderExitReportAfter, procgroup.UnendedMembers, held.wait, killStoppedGroup, d.memberStalled); err != nil {
		d.t.Errorf("harness: await the exit of the killed daemon's group %d: %v", pgid, err)
	}
	// THE STATE ROOT IS FREE WHEN KILL RETURNS, by the kernel's own account:
	// the boot claim and the serving lock are record locks, which die with
	// the process that took them whatever its children hold, so no process
	// of the killed group can hold either by now. A successor that already
	// took them over holds them under its own pid, which is no violation.
	if err := killedHoldsNoLock(filepath.Join(d.StateDir, "daemon.addr"), append([]int{pgid}, held.pids()...), daemonaddr.LockHolders); err != nil {
		d.t.Errorf("harness: %v", err)
	}
	return true
}

// killedHoldsNoLock reports a boot claim or serving lock beside addrPath that
// the kernel names as held by one of KILLED, a group Kill already ended.
func killedHoldsNoLock(addrPath string, killed []int, holders func(string) (daemonaddr.Holder, daemonaddr.Holder, error)) error {
	boot, serving, err := holders(addrPath)
	if err != nil {
		return fmt.Errorf("name the locks' holders after the kill: %w", err)
	}
	for _, h := range []struct {
		lock   string
		holder daemonaddr.Holder
	}{{daemonaddr.LockName, boot}, {daemonaddr.ServingName, serving}} {
		if h.holder.PID != 0 && slices.Contains(killed, h.holder.PID) {
			return fmt.Errorf("the killed daemon's pid %d still holds %s beside %s after Kill returned", h.holder.PID, h.lock, addrPath)
		}
	}
	return nil
}

// heldExits is the exit events of a stopped group's members, taken before any
// of them was killed (holdGroupExits), by pid.
type heldExits map[int]*exitEvent

// holdGroupExits takes the exit event of every member of the stopped group
// pgid that list answers. A member the kernel refuses the registration for is
// one already exiting on its own, before the stop; it is left to
// awaitGroupEnd's listing. On an error the events taken so far stand, for the
// caller's close.
func holdGroupExits(pgid int, list func(int) ([]procgroup.Member, error), take func(int) (*exitEvent, error)) (heldExits, error) {
	held := heldExits{}
	members, err := list(pgid)
	if err != nil {
		return held, err
	}
	for _, m := range members {
		ev, err := take(m.Pid)
		if err != nil {
			return held, fmt.Errorf("member %d (%s, %s): %w", m.Pid, m.Comm, m.State, err)
		}
		if ev != nil {
			held[m.Pid] = ev
		}
	}
	return held, nil
}

// pids answers the held members' pids, in ascending order.
func (h heldExits) pids() []int {
	return slices.Sorted(maps.Keys(h))
}

// wait awaits pid's exit on its held event, else on one taken now
// (awaitExitEvent): awaitGroupEnd's wait.
func (h heldExits) wait(ctx context.Context, pid int) (bool, error) {
	ev, ok := h[pid]
	if !ok {
		return awaitExitEvent(ctx, pid)
	}
	if err := ev.await(ctx); err != nil {
		return false, err
	}
	return true, nil
}

// close releases every held event.
func (h heldExits) close() {
	for _, ev := range h {
		ev.close()
	}
}

// errHeldExitUnseen is a held member's wait answering an exit it did not see:
// a held event is registered, so its wait sees the exit or fails.
var errHeldExitUnseen = errors.New("the wait on its held exit event answered no exit")

// awaitGroupEnd waits, unbounded, for the exit of every process of group pgid,
// through awaitKilledLeader (so a stuck one is reported while the wait goes
// on). It answers once every member is one whose exit it has SEEN.
//
// THE HELD MEMBERS ARE AWAITED FIRST, WHATEVER THE LISTING SHOWS. Each was
// stopped and alive when its event was taken (holdGroupExits), so its wait
// sees its exit, and the exit event is posted only once its files are
// released. The listing is no evidence of that: on Darwin a member whose exit
// is under way drops out of it while it still holds its files.
//
// THEN THE GROUP IS LISTED, after each round, for a member the hold could not
// cover: one forked once the leader's exit orphaned the group (which continues
// its stopped members), or one already exiting before the stop. An exited
// process stays listed until it turns zombie, and is never awaited twice. Such
// a member whose exit was not seen (awaitExitEvent) is listed again until the
// listing drops it, yielding between rounds rather than spinning the core.
//
// A ROUND THAT LISTS A MEMBER STILL TO AWAIT KILLS THE GROUP AGAIN FIRST. A
// member whose fork was under way while the group's SIGKILL was sent is born
// after that signal and never receives it, so awaiting it alone waits out its
// whole life (2026-10-22: TestKillReturnsOnceEveryMemberHasReleasedItsFiles
// waited 100s on the `sleep 100` its subshell forked as the orphaned group was
// continued). A SIGKILLed process forks nothing more, so every round's kill
// reaches every member that can still exist, and the group's end is reached
// by construction rather than by the members' own exits. The group id is
// still ours for that kill, exactly as for Kill's own: the caller holds sigMu
// and the leader is unreaped.
func awaitGroupEnd(pgid int, held []int, reportAfter time.Duration, list func(int) ([]procgroup.Member, error),
	wait func(context.Context, int) (bool, error), kill func(pgid int) error, stalled func(pid int, waited time.Duration)) error {
	exited := map[int]bool{}
	for _, pid := range held {
		seen, err := awaitKilledLeader(pid, reportAfter, wait, stalled)
		if err != nil {
			return fmt.Errorf("held member %d: %w", pid, err)
		}
		if !seen {
			return fmt.Errorf("held member %d: %w", pid, errHeldExitUnseen)
		}
		exited[pid] = true
	}
	for {
		members, err := list(pgid)
		if err != nil {
			return err
		}
		var waiting []procgroup.Member
		for _, m := range members {
			if !exited[m.Pid] {
				waiting = append(waiting, m)
			}
		}
		if len(waiting) == 0 {
			return nil
		}
		if err := kill(pgid); err != nil {
			return fmt.Errorf("kill the group again for its %d members still to await: %w", len(waiting), err)
		}
		unseen := 0
		for _, m := range waiting {
			seen, err := awaitKilledLeader(m.Pid, reportAfter, wait, stalled)
			if err != nil {
				return fmt.Errorf("member %d (%s, %s): %w", m.Pid, m.Comm, m.State, err)
			}
			if !seen {
				unseen++
				continue
			}
			exited[m.Pid] = true
		}
		if unseen == len(waiting) {
			runtime.Gosched()
		}
	}
}

// leaderExitReportAfter is how long a SIGKILLed leader may take to exit before
// the wait reports it. Every observed exit is scheduled in single-digit
// milliseconds and the slowest under 16 CPU loads within reapGrace; this is
// fifteen times that, so it reports a stuck process, never a starved one. It
// does NOT end the wait.
const leaderExitReportAfter = 15 * reapGrace

// awaitKilledLeader waits, unbounded, for the exit of a leader whose SIGKILL
// was accepted, and calls stalled once if the exit has not come within
// reportAfter. It answers whether the exit itself was seen (awaitExitEvent),
// and the wait's own failure, never the stall.
func awaitKilledLeader(pid int, reportAfter time.Duration, wait func(context.Context, int) (bool, error), stalled func(pid int, waited time.Duration)) (seen bool, err error) {
	ctx, cancel := grantedclock.WithBound(context.Background(), reportAfter)
	seen, err = wait(ctx, pid)
	cancel()
	if !errors.Is(err, context.DeadlineExceeded) {
		return seen, err
	}
	stalled(pid, reportAfter)
	return wait(context.Background(), pid)
}

// leaderStalled reports a SIGKILLed, continued leader that has not exited,
// with the kernel's view of every process in its group: the evidence a stuck
// kill leaves nowhere else.
func (d *Daemon) leaderStalled(pid int, waited time.Duration) {
	d.t.Helper()
	out, err := stallListing("-g", pid)
	d.t.Errorf("harness: the daemon %d has not exited %s after its SIGKILL was accepted and it was continued; still waiting. Its group (ps err %v):\n%s", pid, waited, err, out)
}

// stallListing is what a stuck kill's report shows of the processes ps
// selects by selector ("-g" a group, "-p" one process) and id.
func stallListing(selector string, id int) ([]byte, error) {
	return exec.Command("ps", "-o", "pid,ppid,pgid,stat,wchan,flags,time,command", selector, strconv.Itoa(id)).CombinedOutput()
}

// memberStalled reports a member of a killed daemon's group that has not
// exited, with its group as the kernel shows it.
func (d *Daemon) memberStalled(pid int, waited time.Duration) {
	d.t.Helper()
	out, err := stallListing("-p", pid)
	d.t.Errorf("harness: member %d of the killed daemon's group has not exited %s after the group's SIGKILL and SIGCONT; still waiting (ps err %v):\n%s", pid, waited, err, out)
}

// killStoppedGroup SIGKILLs process group pgid and continues it
// (killStopped), through procgroup.Signal: awaitGroupEnd's kill.
func killStoppedGroup(pgid int) error {
	return killStopped(pgid, procgroup.Signal)
}

// killStopped SIGKILLs a process this harness may hold stopped, then
// SIGCONTs it, through kill. It answers the SIGKILL's error as kill(2) gave
// it, and a SIGCONT error other than ESRCH (the process already gone).
//
// ON DARWIN AN ACCEPTED SIGKILL DOES NOT CONTINUE A STOPPED PROCESS. A stop
// suspends the process's task (bsd/kern/kern_sig.c, stop():
// task_suspend_internal), and psignal's SIGKILL arm only marks the process
// runnable and thread_aborts the ONE thread it picked to run the exit; it
// never resumes the task. Only SIGCONT does (task_resume_internal). So the
// exit hung on that one thread being pulled out of its suspension, and when
// it was not, the killed process sat sleeping -- not stopped -- with its
// signals pending unread, for good: Kill's unbounded wait hung fifteen
// minutes (2026-10-03), and TestColdBringUpServesTheMainBookWithoutAsking
// failed on a leader still alive 30s after its SIGKILL, whose exit the
// report's own SIGCONT then let through (2026-10-06).
//
// CONTINUING IT OBSERVES NOTHING. The SIGKILL is already pending, so the
// process's next return to user space is its exit; and only this pid is
// continued, so every other process the harness stopped stays stopped.
func killStopped(pid int, kill func(int, syscall.Signal) error) error {
	if err := kill(pid, syscall.SIGKILL); err != nil {
		return err
	}
	if err := kill(pid, syscall.SIGCONT); err != nil && !errors.Is(err, syscall.ESRCH) {
		return fmt.Errorf("SIGCONT after its SIGKILL: %w", err)
	}
	return nil
}

// signalGroup sends sig to the daemon's process group and reports whether the
// kill should go on.
//
// The caller holds sigMu and found the reap not begun, so the group id is
// still ours. ESRCH is the benign race: the group left on its own between the
// caller's decision and this signal, and the reap that follows confirms it.
//
// EPERM IS DARWIN'S ANSWER WHEN killpg SIGNALLED NOBODY: it skips the
// group's zombies (our own exited, unreaped leader among them) and answers
// EPERM when no member was left that it would signal. That is the ordinary
// state of the group kill once the leader's exit has orphaned the group and
// the kernel's SIGHUP has ended its members -- but an EPERM was also seen with
// the group still holding a live member (2026-10-04, `killed process group
// 28508: operation not permitted` in TestColdGate's cleanup), and the old
// answer then was a report and an abandoned kill.
//
// So an EPERM is answered the way the webapp-layer harness answers it
// (wlChild.killTree): the group's LIVING members are listed from the kernel's
// process table and each is signalled by pid. Only a member that refuses its
// own signal, a listing that fails, and every other error are faults, each
// reported with the member it names, and each ends the kill.
func (d *Daemon) signalGroup(pgid int, sig syscall.Signal) bool {
	d.t.Helper()
	if err := procgroup.Signal(pgid, sig); err != nil {
		d.t.Errorf("harness: %v", err)
		return false
	}
	return true
}

// KillGroup SIGKILLs process group pgid, answering nil once every live member
// was killed or was already gone. A group whose members have all exited is no
// failure, including Darwin's EPERM for a killpg that reached only exiting
// members, which procgroup.Signal answers by signalling the live members by
// pid.
func KillGroup(pgid int) error {
	return procgroup.Signal(pgid, syscall.SIGKILL)
}

// killLeader SIGKILLs the daemon's own process, and only it, continues it out
// of the group stop (killStopped), and reports whether the kill should go on.
// The caller holds sigMu and found the reap not begun, so its pid is still
// ours, exactly as the group id is for signalGroup: ESRCH is a leader that
// already exited, which is what the caller awaits next. Every other error is a
// real fault, reported, and ends the kill.
func (d *Daemon) killLeader() bool {
	d.t.Helper()
	err := killStopped(d.cmd.Process.Pid, syscall.Kill)
	if err == nil || errors.Is(err, syscall.ESRCH) {
		return true
	}
	d.t.Errorf("harness: SIGKILL the daemon %d: %v", d.cmd.Process.Pid, err)
	return false
}

// reaped reports whether cmd.Wait has already returned for this process, which
// is what frees its pid — and with it its process group id — for reuse.
func (d *Daemon) reaped() bool {
	d.mu.Lock()
	defer d.mu.Unlock()
	return d.exited
}

// awaitReapWithin drives the process's one cmd.Wait to completion on a
// background goroutine and reports whether it finished within budget, granted
// (grantedclock.WithBound). The waitOnce still guarantees exactly one Wait per
// process; moving the blocking call off the caller's goroutine is what makes
// the caller's wait bounded.
func (d *Daemon) awaitReapWithin(budget time.Duration) bool {
	reaped := make(chan struct{})
	go func() {
		d.waitOnce.Do(d.wait)
		close(reaped)
	}()
	ctx, cancel := grantedclock.WithBound(context.Background(), budget)
	defer cancel()
	select {
	case <-reaped:
		return true
	case <-ctx.Done():
		return false
	}
}

func (d *Daemon) signal(sig syscall.Signal) {
	d.t.Helper()
	d.sigMu.Lock()
	defer d.sigMu.Unlock()
	if d.reapBegun {
		return
	}
	if err := d.cmd.Process.Signal(sig); err != nil && !errors.Is(err, os.ErrProcessDone) {
		d.t.Fatalf("harness: signal %v: %v", sig, err)
	}
}

// Wait blocks until the process has left and answers its exit status.
func (d *Daemon) Wait() int {
	d.waitOnce.Do(d.wait)
	d.mu.Lock()
	defer d.mu.Unlock()
	var exit *exec.ExitError
	if errors.As(d.exitErr, &exit) {
		return exit.ExitCode()
	}
	return 0
}

// wait reaps the process, as sigMu's owner: it awaits the exit on the kernel's
// exit event, which does not reap, then takes sigMu and marks the reap begun
// before cmd.Wait frees the pid, so no signal is ever in flight across the
// reap. A failed exit wait is kept in exitErr, where every reader of the exit
// sees it, and the reap still goes ahead: cmd.Wait blocks on the exit itself.
func (d *Daemon) wait() {
	exitErr := WaitProcessExit(context.Background(), d.cmd.Process.Pid)
	d.sigMu.Lock()
	d.reapBegun = true
	d.sigMu.Unlock()
	err := d.cmd.Wait()
	if exitErr != nil {
		err = errors.Join(fmt.Errorf("harness: await the daemon's exit before the reap: %w", exitErr), err)
	}
	d.mu.Lock()
	d.exited, d.exitErr = true, err
	d.mu.Unlock()
}

// Exited reports whether the process has already left.
func (d *Daemon) Exited() bool {
	if d.cmd == nil || d.cmd.Process == nil {
		return true
	}
	d.mu.Lock()
	already := d.exited
	d.mu.Unlock()
	if already {
		return true
	}
	// Signal 0 probes liveness without disturbing the process, and like
	// every signal it is sent only while no reap can free the pid.
	d.sigMu.Lock()
	defer d.sigMu.Unlock()
	if d.reapBegun || d.cmd.Process.Signal(syscall.Signal(0)) != nil {
		return true
	}
	// A ZOMBIE HAS EXITED. Signal 0 succeeds against a process that has run to
	// completion but has not been reaped, and this harness reaps only in Wait
	// — which the tests asking this question have deliberately NOT called. The
	// probe alone therefore reported a daemon that had already gone as still
	// running, which is how a drain test asserting "the daemon is still up"
	// passed against a daemon that had exited milliseconds earlier. The kernel
	// is the only witness left, so it is asked.
	return isZombie(d.cmd.Process.Pid)
}

// isZombie reports whether a pid names a process that has exited -- a zombie
// waiting to be reaped, or one whose exit is under way. An unreadable state is NOT read as exited: the caller
// already has the signal probe's answer, and guessing here would turn a
// failed read into a false exit report. The state is the kernel's, read in
// process (procstate) rather than by a `ps` per probe.
func isZombie(pid int) bool {
	st, err := readProcessState(pid)
	if err != nil {
		return false
	}
	return st.exited
}

// AwaitExit waits for the process to leave and answers its exit status,
// failing the test if it outlives ONE wait's bound.
func (d *Daemon) AwaitExit() int {
	d.t.Helper()
	wait, cancelWait := d.waitCtx()
	defer cancelWait()
	done := make(chan int, 1)
	go func() { done <- d.Wait() }()
	select {
	case code := <-done:
		return code
	case <-wait.Done():
		d.t.Fatalf("daemon did not exit: %v\nstderr:\n%s", wait.Err(), d.Stderr())
		return -1
	}
}

// Stderr is everything the daemon wrote to its terminal mirror.
func (d *Daemon) Stderr() string {
	raw, err := os.ReadFile(d.stderrPath)
	if err != nil {
		return ""
	}
	return string(raw)
}

// Register registers a repository's worktree and answers the minted ref.
func Register(t testing.TB, d *Daemon, dir string) *workspacev1.WorkspaceRef {
	t.Helper()
	resp, err := d.Client().RegisterWorkspace(d.Ctx(), connect.NewRequest(&agentreplv1.RegisterWorkspaceRequest{Dir: dir}))
	if err != nil {
		t.Fatalf("RegisterWorkspace(%s) = error %v, want a minted id", dir, err)
	}
	ref := resp.Msg.GetSuccess().GetWorkspace()
	if ref.GetId() == "" {
		t.Fatalf("RegisterWorkspace(%s) = %v, want a success carrying a workspace id", dir, resp.Msg)
	}
	// A VIEW ANSWERS FOR WHAT IT REGISTERS (view.go): the daemon may name the
	// workspace by the directory given or by its own resolved form.
	if d.view {
		d.Claim(dir)
		if ref.GetDir() != dir {
			d.Claim(ref.GetDir())
		}
	}
	return ref
}

// SocketPath is the per-workspace shim UDS the daemon spawns against.
func (d *Daemon) SocketPath(ws *workspacev1.WorkspaceRef) string {
	return filepath.Join(d.StateDir, "sock", ws.GetId()+".sock")
}

// WriteShimProfile files a fake-shim startup profile for a workspace
// directory, to be read when the daemon spawns the shim there.
func (d *Daemon) WriteShimProfile(dir string, profile any) {
	d.t.Helper()
	writeJSON(d.t, filepath.Join(d.ProfileDir, profileFileName(dir)), profile)
}

// WriteDefaultShimProfile scripts EVERY fake shim this daemon spawns, whatever
// workspace it serves (fakeshim's `default.json` fallback).
//
// It exists for the workspace whose dir the test does not know in advance: a
// CREATE mints the dir, so there is no key to write a per-workspace profile
// under until the verb whose behavior is under test has already run. A
// per-workspace profile still wins over this one.
func (d *Daemon) WriteDefaultShimProfile(profile any) {
	d.t.Helper()
	writeJSON(d.t, filepath.Join(d.ProfileDir, "default.json"), profile)
}

// ExpectFileUnchanged asserts a file still holds exactly `want` after the
// probe window. It is a negative assertion, so it necessarily waits out a
// bound rather than synchronizing on an event.
func (d *Daemon) ExpectFileUnchanged(path, want string, probe time.Duration) {
	d.t.Helper()
	deadline := time.Now().Add(probe)
	ticker := time.NewTicker(pollInterval)
	defer ticker.Stop()
	for time.Now().Before(deadline) {
		body, err := os.ReadFile(path)
		if err != nil {
			if want == "" {
				<-ticker.C
				continue
			}
			d.t.Fatalf("read %s: %v, want it to still hold %q", path, err, want)
		}
		if string(body) != want {
			d.t.Fatalf("%s = %q, want it unchanged at %q", path, body, want)
		}
		<-ticker.C
	}
}

// sparedFromReaping holds the pids of processes a TEST owns and stands down
// itself. Nothing else may reap them.
//
// WHY IT EXISTS. strayPIDs keys on "this run's state directory appears in the
// process's argv", which is an exact key for the shims the daemon spawned —
// and an OVER-broad one for a world that also routes its own store's and
// sidecar's log files into that same state root (e2e/world_test.go's logsDir).
// Those two processes are not strays: the test starts them, stops them, and
// asserts that they were still running when it ended. Reaping one SIGKILLs it
// with no exit record and turns the world's own "exited before cleanup" check
// into a failure whose cause has left no trace.
var (
	sparedMu          sync.Mutex
	sparedFromReaping = map[int]bool{}
)

// SpareFromStrayReaping declares that a pid belongs to the TEST rather than to
// the daemon's process tree, so no Daemon's ReapStrays will signal it. The
// exemption is dropped at the end of the test that declared it, because the pid
// is free for reuse the moment that process is reaped.
func SpareFromStrayReaping(t testing.TB, pid int) {
	t.Helper()
	sparedMu.Lock()
	sparedFromReaping[pid] = true
	sparedMu.Unlock()
	t.Cleanup(func() {
		sparedMu.Lock()
		delete(sparedFromReaping, pid)
		sparedMu.Unlock()
	})
}

// SparedFromStrayReaping answers whether a pid was declared the TEST's own
// through SpareFromStrayReaping.
//
// IT IS EXPORTED SO THERE IS ONE REGISTRY, NOT TWO. The Emacs layer keeps a
// reaper of its own (e2e/emacs_test.go's findStrays) because it keys on a
// scenario ROOT rather than on a state directory, and that reaper also has to
// know which processes belong to the test. A second list of exemptions would
// drift from this one the moment either side gained a process, and the drift
// is not visible as a failure: it shows up as a teardown WAIT that can never
// come up empty, which is exactly what it did.
func SparedFromStrayReaping(pid int) bool {
	sparedMu.Lock()
	defer sparedMu.Unlock()
	return sparedFromReaping[pid]
}

// ErrLeakedProcess names a process this test started that is still running
// after its teardown's reap. It is the teardown's own failure: whatever the
// test asserted, a leaked shim holds its workspace lock and memory for as long
// as the machine is up.
var ErrLeakedProcess = errors.New("harness: a process this test started outlived its teardown")

// maxReapRounds bounds ReapStrays' rounds. Every round after the first needs
// a stray to have escaped the previous round's stop inside execve (453 of
// 3000 for a process stopped just after its fork; see Kill), so a sweep
// still going after eight rounds is being fed by a spawner no listing names,
// and more rounds would not end it.
const maxReapRounds = 8

// ReapStrays kills every process whose command line names this run's state
// directory, whatever process group it is in — except a pid the test declared
// its own through SpareFromStrayReaping — waits for each to exit, and repeats
// until a listing names no live process; it then fails the test with
// ErrLeakedProcess if any such process is still running.
//
// IT IS THE ONLY THING THAT BOUNDS A TEST'S PROCESS TREE. The daemon runs in
// its own process group and Kill ends that group, but every shim the daemon
// spawns is put in a group of ITS own (the spawn's process-group discipline),
// so a daemon that dies without standing its shims down leaves them running —
// and a leaked daemon keeps prelaunching more. The state directory is unique to
// this run and appears in both the daemon's argv and every shim's `--listen`
// path, so it is an exact key for "processes this test started".
//
// THE GUARANTEE RESTS ON EXITS, NOT ON STOPS. A stray can be a LIVE DAEMON the
// harness never started: a layout change's replacement (and a handover's
// successor) is spawned by the incumbent into a session of its own, so no Kill
// ever reaches it. A snapshot-then-kill sweep raced it: the replacement
// brought a pending workspace's session up — or revived a shim the sweep had
// just killed — between the `ps` snapshot and its own SIGKILL, and the new
// shim, in no snapshot, outlived the test (2026-09-27: an orphaned fakeshim of
// TestALayoutChangeRestartsTheDaemonAndItsReplacementServes, reparented to
// launchd). Stopping every stray first narrows that, but a stop is revocable:
// on Darwin a SIGSTOP that reaches a process inside execve is discarded when
// the exec completes (see Kill), so a stray just spawned can run on through
// the freeze and spawn again. So the sweep runs in ROUNDS, each one a freeze
// of every listed stray (containment: a stray the stop holds forks nothing
// and observes no other's death), a SIGKILL of each, and the kernel's exit
// event for each, and it ends only on a listing that names no live process.
// An exit cannot be undone, so every process a round lists is gone before
// the next listing, and whatever that listing names was spawned by a process
// that ran after its round's stop: a stray that escaped it.
//
// TERMINATION: every round kills everything it lists, so a round after the
// first exists only because a stray escaped the previous round's stop and
// spawned before its SIGKILL landed, which needs it to have been inside
// execve at that stop; a spawner the stops always hold ends in two rounds.
// The rounds are COUNTED, not timed: maxReapRounds ends a sweep that a source
// of strays never listed (a spawner whose argv does not name the state
// directory) would otherwise feed forever, and the leak check then names what
// it left running.
func (d *Daemon) ReapStrays() {
	d.t.Helper()
	for round, again := 0, true; again && round < maxReapRounds; round++ {
		killed, err := d.freezeStrays()
		// A freeze that failed (a listing that could not be read, a stop the
		// kernel refused) still has what it stopped killed, and ends the
		// rounds: repeating it would only repeat the report, and the leak
		// check below still names whatever is left.
		if err != nil {
			d.t.Errorf("harness: freeze the strays before the kill: %v", err)
			again = false
		}
		live := d.liveOf(killed)
		if len(live) == 0 {
			break
		}
		var killedNow []int
		for _, pid := range live {
			// Each is continued as it is killed (killStopped): the freeze
			// suspended it, and a SIGKILL alone does not end a suspended
			// process on Darwin.
			if err := killStopped(pid, syscall.Kill); err != nil && !errors.Is(err, syscall.ESRCH) {
				d.t.Errorf("harness: SIGKILL stray %d: %v", pid, err)
				continue
			}
			killedNow = append(killedNow, pid)
		}
		// EACH EXIT IS AWAITED ON THE EXIT ITSELF, NOT RACED AGAINST A CLOCK,
		// as in Kill: SIGKILL cannot be caught, blocked or ignored, kill(2)
		// accepted it and the stray was continued, so the exit is decided and
		// only its scheduling is left.
		// Under 16 CPU loads a clock bound here failed 2 of 10000 sweeps on
		// strays that had done exactly what they were told. A stray that never
		// exits after an accepted SIGKILL is a kernel fault the test binary's
		// own -timeout reports with every stack.
		began := time.Now()
		for _, pid := range killedNow {
			if err := WaitProcessExit(context.Background(), pid); err != nil {
				d.t.Errorf("harness: await the SIGKILLed stray %d's exit: %v", pid, err)
			}
		}
		if took := time.Since(began); took > reapGrace {
			d.t.Logf("harness: the SIGKILLed strays took %s to exit; the host was starving them", took)
		}
	}
	if err := d.leakedStrays(); err != nil {
		d.t.Error(err)
	}
}

// liveOf answers the pids that have not exited. One whose state cannot be
// read is kept, so the kill and the leak check still see it, and reported.
func (d *Daemon) liveOf(pids []int) []int {
	d.t.Helper()
	var live []int
	for _, pid := range pids {
		state, err := readProcessState(pid)
		if err != nil {
			d.t.Errorf("harness: read the state of stray %d: %v", pid, err)
			live = append(live, pid)
			continue
		}
		if !state.exited {
			live = append(live, pid)
		}
	}
	return live
}

// freezeStrays SIGSTOPs every stray, re-listing until a listing names none it
// has not stopped, and answers the ones the final listing still names: those
// are the processes to kill. A pid stopped earlier that the final listing no
// longer names has exited — or, having exited, was recycled into a stranger's
// process before the stop reached it — so it is continued, never killed.
func (d *Daemon) freezeStrays() ([]int, error) {
	stopped := map[int]bool{}
	for {
		strays, err := d.listStrays()
		if err != nil {
			return d.settleFrozen(stopped, nil), err
		}
		var fresh []stray
		for _, s := range strays {
			if !stopped[s.pid] {
				fresh = append(fresh, s)
			}
		}
		if len(fresh) == 0 {
			listed := map[int]bool{}
			for _, s := range strays {
				listed[s.pid] = true
			}
			if d.afterStraysFrozen != nil && len(strays) > 0 {
				d.afterStraysFrozen()
			}
			return d.settleFrozen(stopped, listed), nil
		}
		for _, s := range fresh {
			err := stopFrozen(s.pid, stopFrozenReportAfter, syscall.Kill, d.strayStopStalled(s))
			if errors.Is(err, syscall.ESRCH) {
				continue
			}
			stopped[s.pid] = true
			if err != nil {
				return d.settleFrozen(stopped, nil), fmt.Errorf("stop stray %d (%s): %w", s.pid, s.args, err)
			}
		}
	}
}

// strayStopStalled reports a stray whose stop has not held after waited, with
// the kernel's view of the process: the evidence a stuck stop leaves nowhere
// else. The sweep goes on waiting.
func (d *Daemon) strayStopStalled(s stray) func(pid int, state processState, waited time.Duration) {
	return func(pid int, state processState, waited time.Duration) {
		d.t.Helper()
		out, err := exec.Command("ps", "-M", "-o", "pid,stat,pri,wchan,flags,time,command", "-p", strconv.Itoa(pid)).CombinedOutput()
		reportStrayStall(d.t, s, pid, state, waited, fmt.Sprintf("(ps err %v):\n%s", err, out))
	}
}

// stallReporter is the part of testing.TB a stall report writes to.
type stallReporter interface {
	Helper()
	Errorf(format string, args ...any)
	Logf(format string, args ...any)
}

// reportStrayStall reports a stray whose stop has not held after waited, on
// the KERNEL'S account of why, not on the duration.
//
// A STOP WHOSE LAST THREADS AWAIT THE CPU IS STARVED, NOT STUCK: it is
// LOGGED. The kernel has posted the stop and suspended the task, and every
// thread still to take it is runnable, so it holds the moment the scheduler
// runs them. On a host loaded by other work a test process's threads at
// background QoS (a shim's FSEvents dispatch thread, priority 4) waited past
// thirty seconds for that (2026-10-14, load average 81 on 16 cores:
// TestDisplacedTurnCapturedEndedThenResubmittedExactlyOnce). Anything else --
// a stop not yet posted, a thread blocked rather than runnable -- is a stop
// that may never hold, and FAILS the test while the wait goes on. A starved
// stop that never gets the CPU is still bounded, by the test's own timeout.
func reportStrayStall(r stallReporter, s stray, pid int, state processState, waited time.Duration, view string) {
	r.Helper()
	if state.awaitingCPU {
		r.Logf("harness: stray %d (%s) is still %s %s after its SIGSTOP; its stop is posted and its remaining threads await only the CPU, so it is starved, not stuck; still waiting. The kernel's view %s", pid, s.args, state.name, waited, view)
		return
	}
	r.Errorf("harness: stray %d (%s) is still %s %s after its SIGSTOP; still waiting. The kernel's view %s", pid, s.args, state.name, waited, view)
}

// settleFrozen answers the stopped pids to kill: those listed (every stopped
// one when listed is nil, as after a failed freeze, when killing a stranger
// is the lesser harm than leaving a stray running). The rest are continued.
func (d *Daemon) settleFrozen(stopped, listed map[int]bool) []int {
	var kill []int
	for pid := range stopped {
		if listed == nil || listed[pid] {
			kill = append(kill, pid)
			continue
		}
		if err := syscall.Kill(pid, syscall.SIGCONT); err != nil && !errors.Is(err, syscall.ESRCH) {
			d.t.Errorf("harness: SIGCONT pid %d that left the stray set: %v", pid, err)
		}
	}
	return kill
}

// leakedStrays answers ErrLeakedProcess naming every process that still names
// this run's state directory and has not exited, or nil when there is none.
// A zombie has exited: only its reaping, by a parent that is not this test,
// is left.
func (d *Daemon) leakedStrays() error {
	strays, err := d.listStrays()
	if err != nil {
		return fmt.Errorf("harness: list the processes left after the reap: %w", err)
	}
	var leaked []string
	for _, s := range strays {
		state, err := readProcessState(s.pid)
		if err != nil {
			return fmt.Errorf("harness: read the state of process %d left after the reap: %w", s.pid, err)
		}
		if !state.exited {
			leaked = append(leaked, fmt.Sprintf("pid %d (%s): %s", s.pid, state.name, s.args))
		}
	}
	if len(leaked) == 0 {
		return nil
	}
	return fmt.Errorf("%w: %d process(es) under %s: %s", ErrLeakedProcess, len(leaked), d.StateDir, strings.Join(leaked, "; "))
}

// StrayPIDs answers the live processes naming this run's state directory,
// excluding the harness's own process. A test asserts on it; ReapStrays acts
// on it. A listing that cannot be read fails the test and answers none.
func (d *Daemon) StrayPIDs() []int { return d.strayPIDs() }

func (d *Daemon) strayPIDs() []int {
	d.t.Helper()
	strays, err := d.listStrays()
	if err != nil {
		d.t.Errorf("harness: list the strays: %v", err)
		return nil
	}
	pids := make([]int, 0, len(strays))
	for _, s := range strays {
		pids = append(pids, s.pid)
	}
	return pids
}

// stray is one process naming this run's state directory.
type stray struct {
	pid  int
	args string
}

// listStrays reads the process table for every process naming this run's
// state directory, excluding the harness's own process and every pid a test
// declared its own.
func (d *Daemon) listStrays() ([]stray, error) {
	if d.StateDir == "" {
		return nil, nil
	}
	table, err := processTable()
	if err != nil {
		return nil, err
	}
	self := os.Getpid()
	var strays []stray
	for _, p := range table {
		if !namesPath(p.args, d.StateDir) || p.pid == self || SparedFromStrayReaping(p.pid) {
			continue
		}
		strays = append(strays, stray{pid: p.pid, args: strings.TrimSpace(p.args)})
	}
	return strays, nil
}

// namesPath reports whether line names dir itself or a path beneath it: an
// occurrence of dir that ends the line, or is followed by a separator or by
// whitespace.
//
// A BARE SUBSTRING IS NOT A PATH. Two ShortTempDir roots are `ar` plus a
// random decimal, so one can be a textual prefix of its sibling (`ar12` of
// `ar123`), and a state root handed to StartDaemon without a trailing
// component (TestBootOpensTheWorkspaceStateFresh's) would then have matched —
// and ReapStrays SIGKILLed — the other test's daemon and shims.
func namesPath(line, dir string) bool {
	for rest := line; ; {
		i := strings.Index(rest, dir)
		if i < 0 {
			return false
		}
		after := rest[i+len(dir):]
		if after == "" || after[0] == filepath.Separator || after[0] == ' ' || after[0] == '\t' {
			return true
		}
		rest = rest[i+1:]
	}
}

// ProjectDir answers where the vendor CLI files one workspace's conversations
// under one account root: `projects/<every non-alphanumeric byte of the
// absolute cwd replaced by a dash>`.
func ProjectDir(configDir, workspaceDir string) string {
	return filepath.Join(configDir, "projects", projectDirRule.ReplaceAllString(workspaceDir, "-"))
}

var projectDirRule = regexp.MustCompile(`[^A-Za-z0-9]`)

// RemoveTranscripts deletes every transcript a workspace has under BOTH account
// roots, so a re-open resumes a conversation whose transcript is gone. That is
// the state the daemon's resume guard exists for, and nothing else in the
// harness can produce it: the fake shim lays a transcript down at every
// StartSession, exactly as the vendor does.
func (d *Daemon) RemoveTranscripts(workspaceDir string) {
	d.t.Helper()
	for _, root := range []string{d.DefaultConfigDir, d.MultiRepoConfigDir} {
		if root == "" {
			continue
		}
		if err := os.RemoveAll(ProjectDir(root, workspaceDir)); err != nil {
			d.t.Fatalf("harness: remove the transcripts under %s: %v", root, err)
		}
	}
}

// FeedPageSize is the fake store's page size in ENTRIES
// (fakeshim.DefaultHistoryPageSize): a page is the store's page and the daemon
// states none of its own (feed paging on demand). A walk test pushing one
// row-drawing entry per row and more than this many is guaranteed a second
// page. The fakeshim is its own main package, so the value is restated here;
// fakeshim's book_test pins the two together.
const FeedPageSize = 50

// TranscriptPath answers where the vendor CLI files one conversation's
// transcript under an account root: `<ProjectDir>/<vendor session id>.jsonl`.
func TranscriptPath(configDir, workspaceDir, vendorSessionID string) string {
	return filepath.Join(ProjectDir(configDir, workspaceDir), vendorSessionID+".jsonl")
}

// HasTranscript reports whether a workspace's transcript exists under a root.
func HasTranscript(configDir, workspaceDir, vendorSessionID string) bool {
	_, err := os.Stat(TranscriptPath(configDir, workspaceDir, vendorSessionID))
	return err == nil
}
