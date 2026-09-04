package harness

import (
	"context"
	"crypto/tls"
	"errors"
	"net"
	"net/http"
	"os"
	"os/exec"
	"path/filepath"
	"regexp"
	"strconv"
	"strings"
	"sync"
	"syscall"
	"testing"
	"time"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	"agentrepl/proto/agentrepl/v1/agentreplv1connect"
	workspacev1 "agentrepl/proto/workspace/v1"

	"claude-repld/integration/fakegit"
	"claude-repld/internal/resolve/feed"
	"claude-repld/internal/stateroot"

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
const DefaultTimeout = 5 * time.Second

// HandoverChainTimeout bounds the few tests whose single Daemon context must
// span an entire self-reload handover: a merge landing, the rollout trigger,
// a SECOND real claude-repld's full boot and workspace adoption, and the
// incumbent's own orderly exit — all chained on the ONE budget that started
// ticking at the incumbent's own process start, never a fresh context of its
// own. That is structurally two real process lifecycles, not one, so it gets
// 3x DefaultTimeout rather than sharing the ordinary bound: run 8's observed
// max (1.7s) already covers this chain today, so 15s leaves ample headroom
// for exactly the case that is most likely to grow (a slower deploy chain or
// a slower successor boot) without reintroducing a blanket 30s bound.
const HandoverChainTimeout = 3 * DefaultTimeout

// pollInterval is how often a file-existence wait re-checks. Nothing in the
// harness sleeps to let another party make progress.
const pollInterval = 5 * time.Millisecond

// FakeShimDefaultBuildSHA is the runtime shim_build_sha the fake shim reports
// when FAKESHIM_BUILD_SHA is unset. It duplicates fakeshim's own
// DefaultBuildSHA because the fake is a program, not an importable package;
// the two constants are documented on each other and move together.
const FakeShimDefaultBuildSHA = "fake"

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

// Opts configures one daemon process.
type Opts struct {
	// StateDir overrides the state root; empty mints a fresh temp one.
	StateDir string
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
	// SelfRepo names the daemon's own checkout via AGENT_REPL_SELF_REPO_DIR,
	// so a merge target can be recognized as the emacs repo. The self-reload
	// trigger stays ON under this override: test safety comes from
	// AGENT_REPL_DEPLOY_SCRIPT naming the fake deploy script, so landed range
	// to rollout trigger to deploy is assertable end to end.
	SelfRepo string
	// MultiRepoRoot is the tree whose workspaces use the multi-repo account.
	MultiRepoRoot string
	// DefaultAccountEmail is written into the default config root's
	// .claude.json; empty leaves the root logged out.
	DefaultAccountEmail string
	// MultiRepoAccountEmail is written into the multi-repo config root.
	MultiRepoAccountEmail string
	// NoFake starts the daemon WITHOUT `--fake` and without
	// AGENT_REPL_CLAUDE_BIN, so every vendor call site (the classifier's
	// headless run, the login pty) reaches its real implementation and is
	// refused by the vendor guard. It is the only way to exercise the
	// guard's refusal sites; the fake shim is unaffected, since `--node`
	// still names it.
	NoFake bool
	// JSONCodec dials the daemon with the JSON codec instead of binary.
	JSONCodec bool
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

// Daemon is one running claude-repld process and the client dialed to it.
type Daemon struct {
	// StateDir is the daemon's state root.
	StateDir string
	// Addr is the loopback address it serves on, read from daemon.addr.
	Addr string
	// ProfileDir holds the fake shim's per-workspace startup profiles.
	ProfileDir string
	// LockDir is the redirected kernel-lock directory.
	LockDir string
	// Browser, Deploy record what the daemon invoked.
	Browser *Recorder
	Deploy  *Recorder
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

	t          *testing.T
	ctx        context.Context
	cmd        *exec.Cmd
	stderrPath string
	client     agentreplv1connect.AgentReplClient
	http       *http.Client

	mu            sync.Mutex
	workspaceDirs []string
	exited        bool
	exitErr       error
	waitOnce      sync.Once
	expected      map[string]bool
	shims         map[string]*ShimControl
}

// installFakeGit copies the scripted `git` into the directory that leads the
// daemon's PATH.
func installFakeGit(t *testing.T, dir string) {
	t.Helper()
	if err := os.MkdirAll(dir, 0o755); err != nil {
		t.Fatalf("harness: mkdir %s: %v", dir, err)
	}
	body, err := os.ReadFile(FakeGitBinary(t))
	if err != nil {
		t.Fatalf("harness: read the fake git: %v", err)
	}
	if err := os.WriteFile(filepath.Join(dir, "git"), body, 0o755); err != nil {
		t.Fatalf("harness: install the fake git: %v", err)
	}
}

// gitEnvKeys are the repository-selecting variables that must never be
// inherited by a git child: a hook-leaked GIT_DIR is a real, previously
// observed source of bogus work-tree errors.
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
func StartDaemon(t *testing.T, opts Opts) *Daemon {
	t.Helper()
	binary := DaemonBinary(t)
	root := t.TempDir()
	// Unix-domain socket paths are capped at 103 bytes, and t.TempDir() encodes
	// the whole test name, which blows that budget for the longer names in this
	// suite. Everything the daemon opens a SOCKET under lives beneath a short
	// root of its own; everything else stays under t.TempDir().
	sockRoot := shortTempDir(t)

	d := &Daemon{
		StateDir:           opts.StateDir,
		ProfileDir:         filepath.Join(root, "shim-profiles"),
		PromptsDir:         CopyPrompts(t, filepath.Join(root, "prompts")),
		WebappDir:          NewFakeWebappDist(t, filepath.Join(root, "dist")),
		DefaultConfigDir:   NewConfigRoot(t, filepath.Join(root, "config-default"), accountEmail(opts.DefaultAccountEmail, "default@example.invalid")),
		MultiRepoConfigDir: NewConfigRoot(t, filepath.Join(root, "config-multi"), accountEmail(opts.MultiRepoAccountEmail, "multi@example.invalid")),
		StoreSocket:        opts.StoreSocket,
		Browser:            NewFakeBrowser(t, filepath.Join(root, "bin")),
		Deploy:             NewFakeDeployScript(t, filepath.Join(root, "bin")),
		t:                  t,
		expected:           map[string]bool{},
		shims:              map[string]*ShimControl{},
	}
	if d.StateDir == "" {
		d.StateDir = filepath.Join(sockRoot, "state")
		if err := os.MkdirAll(d.StateDir, 0o755); err != nil {
			t.Fatalf("harness: mkdir state root: %v", err)
		}
	}
	if d.StoreSocket == "" {
		d.StoreSocket = filepath.Join(sockRoot, "store.sock")
	}
	// The warning sweep is UNCONDITIONAL: every daemon sweeps its logs at test
	// end with an empty expected set, so a test that never calls ExpectWarnings
	// still gets the assertion. ExpectWarnings only widens this set.
	t.Cleanup(d.assertNoUnexpectedWarnings)
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
	d.LockDir = filepath.Join(filepath.Dir(d.StateDir), "locks")
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
	fakeClaude := NewFakeClaude(t, fakeBin)
	// The scripted `git` goes first on the daemon's PATH, so every git fact the
	// daemon reads comes out of this test's fixture file and the real binary is
	// never reached.
	d.Git = World(t)
	installFakeGit(t, fakeBin)

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
	ctx, cancel := context.WithTimeout(context.Background(), timeout)
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
		"AGENT_REPL_FORBID_VENDOR_CALLS=1",
		"AGENT_REPL_LOCK_DIR="+d.LockDir,
		"AGENT_REPL_STORE_SOCKET="+filepath.Join(sockRoot, "unused-store.sock"),
		"MULTI_REPO_ROOT="+multiRoot,
		"AGENT_REPL_BROWSER_CMD="+d.Browser.Path,
		"AGENT_REPL_DEPLOY_SCRIPT="+d.Deploy.Path,
		fakegit.EnvStateFile+"="+d.Git.StateFile,
		"FAKESHIM_PROFILE_DIR="+d.ProfileDir,
		// HOME and PATH aside, the build identity is stated in one place only.
		"HOME="+root,
		"PATH="+fakeBin+string(os.PathListSeparator)+os.Getenv("PATH"),
	)
	env = append(env, BuildIdentityEnv(PinnedCheckout(t))...)
	// The daemon's OWN checkout identity is always overridden, whether or not
	// a test cares which repository it is. The merge orchestrator's two
	// methods key on it, so it resolves that identity for EVERY merge -- and
	// left at the real checkout the scripted `git` on PATH knows nothing about
	// it, refuses, and the merge aborts before it starts. A test that does not
	// name one gets a fake repository that is deliberately no test repository,
	// so the comparison resolves and answers "not the self repo".
	selfRepo := opts.SelfRepo
	if selfRepo == "" {
		selfRepo = NewRepoAt(t, filepath.Join(root, "self-repo")).Dir
	}
	env = append(env, "AGENT_REPL_SELF_REPO_DIR="+selfRepo)
	// THE VENDOR BINARY IS THE GUARD'S OTHER HALF. Left unset, the login pty
	// falls back to the default `claude` and the guard refuses it; naming the
	// fake claude is what makes the ordinary tests spawn something harmless.
	// NoFake therefore withholds it deliberately.
	if !opts.NoFake {
		env = append(env, "AGENT_REPL_CLAUDE_BIN="+fakeClaude)
	} else {
		// The SHIMS stay fake even with the whole stack's fake mode off:
		// --node names the fake shim, but the daemon cannot know that and its
		// vendor guard refuses a non-fake spawn before any session exists.
		// Without this, NoFake could never reach a REAL vendor call site that
		// needs a live session — which is the only thing NoFake is for.
		env = append(env, "AGENT_REPL_FAKE_SHIMS=1")
	}
	// COVERAGE, WHEN THE RUN ASKED FOR IT. GOCOVERDIR is this daemon's own
	// counter directory; NODE_V8_COVERAGE is inherited by every shim the
	// daemon spawns (shimclient.spawnEnv copies the daemon's environment
	// forward verbatim outside its fixed override set). Both are absent
	// entirely on an ordinary run.
	if root := CoverageRoot(); root != "" {
		daemonCov, err := CoverageEnv(root, "claude-repld")
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
	if opts.Joining == "" && !opts.ExpectEarlyExit {
		if err := os.Remove(filepath.Join(d.StateDir, "daemon.addr")); err != nil && !os.IsNotExist(err) {
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
	t.Cleanup(func() {
		// A COVERAGE RUN ASKS FOR A GRACEFUL EXIT FIRST. The Go runtime
		// writes an instrumented binary's counters as it leaves through
		// main; a SIGKILLed daemon writes nothing, so the cleanup kill
		// below would discard every counter the run just earned. This adds
		// a bounded SIGTERM ahead of the kill and never replaces it: a
		// daemon that ignores the signal, or one the test already killed,
		// still gets the unconditional Kill/ReapStrays underneath.
		d.gracefulStopForCoverage()
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

// shortTempDir mints a directory directly under /tmp, short enough that a
// state root beneath it still fits a unix-domain socket path. t.TempDir()
// cannot be used: it encodes the test's whole name.
func shortTempDir(t *testing.T) string {
	t.Helper()
	dir, err := os.MkdirTemp("/tmp", "ar")
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
func requireSocketPathBudget(t *testing.T, stateDir string) {
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

// AwaitAddrFile waits for daemon.addr to appear and answers its address,
// asserting the contracted `127.0.0.1:<port>\n` shape.
func (d *Daemon) AwaitAddrFile() string {
	d.t.Helper()
	raw := d.awaitFile(d.AddrFile())
	if !strings.HasSuffix(raw, "\n") {
		d.t.Fatalf("daemon.addr = %q, want a trailing newline", raw)
	}
	addr := strings.TrimSuffix(raw, "\n")
	host, _, err := net.SplitHostPort(addr)
	if err != nil {
		d.t.Fatalf("daemon.addr = %q, want 127.0.0.1:<port>: %v", raw, err)
	}
	if host != "127.0.0.1" {
		d.t.Fatalf("daemon.addr host = %q, want the loopback address", host)
	}
	return addr
}

// awaitFile polls for a file, bounded by the daemon's context.
func (d *Daemon) awaitFile(path string) string {
	d.t.Helper()
	ticker := time.NewTicker(pollInterval)
	defer ticker.Stop()
	for {
		if body, err := os.ReadFile(path); err == nil && len(body) > 0 {
			return string(body)
		}
		if d.Exited() && !d.expectedExit() {
			d.t.Fatalf("daemon exited before writing %s (exit %v)\nstderr:\n%s", filepath.Base(path), d.exitErr, d.Stderr())
		}
		select {
		case <-ticker.C:
		case <-d.ctx.Done():
			d.t.Fatalf("waiting for %s: %v\nstderr:\n%s", path, d.ctx.Err(), d.Stderr())
		}
	}
}

// AwaitFileGone waits for a path to disappear.
func (d *Daemon) AwaitFileGone(path string) {
	d.t.Helper()
	ticker := time.NewTicker(pollInterval)
	defer ticker.Stop()
	for {
		if _, err := os.Stat(path); errors.Is(err, os.ErrNotExist) {
			return
		}
		select {
		case <-ticker.C:
		case <-d.ctx.Done():
			d.t.Fatalf("waiting for %s to be removed: %v", path, d.ctx.Err())
		}
	}
}

// AwaitFileExists waits for a path to appear, bounded by the daemon's context.
func (d *Daemon) AwaitFileExists(path string) {
	d.t.Helper()
	ticker := time.NewTicker(pollInterval)
	defer ticker.Stop()
	for {
		if _, err := os.Stat(path); err == nil {
			return
		}
		select {
		case <-ticker.C:
		case <-d.ctx.Done():
			d.t.Fatalf("waiting for %s to appear: %v", path, d.ctx.Err())
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

// PID is the daemon process's id.
func (d *Daemon) PID() int { return d.cmd.Process.Pid }

// reapGrace bounds the wait for the kernel to reap a process group that has
// already been sent SIGKILL. SIGKILL cannot be caught, blocked or ignored, so
// this is not a shutdown budget: it covers only the scheduling of an already
// doomed process, which every observed run completes in single-digit
// milliseconds. A process still unreaped after this is a fault to REPORT, not
// something to keep waiting on — an unbounded teardown wait costs the whole
// suite its remaining budget, not just its own test.
const reapGrace = 2 * time.Second

// Stop sends SIGTERM and waits, BOUNDED, for the process to leave. A daemon
// that ignores SIGTERM is escalated to SIGKILL and reported, never waited on
// indefinitely.
func (d *Daemon) Stop() {
	d.t.Helper()
	d.signal(syscall.SIGTERM)
	if d.awaitReapWithin(DefaultTimeout) {
		return
	}
	d.t.Errorf("harness: the daemon did not exit within %s of SIGTERM; killing it", DefaultTimeout)
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
	if err := d.cmd.Process.Signal(syscall.SIGTERM); err != nil {
		return
	}
	d.awaitReapWithin(DefaultTimeout)
}

// Kill ends the process group without warning, for crash simulation and for
// the cleanup every test gets.
func (d *Daemon) Kill() {
	d.t.Helper()
	if d.cmd == nil || d.cmd.Process == nil {
		return
	}
	// A REAPED PROCESS IS NEVER SIGNALED. cmd.Wait has returned, so the kernel
	// has freed the pid and the group id that shares it: -pid names no group
	// of ours any more, and signaling it can only reach whatever process the
	// pid was recycled into. This is the ordinary state of every test that
	// waits for its daemon to leave on its own (a refused second daemon, a
	// joining daemon) before the cleanup kill runs.
	if d.reaped() {
		return
	}
	// ESRCH is the benign race: the group left on its own between the caller's
	// decision and this signal. EPERM is the same race after a recycle — the
	// pid now belongs to someone else — and is accepted ONLY once the reap
	// confirms our own process is in fact gone. Every other error is a real
	// fault.
	if err := syscall.Kill(-d.cmd.Process.Pid, syscall.SIGKILL); err != nil && !errors.Is(err, syscall.ESRCH) {
		if !errors.Is(err, syscall.EPERM) {
			d.t.Errorf("harness: SIGKILL process group %d: %v", d.cmd.Process.Pid, err)
		} else if !d.awaitReapWithin(reapGrace) {
			d.t.Errorf("harness: SIGKILL process group %d: %v, and it was still unreaped %s later",
				d.cmd.Process.Pid, err, reapGrace)
		}
		return
	}
	if !d.awaitReapWithin(reapGrace) {
		d.t.Errorf("harness: the daemon was still unreaped %s after SIGKILL", reapGrace)
	}
}

// reaped reports whether cmd.Wait has already returned for this process, which
// is what frees its pid — and with it its process group id — for reuse.
func (d *Daemon) reaped() bool {
	d.mu.Lock()
	defer d.mu.Unlock()
	return d.exited
}

// awaitReapWithin drives the process's one cmd.Wait to completion on a
// background goroutine and reports whether it finished within budget. The
// waitOnce still guarantees exactly one Wait per process; moving the blocking
// call off the caller's goroutine is what makes the caller's wait bounded.
func (d *Daemon) awaitReapWithin(budget time.Duration) bool {
	reaped := make(chan struct{})
	go func() {
		d.waitOnce.Do(d.wait)
		close(reaped)
	}()
	select {
	case <-reaped:
		return true
	case <-time.After(budget):
		return false
	}
}

func (d *Daemon) signal(sig syscall.Signal) {
	d.t.Helper()
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

func (d *Daemon) wait() {
	err := d.cmd.Wait()
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
	// Signal 0 probes liveness without disturbing the process.
	return d.cmd.Process.Signal(syscall.Signal(0)) != nil
}

// AwaitExit waits for the process to leave and answers its exit status,
// failing the test if it outlives the context.
func (d *Daemon) AwaitExit() int {
	d.t.Helper()
	done := make(chan int, 1)
	go func() { done <- d.Wait() }()
	select {
	case code := <-done:
		return code
	case <-d.ctx.Done():
		d.t.Fatalf("daemon did not exit: %v\nstderr:\n%s", d.ctx.Err(), d.Stderr())
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
func Register(t *testing.T, d *Daemon, dir string) *workspacev1.WorkspaceRef {
	t.Helper()
	resp, err := d.Client().RegisterWorkspace(d.Ctx(), connect.NewRequest(&agentreplv1.RegisterWorkspaceRequest{Dir: dir}))
	if err != nil {
		t.Fatalf("RegisterWorkspace(%s) = error %v, want a minted id", dir, err)
	}
	ref := resp.Msg.GetSuccess().GetWorkspace()
	if ref.GetId() == "" {
		t.Fatalf("RegisterWorkspace(%s) = %v, want a success carrying a workspace id", dir, resp.Msg)
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

// ReapStrays kills every process whose command line names this run's state
// directory, whatever process group it is in — except a pid the test declared
// its own through SpareFromStrayReaping.
//
// IT IS THE ONLY THING THAT BOUNDS A TEST'S PROCESS TREE. The daemon runs in
// its own process group and Kill ends that group, but every shim the daemon
// spawns is put in a group of ITS own (the spawn's process-group discipline),
// so a daemon that dies without standing its shims down leaves them running —
// and a leaked daemon keeps prelaunching more. The state directory is unique to
// this run and appears in both the daemon's argv and every shim's `--listen`
// path, so it is an exact key for "processes this test started".
func (d *Daemon) ReapStrays() {
	for _, pid := range d.strayPIDs() {
		_ = syscall.Kill(pid, syscall.SIGKILL)
	}
}

// StrayPIDs answers the live processes naming this run's state directory,
// excluding the harness's own process. A test asserts on it; ReapStrays acts
// on it.
func (d *Daemon) StrayPIDs() []int { return d.strayPIDs() }

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
func SpareFromStrayReaping(t *testing.T, pid int) {
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

func isSparedFromStrayReaping(pid int) bool {
	sparedMu.Lock()
	defer sparedMu.Unlock()
	return sparedFromReaping[pid]
}

func (d *Daemon) strayPIDs() []int {
	if d.StateDir == "" {
		return nil
	}
	out, err := exec.Command("ps", "-Ao", "pid=,args=").Output()
	if err != nil {
		return nil
	}
	self := os.Getpid()
	var pids []int
	for _, line := range strings.Split(string(out), "\n") {
		line = strings.TrimSpace(line)
		if line == "" || !strings.Contains(line, d.StateDir) {
			continue
		}
		fields := strings.Fields(line)
		pid, err := strconv.Atoi(fields[0])
		if err != nil || pid == self || isSparedFromStrayReaping(pid) {
			continue
		}
		pids = append(pids, pid)
	}
	return pids
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

// FeedPageSize is the number of rows one feed page carries. It is the daemon's
// OWN constant rather than a copy, so a page-size change can never leave a
// walk test silently pushing too few rows to produce a second page.
const FeedPageSize = feed.DefaultPageSize

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
