package e2e

import (
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"io/fs"
	"os"
	"os/exec"
	"path/filepath"
	"strconv"
	"strings"
	"sync"
	"sync/atomic"
	"syscall"
	"testing"
	"time"

	"claude-repld/integration/harness"
)

// THE EMACS CLIENT LAYER.
//
// EMACS-LAYER-SPEC.md is the design; this file is its harness. The short
// version of why it exists: a test that dials the daemon's Connect API
// covers the daemon's serving surface, while the PRODUCT is an Emacs
// frontend. Two defects found by hand are invisible from the Connect side —
// the launcher's missing account flags (only Emacs composes that argv) and a
// sentinel/kill-buffer recursion that has no frame at all, only a wedged
// editor — and both are what this layer is shaped around.
//
// Emacs runs as a REAL tty frame inside a pty, with `server-start`, driven
// through `emacsclient --eval`. Batch mode is not an option: `config.el`
// gates cold start on `(not noninteractive)`, so a batch Emacs never spawns
// the daemon at all.
//
// AND IT BOOTS THE IMAGE'S REAL DOOM, not `emacs -Q`. The sandbox image
// bakes `doom install` + `doom sync` against a profile that enables
// `:app agent-repl` and `:config (default +bindings)`, so booting it is
// strictly more faithful than loading `config.el` by hand: real `map!`
// bindings, the real `set-popup-rule!` the notes popup needs (`config.el`
// skips that rule when `set-popup-rule!` is unbound), real module load
// order, and the module reached through `doom!` exactly as a user reaches
// it. There is NO `-Q` fallback, and none is wanted: the only thing `-Q`
// buys is a bare Emacs, and bare-Emacs coverage is the module's own ERT
// suites under `lisp/test-*.el`, which run in batch.
//
// The Doom side of the seam is two files this layer does not own:
//   * `sandbox/doom/init.el` loads AGENT_REPL_E2E_SETTINGS after its
//     `doom!` form, which is BEFORE any module `config.el` -- necessary,
//     because `config.el` decides at load time whether to register cold
//     start, and this layer needs cold start off so `EnsureDaemon` is the
//     spawn a scenario can observe from its first instant.
//   * `sandbox/doom/config.el` defines the readback helper and, on Doom's
//     own after-init edge, starts the server and writes a readiness stamp.

// THE BOUNDS BELOW ARE MEASURED, not guessed.
//
// Every one is derived from ten healthy boots of this layer inside the
// sandbox (two `-count=5` runs), whose per-phase durations the harness itself
// reports on every run -- see `Emacs.record` / `reportPhases`, and the table
// in EMACS-LAYER-SPEC.md, "The bounds, measured". Rerunning `go test -v`
// prints the numbers a future revision must re-derive them from, so no bound
// here can quietly drift back into a guess.

// HeartbeatBound is how long one `emacsclient --eval '(emacs-pid)'` probe
// may take before the running test fails.
//
// MEASURED: the longest healthy probe was 414ms. It is not a measure of
// emacsclient's round trip -- probes ride the same socket every scenario
// uses, so a probe queues behind whatever Emacs is doing and the maximum is
// set by the longest command a scenario runs. 3x that, because the failure
// this exists to catch is a hang, and a hang that clears itself is still the
// sentinel/kill-buffer recursion.
const HeartbeatBound = 1250 * time.Millisecond

// evalBound is how long ONE `emacsclient --eval` of a SCENARIO'S OWN FORM may
// take before the running test fails.
//
// It used to be HeartbeatBound, which was wrong in kind rather than in size --
// the same defect daemonLinkBound was split out of emacsBootBound to fix. The
// heartbeat probe is `(emacs-pid)`: it does nothing, and its cost is purely
// how long the command loop takes to reach it. A scenario's form is arbitrary
// elisp that does real work -- `agent-repl-add-project-workspace` registers a
// workspace, opens the panels and creates an xwidget webview -- so one bound
// covering both could only ever be too tight for one of them, and it was: the
// workspace-creating evals were killed at 1.25s under nothing worse than a
// second scenario running beside them.
//
// LENGTHENING IT GIVES UP NO HANG DETECTION, which is the only reason it is
// allowed to be longer. A wedged Emacs is caught by the HEARTBEAT, which rides
// the same socket, samples every 250ms and closes `wedged` -- and every wait
// loop in this layer, this one included, short-circuits on `wedged` rather
// than running out its own bound. This bound catches an eval that is slow;
// the heartbeat catches an Emacs that is gone, and it is still the faster of
// the two.
//
// MEASURED, over 92 scenarios of a `-count=2` run at the layer's own
// parallelism bound: the slowest healthy eval was 409ms and the spread is
// tight (the top eight were 409, 408, 406, 403, 384, 383, 381ms). 3x that.
//
// The one observation above it in that run was 5.001s, and it is NOT a slow
// eval: it is `agent-repl-add-project-workspace` blocking Emacs's command
// loop, which the heartbeat catches as EMACS WEDGED at 1.25s while the eval
// sits there. Deriving a bound from it would enshrine that defect as the
// expectation.
//
// It lands on the same number HeartbeatBound carries, which is a coincidence
// of two similar measurements and not a reason to fuse them again: they bound
// different phenomena and will move apart the moment either one does.
//
// `go test -v` prints `emacs phase eval-max` for every scenario, which is the
// number a future revision must re-derive this from.
const evalBound = 1250 * time.Millisecond

// daemonStopForm is the teardown's stop, WAITED ON.
//
// THE DEFECT IT FIXES. `agent-repl-frontend-daemon-stop` is asynchronous: it
// hands the `UpdateShutdownSchedule{now}` to an in-flight exchange and returns
// `t` the
// same instant. The teardown's very next act was `(kill-emacs)`, which takes
// that exchange down with it — so the request the daemon never received could not
// possibly have made it exit, and the reaper found the whole tree standing with
// nothing to blame. Measured over one 24-scenario run: 17 surviving
// `claude-repld` processes, 24 shims and 48 `shim-lock` holders.
//
// So the form BLOCKS on the command's own on-done callback and reports what it
// carried. It waits through `accept-process-output`, never `sleep-for`, because
// the answer it is waiting for arrives on exactly the process output a sleep
// would refuse to serve.
const daemonStopForm = `(let ((done nil) (accepted nil))
  (agent-repl-frontend-daemon-stop (lambda (ok) (setq accepted ok done t)))
  (with-timeout (` + teardownStopAckSeconds + ` (setq done 'timeout))
    (while (not done) (accept-process-output nil 0.05)))
  (format "%S" (list :done done :accepted accepted)))`

// teardownStopAckSeconds is how long Emacs waits INSIDE that form for the ack,
// and teardownStopBound is the Go side's outer bound on the same call.
//
// The ack is a loopback unary rpc, and the daemon answers it only once every
// shim is down — so the worst case this has to cover is one shim that accepts
// the forced stand-down and never answers, which costs the daemon exactly
// `drain.DefaultStandBound` (5s). Eight seconds covers that with room for the
// round trip; ten is the outer bound, which exists so a wedge inside Emacs
// itself still ends the teardown rather than hanging it. In the healthy case
// both are unreached: the stand-down of a live fake shim was measured at 2ms.
const (
	teardownStopAckSeconds = "8"
	teardownStopBound      = 10 * time.Second
)

// daemonStopAcceptedArm is how the accepted outcome reads inside the `%S` the
// form above prints: `agent-repl-frontend-daemon-stop` hands its on-done
// callback the arm plist `(:arm :accepted)`.
const daemonStopAcceptedArm = ":accepted (:arm :accepted)"

// emacsExitBound is how long EMACS AND ITS PROCESS GROUP may take to be gone
// after Emacs has been asked to exit with `(kill-emacs)'.
//
// It is the group rather than the pid because the group is what the scenario
// actually owns. `script' runs Emacs in a session of its own, and Emacs leads
// a group inside it that also holds the WebKit network and web processes the
// panel's webview starts -- both measured outliving Emacs and reaching the
// reaper. Nothing else of this scenario is in that group: the daemon and
// every shim are spawned into groups of their own.
//
// MEASURED twice over full 47-scenario layer runs instrumented at a 30s
// bound. Waiting on the pid alone: 51ms, 52ms, 53ms, 54ms. Waiting on the
// whole group, which is what ships: 51ms, 52ms, 53ms, 54ms, 55ms -- the same
// numbers, because the children go with Emacs -- with two scenarios below
// that whose Emacs had already exited. The spread is poll quantization, not
// variance: `strayPollInterval' is 50ms, so a 51ms observation means the
// group was already empty at the second /proc read.
//
// 500ms is roughly ten times the worst of those. The multiple is 10x rather
// than 3x for the same reason `emacsBootBound' gives: three times a number
// this small is not a bound, it is a race with the scheduler. And missing
// this bound is not a scenario failure -- it is the trigger for the
// escalation below -- so the number is a patience budget, and buying more of
// it would only hold the scenario's parallelism slot while a process that was
// never going to answer is waited on.
const emacsExitBound = 500 * time.Millisecond

// emacsSignalBound is how long an Emacs group that ignored `(kill-emacs)'
// gets to disappear after each signal sent to it.
//
// No healthy run has ever reached this: every Emacs in the runs above exited
// on the ask. It is therefore sized from the observation the reaper already
// records for the same question -- see `strayTermBound', where every process
// that honors a signal at all does so within one 50ms poll -- and it is the
// same 500ms, deliberately, so the two cannot drift.
const emacsSignalBound = 500 * time.Millisecond

// heartbeatInterval is how often the probe runs. It rides the SAME server
// socket every scenario uses, so it queues behind whatever Emacs is doing
// and therefore measures the command loop's real responsiveness rather than
// merely whether the process is alive.
//
// MEASURED, and left where it was: at 250ms it is already BELOW the observed
// probe latency, so the detector samples as fast as the command loop can
// answer and a shorter interval would buy nothing but queued probes.
const heartbeatInterval = 250 * time.Millisecond

// emacsBootBound is how long emacsclient may take to reach the server socket
// once Doom's readiness stamp says the server was started.
//
// MEASURED: the longest healthy wait was 49ms. The multiple here is 10x
// rather than 3x, and deliberately: three times a number this small is not a
// bound, it is a race with the scheduler.
const emacsBootBound = 500 * time.Millisecond

// daemonLinkBound is how long EnsureDaemon may take: the launcher spawning
// `claude-repld`, the daemon binding its address, and Emacs dialling it.
//
// It used to be emacsBootBound, which was wrong in kind rather than in size:
// one name covered two unrelated events, so neither could ever be measured
// against its own phase. Per SPEC.md section B a per-site bound is a NAMED
// constant with a stated reason, and this is that site's.
//
// MEASURED: the longest healthy link was 265ms, remarkably stable across
// runs (263-265ms). Not quite 4x that.
const daemonLinkBound = 1 * time.Second

// doomBootBound is how long Emacs may take to finish Doom's own
// initialization and publish the readiness stamp.
//
// RE-MEASURED after the image began carrying Doom's native code (the
// Dockerfile's `doom sync --aot`) and after the scenarios became parallel,
// because both moved this phase:
//
//	serial, JIT-compiling per test (the old regime)  mean 1437ms  max 1482ms
//	serial, image-baked native code                  mean  836ms  max 1501ms
//	at the layer's parallelism bound of 3 slots      mean  990ms  max 1780ms
//
// The bound STAYS at 3500ms. It is no longer 3x the worst healthy boot -- at
// three slots that would be 5.3s -- and it is not raised to keep that ratio,
// because a bound is a promise about the product and loosening it to
// accommodate the harness's own concurrency would be the harness marking its
// own homework. 3500ms is twice the worst boot observed under the bound the
// layer actually runs at, and a scenario that misses it is telling the truth:
// this machine is too loaded to run three Emacsen.
//
// It is not expressed as a multiple of emacsBootBound: the two phases differ
// by a factor of twenty, so tying them together would let a change in one
// silently move the other.
const doomBootBound = 3500 * time.Millisecond

// doomStageBound bounds each `cp -a` that stages one entry of Doom's
// `.local` tree into the test's scratch.
//
// RE-MEASURED since the image began baking Doom's native code: `.local/cache`
// carries 591 `.eln` files (58 MiB) now rather than 228 KiB, so this copy is
// no longer free. The whole staging -- every entry, not one `cp` -- took
// 310ms at its slowest serially and 1.039s at its slowest with four scenarios
// contending for the VM's four CPUs, against 4ms before.
//
// Five seconds still stands, and the reason is unchanged rather than
// stretched: what this exists to catch is a copy that cannot finish AT ALL,
// and five seconds remains a large multiple of the worst observation while
// still failing a hang in the test that caused it rather than at the suite's
// own timeout.
const doomStageBound = 5 * time.Second

// Emacs is one sandboxed Emacs process, its server socket, and its
// heartbeat.
type Emacs struct {
	// ArtifactPaths are extra files or trees dumpArtifacts preserves on failure.
	ArtifactPaths []string
	t             *testing.T
	box           sandbox

	// Root is the scratch subtree this Emacs owns. Every path below is
	// under it, so a swept scratch leaves nothing behind.
	Root string
	// Display is the Xvfb this Emacs draws its GUI frame on. The frame has
	// to be graphical because the panel is an xwidget-webkit webview.
	Display *xdisplay
	// ServerSocket is the `server-name` emacsclient dials.
	ServerSocket string
	// EmacsDir is the per-test `~/.emacs.d` this Emacs boots Doom from. It
	// is staged from the image's own EMACSDIR: see stageEmacsDir.
	EmacsDir string
	// ReadyStamp is the file Doom's after-init hook writes. Its appearance
	// is the ONE edge that means "Doom finished initializing".
	ReadyStamp string
	// Doom is what that stamp said, so a scenario can assert it really is a
	// Doom boot with `map!` expanded rather than assume it.
	Doom DoomReady
	// StateDir is what travels to the daemon in AGENT_REPL_STATE_DIR. ONE
	// state root is the cross-system contract, so the daemon Emacs spawns
	// and the Go client that cross-checks frames read the same tree.
	StateDir string
	// LogFile is where the MODULE's own elisp log goes for this scenario.
	// Under the state root, so the failure artifacts already carry it, and
	// per scenario, so no other Emacs in this container writes to it.
	LogFile string
	// DefaultConfigDir and MultiRepoConfigDir are the two account roots the
	// launcher is given, and the same two the sidecar is told to watch.
	DefaultConfigDir   string
	MultiRepoConfigDir string
	MultiRepoRoot      string

	proc sandboxProc

	// evalSeq names each eval's request/response file pair uniquely.
	evalSeq atomic.Uint64

	// wedged closes when the heartbeat misses, so every wait loop gives up
	// immediately instead of running out its own bound behind a dead
	// command loop.
	wedged     chan struct{}
	wedgeOnce  sync.Once
	wedgeCause atomic.Pointer[string]

	stopHeartbeat context.CancelFunc
	// heartbeatDone closes when the detector's goroutine has returned. `stop`
	// WAITS on it before killing Emacs, because a probe that loses the race
	// with teardown would otherwise report a killed Emacs as a wedge -- and
	// `t.Errorf` from a goroutine the test no longer owns is a panic, not a
	// failure. Cancelling is not enough on its own: the goroutine can already
	// be past its cancellation check when the cancel lands.
	heartbeatDone chan struct{}

	// reap are the paths whose appearance in a process's argv marks that
	// process as this scenario's, so reapStrays can hunt down the daemon
	// and shims Emacs detached from itself. See reapStrays.
	reapMu sync.Mutex
	reap   []string

	// evalMax is the longest scenario eval this Emacs has answered, in
	// nanoseconds. It is what MEASURES evalBound, the same way heartbeatMax
	// measures HeartbeatBound. Reported once per test by reportPhases.
	evalMax atomic.Int64
	// heartbeatMax is the longest probe this Emacs has answered, in
	// nanoseconds. It is what MEASURES HeartbeatBound: the bound is a small
	// multiple of this number across healthy runs, never a guess. Reported
	// once per test by reportPhases.
	heartbeatMax atomic.Int64
	// artifactDirOnce and artifactRoot name the ONE directory this Emacs's
	// evidence lands in, claimed on first use. See artifactDir.
	artifactDirOnce sync.Once
	artifactRoot    string

	// nativeOnce guards the gdb capture. ONE capture per Emacs: gdb attaching
	// STOPS the inferior for the duration, so a second attach would describe
	// a process the first one already perturbed, and the stall this exists
	// for is a permanent loop whose first sample is its whole story.
	nativeOnce sync.Once
	// nativeStack is what that one capture said, so both the wedge report and
	// a stuck eval can quote it without racing to take it.
	nativeStack string

	// teardownSteps are the things stop() actually tried, in order, so the
	// reaper can name them when it still finds a survivor afterwards.
	teardownMu    sync.Mutex
	teardownSteps []string

	// fail is how reapStrays reports a survivor. It is nil in every
	// scenario, which means e.t.Errorf; the field exists so the teardown
	// tests can assert that a stray FAILS rather than merely logging, which
	// nothing can observe through *testing.T itself.
	fail func(format string, args ...any)

	// phases are the boot durations this Emacs observed, in the order they
	// happened. They MEASURE emacsBootBound, doomBootBound and
	// doomStageBound the same way.
	phasesMu sync.Mutex
	phases   []phase
}

// phase is one measured boot step.
type phase struct {
	name string
	took time.Duration
}

// record adds one measured phase.
func (e *Emacs) record(name string, took time.Duration) {
	e.phasesMu.Lock()
	defer e.phasesMu.Unlock()
	e.phases = append(e.phases, phase{name: name, took: took})
}

// reportPhases logs every measured duration, ALWAYS -- a passing run is
// exactly the run whose numbers the bounds are derived from, so they must not
// be visible only on failure. `go test -v` is where the measurement is read.
func (e *Emacs) reportPhases() {
	e.phasesMu.Lock()
	defer e.phasesMu.Unlock()
	for _, p := range e.phases {
		e.t.Logf("emacs phase %s took %s", p.name, p.took.Round(time.Millisecond))
	}
	if max := e.evalMax.Load(); max > 0 {
		e.t.Logf("emacs phase eval-max took %s (bound %s)",
			time.Duration(max).Round(time.Millisecond), evalBound)
	}
	if max := e.heartbeatMax.Load(); max > 0 {
		e.t.Logf("emacs phase heartbeat-probe-max took %s (bound %s)",
			time.Duration(max).Round(time.Millisecond), HeartbeatBound)
	}
}

// DoomReady is the readiness stamp `sandbox/doom/config.el` writes once Doom
// has finished initializing and the server socket answers.
type DoomReady struct {
	OK           bool   `json:"ok"`
	Error        string `json:"error"`
	PID          int    `json:"pid"`
	EmacsVersion string `json:"emacs_version"`
	Doom         bool   `json:"doom"`
	DoomVersion  string `json:"doom_version"`
	MapBang      bool   `json:"map_bang"`
	PopupRule    bool   `json:"popup_rule"`
	AgentRepl    bool   `json:"agent_repl"`
	ServerName   string `json:"server_name"`
}

// EmacsOpts configures one Emacs.
type EmacsOpts struct {
	// DaemonBinary is the `claude-repld` the launcher will spawn. It is the
	// binary the Go side already built; Emacs composes the argv around it.
	DaemonBinary string
	// DaemonArgs are stated on `agent-repl-daemon-command` after the binary,
	// which is where a test states a fact the daemon reads from ITS OWN
	// argv rather than from the environment -- the built shim bundle and the
	// built webapp dist being the two. They do NOT touch the launcher's own
	// contribution: `agent-repl-daemon--argv` appends the account-root flags
	// to whatever this command is, so the flags this layer exists to cover
	// are still composed by the launcher and only by it.
	DaemonArgs []string
	// StoreSocket is handed to the daemon through the spawn environment.
	StoreSocket string
	// ExtraEnv is added to the Emacs process's environment, and therefore
	// inherited by the daemon, the shim, and everything below them. The
	// build identity and the vendor-call prohibition ride here.
	ExtraEnv []string
}

// StartEmacs brings up one sandboxed Emacs with a real tty frame, loads the
// module's own sources into it, and arms the heartbeat.
//
// It does NOT start the daemon: that is `EnsureDaemon`, because EMACS
// spawning the daemon through its own launcher is the point of this layer.
func StartEmacs(t *testing.T, box sandbox, opts EmacsOpts) *Emacs {
	t.Helper()

	// The parallelism slot is NewEmacsWorld's to take, and it takes it before
	// the per-run builds rather than here -- see the comment there. This is
	// asserted rather than assumed, because the only thing standing between
	// this layer and an unbounded number of concurrent Emacsen is that every
	// caller goes through NewEmacsWorld.
	requireEmacsSlot(t)

	root := filepath.Join(box.Scratch(), "emacs")
	// 0o700 before anything else creates it: this root is the Emacs HOME,
	// and Emacs 30 refuses an init tree "accessible by others" -- Doom's
	// early-init then aborts before any readiness stamp is written.
	if err := os.MkdirAll(root, 0o700); err != nil {
		t.Fatalf("prepare the Emacs root %s: %v", root, err)
	}
	if err := os.Chmod(root, 0o700); err != nil {
		t.Fatalf("restrict the Emacs root %s: %v", root, err)
	}
	e := &Emacs{
		t:                  t,
		box:                box,
		Root:               root,
		ServerSocket:       filepath.Join(root, "server"),
		EmacsDir:           filepath.Join(root, ".emacs.d"),
		ReadyStamp:         filepath.Join(root, "doom-ready.json"),
		StateDir:           filepath.Join(root, "state"),
		LogFile:            filepath.Join(root, "state", "logs", "doom-agent-repl.log"),
		DefaultConfigDir:   filepath.Join(root, "account-default"),
		MultiRepoConfigDir: filepath.Join(root, "account-multi"),
		MultiRepoRoot:      filepath.Join(root, "multi-repo"),
		wedged:             make(chan struct{}),
	}
	// The scratch root is unique per test (os.MkdirTemp), so an argv naming
	// it belongs to this scenario and to nothing else.
	e.reap = []string{root}

	// REGISTERED FIRST, so t.Cleanup's LIFO unwind runs it LAST: the reaper
	// must observe what is STILL alive after Emacs, its daemon and the
	// sidecar have all been stopped on purpose, otherwise it would report
	// processes that were about to exit anyway.
	t.Cleanup(e.reapStrays)

	e.writeSettings(opts)
	// THE VENDOR STAND-IN THE DAEMON'S OWN CALLS EXEC.
	//
	// A create that supplies no name is named by a headless `claude -p` call
	// the daemon makes inside Create, and a naming call that cannot answer
	// REFUSES the create (daemon/internal/workspace/namecall.go). This layer
	// exports AGENT_REPL_FORBID_VENDOR_CALLS=1, so with nothing naming a
	// binary the guard refuses the spawn outright and every nameless create
	// in this layer fails with `naming_failed`.
	//
	// So the same fake the daemon's integration harness writes is staged
	// here too, and named EXPLICITLY -- which is also what makes the spawn
	// legal under the guard, since an explicit path is by definition not a
	// call to the real CLI. Its naming answer is the deterministic
	// harness.FakeClaudeMintedName, so a scenario may assert on the name a
	// nameless create produced.
	fakeClaude := harness.NewFakeClaude(t, filepath.Join(root, "fakebin"))
	// THE DESKTOP BANNER PROGRAM the daemon posts through is a recorder too:
	// no scenario may raise a real banner, and the container carries no
	// banner program, which a daemon records at ERROR on its boot.
	fakeNotifier := harness.NewFakeNotifier(t, filepath.Join(root, "fakebin"))
	prewarmTrampolines(t, box)
	staged := time.Now()
	e.stageEmacsDir()
	e.record("stage-emacs-dir", time.Since(staged))

	// THE DISPLAY COMES FIRST. The panel is an `xwidget-webkit` webview, so
	// Emacs must take a GRAPHICAL frame, which needs an X display that
	// already exists when it starts. Starting it here also gets the teardown
	// order right for free: t.Cleanup unwinds LIFO, so the Emacs stop
	// registered below runs BEFORE the display is torn down.
	displayStarted := time.Now()
	display := startXvfb(t, box, filepath.Join(root, "display"))
	e.Display = display
	e.record("xvfb-ready", time.Since(displayStarted))

	// HOME is the per-test scratch root, and `~/.emacs.d` under it is the
	// staged Doom. The image's Emacs 30.2 does have `--init-directory`
	// (Emacs 29+), but HOME is used instead of it: the container runs
	// `--read-only` and the staged `/sandbox/emacs.d` is not a tmpfs mount,
	// so a flag alone could not make Doom's local tree writable per test --
	// which is also why the staging exists.
	env := append([]string{
		"HOME=" + root,
		"EMACSDIR=" + e.EmacsDir,
		// THE SHARED TRAMPOLINE CACHE. See prewarmTrampolines: this is what
		// puts the prewarmed `.eln' files on `native-comp-eln-load-path', so
		// `comp-trampoline-search' finds them and this boot compiles nothing.
		"EMACSNATIVELOADPATH=" + trampolineCacheDir,
		"AGENT_REPL_E2E_EMACS=1",
		"AGENT_REPL_E2E_SERVER=" + e.ServerSocket,
		"AGENT_REPL_E2E_READY=" + e.ReadyStamp,
		"AGENT_REPL_E2E_SETTINGS=" + e.settingsPath(),
		"AGENT_REPL_STATE_DIR=" + e.StateDir,
		"AGENT_REPL_FORBID_VENDOR_CALLS=1",
		"AGENT_REPL_CLAUDE_BIN=" + fakeClaude,
		"AGENT_REPL_NOTIFIER_CMD=" + fakeNotifier.Path,
		"MULTI_REPO_ROOT=" + e.MultiRepoRoot,
		// A tty frame needs a terminal that can position the cursor, and
		// `dumb` by definition cannot: it has no `cup` capability, so Emacs
		// refuses to start on it outright ("Terminal type \"dumb\" is not
		// powerful enough to run Emacs. It lacks the ability to position the
		// cursor.") and exits before Doom loads. `dumb` is the right answer
		// for a batch Emacs, which draws nothing; it is never an answer for
		// `-nw`. xterm-256color is chosen because the image provably carries
		// its terminfo entry at /lib/terminfo/x/xterm-256color, and because
		// it is the terminal a user of this module actually runs Emacs on, so
		// the frame the scenarios inspect is the frame a user would see.
		"TERM=xterm-256color",
		// NO ACCESSIBILITY BRIDGE, AND THEREFORE NO D-BUS.
		//
		// MEASURED, and it is a boot bound's worth: a graphical GTK Emacs
		// asks D-Bus for the accessibility bus on startup, and this
		// container has no session bus, so GTK autolaunches one --
		// `dbus-launch --autolaunch <machine-id>` plus a `dbus-daemon
		// --session`, both of which showed up as strays -- and then fails
		// the lookup anyway with
		//
		//   AT-SPI: Error retrieving accessibility bus address:
		//   org.freedesktop.DBus.Error.ServiceUnknown: The name org.a11y.Bus
		//   was not provided by any .service files
		//
		// The failure is harmless; the WAIT is not. Boots that took that
		// path were the only ones to miss doomBootBound -- 3.5s against a
		// mean of 792ms -- and they did it in three unrelated scenarios per
		// `-count=2` run, which is exactly the shape of a flake.
		//
		// Nothing is given up. There is no screen reader in a container and
		// no assertion in this layer touches accessibility or D-Bus; the
		// only thing switched off is a lookup that was always going to fail.
		"NO_AT_BRIDGE=1",
		"GTK_A11Y=none",
		// AND THE AUTOLAUNCH ITSELF, which the two above do not stop.
		//
		// `NO_AT_BRIDGE'/`GTK_A11Y' switch off the accessibility CLIENT;
		// they do not tell libdbus there is no session bus. With
		// DBUS_SESSION_BUS_ADDRESS unset, the first thing in the process to
		// want a session bus -- the a11y lookup, GIO, or WebKitGTK, which
		// the panel's webview starts -- makes libdbus run `dbus-launch
		// --autolaunch', and that is still observed on every scenario: a
		// `dbus-launch' and a `dbus-daemon --session' in each teardown's
		// stray list, on the shipped layer with both variables already set.
		//
		// The autolaunch is not merely a wasted fork. It arbitrates through
		// a PROPERTY ON THE X ROOT WINDOW, taking a server grab to do it, so
		// two Emacsen booting at once on their own displays still queue --
		// and boots that took that path are the ones that miss
		// `doomBootBound' by a factor of six while the pty stays empty,
		// because nothing about the wait is Emacs's to report.
		//
		// `disabled:' is not a bus this layer runs; it is an address libdbus
		// cannot parse, which is what makes the connection fail AT ONCE
		// instead of autolaunching. Nothing is given up: there is no session
		// bus in this container to reach, and no assertion in this layer
		// touches D-Bus.
		"DBUS_SESSION_BUS_ADDRESS=disabled:",
	}, display.Env()...)
	env = append(env, opts.ExtraEnv...)
	if opts.StoreSocket != "" {
		env = append(env, "AGENT_REPL_STORE_SOCKET="+opts.StoreSocket)
	}

	// No `-Q` and no `-l`: the image's Doom profile is the init path, and
	// `sandbox/doom/init.el` picks the settings file up itself.
	//
	// And no `-nw` either, which is the one thing that changed here. A tty
	// frame is a real frame for `window-list`, `tab-bar-tabs` and
	// `mode-line-format` -- but it is not a frame an xwidget can live on:
	// `make-xwidget` signals "GTK has not been initialized" on one, so the
	// panel this module opens could never be created. With DISPLAY set,
	// plain `emacs` takes a GRAPHICAL frame on the Xvfb above, and every
	// property the tty frame had is still true of it.
	argv := append(envPrefix(env), "emacs")

	ctx, cancel := context.WithCancel(context.Background())
	t.Cleanup(cancel)
	proc, err := box.StartPTY(ctx, argv...)
	if err != nil {
		t.Fatalf("start emacs in the sandbox: %v", err)
	}
	e.proc = proc

	// Teardown is registered BEFORE the boot wait, so an Emacs that comes
	// up and then wedges during load is still reaped.
	t.Cleanup(e.stop)
	// Any failure, not only a wedge, preserves the Emacs artifacts: a
	// refused eval or an unmet await leaves its cause in the pty and the
	// state root, which stop discards.
	t.Cleanup(func() {
		if t.Failed() && !e.isWedged() {
			e.dumpArtifacts()
		}
	})

	doomStarted := time.Now()
	e.awaitDoom()
	e.record("doom-boot", time.Since(doomStarted))
	serverStarted := time.Now()
	e.awaitServer()
	e.record("server-answers", time.Since(serverStarted))
	e.armHeartbeat()
	e.assertTrampolinesWerePrewarmed()
	t.Cleanup(e.reportPhases)
	return e
}

// settingsPath is the ONE elisp file this layer writes. It is not a
// bootstrap any more: it loads no sources and starts nothing, because Doom
// does both. It only sets the launcher's own defcustoms at the sandbox and
// turns cold start off, and `sandbox/doom/init.el` loads it before any
// module's `config.el` -- which is the only moment at which those settings
// can still be in effect when `config.el` reads them.
func (e *Emacs) settingsPath() string { return filepath.Join(e.Root, "e2e-settings.el") }

func (e *Emacs) writeSettings(opts EmacsOpts) {
	e.t.Helper()

	for _, dir := range []string{
		e.Root,
		e.StateDir,
		e.DefaultConfigDir,
		e.MultiRepoConfigDir,
		e.MultiRepoRoot,
		filepath.Dir(e.LogFile),
		filepath.Join(e.Root, "eval"),
	} {
		if err := os.MkdirAll(dir, 0o755); err != nil {
			e.t.Fatalf("prepare %s: %v", dir, err)
		}
	}

	// A build script that does nothing and succeeds: the binaries are
	// already built by the Go side, and cold start must not shell out to
	// the real bin/build-frontend.sh from inside a test.
	buildScript := filepath.Join(e.Root, "build-noop.sh")
	if err := os.WriteFile(buildScript, []byte("#!/usr/bin/env bash\nexit 0\n"), 0o755); err != nil {
		e.t.Fatalf("write the no-op build script: %v", err)
	}

	// `setq` AHEAD of the `defcustom`s in `lisp/daemon.el` is deliberate and
	// it holds: `custom-declare-variable` leaves an already-bound variable's
	// value alone, so these survive the module's own declaration. Doing it
	// the other way round -- waiting until the module is loaded -- is too
	// late, because `config.el` has by then already decided whether to
	// register cold start.
	src := fmt.Sprintf(`;;; e2e-settings.el --- e2e emacs client layer -*- lexical-binding: t; -*-
;;; Commentary:
;; Written by the Go e2e harness and loaded by sandbox/doom/init.el BEFORE
;; any Doom module's config.el.  It adds no behavior: it only points the
;; module's own defcustoms at this test's sandbox paths.  See
;; e2e/EMACS-LAYER-SPEC.md.
;;; Code:

;; Cold start is NOT armed: this layer drives
;; agent-repl-frontend-daemon-ensure explicitly, so the spawn happens at a
;; moment a scenario can observe from its first instant.  This must be set
;; before modules/app/agent-repl/config.el loads, which is why it is here.
(setq agent-repl-frontend-auto-start nil)

;; NOTHING IN THIS LAYER CAN ANSWER A QUESTION.
;;
;; A y-or-n-p nobody answers does not fail, it HANGS: the reader enters a
;; recursive edit, the command loop stops running the scenario's forms, and
;; the process stops answering emacsclient -- so the layer reports "emacs did
;; not answer a heartbeat probe", which names neither the prompt nor the call
;; that raised it.  Measured: a native backtrace of that stall showed
;; magit-status calling y-or-n-p directly, and nothing above it in the elisp
;; log said so.
;;
;; Every scenario that drives a prompting command stubs the reader for the
;; duration (cl-letf on the symbol-function, which wins over this advice), so
;; a prompt that reaches here is by definition one nobody will ever answer.
;; Refusing it loudly turns a permanent stall into a named failure.
(defun agent-repl-e2e--refuse-prompt (prompt &rest _)
  (let ((text (format "%%s" prompt)))
    (message "agent-repl-e2e: UNANSWERABLE PROMPT: %%s" text)
    (when (fboundp 'agent-repl--warn)
      (agent-repl--warn nil "elisp.e2e.unanswerable-prompt prompt=%%s" text))
    (error "agent-repl-e2e: no scenario can answer this prompt: %%s" text)))
(advice-add 'y-or-n-p :override #'agent-repl-e2e--refuse-prompt)
(advice-add 'yes-or-no-p :override #'agent-repl-e2e--refuse-prompt)

;; THE MODULE'S OWN LOG GETS A PER-SCENARIO ROOT.
;; The agent-repl-log-file-name default was once
;; <temporary-file-directory>/doom-agent-repl-<uid>/doom-agent-repl.log --
;; ONE file, keyed by uid and nothing else (it is now the state root's
;; logs/emacs.central.log, and this override still pins it per scenario).  Every scenario in this container
;; runs as the same uid, so in a parallel run every Emacs appended to that
;; one file and rotated it out from under the others: the records a failure
;; needed were interleaved with three unrelated scenarios' and then truncated
;; mid-scenario by whichever of them hit the size cap first.  Pointing it
;; under this scenario's own state root makes the log this scenario's alone,
;; and the state root is already what dumpArtifacts copies on a failure.
(setq agent-repl-log-file-name %q)

(setq agent-repl-daemon-command (list %s)
      agent-repl-daemon-build-script %q
      agent-repl-daemon-default-config-dir %q
      agent-repl-daemon-multi-repo-config-dir %q
      agent-repl-daemon-multi-repo-root %q)

(provide 'agent-repl-e2e-settings)
;;; e2e-settings.el ends here
`,
		e.LogFile,
		elispStringList(append([]string{opts.DaemonBinary}, opts.DaemonArgs...)),
		buildScript,
		e.DefaultConfigDir,
		e.MultiRepoConfigDir,
		e.MultiRepoRoot,
	)

	if err := os.WriteFile(e.settingsPath(), []byte(src), 0o644); err != nil {
		e.t.Fatalf("write e2e-settings.el: %v", err)
	}
}

// ---------------------------------------------------------------------------
// SUBR TRAMPOLINES ARE BUILT ONCE PER CONTAINER, NEVER PER SCENARIO.
// ---------------------------------------------------------------------------
//
// MEASURED, and it is the whole of a four-scenario flake. Redefining a
// PRIMITIVE -- which every `advice-add' on a subr does, and this module does
// nine times between its own `lisp/' and Doom's own `map!' -- makes Emacs
// synthesize a "subr trampoline" so natively compiled callers see the
// redefinition too. Synthesizing one is `comp-trampoline-compile', and that
// is a SYNCHRONOUS `call-process' of a whole second Emacs:
//
//	pid=22091 state=R /usr/local/bin/emacs -no-comp-spawn -Q --batch \
//	  -l /tmp/emacs-int-comp-subr--trampoline-646566696e652d6b6579_define_key_0...
//
// with the booting Emacs blocked in `read' on its pipe until it finishes.
// `sandbox/doom/init.el' turns JIT compilation off, and that does NOT cover
// this: trampoline synthesis is governed by `native-comp-enable-subr-trampolines'
// and happens whether or not the JIT is armed.
//
// The image's `doom sync --aot' never produces these, because none of the
// advice runs during a sync -- so EVERY scenario compiled all nine into its
// own throwaway `~/.emacs.d' cache, forty-five times a run, concurrently.
// On 2026-09-10 four scenarios died of it in one run: Doom's 3.5s boot bound
// missed with the breadcrumb stopping at "init.el finished", `emacsclient'
// then failing with "exit status 1" because the server that boot had not
// reached yet could not answer, and the scenario reported as a wedge.
//
// TURNING TRAMPOLINES OFF IS NOT THE FIX, and would be a hole in the layer
// rather than a speedup: `lisp/status.el' advises `modify-frame-parameters'
// and `lisp/close-panels-on-open.el' advises `set-window-buffer', so an Emacs
// without trampolines is one where THIS MODULE'S OWN production advice is
// bypassed by every natively compiled caller. The layer would go on passing
// while testing something the user never runs.
//
// So they are compiled ONCE, ahead of every scenario, into a container-wide
// directory that `EMACSNATIVELOADPATH' puts on `native-comp-eln-load-path'.
// `comp-trampoline-search' consults that path BEFORE compiling, so every
// scenario boot finds all nine already built and compiles nothing.
const trampolineCacheDir = "/tmp/agent-repl-e2e-trampolines"

// emacsAdvisedPrimitives is every primitive a scenario's Emacs redefines, as
// OBSERVED: it is the key set of `comp-installed-trampolines-h' after a boot,
// not a reading of the sources. Six come from this module (`status.el',
// `close-panels-on-open.el', `find-file-workspace.el'), `define-key' from
// Doom's `general-auto-unbind-keys', and `yes-or-no-p' from the unanswerable
// -prompt guard the harness itself installs in e2e-settings.el.
//
// A NAME MISSING FROM THIS LIST CANNOT ROT SILENTLY: assertTrampolinesWerePrewarmed
// fails the scenario that installs one from anywhere but this cache.
var emacsAdvisedPrimitives = []string{
	"define-key",
	"modify-frame-parameters",
	"read-key-sequence",
	"read-key-sequence-vector",
	"select-window",
	"set-window-buffer",
	"use-global-map",
	"use-local-map",
	"yes-or-no-p",
}

// trampolinePrewarmBound bounds the one prewarm. It is nine `call-process'
// compilations of a second Emacs each, run serially. MEASURED at 887ms for
// all nine on a cold container with the machine otherwise idle, which is
// what it always is here: the prewarm runs holding a parallelism slot,
// before any scenario's Emacs exists. The bound is that measurement's own
// order of magnitude over again, for a box under other agents' load.
const trampolinePrewarmBound = 20 * time.Second

var trampolinePrewarm struct {
	sync.Once
	err  error
	took time.Duration
}

// prewarmTrampolines builds every subr trampoline this layer needs, once per
// test binary, before any scenario's Emacs starts.
//
// It is called with a parallelism slot already held, so the compilations get
// the machine to themselves rather than racing the boots they exist to spare.
// A failure here fails EVERY caller, not only the first: an unwarmed cache is
// the flake this exists to remove, and a silent fallback to per-scenario
// compilation would hide it again.
func prewarmTrampolines(t *testing.T, box sandbox) {
	t.Helper()
	trampolinePrewarm.Do(func() {
		started := time.Now()
		form := fmt.Sprintf(`(progn
  (require 'comp)
  (require 'comp-run)
  (let ((native-compile-target-directory %q))
    (dolist (subr '(%s))
      (unless (comp--trampoline-search subr)
        (comp-trampoline-compile subr)))))`,
			trampolineCacheDir+"/", strings.Join(emacsAdvisedPrimitives, " "))
		ctx, cancel := context.WithTimeout(context.Background(), trampolinePrewarmBound)
		defer cancel()
		out, err := box.Exec(ctx, "env", "EMACSNATIVELOADPATH="+trampolineCacheDir,
			"emacs", "-Q", "--batch", "--eval", form)
		trampolinePrewarm.took = time.Since(started)
		if err != nil {
			trampolinePrewarm.err = fmt.Errorf("compile the subr trampolines into %s: %w\n%s",
				trampolineCacheDir, err, out)
		}
	})
	if trampolinePrewarm.err != nil {
		t.Fatalf("the shared subr-trampoline cache could not be built, and every "+
			"scenario boot would otherwise compile all %d of them itself: %v",
			len(emacsAdvisedPrimitives), trampolinePrewarm.err)
	}
	t.Logf("subr trampolines prewarmed in %s (bound %s)", trampolinePrewarm.took.Round(time.Millisecond), trampolinePrewarmBound)
}

// assertTrampolinesWerePrewarmed proves this boot compiled no trampoline.
//
// It reads where each installed trampoline was LOADED FROM rather than merely
// which ones exist, because that is the difference the bound is spent on: a
// trampoline already on disk costs a `native-elisp-load', and one that is not
// costs a whole child Emacs inside Doom's 3.5s boot.
//
// TWO DIRECTORIES ARE LEGITIMATE, and the second is why this is a stat rather
// than a prefix test. Four of the nine come from the shared prewarm cache.
// The other five are already in the image's own Doom eln cache -- `doom sync
// --aot' happens to produce them -- and reach the boot through the staged
// copy of it, so they load from a path under THIS scenario's `~/.emacs.d'
// that is nonetheless not this scenario's work. A staged copy is told from a
// fresh compilation by asking whether the same file exists in the image the
// staging copied from.
//
// Any primitive advised in future that nobody added to emacsAdvisedPrimitives
// therefore fails HERE, named, on its first scenario -- instead of returning
// as a boot-bound flake on a busy machine.
func (e *Emacs) assertTrampolinesWerePrewarmed() {
	e.t.Helper()
	loaded := e.EvalStrings(`(let (r)
  (maphash (lambda (name trampoline)
             (push (format "%s %s" name
                           (native-comp-unit-file
                            (subr-native-comp-unit trampoline)))
                   r))
           comp-installed-trampolines-h)
  r)`)
	image := os.Getenv("EMACSDIR")
	if image == "" {
		image = imageEmacsDir
	}
	var strays []string
	for _, entry := range loaded {
		name, file, ok := strings.Cut(entry, " ")
		if ok && e.trampolineWasOnDiskBeforeThisBoot(file, image) {
			continue
		}
		strays = append(strays, fmt.Sprintf("%s (from %s)", name, file))
	}
	if len(strays) > 0 {
		e.t.Fatalf("this boot built %d subr trampoline(s) itself instead of finding one "+
			"already on disk: %s\nAdd the name(s) to emacsAdvisedPrimitives so the prewarm "+
			"builds them once into %s, rather than every scenario paying a child Emacs "+
			"inside Doom's %s boot bound.",
			len(strays), strings.Join(strays, ", "), trampolineCacheDir, doomBootBound)
	}
}

// trampolineWasOnDiskBeforeThisBoot reports whether the trampoline loaded
// from file predates this scenario: it is either in the shared prewarm cache,
// or it is a staged copy of one the image already carried.
func (e *Emacs) trampolineWasOnDiskBeforeThisBoot(file, image string) bool {
	if strings.HasPrefix(file, trampolineCacheDir+"/") {
		return true
	}
	staged, err := filepath.Rel(e.EmacsDir, file)
	if err != nil || strings.HasPrefix(staged, "..") {
		return false
	}
	_, err = os.Stat(filepath.Join(image, staged))
	return err == nil
}

// imageEmacsDir is the image's own Doom install -- `doom install` + `doom
// sync` ran against it at BUILD time, so no test pays for either.
const imageEmacsDir = "/sandbox/emacs.d"

// doomReadOnlySubdir is the one entry of the image's `.local` tree that is
// staged as a SYMLINK rather than copied: `straight/` holds every package
// checkout and build, it is large, and nothing writes to it outside a `doom
// sync`, which a test never runs.
const doomReadOnlySubdir = "straight"

// stageEmacsDir builds this test's `~/.emacs.d` out of the image's.
//
// The image's Emacs 30.2 has `--init-directory` (Emacs 29+), but that alone
// would not be enough, and the staging is what resolves the real constraint:
// the container runs `--read-only`, and `/sandbox/emacs.d` is NOT one of its
// tmpfs mounts, so Doom's own local tree cannot be written in place no
// matter how init is pointed at it. HOME is used to aim Emacs at a per-test
// init tree instead, which needs the tree staged into writable scratch.
//
// So Doom's sources are symlinked (read-only is fine; they are only loaded)
// and its `.local` tree is copied into the scratch, minus `straight/`, which
// is symlinked for size. Anything Doom writes at startup then lands in the
// test's own scratch and dies with it.
//
// UNVERIFIED: no container has ever run this. Docker is down on the machine
// this was authored on, so which paths Doom actually writes at startup --
// and therefore whether the copy set is exactly right -- is a claim about
// Doom's layout, not an observation. A boot that fails on a read-only path
// fails LOUDLY with the pty output, which is what awaitDoom is for.
func (e *Emacs) stageEmacsDir() {
	e.t.Helper()

	src := os.Getenv("EMACSDIR")
	if src == "" {
		src = imageEmacsDir
	}
	entries, err := os.ReadDir(src)
	if err != nil {
		e.t.Fatalf("read the image's Doom install at %s: %v", src, err)
	}
	// 0o700: Emacs 30 refuses a user-emacs-directory "accessible by others"
	// and Doom's early-init aborts before any stamp is written.
	if err := os.MkdirAll(e.EmacsDir, 0o700); err != nil {
		e.t.Fatalf("prepare %s: %v", e.EmacsDir, err)
	}

	for _, entry := range entries {
		from := filepath.Join(src, entry.Name())
		to := filepath.Join(e.EmacsDir, entry.Name())
		if entry.Name() != ".local" {
			if err := os.Symlink(from, to); err != nil {
				e.t.Fatalf("stage %s: %v", to, err)
			}
			continue
		}
		if err := os.MkdirAll(to, 0o755); err != nil {
			e.t.Fatalf("prepare %s: %v", to, err)
		}
		local, err := os.ReadDir(from)
		if err != nil {
			e.t.Fatalf("read the image's Doom local tree at %s: %v", from, err)
		}
		for _, l := range local {
			lFrom := filepath.Join(from, l.Name())
			lTo := filepath.Join(to, l.Name())
			if l.Name() == doomReadOnlySubdir {
				if err := os.Symlink(lFrom, lTo); err != nil {
					e.t.Fatalf("stage %s: %v", lTo, err)
				}
				continue
			}
			ctx, cancel := context.WithTimeout(context.Background(), doomStageBound)
			out, err := e.box.Exec(ctx, "cp", "-a", lFrom, lTo)
			cancel()
			if err != nil {
				e.t.Fatalf("stage %s into the scratch: %v\n%s", lFrom, err, out)
			}
		}
	}
}

// rootListing renders the top level of this Emacs's scratch root: one line per
// entry, with its type and mode.
//
// WHY A BOOT FAILURE CARRIES IT. The root is minted per scenario and deleted
// with the test, so anything unexpected in it exists only for as long as the
// failure does. It is the deciding evidence for a whole family of boot
// failures whose message names none of it -- most sharply `server-start`'s
// "Cannot bind server socket: Address already in use", which Emacs raises for
// EXACTLY ONE cause: an entry at the socket path that `server-stop` could not
// delete (a directory, or one this user may not unlink). A stale socket is
// deleted and rebound, and a LIVE one is reported as a warning, so the error
// says nothing about which of the two it hit and the listing says everything.
//
// AND AN EMPTY LISTING IS ITSELF A FINDING, which is how the defect this
// comment used to point away from was caught. A bind error with no `server`
// entry in the root means the socket that failed to bind was NOT this
// scenario's: it was /tmp/emacs<uid>/server, bound under the DEFAULT
// `server-name` by Doom's own `use-package! server` before the boot hook had
// set the name, at a path every scenario in the container shares. See the
// header of `sandbox/doom/init.el`, which now pins `server-name` and
// `server-socket-dir` before anything can load the feature.
//
// It is a listing and never a walk: the root holds a staged Doom tree and a
// whole state root, and printing those would bury the one line that matters.
func (e *Emacs) rootListing() string {
	entries, err := os.ReadDir(e.Root)
	if err != nil {
		return fmt.Sprintf("\n(the scratch root %s could not be listed: %v)", e.Root, err)
	}
	var b strings.Builder
	fmt.Fprintf(&b, "\nthe scratch root %s holds:", e.Root)
	for _, entry := range entries {
		info, statErr := entry.Info()
		if statErr != nil {
			fmt.Fprintf(&b, "\n  %-24s (could not stat: %v)", entry.Name(), statErr)
			continue
		}
		fmt.Fprintf(&b, "\n  %-24s %s", entry.Name(), info.Mode())
	}
	return b.String()
}

// awaitDoom waits for the readiness stamp `sandbox/doom/config.el` writes.
//
// The stamp is the layer's Doom-initialized edge, and it is written AFTER
// `server-start`, so its appearance means both that Doom finished and that
// emacsclient will answer. A boot that fails writes the same file with
// `"ok": false` and the elisp error, which is reported as itself rather than
// as a socket that never appears.
func (e *Emacs) awaitDoom() {
	e.t.Helper()
	ctx, cancel := context.WithTimeout(context.Background(), doomBootBound)
	defer cancel()
	ticker := time.NewTicker(pollInterval)
	defer ticker.Stop()
	for {
		if e.proc.Exited() {
			e.t.Fatalf("emacs exited before Doom finished initializing; pty output:\n%s", e.proc.Output())
		}
		body, err := os.ReadFile(e.ReadyStamp)
		if err == nil {
			var ready DoomReady
			if jsonErr := json.Unmarshal(body, &ready); jsonErr != nil {
				e.t.Fatalf("decode the Doom readiness stamp %q: %v", string(body), jsonErr)
			}
			if !ready.OK {
				// THE ROOT'S OWN CONTENTS COME WITH IT. A boot error names
				// what Emacs could not do; several of them -- a server socket
				// that will not bind, a settings file that will not load, a
				// Doom tree Emacs refuses as unsafe -- are explained only by
				// what is actually sitting in the scratch root, which is
				// deleted with the test.
				// THE BREADCRUMB COMES WITH IT, exactly as it does for a boot
				// that never stamps at all. A GUI Emacs writes its messages
				// to the frame rather than the pty, so a failed stamp is
				// routinely accompanied by an EMPTY pty -- measured -- and
				// the breadcrumb is then the only record of which boot step
				// the error came out of.
				e.t.Fatalf("Doom failed to initialize: %s%s%s; pty output:\n%s",
					ready.Error, e.bootBreadcrumb(), e.rootListing(), e.proc.Output())
			}
			// The whole reason for booting Doom rather than `-Q` is that
			// these are real. A stamp that says otherwise means the profile
			// silently degraded, and every keybinding and popup assertion
			// below it would be vacuous.
			if !ready.Doom {
				e.t.Fatalf("Doom is not loaded in the e2e Emacs; the stamp reports %+v", ready)
			}
			if !ready.MapBang {
				e.t.Fatalf("`map!' is unbound in the e2e Emacs, so no keybinding is real; the stamp reports %+v", ready)
			}
			if !ready.PopupRule {
				e.t.Fatalf("`set-popup-rule!' is unbound, so config.el skipped the notes popup rule; the stamp reports %+v", ready)
			}
			if !ready.AgentRepl {
				e.t.Fatalf("`:app agent-repl' did not load; the stamp reports %+v", ready)
			}
			e.Doom = ready
			return
		}
		if !os.IsNotExist(err) {
			e.t.Fatalf("read the Doom readiness stamp: %v", err)
		}
		select {
		case <-ctx.Done():
			// The kernel's account comes with it, because a boot that
			// misses this bound writes NOTHING to the pty -- so the pty
			// output alone reports an empty string and explains nothing.
			// The Emacs has no readiness stamp and therefore no pid the Go
			// side knows, but the reaper finds it the same way it always
			// does: by this scenario's own paths in its environment.
			// AND THE NATIVE STACK. A boot that misses this bound is an
			// Emacs that is ALIVE and has published no server socket, so
			// there is nothing to ask it with: the breadcrumb says which
			// step it was in and only the debugger can say where inside it.
			e.t.Fatalf("Doom did not finish initializing within %s (no readiness stamp at %s)%s%s%s%s; pty output:\n%s",
				doomBootBound, e.ReadyStamp, e.bootBreadcrumb(), e.rootListing(),
				e.processSnapshot(), e.nativeBacktrace(), e.proc.Output())
		case <-ticker.C:
		}
	}
}

// envPrefix renders an environment as an `env` command prefix, which is how
// variables reach a process the sandbox starts.
func envPrefix(env []string) []string {
	return append([]string{"env"}, env...)
}

// awaitServer waits for the server socket to answer.
//
// It runs AFTER awaitDoom, and is not redundant with it: the stamp says the
// server was started, and this says emacsclient can actually reach it, which
// is the transport every readback below rides.
func (e *Emacs) awaitServer() {
	e.t.Helper()
	ctx, cancel := context.WithTimeout(context.Background(), emacsBootBound)
	defer cancel()
	ticker := time.NewTicker(pollInterval)
	defer ticker.Stop()
	var last error
	for {
		if e.proc.Exited() {
			e.t.Fatalf("emacs exited during boot; pty output:\n%s", e.proc.Output())
		}
		probe, probeCancel := context.WithTimeout(ctx, HeartbeatBound)
		_, err := e.box.Exec(probe, "emacsclient", "--socket-name", e.ServerSocket, "--eval", "(emacs-pid)")
		probeCancel()
		if err == nil {
			return
		}
		last = err
		select {
		case <-ctx.Done():
			e.t.Fatalf("emacs server did not answer within %s: %v; pty output:\n%s",
				emacsBootBound, last, e.proc.Output())
		case <-ticker.C:
		}
	}
}

// heartbeatProbe is the form each probe evaluates.
//
// It is NOT `(emacs-pid)' any more, and the reason is a hang this layer used
// to watch go by. A `y-or-n-p' or `completing-read' nobody can answer blocks
// the COMMAND LOOP but not the SERVER: `emacsclient --eval' is answered from
// a process filter, which Emacs still runs while it waits for the keystroke
// the prompt is asking for. So a standing prompt kept answering `(emacs-pid)'
// at full speed while the scenario's own eval sat unanswered until its bound
// killed it -- reported as "emacsclient: signal: killed", which names neither
// the prompt nor the form that raised it.
//
// The probe therefore reports the standing prompt as well as the pid. Nothing
// in this layer ever types into a minibuffer -- every prompting command is
// either resolved with `BindingFor' or has its reader stubbed for the
// duration of the call -- so a prompt that is still up is a prompt that will
// never come down.
const heartbeatProbe = `(if-let* ((win (active-minibuffer-window)))
                            (with-current-buffer (window-buffer win)
                              (concat "PROMPT " (or (minibuffer-prompt) "<no prompt text>")))
                          "")`

// bootBreadcrumb answers how far the boot got, from the file the sandbox
// profile appends a line to at each stage. It is the only witness a boot that
// never publishes a stamp leaves: the frame is graphical, so Emacs's own
// messages never reach the pty, and no server exists yet to be asked.
func (e *Emacs) bootBreadcrumb() string {
	body, err := os.ReadFile(e.ReadyStamp + ".progress")
	if err != nil {
		return fmt.Sprintf("\n  the boot left no breadcrumb at all (%v), so it died before %s was loaded",
			err, filepath.Join(e.EmacsDir, "init.el"))
	}
	return "\n  the boot got as far as:\n    " +
		strings.ReplaceAll(strings.TrimSpace(string(body)), "\n", "\n    ")
}

// armHeartbeat starts the wedge detector. It runs for the WHOLE life of the
// process, teardown included, because the recursion defect this exists to
// catch fired during close and kill.
func (e *Emacs) armHeartbeat() {
	ctx, cancel := context.WithCancel(context.Background())
	e.stopHeartbeat = cancel
	e.heartbeatDone = make(chan struct{})
	go func() {
		defer close(e.heartbeatDone)
		ticker := time.NewTicker(heartbeatInterval)
		defer ticker.Stop()
		// The prompt seen by the PREVIOUS probe. A wedge is declared only
		// once the SAME prompt has stood across two consecutive probes, so a
		// reader a scenario is in the middle of answering through
		// `execute-kbd-macro' cannot be mistaken for one nobody will.
		var standing string
		for {
			select {
			case <-ctx.Done():
				return
			case <-ticker.C:
			}
			probe, probeCancel := context.WithTimeout(ctx, HeartbeatBound)
			started := time.Now()
			out, err := e.box.Exec(probe, "emacsclient", "--socket-name", e.ServerSocket, "--eval", heartbeatProbe)
			probeCancel()
			if err == nil {
				if took := int64(time.Since(started)); took > e.heartbeatMax.Load() {
					e.heartbeatMax.Store(took)
				}
				prompt := heartbeatPrompt(out)
				if prompt != "" && prompt == standing {
					e.declareWedged(fmt.Sprintf(
						"a minibuffer prompt nobody can answer has stood for %s: %s",
						heartbeatInterval, prompt))
					return
				}
				standing = prompt
				continue
			}
			if ctx.Err() != nil || e.proc.Exited() {
				return
			}
			e.declareWedged(fmt.Sprintf("emacs did not answer a heartbeat probe within %s: %v", HeartbeatBound, err))
			return
		}
	}()
}

// heartbeatPrompt reads the probe's answer, which emacsclient prints as an
// elisp string literal, and answers the standing prompt or "" for none.
func heartbeatPrompt(out string) string {
	out = strings.TrimSpace(out)
	if unquoted, err := strconv.Unquote(out); err == nil {
		out = unquoted
	}
	return strings.TrimPrefix(out, "PROMPT ")
}

// declareWedged records the wedge once and fails the test.
//
// It never retries. A hang that clears itself is still the sentinel/
// kill-buffer recursion, and treating it as transient is how that defect
// stayed alive long enough to reach a user.
func (e *Emacs) declareWedged(cause string) {
	e.wedgeOnce.Do(func() {
		e.wedgeCause.Store(&cause)
		close(e.wedged)
		// THE NATIVE STACK IS TAKEN FIRST, and it is taken before anything
		// that could end the stall. `processSnapshot` reads /proc and costs
		// microseconds; the gdb attach after it is the only witness that
		// survives an Emacs answering nothing at all, and it is worthless
		// once `breakStall` has unwound the loop it exists to name.
		snapshot := e.processSnapshot()
		native := e.nativeBacktrace()
		e.t.Errorf("EMACS WEDGED: %s%s%s", cause, snapshot, native)
		e.dumpArtifacts()
		// LAST, because it waits for Emacs to come back. A wedged Emacs
		// answers nothing at all -- not the server socket, not a nested eval,
		// and (measured) not `debug-on-event''s SIGUSR2 either -- so the only
		// thing that can be recovered is what the profiler armed at boot
		// already recorded, read out once the stall ends.
		e.t.Logf("EMACS WEDGED, continued:%s", e.profileWhenStuck())
	})
}

// dumpArtifacts preserves what a wedge diagnosis needs. The state root is a
// scratch directory the sandbox sweeps, so a failure that left nothing
// behind used to have to be reproduced with instrumentation added.
func (e *Emacs) dumpArtifacts() {
	dir := os.Getenv(ArtifactsEnv)
	if dir == "" {
		e.t.Logf("emacs pty output (set %s to preserve full artifacts):\n%s",
			ArtifactsEnv, tailBytes([]byte(e.proc.Output()), artifactTailBytes))
		return
	}
	out := e.artifactDir(dir)
	if out == "" {
		return
	}
	path := filepath.Join(out, "emacs.pty.log")
	if err := os.WriteFile(path, []byte(e.proc.Output()), 0o644); err != nil {
		e.t.Logf("write %s: %v", path, err)
		return
	}
	e.t.Logf("emacs pty output preserved at %s", path)
	e.dumpMessages(out)
	// The scripted git's fixture file carries every call made against it,
	// and the state root carries the daemon's own logs: a prompt Emacs is
	// stuck on is usually explained by one of the two.
	// The Xvfb log is always preserved, not only when a scenario asked for
	// it: a GUI frame that never appears -- or a webview that never loads --
	// is usually explained there and nowhere else.
	for _, extra := range append([]string{e.StateDir, e.Display.LogPath}, e.ArtifactPaths...) {
		if extra == "" {
			continue
		}
		dest := filepath.Join(out, filepath.Base(extra))
		if err := copyTree(extra, dest); err != nil {
			e.t.Logf("preserve %s: %v", extra, err)
		}
	}
	e.preserveWorkspaceElispLogs(out, e.box.Scratch())
}

// preserveWorkspaceElispLogs copies every workspace-scoped ELISP sink this
// scenario wrote into the artifacts, and it closes a gap that cost a whole
// diagnosis.
//
// The daemon's workspace-bound records are minted under the state root and
// only SYMLINKED into the workspace (daemon/AGENTS.md "Logging"), so copying
// the state root collects them. The Emacs side is the other way round: the
// canonical `<workspace>/.claude/emacs/emacs.log` is the symlink and its
// durable target is minted in `temporary-file-directory`
// (`agent-repl--workspace-emacs-log-target`, lisp/core.el), which is outside
// the state root and is swept with the container.
//
// The consequence, observed on a red `TestEmacsHandoverTransfersAtFreeness`:
// the preserved artifacts held the module's GLOBAL elisp log and nothing else,
// so the run could be seen to decode two `WatchHostWorkspaceResponse` pushes
// and then fall silent -- while every record that says what host.el DID with
// them (`elisp.host.transferred`, `elisp.host.transferred-awaiting-successor`,
// `elisp.host.transferred-without-successor`, `elisp.host.adopt-*`) is
// workspace-scoped and had gone with the temp file. The one question the
// artifacts existed to answer was the one they could not.
//
// THE SWEEP IS OVER THE WHOLE SCRATCH SUBTREE, not over `e.Root`. `e.Root` is
// `<scratch>/emacs`, the Emacs HOME; the workspaces this layer registers and
// the worktrees the daemon mints for it are `e.Root`'s SIBLINGS under the same
// scratch directory, so a sweep rooted at `e.Root` finds nothing at all — which
// is what the first version of this did, silently, on a red run that needed it.
// Sweeping by SHAPE from the scratch root catches both, with no per-site
// bookkeeping. `copyTree` resolves symlinks, so what lands in the artifacts is
// the durable target's own bytes.
//
// The count is reported whether or not anything was found: "no workspace elisp
// sinks" is itself a diagnosis, and reporting only the non-empty case is how
// an empty sweep went unnoticed.
func (e *Emacs) preserveWorkspaceElispLogs(out string, root string) {
	if root == "" {
		return
	}
	copied := 0
	err := filepath.WalkDir(root, func(path string, entry fs.DirEntry, err error) error {
		if err != nil {
			// An unreadable subtree is reported and stepped over: one
			// unreachable directory must not cost the sweep every sink
			// after it.
			e.t.Logf("preserve workspace elisp logs: walk %s: %v", path, err)
			return nil
		}
		if !entry.IsDir() {
			return nil
		}
		// The staged Doom tree and any node_modules under the scratch root
		// hold tens of thousands of files and no workspace sink; walking them
		// costs the whole sweep and finds nothing.
		if entry.Name() == ".emacs.d" || entry.Name() == "node_modules" {
			return fs.SkipDir
		}
		if entry.Name() != "emacs" || filepath.Base(filepath.Dir(path)) != ".claude" {
			return nil
		}
		rel, relErr := filepath.Rel(root, path)
		if relErr != nil {
			e.t.Logf("preserve workspace elisp logs: relate %s: %v", path, relErr)
			return fs.SkipDir
		}
		if copyErr := copyTree(path, filepath.Join(out, "workspace-elisp-logs", rel)); copyErr != nil {
			e.t.Logf("preserve %s: %v", path, copyErr)
		} else {
			copied++
		}
		return fs.SkipDir
	})
	if err != nil {
		e.t.Logf("preserve workspace elisp logs under %s: %v", root, err)
	}
	e.t.Logf("preserved %d workspace elisp sink(s) from %s under %s", copied, root, filepath.Join(out, "workspace-elisp-logs"))
}

// messagesTailLines is how much of `*Messages*` a failure carries.
//
// The whole buffer is unbounded and mostly Doom's own boot chatter; what a
// failure turns on is what Emacs said LAST, which is where a `user-error`, a
// refused command and the module's own echo-area messages land.
const messagesTailLines = 200

// messagesFile is where that tail is filed with the other evidence.
const messagesFile = "emacs.messages.log"

// dumpMessages preserves the tail of Emacs's own `*Messages*` buffer.
//
// WHY IT IS PART OF EVERY FAILURE AND NOT A WEDGE-ONLY EXTRA. A GUI frame
// writes its messages INTO THE FRAME, not to the pty, so the pty output a
// failure already carries is routinely EMPTY -- measured. `*Messages*` is
// then the only record of what Emacs said, and the elisp error text a
// scenario needs is in it and nowhere else.
//
// It never fails the test: this is evidence about a failure that has already
// been reported, so every way it can come up empty is said in place of the
// tail rather than swallowed.
func (e *Emacs) dumpMessages(out string) {
	if e.isWedged() {
		// A wedged Emacs answers nothing, and asking would only spend a
		// bound. `nativeBacktrace` and the profiler are that case's witnesses.
		return
	}
	form := fmt.Sprintf(`(with-current-buffer "*Messages*"
             (let ((end (point-max)))
               (save-excursion
                 (goto-char end)
                 (forward-line %d)
                 (buffer-substring-no-properties (point) end))))`, -messagesTailLines)
	res, err := e.eval(form)
	if err != nil {
		e.t.Logf("emacs could not be asked for its *Messages*: %v", err)
		return
	}
	if !res.OK {
		e.t.Logf("reading emacs's *Messages* signalled in emacs: %s", res.Error)
		return
	}
	var text string
	if unmarshalErr := json.Unmarshal(res.Value, &text); unmarshalErr != nil {
		e.t.Logf("emacs's *Messages* did not come back as a string: %s", res.Value)
		return
	}
	path := filepath.Join(out, messagesFile)
	if writeErr := os.WriteFile(path, []byte(text), 0o644); writeErr != nil {
		e.t.Logf("write %s: %v", path, writeErr)
		return
	}
	e.t.Logf("emacs *Messages* tail preserved at %s", path)
}

// artifactDirName is not unique enough on its own, and this is what makes it
// so. A `-count=N` run repeats ONE test name, so every repetition resolved to
// the same directory and each failure ERASED the one before it -- which for a
// defect that fires once in twenty-four runs means the evidence collected is
// the evidence of whichever repetition happened to fail last. The first free
// suffix is claimed with `os.Mkdir`, the one filesystem operation that is
// atomic and fails if the name exists, so two Emacsen failing at once cannot
// both believe they own the same directory. Memoized, because one Emacs's
// wedge report and its teardown dump belong together.
//
// Answers "" when no directory could be made, having said why.
func (e *Emacs) artifactDir(root string) string {
	e.artifactDirOnce.Do(func() {
		if err := os.MkdirAll(root, 0o755); err != nil {
			e.t.Logf("preserve emacs artifacts under %s: %v", root, err)
			return
		}
		base := filepath.Join(root, artifactDirName(e.t.Name()))
		for n := 0; ; n++ {
			dir := base
			if n > 0 {
				dir = fmt.Sprintf("%s.%d", base, n)
			}
			err := os.Mkdir(dir, 0o755)
			if err == nil {
				e.artifactRoot = dir
				return
			}
			if os.IsExist(err) {
				continue
			}
			e.t.Logf("preserve emacs artifacts under %s: %v", dir, err)
			return
		}
	})
	return e.artifactRoot
}

// copyTree copies a file or a directory tree; a missing source is not an
// error, since a failure may predate the source's creation.
func copyTree(src, dest string) error {
	info, err := os.Lstat(src)
	if os.IsNotExist(err) {
		return nil
	}
	if err != nil {
		return err
	}
	if info.Mode()&os.ModeSymlink != 0 {
		target, err := filepath.EvalSymlinks(src)
		if err != nil {
			return err
		}
		return copyTree(target, dest)
	}
	if !info.IsDir() {
		// ONLY REGULAR FILES ARE COPYABLE. The state root holds the daemon's
		// live unix sockets, and opening one to read it fails with ENXIO --
		// which used to abort the whole artifact sweep at whichever socket it
		// reached first, losing every log after it. A socket carries no
		// diagnosis anyway; its presence is already visible in the tree.
		if !info.Mode().IsRegular() {
			return nil
		}
		body, err := os.ReadFile(src)
		if err != nil {
			return err
		}
		return os.WriteFile(dest, body, 0o644)
	}
	entries, err := os.ReadDir(src)
	if err != nil {
		return err
	}
	if err := os.MkdirAll(dest, 0o755); err != nil {
		return err
	}
	for _, entry := range entries {
		if err := copyTree(filepath.Join(src, entry.Name()), filepath.Join(dest, entry.Name())); err != nil {
			return err
		}
	}
	return nil
}

// stop tears Emacs down. Registered via t.Cleanup, so it runs before the
// sidecar's and the store's cleanups, which were registered earlier.
func (e *Emacs) stop() {
	if e.stopHeartbeat != nil {
		e.stopHeartbeat()
		<-e.heartbeatDone
	}
	if e.proc.Exited() {
		return
	}

	// Ask the DAEMON to exit through Emacs's own command: per daemon.el,
	// "EMACS NEVER KILLS A DAEMON". A wedged Emacs cannot honor this, which
	// is exactly why the kill below is unconditional.
	asked := false
	stopAsked := false
	if !e.isWedged() {
		// ONLY A WORLD WITH A DAEMON IS ASKED TO STOP ONE. Several worlds in
		// this layer never spawn a daemon at all -- the substrate pages put a
		// bare webview in a bare Emacs -- and `agent-repl-frontend-daemon-stop`
		// answers those with a REFUSAL (`elisp.daemon.stop-skipped
		// reason=no-link`), which the report below then files as an unaccepted
		// stop. A refusal that every daemon-less teardown produces is noise
		// covering the refusals that mean something, so the question is asked
		// only where there is something to answer it.
		if e.holdsDaemonLink() {
			e.askDaemonToStop()
			stopAsked = true
		} else {
			e.noteTeardownStep("no daemon link was held, so no daemon stop was asked")
		}

		ctx, cancel := context.WithTimeout(context.Background(), DefaultTimeout)
		// `kill-emacs' never answers by design -- Emacs exits with the client
		// still waiting -- so only a TIMEOUT is worth reporting: it means
		// Emacs neither answered nor died, and the reaper is about to find it.
		_, err := e.box.Exec(ctx, "emacsclient", "--socket-name", e.ServerSocket,
			"--eval", "(kill-emacs)")
		if err != nil && ctx.Err() != nil {
			e.t.Logf("emacs did not act on (kill-emacs) within %s: %v", DefaultTimeout, err)
		}
		cancel()
		asked = true
	}
	// AND THEN WAIT FOR IT. Asking is not stopping.
	e.awaitEmacsExit(asked)
	e.proc.Kill()
	e.awaitDaemonExit(stopAsked)
}

// holdsDaemonLink answers whether this Emacs has a daemon to be asked to
// stop -- the same link `agent-repl-frontend-daemon-stop` refuses without.
//
// AN UNANSWERED QUESTION IS A YES. An Emacs that could not answer may still
// hold a live daemon, and skipping the stop on an unanswered question would
// strand exactly the tree the stop exists to take down; the failure is
// reported and the stop is asked anyway.
func (e *Emacs) holdsDaemonLink() bool {
	ctx, cancel := context.WithTimeout(context.Background(), teardownStopBound)
	defer cancel()
	ticker := time.NewTicker(pollInterval)
	defer ticker.Stop()
	for {
		out, err := e.box.Exec(ctx, "emacsclient", "--socket-name", e.ServerSocket,
			"--eval", settleDaemonLinkForm)
		if err != nil {
			e.t.Logf("emacs was asked whether it holds a daemon link and did not answer: %v", err)
			return true
		}
		switch answer := strings.Trim(strings.TrimSpace(out), `"`); answer {
		case "link":
			return true
		case "none":
			return false
		case "pending":
			e.noteTeardownStep("a daemon spawn or link acceptance was in flight, so its outcome was awaited")
		default:
			e.t.Logf("emacs answered the teardown's daemon-link question with %q", answer)
			return true
		}
		select {
		case <-ticker.C:
		case <-ctx.Done():
			e.t.Logf("a daemon spawn in flight did not settle within %s", teardownStopBound)
			return true
		}
	}
}

// settleDaemonLinkForm answers, in ONE command-loop turn, whether Emacs holds
// a daemon link the teardown must ask to stop.
//
// A LINK IS NOT THE ONLY WAY EMACS CAN OWN A DAEMON. A scenario that ends with
// the link down (a stopped daemon, a submit with none behind it) can leave an
// ensure in flight that has already SPAWNED a daemon and not yet linked to it:
// asking "is there a link?" then answered no, no stop was asked, and the
// daemon outlived the scenario as a stray. So while an ensure, or a link
// acceptance, is in flight the answer is "pending" and the teardown waits for
// its outcome. Once neither is, the SAME form disarms every trigger that could
// begin a new ensure -- the reconnect poll, the no-daemon hook, the cold-start
// idle timer -- so none can start between this answer and Emacs's exit:
// Emacs runs one form at a time, so the answer and the disarm are one act.
const settleDaemonLinkForm = `(cond
  ((or (bound-and-true-p agent-repl-daemon--ensure-in-flight)
       (bound-and-true-p agent-repl-link--pending))
   "pending")
  (t
   (when (fboundp 'agent-repl-link--cancel-reconnect)
     (agent-repl-link--cancel-reconnect))
   (when (boundp 'agent-repl-link-no-daemon-functions)
     (remove-hook 'agent-repl-link-no-daemon-functions #'agent-repl-daemon-ensure))
   (when (timerp (bound-and-true-p agent-repl-daemon--startup-timer))
     (cancel-timer agent-repl-daemon--startup-timer))
   (if (and (fboundp 'agent-repl-link-primary) (agent-repl-link-primary)) "link" "none")))`

// askDaemonToStop asks the daemon to exit through Emacs's own command and
// reports what came back.
func (e *Emacs) askDaemonToStop() {
	ctx, cancel := context.WithTimeout(context.Background(), teardownStopBound)
	out, err := e.box.Exec(ctx, "emacsclient", "--socket-name", e.ServerSocket,
		"--eval", daemonStopForm)
	cancel()
	if err != nil {
		// NOT DISCARDED. This is the ONE place Emacs asks the daemon to
		// exit, and every daemon, shim and shim-lock the reaper then
		// reports as a stray is downstream of it. Swallowing the error
		// left the reap looking like an unexplained leak.
		e.t.Logf("emacs was asked to stop its daemon and did not answer: %v", err)
	}
	// The OUTCOME, not merely the transport. `agent-repl-frontend-daemon-stop`
	// answers its callback with an ARM -- `(:arm :accepted)` on acceptance,
	// `(:arm :not-restarted :reason STRING)` on a refusal, a missing link or
	// a transport failure -- and a refused stop is exactly the case whose
	// leaked tree the reaper below then reports with no cause attached. The
	// accepted arm is matched by NAME rather than by truthiness: the callback
	// used to be handed a bare boolean, and reading the arm plist as one
	// silently reported every healthy teardown as unaccepted.
	if outcome := strings.TrimSpace(out); !strings.Contains(outcome, daemonStopAcceptedArm) {
		e.t.Logf("emacs's daemon stop was not accepted: %s", outcome)
	}
}

// daemonExitBound is how long the daemon this scenario asked Emacs to stop
// gets to actually be gone.
//
// IT IS PRODUCTION'S OWN BUDGET, ADDED UP, not a number chosen here. The stop
// is acked the moment the exit is STARTED -- `drain.ShutdownNow` announces,
// stands every session down, calls Exit and answers -- and everything after
// that ack is the daemon's orderly exit, whose every step is separately
// bounded: `shutdownGrace` (2s) for the in-flight requests, then
// `merge.TerminalDrainBound` (2s) for a merge terminal's durable stamps, then
// `loopJoinBound` (2s) for the background loops. Six seconds is the sum, so a
// daemon still here afterwards has missed a bound it states itself, which is a
// failure rather than a slow machine.
//
// MEASURED, and the healthy case never comes near it: `go test -v` prints
// `emacs phase daemon-exit` for every scenario, and on a quiet box it is a
// single poll interval -- the daemon is gone before the reaper's first /proc
// read. The observation that made this wait exist is the other end: with
// Emacs's own exit escalating (1.06s of a 500ms bound on a loaded host), the
// daemon's shutdown grace was still running when the reaper looked, because
// the grace does not end until the standing streams' client -- that Emacs --
// is gone. The reaper then reported a daemon that was exiting exactly as
// designed.
const daemonExitBound = 6 * time.Second

// awaitDaemonExit does not return until the daemon and the shims this scenario
// asked Emacs to stop are actually gone, or until their own bound expires.
//
// IT IS THE SECOND HALF OF "ASKING IS NOT STOPPING". The teardown asks Emacs
// to stop its daemon and WAITS for the ack, and it asks Emacs to exit and
// WAITS for the process -- and then it went straight to the reaper, which
// reads /proc once. But the ack says the exit was STARTED, not that it
// finished, so between the two there was a race the scenario could only lose:
// the daemon's shutdown grace is held open by the standing streams of the very
// Emacs teardown has just killed, so the slower Emacs is to go, the more of
// that grace is still running when the reaper looks.
//
// Nothing is loosened by waiting here. What is left when the bound expires is
// still the reaper's to report and still fails the scenario; the wait only
// stops the reaper from calling a bounded exit a leak.
func (e *Emacs) awaitDaemonExit(asked bool) {
	if !asked {
		// A wedged Emacs was never asked to stop its daemon, so nothing is on
		// its way out and there is nothing to wait for. The reaper below still
		// finds every one of them.
		return
	}
	started := time.Now()
	left := e.awaitStraysGone(daemonExitBound)
	e.t.Logf("emacs phase daemon-exit took %s (bound %s)",
		time.Since(started).Round(time.Millisecond), daemonExitBound)
	if len(left) == 0 {
		e.noteTeardownStep(fmt.Sprintf("the daemon and its shims exited on the stop within %s",
			time.Since(started).Round(time.Millisecond)))
		return
	}
	e.noteTeardownStep(fmt.Sprintf("waited %s for the stopped daemon to exit, still holding %s",
		daemonExitBound, pidList(left)))
}

// awaitEmacsExit does not return until this scenario's Emacs process is
// actually gone, or until every escalation has been sent AND waited on.
//
// It exists because killing the pty parent is not killing Emacs. `script'
// forks its child into a session of its own -- setsid plus TIOCSCTTY, which
// is how the child gets a controlling terminal -- so `e.proc.Kill()' reaps
// the wrapper and REPARENTS the ~200 MiB Emacs to init. That is the same
// trap the webapp layer's driver had. Before this, teardown asked Emacs to
// exit and then assumed it had; the reaper below routinely found it alive
// afterwards on scenarios that otherwise passed, and a scenario that ends
// with an Emacs of its own still running is the exact shape that earlier in
// this overhaul produced a shared-socket failure and a per-scenario leak
// that drove the layer's peak to 3 GiB.
//
// asked says whether `(kill-emacs)' was actually delivered. A WEDGED Emacs
// was never asked anything, so it skips the polite wait and goes straight to
// the signals: waiting out a bound for an answer nobody requested would only
// hold this scenario's parallelism slot.
func (e *Emacs) awaitEmacsExit(asked bool) {
	started := time.Now()
	// Logged here rather than through `record'/`reportPhases': t.Cleanup
	// unwinds LIFO, so reportPhases -- registered last -- has already run by
	// the time teardown reaches this. The line matches its format so the
	// same grep reads both, and it is emitted on a PASSING run, because a
	// passing run is where this bound's numbers come from.
	defer func() {
		e.t.Logf("emacs phase emacs-exit took %s (bound %s)",
			time.Since(started).Round(time.Millisecond), emacsExitBound)
	}()

	// THE GROUP IS TAKEN FIRST, WHILE EMACS IS STILL ALIVE TO NAME IT. Once
	// the process is gone its /proc entry is gone with it, and the children
	// it left cannot be tied to it by anything else: they carry the
	// scenario's HOME, which says whose scenario they are, and their process
	// group, which says whose EMACS they are.
	groups := e.emacsGroups()
	if len(groups) == 0 {
		// Emacs was already gone before teardown reached this. Nothing was
		// left to wait for, and there is no group to escalate against.
		return
	}

	if asked {
		if left := e.awaitGroupsGone(groups, emacsExitBound); len(left) == 0 {
			e.noteTeardownStep(fmt.Sprintf("emacs and its process group exited on (kill-emacs) within %s",
				time.Since(started).Round(time.Millisecond)))
			return
		}
		e.noteTeardownStep(fmt.Sprintf("waited %s for (kill-emacs) to be honored", emacsExitBound))
		e.t.Logf("emacs teardown: %s did not exit within %s of (kill-emacs); escalating to process group(s) %v",
			pidList(e.groupStrays(groups)), emacsExitBound, groups)
	}
	// THE GROUP, NOT THE PID, and it is measured rather than assumed to
	// matter: `script' runs Emacs in a session of its own, so Emacs leads a
	// group that also holds the WebKit network and web processes its panel's
	// webview starts -- both of which have been observed outliving Emacs and
	// reaching the reaper. The daemon and the shims are NOT in it; each of
	// those is spawned into a group of its own, so signalling here cannot
	// reach them.
	for _, sig := range []syscall.Signal{syscall.SIGTERM, syscall.SIGKILL} {
		live := e.groupStrays(groups)
		if len(live) == 0 {
			return
		}
		for _, g := range groups {
			e.signalGroup(g, sig)
		}
		e.noteTeardownStep(fmt.Sprintf("sent %v to process group(s) %v, holding %s", sig, groups, pidList(live)))
		if left := e.awaitGroupsGone(groups, emacsSignalBound); len(left) == 0 {
			return
		}
		e.noteTeardownStep(fmt.Sprintf("waited %s after %v", emacsSignalBound, sig))
	}
	// Anything still alive here is the reaper's to REPORT. Nothing is
	// swallowed and nothing is retried silently: reapStrays runs last, finds
	// it, kills it, and fails this scenario with the list of steps above.
}

// emacsGroups answers the process groups this scenario's Emacsen lead.
//
// A group that reads back as the test binary's own -- or as init's -- is
// DROPPED and reported: `script' puts Emacs in a session of its own, so such
// a reading means the /proc read was wrong, and signalling it would signal
// the suite.
func (e *Emacs) emacsGroups() []int {
	self := syscall.Getpgrp()
	var out []int
	for _, p := range e.emacsProcs() {
		pgid, err := procPGID(p.pid)
		if err != nil {
			e.t.Logf("emacs teardown: cannot read the process group of pid %d (%v); it can only be signalled by pid", p.pid, err)
			out = append(out, p.pid)
			continue
		}
		if pgid <= 1 || pgid == self {
			e.t.Logf("emacs teardown: pid %d reports process group %d, which is not a group this scenario owns; it can only be signalled by pid",
				p.pid, pgid)
			out = append(out, p.pid)
			continue
		}
		if !containsInt(out, pgid) {
			out = append(out, pgid)
		}
	}
	return out
}

// groupStrays answers this scenario's live processes that belong to one of
// the given process groups.
func (e *Emacs) groupStrays(groups []int) []stray {
	var out []stray
	for _, s := range e.findStrays() {
		pgid, err := procPGID(s.pid)
		if err != nil {
			// The process exited between the walk and this read. Not a
			// member of anything any more.
			continue
		}
		if containsInt(groups, pgid) {
			out = append(out, s)
		}
	}
	return out
}

// awaitGroupsGone polls until no process of this scenario remains in the
// given groups or the bound expires, answering whatever is left.
func (e *Emacs) awaitGroupsGone(groups []int, bound time.Duration) []stray {
	return awaitGone(bound, func() []stray { return e.groupStrays(groups) })
}

// signalGroup sends one signal to one process group, reporting anything but
// "it is already gone".
func (e *Emacs) signalGroup(pgid int, sig syscall.Signal) {
	if err := syscall.Kill(-pgid, sig); err != nil && !errors.Is(err, syscall.ESRCH) {
		e.t.Logf("emacs teardown: sending %v to process group %d failed: %v", sig, pgid, err)
	}
}

// containsInt reports whether a small int slice holds one value.
func containsInt(list []int, want int) bool {
	for _, v := range list {
		if v == want {
			return true
		}
	}
	return false
}

// emacsProcs answers the live processes of this scenario that are Emacs
// itself, as opposed to the daemon, the shims or anything else the reaper
// matches on the same paths.
//
// The discriminator is `comm', not the argv: Emacs is started as bare `emacs'
// through `env', which execs, so its argv carries no path at all and only its
// ENVIRONMENT ties it to this scenario -- which is exactly what findStrays
// matches on.
func (e *Emacs) emacsProcs() []stray {
	var out []stray
	for _, s := range e.findStrays() {
		if procField(strconv.Itoa(s.pid), "comm") == "emacs" {
			out = append(out, s)
		}
	}
	return out
}

// procPGID reads the process group id of one pid from /proc.
func procPGID(pid int) (int, error) {
	raw, err := os.ReadFile(filepath.Join("/proc", strconv.Itoa(pid), "stat"))
	if err != nil {
		return 0, err
	}
	text := strings.TrimSpace(string(raw))
	// The comm field is parenthesized and may itself contain spaces, so the
	// fields that matter start after the last ")": state, ppid, pgrp.
	idx := strings.LastIndex(text, ")")
	if idx < 0 {
		return 0, fmt.Errorf("no comm field in /proc/%d/stat", pid)
	}
	fields := strings.Fields(text[idx+1:])
	if len(fields) < 3 {
		return 0, fmt.Errorf("/proc/%d/stat has %d fields after comm, want at least 3", pid, len(fields))
	}
	pgid, err := strconv.Atoi(fields[2])
	if err != nil {
		return 0, fmt.Errorf("process group of pid %d is not a number (%q): %w", pid, fields[2], err)
	}
	return pgid, nil
}

// pidList renders a stray list as pids, for one teardown message.
func pidList(list []stray) string {
	if len(list) == 0 {
		return "no process"
	}
	parts := make([]string, 0, len(list))
	for _, s := range list {
		parts = append(parts, "pid "+strconv.Itoa(s.pid))
	}
	return strings.Join(parts, ", ")
}

// noteTeardownStep records one thing teardown tried, so a stray the reaper
// finds afterwards is reported with what had already been done to it rather
// than as an unexplained survivor.
func (e *Emacs) noteTeardownStep(step string) {
	e.teardownMu.Lock()
	defer e.teardownMu.Unlock()
	e.teardownSteps = append(e.teardownSteps, step)
}

// teardownTried renders those steps for the reaper's failure message.
func (e *Emacs) teardownTried() string {
	e.teardownMu.Lock()
	defer e.teardownMu.Unlock()
	if len(e.teardownSteps) == 0 {
		return "nothing (teardown never reached the exit wait)"
	}
	return strings.Join(e.teardownSteps, "; ")
}

func (e *Emacs) isWedged() bool {
	select {
	case <-e.wedged:
		return true
	default:
		return false
	}
}

// evalResult is what the readback helper writes.
type evalResult struct {
	OK    bool            `json:"ok"`
	Value json.RawMessage `json:"value"`
	Error string          `json:"error"`
}

// Eval evaluates one elisp form in Emacs and returns its value as JSON.
//
// The form must produce JSON-encodable data — numbers, strings, booleans,
// lists, alists. That is not a limitation in practice: the rule this layer
// works by is to read state from the VARIABLE that holds it, and a form that
// wants a hash table's contents maps over it. Returning a buffer, window or
// process object is a defect in the scenario, not in this helper.
func (e *Emacs) Eval(form string) json.RawMessage {
	e.t.Helper()
	res, err := e.eval(form)
	if err != nil {
		e.t.Fatalf("eval %s: %v", summarize(form), err)
	}
	if !res.OK {
		e.t.Fatalf("eval %s signalled in emacs: %s", summarize(form), res.Error)
	}
	return res.Value
}

// eval is the non-fatal core, so wait loops can poll without failing.
func (e *Emacs) eval(form string) (evalResult, error) {
	var zero evalResult
	if e.isWedged() {
		return zero, fmt.Errorf("emacs is wedged: %s", e.wedgeCauseString())
	}

	n := e.evalSeq.Add(1)
	in := filepath.Join(e.Root, "eval", fmt.Sprintf("%d.in.el", n))
	out := filepath.Join(e.Root, "eval", fmt.Sprintf("%d.out.json", n))
	if err := os.WriteFile(in, []byte(form), 0o644); err != nil {
		return zero, fmt.Errorf("write the eval request: %w", err)
	}

	ctx, cancel := context.WithTimeout(context.Background(), evalBound)
	defer cancel()
	call := fmt.Sprintf("(agent-repl-e2e--eval %q %q)", in, out)
	started := time.Now()
	_, err := e.box.Exec(ctx, "emacsclient", "--socket-name", e.ServerSocket, "--eval", call)
	e.recordEvalMax(time.Since(started))
	if err != nil {
		// THE FORM IS STILL RUNNING. Killing emacsclient closes one socket;
		// it does not unwind the elisp underneath, and Emacs answers a fresh
		// probe from a process filter NESTED INSIDE the form that overran. So
		// the stack is asked for right here, while the evidence still exists
		// -- one round trip later it is gone.
		// The kernel snapshot is taken FIRST and the stack second: reading
		// /proc costs microseconds and cannot be refused, while the stack
		// probe is a round trip that may itself have to time out. Asking in
		// the other order would describe the machine a second and a half
		// after the moment being diagnosed.
		snapshot := e.processSnapshot()
		// The NATIVE stack comes before the two lisp witnesses: both of those
		// are round trips a stalled Emacs cannot answer, and the second one
		// deliberately breaks the stall to get its answer, which destroys the
		// very loop the debugger would otherwise have named.
		native := e.nativeBacktrace()
		return zero, fmt.Errorf("emacsclient: %w%s%s%s%s", err, snapshot, native, e.stackWhenStuck(), e.profileWhenStuck())
	}

	body, readErr := os.ReadFile(out)
	if readErr != nil {
		return zero, fmt.Errorf("read the eval response: %w", readErr)
	}
	var res evalResult
	if err := json.Unmarshal(body, &res); err != nil {
		return zero, fmt.Errorf("decode the eval response %q: %w", string(body), err)
	}
	return res, nil
}

// nativeBacktraceBound is how long gdb is given to attach, unwind every
// thread and detach. It is not a healthy-phase measurement: the process it
// attaches to is by then already reported as failed, and this is the budget
// for the evidence about it. Measured on a healthy Emacs in this image, the
// whole capture takes about a second; the bound is a small multiple.
const nativeBacktraceBound = 20 * time.Second

// nativeBacktraceFile is where the full capture lands in the failure
// artifacts. The error message carries a trimmed head; the file carries every
// frame of every thread.
const nativeBacktraceFile = "emacs.native-backtrace.txt"

// nativeBacktraceHeadFrames is how much of the capture the failure message
// quotes inline BEYOND the main thread, which is never trimmed. Enough to
// name the loop without burying the failure.
const nativeBacktraceHeadFrames = 40

// nativeBacktrace is the LAST witness this layer has.
//
// An Emacs looping in C answers nothing a lisp witness can reach: not the
// server socket, not a nested eval, not `debug-on-event”s SIGUSR2, not a
// plain SIGINT. Every one of those needs Emacs to reach a QUIT check, and a C
// loop without one never does. What is still true is that the kernel holds
// the process's registers and stack, so a debugger attaching from OUTSIDE can
// say exactly which C function is spinning -- which is the one fact a stall
// like that turns on.
//
// It is taken WHILE THE STALL IS LIVE, which is why it is called from
// `declareWedged` before anything that waits for Emacs to come back.
func (e *Emacs) nativeBacktrace() string {
	e.nativeOnce.Do(func() { e.nativeStack = e.captureNativeBacktrace() })
	return e.nativeStack
}

// captureNativeBacktrace runs the debugger once. It never fails a test on its
// own: it is evidence about a failure that has already been reported, so
// every way it can come up empty is REPORTED IN PLACE OF the backtrace rather
// than swallowed.
func (e *Emacs) captureNativeBacktrace() string {
	pid := e.emacsPID()
	if pid == 0 {
		return "\n  (no native backtrace: no emacs pid, from the readiness stamp or from /proc)"
	}
	gdb, err := exec.LookPath("gdb")
	if err != nil {
		return fmt.Sprintf("\n  (no native backtrace: gdb is not on PATH here: %v)", err)
	}
	ctx, cancel := context.WithTimeout(context.Background(), nativeBacktraceBound)
	defer cancel()
	// `-batch` runs the -ex list and quits; `-nx` keeps a stray ~/.gdbinit
	// out of it. `detach` before `quit` is explicit rather than relied upon:
	// gdb kills only inferiors IT started and detaches the ones it attached
	// to, but this one must never be the thing that ends the process the
	// test is still tearing down.
	cmd := exec.CommandContext(ctx, gdb,
		"-nx", "-batch",
		"-ex", "set pagination off",
		"-ex", "set confirm off",
		"-ex", "info threads",
		// THE MAIN THREAD IS ASKED FOR FIRST, BY NAME. `thread apply all bt`
		// walks threads in DESCENDING order, so on an Emacs with glib helper
		// threads the one that matters -- thread 1, the lisp interpreter --
		// comes out last, and a capture read inline had already run out of
		// room by the time it arrived (measured 2026-09-04 on
		// TestEmacsVisitingAFileRoutesIntoItsOwningWorkspace, whose inline
		// excerpt printed threads 4, 3 and 2 and cut off before thread 1).
		// Asking for it separately puts it at the top of the capture, ahead
		// of the walk that repeats it.
		"-ex", "thread apply 1 bt",
		"-ex", "thread apply all bt",
		"-ex", "detach",
		"-ex", "quit",
		"-p", strconv.Itoa(pid))
	out, runErr := cmd.CombinedOutput()
	text := strings.TrimSpace(string(out))
	if runErr != nil && text == "" {
		return fmt.Sprintf("\n  (no native backtrace: gdb -p %d failed: %v)", pid, runErr)
	}
	if text == "" {
		return fmt.Sprintf("\n  (no native backtrace: gdb -p %d said nothing)", pid)
	}
	// gdb's own exit status is reported alongside the output rather than
	// instead of it: a partial unwind still names the frame that matters.
	if runErr != nil {
		text += fmt.Sprintf("\n(gdb exited with: %v)", runErr)
	}
	e.writeNativeBacktrace(text)
	return "\n  emacs's native stack, from outside the process:\n    " +
		strings.Join(nativeBacktraceExcerpt(text), "\n    ")
}

// nativeBacktraceMainThread names the thread whose stack is the answer. Emacs
// runs lisp on ONE thread and gdb numbers it 1; the rest are glib's helpers,
// which are asleep in `poll` in every capture ever taken here and say nothing
// about why the editor stopped.
const nativeBacktraceMainThread = "Thread 1 "

// nativeBacktraceExcerpt is what the failure message quotes inline.
//
// WHAT IT GUARANTEES, and the reason it is not a plain head: THE MAIN
// THREAD'S STACK APPEARS IN FULL, FIRST, whatever else is trimmed. gdb's own
// output puts it last and a head-of-N excerpt therefore dropped exactly the
// frames the failure turns on. Everything before the first thread section --
// gdb's preamble and its `info threads` table -- is kept as the index to what
// follows, and the remaining threads fill whatever budget is left.
func nativeBacktraceExcerpt(text string) []string {
	lines := strings.Split(text, "\n")
	preamble, sections := nativeBacktraceSections(lines)

	out := append([]string{}, preamble...)
	var rest [][]string
	seenMain := false
	for _, section := range sections {
		if !seenMain && strings.HasPrefix(section[0], nativeBacktraceMainThread) {
			// The main thread, in full, immediately after the index. gdb
			// prints it twice (once for `thread apply 1 bt`, once inside the
			// `all` walk); only the first copy is quoted.
			seenMain = true
			out = append(out, section...)
			continue
		}
		if seenMain && strings.HasPrefix(section[0], nativeBacktraceMainThread) {
			continue
		}
		rest = append(rest, section)
	}

	budget := nativeBacktraceHeadFrames
	dropped := 0
	for _, section := range rest {
		if budget-len(section) < 0 {
			dropped += len(section)
			continue
		}
		budget -= len(section)
		out = append(out, section...)
	}
	if dropped > 0 {
		out = append(out, fmt.Sprintf("... %d more lines in %s", dropped, nativeBacktraceFile))
	}
	return out
}

// nativeBacktraceSections splits a capture into gdb's preamble and one group
// of lines per thread. A thread's group runs from its `Thread N (...)` header
// to the line before the next one, so a stack is never cut in half.
func nativeBacktraceSections(lines []string) (preamble []string, sections [][]string) {
	current := -1
	for _, line := range lines {
		if strings.HasPrefix(line, "Thread ") && strings.Contains(line, "(") {
			sections = append(sections, []string{line})
			current = len(sections) - 1
			continue
		}
		if current < 0 {
			preamble = append(preamble, line)
			continue
		}
		sections[current] = append(sections[current], line)
	}
	return preamble, sections
}

// emacsPID answers the Emacs this scenario owns.
//
// The readiness stamp is the authority once it exists, but a boot that never
// PUBLISHES one is exactly a case the native backtrace is for -- so the
// kernel's own view is the fallback, found the same way the reaper finds
// strays: by this scenario's paths in the process's environment.
func (e *Emacs) emacsPID() int {
	if e.Doom.PID != 0 {
		return e.Doom.PID
	}
	for _, p := range e.findStrays() {
		// `argv` is /proc/<pid>/cmdline with NULs turned into spaces, and
		// this layer starts Emacs as a bare `emacs` with every path in its
		// environment -- so the whole argv IS the program name. The `script`
		// and `sh` wrappers above it carry the full env-prefixed command
		// line, which is why an exact match is the right test and a prefix
		// one would catch the wrapper instead.
		if p.argv == "emacs" {
			return p.pid
		}
	}
	return 0
}

// writeNativeBacktrace files the full capture with the failure artifacts,
// where dumpArtifacts's other evidence for the same test already lands.
func (e *Emacs) writeNativeBacktrace(text string) {
	dir := os.Getenv(ArtifactsEnv)
	if dir == "" {
		return
	}
	out := e.artifactDir(dir)
	if out == "" {
		return
	}
	path := filepath.Join(out, nativeBacktraceFile)
	if err := os.WriteFile(path, []byte(text+"\n"), 0o644); err != nil {
		e.t.Logf("write %s: %v", path, err)
		return
	}
	e.t.Logf("emacs native backtrace preserved at %s", path)
}

// stackWhenStuck answers what Emacs is standing in, formatted for an error
// message, or "" if it cannot be asked. It never fails a test on its own: it
// is evidence about a failure that has already happened.
func (e *Emacs) stackWhenStuck() string {
	ctx, cancel := context.WithTimeout(context.Background(), HeartbeatBound)
	defer cancel()
	out, err := e.box.Exec(ctx, "emacsclient", "--socket-name", e.ServerSocket,
		"--eval", "(agent-repl-e2e--stack)")
	if err != nil {
		return fmt.Sprintf("\n  (emacs could not be asked what it was doing: %v)", err)
	}
	stack := strings.TrimSpace(out)
	if unquoted, uerr := strconv.Unquote(stack); uerr == nil {
		stack = unquoted
	}
	if stack == "" {
		return ""
	}
	return "\n  emacs was standing in: " + stack
}

// stallProfileFrames is how many sampled call chains a stall report carries.
// Enough that the hot path is not one line that could be a coincidence, few
// enough that the failure output stays readable.
const stallProfileFrames = 12

// stallProfileBound is how long Emacs is given to come back and hand over its
// profile. It is not a healthy-phase measurement: a stall is already a
// reported failure by the time this runs, and this is the budget for the
// evidence about it.
const stallProfileBound = 10 * time.Second

// profileWhenStuck answers where Emacs's CPU actually went.
//
// It RETRIES until Emacs answers, which is the whole point: a stalled Emacs
// answers nothing while it stalls, so the profile can only be collected once
// it is over. The sampling profiler was armed at boot precisely so that the
// evidence survives the window in which nothing can be asked.
func (e *Emacs) profileWhenStuck() string {
	// The debugger's own buffer comes back with the profile when SIGUSR2
	// managed to open it: it names the exact frame Emacs was standing in,
	// which the sampled chains can only suggest.
	form := fmt.Sprintf(`(concat (if-let* ((buf (get-buffer "*Backtrace*")))
                                     (concat "debugger backtrace:\n"
                                             (with-current-buffer buf (buffer-string))
                                             "\n")
                                   "")
                                 (agent-repl-e2e--cpu-profile %d))`, stallProfileFrames)
	deadline := time.Now().Add(stallProfileBound)
	var last error
	broken := false
	for {
		ctx, cancel := context.WithTimeout(context.Background(), HeartbeatBound)
		out, err := e.box.Exec(ctx, "emacsclient", "--socket-name", e.ServerSocket, "--eval", form)
		cancel()
		if err != nil && !broken {
			// A STALL THAT DOES NOT END ON ITS OWN still has to hand over its
			// profile, and the profile can only be read from an Emacs that is
			// answering again. So the loop is broken from outside, in the two
			// ways Emacs itself documents: SIGUSR2 is `debug-on-event', which
			// enters the debugger at the next safe point, and SIGINT is a
			// plain quit, which unwinds a Lisp loop that checks for one.
			// Neither kills Emacs, and both are sent only AFTER the wedge has
			// already been reported as a failure.
			broken = true
			e.breakStall()
			continue
		}
		if err == nil {
			text := strings.TrimSpace(out)
			if unquoted, uerr := strconv.Unquote(text); uerr == nil {
				text = unquoted
			}
			if text == "" {
				return "\n  (emacs recorded no cpu samples)"
			}
			return "\n  emacs's hottest sampled call chains:\n    " +
				strings.ReplaceAll(text, "\n", "\n    ")
		}
		last = err
		if time.Now().After(deadline) {
			return fmt.Sprintf("\n  (emacs never came back to hand over its cpu profile within %s: %v)",
				stallProfileBound, last)
		}
	}
}

// breakStall asks a stalled Emacs to stop, so that what it recorded can be
// read out. Best-effort and loud about every failure: the test it belongs to
// has already failed, and nothing here may replace that failure with another.
func (e *Emacs) breakStall() {
	if e.Doom.PID == 0 {
		e.t.Logf("cannot interrupt the stall: the readiness stamp carried no pid")
		return
	}
	for _, sig := range []syscall.Signal{syscall.SIGUSR2, syscall.SIGINT} {
		if err := syscall.Kill(e.Doom.PID, sig); err != nil {
			e.t.Logf("cannot send %v to emacs pid %d: %v", sig, e.Doom.PID, err)
		}
	}
}

func (e *Emacs) wedgeCauseString() string {
	if cause := e.wedgeCause.Load(); cause != nil {
		return *cause
	}
	return "cause unrecorded"
}

// EvalBool reads a form's value as a boolean. Elisp nil decodes as JSON
// null, which is how a false reaches here.
func (e *Emacs) EvalBool(form string) bool {
	e.t.Helper()
	raw := e.Eval(form)
	if isJSONNull(raw) {
		return false
	}
	var b bool
	if err := json.Unmarshal(raw, &b); err == nil {
		return b
	}
	// Any other non-nil elisp value is truthy, which matches elisp's own
	// rule and keeps a scenario from having to coerce.
	return true
}

// EvalString reads a form's value as a string.
func (e *Emacs) EvalString(form string) string {
	e.t.Helper()
	raw := e.Eval(form)
	if isJSONNull(raw) {
		return ""
	}
	var s string
	if err := json.Unmarshal(raw, &s); err != nil {
		e.t.Fatalf("eval %s: want a string, got %s", summarize(form), raw)
	}
	return s
}

// EvalInt reads a form's value as an integer.
func (e *Emacs) EvalInt(form string) int {
	e.t.Helper()
	raw := e.Eval(form)
	if isJSONNull(raw) {
		e.t.Fatalf("eval %s: want an integer, got nil", summarize(form))
	}
	var n int
	if err := json.Unmarshal(raw, &n); err != nil {
		e.t.Fatalf("eval %s: want an integer, got %s", summarize(form), raw)
	}
	return n
}

// EvalStrings reads a form's value as a list of strings.
func (e *Emacs) EvalStrings(form string) []string {
	e.t.Helper()
	raw := e.Eval(form)
	if isJSONNull(raw) {
		return nil
	}
	var xs []string
	if err := json.Unmarshal(raw, &xs); err != nil {
		e.t.Fatalf("eval %s: want a list of strings, got %s", summarize(form), raw)
	}
	return xs
}

func isJSONNull(raw json.RawMessage) bool {
	return len(raw) == 0 || string(raw) == "null"
}

func summarize(form string) string {
	flat := strings.Join(strings.Fields(form), " ")
	if len(flat) > 120 {
		flat = flat[:117] + "..."
	}
	return strconv.Quote(flat)
}

// AwaitEval polls one form until pred accepts its value.
//
// This is this layer's only wait primitive, and it is a POLL rather than a
// subscription because Emacs state has no push channel out. It never sleeps
// to synchronize: the bound is a real deadline and the interval is the
// package's own pollInterval. A wedge short-circuits it, so a hung Emacs
// fails as a hang rather than as whatever the scenario happened to be
// waiting for.
func (e *Emacs) AwaitEval(what, form string, pred func(json.RawMessage) bool) json.RawMessage {
	e.t.Helper()
	return e.AwaitEvalFor(DefaultTimeout, what, form, pred)
}

// AwaitEvalFor is AwaitEval with an explicit bound. Per SPEC.md §B and the
// module AGENTS.md, a per-site bound is a NAMED constant with a stated
// reason, never an ad hoc duration written at the call site.
func (e *Emacs) AwaitEvalFor(bound time.Duration, what, form string, pred func(json.RawMessage) bool) json.RawMessage {
	e.t.Helper()
	// Every satisfied wait is RECORDED as a measured phase, for the same
	// reason `reportPhases` logs the boot phases on a passing run: a bound
	// in this layer must be a stated multiple of an OBSERVED healthy
	// maximum, and the only run that produces that observation is a run
	// that passed.
	started := time.Now()
	ctx, cancel := context.WithTimeout(context.Background(), bound)
	defer cancel()
	ticker := time.NewTicker(pollInterval)
	defer ticker.Stop()
	var lastValue json.RawMessage
	var lastErr error
	for {
		if e.isWedged() {
			e.t.Fatalf("await %s: emacs wedged: %s", what, e.wedgeCauseString())
		}
		if e.proc.Exited() {
			e.t.Fatalf("await %s: emacs exited; pty output:\n%s", what,
				tailBytes([]byte(e.proc.Output()), artifactTailBytes))
		}
		res, err := e.eval(form)
		switch {
		case err != nil:
			lastErr = err
		case !res.OK:
			lastErr = fmt.Errorf("elisp error: %s", res.Error)
		default:
			lastErr = nil
			lastValue = res.Value
			if pred(res.Value) {
				e.record("await "+what, time.Since(started))
				return res.Value
			}
		}
		select {
		case <-ctx.Done():
			if lastErr != nil {
				e.t.Fatalf("await %s: never satisfied within %s; last attempt failed: %v", what, bound, lastErr)
			}
			e.t.Fatalf("await %s: never satisfied within %s; last value was %s", what, bound, lastValue)
			return nil
		case <-ticker.C:
		}
	}
}

// AwaitTrue waits for a form to become non-nil, which covers most readbacks.
func (e *Emacs) AwaitTrue(what, form string) {
	e.t.Helper()
	e.AwaitEval(what, form, func(raw json.RawMessage) bool { return !isJSONNull(raw) })
}

// ---------------------------------------------------------------------------
// KEY SEQUENCES
// ---------------------------------------------------------------------------
//
// These exist because the layer boots real Doom: `map!` expands for real, so
// `keybindings.el`'s leader forms are live bindings and not compiled to a
// no-op. Under `-Q` there was nothing to press.
//
// TWO affordances, and the difference between them matters:
//
//   * Lookup (`BindingFor`) resolves a key sequence to the command it names,
//     WITHOUT running it. This is what a scenario uses when the command it
//     reaches would prompt -- `completing-read`, `y-or-n-p` -- because a
//     prompt inside `execute-kbd-macro` would block the command loop and
//     surface as a wedge rather than as an assertion.
//   * Press (`Keys`, `KeysIn`, `Leader`) actually runs the sequence through
//     `execute-kbd-macro`, which is the real keymap lookup plus the real
//     command, exactly as a user's keystroke does.
//
// The leader is `SPC` in evil NORMAL state, so every press enters normal
// state first: a composer buffer left in insert state would otherwise send
// a literal space, which is a wrong pass rather than a failure.

// evilNormalForm puts the current buffer into evil normal state when evil is
// loaded. `(evil +everywhere)` is in the sandbox profile, so it is; the
// guard is there so a profile change degrades to a plain Emacs press rather
// than to an unbound-function error.
const evilNormalForm = `(when (fboundp 'evil-normal-state) (evil-normal-state))`

// BindingFor returns the command a key sequence resolves to, as a string,
// without running it. An unbound sequence returns "nil", which is what elisp
// prints for it.
func (e *Emacs) BindingFor(keys string) string {
	e.t.Helper()
	return e.EvalString(fmt.Sprintf(`(progn %s (format "%%s" (key-binding (kbd %s) t)))`,
		evilNormalForm, elispString(keys)))
}

// BindingForIn is BindingFor resolved inside one buffer, which is how a
// binding defined on a mode map (the composer's, the roster's) is looked up.
func (e *Emacs) BindingForIn(buffer, keys string) string {
	e.t.Helper()
	return e.EvalString(fmt.Sprintf(`(with-current-buffer %s %s (format "%%s" (key-binding (kbd %s) t)))`,
		elispString(buffer), evilNormalForm, elispString(keys)))
}

// Keys presses one key sequence in the SELECTED window, through the real
// keymaps.
//
// A sequence that prompts will hang the command loop and be reported as a
// wedge; use BindingFor for those, or stub the prompt for the duration of
// the press the same way a scenario stubs it for a command.
func (e *Emacs) Keys(keys string) {
	e.t.Helper()
	e.Eval(keysForm(keys))
}

// keysForm is the form that presses one key sequence in normal state.
func keysForm(keys string) string {
	return fmt.Sprintf(`(progn %s (execute-kbd-macro (kbd %s)) t)`, evilNormalForm, elispString(keys))
}

// answeringYes wraps FORM so that, for its duration, a `y-or-n-p' whose
// prompt matches the regexp PROMPT is answered yes.
//
// IT ANSWERS ONE QUESTION, NOT ALL OF THEM. Any other prompt still reaches
// the harness's refusal (`agent-repl-e2e--refuse-prompt'), so a scenario
// that confirms a kill cannot also wave through a question nobody expected.
// The stub is a `cl-letf', which wins over the refusing advice for exactly
// the form's extent, as the settings file's commentary describes.
func answeringYes(prompt, form string) string {
	return fmt.Sprintf(`(cl-letf (((symbol-function 'y-or-n-p)
            (lambda (question &rest args)
              (if (string-match-p %s (format "%%s" question))
                  t
                (apply #'agent-repl-e2e--refuse-prompt question args)))))
  %s)`, elispString(prompt), form)
}

// killConfirmPrompt matches the question `agent-repl-kill-workspace' asks
// before it kills (lisp/verbs.el, "Kill workspace NAME? ").
const killConfirmPrompt = "\\`Kill workspace "

// LeaderAnsweringYes presses a leader sequence whose command asks a
// `y-or-n-p' matching PROMPT, and answers it yes (answeringYes).
func (e *Emacs) LeaderAnsweringYes(prompt, keys string) {
	e.t.Helper()
	e.Eval(answeringYes(prompt, keysForm("SPC "+keys)))
}

// killWorkspaceForm kills the workspace NAME, confirming the kill.
func killWorkspaceForm(name string) string {
	return answeringYes(killConfirmPrompt, `(progn (agent-repl-kill-workspace `+elispString(name)+`) t)`)
}

// KeysIn presses one key sequence in a named buffer, selecting its window
// first when it has one: `map!` bindings on a mode map only resolve in a
// buffer of that mode, and a command that acts on the selected window needs
// that window actually selected.
func (e *Emacs) KeysIn(buffer, keys string) {
	e.t.Helper()
	e.Eval(fmt.Sprintf(`(let ((buf (get-buffer %s)))
             (unless buf (error "no buffer named %%s" %s))
             (let ((win (get-buffer-window buf)))
               (if win (select-window win) (switch-to-buffer buf)))
             (with-current-buffer buf
               %s
               (execute-kbd-macro (kbd %s))
               t))`,
		elispString(buffer), elispString(buffer), evilNormalForm, elispString(keys)))
}

// Leader presses a leader sequence, i.e. the same one the module's own
// `map! :leader` forms and the module AGENTS.md write as `SPC TAB C-n`.
func (e *Emacs) Leader(keys string) {
	e.t.Helper()
	e.Keys("SPC " + keys)
}

// LeaderBinding resolves a leader sequence to its command without running
// it, for the leader bindings whose commands prompt.
func (e *Emacs) LeaderBinding(keys string) string {
	e.t.Helper()
	return e.BindingFor("SPC " + keys)
}

// EnsureDaemon has EMACS spawn the daemon, through the module's own
// launcher, and waits until Emacs has a live link to it.
//
// This is the layer's reason for existing. SPEC.md §G raised it as an open
// item — "an e2e harness that builds its own argv is a SECOND spelling of
// the launch contract and can drift from the one that ships" — and this
// settles it by inversion: nothing here derives an argv, because the
// launcher performs the launch.
func (e *Emacs) EnsureDaemon() {
	e.t.Helper()
	started := time.Now()
	e.Eval("(agent-repl-frontend-daemon-ensure)")
	e.AwaitEvalFor(daemonLinkBound, "emacs to hold a daemon link",
		"(and agent-repl-link--primary t)",
		func(raw json.RawMessage) bool { return !isJSONNull(raw) })
	e.record("daemon-link", time.Since(started))
}

// DaemonAddr is the address the daemon published, read from the same
// `daemon.addr` file Emacs read it from. A scenario that cross-checks a
// frame on the Go client dials this, so both clients agree on one address by
// construction rather than by assumption.
func (e *Emacs) DaemonAddr() string {
	e.t.Helper()
	body, err := os.ReadFile(filepath.Join(e.StateDir, "daemon.addr"))
	if err != nil {
		e.t.Fatalf("read the daemon address Emacs's launcher published: %v", err)
	}
	// The address is the first line; a "pid=<n>" line may follow it.
	return harness.AddrLine(string(body))
}

// ===========================================================================
// STRAY REAPING
// ===========================================================================

// THE LEAK THIS EXISTS FOR, MEASURED.
//
// After eight scenarios in one container, `ps` still showed a live shim
// (`node .../shim-dist/main.js --listen /tmp/emacs-e2e-*/...`, 95 MiB
// resident), two `shim-lock` holders and two `dbus-launch` daemons that no
// test owned any more. Over a full 45-scenario run the container's memory
// climbed monotonically from 0.5 GiB to 3.1 GiB, and that climb is what made
// a second concurrent sandbox OOM-kill the first on a 5.8 GiB VM.
//
// The cause is not in this harness and is not fixed here: Emacs is stopped
// through its own `agent-repl-frontend-daemon-stop`, per daemon.el's "EMACS
// NEVER KILLS A DAEMON", and a daemon that exits without reaping the shims it
// spawned -- or a WEDGED Emacs, which skips the polite stop entirely -- leaves
// them running. That belongs to the daemon.
//
// What belongs HERE is the scenario's own resource footprint: a test that
// leaves a 95 MiB process behind has not finished. So every stray is hunted
// down by the path it was started with, killed, and REPORTED -- the leak is
// bounded and visible, never bounded and silent.

// strayTermBound is how long a stray gets to honor SIGTERM before it is
// killed outright.
//
// MEASURED: every stray that honors SIGTERM at all exited within one poll of
// it (50ms); the ones that do not honor it never do, so waiting longer buys
// nothing. 500ms is ten times the observation.
//
// It was 2s, and that was too generous in a way that COSTS: this runs while
// the scenario still holds its parallelism slot, so every second spent
// waiting on a process that was never going to answer is a second no other
// scenario can start in. Nothing depends on a stray's graceful exit -- the
// test that owned it has already finished asserting.
const strayTermBound = 500 * time.Millisecond

// strayKillBound is how long a stray gets to disappear after SIGKILL.
//
// MEASURED: same observation, same poll. SIGKILL is not refusable, so
// anything still present after this is a process stuck in the kernel, which
// is reported rather than waited on -- and reporting it sooner is strictly
// better, for the same slot-holding reason.
const strayKillBound = 500 * time.Millisecond

// strayPollInterval is how often the reaper re-reads /proc while waiting.
const strayPollInterval = 50 * time.Millisecond

// reapStrays kills every process whose argv names one of this Emacs's own
// paths and is still alive after the test's own teardown.
//
// Matching is by ARGV, not by process tree: the daemon and the shims are
// deliberately detached from Emacs, so a tree walk would not find them, while
// every one of them carries this test's scratch paths on its command line
// (`--listen <root>/state/sock/...`, `<scratch>/locks/...`). The paths are
// unique per test -- `os.MkdirTemp` mints them -- so the match cannot reach a
// process belonging to another test, another suite, or the host.
func (e *Emacs) reapStrays() {
	strays := e.findStrays()
	if len(strays) == 0 {
		return
	}
	tried := e.teardownTried()
	for _, s := range strays {
		// A FAILURE, NOT A LOG LINE. Teardown is ordered and every step of
		// it is waited on, so a process still alive at this point is a
		// guarantee that did not hold -- and a scenario that ends owning a
		// live process of its own is precisely what produced this layer's
		// shared-socket failure and its 3 GiB peak. The reaping below still
		// happens: a failing scenario must not leak into the next one.
		e.reportFailure("emacs teardown: this scenario left a stray behind: pid %d %s (state %s, wchan %s); teardown had already tried: %s",
			s.pid, s.argv, s.state, s.wchan, tried)
		_ = syscall.Kill(s.pid, syscall.SIGTERM)
	}
	if left := e.awaitStraysGone(strayTermBound); len(left) > 0 {
		for _, s := range left {
			e.t.Logf("emacs teardown: pid %d ignored SIGTERM; killing it: %s", s.pid, s.argv)
			_ = syscall.Kill(s.pid, syscall.SIGKILL)
		}
		if stuck := e.awaitStraysGone(strayKillBound); len(stuck) > 0 {
			for _, s := range stuck {
				e.reportFailure("emacs teardown: pid %d survived SIGKILL and is still holding this scenario's resources: %s",
					s.pid, s.argv)
			}
		}
	}
}

// reportFailure fails the scenario for a teardown guarantee that did not
// hold.
func (e *Emacs) reportFailure(format string, args ...any) {
	if e.fail != nil {
		e.fail(format, args...)
		return
	}
	e.t.Errorf(format, args...)
}

// stray is one leaked process.
type stray struct {
	pid  int
	argv string
	// state and wchan are read from /proc for the DIAGNOSTIC use of this
	// walk rather than the reaping one: when Emacs stops answering its own
	// server socket, the only witness left is the kernel's. `D`/`S` in
	// `wait4` with a child of its own standing beside it is a different
	// failure from `R` in redisplay, and neither is visible from inside a
	// process that has stopped answering.
	state string
	wchan string
}

// awaitStraysGone polls until no stray remains or the bound expires,
// answering whatever is left.
func (e *Emacs) awaitStraysGone(bound time.Duration) []stray {
	return awaitGone(bound, e.findStrays)
}

// awaitGone polls one finder until it comes up empty or the bound expires,
// answering whatever is left. It is shared by the stray reaper and the Emacs
// exit wait so the two cannot drift in how they wait.
func awaitGone(bound time.Duration, find func() []stray) []stray {
	deadline := time.Now().Add(bound)
	for {
		left := find()
		if len(left) == 0 || time.Now().After(deadline) {
			return left
		}
		time.Sleep(strayPollInterval)
	}
}

// findStrays reads /proc and answers every live process whose argv OR
// ENVIRONMENT contains one of this Emacs's reap paths.
//
// /proc is read directly rather than shelling out to `ps`: the container's
// procps is not a dependency this layer wants on a teardown path, and the
// argv is exactly what /proc/<pid>/cmdline holds.
//
// THE ENVIRONMENT IS NOT A BELT-AND-BRACES SECOND CHANCE, it is the only
// thing that finds the largest stray of all. Emacs is started as plain
// `emacs` -- every path it works from travels in its environment -- so its
// argv names nothing, and a first revision of this reaper that matched only
// on argv left a 206 MiB Emacs resident after each scenario whose `script`
// parent it had killed. `HOME=<root>` in /proc/<pid>/environ is what
// identifies it, and the same read catches the daemon and anything else that
// took its paths from the environment rather than a flag.
func (e *Emacs) findStrays() []stray {
	paths := e.reapPaths()
	if len(paths) == 0 {
		return nil
	}
	entries, err := os.ReadDir("/proc")
	if err != nil {
		// Not fatal, and not silent: without /proc the reaper cannot run,
		// and a reader must know the footprint was not bounded.
		e.t.Logf("emacs teardown: cannot read /proc, so strays were not reaped: %v", err)
		return nil
	}
	self := os.Getpid()
	var out []stray
	for _, entry := range entries {
		pid, convErr := strconv.Atoi(entry.Name())
		if convErr != nil || pid == self {
			continue
		}
		// A PROCESS THE SCENARIO OWNS IS NEVER A STRAY, AND THE WAIT ABOVE IS
		// WHY THIS MATTERS RATHER THAN THE KILL BELOW.
		//
		// The reap key is this scenario's own root, and the scenario's own
		// infrastructure names it: the Xvfb's framebuffer directory is under
		// it, and the sidecar's `--state-dir`, `--config-roots` and `--log`
		// are all under it too. Each of those is started by the test, stopped
		// by the test, and asserted to have been alive at the end — so the
		// set this finder answers could NEVER come up empty, and
		// `awaitDaemonExit` therefore burned its whole 6s bound on every
		// scenario, including scenarios that never started a daemon at all.
		// MEASURED: `emacs phase daemon-exit took 6.04s (bound 6s)` on 7 of 7
		// sandbox observations, against a daemon that a host e2e measures
		// exiting 5ms after the same stop.
		//
		// The exemption list is `harness`'s, not a second one of this layer's:
		// the store and the sidecar already declare themselves there, and two
		// lists would drift the moment either layer gained a process.
		if harness.SparedFromStrayReaping(pid) {
			continue
		}
		raw, readErr := os.ReadFile(filepath.Join("/proc", entry.Name(), "cmdline"))
		if readErr != nil || len(raw) == 0 {
			// The process exited between the ReadDir and the read, or it is
			// a kernel thread. Neither is a stray.
			continue
		}
		argv := strings.TrimRight(strings.ReplaceAll(string(raw), "\x00", " "), " ")
		// An unreadable environ is not an error here: the process may have
		// exited, and a process owned by another uid is not this scenario's
		// to reap in any case.
		env, _ := os.ReadFile(filepath.Join("/proc", entry.Name(), "environ"))
		haystack := argv + "\x00" + string(env)
		for _, p := range paths {
			if strings.Contains(haystack, p) {
				out = append(out, stray{
					pid:   pid,
					argv:  argv,
					state: procField(entry.Name(), "stat"),
					wchan: procField(entry.Name(), "wchan"),
				})
				break
			}
		}
	}
	return out
}

// procField reads one small /proc file for a pid, answering "" when it cannot
// be read. `stat` is trimmed to the process state letter, which is the field
// this layer wants out of it.
func procField(pid, name string) string {
	raw, err := os.ReadFile(filepath.Join("/proc", pid, name))
	if err != nil {
		return ""
	}
	text := strings.TrimSpace(string(raw))
	if name != "stat" {
		return text
	}
	// The comm field is parenthesized and may itself contain spaces, so the
	// state letter is the first field AFTER the last ")".
	if idx := strings.LastIndex(text, ")"); idx >= 0 {
		fields := strings.Fields(text[idx+1:])
		if len(fields) > 0 {
			return fields[0]
		}
	}
	return ""
}

// processSnapshot answers what the kernel says about every process of this
// scenario, for a failure where Emacs itself can no longer be asked.
func (e *Emacs) processSnapshot() string {
	found := e.findStrays()
	if len(found) == 0 {
		return ""
	}
	var b strings.Builder
	b.WriteString("\n  this scenario's processes, as the kernel sees them:")
	for _, p := range found {
		b.WriteString(fmt.Sprintf("\n    pid=%d state=%s wchan=%s %s",
			p.pid, p.state, p.wchan, summarizeArgv(p.argv)))
	}
	return b.String()
}

// summarizeArgv keeps a process line readable.
func summarizeArgv(argv string) string {
	const limit = 120
	if len(argv) <= limit {
		return argv
	}
	return argv[:limit] + "..."
}

// reapPaths are the unique paths a stray of this scenario must name. They are
// read under the lock because NewEmacsWorld adds to them after StartEmacs has
// already registered the reaper.
func (e *Emacs) reapPaths() []string {
	e.reapMu.Lock()
	defer e.reapMu.Unlock()
	return append([]string(nil), e.reap...)
}

// AddReapPath declares one more path whose appearance in a process's argv
// marks that process as this scenario's to reap. `NewEmacsWorld` uses it for
// the world-level directories Emacs itself does not own -- the kernel-lock
// directory the shims claim in, and the fake SDK's spool root.
func (e *Emacs) AddReapPath(p string) {
	e.reapMu.Lock()
	defer e.reapMu.Unlock()
	e.reap = append(e.reap, p)
}

// ===========================================================================
// THE PARALLELISM BOUND
// ===========================================================================

// Every scenario in this layer is `t.Parallel()`, and it is isolated well
// enough to be: `requireSandbox` mints a fresh `os.MkdirTemp` scratch per
// test, so the Emacs HOME, its Doom tree, the state root, the two account
// roots, the store database, the kernel-lock directory and the fake SDK's
// spool root are all per-test paths; every socket is `shortSocketPath`, which
// carries four random bytes; and each Emacs draws on its OWN Xvfb, started
// with `-displayfd 1` so the X server picks a free display number under its
// own `/tmp/.X<n>-lock` rather than one this layer guesses. Nothing in the
// layer reads or writes a process-wide variable -- there is no `t.Setenv`
// anywhere in it -- and the per-run artifacts (the daemon, the shim bundle,
// the store and sidecar binaries, the staged webapp dist) are all built or
// checked under a `sync.Once`.
//
// WHAT IS NOT UNBOUNDED IS THE MACHINE. One scenario is a real Emacs with a
// GUI frame (206 MiB resident), an Xvfb, a daemon, a shim under Node
// (95 MiB), a store and a sidecar; the Docker VM this layer runs in has
// 4 CPUs and 5.8 GiB, and it is shared with whatever else is running on the
// host. So the layer bounds ITSELF rather than trusting `-parallel` to be
// passed correctly: a scenario takes a slot before it starts an Emacs and
// gives it back after its teardown, so a caller who runs `go test` with no
// flags at all still gets the measured degree of concurrency and no more.

// emacsParallelSlots is how many scenarios may hold an Emacs at once.
//
// MEASURED, on the 4-CPU/5.8 GiB Docker VM this layer runs in. See
// EMACS-LAYER-SPEC.md, "The parallelism bound, measured", for the run at each
// setting: wall time, peak container memory, worst Doom boot, and whether the
// pass set held.
//
// TWO, NOT THREE, AND THE DIFFERENCE IS NOT THE WALL CLOCK. Three slots is
// faster over one pass (81s against 107s) and its peak memory is fine
// (1.58 GiB). It is rejected because under SUSTAINED load -- a `-count=2` of
// the whole layer, which is how this bound has to be proven -- three Emacsen
// on four CPUs pushed Doom's boot past its own 3500ms bound and failed three
// scenarios that had nothing to do with each other. The bound is not the
// number that goes fastest on one lucky pass; it is the largest number whose
// worst measured boot still fits in the budget the product is held to. At two
// slots the worst Doom boot over 91 boots was 1360ms, 39% of that budget.
const emacsParallelSlots = 2

// emacsParallelEnv overrides the slot count. It exists so the bound above can
// be RE-MEASURED on a different machine the same way it was measured on this
// one -- `go test` at each setting, reading the wall and the peak -- and not
// so a caller can quietly turn the bound off: a value the machine cannot
// afford does not fail here, it OOM-kills an Emacs mid-scenario.
const emacsParallelEnv = "AGENT_REPL_E2E_EMACS_PARALLEL"

var (
	emacsSlotsOnce sync.Once
	emacsSlots     chan struct{}
)

// emacsSlotGate answers the semaphore, sized once per test binary.
func emacsSlotGate(t *testing.T) chan struct{} {
	emacsSlotsOnce.Do(func() {
		n := emacsParallelSlots
		if raw := os.Getenv(emacsParallelEnv); raw != "" {
			parsed, err := strconv.Atoi(raw)
			if err != nil || parsed < 1 {
				// A misspelled override must not silently fall back to the
				// default: the whole point of setting it is to measure a
				// specific number, and measuring the wrong one is worse
				// than not measuring.
				t.Fatalf("e2e: %s=%q is not a positive integer", emacsParallelEnv, raw)
			}
			n = parsed
		}
		emacsSlots = make(chan struct{}, n)
	})
	return emacsSlots
}

// emacsSlotHolders records which scenarios hold a slot, so StartEmacs can
// REFUSE to start an Emacs outside the gate rather than quietly exceed it.
var emacsSlotHolders sync.Map // *testing.T -> struct{}

// takeEmacsSlot blocks until this scenario may start an Emacs, and returns it
// on the test's way out.
func takeEmacsSlot(t *testing.T) {
	t.Helper()
	gate := emacsSlotGate(t)
	// Registered BEFORE the slot is taken, so t.Cleanup's LIFO unwind
	// returns it LAST -- after the stray reaper, after Emacs and its daemon
	// are gone. A slot handed back while this scenario's processes are still
	// resident would let the next one start against a budget that is not
	// actually free.
	t.Cleanup(func() {
		emacsSlotHolders.Delete(t)
		<-gate
	})
	gate <- struct{}{}
	emacsSlotHolders.Store(t, struct{}{})
}

// requireEmacsSlot fails the test unless it is inside the gate.
//
// The bound is only real if EVERY Emacs is started under it, and the one
// thing that could break that is a future scenario calling StartEmacs
// directly. This makes that a loud failure at the moment it happens rather
// than an OOM in some other test.
func requireEmacsSlot(t *testing.T) {
	t.Helper()
	if _, ok := emacsSlotHolders.Load(t); !ok {
		t.Fatalf("StartEmacs was called outside the parallelism gate: take a slot first "+
			"(NewEmacsWorld does). Starting an Emacs outside it means more than %d "+
			"concurrent editors on a machine measured to hold that many.", emacsParallelSlots)
	}
}

// recordEvalMax keeps the longest eval this Emacs has answered, so evalBound
// is derived from observation rather than guessed.
func (e *Emacs) recordEvalMax(took time.Duration) {
	for {
		prev := e.evalMax.Load()
		if int64(took) <= prev || e.evalMax.CompareAndSwap(prev, int64(took)) {
			return
		}
	}
}
