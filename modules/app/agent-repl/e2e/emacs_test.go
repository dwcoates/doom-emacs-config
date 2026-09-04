package e2e

import (
	"context"
	"encoding/json"
	"fmt"
	"os"
	"path/filepath"
	"strconv"
	"strings"
	"sync"
	"sync/atomic"
	"syscall"
	"testing"
	"time"
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
// hands the `UpdateShutdownSchedule{now}` to a curl child and returns `t` the
// same instant. The teardown's very next act was `(kill-emacs)`, which takes
// that child down with it — so the request the daemon never received could not
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
		"AGENT_REPL_E2E_EMACS=1",
		"AGENT_REPL_E2E_SERVER=" + e.ServerSocket,
		"AGENT_REPL_E2E_READY=" + e.ReadyStamp,
		"AGENT_REPL_E2E_SETTINGS=" + e.settingsPath(),
		"AGENT_REPL_STATE_DIR=" + e.StateDir,
		"AGENT_REPL_FORBID_VENDOR_CALLS=1",
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

(setq agent-repl-daemon-command (list %s)
      agent-repl-daemon-build-script %q
      agent-repl-daemon-default-config-dir %q
      agent-repl-daemon-multi-repo-config-dir %q
      agent-repl-daemon-multi-repo-root %q)

(provide 'agent-repl-e2e-settings)
;;; e2e-settings.el ends here
`,
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
				e.t.Fatalf("Doom failed to initialize: %s; pty output:\n%s", ready.Error, e.proc.Output())
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
			e.t.Fatalf("Doom did not finish initializing within %s (no readiness stamp at %s)%s%s; pty output:\n%s",
				doomBootBound, e.ReadyStamp, e.bootBreadcrumb(), e.processSnapshot(), e.proc.Output())
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
		e.t.Errorf("EMACS WEDGED: %s%s", cause, e.processSnapshot())
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
	out := filepath.Join(dir, artifactDirName(e.t.Name()))
	if err := os.MkdirAll(out, 0o755); err != nil {
		e.t.Logf("preserve emacs artifacts under %s: %v", out, err)
		return
	}
	path := filepath.Join(out, "emacs.pty.log")
	if err := os.WriteFile(path, []byte(e.proc.Output()), 0o644); err != nil {
		e.t.Logf("write %s: %v", path, err)
		return
	}
	e.t.Logf("emacs pty output preserved at %s", path)
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
	if !e.isWedged() {
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
		// answers its callback with nil on a REFUSAL as well as on a transport
		// failure, and a refused stop is exactly the case whose leaked tree the
		// reaper below then reports with no cause attached.
		if outcome := strings.TrimSpace(out); !strings.Contains(outcome, ":accepted t") {
			e.t.Logf("emacs's daemon stop was not accepted: %s", outcome)
		}

		ctx, cancel = context.WithTimeout(context.Background(), DefaultTimeout)
		// `kill-emacs' never answers by design -- Emacs exits with the client
		// still waiting -- so only a TIMEOUT is worth reporting: it means
		// Emacs neither answered nor died, and the reaper is about to find it.
		_, err = e.box.Exec(ctx, "emacsclient", "--socket-name", e.ServerSocket,
			"--eval", "(kill-emacs)")
		if err != nil && ctx.Err() != nil {
			e.t.Logf("emacs did not act on (kill-emacs) within %s: %v", DefaultTimeout, err)
		}
		cancel()
	}
	e.proc.Kill()
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
		return zero, fmt.Errorf("emacsclient: %w%s%s%s", err, snapshot, e.stackWhenStuck(), e.profileWhenStuck())
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
	e.Eval(fmt.Sprintf(`(progn %s (execute-kbd-macro (kbd %s)) t)`, evilNormalForm, elispString(keys)))
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
	return strings.TrimSpace(string(body))
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
	for _, s := range strays {
		e.t.Logf("emacs teardown: reaping a stray this scenario left behind: pid %d %s", s.pid, s.argv)
		_ = syscall.Kill(s.pid, syscall.SIGTERM)
	}
	if left := e.awaitStraysGone(strayTermBound); len(left) > 0 {
		for _, s := range left {
			e.t.Logf("emacs teardown: pid %d ignored SIGTERM; killing it: %s", s.pid, s.argv)
			_ = syscall.Kill(s.pid, syscall.SIGKILL)
		}
		if stuck := e.awaitStraysGone(strayKillBound); len(stuck) > 0 {
			for _, s := range stuck {
				e.t.Errorf("emacs teardown: pid %d survived SIGKILL and is still holding this scenario's resources: %s",
					s.pid, s.argv)
			}
		}
	}
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
	deadline := time.Now().Add(bound)
	for {
		left := e.findStrays()
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
