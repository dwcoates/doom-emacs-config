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

// HeartbeatBound is how long one `emacsclient --eval '(emacs-pid)'` probe
// may take before the running test fails.
//
// PROVISIONAL. EMACS-LAYER-SPEC.md requires this be replaced with a measured
// value — a small multiple of the observed healthy maximum — before the
// scenario list beyond the proof-of-life test is implemented, per the module
// AGENTS.md rule that bounds are measured, never guessed. It is deliberately
// TIGHT rather than generous: the failure it exists to catch is a hang, and
// a hang that clears itself is still the recursion defect.
const HeartbeatBound = 2 * time.Second

// heartbeatInterval is how often the probe runs. It rides the SAME server
// socket every scenario uses, so it queues behind whatever Emacs is doing
// and therefore measures the command loop's real responsiveness rather than
// merely whether the process is alive.
const heartbeatInterval = 250 * time.Millisecond

// emacsBootBound is how long Emacs may take to create its server socket and
// finish loading the module's sources.
//
// PROVISIONAL for the same reason as HeartbeatBound. It is a multiple of the
// default bound rather than a new number because booting Emacs chains a
// process spawn onto loading roughly fifty elisp sources — structurally two
// startup events, the same shape HandoverChainTimeout documents.
const emacsBootBound = HandoverChainTimeout

// doomBootBound is how long Emacs may take to finish Doom's own
// initialization and publish the readiness stamp.
//
// PROVISIONAL, like the two above, and for one extra reason: nobody has ever
// timed a Doom boot in this image, because no container has ever run. It is
// TWICE emacsBootBound because a Doom boot is emacsBootBound's work plus
// Doom's core, its enabled modules and their `config.el`s -- strictly more
// than loading the module alone, and the only honest thing to say about the
// difference until it is measured is that it is a small multiple.
const doomBootBound = 2 * emacsBootBound

// doomStageBound bounds each `cp -a` that stages one entry of Doom's
// `.local` tree into the test's scratch.
//
// PROVISIONAL. The tree's size is unmeasured (see stageEmacsDir), and the
// copy is a tmpfs-to-tmpfs one within one container, so this is generous by
// design: the failure it exists to catch is a copy that cannot finish at
// all, not a slow one.
const doomStageBound = 60 * time.Second

// Emacs is one sandboxed Emacs process, its server socket, and its
// heartbeat.
type Emacs struct {
	t   *testing.T
	box sandbox

	// Root is the scratch subtree this Emacs owns. Every path below is
	// under it, so a swept scratch leaves nothing behind.
	Root string
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

	root := filepath.Join(box.Scratch(), "emacs")
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

	e.writeSettings(opts)
	e.stageEmacsDir()

	// HOME is the per-test scratch root, and `~/.emacs.d` under it is the
	// staged Doom. Emacs 28 has no `--init-directory` (that landed in 29),
	// so HOME is the ONLY way to point an Emacs at a different init tree --
	// which is also why the staging exists rather than a flag.
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
		"TERM=dumb",
	}, opts.ExtraEnv...)
	if opts.StoreSocket != "" {
		env = append(env, "AGENT_REPL_STORE_SOCKET="+opts.StoreSocket)
	}

	// No `-Q` and no `-l`: the image's Doom profile is the init path, and
	// `sandbox/doom/init.el` picks the settings file up itself. `-nw` is a
	// tty frame, which is what makes `window-list`, `tab-bar-tabs` and
	// `mode-line-format` behave as they do for a user.
	argv := append(envPrefix(env), "emacs", "-nw")

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

	e.awaitDoom()
	e.awaitServer()
	e.armHeartbeat()
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

(setq agent-repl-daemon-command (list %q)
      agent-repl-daemon-build-script %q
      agent-repl-daemon-default-config-dir %q
      agent-repl-daemon-multi-repo-config-dir %q
      agent-repl-daemon-multi-repo-root %q)

(provide 'agent-repl-e2e-settings)
;;; e2e-settings.el ends here
`,
		opts.DaemonBinary,
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
// Two constraints collide here, and the staging is what resolves them:
//   - Emacs 28.2 has no `--init-directory`, so HOME is the only way to aim
//     an Emacs at an init tree, and HOME must be per-test.
//   - the container runs `--read-only`, and `/sandbox/emacs.d` is NOT one of
//     its tmpfs mounts, so Doom's own local tree cannot be written in place.
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
	if err := os.MkdirAll(e.EmacsDir, 0o755); err != nil {
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
			e.t.Fatalf("Doom did not finish initializing within %s (no readiness stamp at %s); pty output:\n%s",
				doomBootBound, e.ReadyStamp, e.proc.Output())
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

// armHeartbeat starts the wedge detector. It runs for the WHOLE life of the
// process, teardown included, because the recursion defect this exists to
// catch fired during close and kill.
func (e *Emacs) armHeartbeat() {
	ctx, cancel := context.WithCancel(context.Background())
	e.stopHeartbeat = cancel
	go func() {
		ticker := time.NewTicker(heartbeatInterval)
		defer ticker.Stop()
		for {
			select {
			case <-ctx.Done():
				return
			case <-ticker.C:
			}
			probe, probeCancel := context.WithTimeout(ctx, HeartbeatBound)
			_, err := e.box.Exec(probe, "emacsclient", "--socket-name", e.ServerSocket, "--eval", "(emacs-pid)")
			probeCancel()
			if err == nil {
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

// declareWedged records the wedge once and fails the test.
//
// It never retries. A hang that clears itself is still the sentinel/
// kill-buffer recursion, and treating it as transient is how that defect
// stayed alive long enough to reach a user.
func (e *Emacs) declareWedged(cause string) {
	e.wedgeOnce.Do(func() {
		e.wedgeCause.Store(&cause)
		close(e.wedged)
		e.t.Errorf("EMACS WEDGED: %s", cause)
		e.dumpArtifacts()
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
}

// stop tears Emacs down. Registered via t.Cleanup, so it runs before the
// sidecar's and the store's cleanups, which were registered earlier.
func (e *Emacs) stop() {
	if e.stopHeartbeat != nil {
		e.stopHeartbeat()
	}
	if e.proc.Exited() {
		return
	}

	// Ask the DAEMON to exit through Emacs's own command: per daemon.el,
	// "EMACS NEVER KILLS A DAEMON". A wedged Emacs cannot honor this, which
	// is exactly why the kill below is unconditional.
	if !e.isWedged() {
		ctx, cancel := context.WithTimeout(context.Background(), DefaultTimeout)
		_, _ = e.box.Exec(ctx, "emacsclient", "--socket-name", e.ServerSocket,
			"--eval", "(ignore-errors (agent-repl-frontend-daemon-stop))")
		cancel()

		ctx, cancel = context.WithTimeout(context.Background(), DefaultTimeout)
		_, _ = e.box.Exec(ctx, "emacsclient", "--socket-name", e.ServerSocket,
			"--eval", "(kill-emacs)")
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

	ctx, cancel := context.WithTimeout(context.Background(), HeartbeatBound)
	defer cancel()
	call := fmt.Sprintf("(agent-repl-e2e--eval %q %q)", in, out)
	if _, err := e.box.Exec(ctx, "emacsclient", "--socket-name", e.ServerSocket, "--eval", call); err != nil {
		return zero, fmt.Errorf("emacsclient: %w", err)
	}

	body, err := os.ReadFile(out)
	if err != nil {
		return zero, fmt.Errorf("read the eval response: %w", err)
	}
	var res evalResult
	if err := json.Unmarshal(body, &res); err != nil {
		return zero, fmt.Errorf("decode the eval response %q: %w", string(body), err)
	}
	return res, nil
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
	e.Eval("(agent-repl-frontend-daemon-ensure)")
	e.AwaitEvalFor(emacsBootBound, "emacs to hold a daemon link",
		"(and agent-repl-link--primary t)",
		func(raw json.RawMessage) bool { return !isJSONNull(raw) })
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
