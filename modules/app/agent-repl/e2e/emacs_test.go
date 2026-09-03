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
		StateDir:           filepath.Join(root, "state"),
		DefaultConfigDir:   filepath.Join(root, "account-default"),
		MultiRepoConfigDir: filepath.Join(root, "account-multi"),
		MultiRepoRoot:      filepath.Join(root, "multi-repo"),
		wedged:             make(chan struct{}),
	}

	e.writeBootstrap(opts)

	env := append([]string{
		"HOME=" + root,
		"AGENT_REPL_STATE_DIR=" + e.StateDir,
		"AGENT_REPL_FORBID_VENDOR_CALLS=1",
		"MULTI_REPO_ROOT=" + e.MultiRepoRoot,
		"TERM=dumb",
	}, opts.ExtraEnv...)
	if opts.StoreSocket != "" {
		env = append(env, "AGENT_REPL_STORE_SOCKET="+opts.StoreSocket)
	}

	// `-Q` reads no init file, no site file and no package directory, so the
	// module's sources are the only elisp loaded and there is nothing for an
	// --init-directory to point at. That also keeps this off Emacs 29+: the
	// image is Debian bookworm's emacs-nox, which is 28.2.
	argv := append(envPrefix(env),
		"emacs", "-nw", "-Q",
		"-l", e.bootstrapPath(),
	)

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

	e.awaitServer()
	e.armHeartbeat()
	return e
}

// bootstrapPath is the elisp this layer loads into Emacs. It is the ONLY
// elisp the layer adds: it loads the module's real sources in `config.el`'s
// own order and then starts the server.
func (e *Emacs) bootstrapPath() string { return filepath.Join(e.Root, "bootstrap.el") }

func (e *Emacs) writeBootstrap(opts EmacsOpts) {
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

	moduleRoot := repo.repoDir

	// The readback helper. It reads a form from a file and writes the result
	// as JSON to another file, so nothing has to survive shell quoting or
	// elisp print escaping, and an elisp error becomes a NAMED Go failure
	// rather than an opaque emacsclient exit status.
	src := fmt.Sprintf(`;;; bootstrap.el --- e2e emacs client layer -*- lexical-binding: t; -*-
;;; Commentary:
;; Written by the Go e2e harness.  It loads the module's REAL sources and
;; starts the server; it adds no behavior of its own beyond one readback
;; helper.  See e2e/EMACS-LAYER-SPEC.md.
;;; Code:

(require 'json)
(require 'server)

(setq server-name %q)
(setq inhibit-startup-screen t)

;; Point the launcher's own defcustoms at the sandbox.  The launcher still
;; composes argv and environment; this only tells it where things are.
(defvar agent-repl-e2e--daemon-binary %q)
(defvar agent-repl-e2e--build-script %q)
(defvar agent-repl-e2e--module-root %q)

(add-to-list 'load-path agent-repl-e2e--module-root)

(defun agent-repl-e2e--eval (in out)
  "Evaluate the form in file IN and write a JSON result to file OUT.
Returns t.  The result is an object with an \"ok\" boolean plus either a
\"value\" or an \"error\", so an elisp failure reaches the Go side as
itself rather than as a timeout somewhere downstream."
  (let ((payload
         (condition-case err
             (let ((value (eval (car (read-from-string
                                      (with-temp-buffer
                                        (insert-file-contents in)
                                        (buffer-string))))
                                t)))
               (list (cons "ok" t) (cons "value" value)))
           (error (list (cons "ok" :json-false)
                        (cons "error" (error-message-string err)))))))
    (with-temp-file out
      (insert (json-encode payload))))
  t)

;; Cold start is NOT armed here: this layer drives
;; agent-repl-frontend-daemon-ensure explicitly, so the spawn happens at a
;; moment a scenario can observe from its first instant.
(setq agent-repl-frontend-auto-start nil)

(load (expand-file-name "config.el" agent-repl-e2e--module-root) nil t)

(setq agent-repl-daemon-command (list agent-repl-e2e--daemon-binary)
      agent-repl-daemon-build-script agent-repl-e2e--build-script
      agent-repl-daemon-default-config-dir %q
      agent-repl-daemon-multi-repo-config-dir %q
      agent-repl-daemon-multi-repo-root %q)

;; A real frame with a tab-bar: this layer asserts window and tab state, and
;; both need the modes actually on.
(tab-bar-mode 1)

(server-start)
(provide 'agent-repl-e2e-bootstrap)
;;; bootstrap.el ends here
`,
		e.ServerSocket,
		opts.DaemonBinary,
		buildScript,
		moduleRoot,
		e.DefaultConfigDir,
		e.MultiRepoConfigDir,
		e.MultiRepoRoot,
	)

	if err := os.WriteFile(e.bootstrapPath(), []byte(src), 0o644); err != nil {
		e.t.Fatalf("write bootstrap.el: %v", err)
	}
}

// envPrefix renders an environment as an `env` command prefix, which is how
// variables reach a process the sandbox starts.
func envPrefix(env []string) []string {
	return append([]string{"env"}, env...)
}

// awaitServer waits for the server socket to answer, which is the first
// moment the module's sources are fully loaded.
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
