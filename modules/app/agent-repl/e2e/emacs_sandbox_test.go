package e2e

import (
	"context"
	"testing"
)

// THE SANDBOX SEAM.
//
// The Emacs client layer runs Emacs inside a container sandbox, because it
// starts a REAL editor that spawns a REAL daemon: nothing it does may reach
// the host's own Emacs, `~/.claude`, `~/.emacs.d` or `~/.config`, and
// nothing it writes may land outside a scratch directory that is swept on
// the way out.
//
// `modules/app/agent-repl/e2e/sandbox/` is another agent's work and DOES NOT
// EXIST YET. This file is the interface EMACS-LAYER-SPEC.md declares against
// it, plus a stub whose Available() is always false — so every scenario in
// this layer skips LOUDLY, naming the missing package, rather than silently
// passing or quietly reaching the host.
//
// When `sandbox/` lands, THIS FILE is the only thing that changes: the stub
// is deleted and `newSandbox` returns the real implementation. Nothing in
// `emacs_test.go` or the scenarios refers to the stub.

// sandbox is the container the Emacs client layer runs inside.
//
// Every method is expected to fail the test loudly rather than return an
// error for conditions a caller cannot do anything about; only Exec returns
// an error, because a non-zero emacsclient exit is ordinary information a
// wait loop acts on.
type sandbox interface {
	// Available reports whether a container-sandboxed Emacs can run here,
	// and why not when it cannot. The reason is used VERBATIM in the skip
	// message, so it must name the thing that is missing.
	Available() (ok bool, reason string)

	// HasEmacs reports whether the image carries an Emacs new enough for
	// this module — `tab-bar-tabs` and `--init-directory` are both required,
	// so 27 or later — and the version string it found either way.
	HasEmacs() (ok bool, version string)

	// Scratch is a container-absolute, writable directory, unique per test
	// and swept on cleanup. EVERYTHING this layer writes lives under it: the
	// Emacs init directory, the server socket, the daemon state root, the
	// store database and socket, the sidecar spool root, and the eval
	// request/response files.
	Scratch() string

	// Exec runs one short command inside the sandbox and returns its
	// combined output. This is how `emacsclient` is invoked, so it is on the
	// hot path of every readback and every heartbeat probe.
	Exec(ctx context.Context, argv ...string) (string, error)

	// StartPTY launches a long-lived process inside the sandbox attached to
	// a pty. Emacs needs one to have a real tty frame, which is what makes
	// `window-list`, `tab-bar-tabs` and `mode-line-format` behave the way
	// they do for a user.
	StartPTY(ctx context.Context, argv ...string) (sandboxProc, error)
}

// sandboxProc is a process running inside the sandbox.
type sandboxProc interface {
	// Kill terminates the process and reaps it. Idempotent: the Emacs
	// teardown path calls it unconditionally after asking Emacs to exit
	// politely, precisely so a WEDGED Emacs cannot leak its daemon.
	Kill()

	// Exited reports whether the process is already gone, so a scenario can
	// fail loudly when Emacs died before its own cleanup ran instead of
	// reporting the timeout that death causes downstream.
	Exited() bool

	// Output returns whatever the pty has produced so far, for failure
	// artifacts.
	Output() string
}

// requireSandbox returns the sandbox for one test, or skips loudly.
//
// The skip is deliberately specific: a reader of a skipped run must be able
// to tell "the container work has not landed" apart from "this host cannot
// run containers", because those have completely different answers.
func requireSandbox(t *testing.T) sandbox {
	t.Helper()
	s := newSandbox(t)
	ok, reason := s.Available()
	if !ok {
		t.Skipf("emacs client layer needs the e2e sandbox: %s", reason)
	}
	ok, version := s.HasEmacs()
	if !ok {
		t.Skipf("emacs client layer needs Emacs 27 or later in the sandbox image; found %q", version)
	}
	return s
}

// newSandbox is the one construction point. It returns the stub until
// `modules/app/agent-repl/e2e/sandbox/` lands.
func newSandbox(t *testing.T) sandbox {
	t.Helper()
	return stubSandbox{}
}

// stubSandbox stands in for the unimplemented sandbox package.
//
// It is NOT a fake that pretends to work: every method that could be
// mistaken for a working sandbox fails the test outright, so a scenario that
// somehow gets past requireSandbox cannot run against the host by accident.
type stubSandbox struct{}

const stubSandboxReason = "modules/app/agent-repl/e2e/sandbox is not implemented yet " +
	"(see EMACS-LAYER-SPEC.md, \"The sandbox dependency\", for the interface it must provide)"

func (stubSandbox) Available() (bool, string) { return false, stubSandboxReason }

func (stubSandbox) HasEmacs() (bool, string) { return false, "unknown: " + stubSandboxReason }

func (stubSandbox) Scratch() string {
	panic("e2e: the stub sandbox has no scratch directory — " + stubSandboxReason)
}

func (stubSandbox) Exec(context.Context, ...string) (string, error) {
	panic("e2e: the stub sandbox cannot exec — " + stubSandboxReason)
}

func (stubSandbox) StartPTY(context.Context, ...string) (sandboxProc, error) {
	panic("e2e: the stub sandbox cannot start a process — " + stubSandboxReason)
}
