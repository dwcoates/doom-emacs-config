// Package login runs the vendor's login TUI on a daemon-owned pty, per
// account.
//
// The landed shape is: OpenLogin starts (or joins) the flow;
// WatchLoginTerminal is a SERVER stream of raw output with the scrollback
// replayed on attach; SendLoginInput is a unary verb carrying keystrokes or a
// resize; CloseLogin ends it, and closing an absent session is success. Every
// spawn is guarded by envc.VendorGuard — `claude /login` is a vendor call.
//
// # WHY THE DAEMON, AND NOT EMACS
//
// The login is a full-screen TUI gated behind stateful prompts (theme
// onboarding on a fresh config root, folder trust on an untrusted cwd) before
// it ever reaches the OAuth step. Nothing can reliably SCRAPE that. This
// package keeps the human in the loop and moves the terminal instead: the
// daemon owns the pty, the client renders the raw bytes, and no code anywhere
// parses the TUI.
//
// # WHY ONE LOGIN PER ACCOUNT ROOT
//
// A login is scoped to the account it logs INTO, never to the workspace that
// asked for it. Two workspaces on the same account must land on the same
// terminal, or a second click would race a second OAuth flow against the
// first — which is exactly what "the open is per-account idempotent" means in
// endpoint_open_login.proto. Two DIFFERENT account roots are genuinely
// independent and run concurrently.
package login

import (
	"context"
	"errors"

	"github.com/creack/pty"

	"claude-repld/internal/dlog"
	"claude-repld/internal/envc"
	"claude-repld/internal/ids"
)

// EnvClaudeBin names the vendor binary the login pty runs. AGENTS.md pins it
// as a test knob: a test points it at a fake script, and an explicit path is
// what makes the spawn legal under AGENT_REPL_FORBID_VENDOR_CALLS.
const EnvClaudeBin = "AGENT_REPL_CLAUDE_BIN"

// DefaultVendorBin is the vendor binary a login runs when nothing names one.
// It is ALSO the only spelling the vendor guard refuses: an explicit path is
// by definition not a call to the real CLI on PATH.
const DefaultVendorBin = "claude"

// LoginArg is the vendor subcommand that drops straight into the OAuth flow,
// rather than landing in a plain session the user would have to drive by hand.
const LoginArg = "/login"

// Output is one frame of a login terminal's server stream. Exactly one field
// is set.
type Output struct {
	// Bytes is raw terminal output, drawn verbatim.
	Bytes []byte
	// Closed reports that the pty ended; it is the stream's last frame.
	Closed bool
}

// Resize is a terminal geometry change, as SendLoginInput carries it.
type Resize struct {
	Rows int32
	Cols int32
}

// Winsize renders the resize in the form the pty takes. It exists so the wire
// shape is converted in exactly one place.
func (r Resize) Winsize() *pty.Winsize {
	return &pty.Winsize{Rows: uint16(r.Rows), Cols: uint16(r.Cols)}
}

// Manager owns the fleet of login ptys, one per account config dir.
type Manager interface {
	// Open starts the login flow for the workspace's account, or joins the one
	// already running for that config dir. It answers the config dir the flow
	// runs under, which is what OpenLoginSuccess carries.
	Open(ctx context.Context, ws ids.WorkspaceID) (configDir string, err error)
	// Watch attaches to the workspace's login terminal, REPLAYING the
	// scrollback first so a late or re-attaching client sees the whole screen,
	// then streaming live output until the pty closes or ctx is cancelled. The
	// channel's last frame is Output{Closed: true} when the child exited; the
	// channel is closed after it.
	Watch(ctx context.Context, ws ids.WorkspaceID) (<-chan Output, error)
	// SendKeystrokes writes raw bytes to the pty.
	SendKeystrokes(ctx context.Context, ws ids.WorkspaceID, data []byte) error
	// SendResize sets the pty's geometry.
	SendResize(ctx context.Context, ws ids.WorkspaceID, size Resize) error
	// Close ends the workspace's login session. Closing an absent one is
	// success.
	Close(ctx context.Context, ws ids.WorkspaceID) error
	// CloseAll ends every running login. The daemon calls it at shutdown; a
	// login pty is never worth surviving the process that owns it.
	CloseAll(ctx context.Context)
}

// Observer is told when an account root's login flow opens and when it ends,
// in that order and once each per flow. A flow JOINED by a second workspace is
// not a second opening. internal/agentreplsession reads the root's login record
// at both, which is how a login made THROUGH agent-repl is known to have
// completed without anything parsing the TUI.
//
// Both are called under the manager's lock, which is what orders an ending
// after its opening; neither may call back into the manager.
type Observer interface {
	LoginOpened(configDir string)
	LoginEnded(configDir string)
}

// ConfigDirFunc routes a workspace to the account config root its login flow
// runs under. It is injected rather than imported so login stays a leaf
// alongside account rather than depending on it.
type ConfigDirFunc func(ws ids.WorkspaceID) (string, error)

// New builds the manager. guard refuses the pty spawn when vendor calls are
// forbidden AND the binary is the default `claude`; vendorBin is the vendor
// binary the pty runs (empty takes $AGENT_REPL_CLAUDE_BIN, then
// DefaultVendorBin); configDirFor routes each workspace to its account root;
// observer is told of every flow's opening and ending.
func New(guard envc.VendorGuard, vendorBin string, configDirFor ConfigDirFunc, log dlog.Logger, observer Observer) (Manager, error) {
	if configDirFor == nil {
		return nil, errors.New("login: a config-dir resolver is required (the account root is what a login is keyed by)")
	}
	if log == nil {
		return nil, errors.New("login: a logger is required")
	}
	if observer == nil {
		return nil, errors.New("login: an observer is required (a completed login begins agent-repl's session)")
	}
	return newManager(guard, vendorBin, configDirFor, log, observer), nil
}
