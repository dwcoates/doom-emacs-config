// Package login runs the vendor's login TUI on a daemon-owned pty, per
// account.
//
// The landed shape is: OpenLogin starts (or joins) the flow;
// WatchLoginTerminal is a SERVER stream of raw output with the scrollback
// replayed on attach; SendLoginInput is a unary verb carrying keystrokes or a
// resize; CloseLogin ends it, and closing an absent session is success. Every
// spawn is guarded by envc.VendorGuard — `claude /login` is a vendor call.
package login

import (
	"context"

	"github.com/creack/pty"

	"claude-repld/internal/envc"
	"claude-repld/internal/ids"
	"claude-repld/internal/notimpl"
)

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
	// then streaming live output until the pty closes or ctx is cancelled.
	Watch(ctx context.Context, ws ids.WorkspaceID) (<-chan Output, error)
	// SendKeystrokes writes raw bytes to the pty.
	SendKeystrokes(ctx context.Context, ws ids.WorkspaceID, data []byte) error
	// SendResize sets the pty's geometry.
	SendResize(ctx context.Context, ws ids.WorkspaceID, size Resize) error
	// Close ends the workspace's login session. Closing an absent one is
	// success.
	Close(ctx context.Context, ws ids.WorkspaceID) error
}

// ConfigDirFunc routes a workspace to the account config root its login flow
// runs under. It is injected rather than imported so login stays a leaf
// alongside account rather than depending on it.
type ConfigDirFunc func(ws ids.WorkspaceID) (string, error)

// New builds the manager. guard refuses the pty spawn when vendor calls are
// forbidden; vendorBin is the vendor binary the pty runs (`claude /login`);
// configDirFor routes each workspace to its account root.
func New(guard envc.VendorGuard, vendorBin string, configDirFor ConfigDirFunc) (Manager, error) {
	return nil, notimpl.Err
}
