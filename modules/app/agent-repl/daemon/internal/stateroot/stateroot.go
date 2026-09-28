// Package stateroot resolves $AGENT_REPL_STATE_DIR and names every path
// beneath it.
//
// The layout is fixed by ARCHITECTURE.md "State root layout"; nothing else in
// the daemon joins a path under the state root by hand.
package stateroot

import (
	"fmt"
	"os"
	"path/filepath"
	"strings"
)

// DefaultDirName is the state root's default location under the home
// directory when neither the flag nor the environment names one.
const DefaultDirName = ".claude-emacs"

// maxSocketPathLen is the longest usable unix-domain socket path. macOS's
// sockaddr_un.sun_path is 104 bytes including the terminating NUL, so 103
// bytes of path is the budget. Linux is more generous; the daemon holds
// itself to the tighter limit so the same state root works on both.
const maxSocketPathLen = 103

// socketNameBudget is the longest per-workspace socket file name the layout
// must accommodate: a 16-character workspace id plus the ".sock" suffix. The
// 16 is wsm.IDLength, which stateroot cannot import (wsm sits above it); the
// two constants are documented on each other and move together or not at all.
const socketNameBudget = 16 + len(".sock")

// Layout names every path under one state root. It is a value; construct it
// with Root and pass it down.
type Layout struct {
	dir string
}

// Root resolves the state root. Precedence is the -state-dir flag (passed as
// override), then $AGENT_REPL_STATE_DIR (passed as fromEnv), then
// $HOME/.claude-emacs. The result is absolute and cleaned; resolving it is an
// error the caller surfaces rather than defaults away.
func Root(override, fromEnv string) (Layout, error) {
	dir := override
	if dir == "" {
		dir = fromEnv
	}
	if dir == "" {
		home, err := os.UserHomeDir()
		if err != nil {
			return Layout{}, fmt.Errorf("resolve home directory for the default state root: %w", err)
		}
		dir = filepath.Join(home, DefaultDirName)
	}
	abs, err := filepath.Abs(dir)
	if err != nil {
		return Layout{}, fmt.Errorf("resolve state root %q: %w", dir, err)
	}
	return Layout{dir: filepath.Clean(abs)}, nil
}

// Dir is the state root itself.
func (l Layout) Dir() string { return l.dir }

// DaemonAddr is the file carrying "127.0.0.1:<port>\n": written atomically
// once the listener is bound, removed on orderly exit.
func (l Layout) DaemonAddr() string { return filepath.Join(l.dir, "daemon.addr") }

// DB is the workspace-state-manager database. The old state.db is abandoned
// in place and never opened.
func (l Layout) DB() string { return filepath.Join(l.dir, "wsm.db") }

// LogsDir holds the size-rotated run log and its retained generations.
func (l Layout) LogsDir() string { return filepath.Join(l.dir, "logs") }

// RunLog is the size-rotated daemon run log, shared across process restarts.
func (l Layout) RunLog() string { return filepath.Join(l.LogsDir(), "daemon.run.log") }

// SockDir holds the per-workspace shim unix-domain sockets.
func (l Layout) SockDir() string { return filepath.Join(l.dir, "sock") }

// ShimSocket is one workspace's shim socket path.
func (l Layout) ShimSocket(workspaceID string) string {
	return filepath.Join(l.SockDir(), workspaceID+".sock")
}

// IntentDir holds the stand-down intent manifest.
func (l Layout) IntentDir() string { return filepath.Join(l.dir, "intent") }

// IntentManifest is the stand-down intent manifest: pid plus intent per
// session, written by the outgoing daemon, reconciled by the incoming one.
func (l Layout) IntentManifest() string { return filepath.Join(l.IntentDir(), "manifest.json") }

// OutputDir holds the command-file ingress.
func (l Layout) OutputDir() string { return filepath.Join(l.dir, "output") }

// CommandFileGlob matches every command file in the ingress directory.
func (l Layout) CommandFileGlob() string {
	return filepath.Join(l.OutputDir(), "workspace_commands_*.json")
}

// HeldPromptDir holds the held-prompt ingress: one file per prompt a client
// could not hand to a live daemon, ingested into the prompt queue once one
// serves (internal/heldingress).
func (l Layout) HeldPromptDir() string { return filepath.Join(l.dir, "held-prompts") }

// CheckSocketPathBudget reports whether SockDir plus the longest per-workspace
// socket name still fits a unix-domain socket path. The daemon calls it at
// boot and refuses loudly rather than discovering the truncation at the first
// spawn.
func (l Layout) CheckSocketPathBudget() error {
	longest := filepath.Join(l.SockDir(), strings.Repeat("w", socketNameBudget))
	if len(longest) > maxSocketPathLen {
		return fmt.Errorf(
			"state root %q is too long for shim sockets: %q is %d bytes, the limit is %d",
			l.dir, longest, len(longest), maxSocketPathLen)
	}
	return nil
}

// Dirs is every directory the daemon creates under the state root at boot.
func (l Layout) Dirs() []string {
	return []string{l.dir, l.LogsDir(), l.SockDir(), l.IntentDir(), l.OutputDir()}
}
