// Package sentinel writes agent-state sentinel files for Emacs.
//
// The Doom agent-repl module learns per-workspace agent state from
// sentinel files written into $AGENT_REPL_STATE_DIR/workspace-notifications
// (default ~/.claude-emacs/workspace-notifications). For vterm sessions
// the Claude Code hook scripts are the writers; for SDK/gui sessions the
// daemon is authoritative for exactly the events no hook ever produces:
// permission requests (SDK permissions route through canUseTool, not the
// PermissionRequest hook), permission resolutions, and abnormal shim
// death. Everything else (prompt submit, stop, subagents) keeps coming
// from the real hooks — dedup with the hooks is by DISJOINT EVENT SETS,
// not runtime machinery.
//
// File format is byte-identical to the hook scripts' output: two lines,
// CWD then the claude session id (may be empty pre-init), written with a
// SINGLE direct write to the final name. A same-directory tmp+rename is
// deliberately avoided: Emacs's watcher runs on kqueue where a rename
// arrives as an ignored `renamed` action (the dispatcher only acts on
// created/changed), and the tmp file's own creation event would dispatch
// a partially-matching name.
package sentinel

import (
	"fmt"
	"os"
	"path/filepath"
)

// Writer serializes sentinel writes on a dedicated goroutine so callers
// (the session broadcast path, which runs under the session lock) never
// block on disk I/O.
type Writer struct {
	dir  string
	jobs chan job
	done chan struct{}
	logf func(string, ...any)
}

type job struct {
	name string
	cwd  string
	sid  string
}

// Dir resolves the sentinel directory exactly as the Emacs side and the
// hook scripts do: $AGENT_REPL_STATE_DIR falling back to $HOME/.claude-emacs,
// plus the workspace-notifications component.
func Dir() (string, error) {
	root := os.Getenv("AGENT_REPL_STATE_DIR")
	if root == "" {
		home, err := os.UserHomeDir()
		if err != nil {
			return "", fmt.Errorf("sentinel: resolve home dir: %w", err)
		}
		root = filepath.Join(home, ".claude-emacs")
	}
	return filepath.Join(root, "workspace-notifications"), nil
}

// NewWriter constructs a Writer and starts its I/O goroutine. The
// directory resolution error is returned rather than deferred: a daemon
// that cannot know where sentinels go must fail loudly at startup, not
// drop state signals silently later.
func NewWriter(logf func(string, ...any)) (*Writer, error) {
	dir, err := Dir()
	if err != nil {
		return nil, err
	}
	w := &Writer{
		dir:  dir,
		jobs: make(chan job, 64),
		done: make(chan struct{}),
		logf: logf,
	}
	go w.run()
	return w, nil
}

// Close drains pending writes and stops the goroutine.
func (w *Writer) Close() {
	close(w.jobs)
	<-w.done
}

// PermissionRequested records that a permission prompt appeared.
// Filename: permission_request_<sid>_<reqID> — prefix-matched by the
// existing Emacs dispatch entry; the suffix keeps back-to-back events
// from coalescing on one name before the watcher consumes them.
func (w *Writer) PermissionRequested(cwd, sid, reqID string) {
	w.jobs <- job{name: "permission_request_" + sid + "_" + reqID, cwd: cwd, sid: sid}
}

// PermissionResolved records that a permission prompt was answered or
// cancelled. Filename: permission_resolved_<sid>_<reqID>.
func (w *Writer) PermissionResolved(cwd, sid, reqID string) {
	w.jobs <- job{name: "permission_resolved_" + sid + "_" + reqID, cwd: cwd, sid: sid}
}

// SessionDead records an abnormal shim death. Filename: session_dead_<sid>.
func (w *Writer) SessionDead(cwd, sid string) {
	w.jobs <- job{name: "session_dead_" + sid, cwd: cwd, sid: sid}
}

// LoginRequested relays the gui login button: Emacs is asked to run the
// interactive Claude login for the account cwd resolves to. Filename:
// login_request_<sid>. The sid may be empty (a session whose system:init
// has not landed yet still has a cwd, and the cwd is what selects the
// account) — the Emacs dispatcher keys on the cwd, not the id.
func (w *Writer) LoginRequested(cwd, sid string) {
	w.jobs <- job{name: "login_request_" + sid, cwd: cwd, sid: sid}
}

func (w *Writer) run() {
	defer close(w.done)
	for j := range w.jobs {
		if err := w.writeOnce(j); err != nil {
			// Loud retry, never a silent drop: a lost sentinel is a
			// stuck tab in Emacs.
			w.logf("sentinel: write %s failed (%v); retrying", j.name, err)
			if err := w.writeOnce(j); err != nil {
				w.logf("sentinel: RETRY FAILED, sentinel %s LOST: %v", j.name, err)
			}
		}
	}
}

func (w *Writer) writeOnce(j job) error {
	// MkdirAll per write so a wiped state dir self-heals.
	if err := os.MkdirAll(w.dir, 0o755); err != nil {
		return err
	}
	// Single direct write to the final name (see package comment for why
	// not tmp+rename). Three lines: CWD, claude session id, then the
	// ownership marker — the exact format the hook scripts produce and
	// read-sentinel-file parses. Daemon-driven sessions are ALWAYS
	// module-owned (the daemon only ever runs sessions Emacs asked for),
	// so the marker is unconditional here.
	return os.WriteFile(filepath.Join(w.dir, j.name),
		[]byte(j.cwd+"\n"+j.sid+"\nowned\n"), 0o644)
}
