// Package heldingress is the held-prompt ingress:
// $AGENT_REPL_STATE_DIR/held-prompts/held_*.json.
//
// A client whose SubmitPrompt the daemon did not answer (no daemon, a stuck
// one, a handover refusal) writes the prompt here instead of holding it in its
// own memory. The directory needs no live daemon, survives a restart of the
// client, the daemon and the shim alike, and is read by the ONE path a typed
// prompt takes: every entry is handed to prompthandler.Handler.Submit under
// the idempotency key the client first submitted it with. So the queue holds,
// classifies and delivers it by its ordinary rules, it lands in the held tray
// like any other held prompt, and a prompt the daemon DID accept before the
// client gave up is answered duplicate_submission and never delivered twice.
//
// THE FILE IS REMOVED ONLY AFTER THE QUEUE ACCEPTED ITS PROMPT (or answered
// that it already had). A crash anywhere before the removal leaves the file,
// and the next sweep resubmits it under the same key, which the handler's
// durable claim answers as the duplicate it is. So a crash mid-ingest neither
// loses nor duplicates a prompt.
//
// See ARCHITECTURE.md "heldingress" for the file format.
package heldingress

import (
	"context"
	"fmt"
	"path/filepath"
	"time"

	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
	"claude-repld/internal/intakegate"
	"claude-repld/internal/prompthandler"
	"claude-repld/internal/wsm"
)

// Ingress watches the held-prompt directory and ingests what it finds.
type Ingress interface {
	// Sweep ingests every entry once, oldest first. An entry the queue did
	// not accept stays for a later sweep; only a malformed file is moved
	// aside.
	Sweep(ctx context.Context) error
	// Run sweeps the directory until ctx is cancelled: once at start, which is
	// what ingests everything written while no daemon was serving, then on
	// every interval.
	Run(ctx context.Context) error
}

// WorkspaceByDirFunc resolves an entry's project directory to its
// registered workspace. It is wsm.DB.WorkspaceByDir.
type WorkspaceByDirFunc func(ctx context.Context, dir string) (wsm.Workspace, error)

// Deps are the ingress's collaborators.
type Deps struct {
	// Dir is the ingress directory.
	Dir string
	// WorkspaceByDir resolves an entry's project_dir. The directory is the
	// key because it is the one identity a client holds while no daemon is
	// serving to tell it an id.
	WorkspaceByDir WorkspaceByDirFunc
	// Prompts is the same SubmitPrompt body the rpc calls, which is what makes
	// an ingested prompt indistinguishable from a typed one — and what dedupes
	// it by its idempotency key.
	Prompts prompthandler.Handler
	// PublishHost re-pushes one workspace's host state. It is called AFTER an
	// entry's file is removed, so a client re-counting the directory on that
	// push reads the removal.
	PublishHost func(ws ids.WorkspaceID)
	// Serves reports whether THIS daemon takes the intake now:
	// rollout.Controller.ServesIntake. A sweep while it answers false takes
	// nothing. See internal/intakegate.
	Serves func() bool
	// Log is the ingress's logger.
	Log dlog.Surfaces

	// Interval is how often the directory is polled. Zero means
	// DefaultInterval. It is also the first retry delay of an entry the queue
	// refused.
	Interval time.Duration
	// RetryCeiling caps the doubling retry delay of an entry the queue keeps
	// refusing. Zero means DefaultRetryCeiling.
	RetryCeiling time.Duration
	// QuarantineDir is where a malformed file is renamed to; empty means
	// "quarantine" beneath Dir.
	QuarantineDir string
	// LockPath is the kernel lock a sweep holds for its whole run; empty means
	// LockName beneath Dir.
	LockPath string
	// Remove deletes an ingested entry's file; nil means os.Remove. It is a
	// seam so a test can stand in for a crash between the acceptance and the
	// removal.
	Remove func(path string) error
	// Now supplies the instant a retry is judged against; nil means time.Now.
	Now func() time.Time
}

// DefaultInterval is the ingress's poll cadence: the same cadence the
// command-file ingress polls at, for the same reason — a poll cannot miss a
// file the way a dropped watch can.
const DefaultInterval = 250 * time.Millisecond

// DefaultRetryCeiling caps the retry delay of a refused entry. A refusal is a
// standing condition (a merge in flight, a cold gate, a workspace not yet
// registered); retrying it every poll would re-run the submission and its
// records four times a second for as long as the condition stands.
const DefaultRetryCeiling = 10 * time.Second

// Glob matches the entries the ingress ingests. A producer writes a
// dot-prefixed temporary name and renames it into place, and the dot-prefixed
// names deliberately do not match, so a half-written entry is never read.
const Glob = "held_*.json"

// LockName is the sweep lock's file name beneath the ingress directory. It
// does not match Glob, so it is never read as an entry.
const LockName = ".sweep.lock"

// New builds the ingress.
func New(deps Deps) (Ingress, error) {
	switch {
	case deps.Dir == "":
		return nil, fmt.Errorf("heldingress: an ingress directory is required")
	case deps.WorkspaceByDir == nil:
		return nil, fmt.Errorf("heldingress: the workspace lookup is required")
	case deps.Prompts == nil:
		return nil, fmt.Errorf("heldingress: the prompt handler is required")
	case deps.PublishHost == nil:
		return nil, fmt.Errorf("heldingress: the host publisher is required")
	case deps.Serves == nil:
		return nil, fmt.Errorf("heldingress: the serving answer is required")
	case deps.Log == nil:
		return nil, fmt.Errorf("heldingress: log surfaces are required")
	}
	if deps.Interval <= 0 {
		deps.Interval = DefaultInterval
	}
	if deps.RetryCeiling <= 0 {
		deps.RetryCeiling = DefaultRetryCeiling
	}
	if deps.QuarantineDir == "" {
		deps.QuarantineDir = filepath.Join(deps.Dir, "quarantine")
	}
	if deps.LockPath == "" {
		deps.LockPath = filepath.Join(deps.Dir, LockName)
	}
	if deps.Remove == nil {
		deps.Remove = removeFile
	}
	if deps.Now == nil {
		deps.Now = time.Now
	}
	return &ingress{
		deps:    deps,
		retries: map[string]retry{},
		gate:    intakegate.New(deps.Serves, deps.Log.Global().With(dlog.Context{"dir": deps.Dir}), opGate),
	}, nil
}
