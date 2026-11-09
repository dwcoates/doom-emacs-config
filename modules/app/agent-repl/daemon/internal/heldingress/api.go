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

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/intakegate"
	"agent-repld/internal/prompthandler"
	"agent-repld/internal/resolve/holds"
	"agent-repld/internal/wsm"
)

// Ingress watches the held-prompt directory and ingests what it finds.
type Ingress interface {
	// Sweep ingests every entry once, oldest first. An entry the queue did
	// not accept stays for a later sweep; only a malformed file is moved
	// aside.
	Sweep(ctx context.Context) error
	// Run sweeps the directory until ctx is cancelled: once at start, which is
	// what ingests everything written while no daemon was serving, then at
	// every change that can let an entry in (Deps.IntakeChanged,
	// Deps.FleetChanges, and an entry written or removed).
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
	// Pending is the held section: told every workspace's unaccepted entries
	// after each sweep, and each entry the queue answered at once.
	Pending holds.IngressSink
	// PublishHost re-pushes one workspace's host state. It is called AFTER an
	// entry's file is removed, so a client re-counting the directory on that
	// push reads the removal.
	PublishHost func(ws ids.WorkspaceID)
	// Serves reports whether THIS daemon takes the intake now:
	// rollout.Controller.ServesIntake. A sweep while it answers false takes
	// nothing. See internal/intakegate.
	Serves func() bool
	// IntakeChanged answers a channel that closes at the next change of
	// Serves' answer: rollout.Controller.IntakeChanged. Run sweeps again on
	// it, so a daemon that comes to serve takes what waited on disk at once.
	IntakeChanged func() <-chan struct{}
	// FleetChanges answers a channel that receives once per change of the
	// fleet, until ctx ends: the sidebar roster's own topic. Every condition
	// a refused entry waits on -- a workspace registered, a merge ended, a
	// cold gate answered, a session started -- is drawn on the roster, so its
	// change is what lets the entry in, and Run sweeps again on it while any
	// entry waits. Each change is counted until ack (publish.Changes), which
	// Run calls once the sweep it woke has run, or at once when nothing waits.
	FleetChanges func(ctx context.Context) (changes <-chan struct{}, ack func())
	// Log is the ingress's logger.
	Log dlog.Surfaces

	// SubmitBound bounds one entry's submission. Zero means
	// DefaultSubmitBound.
	SubmitBound time.Duration
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
}

// DefaultSubmitBound bounds one entry's submission: the bound the editor
// gives the same SubmitPrompt (agent-repl-connect-unary-timeout-seconds),
// whose attempt the entry re-drives. Unbounded, a submission whose delivery
// never answered held the workspace's one shim call and every sweep after it:
// on 2026-10-20 a re-driven /compact waited on a session that never came up,
// and every stop and restart of its workspace waited behind it.
const DefaultSubmitBound = 10 * time.Second

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
	case deps.Pending == nil:
		return nil, fmt.Errorf("heldingress: the held section is required")
	case deps.PublishHost == nil:
		return nil, fmt.Errorf("heldingress: the host publisher is required")
	case deps.Serves == nil:
		return nil, fmt.Errorf("heldingress: the serving answer is required")
	case deps.IntakeChanged == nil:
		return nil, fmt.Errorf("heldingress: the intake's change signal is required")
	case deps.FleetChanges == nil:
		return nil, fmt.Errorf("heldingress: the fleet's change signal is required")
	case deps.Log == nil:
		return nil, fmt.Errorf("heldingress: log surfaces are required")
	}
	if deps.SubmitBound <= 0 {
		deps.SubmitBound = DefaultSubmitBound
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
	return &ingress{
		deps:    deps,
		retries: map[string]retry{},
		gate:    intakegate.New(deps.Serves, deps.Log.Global().With(dlog.Context{"dir": deps.Dir}), opGate),
	}, nil
}
