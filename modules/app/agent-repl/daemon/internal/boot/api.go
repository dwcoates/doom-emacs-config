// Package boot is the boot sequence and adoption reconciliation.
//
// It probes the workspace locks to find shims that outlived a crashed daemon,
// ADOPTS them rather than killing and restarting them, reconciles the intent
// manifest an outgoing daemon left, restores the held prompts all-or-nothing,
// and closes the turns that never got a terminal. See ARCHITECTURE.md "boot".
package boot

import (
	"context"

	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
	"claude-repld/internal/merge"
	"claude-repld/internal/notimpl"
	"claude-repld/internal/promptqueue"
	"claude-repld/internal/shimclient"
	"claude-repld/internal/stateroot"
	"claude-repld/internal/wsm"
)

// Report is what one boot reconciled. It is returned rather than only logged
// so the daemon can answer for its own startup.
type Report struct {
	// Adopted are the workspaces whose surviving shims were reconnected.
	Adopted []ids.WorkspaceID
	// Orphaned are the turns closed because they had no terminal.
	Orphaned []ids.TurnID
	// HoldsRestored is how many held prompts came back.
	HoldsRestored int
	// MergesRecovered are the in-flight merges resumed.
	MergesRecovered []ids.WorkspaceID
}

// Sequence runs the boot.
type Sequence interface {
	// Run performs the whole reconciliation: probe the workspace locks, adopt
	// surviving shims, reconcile the intent manifest, restore holds
	// all-or-nothing, close orphaned turns, and recover in-flight merges. Any
	// step that cannot complete fails the boot LOUDLY rather than starting
	// degraded.
	Run(ctx context.Context) (Report, error)
	// Joining reports whether this daemon was started with -joining, in which
	// case it takes ownership workspace by workspace from the incumbent and
	// publishes daemon.addr only once it owns every one.
	Joining() bool
}

// Deps are the boot sequence's collaborators.
type Deps struct {
	// Layout names every path under the state root.
	Layout stateroot.Layout
	// DB is the durable state being reconciled.
	DB wsm.DB
	// Supervisor adopts the surviving shims.
	Supervisor shimclient.Supervisor
	// Queue restores the held prompts.
	Queue promptqueue.Queue
	// Merge recovers in-flight merges.
	Merge merge.Orchestrator
	// RunDir is the kernel-lock directory the workspace locks are probed in.
	RunDir string
	// JoiningAddress is the incumbent's address when this daemon is a joining
	// successor, empty otherwise.
	JoiningAddress string
	// Log is the boot logger.
	Log dlog.Surfaces
}

// New builds the boot sequence.
func New(deps Deps) (Sequence, error) {
	return nil, notimpl.Err
}
