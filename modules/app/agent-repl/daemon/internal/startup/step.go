// Package startup brings the editor's workspaces up when a NEW Emacs process
// connects, and tells that Emacs, step by step, what it is doing
// (agentrepl.v1.DaemonStartupEvent on the Emacs WatchDaemon stream; design
// record docs/protobuf-design/startup-and-fault-domains.md).
//
// THE BRING-UP IS THE EXISTING ONE. Every workspace a run starts goes through
// bringup.Run and so through workspace.Fleet.Start, the one bring-up every
// caller shares; this package adds only the account of it. The fleet reports
// each step of EVERY bring-up (Coordinator.Step), whoever started it, so a run
// that begins while the boot's own bring-up of a workspace is in flight still
// knows how far that one has got.
//
// THE GO-AHEAD. A workspace's tab may open once agent-repl's OWN services
// serve it — the daemon, its shim answering (which proves the store and the
// sidecar the shim stood up beside), never the vendor or the network — or once
// its service-level bring-up has settled as failed. Go-aheads are sent in the
// REGISTRY ORDER (the roster's walk, sidebar.TabOrder), never in the order
// workspaces become ready: workspace 3 waits for 1 and 2 even when it is ready
// first, and says so (`waiting_for`).
//
// EVENTS, NOT STATE: a run's events go to the stream it was started for and
// are never replayed. An Emacs that reconnects reads the roster instead.
package startup

import (
	"fmt"

	"claude-repld/internal/ids"
)

// StepKind is one step of one workspace's bring-up, as the fleet reports it.
type StepKind int

// The steps. The first ones are what the editor prints; the last two are the
// coordinator's own facts and are never relayed.
const (
	// StepStartingSession is a shim being started or adopted.
	StepStartingSession StepKind = iota + 1
	// StepWaking is a hibernated workspace being woken.
	StepWaking
	// StepResuming is the conversation being resumed (a StartSession that
	// resumes, after the shim answered).
	StepResuming
	// StepVendorRetrying is a vendor start that failed and is retried.
	StepVendorRetrying
	// StepVendorRejected is a vendor that refused to start.
	StepVendorRejected
	// StepVendorFailed is a vendor that failed for the whole retry window.
	StepVendorFailed
	// StepColdGate is a resume waiting on the user's cold-gate answer.
	StepColdGate
	// StepOffline is the network unreachable, so the vendor cannot start.
	StepOffline
	// StepFailed is the service-level bring-up failing.
	StepFailed
	// StepServing is agent-repl's services serving the workspace: the shim
	// answered healthy. Never relayed.
	StepServing
	// StepUp is the session up. Never relayed.
	StepUp
)

// String names the step for the records.
func (k StepKind) String() string {
	switch k {
	case StepStartingSession:
		return "starting_session"
	case StepWaking:
		return "waking"
	case StepResuming:
		return "resuming"
	case StepVendorRetrying:
		return "vendor_retrying"
	case StepVendorRejected:
		return "vendor_rejected"
	case StepVendorFailed:
		return "vendor_failed"
	case StepColdGate:
		return "cold_gate"
	case StepOffline:
		return "offline"
	case StepFailed:
		return "failed"
	case StepServing:
		return "serving"
	case StepUp:
		return "up"
	default:
		return fmt.Sprintf("step(%d)", int(k))
	}
}

// Step is one step with what it carries.
type Step struct {
	Kind StepKind
	// Attempt is a vendor retry's failed attempt, counting from 1.
	Attempt uint32
	// Text is a rejection's cause or a failure's reason, verbatim.
	Text string
}

// begins reports whether the step opens a bring-up.
func (s Step) begins() bool { return s.Kind == StepStartingSession || s.Kind == StepWaking }

// serves reports whether the step proves agent-repl's services serve the
// workspace: everything after the shim answered, whatever the vendor did.
func (s Step) serves() bool {
	switch s.Kind {
	case StepServing, StepResuming, StepVendorRetrying, StepVendorRejected, StepVendorFailed,
		StepColdGate, StepOffline, StepUp:
		return true
	}
	return false
}

// ends reports whether the step ends a bring-up.
func (s Step) ends() bool {
	switch s.Kind {
	case StepUp, StepFailed, StepColdGate, StepVendorRejected, StepVendorFailed:
		return true
	}
	return false
}

// relayed reports whether the editor is told the step.
func (s Step) relayed() bool { return s.Kind != StepServing && s.Kind != StepUp }

// StepSink is told every bring-up step of every workspace.
type StepSink func(ws ids.WorkspaceID, step Step)
