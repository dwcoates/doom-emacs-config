// Package opentabs serves the editor's OpenTabs: the editor names the
// workspaces it wants open, the daemon brings each visible one up and answers
// it `opened` once it is open, and every step on the way is a typed frame
// (agentrepl.v1.OpenTabsResponse; design record
// docs/protobuf-design/open-tabs.md).
//
// THE BRING-UP IS THE EXISTING ONE. Every workspace an open starts goes
// through bringup.Run and so through workspace.Fleet.Start, the one bring-up
// every caller shares; this package adds only the account of it. The fleet
// reports each step of EVERY bring-up (Coordinator.Step), whoever started it,
// so an open that begins while the boot's own bring-up of a workspace is in
// flight still knows how far that one has got.
//
// OPEN MEANS SERVED, AND DRAWN WHEN A PAGE IS LOADING. A workspace is served
// once agent-repl's OWN services serve it — the daemon, its shim answering
// (which proves the store and the sidecar the shim stood up beside), never the
// vendor or the network. When the editor loads a web page for it, it is open
// only once an attached page of it has also reported itself drawn
// (ReportPageDrawn). A service-level failure opens it too, `failed`.
//
// NO WORKSPACE WAITS ON ANOTHER. Each is answered the moment it opens; the
// editor places tabs by the registry's order, never by the answers'.
//
// EVENTS, NOT STATE: an open's frames go to the stream that asked for them and
// are never replayed.
package opentabs

import (
	"fmt"

	"agent-repld/internal/ids"
)

// StepKind is one step of one workspace's bring-up, as the fleet reports it.
type StepKind int

// The steps. The first ones are relayed to the editor as OpenTabsStep arms;
// StepFailed answers the workspace `opened{failed}`, and the last two are the
// coordinator's own facts (StepServing is relayed as the `served` step).
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

// relayed reports whether the editor is told the step as its own
// OpenTabsStep arm. Serving is told as `served` once, whichever step proved
// it; a failure is the workspace's `opened{failed}`; the session up is no step
// of its own.
func (s Step) relayed() bool {
	return s.Kind != StepServing && s.Kind != StepUp && s.Kind != StepFailed
}

// StepSink is told every bring-up step of every workspace.
type StepSink func(ws ids.WorkspaceID, step Step)
