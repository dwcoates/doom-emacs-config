// Package opentabs serves the editor's OpenTabs: the editor names the
// workspaces it wants open, the daemon answers each visible one `opened` once
// it is open, and every step on the way is a typed frame
// (agentrepl.v1.OpenTabsResponse; design record
// docs/protobuf-design/open-tabs.md).
//
// AN OPEN NEVER WAITS ON A SHIM, AND STARTS NONE (owner ruling, 2026-10-28).
// A workspace's conversation is read from the store, so it is open once its
// newest page is read whole through the feed resolver's own loading
// (`conversation_read`; the page's OpenFeed then serves it from memory) and,
// when the editor loads a web page for it, an attached page of it has
// reported itself drawn (ReportPageDrawn). Nothing about its shim -- up,
// parked, at its cold gate, starting, or never started -- holds the answer:
// a prompt brings a shim up, and is held "after loading" until it is. A read
// that fails answers `failed`. `ready` and `failed` carry the workspace's
// roster row, taken with its roster's revision once the open no longer marks
// it `opening`. A hidden workspace is answered at once.
//
// A BRING-UP IN FLIGHT IS RELAYED, NEVER WAITED FOR: every step the fleet
// reports for a workspace an open is answering (Coordinator.Step), and every
// account of a cut turn's resumption (Coordinator.Resumption), reaches the
// editor as a progress step.
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

// Begins reports whether the step begins a bring-up: a shim being started or
// adopted, or a parked session waking.
func (s Step) Begins() bool { return s.Kind == StepStartingSession || s.Kind == StepWaking }

// Ends reports whether the step ends a bring-up, however it ended.
func (s Step) Ends() bool {
	switch s.Kind {
	case StepUp, StepFailed, StepColdGate, StepVendorRejected, StepVendorFailed:
		return true
	}
	return false
}

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

// relayed reports whether the editor is told the step as its own
// OpenTabsStep arm. Serving is told as `served` once, whichever step proved
// it; a failure is the workspace's `opened{failed}`; the session up is no step
// of its own.
func (s Step) relayed() bool {
	return s.Kind != StepServing && s.Kind != StepUp && s.Kind != StepFailed
}

// StepSink is told every bring-up step of every workspace.
type StepSink func(ws ids.WorkspaceID, step Step)
