// Package footer is the footer resolver.
//
// It publishes the footer view WHOLE and only when complete: the tokens cell
// and its panels are ALWAYS populated. R1 momentary statuses (`interrupted`,
// `loading`) are retired by a DAEMON-SIDE one-shot successor push — nothing on
// the wire ticks. See ARCHITECTURE.md "resolvers".
package footer

import (
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
	"claude-repld/internal/notimpl"
	"claude-repld/internal/publish"
	"claude-repld/internal/sessionwatcher"
)

// MergeFacts is what the merge orchestrator tells the footer and the sidebar
// about a workspace's merge. ARCHITECTURE.md does not fix its fields; the
// minimum the contract implies is the state, its queue position and the
// evidence a parked merge needs.
type MergeFacts struct {
	// State names the merge's standing: "none", "enqueuing", "queued",
	// "merging", "conflict", "failed", "merged".
	State string
	// QueuePosition is the workspace's place in its repo's queue, zero when it
	// is not queued.
	QueuePosition int
	// Round is the merge's current round number, zero before the first.
	Round int
	// Detail is the evidence a conflicted or failed merge carries.
	Detail string
}

// CloseBlocked is why a close was refused: a close requires quiet, and the
// refusal MANIFESTS IN THE FOOTER rather than only in the rpc's answer.
type CloseBlocked struct {
	// Reason names what is not quiet: "turn_in_flight", "live_work",
	// "held_prompts", "merge_queued".
	Reason string
	// Detail is the human-readable sentence the footer draws.
	Detail string
}

// ColdGate is the standing cold-context gate, which owns the composer while it
// stands.
type ColdGate struct {
	// Standing reports whether a gate is open.
	Standing bool
	// Detail is what was refused cold.
	Detail string
}

// Resolver is the footer's whole surface.
type Resolver interface {
	sessionwatcher.FooterSink

	// SetMerge installs the merge facts the footer draws.
	SetMerge(ws ids.WorkspaceID, facts MergeFacts)
	// SetClosing installs a close refusal, nil to clear it.
	SetClosing(ws ids.WorkspaceID, blocked *CloseBlocked)
	// SetColdGate installs the standing cold gate.
	SetColdGate(ws ids.WorkspaceID, gate ColdGate)
	// SetInterrupting fires the waiting-interrupting status the MOMENT an
	// interrupt registers, before the real turn end arrives.
	SetInterrupting(ws ids.WorkspaceID, on bool)
	// Topic is the workspace's footer publication.
	Topic(ws ids.WorkspaceID) *publish.Topic[*frontendv1.FooterView]
}

// New builds the footer resolver.
func New(log dlog.Surfaces) (Resolver, error) {
	return nil, notimpl.Err
}
