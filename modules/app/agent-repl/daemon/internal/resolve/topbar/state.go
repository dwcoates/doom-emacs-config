package topbar

import (
	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/apiresponses"
	"claude-repld/internal/dlog"
	"claude-repld/internal/shimclient"
)

// usageFingerprint is one unit's reported usage, kept so a re-report can be
// recognized as the SAME usage rather than counted twice.
//
// USAGE IS STAMPED ON EXACTLY ONE UNIT PER API RESPONSE and a unit's frames
// UPSERT, so the same usage arrives again on every later frame of that unit.
// The session totals are therefore a sum over units, and a unit whose
// fingerprint CHANGES is a producer contradiction rather than more spend.
type usageFingerprint struct {
	// read is what the prompt cache served.
	read uint64
	// written is what was processed fresh and entered the cache.
	written uint64
	// unwritten is what was processed fresh and never entered the cache.
	unwritten uint64
	// output is every generated token.
	output uint64
	// thinking is the part of output attributed to reasoning.
	thinking uint64
}

// misses is the expensive sum: both cache-miss buckets, together.
func (f usageFingerprint) misses() uint64 { return f.written + f.unwritten }

// modelTotals is one model's share of the session's spend.
type modelTotals struct {
	// model is the model's own name, the vendor's spelling.
	model string
	// figures are the summed usage attributed to it.
	figures usageFingerprint
	// order is the order the model was first seen in, which the breakdown's
	// per-model sections are drawn in.
	order int
}

// warningKind names which concern a warning carries.
type warningKind int

// The warning kinds, one per TopbarWarning detail arm.
const (
	warnAccounting warningKind = iota
	warnUnmodeledTool
	warnDetachedUnmodeled
	warnSessionFault
	warnDegradedWindow
)

// warning is one accumulated concern plus the order it takes in the dropdown.
type warning struct {
	// kind is which concern it is.
	kind warningKind
	// key identifies the concern so a re-observation updates it rather than
	// reordering the list.
	key string
	// seq is the observation order; the dropdown draws the highest first.
	seq int
	// line is the dropdown row's sentence.
	line string
	// detail is the overlay the click reveals.
	detail func() *frontendv1.TopbarWarning
}

// unmodeledCall is one distinct unmodeled tool the session has run.
type unmodeledCall struct {
	// toolName is the tool as the agent named it.
	toolName string
	// argumentLines are the daemon's abbreviated account of the call.
	argumentLines []string
	// seq is the observation order.
	seq int
}

// faultRecord is one standing shim fault.
type faultRecord struct {
	// component is which part of the shim faulted.
	component string
	// detail is the fault's own account.
	detail string
	// line is the dropdown row's sentence.
	line string
	// seq is the observation order.
	seq int
}

// windowRecord is one degraded window the shim has reported.
type windowRecord struct {
	// component is which part of the shim degraded.
	component string
	// reason is the shim's stated reason.
	reason string
	// beganAtMs is when the window opened.
	beganAtMs int64
	// closed reports whether the window has closed.
	closed bool
	// endedAtMs is when it closed, meaningful only when closed.
	endedAtMs int64
	// dropped is how many observations were lost inside it.
	dropped int64
	// seq is the observation order.
	seq int
}

// wsState is one workspace's whole topbar accumulation. In-memory only: a
// resolver aggregates, it never stores.
type wsState struct {
	// dir is the workspace directory, bound before any frame arrives.
	dir string
	// log is the workspace-bound logger, nil until the directory is bound.
	log dlog.Logger

	// naming is the WSM-derived title and session line.
	naming Naming
	// namingSet reports whether the naming has been installed.
	namingSet bool

	// vendorSessionID is the vendor's identity for the session.
	vendorSessionID string
	// started reports whether the session has opened.
	started bool

	// model is the effective model, LAST-WRITER-WINS in the order the shim
	// stated it: SessionStarted.effective_model first, then every
	// model_changed.
	model string
	// catalog is the switchable model set the selector renders.
	catalog []*conversationv1.ModelOption

	// permissionMode is the mode in force, as the session facts spell it.
	permissionMode string
	// picker is exactly the switchable set the daemon will accept.
	picker *frontendv1.TopbarPermissionModePicker

	// fastMode is the vendor's fast mode as last stated, nil until the
	// session has stated one. Standing, like the permission mode beside it:
	// the state sticks until the vendor states another.
	fastMode *conversationv1.SessionFastMode

	// email is the logged-in account, empty when the root is logged out.
	email string
	// accountSet reports whether the config root has been read at all.
	accountSet bool

	// link is the last observed link state.
	link shimclient.LinkState
	// linkSeen reports whether any link state has been observed.
	linkSeen bool
	// hostStream and webStream are the other two hops of connectivity truth
	// (daemon.md invariant 11): the WatchHostWorkspace and WatchWebWorkspace
	// streams' liveness, stated by the server on every open and close edge.
	hostStream bool
	webStream  bool

	// contextUsage is the vendor's own get_context_usage answer, the ONE fact
	// the chip and the /context panel both resolve from.
	contextUsage *conversationv1.SessionContextUsage

	// counted is every unit whose usage has been folded into the session
	// totals, with what it reported.
	counted map[string]usageFingerprint
	// responses files this session's units under the API RESPONSE each
	// arrived in, and is the accounting warning's denominator. Usage rides
	// EXACTLY ONE unit per API response (the first content block's), so
	// absence on a unit means "not the carrying unit", never "free": the
	// reconciliation is per API response and never per unit. The ledger is
	// SHARED with the footer's per-turn verdict so the rule cannot drift
	// between the two.
	responses *apiresponses.Ledger
	// totals are the session's summed figures.
	totals usageFingerprint
	// perModel is each model's share of the spend, by model name.
	perModel map[string]*modelTotals

	// mcpServers are the MCP server healths the session has stated, in the
	// order the servers were FIRST named. A later update for a server already
	// named replaces its health in place, so the /mcp panel's row order is
	// stable across health churn rather than reordering under the reader.
	mcpServers []*conversationv1.SessionMcpServer

	// contradictions are the reconciliation problems observed this session.
	contradictions []string

	// unmodeled are the distinct unmodeled tools the session has run.
	unmodeled map[string]*unmodeledCall
	// detachedUnmodeled are the live detached-unmodeled items.
	detachedUnmodeled []DetachedUnmodeled
	// detachedSeq is the observation order of the detached-unmodeled set.
	detachedSeq int
	// faults are the standing shim faults, by their identity.
	faults map[string]*faultRecord
	// windows are the degraded windows, by their identity.
	windows map[string]*windowRecord
	// accountingSeq is the accounting warning's observation order, zero until
	// it has ever been raised.
	accountingSeq int

	// seq mints observation orders.
	seq int
}

// newWSState builds an empty accumulation.
func newWSState() *wsState {
	return &wsState{
		counted:   map[string]usageFingerprint{},
		responses: apiresponses.New(),
		perModel:  map[string]*modelTotals{},
		unmodeled: map[string]*unmodeledCall{},
		faults:    map[string]*faultRecord{},
		windows:   map[string]*windowRecord{},
	}
}

// nextSeq mints the next observation order.
func (s *wsState) nextSeq() int {
	s.seq++
	return s.seq
}

// ready reports whether every non-optional element of the view can be
// resolved. See the package comment: these five are the whole gate.
func (s *wsState) ready() bool {
	return s.namingSet && s.started && s.accountSet && s.picker != nil && s.contextUsage != nil
}

// missing names what readiness is still waiting on, for the record.
func (s *wsState) missing() []string {
	var out []string
	if !s.namingSet {
		out = append(out, "naming")
	}
	if !s.started {
		out = append(out, "session_started")
	}
	if !s.accountSet {
		out = append(out, "account")
	}
	if s.picker == nil {
		out = append(out, "permission_mode_picker")
	}
	if s.contextUsage == nil {
		out = append(out, "context_usage")
	}
	return out
}
