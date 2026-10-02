package topbar

import (
	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/apiresponses"
	"claude-repld/internal/claudesettings"
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
	// warnSessionless is the one warning with NO detail arm: the workspace's
	// session-less state is a statement, not a control.
	warnSessionless warningKind = iota
	warnAccounting
	warnUnmodeledTool
	warnDetachedUnmodeled
	warnSessionFault
	warnDegradedWindow
	// warnRaised is a condition the DAEMON raised about its own resolution —
	// detached work it could not place, say. Like the session-less line it
	// carries NO detail arm: the sentence is the whole warning, and the full
	// context is in the record the raising site logged beside it.
	warnRaised
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

// raisedRecord is one warning the daemon raised about its own resolution.
type raisedRecord struct {
	// line is the dropdown row's sentence.
	line string
	// deployFailed is a failed deploy's overlay, nil for a row with none.
	// Only a daemon-scoped warning carries one.
	deployFailed *DeployFailedOverlay
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
	// unboundReported latches that a record for this workspace already
	// arrived unbound and the invariant violation was stated at ERROR.
	unboundReported bool

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

	// sessionTitle is the vendor's OWN summary of this conversation, as the
	// session last stated it (conversation.v1 SessionUpdate.title). Empty
	// until the vendor has written one, which is what makes the workspace name
	// the fallback rather than a second title.
	sessionTitle string

	// synthesizedTitle is the daemon's OWN one-line summary of this
	// conversation, produced by a cheap headless call over the title digest
	// when the vendor has written no ai-title. It is the MIDDLE precedence in
	// title(): the vendor's own summary always wins over it, and it always
	// wins over the workspace name. Empty until the synthesizer has produced
	// one; its sole writer is SetSynthesizedTitle.
	synthesizedTitle string

	// permissionMode is the mode in force, as the session facts spell it.
	permissionMode string
	// picker is exactly the switchable set the daemon will accept.
	picker *frontendv1.TopbarPermissionModePicker

	// fastMode is the vendor's fast mode as last stated, nil until the
	// session has stated one. Standing, like the permission mode beside it:
	// the state sticks until the vendor states another.
	fastMode *conversationv1.SessionFastMode

	// effortSettings is what the session's config root persists for the
	// effort level, read at workspace initialization (claudesettings).
	effortSettings claudesettings.Effort
	// pushedEffort is the level the vendor itself states its next request
	// sends (SessionUpdate.effort_changed), UNSPECIFIED until the session's
	// shim pushes one. It is THE AUTHORITY: it outranks the pick and the
	// settings read, which only stand before it arrives.
	pushedEffort conversationv1.AgentEffortLevel
	// pickedEffort is the level the shim confirmed for the last SetEffort,
	// UNSPECIFIED before any pick. It outranks the settings: it is what the
	// session runs at.
	pickedEffort conversationv1.AgentEffortLevel
	// effortStandingLogged and windowSourceLogged are what the last edge
	// records stated about the effort selector and the chip's window, so a
	// change is recorded once, when it happens, and not on every publication.
	effortStandingLogged string
	windowSourceLogged   string

	// email is the logged-in account, empty when the root is logged out.
	email string
	// accountOptions is every root the cell offers, in the order the daemon
	// served them.
	accountOptions []AccountOption
	// accountSet reports whether the config root has been read at all.
	accountSet bool

	// link is the last observed link state.
	link shimclient.LinkState
	// linkSeen reports whether any link state has been observed.
	linkSeen bool
	// parked reports the idle sweep's deliberate stand-down, the same fact the
	// footer and the roster key their idle arms on. It is cleared by the next
	// link state of any kind, which belongs to the revival's own spawn.
	parked bool
	// parkedAtMs is when the park was installed, epoch ms. It is the
	// hibernated view's only content: the strip ticks the age from it, so the
	// wire carries the instant and never a duration that would be stale on
	// arrival.
	parkedAtMs int64
	// coldGate reports that this workspace is STANDING AT THE COLD GATE: the
	// shim answered `cold` to the session start, so no session was ever
	// created and the feed is showing the gate card the reader has to answer.
	// It is the same fact the footer's own cold-gate status keys on, stated
	// here by the same call sites, and it is retired when the gate is
	// answered.
	coldGate bool
	// coldGateAtMs is when the gate rose, epoch ms, and coldGateTokens is what
	// the cold read would re-read. They are the cold-gate view's whole
	// content, for the same reason parkedAtMs is the hibernated view's.
	coldGateAtMs   int64
	coldGateTokens int64
	// sessionlessPublished is what the LAST published view said about the
	// session-scoped half of the strip. It is the edge detector behind the two
	// info records: a strip drawn with dashes where its controls belong, or
	// the return of the session facts, is only an EDGE by comparison with what
	// the reader was last shown.
	sessionlessPublished bool
	// hostStream and webStream are the other two hops of connectivity truth
	// (daemon.md invariant 11): the WatchHostWorkspace and WatchWebWorkspace
	// streams' liveness, stated by the server on every open and close edge.
	hostStream bool
	webStream  bool

	// contextUsage is the vendor's own get_context_usage answer, the ONE fact
	// the chip and the /context panel both resolve from.
	contextUsage *conversationv1.SessionContextUsage

	// contextCut reports that a context cut (a /clear or a completed
	// compaction) discarded the transcript the last `contextUsage` was read
	// off, so that reading no longer describes the context. It is set by
	// OnContextCut when the cut actually removed context and cleared by the
	// next `context_usage` reading — the vendor's fresh answer for the cut
	// context. While it stands and no fresh reading has landed, the chip
	// states the count is unknown rather than a stale figure or a fabricated 0.
	contextCut bool

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
	// raised are the warnings the daemon raised about its own resolution, by
	// the key the raising site named. Never retracted: each is a thing that
	// already went wrong, and the record of it stays in front of the reader.
	raised map[string]*raisedRecord
	// daemonRaised are the DAEMON-scoped warnings standing on this strip, by
	// key: the resolver's daemon set, each with this strip's own observation
	// order. Retracted when the daemon retracts them.
	daemonRaised map[string]*raisedRecord

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
		raised:    map[string]*raisedRecord{},

		daemonRaised: map[string]*raisedRecord{},
	}
}

// nextSeq mints the next observation order.
func (s *wsState) nextSeq() int {
	s.seq++
	return s.seq
}

// sessionless reports whether this workspace HAS NO SESSION AT ALL — the idle
// sweep stood it down, it is standing at the cold gate, or no session has
// started yet. It is ONE predicate so a cell can never be drawn as live in one
// of those states and dead in another.
//
// IT IS NOT A WHOLE-VIEW STATE. Under the FIXED SCHEMA ruling the strip has
// one shape; this only decides what each session-scoped cell states, and the
// two named states additionally contribute their reason to the context chip's
// hover and one line to the warning strip.
func (s *wsState) sessionless() bool {
	return s.parked || s.coldGate || !s.started
}

// ready reports whether the view can be resolved at all. THE GATE IS THE
// WORKSPACE FACTS AND NOTHING ELSE — the naming (WSM's) and the account (the
// config root's) — under the FIXED SCHEMA ruling of 2026-09-13.
//
// NO SESSION FACT IS EVER A GATE. Every session-scoped cell states "I do not
// know yet" in its own slot: the three controls by ABSENCE, which the client
// draws as a dash, and the context chip and warning strip by their own
// content. So a workspace with no session — hibernated, cold-gated, or simply
// not started — publishes the same strip as every other workspace, and the
// facts fill in as they arrive. Gating on a session fact was how a strip that
// would never get one stayed BLANK for as long as the state stood.
func (s *wsState) ready() bool {
	return s.namingSet && s.accountSet
}

// missing names what readiness is still waiting on, for the record.
func (s *wsState) missing() []string {
	var out []string
	if !s.namingSet {
		out = append(out, "naming")
	}
	if !s.accountSet {
		out = append(out, "account")
	}
	return out
}
