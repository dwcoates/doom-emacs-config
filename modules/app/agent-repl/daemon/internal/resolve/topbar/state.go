package topbar

import (
	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"agent-repld/internal/claudesettings"
	"agent-repld/internal/dlog"
	"agent-repld/internal/shimclient"
	"agent-repld/internal/wsm"
)

// usageFingerprint is a usage's figures, the operand of the session totals'
// arithmetic.
//
// USAGE IS STAMPED ON EXACTLY ONE UNIT PER API RESPONSE and a unit's frames
// UPSERT, so the same usage arrives again on every later frame of that unit.
// The session totals are therefore a sum over units, each unit counted with
// the report tokenusage.Reconcile last took for it.
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

// wsState is one workspace's whole topbar accumulation. In-memory only: a
// resolver aggregates, it never stores.
type wsState struct {
	// unboundRecorded records that a frame for this never-bound workspace was
	// already recorded at ERROR, so the violation is stated once per
	// workspace rather than at every lookup of its logger.
	unboundRecorded bool
	// quietStale records that a fact moved while the quiet gate stood, so
	// leaving it publishes this strip once (followQuiet).
	quietStale bool
	// dir is the workspace directory, bound before any frame arrives.
	dir string
	// log is the workspace-bound logger, nil until the directory is bound.
	log dlog.Logger

	// naming is the WSM-derived workspace name and session line.
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
	// session last stated it (conversation.v1 SessionUpdate.title). It is the
	// MIDDLE precedence in title(): drawn only while the workspace's agent has
	// set no summary, and always in preference to the workspace name.
	sessionTitle string

	// summary is the summary of this workspace's task that its own agent set.
	// Its short summary is the FIRST precedence in title(), ahead of the
	// vendor's ai-title, and its long summary the title's hover description.
	// The zero value while none stands; its sole writer is SetSummary.
	summary wsm.WorkspaceSummary

	// links are the links the workspace name's menu shows, most recently set
	// first; empty while none is shown. Its sole writer is SetLinks.
	links []Link

	// permissionMode is the mode in force, as the session facts spell it.
	permissionMode string
	// picker is exactly the switchable set the daemon will accept.
	picker *frontendv1.TopbarPermissionModePicker

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
	// accountConfigDir is the login the session spends as, and defaultModel
	// that login's default model (Account.DefaultModel).
	accountConfigDir string
	defaultModel     string

	// link is the last observed link state.
	link shimclient.LinkState
	// linkSeen reports whether any link state has been observed.
	linkSeen bool
	// parked is the daemon's deliberate stand-down of an idle session, the
	// same fact the footer and the roster key their idle arms on. There is
	// one park, drawn as the live idle session it stands in for. It is
	// cleared by the next link state of any kind, which belongs to the
	// revival's own spawn.
	// The park it composes carries the cache keeper's next scheduled
	// keep-alive from the resolver's own table (resolver.parkLocked).
	parked bool
	// coldGate reports that this workspace is STANDING AT THE COLD GATE: the
	// shim answered `cold` to the session start, so no session was ever
	// created and the feed is showing the gate card the reader has to answer.
	// It is the same fact the footer's own cold-gate status keys on, stated
	// here by the same call sites, and it is retired when the gate is
	// answered.
	coldGate bool
	// coldGateAtMs is when the gate rose, epoch ms, and coldGateTokens is what
	// the cold read would re-read. They are the cold-gate view's whole
	// content.
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

	// bankedTokens is the context every earlier cut discarded, summed: each
	// completed cut adds the reading it removed (OnContextCut). The chip's
	// total session context is this plus the live context, so a cut never
	// shrinks it.
	bankedTokens int64

	// counted is every unit whose usage has been folded into the session
	// totals, with the report counted for it.
	counted map[string]*conversationv1.TokenUsage
	// totals are the session's summed figures.
	totals usageFingerprint
	// perModel is each model's share of the spend, by model name.
	perModel map[string]*modelTotals

	// mcpServers are the MCP server healths the session has stated, in the
	// order the servers were FIRST named. A later update for a server already
	// named replaces its health in place, so the /mcp panel's row order is
	// stable across health churn rather than reordering under the reader.
	mcpServers []*conversationv1.SessionMcpServer

	// seq mints observation orders.
	seq int
}

// newWSState builds an empty accumulation.
func newWSState() *wsState {
	return &wsState{
		counted:  map[string]*conversationv1.TokenUsage{},
		perModel: map[string]*modelTotals{},
	}
}

// nextSeq mints the next observation order.
func (s *wsState) nextSeq() int {
	s.seq++
	return s.seq
}

// sessionless reports whether this workspace HAS NO SESSION AT ALL — it is
// standing at the cold gate, or no session has started yet. A PARKED session
// is not session-less: it draws as the live idle session it stands in for. It is ONE predicate so a cell can never be drawn as live in one
// of those states and dead in another.
//
// IT IS NOT A WHOLE-VIEW STATE. Under the FIXED SCHEMA ruling the strip has
// one shape; this only decides what each session-scoped cell states, and the
// two named states additionally contribute their reason to the context chip's
// hover.
func (s *wsState) sessionless() bool {
	return s.coldGate || !s.started
}

// controlsWithheld reports whether the three session controls (model,
// permission mode, effort) are withheld. Only a standing cold gate withholds
// them: its answer comes before anything else the workspace does. A workspace
// with no shim otherwise draws the controls it last knew, and a pick starts
// the shim (owner request, 2026-10-29; selections.go).
func (s *wsState) controlsWithheld() bool {
	return s.coldGate
}

// ready reports whether the view can be resolved at all. THE GATE IS THE
// WORKSPACE FACTS AND NOTHING ELSE — the naming (WSM's) and the account (the
// config root's) — under the FIXED SCHEMA ruling of 2026-09-13.
//
// NO SESSION FACT IS EVER A GATE. Every session-scoped cell states "I do not
// know yet" in its own slot: the two controls by ABSENCE, which the client
// draws as a dash, and the context chip by its own content. So a workspace with no session — cold-gated, or simply not
// started — publishes the same strip as every other workspace, and the
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
