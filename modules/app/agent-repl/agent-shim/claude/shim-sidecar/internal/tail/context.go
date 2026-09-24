package tail

import storev1 "agentrepl/proto/store/v1"

// Kind classifies a watched file so the reader picks the right codec + handler.
type Kind int

const (
	KindSessionTranscript Kind = iota // projects/*/<session>.jsonl
	KindAgentTranscript               // .../subagents/agent-*.jsonl (+ a*.output spool)
	KindWorkflowJournal               // .../workflows/wf_*/journal.jsonl
	KindShellSpool                    // <spool root>/.../tasks/b*.output
	// KindResidueSpool is a spool whose task id carries no a/b/w kind prefix.
	// The prefix is how a spool's conversion is selected, so an unrecognized
	// one means the bytes cannot be converted and nothing could render them:
	// such a spool is discovered, stated, and never read.
	KindResidueSpool
	// KindWorkflowSpool is a w*.output task spool. Workflow is KICKED this wave,
	// so its bytes are ingested WHOLE as declared residue rather than converted
	// — it is not an unrecognized prefix (that is KindResidueSpool) but a
	// recognized one whose conversion deliberately does not exist yet.
	KindWorkflowSpool
)

// String renders a kind for a log record.
func (k Kind) String() string {
	switch k {
	case KindSessionTranscript:
		return "session-transcript"
	case KindAgentTranscript:
		return "agent-transcript"
	case KindWorkflowJournal:
		return "workflow-journal"
	case KindShellSpool:
		return "shell-spool"
	case KindResidueSpool:
		return "residue-spool"
	case KindWorkflowSpool:
		return "workflow-spool"
	default:
		return "unknown-kind"
	}
}

// Context carries per-file attribution and the tailer-owned cumulative counters
// a handler needs. The tailer fills the counters (RecordsObserved /
// BytesObserved) with the totals THROUGH the current batch before each Handle
// call.
type Context struct {
	WorkspaceDir    string
	WorkspaceID     string
	ClaudeSessionID string
	// SessionID is the vendor's session uuid for this file, read from the file
	// PATH (the `<session>.jsonl` basename, or the session directory a subagent
	// transcript sits under). It is an ATTRIBUTE, never an address: the
	// per-record `sessionId` field diverges from it and is never trusted.
	SessionID string
	Path      string // absolute file path (residue evidence + logging)
	Kind      Kind

	// FileID is the file's stable "dev:inode" identity, the cursor's key. It is
	// set from the last poll's stat, so a handler naming a file in a log record
	// names the same identity the store's cursor row does.
	FileID string

	// MainAgentID is the AgentId of the MAIN agent that owns this file's work:
	// the transcript file's own session uuid for a session transcript, and the
	// owning session's main agent for a subagent transcript or a spool.
	MainAgentID string
	// AgentID is the AgentId whose stream this file IS: the main agent for a
	// session transcript, the vendor `agentId` for a subagent transcript.
	// Empty when the file's agent is only knowable from its records.
	AgentID string
	// AgentType is the resolved subagent type this file's agent IS
	// ("general-purpose", a plugin name, "fork", "workflow-subagent"), read from
	// the companion meta file. It is EMPTY for a session transcript, which has
	// no meta.
	//
	// IT IS HOW A QUOTED MESSAGE IS TOLD FROM A PRODUCED ONE. A fork transcript
	// COPIES the parent's conversation ahead of its own work, and each copied
	// assistant record keeps its true producer's type in `attributionAgent`
	// while the fork stamps its OWN records `attributionAgent == AgentType`. A
	// record whose `attributionAgent` names a different type is quoted, not
	// produced here, and must not be re-booked under this agent (convert
	// assistant.go). A session transcript never carries `attributionAgent` at
	// all, so an empty AgentType simply never triggers the check.
	AgentType string
	// SpawnBackgrounded reports that the spawning Agent call ran in the
	// background (run_in_background, or an a* spool exists for it), which is
	// what makes a subagent its own top_level rather than the owning session's
	// main agent.
	SpawnBackgrounded bool

	// MetaPath is a subagent transcript's companion agent-<id>.meta.json — the
	// ONLY source of the agent's type, spawn depth, model and worktree.
	MetaPath string

	// ConfigRoots are the discovery roots in effect, so a handler can state
	// which root a path came from without re-deriving it.
	ConfigRoots []string

	TaskID   string // detached-task id for agent/shell/workflow files
	SpoolDir string // the session's task spool dir
	RunID    string // workflow run id from the journal PATH

	// RunActivityID is the SPAWNING CALL's tool_use_id for a detached spool —
	// the identity a detached run is announced under on BOTH planes, and
	// therefore the only thing a `bash:` frame may be keyed by.
	//
	// THE VENDOR TASK ID IS NOT AN IDENTITY. It names the harness's runtime
	// bookkeeping for the launch, is absent from the stream plane entirely, and
	// keying frames by it would put the spool's output on a row no reader of the
	// conversation can ever join to the call that produced it. The reader
	// resolves it once, from the launch the converter observed, and hands it
	// over here; it is empty exactly while the spool is unclaimed, and a spool
	// is not tailed in that state.
	RunActivityID string

	RecordsObserved int64
	BytesObserved   int64

	// --- the hold ------------------------------------------------------------
	//
	// A record can be UNSETTLED at the end of a batch: its meaning depends on
	// the line that follows it, and that line has not been written yet (the
	// compaction boundary and its summary are written ~1ms apart, so a ~1s poll
	// lands between them now and then). Rather than convert such a record on
	// incomplete evidence, a handler may HOLD it and let the reader hand it back
	// once the file has more to say.
	//
	// THE HOLD IS BOUNDED TO ONE REDELIVERY. A record that is held forever is a
	// record that is never stored, which the total-ingestion mandate forbids —
	// so the second delivery is FORCED and the handler must convert on whatever
	// evidence it has.

	// Redelivers reports whether the reader will deliver held frames AGAIN. The
	// tailer sets it on every Poll, because it can roll its cursor back; a
	// caller handing a handler one standalone batch leaves it false, and the
	// handler must then convert everything it was given.
	Redelivers bool

	// HoldForced is set by the tailer on the REDELIVERY of a held frame. The
	// handler MUST convert the frame on this delivery: a hold that survives it
	// is refused and the cursor advances past the frame regardless.
	HoldForced bool

	// HeldOffset is the file offset of the first frame the handler held, and
	// HeldDeliveries counts the consecutive deliveries that same offset has been
	// held for (0 = nothing held). The HANDLER writes both on every Handle call;
	// the tailer reads them afterwards to roll its cursor back to HeldOffset.
	HeldOffset     int64
	HeldDeliveries int
}

// Handler converts a batch of framed records into stored records (pure; no IO).
// Layer-2 implementations live in the handler package; the tailer drives them
// through this interface.
type Handler interface {
	Handle(frames []Frame, ctx *Context) []*storev1.StoreEntry
}
