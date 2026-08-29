package tail

import storev1 "agentrepl/proto/store/v1"

// Kind classifies a watched file so the reader picks the right codec + handler.
type Kind int

const (
	KindSessionTranscript Kind = iota // projects/*/<session>.jsonl
	KindAgentTranscript               // .../subagents/agent-*.jsonl (+ a*.output spool)
	KindWorkflowJournal               // .../workflows/wf_*/journal.jsonl (+ w*.output spool)
	KindShellSpool                    // <spool root>/.../tasks/b*.output
	// KindResidueSpool is a spool whose task id carries no a/b/w kind prefix.
	// It is a TOTAL-INGESTION VIOLATION rather than a file to skip: the prefix
	// is how a spool's conversion is selected, so an unrecognized one means the
	// bytes cannot be converted — but they are still ingested, whole, as
	// unparsed residue, because nothing on disk is ever dropped.
	KindResidueSpool
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
	default:
		return "unknown-kind"
	}
}

// Context carries per-file attribution and the tailer-owned cumulative counters
// a handler needs. The tailer fills the counters (RecordsObserved /
// BytesObserved) with the totals THROUGH the current batch before each Handle
// call.
type Context struct {
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
