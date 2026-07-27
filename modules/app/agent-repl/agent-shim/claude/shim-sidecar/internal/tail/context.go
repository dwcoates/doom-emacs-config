package tail

import corev1 "agentrepl/proto/agentshim/core/v1"

// Kind classifies a watched file so the tailer picks the right codec + handler.
type Kind int

const (
	KindSessionTranscript Kind = iota // projects/*/<session>.jsonl
	KindAgentTranscript               // .../subagents/agent-*.jsonl (+ a*.output spool)
	KindWorkflowJournal               // .../workflows/wf_*/journal.jsonl (+ w*.output spool)
	KindShellSpool                    // /tmp .../tasks/b*.output
)

// Context carries per-file attribution and the tailer-owned cumulative counters a
// handler needs. The tailer fills the counters (RecordsObserved / BytesObserved)
// with the totals THROUGH the current batch before each Handle call.
type Context struct {
	SessionID string // attribution from the file PATH
	Path      string // absolute file path (UnparsedEvent evidence + logging)
	Kind      Kind

	TaskID   string // detached-task id for agent/shell/workflow files
	SpoolDir string // session's /tmp task spool dir (shell output_path construction)
	RunID    string // workflow run id from the journal PATH (wf dedup-key root)

	RecordsObserved int64
	BytesObserved   int64

	// --- deferred frames -----------------------------------------------------
	//
	// A record can be UNSETTLED at the end of a batch: its meaning depends on
	// the line that follows it, and that line has not been written yet (the
	// session transcript's compaction boundary and its summary are written
	// ~1ms apart, so a ~1s poll lands between them now and then). Rather than
	// convert such a record on incomplete evidence, a handler may DEFER it and
	// let the reader hand it back once the file has more to say.

	// Redelivers reports whether the reader will deliver deferred frames AGAIN.
	// The tailer sets it on every Poll, because it can roll its cursor back; a
	// caller handing a handler one standalone batch leaves it false, and the
	// handler must then convert everything it was given — deferring would drop
	// the record for good, which is never allowed.
	Redelivers bool

	// HeldOffset is the file offset of the first frame the handler deferred, and
	// HeldDeliveries counts the consecutive deliveries that same offset has been
	// deferred for (0 = nothing deferred). The HANDLER writes both on every
	// Handle call; the tailer reads them afterwards to roll its cursor back to
	// HeldOffset, and leaves them in place so the next Handle can see how long
	// it has been holding and bound the wait.
	HeldOffset     int64
	HeldDeliveries int
}

// Handler converts a batch of framed records into events (pure; no IO). Layer-2
// implementations live in the handler package; the tailer drives them through
// this interface.
type Handler interface {
	Handle(frames []Frame, ctx *Context) []*corev1.Event
}
