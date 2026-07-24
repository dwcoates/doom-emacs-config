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
}

// Handler converts a batch of framed records into events (pure; no IO). Layer-2
// implementations live in the handler package; the tailer drives them through
// this interface.
type Handler interface {
	Handle(frames []Frame, ctx *Context) []*corev1.Event
}
