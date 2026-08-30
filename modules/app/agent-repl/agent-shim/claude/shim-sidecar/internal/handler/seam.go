package handler

// seam.go — the CONVERTER'S half of the boundary the reader declares.
//
// The reader (the root package) states WHICH FILES exist, WHERE it has read to,
// WHO owns a spool and WHEN it stopped seeing a run. This side states WHAT a
// record means. The two optional interfaces the reader looks for take plain
// function and string arguments so neither package imports the other's types —
// so these methods are deliberately thin adapters onto the typed conversion.

import (
	storev1 "agentrepl/proto/store/v1"
	"agentrepl/shim-claude-sidecar/internal/convert"
	"agentrepl/shim-claude-sidecar/internal/logging"
)

// seamObserver adapts the reader's plain callbacks to the converter's typed
// Observer, which is the interface the conversion side actually reports through.
//
// ONE ADAPTER HOLDING BOTH CALLBACKS, because the reader adopts them
// independently — a caller may want spawn observations and not stops — and a
// converter has exactly one observer. An unadopted callback is a reader that
// asked for nothing, which is silence rather than a defect; the reader states
// its own expectations at the plumbing site.
type seamObserver struct {
	spawned func(taskID, toolUseID, agentID, outputPath string)
	stopped func(taskID string)
}

// TaskSpawned implements convert.Observer.
func (o *seamObserver) TaskSpawned(taskID, toolUseID, agentID, outputPath string) {
	if o.spawned == nil {
		return
	}
	o.spawned(taskID, toolUseID, agentID, outputPath)
}

// TaskStopped implements convert.Observer.
func (o *seamObserver) TaskStopped(taskID string) {
	if o.stopped == nil {
		return
	}
	o.stopped(taskID)
}

// SetTaskObserver adopts the reader's spawn-observation sink.
//
// A LAUNCH IS THE ONLY PLACE THE VENDOR STATES WHICH CALL OPENED WHICH SPOOL, and
// only this package reads tool results — so without this the reader cannot claim
// a spool at all and every one of them waits out its hold and goes to residue.
func (h *SessionTranscriptHandler) SetTaskObserver(fn func(taskID, toolUseID, agentID, outputPath string)) {
	h.obs.spawned = fn
}

// SetTaskObserver adopts the reader's spawn-observation sink. A sidechain can
// itself launch detached work, so a subagent's transcript reports launches on the
// same terms as a session's.
func (h *AgentTranscriptHandler) SetTaskObserver(fn func(taskID, toolUseID, agentID, outputPath string)) {
	h.obs.spawned = fn
}

// SetTaskStopObserver adopts the reader's task-stop sink.
//
// A CANCELLED SHELL RUN'S TERMINAL OWES THE OUTPUT THE SPOOL HOLDS, and this
// converter reads transcripts rather than spools — so the stop travels to the
// reader, which mints the terminal through the spool's own handler.
func (h *SessionTranscriptHandler) SetTaskStopObserver(fn func(taskID string)) {
	h.obs.stopped = fn
}

// SetTaskStopObserver adopts the reader's task-stop sink. A sidechain can stop
// detached work it launched itself, on the same terms as a session.
func (h *AgentTranscriptHandler) SetTaskStopObserver(fn func(taskID string)) {
	h.obs.stopped = fn
}

// CancelTerminal spells a person's stop as the run's terminal, carrying the
// output THIS handler has read off the spool.
//
// IT IS THE SAME SEAM AS LostTerminal, for the same reason: the reader learns
// the fact (from another file's records, or from its own staleness policy) and
// only this side can spell it, because only this side holds the run's bytes.
func (h *ShellOutputHandler) CancelTerminal(taskID, run, ownerAgentID string, settledAtMs int64) []*storev1.StoreEntry {
	if run == "" {
		h.log.With(logging.Context{Operation: "cancel-terminal", Level: "error", TaskID: taskID, AgentID: ownerAgentID}).
			Log("no terminal minted for a stopped run: no spawning-call activity id was supplied, so the frame would name no unit")
		return nil
	}
	at, ok := h.terminalAttribution(taskID, ownerAgentID, "cancel-terminal", "stopped")
	if !ok {
		return nil
	}
	return []*storev1.StoreEntry{h.conv.BashCancelled(at, run, string(h.seen), h.omitted, settledAtMs)}
}

// SetTerminalObserver adopts the reader's terminal-read sink.
//
// THE FILE IS THE ONLY PLACE A DETACHED RUN'S END IS WRITTEN, and only this
// package reads it — so without this the reader has no way to tell a run that
// finished from one it merely stopped hearing from, and every completed run
// would eventually be restated LOST by the staleness sweep.
func (h *ShellOutputHandler) SetTerminalObserver(fn func(path, run string)) {
	h.onTerminal = fn
}

// LostTerminal spells the reader's LOST conclusion as the detached run's terminal.
//
// LOST IS ITS OWN WORD — "we stopped seeing it", not "known failed" — and the
// wire carries no DetachedLost message this wave, so the run resolves as
// AgentBash.success.interrupted with NO cause arm set. Setting `by_user` or
// `timed_out` would be an accusation with no evidence, so HOW we concluded it
// survives in the log record this writes and nowhere else. That is a contract
// gap, stated rather than papered over.
//
// The reason is the reader's own vocabulary (file_vanished | went_silent |
// swept_up); an unrecognized one is carried through rather than remapped, because
// silently normalizing it would lose the only account of what was observed.
func (h *ShellOutputHandler) LostTerminal(taskID, runActivityID, ownerAgentID, reason string) []*storev1.StoreEntry {
	// THE RUN IS THE SPAWNING CALL AND NOTHING ELSE. Falling back to the vendor
	// task id would key the terminal on a row no reader of the conversation can
	// join to the call, which is worse than saying nothing: the run would appear
	// settled while the call it belongs to stayed open forever.
	run := runActivityID
	if run == "" {
		// Nothing to name the run by: the terminal would upsert no row. Refused
		// loudly rather than emitted against an invented key.
		h.log.With(logging.Context{Operation: "lost-terminal", Level: "error", TaskID: taskID, AgentID: ownerAgentID}).
			Log("no terminal minted for a LOST run: no spawning-call activity id was supplied, so the frame would name no unit (reason=%s)", reason)
		return nil
	}
	at, ok := h.terminalAttribution(taskID, ownerAgentID, "lost-terminal", "LOST")
	if !ok {
		return nil
	}
	return []*storev1.StoreEntry{h.conv.BashLost(at, run, string(h.seen), h.omitted, convert.LostReason(reason))}
}

// terminalAttribution states WHERE a reader-concluded terminal is written from.
//
// THE FILE COORDINATES ARE NOT DECORATION: the write identity is the digest of
// "producer|file_id|offset|discriminator" (R-S1), so a terminal built without
// them digests the SAME id for every run in the process and the store absorbs
// the second one as a replay of the first, losing a run's only terminal. The
// coordinates are the ones this handler last read at, which are the same ones
// the cursor is stated in.
//
// A HANDLER THAT HAS READ NOTHING HAS NO POSITION TO STATE, and it refuses
// loudly rather than minting a terminal at a coordinate it invented.
func (h *ShellOutputHandler) terminalAttribution(taskID, ownerAgentID, operation, what string) (convert.Attribution, bool) {
	if h.coords.FileID == "" {
		h.log.With(logging.Context{
			Operation: operation, Level: "error", TaskID: taskID,
			AgentID: ownerAgentID, Path: h.coords.Path,
		}).Log("no terminal minted for a %s run: this handler has read no batch of its spool, so it has no file position to state the terminal at and every such terminal would share one write identity", what)
		return convert.Attribution{}, false
	}
	return convert.Attribution{
		VendorSessionID: ownerAgentID,
		MainAgentID:     ownerAgentID,
		AgentID:         ownerAgentID,
		TaskID:          taskID,
		Path:            h.coords.Path,
		FileID:          h.coords.FileID,
		Offset:          h.coords.Offset,
	}, true
}
