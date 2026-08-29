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

// taskObserverFunc adapts the reader's plain callback to the converter's typed
// Observer, which is the interface the conversion side actually reports through.
type taskObserverFunc func(taskID, toolUseID, agentID, outputPath string)

// TaskSpawned implements convert.Observer.
func (f taskObserverFunc) TaskSpawned(taskID, toolUseID, agentID, outputPath string) {
	f(taskID, toolUseID, agentID, outputPath)
}

// SetTaskObserver adopts the reader's spawn-observation sink.
//
// A LAUNCH IS THE ONLY PLACE THE VENDOR STATES WHICH CALL OPENED WHICH SPOOL, and
// only this package reads tool results — so without this the reader cannot claim
// a spool at all and every one of them waits out its hold and goes to residue.
func (h *SessionTranscriptHandler) SetTaskObserver(fn func(taskID, toolUseID, agentID, outputPath string)) {
	h.conv.SetObserver(taskObserverFunc(fn))
}

// SetTaskObserver adopts the reader's spawn-observation sink. A sidechain can
// itself launch detached work, so a subagent's transcript reports launches on the
// same terms as a session's.
func (h *AgentTranscriptHandler) SetTaskObserver(fn func(taskID, toolUseID, agentID, outputPath string)) {
	h.conv.SetObserver(taskObserverFunc(fn))
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
	at := convert.Attribution{
		VendorSessionID: ownerAgentID,
		MainAgentID:     ownerAgentID,
		AgentID:         ownerAgentID,
		TaskID:          taskID,
	}
	return []*storev1.StoreEntry{h.conv.BashLost(at, run, string(h.seen), h.omitted, convert.LostReason(reason))}
}
