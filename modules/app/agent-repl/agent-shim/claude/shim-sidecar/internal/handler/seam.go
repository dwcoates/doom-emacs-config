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
	spawned         func(taskID, toolUseID, agentID, outputPath string, backgrounded bool, workspaceDir, workspaceID, claudeSessionID string)
	stopped         func(taskID string)
	concluded       func(taskID string)
	shellConcluded  func(taskID, status string, atMs int64)
	workspaceDir    string
	workspaceID     string
	claudeSessionID string
}

// TaskSpawned implements convert.Observer.
func (o *seamObserver) TaskSpawned(taskID, toolUseID, agentID, outputPath string, backgrounded bool) {
	if o.spawned == nil {
		return
	}
	o.spawned(taskID, toolUseID, agentID, outputPath, backgrounded, o.workspaceDir, o.workspaceID, o.claudeSessionID)
}

func (o *seamObserver) bind(ctx *Context) {
	o.workspaceDir = ctx.WorkspaceDir
	o.workspaceID = ctx.WorkspaceID
	o.claudeSessionID = ctx.ClaudeSessionID
}

// TaskStopped implements convert.Observer.
func (o *seamObserver) TaskStopped(taskID string) {
	if o.stopped == nil {
		return
	}
	o.stopped(taskID)
}

// TaskConcluded implements convert.Observer.
func (o *seamObserver) TaskConcluded(taskID string) {
	if o.concluded == nil {
		return
	}
	o.concluded(taskID)
}

// ShellConcluded implements convert.Observer.
func (o *seamObserver) ShellConcluded(taskID, status string, atMs int64) {
	if o.shellConcluded == nil {
		return
	}
	o.shellConcluded(taskID, status, atMs)
}

// SetShellConclusionObserver adopts the reader's shell-conclusion sink: the
// vendor's notification that a detached shell run ended travels to the reader,
// which writes the run's one terminal from the run's spool.
func (h *SessionTranscriptHandler) SetShellConclusionObserver(fn func(taskID, status string, atMs int64)) {
	h.obs.shellConcluded = fn
}

// SetShellConclusionObserver adopts the reader's shell-conclusion sink. A
// sidechain's detached shells end on the same terms as a session's.
func (h *AgentTranscriptHandler) SetShellConclusionObserver(fn func(taskID, status string, atMs int64)) {
	h.obs.shellConcluded = fn
}

// SetTaskConclusionObserver adopts the reader's run-concluded sink: a
// backgrounded agent run this transcript settled itself can never be LOST.
func (h *SessionTranscriptHandler) SetTaskConclusionObserver(fn func(taskID string)) {
	h.obs.concluded = fn
}

// SetTaskConclusionObserver adopts the reader's run-concluded sink. A sidechain
// settles the agent runs it launched on the same terms as a session.
func (h *AgentTranscriptHandler) SetTaskConclusionObserver(fn func(taskID string)) {
	h.obs.concluded = fn
}

// SetTaskObserver adopts the reader's spawn-observation sink.
//
// A LAUNCH IS THE ONLY PLACE THE VENDOR STATES WHICH CALL OPENED WHICH SPOOL, and
// only this package reads tool results — so without this the reader cannot claim
// a spool at all and every one of them waits out its hold and goes to residue.
func (h *SessionTranscriptHandler) SetTaskObserver(fn func(taskID, toolUseID, agentID, outputPath string, backgrounded bool, workspaceDir, workspaceID, claudeSessionID string)) {
	h.obs.spawned = fn
}

// SetTaskObserver adopts the reader's spawn-observation sink. A sidechain can
// itself launch detached work, so a subagent's transcript reports launches on the
// same terms as a session's.
func (h *AgentTranscriptHandler) SetTaskObserver(fn func(taskID, toolUseID, agentID, outputPath string, backgrounded bool, workspaceDir, workspaceID, claudeSessionID string)) {
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
// The frame itself is RunOutput's, so it is the identical frame every other
// handler asked for a cancelled terminal spells.
func (h *ShellOutputHandler) CancelTerminal(taskID, run, ownerAgentID string, settledAtMs int64) []*storev1.StoreEntry {
	return h.Cancelled(taskID, run, ownerAgentID, settledAtMs)
}

// NotifiedTerminal spells the vendor's task notification as the run's
// terminal, carrying the output THIS handler has read off the spool. The reader
// asks for it only once the spool has been read to its end after the
// notification and carried no terminator. The frame is RunOutput's, for the
// reason CancelTerminal's is.
func (h *ShellOutputHandler) NotifiedTerminal(taskID, run, ownerAgentID, status string, settledAtMs int64) []*storev1.StoreEntry {
	return h.Notified(taskID, run, ownerAgentID, status, settledAtMs)
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

// LostTerminal spells the reader's LOST conclusion as the detached run's
// terminal. The frame is RunOutput's, for the reason CancelTerminal's is.
func (h *ShellOutputHandler) LostTerminal(taskID, runActivityID, ownerAgentID, reason string, catchup bool) []*storev1.StoreEntry {
	return h.Lost(taskID, runActivityID, ownerAgentID, reason, catchup)
}

// LostTerminal spells the reader's LOST conclusion as the BACKGROUNDED
// SUBAGENT's terminal.
//
// A BACKGROUNDED AGENT IS A DETACHED RUN TOO. Its transcript is delivered
// through an `a*` task spool, and when that spool vanishes or goes quiet the
// reader concludes LOST exactly as it does for a shell spool — but until this
// existed only the shell handler could spell one, so the seam wrote
// `lost-terminal-unsupported` and the SPAWN UNIT stayed open forever in every
// consumer downstream.
//
// WHAT IT SETTLES IS THE SPAWN, NOT THE AGENT'S BOOK. The subagent's own
// constituents are its own book and nothing there is left open by silence; the
// unit that is left open is the CALL that spawned it, `activity:<tool_use_id>`,
// which is a line in the PARENT's book. That is what this upserts, with
// AgentSubagent.failure.cause.lost naming the arm the reader concluded.
//
// The reason is the reader's own vocabulary and convert.DetachedLostArm RAISES
// on one it does not know, rather than leaving the oneof unset.
func (h *AgentTranscriptHandler) LostTerminal(taskID, runActivityID, ownerAgentID, reason string, catchup bool) []*storev1.StoreEntry {
	// THE RUN IS THE SPAWNING CALL AND NOTHING ELSE, for the same reason the
	// shell terminal refuses without one: keying a settle on the vendor task id
	// would name a row no reader of the conversation can join to the call.
	run := runActivityID
	if run == "" {
		h.log.With(logging.Context{Operation: "lost-terminal", Level: "error", TaskID: taskID, AgentID: ownerAgentID}).
			Log("no terminal minted for a LOST subagent: no spawning-call activity id was supplied, so the settle would name no unit (reason=%s)", reason)
		return nil
	}
	if ownerAgentID == "" {
		// The spawn unit is a line in the PARENT's book, and a page line with no
		// book is residue. Refused loudly rather than mis-filed.
		h.log.With(logging.Context{Operation: "lost-terminal", Level: "error", TaskID: taskID, ActivityID: run}).
			Log("no terminal minted for a LOST subagent: no owning agent was supplied, so the spawn unit's settle would name no book (reason=%s)", reason)
		return nil
	}
	at := h.terminalAttribution(taskID, ownerAgentID, run)
	return []*storev1.StoreEntry{h.conv.SubagentLost(at, run, ownerAgentID, convert.LostReason(reason), catchup)}
}

// terminalAttribution states WHERE a reader-concluded subagent terminal is
// written from. It is the shell handler's rule, for the shell handler's reason
// (R-S1): a terminal built with neither a file id nor a run scope would digest
// ONE write identity for every such terminal in the process, and the store —
// whose absorption IS write_id equality — would swallow all but the first.
//
// It NEVER refuses: a refused terminal is a spawn left open forever.
func (h *AgentTranscriptHandler) terminalAttribution(taskID, ownerAgentID, run string) convert.Attribution {
	main := h.mainAgent
	if main == "" {
		// The spool was never read, so nothing told us whose stream carried the
		// spawn. The owner is the agent whose book the spawn unit is a line in,
		// which is the closest true answer available.
		main = ownerAgentID
	}
	if h.coords.FileID == "" {
		h.log.With(logging.Context{
			Operation: "terminal-attribution", TaskID: taskID,
			AgentID: ownerAgentID, ActivityID: run, Path: h.coords.Path,
		}).LogVerbose("this handler read no batch of the subagent's spool, so its terminal is identified by the run")
		return convert.Attribution{
			VendorSessionID: ownerAgentID,
			MainAgentID:     main,
			AgentID:         ownerAgentID,
			TaskID:          taskID,
			WriteScope:      convert.RunScope(run),
		}
	}
	return convert.Attribution{
		VendorSessionID: ownerAgentID,
		MainAgentID:     main,
		AgentID:         ownerAgentID,
		TaskID:          taskID,
		Path:            h.coords.Path,
		FileID:          h.coords.FileID,
		Offset:          h.coords.Offset,
	}
}
