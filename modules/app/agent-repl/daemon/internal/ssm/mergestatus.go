package ssm

import (
	frontendv1 "agentrepl/proto/agentshim/frontend/v1"
)

// This file carries the merge PIPELINE's own MergeStatus onto WorkspaceState.
//
// THERE IS NO PROJECTION OVER THE STATE LOG ANY MORE, and its absence is the
// contract rather than a gap. A MergeStatus names a RUN — `run_id` is the field
// every other one hangs off — and the state log has no run identity in it. The
// wave-0 projection stood in with `<workspace>@<instant>`, which changed on
// every phase transition, so one run published a different id at each step and a
// frontend correlating on the field blended and split runs at random. A status
// stamped with an id nothing minted is worse than no status, so merge_status
// stays UNSET until the run that owns it publishes. The merge AXIS still
// resolves the frame's render state in the meantime, so a merge in flight is
// never invisible — only its per-run progress is withheld.
//
// It is the same rule merge.QueueCoordinator already applies to a merge it fails
// before any run exists (the boot sweep of orphaned merge_enqueuing marks): no
// run, no status, never a fabricated identity.

// recordPipelineStatusLocked retains the status the merge PIPELINE published for
// a workspace, so every later frame carries the run's own account of itself
// rather than nothing. Caller holds mu.
//
// IT IS IN MEMORY, AND DELIBERATELY SO. A MergeStatus describes a run's
// PROGRESS, and progress does not survive the process that was making it: a
// daemon that dies mid-merge leaves a durable queue entry the next boot replays,
// and that replay resumes the run's IDENTITY (persisted with the entry) while
// republishing its progress from what the resumed run actually observes.
// Persisting the old process's commit cursor would hand a frontend figures
// nothing is advancing.
func (m *Manager) recordPipelineStatusLocked(workspace string, status *frontendv1.MergeStatus) {
	if m.pipelineStatus == nil {
		m.pipelineStatus = make(map[string]*frontendv1.MergeStatus)
	}
	m.pipelineStatus[workspace] = status
}

// clearPipelineStatusLocked drops a workspace's retained run status. It is called
// when the merge axis is cleared: the run is over and nothing about it is true
// any more. Caller holds mu.
func (m *Manager) clearPipelineStatusLocked(workspace string) {
	delete(m.pipelineStatus, workspace)
}

// stampMergeStatusLocked writes merge_status onto a WorkspaceState about to be
// pushed. Caller holds mu.
//
// IT RIDES THE SAME CONSTRUCTION FUNNEL the rest of the merge facts do
// (stampMergeFactsLocked, called from workspaceMessageLocked and nowhere else),
// so a frame carrying a merge_status the pipeline did not publish is
// unrepresentable rather than merely unlikely.
//
// ABSENCE IS THE ABSENCE OF A PUBLISHED RUN. A workspace whose merge axis has
// never spoken, whose axis was cleared, or whose run belongs to a process that
// is gone gets no status at all — never a zero-valued one, and never one keyed
// to an invented run.
// resolvedMergePhase is the merge AXIS's own verdict for this frame, threaded
// in from the resolution rather than read back off the message. It used to be
// read back off msg.GetMergePhase(); that wire field is retired, but the axis
// behind it is untouched and is still what a retained status must agree with,
// so the check reads the resolution directly instead of being dropped.
//
// THE FRAME'S OWN RENDER STATE IS THE SECOND HALF OF THAT AGREEMENT, and the
// axis alone was never enough to establish it. The merge axis is not the last
// word on the render state: the composite resolution hands a live turn the win
// over `merge_failed` on purpose (see compositeRenderState — a terminal merge
// owns nothing, so it may not mask a turn the user just started), and a
// workspace whose merge stopped therefore keeps a merge row standing forever
// while its session goes on working. Stamping off the axis alone published
// exactly that contradiction: state=THINKING with merge_status=failed, which
// every surface reading the status renders in preference to the phase word. The
// footer said "merge failed" and the sidebar showed the failure's cause while
// the agent answered the prompt underneath them.
//
// So a merge_status does not ride a frame that reports the LIVE AGENT. The wire
// then cannot carry a merge account of a workspace the same frame says is
// thinking, and every surface that draws from merge_status is cleared by the
// ordinary next frame rather than needing its own rule for when a merge stops
// being the news.
//
// IT IS THE SESSION-STATUS BAND SPECIFICALLY, not "any non-merge state", and it
// asks the SAME classifier the composite builder asks when it decides the frame's
// cause and turn flag came from the status axis (isSessionStatusRenderState). A
// merging workspace with no session behind it resolves HIBERNATED, SEVERED or
// INIT — connectivity verdicts that say nothing about the merge and routinely
// stand over a run in flight, because a merge does not need a live shim to
// cherry-pick (the whole acceptance gate runs against workspaces in exactly that
// shape). Withholding there would delete the status of every merge that matters
// most.
func (m *Manager) stampMergeStatusLocked(workspace, resolvedMergePhase string, msg *frontendv1.WorkspaceState) {
	status, ok := m.pipelineStatus[workspace]
	if !ok {
		return
	}
	if resolvedMergePhase == "" {
		// The retained run says the workspace is merging and the resolution says
		// it is not. Stamping the status anyway would push a frame whose merge
		// status contradicts the axis that resolved the frame's render state, so
		// the disagreement is reported and the resolution wins.
		m.logf("ssm: INVARIANT VIOLATION ws=%s has a retained pipeline merge_status (run=%s) with NO resolved merge axis behind it — merge_status is left unset",
			workspace, status.GetRunId())
		return
	}
	if isSessionStatusRenderState(msg.GetState()) {
		// ORDINARY, NOT A VIOLATION. The axis still rests on a merge row and the
		// frame reports the agent instead, which is precisely what the composite
		// resolution does when a live turn outranks a merge run that stopped. The
		// run is retained — it is still the account of that merge, and it rides
		// the next frame that reports the merge again — and it is this frame that
		// has no business carrying it.
		m.logf("ssm: merge_status WITHHELD ws=%s run=%s axis=%s state=%s — the frame reports the live agent rather than the merge, so the merge run is not what this frame is about",
			workspace, status.GetRunId(), resolvedMergePhase, msg.GetState())
		return
	}
	msg.MergeStatus = status
}
