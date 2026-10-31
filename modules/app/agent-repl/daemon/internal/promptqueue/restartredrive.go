package promptqueue

import (
	"context"

	conversationv1 "agentrepl/proto/conversation/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/sessionwatcher"
	"agent-repld/internal/wsm"
)

// A TURN THE DAEMON'S OWN STAND-DOWN CUT IS RESUMED ON THE NEXT SESSION THAT
// SERVES ITS WORKSPACE (owner requirement, 2026-10-12;
// PROMPT_ORIGIN_RESUME_AFTER_RESTART). A bounce or an immediate shutdown kills
// every shim, and the vendor ends each running turn as interrupted; nothing
// the user did ended it, so the work is owed back. The vendor SDK has no
// resume-the-aborted-turn primitive (`resume`, `continue` and
// `resumeSessionAt` restore CONTEXT only), so the resumption is an
// instruction submitted into the restored conversation.
//
// THE DEBT IS RECORDED WHERE THE TURN CLOSES. Every live close comes through
// OnTurnEnded; one that closes INTERRUPTED while the supervisor's stand-down
// latch stands is a cut this daemon ordered, and it is owed. Recording at the
// close rather than at the stand-down's walk catches a turn that opened
// behind the cut one in the instant before its shim went (the queued turn the
// 2026-10-12 hard bounce cut twice over), and leaves every interrupt the user
// asked for, which closes while no stand-down stands, owing nothing.
//
// THE DEBT IS PAID WHEN A SESSION SERVES, FIRST. The release that runs on
// every session-up edge claims what the workspace is owed BEFORE it delivers
// anything held, so the resumed work runs before the prompts that waited out
// the restart, which deliver on its end through the ordinary path. The claim
// comes before the delivery and the database arbitrates it, so a second boot
// or a second session-up edge resumes nothing twice.
//
// AN OPEN WAITS FOR IT. Every release reports what it did about the debt
// (Deps.Resumptions: starting, started, failed, not now), and an editor's
// OpenTabs that brought the workspace up answers it `ready` only once the
// resumed turn has started (internal/opentabs).
//
// IT IS UNDER THE HOOD. The instruction is the daemon's, not the user's: the
// feed draws no prompt row for this origin (feed.drawAgentPrompt and the
// queue's own mirror skip it), and the turn it opens shows only as the status
// any running turn shows.

// ResumeKind is what one session-up release did about the resumption a
// stand-down owes the workspace.
type ResumeKind int

const (
	// ResumeStarting is a resumption claimed and being delivered.
	ResumeStarting ResumeKind = iota + 1
	// ResumeStarted is the claimed resumption delivered: its turn started.
	ResumeStarted
	// ResumeFailed is a resumption that could not be claimed or delivered.
	ResumeFailed
	// ResumeNotNow is a release that resumed nothing: nothing was owed, or
	// what is owed waits for a later session-up (a turn already runs, the
	// session does not serve, the vendor refuses it).
	ResumeNotNow
)

// String names the kind for the records.
func (k ResumeKind) String() string {
	switch k {
	case ResumeStarting:
		return "starting"
	case ResumeStarted:
		return "started"
	case ResumeFailed:
		return "failed"
	case ResumeNotNow:
		return "not_now"
	default:
		return "unknown"
	}
}

// ResumeReport is one release's account of the owed resumption.
type ResumeReport struct {
	Kind ResumeKind
	// Text is a failure's cause, or why nothing was resumed now.
	Text string
}

// reportResume tells Deps.Resumptions one release's account.
func (q *queue) reportResume(ws ids.WorkspaceID, kind ResumeKind, text string) {
	if q.deps.Resumptions == nil {
		return
	}
	q.deps.Resumptions(ws, ResumeReport{Kind: kind, Text: text})
}

// resumptionInstruction is the text the resumed turn is driven with (the
// pre-overhaul session controller's, 184d84fcd). It names the situation
// rather than restating the task, which the restored context already holds.
const resumptionInstruction = "Your previous turn was interrupted by a planned restart of the tooling that hosts this session, not by the user. Continue that interrupted work from where it stopped. Do not restate or re-plan it, and do not ask the user to repeat themselves."

// opResume is the resumption's record operation.
const opResume = "daemon.promptqueue.resume_after_restart"

// oweResumption records the resumption a stand-down owes when a live turn
// closes interrupted while the daemon stands down.
func (q *queue) oweResumption(ctx context.Context, ws ids.WorkspaceID, turn ids.TurnID, how sessionwatcher.TurnClose, log dlog.Logger) {
	if how != wsm.CloseKilled || !q.deps.StandingDown() {
		return
	}
	fields := dlog.Context{"turn": string(turn)}
	if err := q.deps.DB.OweResumption(ctx, ws, turn, q.deps.Now()); err != nil {
		log.Error(opResume, "could not record the resumption the stand-down owes; the cut turn will not be resumed",
			dlog.Merge(fields, dlog.Context{"cause": err.Error()}))
		return
	}
	log.Info(opResume, "the daemon's own stand-down cut the turn; the next session that serves the workspace resumes it", fields)
}

// resumeShimCrash answers how a turn its shim's death ended is closed.
//
// A SHIM THAT DIES UNDER A TURN CUT IT (owner ruling, 2026-10-30): the work
// was not the turn's own failure, so the turn closes as killed, which every
// surface draws "interrupted", and the workspace is owed its resumption, which
// the revived session pays with the "continue" prompt. The death itself stays
// loud: the shim's exit is recorded at ERROR where the daemon sees it.
//
// A RESUMPTION THAT DIES IS NOT RESUMED AGAIN. A shim that crashes on the
// same work would otherwise be revived into it, crash, and be revived again
// forever, so a dead turn of origin PROMPT_ORIGIN_RESUME_AFTER_RESTART keeps
// its agent-died close, which draws the failure for the user to act on.
// Every close that is not a shim's death passes through unchanged.
func (q *queue) resumeShimCrash(ctx context.Context, ws ids.WorkspaceID, turn ids.TurnID, how sessionwatcher.TurnClose, log dlog.Logger) sessionwatcher.TurnClose {
	if how != wsm.CloseAgentDied {
		return how
	}
	row, err := q.deps.DB.Turn(ctx, ws, turn)
	if err != nil {
		log.Error(opResume, "could not read the turn the shim's death cut; it is not resumed and its failure stands",
			dlog.Context{"cause": err.Error()})
		return how
	}
	if row.Origin == conversationv1.PromptOrigin_PROMPT_ORIGIN_RESUME_AFTER_RESTART.String() {
		// ONE DEATH IS ONE ERROR: the shim's exit is recorded at ERROR where
		// the daemon sees it, so this decision about it is INFO.
		log.Info(opResume, "the shim died again under the resumption of work a cut ended; it is not resumed again, and its failure stands", nil)
		return how
	}
	if err := q.deps.DB.OweResumption(ctx, ws, turn, q.deps.Now()); err != nil {
		log.Error(opResume, "could not record the resumption the shim's death owes; the cut turn will not be resumed and its failure stands",
			dlog.Context{"cause": err.Error()})
		return how
	}
	log.Info(opResume, "the shim died under the turn; it ends interrupted and the revived session resumes it", nil)
	return wsm.CloseKilled
}

// redriveOwedLocked resumes what the workspace is owed, as the first turn the
// serving session runs, and reports whether it started one. The caller holds
// the delivery lock (d), has seen the session serve and its facts in.
func (q *queue) redriveOwedLocked(ctx context.Context, d *delivery, log dlog.Logger) bool {
	ws := d.ws
	if running, ok := q.runningTurn(ws); ok {
		log.Debug(opResume, "a turn runs on the serving session; anything owed waits for the next session-up", dlog.Context{"running": string(running)})
		q.reportResume(ws, ResumeNotNow, "a turn already runs on the serving session")
		return false
	}
	owed, err := q.deps.DB.ClaimResumptions(ctx, ws)
	if err != nil {
		log.Error(opResume, "could not claim the resumptions the workspace is owed", dlog.Context{"cause": err.Error()})
		q.reportResume(ws, ResumeFailed, "could not claim the resumption owed: "+err.Error())
		return false
	}
	if owed == 0 {
		q.reportResume(ws, ResumeNotNow, "nothing is owed")
		return false
	}
	q.reportResume(ws, ResumeStarting, "")
	sender, ok := q.deps.Client(ws)
	if !ok {
		log.Error(opResume, "the session serves but has no shim client; the claimed resumption is lost", dlog.Context{"owed": owed})
		q.reportResume(ws, ResumeFailed, "the session serves but has no shim client")
		return false
	}
	watcher, ok := q.deps.Watcher(ws)
	if !ok {
		log.Error(opResume, "the session serves but has no watcher; the claimed resumption is lost", dlog.Context{"owed": owed})
		q.reportResume(ws, ResumeFailed, "the session serves but has no watcher")
		return false
	}
	sub := Submission{WS: ws, Turn: wsm.NewTurnID(), Said: textSaid(resumptionInstruction),
		Origin: conversationv1.PromptOrigin_PROMPT_ORIGIN_RESUME_AFTER_RESTART}
	disposition, err := q.deliver(ctx, d, sub, sender, watcher, log)
	if err != nil {
		log.Error(opResume, "could not deliver the resumption of the work the stand-down cut", dlog.Context{"owed": owed, "cause": err.Error()})
		q.reportResume(ws, ResumeFailed, "could not deliver the resumption: "+err.Error())
		return false
	}
	if !disposition.Delivered {
		log.Error(opResume, "the serving session took no turn for the resumption; the cut work is not resumed", dlog.Context{"owed": owed})
		q.reportResume(ws, ResumeFailed, "the serving session took no turn for the resumption")
		return false
	}
	log.Info(opResume, "resumed the work the stand-down cut, with no prompt shown", dlog.Context{"owed": owed, "turn": string(sub.Turn)})
	q.reportResume(ws, ResumeStarted, "")
	return true
}

// textSaid composes the canonical prompt form of plain text.
func textSaid(text string) *conversationv1.UserSaid {
	return &conversationv1.UserSaid{Content: &conversationv1.UserContent{
		Blocks: []*conversationv1.UserContentBlock{{
			Block: &conversationv1.UserContentBlock_Text{Text: &conversationv1.TextBlock{Text: text}},
		}},
	}}
}
