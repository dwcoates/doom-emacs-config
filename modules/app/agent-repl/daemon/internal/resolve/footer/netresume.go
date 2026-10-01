package footer

import (
	"sort"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
)

// A BACKGROUND SUBAGENT WAITING TO BE RESUMED AFTER A NETWORK OUTAGE
// (footer-activity-tiers.md, landed change 2).
//
// THE CHANGE IS VISIBILITY ONLY (owner ruling, 2026-09-28). The shim states the
// standing set of waits (`SessionUpdate.network_resume_waits`) and one outcome
// per ended wait (`network_resume_outcome`); the footer draws them in exactly
// three places and nowhere else:
//
//   - the agents panel keeps the waiting agent's ROW, drawn `waiting_for_api`,
//     although the agent's failed run has ended;
//   - the agents chip counts that row and carries the waiting glyph;
//   - each edge of a wait raises the `network_resume` transient, labelled with
//     the subagent.
//
// NO OTHER RULE READS A WAIT. The waits live in their own field, never in
// `agents` or the watcher's live-work set, so the `background` status, the
// close-quiet check and the deploy's drain — every one of which reads the
// live-work authority — cannot see them.
//
// THE ROW DATA OUTLIVES THE RUN. The agent's failure terminal reaches the
// footer BEFORE the shim opens the wait, and it retires the row; so every
// retired detached row is remembered (`retiredRows`), and a wait draws the row
// it names from there.

// resumeWait is one standing wait, as the shim last stated it.
type resumeWait struct {
	// work is the failed run's detached-work handle.
	work string
	// failedAt is when the network failure that opened the wait was reported.
	failedAt time.Time
	// givesUpAt is when the shim gives up waiting.
	givesUpAt time.Time
	// resumes counts the resumes already delivered for this agent.
	resumes uint32
}

// observeResumeWaits takes the shim's STANDING SET of waits, whole. A wait the
// set lists for the first time raises the `waiting` edge. A statement naming a
// wait with no work handle is a producer defect: it is refused WHOLE at ERROR
// and the set on hand stands, so no half-applied set is ever drawn.
func (r *resolver) observeResumeWaits(ws ids.WorkspaceID, s *wsState, stated *conversationv1.SessionNetworkResumeWaits) {
	next := make([]resumeWait, 0, len(stated.GetWaits()))
	for i, wait := range stated.GetWaits() {
		work := wait.GetWork().GetValue()
		if work == "" {
			r.logOf(ws, s).Error("daemon.footer.network_resume_waits_refused",
				"a network-resume wait names no work; the statement is refused and the waits on hand stand",
				dlog.Context{
					"index":               i,
					"waits":               len(stated.GetWaits()),
					"invariant_violation": "every network-resume wait names the work whose run failed",
				})
			return
		}
		next = append(next, resumeWait{
			work:      work,
			failedAt:  time.UnixMilli(wait.GetFailedAtMs()),
			givesUpAt: time.UnixMilli(wait.GetGivesUpAtMs()),
			resumes:   wait.GetResumesDelivered(),
		})
	}
	previous := s.resumeWaits
	s.resumeWaits = next
	opened := []string{}
	for _, wait := range next {
		if waitFor(previous, wait.work) != nil {
			continue
		}
		opened = append(opened, wait.work)
		r.describeUnknownWait(ws, s, wait)
		r.raiseTransient(ws, s, s.waitingAgentLabel(wait.work), &frontendv1.FooterActivityTransient{
			Kind: &frontendv1.FooterActivityTransient_NetworkResume{NetworkResume: &frontendv1.FooterActivityTransientNetworkResume{
				Edge: &frontendv1.FooterActivityTransientNetworkResume_Waiting{
					Waiting: &frontendv1.FooterActivityTransientNetworkResumeWaiting{GivesUpAtMs: epochMs(wait.givesUpAt)}}}},
		})
	}
	r.logOf(ws, s).Info("daemon.footer.network_resume_waits",
		"the footer took the standing network-resume waits",
		dlog.Context{"waits": waitWorks(next), "opened": opened, "previous": waitWorks(previous)})
}

// describeUnknownWait gives a wait for work the footer never described (a
// daemon that came up mid-wait learns the set before any frame of the agent) a
// MINIMAL description — the generic label, no description, no tokens, its
// clock from the failure — kept with the retired rows so it is one row for as
// long as the wait stands. It is recorded so a bare row is explained by the
// log alone. WHETHER IT IS DRAWN is drawsWork's (owner.go): no source has
// stated whose work it is, so it is drawn only once one does.
func (r *resolver) describeUnknownWait(ws ids.WorkspaceID, s *wsState, wait resumeWait) {
	if s.waitingRow(wait.work) != nil {
		return
	}
	s.retiredRows[wait.work] = &agentRow{
		work: wait.work, spawnUnit: wait.work, createdAgent: wait.work,
		label: subagentLabel(nil), startedAt: wait.failedAt,
		order: s.nextOrder(), provenance: provenanceResumeWait,
	}
	r.logOf(ws, s).Info("daemon.footer.network_resume_row_minimal",
		"a network-resume wait names work the footer never described; its row is described minimally",
		dlog.Context{"work": wait.work})
}

// observeResumeOutcome takes one ended wait and raises its edge. An outcome with
// no work or no arm is a producer defect, recorded at ERROR, and raises
// nothing. The wait itself leaves with the next standing set, which the shim
// states when a wait ends.
func (r *resolver) observeResumeOutcome(ws ids.WorkspaceID, s *wsState, outcome *conversationv1.SessionNetworkResumeOutcome) {
	work := outcome.GetWork().GetValue()
	edge := &frontendv1.FooterActivityTransientNetworkResume{}
	arm := ""
	switch o := outcome.GetOutcome().(type) {
	case *conversationv1.SessionNetworkResumeOutcome_Resumed:
		arm = "resumed"
		edge.Edge = &frontendv1.FooterActivityTransientNetworkResume_Resumed{
			Resumed: &frontendv1.FooterActivityTransientNetworkResumeResumed{}}
	case *conversationv1.SessionNetworkResumeOutcome_GaveUp:
		arm = "gave_up"
		edge.Edge = &frontendv1.FooterActivityTransientNetworkResume_GaveUp{
			GaveUp: &frontendv1.FooterActivityTransientNetworkResumeGaveUp{}}
	case *conversationv1.SessionNetworkResumeOutcome_Abandoned:
		arm = "abandoned"
		edge.Edge = &frontendv1.FooterActivityTransientNetworkResume_Abandoned{
			Abandoned: &frontendv1.FooterActivityTransientNetworkResumeAbandoned{Reason: o.Abandoned.GetReason()}}
	}
	if work == "" || arm == "" {
		r.logOf(ws, s).Error("daemon.footer.network_resume_outcome_refused",
			"a network-resume outcome names no work or no outcome; nothing is announced",
			dlog.Context{
				"work":                work,
				"outcome":             arm,
				"invariant_violation": "every network-resume outcome names its work and how its wait ended",
			})
		return
	}
	r.logOf(ws, s).Info("daemon.footer.network_resume_outcome", "a network-resume wait ended",
		dlog.Context{"work": work, "outcome": arm})
	r.raiseTransient(ws, s, s.waitingAgentLabel(work), &frontendv1.FooterActivityTransient{
		Kind: &frontendv1.FooterActivityTransient_NetworkResume{NetworkResume: edge},
	})
}

// waitFor answers the wait for this work in a set, or nil.
func waitFor(waits []resumeWait, work string) *resumeWait {
	for i := range waits {
		if waits[i].work == work {
			return &waits[i]
		}
	}
	return nil
}

// waitWorks is a set's work handles, for the record.
func waitWorks(waits []resumeWait) []string {
	out := make([]string, 0, len(waits))
	for _, w := range waits {
		out = append(out, w.work)
	}
	return out
}

// waitingRow answers the row a wait draws: the LIVE row addressed by the work
// when one still stands (the wait's statement beat the failure terminal), else
// the row retired at the terminal, else nil when the footer never described
// this work (a daemon that came up mid-wait).
func (s *wsState) waitingRow(work string) *agentRow {
	if row := s.subagentRow(work); row != nil {
		return row
	}
	return s.retiredRows[work]
}

// waitingAgentLabel is the label a wait's transient carries: the waiting
// subagent as its row names it, or the generic label for work the footer never
// described.
func (s *wsState) waitingAgentLabel(work string) string {
	if row := s.waitingRow(work); row != nil {
		return rowAgentLabel(row)
	}
	return subagentLabel(nil)
}

// drawnAgent is one agents-panel row as it will be drawn: the row's data and,
// for a row waiting for the API, its wait.
type drawnAgent struct {
	row  *agentRow
	wait *resumeWait
}

// agentRowsDrawn is the agents panel's rows in spawn order: every live
// subagent, plus every waiting subagent whose run has ended. A live row that a
// wait names is drawn waiting. Every wait has a row by construction
// (describeUnknownWait), so a standing wait is never invisible -- unless it is
// a SUBAGENT'S own subagent, which drawsWork keeps out of the footer whether
// it runs or waits (owner.go).
func (r *resolver) agentRowsDrawn(s *wsState) []drawnAgent {
	out := make([]drawnAgent, 0, len(s.agents)+len(s.resumeWaits))
	claimed := map[*agentRow]bool{}
	for i := range s.resumeWaits {
		wait := &s.resumeWaits[i]
		row := s.waitingRow(wait.work)
		if row == nil {
			continue
		}
		claimed[row] = true
		if r.drawsWork(s, "agent", wait.work, row.work, row.spawnUnit, row.createdAgent) {
			out = append(out, drawnAgent{row: row, wait: wait})
		}
	}
	for _, row := range s.agents {
		if !claimed[row] && r.drawsWork(s, "agent", row.work, row.spawnUnit, row.createdAgent) {
			out = append(out, drawnAgent{row: row})
		}
	}
	sort.SliceStable(out, func(i, j int) bool { return out[i].row.order < out[j].row.order })
	return out
}

// agentRowState is a drawn row's state arm: running, or waiting for the API.
func agentRowState(d drawnAgent, row *frontendv1.FooterAgentRow) {
	if d.wait == nil {
		row.State = &frontendv1.FooterAgentRow_Running{Running: &frontendv1.FooterAgentRowRunning{}}
		return
	}
	row.State = &frontendv1.FooterAgentRow_WaitingForApi{WaitingForApi: &frontendv1.FooterAgentRowWaitingForApi{
		FailedAtMs:       epochMs(d.wait.failedAt),
		GivesUpAtMs:      epochMs(d.wait.givesUpAt),
		ResumesDelivered: d.wait.resumes,
	}}
}
