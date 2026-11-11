package workspace

import (
	"context"
	"errors"
	"fmt"

	conversationv1 "agentrepl/proto/conversation/v1"
	shimv1 "agentrepl/proto/shim/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/wsm"
)

// opOwedStartWork is the operation every owed-start-work record is filed
// under.
const opOwedStartWork = "daemon.workspace.owed_start_work"

// carryOwedStartWork puts the start work workspace WS's binding owes
// (wsm.OwedStartWork) on REQ, and answers what it put there so the answer
// settles it (settleOwedStartWork); nil when nothing is owed.
//
// THE OWED WORK RIDES EVERY START UNTIL ONE SUCCEEDS: it is read afresh for
// each StartSession askToStartSession sends, never carried in the source, so
// a start that failed on it, a retry and a restart all ask for it again. The
// parked bridge's resume never carries it: that start is a template the
// bridge's shim holds, answered through the bridge's stream, which settles
// nothing; the gate's pay answer then starts the session here and carries it.
//
// AN OWED COMPACTION IS THE RESUME'S REMEDIATION. It replaces any remediation
// the source carried, since a compacted resume reads no cold context to pay
// for or clear. An owed summary is the resume's carry_summary. Both are made
// by the cold gate's one summarizer at its default choice (defaultSummarizer).
func (f *Fleet) carryOwedStartWork(ctx context.Context, log dlog.Logger, ws ids.WorkspaceID, req *shimv1.StartSessionRequest) (*wsm.OwedStartWork, error) {
	owed, found, err := f.deps.DB.OwedStartWork(ctx, ws)
	if err != nil {
		log.Error(opOwedStartWork, "could not read the start work the workspace's binding owes; the session was not started", dlog.Context{"cause": err.Error()})
		return nil, fmt.Errorf("start session for %q: read the owed start work: %w", ws, err)
	}
	if !found {
		return nil, nil
	}
	fields := dlog.Context{"compact": owed.Compact, "summary_of": owed.SummaryOf}
	resume := req.GetResume()
	if resume == nil {
		// A BINDING THAT OWES START WORK BINDS A CONVERSATION: a queue-bypass
		// switch records it only with a resume or an own fork, which is
		// resumed once made.
		err := fmt.Errorf("start session for %q: the binding owes start work but the start is fresh", ws)
		log.Error(opOwedStartWork, "a fresh start was asked of a binding that owes start work; the session was not started",
			dlog.WithCause(dlog.Context{"invariant_violation": "owed start work is recorded only with a conversation to resume", "compact": owed.Compact, "summary_of": owed.SummaryOf}, err))
		return nil, err
	}
	if owed.Compact {
		if resume.ColdRemediation != nil {
			log.Info(opOwedStartWork, "the owed compaction replaces the remediation the resume carried", dlog.Context{"replaced": remediationName(resume.ColdRemediation)})
		}
		resume.ColdRemediation = &conversationv1.SessionColdRemediation{
			Remediation: &conversationv1.SessionColdRemediation_Compact{Compact: defaultSummarizer()},
		}
	} else {
		resume.CarrySummary = &shimv1.StartSessionCarrySummary{VendorSessionId: owed.SummaryOf, Summarizer: defaultSummarizer()}
	}
	log.Info(opOwedStartWork, "the start carries the work the workspace's binding owes", fields)
	return &owed, nil
}

// settleOwedStartWork retires CARRIED, the owed work a start carried, once
// SUCCESS answered it, recording the summary it carried for the workspace's
// next prompt (wsm.SettleOwedStartWork). A summary answered for a start that
// asked for none, or none for one that asked, is the shim breaking its
// contract, refused loudly.
func (f *Fleet) settleOwedStartWork(ctx context.Context, log dlog.Logger, ws ids.WorkspaceID, carried *wsm.OwedStartWork, success *shimv1.StartSessionSuccess) error {
	summary := success.GetCarriedSummary().GetText()
	asked := carried != nil && carried.SummaryOf != ""
	if asked != (summary != "") {
		err := fmt.Errorf("start session for %q: the shim answered a carried summary %v for a start that asked for one %v", ws, summary != "", asked)
		log.Error(opOwedStartWork, "the start's carried summary disagrees with what it asked for; the session's start is refused",
			dlog.WithCause(dlog.Context{"invariant_violation": "StartSessionSuccess.carried_summary is set exactly when the resume carried carry_summary", "asked": asked}, err))
		return err
	}
	if carried == nil {
		return nil
	}
	fields := dlog.Context{"compact": carried.Compact, "summary_of": carried.SummaryOf, "summary_chars": len(summary)}
	if err := f.deps.DB.SettleOwedStartWork(ctx, ws, summary); err != nil {
		log.Error(opOwedStartWork, "could not settle the start work the session carried out; the next start asks for it again", dlog.WithCause(fields, err))
		return fmt.Errorf("start session for %q: settle the owed start work: %w", ws, err)
	}
	log.Info(opOwedStartWork, "settled the start work the session carried out", fields)
	return nil
}

// defaultSummarizer is the cold gate's default compaction choice: the first
// model and the first scope its menu offers (coldCompactMenu), so a switch's
// compaction and summary are made exactly as the gate's default would be.
func defaultSummarizer() *conversationv1.SessionColdCompact {
	served, _ := coldCompactMenu()
	return &conversationv1.SessionColdCompact{Model: served.Models[0], Scope: served.Scopes[0]}
}

// compactionFailure answers the shim's account when ERR is a start that
// failed on the compaction or summary it was asked for
// (shim.v1.StartSessionVendorStartFailed.compaction).
func compactionFailure(err error) (string, bool) {
	var label *startLabel
	if errors.As(err, &label) && label.compaction {
		return label.cause, true
	}
	return "", false
}
