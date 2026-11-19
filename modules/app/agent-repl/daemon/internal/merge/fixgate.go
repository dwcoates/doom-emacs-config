package merge

import (
	"context"
	"fmt"
	"time"

	frontendv1 "agentrepl/proto/frontend/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/feedid"
	"agent-repld/internal/ids"
	"agent-repld/internal/wsm"
)

// A DEPLOY MEDIATOR'S FIX AT ITS GATE (frontend.v1.FeedFixGate; owner ruling,
// 2026-11-19). The full gate remediationGate runs on the mediator's branch is
// drawn in the MEDIATOR'S OWN FEED as it runs: one card per gate attempt, the
// merge bubble's head over the tests tab's run output. Its lines are read by
// the same suiteTracker a queue merge's gate reads its lines through, so the
// two draw a run alike.

// fixGateLabel is the card's head label.
const fixGateLabel = "testing the fix"

// fixGateLog names the log of attempt ROUND of deploy mediator WS's gate: its
// token is the mediator and the attempt, which TestLogPath resolves for the
// mediator's own feed.
func (o *orchestrator) fixGateLog(ws ids.WorkspaceID, round int) testLog {
	return o.testsLogOf(string(ws), round)
}

// fixGateRun is one attempt of a mediator's fix at its gate, as its card
// draws it.
type fixGateRun struct {
	o       *orchestrator
	f       wsm.RemediationFinish
	attempt int
	log     testLog
	begun   time.Time
	tracker *suiteTracker
	fields  dlog.Context
}

// newFixGateRun answers finish F's next gate attempt, begun now, and draws
// its card live.
func (o *orchestrator) newFixGateRun(f wsm.RemediationFinish, fields dlog.Context) *fixGateRun {
	g := &fixGateRun{o: o, f: f, attempt: f.GateRounds + 1, begun: o.deps.Now(), tracker: newSuiteTracker(o.nowMS), fields: fields}
	g.log = o.fixGateLog(f.Workspace, g.attempt)
	g.publish(g.live(), false)
	return g
}

// line follows one line of the gate's run: a line that moved the run's
// tests view redraws the card, and one the tracker cannot read is recorded
// and moves nothing (suiteTracker.take).
func (g *fixGateRun) line(line string) {
	took, err := g.tracker.take(line)
	recordUnreadLine(g.o.deps.Log.Global(), opRemediationFinish, line, err, g.fields)
	if took.moved {
		g.publish(g.live(), false)
	}
}

// passed settles the card on a run every suite passed.
func (g *fixGateRun) passed() {
	g.publish(g.settled(&frontendv1.FeedMergeTabSettled_Succeeded{Succeeded: &frontendv1.FeedMergeTabSucceeded{}}), true)
}

// failed settles the card on a run that failed, or a gate that could not
// run, with SUMMARY its one-line account.
func (g *fixGateRun) failed(summary string) {
	g.publish(g.settled(&frontendv1.FeedMergeTabSettled_Failed{Failed: &frontendv1.FeedMergeTabFailed{Summary: summary}}), true)
}

// live is the card's state while the suites run.
func (g *fixGateRun) live() *frontendv1.FeedFixGate {
	return &frontendv1.FeedFixGate{State: &frontendv1.FeedFixGate_Live{Live: &frontendv1.FeedMergeTabLive{StartedAtMs: g.begun.UnixMilli()}}}
}

// settled is the card's state once the run ended, with OUTCOME.
func (g *fixGateRun) settled(outcome any) *frontendv1.FeedFixGate {
	st := &frontendv1.FeedMergeTabSettled{StartedAtMs: g.begun.UnixMilli(), EndedAtMs: g.o.nowMS()}
	switch o := outcome.(type) {
	case *frontendv1.FeedMergeTabSettled_Succeeded:
		st.Outcome = o
	case *frontendv1.FeedMergeTabSettled_Failed:
		st.Outcome = o
	default:
		panic(fmt.Sprintf("merge: a fix gate settles with %T, which is no outcome", outcome))
	}
	return &frontendv1.FeedFixGate{State: &frontendv1.FeedFixGate_Settled{Settled: st}}
}

// publish draws CARD, with the head and the run's tests view, in the
// mediator's root feed under the attempt's own row. WRITTEN reports that the
// run's log is on disk, so the card links it.
func (g *fixGateRun) publish(card *frontendv1.FeedFixGate, written bool) {
	var link *frontendv1.FeedMergeTestLog
	if written {
		link = g.o.testLogLink(g.log)
	}
	card.Head = &frontendv1.FeedFixGateHead{
		Label:       &frontendv1.FeedMergeLabel{Text: fixGateLabel},
		Attempt:     uint32(g.attempt),
		MaxAttempts: uint32(MaxFixAttempts),
	}
	card.Output = g.tracker.view(link)
	g.o.deps.Feed.UpsertDurable(g.f.Workspace, feedid.Feed{Root: true}, fixGateRow(g.f, g.attempt, card))
}

// fixGateRow is attempt ATTEMPT of finish F's card as its feed row. THE ROW
// IS KEYED BY THE FINISH AND THE ATTEMPT, so a later chain's first attempt
// never overwrites an earlier chain's card.
func fixGateRow(f wsm.RemediationFinish, attempt int, card *frontendv1.FeedFixGate) *frontendv1.FeedRow {
	return &frontendv1.FeedRow{
		Id: feedid.Encode(feedid.Ref{
			WS:   f.Workspace,
			Feed: feedid.Feed{Root: true},
			Row:  feedid.RowKey{Kind: feedid.KindFixGate, ID: fmt.Sprintf("%d-%d", f.StartedAt.UnixMilli(), attempt)},
		}),
		Row: &frontendv1.FeedRow_Activity{Activity: &frontendv1.FeedTurnActivity{
			Unit: &frontendv1.FeedTurnActivity_FixGate{FixGate: card},
		}},
	}
}

// fixGateLogPath resolves deploy mediator WS's own gate log for ROUND, or
// answers false when WS is no deploy mediator (TestLogPath).
func (o *orchestrator) fixGateLogPath(ctx context.Context, ws ids.WorkspaceID, round int) (string, bool, error) {
	_, isMediator, err := o.deps.DB.MediatorRepoOf(ctx, ws)
	if err != nil || !isMediator {
		return "", false, err
	}
	return o.fixGateLog(ws, round).path, true, nil
}
