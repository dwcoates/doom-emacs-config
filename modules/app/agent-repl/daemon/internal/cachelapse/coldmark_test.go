package cachelapse

import (
	"context"
	"errors"
	"testing"
	"time"

	shimv1 "agentrepl/proto/shim/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/sourcescan"
)

// overdue is a visible workspace whose last turn ended more than an interval
// ago: the shape a cold gate's answer left its session in on 2026-10-29, its
// beat due at once.
func overdue(t *testing.T) *harness {
	t.Helper()
	return newHarness(t, beatable(visible(), KeepaliveInterval+time.Minute))
}

func lapsedAnswer() *shimv1.SendKeepaliveSuccess {
	return skipped(&shimv1.SendKeepaliveSkipped{Reason: &shimv1.SendKeepaliveSkipped_CacheLapsed{CacheLapsed: &shimv1.SendKeepaliveSkippedCacheLapsed{LapsedAtMs: 1}}})
}

func (h *harness) markedAt() *time.Time {
	h.db.mu.Lock()
	defer h.db.mu.Unlock()
	return h.db.records[theWorkspace].CacheColdTurnAt
}

func TestTheColdGateMarksTheCacheCold(t *testing.T) {
	tests := []struct {
		name string
		act  func(h *harness) error
	}{
		{name: "when it is raised", act: func(h *harness) error { h.keeper.ColdGateRaised(theWorkspace, true); return nil }},
		{name: "when it is raised again", act: func(h *harness) error { h.keeper.ColdGateRaised(theWorkspace, false); return nil }},
		{name: "when it is answered", act: func(h *harness) error { return h.keeper.ColdGateAnswered(context.Background(), theWorkspace) }},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := overdue(t)
			ended := *h.db.records[theWorkspace].LastTurnEndedAt

			// Act
			err := tt.act(h)

			// Assert
			if err != nil {
				t.Fatalf("err = %v, want nil", err)
			}
			if got := h.markedAt(); got == nil || !got.Equal(ended) {
				t.Fatalf("cold mark = %v, want the last turn end %v", got, ended)
			}
			if !h.found(dlog.RecordQuery{Level: "info", Operation: opColdMark}) {
				t.Fatalf("no record of the mark: %v", h.log.Records())
			}
		})
	}
}

// The defect of 2026-10-29: a cleared or compacted conversation's shim never
// refuses the beat, and the answer's session came up with it already overdue.
func TestTheSessionAnAnswerBringsUpOwesNoKeepalive(t *testing.T) {
	// Arrange
	h := overdue(t)
	h.shim.answer = sent()
	if err := h.keeper.ColdGateAnswered(context.Background(), theWorkspace); err != nil {
		t.Fatalf("ColdGateAnswered: %v", err)
	}

	// Act
	for _, e := range h.keeper.take() {
		h.keeper.apply(context.Background(), e)
	}
	h.keeper.apply(context.Background(), edge{ws: theWorkspace, why: "a session came up"})
	h.keeper.beatsDue(context.Background())

	// Assert
	if _, armed := h.keeper.beats[theWorkspace]; armed || h.shim.beats != 0 {
		t.Fatalf("beats sent = %d, armed = %v; want none until the next turn ends", h.shim.beats, armed)
	}
}

func TestAnAnswerQueuesAReconcile(t *testing.T) {
	// Arrange
	h := overdue(t)

	// Act
	err := h.keeper.ColdGateAnswered(context.Background(), theWorkspace)

	// Assert
	queued := h.keeper.take()
	if err != nil || len(queued) != 1 || queued[0].kind != edgeReconcile {
		t.Fatalf("err = %v, queued = %+v; want one reconcile", err, queued)
	}
}

func TestAnAnswerWhoseMarkFailsIsAnsweredTheFailure(t *testing.T) {
	// Arrange
	h := overdue(t)
	h.db.coldErr = errors.New("store unreachable")

	// Act
	err := h.keeper.ColdGateAnswered(context.Background(), theWorkspace)

	// Assert
	if err == nil || !errors.Is(err, h.db.coldErr) {
		t.Fatalf("err = %v, want the store's failure", err)
	}
	if queued := h.keeper.take(); len(queued) != 0 {
		t.Fatalf("queued = %+v, want nothing for an unmarked answer", queued)
	}
	assertColdMarkFailureRecorded(t, h, "its cold gate was answered")
}

func TestARaiseWhoseMarkFailsIsRecordedAndStillReconciled(t *testing.T) {
	// Arrange
	h := overdue(t)
	h.db.coldErr = errors.New("store unreachable")

	// Act
	h.keeper.ColdGateRaised(theWorkspace, true)

	// Assert
	if queued := h.keeper.take(); len(queued) != 1 {
		t.Fatalf("queued = %+v, want the raise's reconcile", queued)
	}
	assertColdMarkFailureRecorded(t, h, "the session parked at its cold gate")
}

func assertColdMarkFailureRecorded(t *testing.T, h *harness, why string) {
	t.Helper()
	rec, ok := dlog.FindRecord(h.log.Records(), dlog.RecordQuery{Level: "error", Operation: opColdMark})
	if !ok {
		t.Fatalf("no ERROR record of the failed mark: %v", h.log.Records())
	}
	if rec.Context["why"] != why || rec.Context["cause"] != "store unreachable" || rec.Context["workspace"] != string(theWorkspace) {
		t.Fatalf("record context = %v, want why %q, the cause and the workspace", rec.Context, why)
	}
}

func TestALapsedJudgmentMarksTheCacheCold(t *testing.T) {
	// Arrange
	h := dueHarness(t, lapsedAnswer())
	h.keeper.beats[theWorkspace] = now

	// Act
	h.keeper.beatsDue(context.Background())

	// Assert
	if got := h.markedAt(); got == nil || !got.Equal(*h.db.records[theWorkspace].LastTurnEndedAt) {
		t.Fatalf("cold mark = %v, want the last turn end", got)
	}
	if _, armed := h.keeper.beats[theWorkspace]; armed {
		t.Fatal("a beat was armed for a marked cache")
	}
}

func TestALapsedJudgmentWhoseMarkFailsIsAskedAgainShortly(t *testing.T) {
	// Arrange
	h := dueHarness(t, lapsedAnswer())
	h.db.coldErr = errors.New("store unreachable")
	h.keeper.beats[theWorkspace] = now

	// Act
	h.keeper.beatsDue(context.Background())

	// Assert
	if got := h.keeper.beats[theWorkspace]; !got.Equal(now.Add(BeatRetry)) {
		t.Fatalf("next beat = %v, want a retry at %v", got, now.Add(BeatRetry))
	}
	assertColdMarkFailureRecorded(t, h, "the shim judged the cache lapsed")
}

func TestTheColdMarkOutlivesTheKeeper(t *testing.T) {
	// Arrange
	h := overdue(t)
	h.keeper.ColdGateRaised(theWorkspace, false)
	rec := h.db.records[theWorkspace]
	successor := newHarness(t, rec)

	// Act
	if err := successor.keeper.restore(context.Background()); err != nil {
		t.Fatalf("restore: %v", err)
	}

	// Assert
	if _, armed := successor.keeper.beats[theWorkspace]; armed {
		t.Fatal("a restarted keeper armed a beat for a cache marked cold")
	}
}

func TestTheNextTurnsEndResumesTheBeats(t *testing.T) {
	// Arrange
	h := overdue(t)
	if err := h.keeper.ColdGateAnswered(context.Background(), theWorkspace); err != nil {
		t.Fatalf("ColdGateAnswered: %v", err)
	}
	ended := now.Add(-time.Minute)
	rec := h.db.records[theWorkspace]
	rec.LastTurnEndedAt = &ended
	h.db.records[theWorkspace] = rec

	// Act
	h.keeper.apply(context.Background(), edge{ws: theWorkspace, why: "a turn's close was recorded"})

	// Assert
	if got, want := h.keeper.beats[theWorkspace], ended.Add(KeepaliveInterval); !got.Equal(want) {
		t.Fatalf("beat = %v, want an interval after the turn's end, %v", got, want)
	}
}

// TestColdMarkWritesHaveOneHome fails when a source file in this package
// writes the cold mark other than through Keeper.markCold.
func TestColdMarkWritesHaveOneHome(t *testing.T) {
	// Act
	sites := sourcescan.Sites(t, ".MarkCacheCold(ctx")

	// Assert
	if len(sites) != 1 || sites[0] != "coldmark.go" {
		t.Fatalf("the cold mark is written at %v, want only Keeper.markCold in coldmark.go", sites)
	}
}
