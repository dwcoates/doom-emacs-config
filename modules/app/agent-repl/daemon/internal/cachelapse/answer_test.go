package cachelapse

import (
	"context"
	"testing"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"

	"agent-repld/internal/dlog"
)

// restated is a lapse the shim restates, falling at AT.
func restated(at time.Time) *conversationv1.SessionCacheLapse {
	return &conversationv1.SessionCacheLapse{
		LapsesAtMs: at.UnixMilli(),
		AfterLapse: &conversationv1.SessionCacheLapse_Ungated{Ungated: &conversationv1.SessionCacheLapseUngated{}},
	}
}

// awaiting is a harness whose keep-alive went out at now and whose stand-down
// awaits its answer.
func awaiting(t *testing.T) *harness {
	t.Helper()
	h := dueHarness(t, sent())
	h.keeper.beats[theWorkspace] = now
	h.keeper.beatsDue(context.Background())
	if _, waiting := h.keeper.answering[theWorkspace]; !waiting {
		t.Fatal("the sent beat awaits no answer")
	}
	return h
}

func TestTheAnsweredKeepaliveStandsTheSessionDown(t *testing.T) {
	tests := []struct {
		name     string
		lapsesAt time.Time
		want     int
	}{
		{name: "a lapse re-anchored by the answer stands it down", lapsesAt: now.Add(time.Hour), want: 1},
		{name: "a lapse an interval after the send stands it down", lapsesAt: now.Add(KeepaliveInterval), want: 1},
		{name: "a lapse restated from the anchor before the send waits on", lapsesAt: now.Add(8 * time.Minute), want: 0},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := awaiting(t)

			// Act
			h.keeper.apply(context.Background(), edge{ws: theWorkspace, kind: edgeLapse, lapse: restated(tt.lapsesAt)})

			// Assert
			if len(h.cycled) != tt.want {
				t.Fatalf("cycle stand-downs = %+v, want %d", h.cycled, tt.want)
			}
		})
	}
}

func TestTheAnsweredKeepaliveStandsDownAtItsBeat(t *testing.T) {
	// Arrange
	h := awaiting(t)

	// Act
	h.keeper.apply(context.Background(), edge{ws: theWorkspace, kind: edgeLapse, lapse: restated(now.Add(time.Hour))})

	// Assert
	if len(h.cycled) != 1 || h.cycled[0].ws != theWorkspace || !h.cycled[0].beat.Equal(now) {
		t.Fatalf("cycle stand-downs = %+v, want one at the beat", h.cycled)
	}
}

func TestAnAnsweredKeepaliveIsNoLongerAwaited(t *testing.T) {
	// Arrange
	h := awaiting(t)

	// Act
	h.keeper.apply(context.Background(), edge{ws: theWorkspace, kind: edgeLapse, lapse: restated(now.Add(time.Hour))})

	// Assert
	if len(h.keeper.answering) != 0 || len(h.keeper.answerBy) != 0 {
		t.Fatalf("answers awaited = %v, bounds %v, want none", h.keeper.answering, h.keeper.answerBy)
	}
}

func TestALapseWithNoKeepaliveAwaitedStandsNothingDown(t *testing.T) {
	// Arrange
	h := newHarness(t, visible())

	// Act
	h.keeper.apply(context.Background(), edge{ws: theWorkspace, kind: edgeLapse, lapse: restated(now.Add(time.Hour))})

	// Assert
	if len(h.cycled) != 0 {
		t.Fatalf("cycle stand-downs = %+v, want none", h.cycled)
	}
}

func TestAnUnansweredKeepaliveStandsTheSessionDownAtItsBound(t *testing.T) {
	tests := []struct {
		name  string
		after time.Duration
		want  int
	}{
		{name: "before the bound it waits on", after: AnswerBound - time.Second, want: 0},
		{name: "at the bound it is stood down", after: AnswerBound, want: 1},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := awaiting(t)
			h.clock.Set(now.Add(tt.after))

			// Act
			h.keeper.answersDue(context.Background())

			// Assert
			if len(h.cycled) != tt.want {
				t.Fatalf("cycle stand-downs = %+v, want %d", h.cycled, tt.want)
			}
		})
	}
}

func TestAnUnansweredKeepaliveIsRecordedAtWarn(t *testing.T) {
	// Arrange
	h := awaiting(t)
	h.clock.Set(now.Add(AnswerBound))

	// Act
	h.keeper.answersDue(context.Background())

	// Assert
	if _, ok := dlog.FindRecord(h.log.Records(), dlog.RecordQuery{Level: "warn", Operation: opDrive}); !ok {
		t.Fatalf("no WARN record of the unanswered keep-alive: %v", h.log.Records())
	}
}

func TestTheAnswerBoundIsAnArmedDeadline(t *testing.T) {
	// Arrange
	h := awaiting(t)
	delete(h.keeper.beats, theWorkspace)

	// Act
	next, armed := h.keeper.earliest()

	// Assert
	if !armed || !next.Equal(now.Add(AnswerBound)) {
		t.Fatalf("earliest = %v (%t), want the answer's bound", next, armed)
	}
}

// failedKeepalive is the shim's failed answer to a keep-alive.
func failedKeepalive() *conversationv1.SessionKeepaliveFailed {
	return &conversationv1.SessionKeepaliveFailed{Detail: "Can't reach the API server (ENOTFOUND)"}
}

func TestAFailedKeepaliveAnswersTheStandDownItAwaits(t *testing.T) {
	// Arrange.
	h := awaiting(t)

	// Act.
	h.keeper.apply(context.Background(), edge{ws: theWorkspace, kind: edgeKeepaliveFailed, failed: failedKeepalive()})

	// Assert.
	t.Run("the session is stood down at its beat", func(t *testing.T) {
		if len(h.cycled) != 1 || h.cycled[0].ws != theWorkspace || !h.cycled[0].beat.Equal(now) {
			t.Fatalf("cycle stand-downs = %+v, want one at the beat", h.cycled)
		}
	})
	t.Run("the answer is no longer awaited", func(t *testing.T) {
		if len(h.keeper.answering) != 0 || len(h.keeper.answerBy) != 0 {
			t.Fatalf("answers awaited = %v, bounds %v, want none", h.keeper.answering, h.keeper.answerBy)
		}
	})
	t.Run("it is recorded at INFO with the failure's detail", func(t *testing.T) {
		record, ok := dlog.FindRecord(h.log.Records(), dlog.RecordQuery{Level: "info", Operation: opDrive,
			Message: "the keep-alive's request failed; its session is stood down, and the cache lapses at its old anchor"})
		if !ok || record.Context["detail"] != "Can't reach the API server (ENOTFOUND)" {
			t.Fatalf("records = %v, want the INFO record carrying the detail", h.log.Records())
		}
	})
	t.Run("nothing is recorded at WARN when the bound later passes", func(t *testing.T) {
		h.clock.Set(now.Add(AnswerBound))
		h.keeper.answersDue(context.Background())
		if record, ok := dlog.FindRecord(h.log.Records(), dlog.RecordQuery{Level: "warn", Operation: opDrive}); ok {
			t.Fatalf("WARN record %+v, want none for an answered keep-alive", record)
		}
	})
}

func TestAFailedKeepaliveWithNoStandDownAwaitedStandsNothingDown(t *testing.T) {
	// Arrange: a lapse the request moved already answered the keep-alive.
	h := awaiting(t)
	h.keeper.apply(context.Background(), edge{ws: theWorkspace, kind: edgeLapse, lapse: restated(now.Add(time.Hour))})

	// Act.
	h.keeper.apply(context.Background(), edge{ws: theWorkspace, kind: edgeKeepaliveFailed, failed: failedKeepalive()})

	// Assert.
	if len(h.cycled) != 1 {
		t.Fatalf("cycle stand-downs = %+v, want only the lapse's", h.cycled)
	}
}

func TestOnKeepaliveFailedQueuesTheFailedAnswer(t *testing.T) {
	// Arrange.
	h := newHarness(t, visible())

	// Act.
	h.keeper.OnKeepaliveFailed(theWorkspace, failedKeepalive())

	// Assert.
	queued := h.keeper.take()
	if len(queued) != 1 || queued[0].kind != edgeKeepaliveFailed || queued[0].failed.GetDetail() != "Can't reach the API server (ENOTFOUND)" {
		t.Fatalf("queued = %+v, want the failed answer", queued)
	}
}

func TestSettleAnswerEndsTheWaitAndStandsTheSessionDownAtTheBeat(t *testing.T) {
	// Arrange.
	h := awaiting(t)

	// Act.
	h.keeper.settleAnswer(context.Background(), theWorkspace, now, nil)

	// Assert.
	if len(h.keeper.answering) != 0 || len(h.keeper.answerBy) != 0 {
		t.Fatalf("answers awaited = %v, bounds %v, want none", h.keeper.answering, h.keeper.answerBy)
	}
	if len(h.cycled) != 1 || h.cycled[0].ws != theWorkspace || !h.cycled[0].beat.Equal(now) {
		t.Fatalf("cycle stand-downs = %+v, want one at the beat", h.cycled)
	}
}

func TestEveryKeepaliveOutcomeSettlesItsWaitTheSameWay(t *testing.T) {
	tests := []struct {
		name   string
		settle func(h *harness)
	}{
		{name: "answered by its lapse", settle: func(h *harness) {
			h.keeper.apply(context.Background(), edge{ws: theWorkspace, kind: edgeLapse, lapse: restated(now.Add(time.Hour))})
		}},
		{name: "answered by its failure", settle: func(h *harness) {
			h.keeper.apply(context.Background(), edge{ws: theWorkspace, kind: edgeKeepaliveFailed, failed: failedKeepalive()})
		}},
		{name: "unanswered past its bound", settle: func(h *harness) {
			h.clock.Set(now.Add(AnswerBound))
			h.keeper.answersDue(context.Background())
		}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			h := awaiting(t)

			// Act.
			tt.settle(h)

			// Assert.
			if len(h.keeper.answering) != 0 || len(h.keeper.answerBy) != 0 ||
				len(h.cycled) != 1 || !h.cycled[0].beat.Equal(now) {
				t.Fatalf("awaited %v, bounds %v, stand-downs %+v; want the wait ended and one stand-down at the beat",
					h.keeper.answering, h.keeper.answerBy, h.cycled)
			}
		})
	}
}
