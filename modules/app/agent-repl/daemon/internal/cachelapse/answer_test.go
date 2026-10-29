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
			h.keeper.answersDue()

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
	h.keeper.answersDue()

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
