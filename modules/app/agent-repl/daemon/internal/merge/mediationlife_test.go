package merge

import (
	"context"
	"errors"
	"testing"
	"time"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
)

func TestTheMediationLifetime(t *testing.T) {
	tests := []struct {
		name string
		act  func(t *testing.T, h *harness)
	}{
		{name: "a stop with nothing running has nothing to wait for, and nothing enters after it", act: func(t *testing.T, h *harness) {
			if idle := h.o.stopMediation(); idle != nil {
				t.Fatal("stopMediation answered a wait with nothing running")
			}
			if _, _, entered := h.o.enterMediation(); entered {
				t.Fatal("a mediation entered after the stop")
			}
		}},
		{name: "a stop cuts what runs and is idle once it leaves", act: func(t *testing.T, h *harness) {
			ctx, leave, entered := h.o.enterMediation()
			if !entered {
				t.Fatal("enterMediation refused before any stop")
			}
			idle := h.o.stopMediation()
			if idle == nil || ctx.Err() == nil {
				t.Fatalf("stop answered %v with the context %v; want a wait and the context cut", idle, ctx.Err())
			}
			select {
			case <-idle:
				t.Fatal("idle before the mediation left")
			default:
			}
			leave()
			select {
			case <-idle:
			case <-time.After(waitBound):
				t.Fatalf("not idle %v after the mediation left", waitBound)
			}
		}},
		{name: "a leave called twice leaves once", act: func(t *testing.T, h *harness) {
			_, leaveA, _ := h.o.enterMediation()
			_, leaveB, _ := h.o.enterMediation()
			leaveA()
			leaveA()
			idle := h.o.stopMediation()
			if idle == nil {
				t.Fatal("the second mediation was counted out by the first's repeated leave")
			}
			leaveB()
			select {
			case <-idle:
			case <-time.After(waitBound):
				t.Fatalf("not idle %v after both left", waitBound)
			}
		}},
		{name: "leaving more often than entering is a defect", act: func(t *testing.T, h *harness) {
			defer func() {
				if recover() == nil {
					t.Fatal("an unmatched leave did not panic")
				}
			}()
			h.o.leaveMediation()
		}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)

			// Act and Assert
			tt.act(t, h)
		})
	}
}

func TestMediationFailed(t *testing.T) {
	tests := []struct {
		name      string
		cut       bool
		err       error
		wantLevel string
		wantCut   bool
	}{
		{name: "a failure the daemon's exit cut is recorded at info", cut: true, err: context.Canceled, wantLevel: "info", wantCut: true},
		{name: "a failure while the daemon runs is an error", err: errors.New("disk gone"), wantLevel: "error"},
		{name: "a failure that is no cut is an error even once the context ended", cut: true, err: errors.New("disk gone"), wantLevel: "error"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			ctx, cancel := context.WithCancel(context.Background())
			defer cancel()
			if tt.cut {
				cancel()
			}

			// Act
			cut := h.o.mediationFailed(ctx, dlog.Context{"repo": "r"}, "could not read it", tt.err)

			// Assert
			if cut != tt.wantCut {
				t.Fatalf("mediationFailed = %v, want %v", cut, tt.wantCut)
			}
			rec, ok := h.recordFor(tt.wantLevel, opMediation)
			if !ok || rec.Context["repo"] != "r" {
				t.Fatalf("records = %v, want one at %s carrying the repo", h.logs.Records(), tt.wantLevel)
			}
			other := map[string]string{"info": "error", "error": "info"}[tt.wantLevel]
			if _, both := h.recordFor(other, opMediation); both {
				t.Fatalf("records = %v, want none at %s", h.logs.Records(), other)
			}
		})
	}
}

func TestTheDrainCutsAMediationRoundAndWaitsForIt(t *testing.T) {
	// Arrange: a round told of the deploy and awaiting its mediator's turn.
	h := mediated(t)
	owe(t, h)
	h.awaitGate, h.awaitStarted = make(chan struct{}), make(chan ids.TurnID, 1)
	mediated := make(chan struct{})
	go func() {
		defer close(mediated)
		h.o.Mediate(context.Background())
	}()
	select {
	case <-h.awaitStarted:
	case <-time.After(waitBound):
		t.Fatalf("the round did not await its turn within %v", waitBound)
	}

	// Act
	h.o.Drain(context.Background())

	// Assert: the round returned before the drain did, cut at INFO, its
	// mediation still owed for the next boot; and no pass runs after.
	select {
	case <-mediated:
	default:
		t.Fatal("the drain returned with the mediation round still running")
	}
	if _, ok := dlog.FindRecord(h.logs.Records(), dlog.RecordQuery{Level: "info", Operation: opMediation,
		Message: "the mediation was cut by the daemon's exit; the next boot's pass runs it again"}); !ok {
		t.Fatalf("records = %v, want the cut round at INFO", h.logs.Records())
	}
	if _, errored := h.recordFor("error", opMediation); errored {
		t.Fatalf("records = %v, want no ERROR for a round the exit cut", h.logs.Records())
	}
	if m, _ := h.db.MediationOf(context.Background(), mediationKey(h)); m == nil {
		t.Fatal("the cut round released its mediation; the next boot has nothing to reattach")
	}
	submitted := len(h.queue.submissions)
	h.o.Mediate(context.Background())
	if len(h.queue.submissions) != submitted {
		t.Fatal("a pass after the drain told the mediator again")
	}
	if _, ok := dlog.FindRecord(h.logs.Records(), dlog.RecordQuery{Level: "info", Operation: opMediation, Message: mediationNotRun}); !ok {
		t.Fatalf("records = %v, want the refused pass at INFO", h.logs.Records())
	}
}

func TestADrainWhoseMediationOutlivesItsBoundIsAnError(t *testing.T) {
	// Arrange: a mediation that never leaves, and a drain bound already
	// spent.
	h := newHarness(t)
	h.o.drainBound = time.Nanosecond
	_, leave, _ := h.o.enterMediation()
	defer leave()

	// Act
	h.o.Drain(context.Background())

	// Assert
	if _, ok := dlog.FindRecord(h.logs.Records(), dlog.RecordQuery{Level: "error", Operation: "daemon.merge.drain",
		Message: "the deploy mediation outlived the drain's bound; the state client closes under it"}); !ok {
		t.Fatalf("records = %+v, want the overrun recorded at error", h.logs.Records())
	}
}
