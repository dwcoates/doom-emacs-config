package cachelapse

import (
	"context"
	"errors"
	"testing"
	"time"

	"agent-repld/internal/dlog"
)

// viewedHarness is the workspace the user looks at, parked by the keep-alive
// cycle, with a reader's restore of it asked for and in flight.
func viewedHarness(t *testing.T) *harness {
	t.Helper()
	rec := beatable(visible(), 10*time.Minute)
	rec.Current = true
	h := newHarness(t, rec)
	h.db.sessions[theWorkspace] = parkedSession(theWorkspace)
	h.serving = false
	h.keeper.apply(context.Background(), edge{ws: theWorkspace, kind: edgeRestoreAsked, why: "test"})
	return h
}

// restoreUp ends the harness's in-flight restore of the workspace.
func (h *harness) restoreUp(err error) {
	h.keeper.apply(context.Background(), edge{ws: theWorkspace, kind: edgeRestored, err: err, why: "test"})
}

// lookAway moves the user's selection off the workspace.
func (h *harness) lookAway() {
	rec := h.db.records[theWorkspace]
	rec.Current = false
	h.db.records[theWorkspace] = rec
	h.keeper.apply(context.Background(), edge{ws: theWorkspace, why: "test"})
}

func TestAViewedSession(t *testing.T) {
	upAt := now.Add(5 * time.Second)
	tests := []struct {
		name string
		// act runs after the reader's restore was asked for.
		act        func(h *harness)
		wantCycled []cycleCall
	}{
		{
			name: "is stood down once the user looks away, timed from its coming up",
			act: func(h *harness) {
				h.clock.Set(upAt)
				h.restoreUp(nil)
				h.clock.Set(upAt.Add(time.Minute))
				h.lookAway()
			},
			wantCycled: []cycleCall{{ws: theWorkspace, beat: upAt}},
		},
		{
			name: "is kept up while the user looks at it",
			act: func(h *harness) {
				h.restoreUp(nil)
				h.keeper.apply(context.Background(), edge{ws: theWorkspace, why: "test"})
			},
		},
		{
			name: "coming up after the user looked away is stood down at once",
			act: func(h *harness) {
				h.lookAway()
				h.clock.Set(upAt)
				h.restoreUp(nil)
			},
			wantCycled: []cycleCall{{ws: theWorkspace, beat: upAt}},
		},
		{
			name: "still restoring when the user looks away is not stood down yet",
			act: func(h *harness) {
				h.lookAway()
			},
		},
		{
			name: "is stood down once only",
			act: func(h *harness) {
				h.clock.Set(upAt)
				h.restoreUp(nil)
				h.lookAway()
				h.keeper.apply(context.Background(), edge{ws: theWorkspace, why: "test"})
			},
			wantCycled: []cycleCall{{ws: theWorkspace, beat: upAt}},
		},
		{
			name: "whose work ended is left to the keep-alive cycle",
			act: func(h *harness) {
				h.restoreUp(nil)
				h.keeper.apply(context.Background(), edge{ws: theWorkspace, kind: edgeFree, workEnded: true, why: "test"})
				h.lookAway()
			},
		},
		{
			name: "whose attach came up free is still stood down",
			act: func(h *harness) {
				h.clock.Set(upAt)
				h.restoreUp(nil)
				h.keeper.apply(context.Background(), edge{ws: theWorkspace, kind: edgeFree, workEnded: false, why: "test"})
				h.lookAway()
			},
			wantCycled: []cycleCall{{ws: theWorkspace, beat: upAt}},
		},
		{
			name: "whose restore failed is forgotten",
			act: func(h *harness) {
				h.restoreUp(errors.New("the shim would not start"))
				h.lookAway()
			},
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := viewedHarness(t)

			// Act
			tt.act(h)

			// Assert
			if len(h.cycled) != len(tt.wantCycled) {
				t.Fatalf("cycled = %+v, want %+v", h.cycled, tt.wantCycled)
			}
			for i, want := range tt.wantCycled {
				if got := h.cycled[i]; got.ws != want.ws || !got.beat.Equal(want.beat) {
					t.Fatalf("cycled = %+v, want %+v", h.cycled, tt.wantCycled)
				}
			}
		})
	}
}

func TestAViewedRestoreStillInLineWhenTheUserLooksAwayIsTakenOut(t *testing.T) {
	// Arrange: every restore slot is taken, so the reader's restore waits.
	h, _ := parkedFleet(t, MaxRestores)
	h.keeper.restoresDue(context.Background())
	rec := h.db.records[theWorkspace]
	rec.Current = true
	h.db.records[theWorkspace] = rec
	h.db.sessions[theWorkspace] = parkedSession(theWorkspace)
	h.keeper.apply(context.Background(), edge{ws: theWorkspace, kind: edgeRestoreAsked, why: "test"})

	// Act
	h.lookAway()

	// Assert
	if len(h.keeper.queued) != 0 || len(h.cycled) != 0 {
		t.Fatalf("waiting %+v, cycled %+v, want the restore taken out of line and nothing stood down", h.keeper.queued, h.cycled)
	}
	if !h.found(dlog.RecordQuery{Level: "info", Operation: opRestore, Message: "the user looked away before the session restored to be viewed was started; it is not restored"}) {
		t.Fatalf("records = %+v, want the restore's withdrawal recorded", h.log.Records())
	}
}

func TestABeatsRestoreIsNotStoodDownWhenTheUserLooksAway(t *testing.T) {
	// Arrange
	h, fleet := parkedFleet(t, 1)
	h.keeper.restoresDue(context.Background())
	h.keeper.apply(context.Background(), edge{ws: fleet[0], kind: edgeRestored, why: "test"})

	// Act
	h.keeper.apply(context.Background(), edge{ws: fleet[0], why: "test"})

	// Assert
	if len(h.cycled) != 0 {
		t.Fatalf("cycled = %+v, want none: the beat's own stand-down follows the beat", h.cycled)
	}
}

func TestAViewedSessionWhoseRecordCannotBeReadIsLeftUp(t *testing.T) {
	// Arrange
	h := viewedHarness(t)
	h.db.readErr = errors.New("the state file is locked")

	// Act
	h.restoreUp(nil)

	// Assert
	if len(h.cycled) != 0 {
		t.Fatalf("cycled = %+v, want none", h.cycled)
	}
	if !h.found(dlog.RecordQuery{Level: "error", Operation: opRestore}) {
		t.Fatalf("records = %+v, want the failed read at ERROR", h.log.Records())
	}
}
