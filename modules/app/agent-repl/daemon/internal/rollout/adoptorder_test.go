package rollout

import (
	"context"
	"errors"
	"testing"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
)

func TestRankOfAManifestSession(t *testing.T) {
	tests := []struct {
		name    string
		session ManifestSession
		want    adoptionRank
	}{
		{name: "the viewed workspace is first", session: ManifestSession{Viewed: true}, want: rankViewed},
		{name: "viewed outranks work in flight", session: ManifestSession{Viewed: true, WorkInFlight: true}, want: rankViewed},
		{name: "work in flight is next", session: ManifestSession{WorkInFlight: true}, want: rankWorkInFlight},
		{name: "an idle workspace is last", session: ManifestSession{}, want: rankRest},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act.
			got := rankOf(tt.session)

			// Assert.
			if got != tt.want {
				t.Fatalf("rankOf = %v, want %v", got, tt.want)
			}
		})
	}
}

// admitted runs O's admission of WS on its own goroutine and answers the
// channel its outcome arrives on.
func admitted(o *adoptionOrder, ctx context.Context, ws ids.WorkspaceID) <-chan error {
	out := make(chan error, 1)
	go func() { out <- o.admit(ctx, ws) }()
	return out
}

// awaitState waits for WS's entry in O to reach STATE, on the order's own
// change signal.
func awaitState(t *testing.T, o *adoptionOrder, ws ids.WorkspaceID, state orderState) {
	t.Helper()
	for {
		o.mu.Lock()
		e, ok := o.entries[ws]
		changed := o.changed.Changed()
		o.mu.Unlock()
		if ok && e.state == state {
			return
		}
		<-changed
	}
}

// stillWaiting fails T when ADMISSION has already answered. It is checked
// only once WS's admission is released and the higher rank that holds it is
// still standing, so the admission cannot have answered yet.
func stillWaiting(t *testing.T, admission <-chan error) {
	t.Helper()
	select {
	case err := <-admission:
		t.Fatalf("admission answered %v, want it held behind the higher rank", err)
	default:
	}
}

func TestALowerRankWaitsForAHigherRankThatHasNotEnded(t *testing.T) {
	tests := []struct {
		name    string
		release func(o *adoptionOrder)
	}{
		{name: "the higher rank reaches ownership", release: func(o *adoptionOrder) { o.owns("viewed") }},
		{name: "the higher rank ends without ownership", release: func(o *adoptionOrder) { o.end("viewed") }},
		{name: "the higher rank waits on its incumbent", release: func(o *adoptionOrder) { o.awaiting("viewed") }},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange: the viewed workspace's adoption is pending.
			o := newAdoptionOrder(dlog.NewTestLogger())
			o.register(map[ids.WorkspaceID]adoptionRank{"viewed": rankViewed, "idle": rankRest})
			admission := admitted(o, context.Background(), "idle")
			awaitState(t, o, "idle", orderReleased)
			stillWaiting(t, admission)

			// Act.
			tt.release(o)

			// Assert.
			if err := <-admission; err != nil {
				t.Fatalf("admission = %v, want admitted", err)
			}
		})
	}
}

func TestARunningHigherRankHoldsALowerRankBack(t *testing.T) {
	// Arrange: the viewed adoption is admitted and running.
	o := newAdoptionOrder(dlog.NewTestLogger())
	o.register(map[ids.WorkspaceID]adoptionRank{"viewed": rankViewed, "busy": rankWorkInFlight})
	if err := o.admit(context.Background(), "viewed"); err != nil {
		t.Fatalf("admit viewed = %v", err)
	}

	// Act.
	admission := admitted(o, context.Background(), "busy")
	awaitState(t, o, "busy", orderReleased)

	// Assert.
	stillWaiting(t, admission)
	o.owns("viewed")
	if err := <-admission; err != nil {
		t.Fatalf("admission = %v, want admitted once the viewed adoption owns its workspace", err)
	}
}

func TestAdmissionIsImmediateWhenNothingRanksHigher(t *testing.T) {
	tests := []struct {
		name    string
		arrange func(o *adoptionOrder)
		ws      ids.WorkspaceID
	}{
		{name: "the highest rank", arrange: func(o *adoptionOrder) {
			o.register(map[ids.WorkspaceID]adoptionRank{"viewed": rankViewed, "idle": rankRest})
		}, ws: "viewed"},
		{name: "one of the same rank", arrange: func(o *adoptionOrder) {
			o.register(map[ids.WorkspaceID]adoptionRank{"a": rankRest, "b": rankRest})
		}, ws: "b"},
		{name: "an unordered workspace", arrange: func(o *adoptionOrder) {
			o.register(map[ids.WorkspaceID]adoptionRank{"viewed": rankViewed})
		}, ws: "straggler"},
		{name: "a retry of an ended adoption", arrange: func(o *adoptionOrder) {
			o.register(map[ids.WorkspaceID]adoptionRank{"viewed": rankViewed, "idle": rankRest})
			o.end("idle")
		}, ws: "idle"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange: a cancelled context fails any admission that would wait.
			o := newAdoptionOrder(dlog.NewTestLogger())
			tt.arrange(o)
			ctx, cancel := context.WithCancel(context.Background())
			cancel()

			// Act.
			err := o.admit(ctx, tt.ws)

			// Assert.
			if err != nil {
				t.Fatalf("admit = %v, want admitted at once", err)
			}
		})
	}
}

func TestAWaitingAdmissionAnswersTheContextsCause(t *testing.T) {
	// Arrange.
	o := newAdoptionOrder(dlog.NewTestLogger())
	o.register(map[ids.WorkspaceID]adoptionRank{"viewed": rankViewed, "idle": rankRest})
	cause := errors.New("the daemon is going away")
	ctx, cancel := context.WithCancelCause(context.Background())
	cancel(cause)

	// Act.
	err := o.admit(ctx, "idle")

	// Assert.
	if !errors.Is(err, cause) {
		t.Fatalf("admit = %v, want the context's cause", err)
	}
}

func TestOwnershipNamesTheAdoptionsRankAndPlace(t *testing.T) {
	// Arrange.
	o := newAdoptionOrder(dlog.NewTestLogger())
	o.register(map[ids.WorkspaceID]adoptionRank{"viewed": rankViewed, "idle": rankRest})

	// Act.
	firstRank, first, firstOK := o.owns("viewed")
	secondRank, second, secondOK := o.owns("idle")
	_, _, againOK := o.owns("idle")
	_, _, strayOK := o.owns("straggler")

	// Assert.
	if !firstOK || firstRank != rankViewed || first != 1 {
		t.Fatalf("viewed owns = (%v, %d, %v), want (viewed, 1, true)", firstRank, first, firstOK)
	}
	if !secondOK || secondRank != rankRest || second != 2 {
		t.Fatalf("idle owns = (%v, %d, %v), want (rest, 2, true)", secondRank, second, secondOK)
	}
	if againOK || strayOK {
		t.Fatalf("owns again = %v, unordered = %v, want neither ordered", againOK, strayOK)
	}
}

func TestRegisteringTheOrderRecordsIt(t *testing.T) {
	// Arrange.
	log := dlog.NewTestLogger()
	o := newAdoptionOrder(log)

	// Act.
	o.register(map[ids.WorkspaceID]adoptionRank{"viewed": rankViewed, "busy": rankWorkInFlight, "a": rankRest, "b": rankRest})

	// Assert.
	rec, found := dlog.FindRecord(log.Records(), dlog.RecordQuery{Level: "info", Operation: opAdopt})
	if !found {
		t.Fatalf("no order record; records = %+v", log.Records())
	}
	viewed, _ := rec.Context["viewed"].([]string)
	if len(viewed) != 1 || viewed[0] != "viewed" || rec.Context["work_in_flight"] != 1 || rec.Context["rest"] != 2 {
		t.Fatalf("order record = %+v, want viewed [viewed], 1 in flight, 2 rest", rec.Context)
	}
}
