package sessioncontroller

import (
	"context"
	"testing"

	corev1 "agentrepl/proto/agentshim/core/v1"
	datav1 "agentrepl/proto/agentshim/data/v1"

	"claude-repld/internal/shimclient"

	"google.golang.org/protobuf/types/known/anypb"
)

// The daemon's half of the two context-cut axes: the edges that reach the SSM.
// The SSM owns what a cut RESOLVES to (internal/ssm/contextcut.go); this file
// covers only that the right edges get there.

// statusEvent is a vendor stream-plane StatusMessage — the ticker the vendor
// opens and closes a compaction with.
func statusEvent(t *testing.T, seq uint64, status string) *corev1.Event {
	t.Helper()
	a, err := anypb.New(&datav1.ClaudeStreamMessage{
		Msg: &datav1.ClaudeStreamMessage_Status{Status: &datav1.StatusMessage{Status: status}},
	})
	if err != nil {
		t.Fatalf("anypb.New: %v", err)
	}
	return &corev1.Event{SessionId: "vendor-uuid", Seq: seq, Payload: &corev1.Event_Vendor{Vendor: a}}
}

// newCutConsumer builds a consumer over a recording applier.
func newCutConsumer(t *testing.T) (*consumer, *fakeApplier) {
	t.Helper()
	applier := &fakeApplier{}
	return newConsumer("ws", "s1", &fakePusher{}, applier, nil, newFakeClearCompactStore(), emptyTurnAccountingStore{},
		func(string, ...any) {}, nil, nil, nil, nil, nil), applier
}

func TestTheVendorStatusMovesTheCompactingAxis(t *testing.T) {
	tests := []struct {
		name     string
		status   string
		wantOpen bool
	}{
		{
			name:     "compacting opens the window",
			status:   "compacting",
			wantOpen: true,
		},
		{
			// The vendor's empty status is its NULL — the window closing — which
			// is the same contract the progress footer folds it under.
			name:     "an empty status closes it",
			status:   "",
			wantOpen: false,
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			c, applier := newCutConsumer(t)

			// Act.
			c.Consume(statusEvent(t, 1, tc.status))

			// Assert.
			cuts := applier.cutsApplied()
			if len(cuts) != 1 {
				t.Fatalf("cut edges = %d, want 1: %+v", len(cuts), cuts)
			}
			if cuts[0].axis != "compacting" || cuts[0].open != tc.wantOpen {
				t.Fatalf("cut = %+v, want the compacting axis open=%t", cuts[0], tc.wantOpen)
			}
		})
	}
}

func TestAVendorEventWithNoStatusMovesNoAxis(t *testing.T) {
	// Arrange — most vendor events carry no status at all, and treating that
	// as an empty status would close a window every assistant message.
	c, applier := newCutConsumer(t)

	// Act.
	c.Consume(assistantEvent(t, 1, "an ordinary answer"))

	// Assert.
	if cuts := applier.cutsApplied(); len(cuts) != 0 {
		t.Fatalf("cut edges = %+v, want none", cuts)
	}
}

func TestTheFirstClassCutEventsCloseTheirAxes(t *testing.T) {
	tests := []struct {
		name     string
		ev       *corev1.Event
		wantAxis string
	}{
		{
			name:     "ContextCleared closes clearing",
			ev:       clearEvent(7, "u-clear"),
			wantAxis: "clearing",
		},
		{
			name:     "ContextCompacted closes compacting",
			ev:       compactEvent(9, "u-compact"),
			wantAxis: "compacting",
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			c, applier := newCutConsumer(t)

			// Act.
			c.Consume(tc.ev)

			// Assert.
			cuts := applier.cutsApplied()
			if len(cuts) != 1 {
				t.Fatalf("cut edges = %d, want 1: %+v", len(cuts), cuts)
			}
			if cuts[0].axis != tc.wantAxis || cuts[0].open {
				t.Fatalf("cut = %+v, want the %s axis closed", cuts[0], tc.wantAxis)
			}
		})
	}
}

// newClearTestManager is newTestManager over an applier the test keeps, since
// the clearing axis is opened by the SESSION CONTROLLER rather than by any event.
func newClearTestManager(t *testing.T) (*Manager, *fakeApplier) {
	t.Helper()
	applier := &fakeApplier{}
	m, err := New(Config{
		Push:              &fakePusher{},
		SSM:               applier,
		Spawner:           &fakeSpawner{},
		Locator:           fakeLocator{m: map[string]string{"ws": "s1"}},
		SeqStore:          &fakeSeqStore{seq: map[string]uint64{}},
		ClearCompactStore: newFakeClearCompactStore(),
		TurnAccountings:   emptyTurnAccountingStore{},
		ProtocolVersion:   "1",
		Source:            stubSource{},
		FileDiagnostics:   fakeFileDiagnosticPersister{},
		newClient:         func(cfg shimclient.Config) sessionClient { return &fakeClient{cfg: cfg} },
	})
	if err != nil {
		t.Fatalf("New: %v", err)
	}
	t.Cleanup(m.Close)
	return m, applier
}

func TestDispatchingAClearOpensTheClearingAxis(t *testing.T) {
	tests := []struct {
		name     string
		text     string
		wantOpen bool
	}{
		{
			// The daemon is the ONLY place that knows a clear has begun.
			name:     "the bare command opens it",
			text:     "/clear",
			wantOpen: true,
		},
		{
			// Whitespace around the command is still the command.
			name:     "surrounding whitespace is still the command",
			text:     "  /clear\n",
			wantOpen: true,
		},
		{
			// An argument means the user asked for something else.
			name:     "an argument is a different prompt",
			text:     "/clear the build cache",
			wantOpen: false,
		},
		{
			name:     "an ordinary prompt opens nothing",
			text:     "hello",
			wantOpen: false,
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			m, applier := newClearTestManager(t)

			// Act.
			if err := m.SubmitPrompt(context.Background(), "ws", "context-cut-request", tc.text, "default", testPromptOrigin); err != nil {
				t.Fatalf("SubmitPrompt: %v", err)
			}

			// Assert.
			cuts := applier.cutsApplied()
			if !tc.wantOpen {
				if len(cuts) != 0 {
					t.Fatalf("cut edges = %+v, want none", cuts)
				}
				return
			}
			if len(cuts) != 1 || cuts[0].axis != "clearing" || !cuts[0].open || cuts[0].workspace != "ws" {
				t.Fatalf("cut edges = %+v, want the clearing axis opened for ws", cuts)
			}
		})
	}
}

func TestASeqlessClearStillClosesTheClearingAxis(t *testing.T) {
	// Arrange — seq 0 costs the event its REPLAY FLOOR, because a floor needs
	// a position. It does not cost it its meaning: the clear still happened,
	// and holding the phase word for a completed cut would wedge the footer on
	// a technicality of store positioning.
	c, applier := newCutConsumer(t)

	// Act.
	c.Consume(clearEvent(0, "u-clear"))

	// Assert.
	cuts := applier.cutsApplied()
	if len(cuts) != 1 || cuts[0].axis != "clearing" || cuts[0].open {
		t.Fatalf("cut edges = %+v, want the clearing axis closed", cuts)
	}
}
