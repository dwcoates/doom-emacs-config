package promptqueue

import (
	"context"
	"errors"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
	shimv1 "agentrepl/proto/shim/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/wsm"
)

func TestHandOverNamesTheMainAgentTheShimNamed(t *testing.T) {
	tests := []struct {
		name  string
		agent string
		want  []string
	}{
		{name: "an agent named", agent: "main-1", want: []string{"main-1"}},
		{name: "no agent named", agent: "", want: nil},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			w := &fakeWatcher{}
			success := &shimv1.StartTurnSuccess{Prompt: &conversationv1.AgentPrompt{
				Id: &conversationv1.TurnId{Value: "t1"}, Agent: &conversationv1.AgentId{Value: tt.agent},
			}}

			// Act
			handOver(theWorkspace, success, w)

			// Assert
			if got := w.mainAgents; len(got) != len(tt.want) || (len(got) == 1 && got[0] != tt.want[0]) {
				t.Fatalf("main agents = %v, want %v", got, tt.want)
			}
		})
	}
}

func TestHandOverGivesTheWatcherTheAcceptedTurn(t *testing.T) {
	// Arrange
	w := &fakeWatcher{}
	success := &shimv1.StartTurnSuccess{Prompt: &conversationv1.AgentPrompt{Id: &conversationv1.TurnId{Value: "t1"}}}

	// Act
	handOver(theWorkspace, success, w)

	// Assert
	if len(w.opened) != 1 || w.opened[0].GetId().GetValue() != "t1" {
		t.Fatalf("opened = %v, want t1 handed over", w.opened)
	}
}

func TestRetireDeliveredTombstonesTheHoldAndClearsTheHead(t *testing.T) {
	// Arrange
	h := newHarness(t)
	running(t, h, "running-turn", "the running work")
	if _, err := h.q.Submit(context.Background(), submission("t1", "a follow-up")); err != nil {
		t.Fatalf("Submit: %v", err)
	}
	h.q.waitForClassifications()
	head := ids.TurnID("t1")
	h.q.state(theWorkspace).head = &head

	// Act
	err := h.q.retireDelivered(context.Background(), theWorkspace, "t1", dlog.NewTestLogger())

	// Assert
	held, _, _ := h.db.HeldPromptByTurn(context.Background(), "t1")
	if err != nil || held.Tombstone == nil || held.Tombstone.Kind != tombstoneDelivered || h.q.state(theWorkspace).head != nil {
		t.Fatalf("err = %v, hold = %+v, head = %v; want it retired as delivered and the head cleared", err, held, h.q.state(theWorkspace).head)
	}
}

func TestRetireDeliveredSurfacesAFailedTombstoneAndLogsIt(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.db.tombstoneErr = errors.New("disk full")

	// Act
	err := h.q.retireDelivered(context.Background(), theWorkspace, "t1", h.log.Global())

	// Assert
	if err == nil || !logged(h.log.Records(), dlog.LevelError, opDeliver, "the hold was delivered but not retired") {
		t.Fatalf("err = %v, records = %+v; want the failure surfaced and logged at error", err, h.log.Records())
	}
}

func TestVerdictStaleLockedAnswersWhetherTheContentMoved(t *testing.T) {
	tests := []struct {
		name      string
		judged    uint64
		wantStale bool
	}{
		{name: "the content still stands", judged: 2, wantStale: false},
		{name: "an edit replaced the content", judged: 1, wantStale: true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			state := &wsState{epochs: map[ids.TurnID]uint64{"t1": 2}}
			log := dlog.NewTestSurfaces()

			// Act
			stale := state.verdictStaleLocked("t1", tt.judged, wsm.Classification{Arm: wsm.ArmInterject}, log.Global())

			// Assert
			logged := false
			for _, r := range log.Records() {
				logged = logged || (r.Level == dlog.LevelInfo && r.Operation == opClassify && r.Context["content_epoch"] == uint64(2))
			}
			if stale != tt.wantStale || logged != tt.wantStale {
				t.Fatalf("stale = %v, logged = %v; want both %v", stale, logged, tt.wantStale)
			}
		})
	}
}
