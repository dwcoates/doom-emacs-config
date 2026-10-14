package footer

import (
	"sync"
	"testing"
	"time"

	"google.golang.org/protobuf/proto"

	frontendv1 "agentrepl/proto/frontend/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/health"
	"agent-repld/internal/ids"
)

// statusEdges records every workspace the status edge was told, in order.
type statusEdges struct {
	mu   sync.Mutex
	told []ids.WorkspaceID
}

func (e *statusEdges) tell(ws ids.WorkspaceID) {
	e.mu.Lock()
	defer e.mu.Unlock()
	e.told = append(e.told, ws)
}

func (e *statusEdges) count() int {
	e.mu.Lock()
	defer e.mu.Unlock()
	return len(e.told)
}

// newEdgeHarness builds a harness whose status edge is recorded.
func newEdgeHarness(t *testing.T) (*harness, *statusEdges) {
	t.Helper()
	edges := &statusEdges{}
	return newHarness(t, WithStatusChanged(edges.tell)), edges
}

func TestStatusAnswersThePublishedStatus(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)

	// Act
	h.r.SetTurn(testWS, &TurnStarted{At: instant, Act: ActPrompt})

	// Assert
	if got, want := statusName(h.r.Status(testWS)), h.status(t); got != want {
		t.Fatalf("Status = %q, want the published %q", got, want)
	}
}

func TestStatusResolvesAWorkspaceTheFooterNeverSawFromTheDaemonFaults(t *testing.T) {
	// Arrange: a daemon-scoped fault stands on every strip.
	h := newHarness(t)
	h.r.OpenFault("", faultOf(t, "fault-1", health.KindPromptsDirMissing, true))

	// Act
	got := h.r.Status(ids.WorkspaceID("ws-never-seen"))

	// Assert
	if name := statusName(got); name != "agent_repl_fault" {
		t.Fatalf("Status = %q, want the daemon fault's agent_repl_fault", name)
	}
}

func TestTheStatusEdgeIsToldWhenTheArmMoves(t *testing.T) {
	// Arrange
	h, edges := newEdgeHarness(t)
	connected(h)
	before := edges.count()

	// Act
	h.r.SetTurn(testWS, &TurnStarted{At: instant, Act: ActPrompt})

	// Assert
	if got := edges.count() - before; got != 1 {
		t.Fatalf("edges told = %d, want 1 for the arm moving to working", got)
	}
}

func TestTheStatusEdgeIsToldWhenOnlyTheStepMoves(t *testing.T) {
	// Arrange: a turn in its submitting step.
	h, edges := newEdgeHarness(t)
	connected(h)
	h.r.SetTurn(testWS, &TurnStarted{At: instant, Act: ActPrompt})
	before := edges.count()

	// Act: the shim takes it, which moves working's step alone.
	h.r.AckTurn(testWS)

	// Assert
	if got := edges.count() - before; got != 1 {
		t.Fatalf("edges told = %d, want 1 for the step moving", got)
	}
}

func TestTheStatusEdgeIsNotToldWhenNothingMovesTheStatus(t *testing.T) {
	// Arrange
	h, edges := newEdgeHarness(t)
	connected(h)
	before := edges.count()

	// Act: a fact that leaves the strip idle.
	h.r.SetStateUnreported(testWS, false)

	// Assert
	if got := edges.count() - before; got != 0 {
		t.Fatalf("edges told = %d, want none: the status did not move", got)
	}
}

func TestADaemonRecordTellsNoStatusEdge(t *testing.T) {
	// Arrange
	h, edges := newEdgeHarness(t)
	connected(h)
	before := edges.count()

	// Act
	h.r.OnWorkspaceRecord(dlog.WorkspaceRecord{
		WorkspaceID: string(testWS), Level: dlog.LevelWarn, Operation: "daemon.x.y", Message: "m"})

	// Assert
	if got := edges.count() - before; got != 0 {
		t.Fatalf("edges told = %d, want none: a teed record moves the line alone", got)
	}
	if hasLevel(h.log.Records(), "error", "daemon.footer.line_moved_status") {
		t.Fatal("a teed record moved the status")
	}
}

func TestStatusOfAnUnboundWorkspaceRecordsNoViolation(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	h.r.Status(ids.WorkspaceID("ws-never-bound"))

	// Assert: a query is not a frame.
	if hasLevel(h.log.Records(), "error", "daemon.footer.unbound_workspace") {
		t.Fatal("a status query for an unbound workspace recorded the unbound-frame violation")
	}
}

func TestADaemonRecordOnAWorkspaceNotYetPublishedRecordsNoViolation(t *testing.T) {
	// Arrange: bound, but nothing published yet.
	h, _ := newEdgeHarness(t)

	// Act
	h.r.OnWorkspaceRecord(dlog.WorkspaceRecord{
		WorkspaceID: string(testWS), Level: dlog.LevelWarn, Operation: "daemon.x.y", Message: "m"})

	// Assert: the first view is no move of the status.
	if hasLevel(h.log.Records(), "error", "daemon.footer.line_moved_status") {
		t.Fatal("a workspace's first view was recorded as a line moving the status")
	}
}

func TestTurnClockAnswersThePublishedClock(t *testing.T) {
	tests := []struct {
		name    string
		arrange func(h *harness)
	}{
		{name: "a running turn", arrange: func(h *harness) {
			h.r.SetTurn(testWS, &TurnStarted{At: instant, Act: ActPrompt})
		}},
		{name: "no turn yet", arrange: func(*harness) {}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			connected(h)

			// Act
			tt.arrange(h)

			// Assert
			if got, want := h.r.TurnClock(testWS), h.view(t).GetStrip().GetClock(); !proto.Equal(got, want) {
				t.Fatalf("TurnClock = %v, want the published %v", got, want)
			}
		})
	}
}

func TestTurnClockResolvesAWorkspaceTheFooterNeverSawAsNoClock(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	got := h.r.TurnClock(ids.WorkspaceID("ws-never-seen"))

	// Assert
	if got == nil || got.TurnStartedAtMs != nil || got.TurnEndedAtMs != nil {
		t.Fatalf("TurnClock = %v, want an empty clock", got)
	}
}

func TestTheStatusEdgeIsToldWhenTheTurnClockMoves(t *testing.T) {
	tests := []struct {
		name string
		pub  publication
		want bool
	}{
		{name: "the clock alone", pub: publication{clockChanged: true}, want: true},
		{name: "the arm alone", pub: publication{armChanged: true}, want: true},
		{name: "the step alone", pub: publication{subChanged: true}, want: true},
		{name: "the line alone", pub: publication{lineChanged: true}, want: false},
		{name: "the merge start alone", pub: publication{mergeStartChanged: true}, want: true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h, edges := newEdgeHarness(t)
			tt.pub.ws = testWS

			// Act
			h.r.tellStatusChanged([]publication{tt.pub})

			// Assert
			if got := edges.count() == 1; got != tt.want {
				t.Fatalf("told = %v, want %v", got, tt.want)
			}
		})
	}
}

func TestObserveClockReportsAChangeOnlyWhenTheClockMoves(t *testing.T) {
	// Arrange
	started := int64(1_000)
	running := &frontendv1.FooterView{Strip: &frontendv1.FooterStrip{Clock: &frontendv1.FooterClock{TurnStartedAtMs: &started}}}
	s := newWSState()

	// Act
	first, again := s.observeClock(running), s.observeClock(running)

	// Assert
	if !first || again {
		t.Fatalf("observeClock = %v then %v, want a change then none", first, again)
	}
}

func TestMergeStartedAtAnswersAMergeTheStripDraws(t *testing.T) {
	queued := instant.Add(-time.Hour)
	tests := []struct {
		name  string
		facts MergeFacts
		want  *int64
	}{
		{name: "a merge waiting in line", facts: MergeFacts{State: "queued", QueuedAt: queued, Step: StepEnqueued, QueuePlace: 1, QueueWaiting: 1}, want: millisOf(queued)},
		{name: "a merge being worked on", facts: MergeFacts{State: "merging", QueuedAt: queued, Step: StepTesting}, want: millisOf(queued)},
		{name: "a failed merge", facts: MergeFacts{State: "failed", QueuedAt: queued, FailedArea: FailedTests}, want: nil},
		{name: "no merge", facts: MergeFacts{State: "none"}, want: nil},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			connected(h)

			// Act
			h.r.SetMerge(testWS, tt.facts)

			// Assert
			got := h.r.MergeStartedAt(testWS)
			if (got == nil) != (tt.want == nil) || (got != nil && *got != *tt.want) {
				t.Fatalf("MergeStartedAt = %v, want %v under the published %q", got, tt.want, h.status(t))
			}
		})
	}
}

func TestMergeStartedAtResolvesAWorkspaceTheFooterNeverSawAsNoMerge(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	got := h.r.MergeStartedAt(ids.WorkspaceID("ws-never-seen"))

	// Assert
	if got != nil {
		t.Fatalf("MergeStartedAt = %d, want no merge", *got)
	}
}

func TestTheStatusEdgeIsToldWhenAMergeStarts(t *testing.T) {
	// Arrange
	h, edges := newEdgeHarness(t)
	connected(h)
	before := edges.count()

	// Act
	h.r.SetMerge(testWS, MergeFacts{State: "merging", QueuedAt: instant, Step: StepTesting})

	// Assert
	if got := edges.count() - before; got != 1 {
		t.Fatalf("edges told = %d, want 1 for the merge appearing", got)
	}
}

func TestObserveMergeStartReportsAChangeOnlyWhenTheStartMoves(t *testing.T) {
	early, late := int64(1_000), int64(2_000)
	tests := []struct {
		name   string
		before *mergeStart
		after  *mergeStart
		want   bool
	}{
		{name: "the first publication", before: nil, after: &mergeStart{}, want: true},
		{name: "still no merge", before: &mergeStart{}, after: &mergeStart{}, want: false},
		{name: "a merge appearing", before: &mergeStart{}, after: &mergeStart{ms: &early}, want: true},
		{name: "a merge leaving", before: &mergeStart{ms: &early}, after: &mergeStart{}, want: true},
		{name: "the same merge", before: &mergeStart{ms: &early}, after: &mergeStart{ms: &early}, want: false},
		{name: "another instant", before: &mergeStart{ms: &early}, after: &mergeStart{ms: &late}, want: true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s := newWSState()
			s.publishedMergeStart = tt.before

			// Act
			got := s.observeMergeStart(tt.after)

			// Assert
			if got != tt.want || s.publishedMergeStart != tt.after {
				t.Fatalf("observeMergeStart = %v, want %v, and the start recorded", got, tt.want)
			}
		})
	}
}

func millisOf(t time.Time) *int64 {
	ms := t.UnixMilli()
	return &ms
}
