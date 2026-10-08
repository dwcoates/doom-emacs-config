package footer

import (
	"sync"
	"testing"

	"claude-repld/internal/dlog"
	"claude-repld/internal/health"
	"claude-repld/internal/ids"
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
