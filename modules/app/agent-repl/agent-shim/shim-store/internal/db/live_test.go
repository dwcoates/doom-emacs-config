package db

import (
	"errors"
	"testing"

	storev1 "agentrepl/proto/store/v1"
)

// ---- LiveWork ----

func TestLiveWorkNeverListsAMainAgent(t *testing.T) {
	// Arrange: a main agent has neither spawn column set. Its liveness is the
	// SESSION's, which the shim knows without asking, so listing it would hand
	// the shim an obligation to resolve against itself.
	d, _ := newStore(t)
	writeOK(t, d, pageEntry("w1", "u1", "agent-main", frameItem(activityFrame("agent-main", "act-1", prose()))))

	// Act
	live, err := d.LiveWork(ctx())

	// Assert
	if err != nil {
		t.Fatalf("LiveWork: %v", err)
	}
	if got := len(live.GetLiveAgents()); got != 0 {
		t.Fatalf("live_agents = %d, want 0", got)
	}
}

func TestLiveWorkListsASpawnedAgentWithNoTerminal(t *testing.T) {
	// Arrange
	d, _ := newStore(t)
	writeOK(t, d, pageEntry("w1", "u1", "agent-main", frameItem(activityFrame("agent-main", "act-1", subagentStart("agent-2")))))

	// Act
	live, err := d.LiveWork(ctx())

	// Assert
	if err != nil {
		t.Fatalf("LiveWork: %v", err)
	}
	if got := len(live.GetLiveAgents()); got != 1 {
		t.Fatalf("live_agents = %d, want 1", got)
	}
	if got := live.GetLiveAgents()[0].GetValue(); got != "agent-2" {
		t.Fatalf("live agent = %q, want agent-2", got)
	}
}

func TestLiveWorkExcludesASpawnedAgentThatEnded(t *testing.T) {
	// Arrange
	d, _ := newStore(t)
	writeOK(t, d, pageEntry("w1", "u1", "agent-main", frameItem(activityFrame("agent-main", "act-1", subagentStart("agent-2")))))

	// Act
	writeOK(t, d, pageEntry("w2", "u2", "agent-2", frameItem(successFrame("agent-2"))))
	live, err := d.LiveWork(ctx())

	// Assert
	if err != nil {
		t.Fatalf("LiveWork: %v", err)
	}
	if got := len(live.GetLiveAgents()); got != 0 {
		t.Fatalf("live_agents = %d, want 0", got)
	}
}

func TestLiveWorkListsADetachedRunWithNoTerminal(t *testing.T) {
	// Arrange
	d, _ := newStore(t)
	writeOK(t, d, bashEntry("w1", "u1", "run-1", bashStart()))

	// Act
	live, err := d.LiveWork(ctx())

	// Assert
	if err != nil {
		t.Fatalf("LiveWork: %v", err)
	}
	if got := len(live.GetLiveDetached()); got != 1 {
		t.Fatalf("live_detached = %d, want 1", got)
	}
	if got := live.GetLiveDetached()[0].GetValue(); got != "run-1" {
		t.Fatalf("live detached = %q, want run-1", got)
	}
}

func TestLiveWorkExcludesADetachedRunThatEnded(t *testing.T) {
	// Arrange
	d, _ := newStore(t)
	writeOK(t, d, bashEntry("w1", "u1", "run-1", bashStart()))

	// Act
	writeOK(t, d, bashEntry("w2", "u1", "run-1", bashSuccess()))
	live, err := d.LiveWork(ctx())

	// Assert
	if err != nil {
		t.Fatalf("LiveWork: %v", err)
	}
	if got := len(live.GetLiveDetached()); got != 0 {
		t.Fatalf("live_detached = %d, want 0", got)
	}
}

func TestLiveWorkNeverListsAWorkflowRunAmongTheDetached(t *testing.T) {
	// Arrange: a workflow is its own arm on the wire, and nothing is routed
	// into the workflow table this wave, so it appears nowhere.
	d, _ := newStore(t)
	writeOK(t, d, pageEntry("w1", "u1", "agent-1", frameItem(detachedFrame("agent-1", createdWork("run-wf", workflowWork())))))

	// Act
	live, err := d.LiveWork(ctx())

	// Assert
	if err != nil {
		t.Fatalf("LiveWork: %v", err)
	}
	if got := len(live.GetLiveDetached()); got != 0 {
		t.Fatalf("live_detached = %d, want 0", got)
	}
	if got := len(live.GetLiveWorkflows()); got != 0 {
		t.Fatalf("live_workflows = %d, want 0 this wave", got)
	}
}

func TestLiveWorkAnswersAnIdleStoreWithEmptyLists(t *testing.T) {
	// Arrange
	d, _ := newStore(t)

	// Act
	live, err := d.LiveWork(ctx())

	// Assert
	if err != nil {
		t.Fatalf("LiveWork: %v", err)
	}
	if len(live.GetLiveAgents())+len(live.GetLiveWorkflows())+len(live.GetLiveDetached()) != 0 {
		t.Fatalf("idle store reported obligations: %+v", live)
	}
}

func TestLiveWorkReportsAStorageFailureOnAClosedDatabase(t *testing.T) {
	// Arrange
	d, s := newStore(t)
	if err := d.Close(); err != nil {
		t.Fatalf("close: %v", err)
	}

	// Act
	_, err := d.LiveWork(ctx())

	// Assert
	if !errors.Is(err, ErrStorage) {
		t.Fatalf("error = %v, want ErrStorage", err)
	}
	s.assertLogged(t, "error", "refused")
}

// ---- Cursors ----

func seedCursor(t *testing.T, d *DB, fileID, path string, offset int64) {
	t.Helper()
	if _, err := d.WriteBatch(ctx(), "sidecar", &storev1.EntryBatch{
		CursorAdvance: &storev1.CursorState{FileId: fileID, Path: path, Offset: offset},
	}); err != nil {
		t.Fatalf("seed cursor: %v", err)
	}
}

func TestCursorsReturnsEveryCursorWhenNoneIsNamed(t *testing.T) {
	// Arrange
	d, _ := newStore(t)
	seedCursor(t, d, "12:34", "/t/a.jsonl", 10)
	seedCursor(t, d, "12:35", "/t/b.jsonl", 20)

	// Act
	cursors, err := d.Cursors(ctx(), nil)

	// Assert
	if err != nil {
		t.Fatalf("Cursors: %v", err)
	}
	if len(cursors) != 2 {
		t.Fatalf("cursors = %d, want 2", len(cursors))
	}
}

func TestCursorsReturnsOnlyTheNamedFile(t *testing.T) {
	// Arrange
	d, _ := newStore(t)
	seedCursor(t, d, "12:34", "/t/a.jsonl", 10)
	seedCursor(t, d, "12:35", "/t/b.jsonl", 20)
	fileID := "12:35"

	// Act
	cursors, err := d.Cursors(ctx(), &fileID)

	// Assert
	if err != nil {
		t.Fatalf("Cursors: %v", err)
	}
	if len(cursors) != 1 || cursors[0].GetFileId() != "12:35" {
		t.Fatalf("cursors = %+v, want only 12:35", cursors)
	}
	if cursors[0].GetOffset() != 20 {
		t.Fatalf("offset = %d, want 20", cursors[0].GetOffset())
	}
}

func TestCursorsAnswersAFreshStoreWithNothing(t *testing.T) {
	// Arrange: empty is the fresh-store answer, and the sidecar starts every
	// file from zero. It is a SUCCESS, never a refusal.
	d, _ := newStore(t)

	// Act
	cursors, err := d.Cursors(ctx(), nil)

	// Assert
	if err != nil {
		t.Fatalf("Cursors refused a fresh store: %v", err)
	}
	if len(cursors) != 0 {
		t.Fatalf("cursors = %d, want 0", len(cursors))
	}
}

func TestCursorsAnswersAnUnknownFileWithNothing(t *testing.T) {
	// Arrange
	d, _ := newStore(t)
	unknown := "99:99"

	// Act
	cursors, err := d.Cursors(ctx(), &unknown)

	// Assert
	if err != nil {
		t.Fatalf("Cursors: %v", err)
	}
	if len(cursors) != 0 {
		t.Fatalf("cursors = %d, want 0", len(cursors))
	}
}

func TestCursorsRefusesAnEmptyFileIdentity(t *testing.T) {
	// Arrange: asking for every cursor is expressed by ABSENCE, so a present
	// but empty value is a caller bug rather than a synonym for "all".
	d, s := newStore(t)
	empty := ""

	// Act
	_, err := d.Cursors(ctx(), &empty)

	// Assert
	if !errors.Is(err, ErrInvalid) {
		t.Fatalf("error = %v, want ErrInvalid", err)
	}
	s.assertTracedRefusal(t, "file_id is present with an empty value")
}

func TestCursorsPreservesTheCarry(t *testing.T) {
	// Arrange: the carry is what makes a line split across two reads parse
	// once and whole.
	d, _ := newStore(t)
	if _, err := d.WriteBatch(ctx(), "sidecar", &storev1.EntryBatch{
		CursorAdvance: &storev1.CursorState{FileId: "12:34", Path: "/t/a.jsonl", Offset: 5, Carry: []byte(`{"partial":`)},
	}); err != nil {
		t.Fatalf("seed: %v", err)
	}

	// Act
	cursors, err := d.Cursors(ctx(), nil)

	// Assert
	if err != nil {
		t.Fatalf("Cursors: %v", err)
	}
	if got := string(cursors[0].GetCarry()); got != `{"partial":` {
		t.Fatalf("carry = %q", got)
	}
}

func TestLiveWorkOrdersSpawnedAgentsByTheStoresOwnArrivalNotTheProducersInstant(t *testing.T) {
	// Arrange: two spawns whose PRODUCER instants disagree with the order the
	// store actually heard them in. agent-late is announced second but carries
	// the earlier producer instant, which is routine: the shim stamps with its
	// own Date.now() on the stream plane and the sidecar stamps with the
	// vendor's transcript timestamp on the file plane, and both planes mint the
	// same key for the same unit on purpose.
	clock := int64(1_000)
	d := newStoreWithClock(t, func() int64 { return clock })
	writeOK(t, d, pageEntry("w1", "u1", "agent-main", frameItem(activityFrame("agent-main", "act-1", subagentStartAt("agent-early", 9_000)))))

	// Act
	clock = 2_000
	writeOK(t, d, pageEntry("w2", "u2", "agent-main", frameItem(activityFrame("agent-main", "act-2", subagentStartAt("agent-late", 5_000)))))
	live, err := d.LiveWork(ctx())

	// Assert: the store's own write order, not the producers' stamps.
	if err != nil {
		t.Fatalf("LiveWork: %v", err)
	}
	got := []string{}
	for _, a := range live.GetLiveAgents() {
		got = append(got, a.GetValue())
	}
	if len(got) != 2 || got[0] != "agent-early" || got[1] != "agent-late" {
		t.Fatalf("live_agents = %v, want [agent-early agent-late]", got)
	}
}

func TestLiveWorkDoesNotSortAnAgentFirstBecauseItsProducerStatedNoInstant(t *testing.T) {
	// Arrange: the sidecar's parseInstant answers 0 for a vendor transcript
	// record whose `timestamp` is missing or unparseable. started_at_ms is NOT
	// NULL, so a 0 was indistinguishable from an instant and sorted ahead of
	// every real one.
	clock := int64(1_000)
	d := newStoreWithClock(t, func() int64 { return clock })
	writeOK(t, d, pageEntry("w1", "u1", "agent-main", frameItem(activityFrame("agent-main", "act-1", subagentStartAt("agent-first", 9_000)))))

	// Act
	clock = 2_000
	writeOK(t, d, pageEntry("w2", "u2", "agent-main", frameItem(activityFrame("agent-main", "act-2", subagentStartAt("agent-unstamped", 0)))))
	live, err := d.LiveWork(ctx())

	// Assert
	if err != nil {
		t.Fatalf("LiveWork: %v", err)
	}
	if got := live.GetLiveAgents()[0].GetValue(); got != "agent-first" {
		t.Fatalf("first live agent = %q, want agent-first", got)
	}
}

func TestASpawnFrameDoesNotMoveTheStartOfAnAgentTheStoreAlreadyHeardFrom(t *testing.T) {
	// Arrange: the created agent speaks BEFORE the spawn that announced it is
	// applied, which the two planes' arrival race makes routine. First sight
	// stamps the row with the store's clock.
	clock := int64(1_000)
	d := newStoreWithClock(t, func() int64 { return clock })
	writeOK(t, d, pageEntry("w1", "u1", "agent-2", frameItem(activityFrame("agent-2", "act-1", prose()))))

	// Act
	clock = 2_000
	writeOK(t, d, pageEntry("w2", "u2", "agent-main", frameItem(activityFrame("agent-main", "act-2", subagentStartAt("agent-2", 9_000)))))

	// Assert: first sight stands. The spawn supplies the metadata and not the
	// instant, so an agent's start never moves forward or backward as later
	// frames about it arrive.
	if got := scalar[int64](t, d, `SELECT started_at_ms FROM agent WHERE agent_id = 'agent-2'`); got != 1_000 {
		t.Fatalf("started_at_ms = %d, want first sight 1000", got)
	}
	if got := scalar[string](t, d, `SELECT spawned_by_agent FROM agent WHERE agent_id = 'agent-2'`); got != "agent-main" {
		t.Fatalf("spawned_by_agent = %q, want agent-main", got)
	}
}
