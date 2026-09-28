package db

import (
	"errors"
	"strings"
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
	live, err := d.LiveWork(ctx(), "agent-main")

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
	live, err := d.LiveWork(ctx(), "agent-main")

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
	live, err := d.LiveWork(ctx(), "agent-main")

	// Assert
	if err != nil {
		t.Fatalf("LiveWork: %v", err)
	}
	if got := len(live.GetLiveAgents()); got != 0 {
		t.Fatalf("live_agents = %d, want 0", got)
	}
}

func TestLiveWorkListsADetachedRunWithNoTerminal(t *testing.T) {
	// Arrange: the session's main agent announced the run, so it owns it.
	d, _ := newStore(t)
	writeOK(t, d, pageEntry("w0", "u0", "agent-main", frameItem(detachedFrame("agent-main", createdWork("run-1", bashWork())))))
	writeOK(t, d, bashEntry("w1", "u1", "run-1", bashStart()))

	// Act
	live, err := d.LiveWork(ctx(), "agent-main")

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
	writeOK(t, d, pageEntry("w0", "u0", "agent-main", frameItem(detachedFrame("agent-main", createdWork("run-1", bashWork())))))
	writeOK(t, d, bashEntry("w1", "u1", "run-1", bashStart()))

	// Act
	writeOK(t, d, bashEntry("w2", "u1", "run-1", bashSuccess()))
	live, err := d.LiveWork(ctx(), "agent-main")

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
	writeOK(t, d, pageEntry("w1", "u1", "agent-main", frameItem(detachedFrame("agent-main", createdWork("run-wf", workflowWork())))))

	// Act
	live, err := d.LiveWork(ctx(), "agent-main")

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
	live, err := d.LiveWork(ctx(), "agent-main")

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
	_, err := d.LiveWork(ctx(), "agent-main")

	// Assert
	if !errors.Is(err, ErrStorage) {
		t.Fatalf("error = %v, want ErrStorage", err)
	}
	s.assertLogged(t, "error", "refused")
}

// ---- LiveWork: the session scope ----

// liveIDs flattens an answer to its agent and detached ids, for an assertion
// about exactly which obligations a session was handed.
func liveIDs(live *storev1.GetLiveWorkSuccess) (agents, detached []string) {
	for _, a := range live.GetLiveAgents() {
		agents = append(agents, a.GetValue())
	}
	for _, w := range live.GetLiveDetached() {
		detached = append(detached, w.GetValue())
	}
	return agents, detached
}

// seedTwoSessions writes two sessions' live work into one store: each main
// agent spawned one subagent and announced one shell run.
func seedTwoSessions(t *testing.T, d *DB) {
	t.Helper()
	writeOK(t, d, pageEntry("a1", "a-u1", "main-a", frameItem(activityFrame("main-a", "a-act-1", subagentStart("sub-a")))))
	writeOK(t, d, pageEntry("a2", "a-u2", "main-a", frameItem(detachedFrame("main-a", createdWork("run-a", bashWork())))))
	writeOK(t, d, pageEntry("b1", "b-u1", "main-b", frameItem(activityFrame("main-b", "b-act-1", subagentStart("sub-b")))))
	writeOK(t, d, pageEntry("b2", "b-u2", "main-b", frameItem(detachedFrame("main-b", createdWork("run-b", bashWork())))))
}

func TestLiveWorkAnswersEachSessionOnlyItsOwnObligations(t *testing.T) {
	tests := []struct {
		name         string
		session      string
		wantAgents   []string
		wantDetached []string
	}{
		{name: "session A", session: "main-a", wantAgents: []string{"sub-a"}, wantDetached: []string{"run-a"}},
		{name: "session B", session: "main-b", wantAgents: []string{"sub-b"}, wantDetached: []string{"run-b"}},
	}
	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			// Arrange: ONE store holding two sessions' live work, which is the
			// host's ordinary state — every workspace shares the store.
			d, _ := newStore(t)
			seedTwoSessions(t, d)

			// Act
			live, err := d.LiveWork(ctx(), test.session)

			// Assert
			if err != nil {
				t.Fatalf("LiveWork: %v", err)
			}
			agents, detached := liveIDs(live)
			if len(agents) != 1 || agents[0] != test.wantAgents[0] {
				t.Fatalf("live_agents = %v, want %v", agents, test.wantAgents)
			}
			if len(detached) != 1 || detached[0] != test.wantDetached[0] {
				t.Fatalf("live_detached = %v, want %v", detached, test.wantDetached)
			}
		})
	}
}

func TestLiveWorkIncludesANestedSubagentOfTheSession(t *testing.T) {
	// Arrange: main spawned sub-1, which spawned sub-2.
	d, _ := newStore(t)
	writeOK(t, d, pageEntry("w1", "u1", "agent-main", frameItem(activityFrame("agent-main", "act-1", subagentStart("sub-1")))))
	writeOK(t, d, pageEntry("w2", "u2", "sub-1", frameItem(activityFrame("sub-1", "act-2", subagentStart("sub-2")))))

	// Act
	live, err := d.LiveWork(ctx(), "agent-main")

	// Assert
	if err != nil {
		t.Fatalf("LiveWork: %v", err)
	}
	agents, _ := liveIDs(live)
	if len(agents) != 2 || agents[0] != "sub-1" || agents[1] != "sub-2" {
		t.Fatalf("live_agents = %v, want [sub-1 sub-2]", agents)
	}
}

func TestLiveWorkIncludesASubagentWhoseSpawnWasDeliveredOnlySettled(t *testing.T) {
	// Arrange: the file plane's shape for a session no shim watched — the
	// subagent's own frames first, then its spawn, stated only as the success.
	d, _ := newStore(t)
	writeOK(t, d, pageEntry("w1", "u1", "toolu_sub", frameItem(activityFrame("toolu_sub", "act-1", prose()))))
	writeOK(t, d, pageEntry("w2", "u2", "agent-main", frameItem(activityFrame("agent-main", "toolu_sub", subagentSuccess("toolu_sub")))))

	// Act
	live, err := d.LiveWork(ctx(), "agent-main")

	// Assert
	if err != nil {
		t.Fatalf("LiveWork: %v", err)
	}
	if agents, _ := liveIDs(live); len(agents) != 1 || agents[0] != "toolu_sub" {
		t.Fatalf("live_agents = %v, want [toolu_sub]", agents)
	}
}

func TestLiveWorkIncludesDetachedWorkOwnedByANestedSubagent(t *testing.T) {
	// Arrange: the shell run was announced by the SUBAGENT, not the main agent.
	d, _ := newStore(t)
	writeOK(t, d, pageEntry("w1", "u1", "agent-main", frameItem(activityFrame("agent-main", "act-1", subagentStart("sub-1")))))
	writeOK(t, d, pageEntry("w2", "u2", "sub-1", frameItem(detachedFrame("sub-1", createdWork("run-1", bashWork())))))

	// Act
	live, err := d.LiveWork(ctx(), "agent-main")

	// Assert
	if err != nil {
		t.Fatalf("LiveWork: %v", err)
	}
	if _, detached := liveIDs(live); len(detached) != 1 || detached[0] != "run-1" {
		t.Fatalf("live_detached = %v, want [run-1]", detached)
	}
}

func TestLiveWorkIncludesAnAgentSpawnedThroughTheSessionsWorkflowAnnouncement(t *testing.T) {
	// Arrange: the main agent announced a workflow run, and an agent names that
	// run as its spawner. No producer writes spawned_by_workflow this wave, so
	// the row is seeded directly; the lineage rule must still hold for it.
	d, _ := newStore(t)
	writeOK(t, d, pageEntry("w1", "u1", "agent-main", frameItem(detachedFrame("agent-main", createdWork("wf-1", workflowWork())))))
	if _, err := d.sql.ExecContext(ctx(), `INSERT INTO agent (agent_id, spawned_by_workflow, started_at_ms) VALUES ('wf-agent', 'wf-1', 1)`); err != nil {
		t.Fatalf("seed workflow agent: %v", err)
	}

	// Act
	live, err := d.LiveWork(ctx(), "agent-main")

	// Assert
	if err != nil {
		t.Fatalf("LiveWork: %v", err)
	}
	if agents, _ := liveIDs(live); len(agents) != 1 || agents[0] != "wf-agent" {
		t.Fatalf("live_agents = %v, want [wf-agent]", agents)
	}
}

func TestLiveWorkIncludesAnAgentSpawnedThroughAWorkflowRowTheSessionStarted(t *testing.T) {
	// Arrange: the workflow table's own spawner column is the other declared
	// home of a workflow's lineage. Nothing routes into it this wave, so both
	// rows are seeded directly.
	d, _ := newStore(t)
	writeOK(t, d, pageEntry("w1", "u1", "agent-main", frameItem(activityFrame("agent-main", "act-1", prose()))))
	if _, err := d.sql.ExecContext(ctx(), `INSERT INTO workflow (run_agent_id, spawner_agent, started_at_ms) VALUES ('wf-run', 'agent-main', 1)`); err != nil {
		t.Fatalf("seed workflow: %v", err)
	}
	if _, err := d.sql.ExecContext(ctx(), `INSERT INTO agent (agent_id, spawned_by_workflow, started_at_ms) VALUES ('wf-agent', 'wf-run', 1)`); err != nil {
		t.Fatalf("seed workflow agent: %v", err)
	}

	// Act
	live, err := d.LiveWork(ctx(), "agent-main")

	// Assert
	if err != nil {
		t.Fatalf("LiveWork: %v", err)
	}
	if agents, _ := liveIDs(live); len(agents) != 1 || agents[0] != "wf-agent" {
		t.Fatalf("live_agents = %v, want [wf-agent]", agents)
	}
}

func TestLiveWorkExcludesAnotherSessionsWorkflowAgent(t *testing.T) {
	// Arrange: the workflow was announced by ANOTHER session's main agent.
	d, _ := newStore(t)
	writeOK(t, d, pageEntry("w1", "u1", "main-b", frameItem(detachedFrame("main-b", createdWork("wf-1", workflowWork())))))
	if _, err := d.sql.ExecContext(ctx(), `INSERT INTO agent (agent_id, spawned_by_workflow, started_at_ms) VALUES ('wf-agent', 'wf-1', 1)`); err != nil {
		t.Fatalf("seed workflow agent: %v", err)
	}

	// Act
	live, err := d.LiveWork(ctx(), "main-a")

	// Assert
	if err != nil {
		t.Fatalf("LiveWork: %v", err)
	}
	if agents, _ := liveIDs(live); len(agents) != 0 {
		t.Fatalf("live_agents = %v, want none", agents)
	}
}

func TestLiveWorkRefusesAnEmptySession(t *testing.T) {
	// Arrange: the store is shared, so an unscoped read would hand this caller
	// every other session's obligations to close.
	d, s := newStore(t)

	// Act
	_, err := d.LiveWork(ctx(), "")

	// Assert
	if !errors.Is(err, ErrInvalid) {
		t.Fatalf("error = %v, want ErrInvalid", err)
	}
	if site := RefusalSite(err); site != SiteSessionEmpty {
		t.Fatalf("site = %q, want %q", site, SiteSessionEmpty)
	}
	s.assertTracedRefusal(t, "unscoped live-work read")
}

func TestLiveWorkExcludesAndLogsALiveDetachedRunWithNoOwner(t *testing.T) {
	// Arrange: a shell run's own frame arrived with no announcement, so the
	// record names no owner and no session's lineage reaches it.
	d, s := newStore(t)
	writeOK(t, d, bashEntry("w1", "u1", "run-orphan", bashStart()))

	// Act
	live, err := d.LiveWork(ctx(), "agent-main")

	// Assert
	if err != nil {
		t.Fatalf("LiveWork: %v", err)
	}
	if _, detached := liveIDs(live); len(detached) != 0 {
		t.Fatalf("live_detached = %v, want none — an unowned row is never guessed into a session", detached)
	}
	s.assertLogged(t, "error", "detached_work:run-orphan")
}

func TestLiveWorkLogsALiveAgentWhoseSpawnerTheRecordDoesNotHold(t *testing.T) {
	// Arrange: the spawn column names an agent with no row.
	d, s := newStore(t)
	if _, err := d.sql.ExecContext(ctx(), `INSERT INTO agent (agent_id, spawned_by_agent, started_at_ms) VALUES ('dangling', 'nobody', 1)`); err != nil {
		t.Fatalf("seed dangling agent: %v", err)
	}

	// Act
	if _, err := d.LiveWork(ctx(), "agent-main"); err != nil {
		t.Fatalf("LiveWork: %v", err)
	}

	// Assert
	s.assertLogged(t, "error", "agent:dangling")
}

func TestLiveWorkLogsALiveAgentWhoseWorkflowTheRecordDoesNotHold(t *testing.T) {
	// Arrange: the workflow column names neither a workflow nor a detached row.
	d, s := newStore(t)
	if _, err := d.sql.ExecContext(ctx(), `INSERT INTO agent (agent_id, spawned_by_workflow, started_at_ms) VALUES ('wf-dangling', 'no-such-run', 1)`); err != nil {
		t.Fatalf("seed dangling workflow agent: %v", err)
	}

	// Act
	if _, err := d.LiveWork(ctx(), "agent-main"); err != nil {
		t.Fatalf("LiveWork: %v", err)
	}

	// Assert
	s.assertLogged(t, "error", "agent:wf-dangling")
}

func TestLiveWorkWritesNoErrorWhenEveryObligationHasAnOwner(t *testing.T) {
	// Arrange
	d, s := newStore(t)
	seedTwoSessions(t, d)

	// Act
	if _, err := d.LiveWork(ctx(), "main-a"); err != nil {
		t.Fatalf("LiveWork: %v", err)
	}

	// Assert: another session's owned work is out of scope, not a gap.
	if errs := recordsAtLevel(t, s, "error"); len(errs) != 0 {
		t.Fatalf("error records = %v, want none", errs)
	}
}

func TestLiveWorkReportsAStorageFailureWhenTheUnscopedScanFails(t *testing.T) {
	// Arrange: the gap scan is the first statement to read detached_work.kind,
	// so breaking that column fails it and nothing before it.
	d, s := newStore(t)
	if _, err := d.sql.ExecContext(ctx(), `ALTER TABLE detached_work RENAME COLUMN kind TO kind_gone`); err != nil {
		t.Fatalf("break the column: %v", err)
	}

	// Act
	_, err := d.LiveWork(ctx(), "agent-main")

	// Assert
	if !errors.Is(err, ErrStorage) {
		t.Fatalf("error = %v, want ErrStorage", err)
	}
	if !strings.Contains(err.Error(), "scanning live work no session's lineage reaches") {
		t.Fatalf("error = %v, want the failure of the statement this test broke", err)
	}
	s.assertLogged(t, "error", "refused")
}

func TestLiveWorkReportsAStorageFailureWhenTheDetachedScanFails(t *testing.T) {
	// Arrange: announced_at_ms is read only by the scoped detached scan.
	d, s := newStore(t)
	if _, err := d.sql.ExecContext(ctx(), `ALTER TABLE detached_work RENAME COLUMN announced_at_ms TO announced_gone`); err != nil {
		t.Fatalf("break the column: %v", err)
	}

	// Act
	_, err := d.LiveWork(ctx(), "agent-main")

	// Assert
	if !errors.Is(err, ErrStorage) {
		t.Fatalf("error = %v, want ErrStorage", err)
	}
	if !strings.Contains(err.Error(), "scanning live detached work") {
		t.Fatalf("error = %v, want the failure of the statement this test broke", err)
	}
	s.assertLogged(t, "error", "refused")
}

// ---- Cursors ----

func seedCursor(t *testing.T, d *DB, fileID, path string, offset int64) {
	t.Helper()
	if _, err := d.WriteBatch(ctx(), "sidecar", WriteInteractive, &storev1.EntryBatch{
		CursorAdvance: &storev1.CursorState{FileId: fileID, Path: path, Offset: offset, Conversion: currentConversion()},
	}, nil); err != nil {
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
	if _, err := d.WriteBatch(ctx(), "sidecar", WriteInteractive, &storev1.EntryBatch{
		CursorAdvance: &storev1.CursorState{FileId: "12:34", Path: "/t/a.jsonl", Offset: 5, Carry: []byte(`{"partial":`), Conversion: currentConversion()},
	}, nil); err != nil {
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
	live, err := d.LiveWork(ctx(), "agent-main")

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
	live, err := d.LiveWork(ctx(), "agent-main")

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

// ---- the live-work statements seek real indexes ----

// liveWorkStatements is every statement GetLiveWork runs, as production text
// with representative arguments.
var liveWorkStatements = []struct {
	name      string
	statement string
	args      []any
}{
	{name: "the live agents listing", statement: liveAgentsSQL, args: []any{"agent-main", "agent-main"}},
	{name: "the live detached listing", statement: liveDetachedSQL, args: []any{"agent-main", detachedKindWorkflow}},
	{name: "the unreachable-obligation scan", statement: liveWorkGapsSQL, args: []any{detachedKindWorkflow}},
}

func TestEveryLiveWorkStatementBuildsNoAutomaticIndex(t *testing.T) {
	for _, test := range liveWorkStatements {
		t.Run(test.name, func(t *testing.T) {
			// Arrange
			d, _ := newStore(t)

			// Act
			plan := queryPlan(t, d, test.statement, test.args...)

			// Assert
			assertNoAutomaticIndex(t, test.name, plan)
		})
	}
}

func TestTheSessionLineageSeeksEachLineageIndex(t *testing.T) {
	for _, index := range lineageIndexes {
		t.Run(index.name, func(t *testing.T) {
			// Arrange
			d, _ := newStore(t)

			// Act
			plan := queryPlan(t, d, liveAgentsSQL, "agent-main", "agent-main")

			// Assert
			if !strings.Contains(plan, "USING INDEX "+index.name+" ") {
				t.Fatalf("the lineage walk does not seek %s:\n%s", index.name, plan)
			}
		})
	}
}

// ---- the cursor's conversion bookkeeping ----

func TestCursorsServeTheConversionACurrentAdvanceStated(t *testing.T) {
	// Arrange
	d, _ := newStore(t)
	healBatch(t, d, 10, 2, nil)

	// Act
	cursors, err := d.Cursors(ctx(), nil)

	// Assert
	if err != nil {
		t.Fatalf("Cursors: %v", err)
	}
	conv := cursors[0].GetConversion()
	if conv.GetVersion() != 2 || conv.GetCurrent() == nil {
		t.Fatalf("conversion = %v, want version 2, current", conv)
	}
}

func TestCursorsServeAHealInProgressWithItsThrough(t *testing.T) {
	// Arrange: a restart mid-heal resumes from exactly this.
	d, _ := newStore(t)
	cursor := cursorAt(10, 2)
	cursor.Conversion.State = &storev1.CursorConversion_Healing{Healing: &storev1.CursorConversionHealing{Through: 9000}}
	if _, err := d.WriteBatch(ctx(), "test-sidecar", WriteBulk, &storev1.EntryBatch{CursorAdvance: cursor}, nil); err != nil {
		t.Fatalf("WriteBatch: %v", err)
	}

	// Act
	cursors, err := d.Cursors(ctx(), nil)

	// Assert
	if err != nil {
		t.Fatalf("Cursors: %v", err)
	}
	if got := cursors[0].GetConversion().GetHealing().GetThrough(); got != 9000 {
		t.Fatalf("healing.through = %d, want 9000", got)
	}
}

func TestCursorsServeAPreVersioningCursorWithNoConversion(t *testing.T) {
	// Arrange: a cursor the live store already held before the bookkeeping
	// existed has no cursor_conversion row.
	d, _ := newStore(t)
	healBatch(t, d, 10, 2, nil)
	if _, err := d.sql.Exec(`DELETE FROM cursor_conversion`); err != nil {
		t.Fatalf("removing the bookkeeping row: %v", err)
	}

	// Act
	cursors, err := d.Cursors(ctx(), nil)

	// Assert
	if err != nil {
		t.Fatalf("Cursors: %v", err)
	}
	if cursors[0].GetConversion() != nil {
		t.Fatalf("conversion = %v, want unset", cursors[0].GetConversion())
	}
}
