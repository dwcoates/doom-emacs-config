package ssm

import (
	"database/sql"
	"fmt"
	"path/filepath"
	"sync"
	"testing"
	"time"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"
)

// seedSyntheticFleet writes a state store of PRODUCTION SHAPE so a snapshot can
// be timed at the scale the defect was observed at: 161 workspaces, ~22k
// workspace_state rows (~140 per workspace), ~10.5k session_connectivity rows,
// one open turn claim per workspace.
func seedSyntheticFleet(t *testing.T, db *sql.DB, workspaces, rowsPer int) {
	t.Helper()
	tx, err := db.Begin()
	if err != nil {
		t.Fatalf("begin: %v", err)
	}
	defer tx.Rollback()
	tokens := []string{sigThinking, sigDone, sigReady, sigTaskStarted, sigTaskEnded}
	for w := 0; w < workspaces; w++ {
		ws := fmt.Sprintf("/w/%03d", w)
		sid := fmt.Sprintf("s-%03d", w)
		at := int64(1)
		if _, err := tx.Exec(`INSERT INTO workspace_state(workspace, session_id, state, cause_kind, cause_seq, at, task_id, event_session_id)
			VALUES (?,?,?,?,?,?,?,?)`, ws, sid, sigWired, causeWired, 0, at, nil, sid); err != nil {
			t.Fatalf("seed wired: %v", err)
		}
		at++
		for r := 0; r < rowsPer; r++ {
			tok := tokens[r%len(tokens)]
			var taskID any
			if tok == sigTaskStarted || tok == sigTaskEnded {
				taskID = fmt.Sprintf("t-%d", r/len(tokens))
			}
			if _, err := tx.Exec(`INSERT INTO workspace_state(workspace, session_id, state, cause_kind, cause_seq, at, task_id, event_session_id)
				VALUES (?,?,?,?,?,?,?,?)`, ws, sid, tok, causeWired, r, at, taskID, sid); err != nil {
				t.Fatalf("seed row: %v", err)
			}
			at++
		}
		// Retired controller generations plus the current operational one.
		for g := 0; g < 20; g++ {
			gen := fmt.Sprintf("g-%03d-%02d", w, g)
			for _, st := range []SessionConnectivity{SessionConnectivityConnecting, SessionConnectivityOperational, SessionConnectivityHibernated} {
				if _, err := tx.Exec(`INSERT INTO session_connectivity(
					workspace, agent_repl_session_id, controller_generation_id, state, cause_kind, at)
					VALUES (?,?,?,?,?,?)`, ws, sid, gen, st, "test", at); err != nil {
					t.Fatalf("seed connectivity: %v", err)
				}
				at++
			}
		}
		gen := fmt.Sprintf("g-%03d-cur", w)
		for _, st := range []SessionConnectivity{SessionConnectivityConnecting, SessionConnectivityOperational} {
			if _, err := tx.Exec(`INSERT INTO session_connectivity(
				workspace, agent_repl_session_id, controller_generation_id, state, cause_kind, at)
				VALUES (?,?,?,?,?,?)`, ws, sid, gen, st, "test", at); err != nil {
				t.Fatalf("seed connectivity: %v", err)
			}
			at++
		}
		if _, err := tx.Exec(`INSERT INTO turn_lifecycle_claim(
			workspace, claimant_session_id, turn_id, start_seq, start_event_session_id)
			VALUES (?,?,?,?,?)`, ws, sid, fmt.Sprintf("turn-%03d", w), 1, sid); err != nil {
			t.Fatalf("seed claim: %v", err)
		}
	}
	if err := tx.Commit(); err != nil {
		t.Fatalf("commit: %v", err)
	}
}

// openFleetTest opens a Manager over a synthetic fleet of the given shape.
func openFleetTest(t *testing.T, workspaces, rowsPer int) *Manager {
	t.Helper()
	db, err := openDB(filepath.Join(t.TempDir(), "state.db"), t.Logf)
	if err != nil {
		t.Fatalf("openDB: %v", err)
	}
	t.Cleanup(func() { db.Close() })
	seedSyntheticFleet(t, db, workspaces, rowsPer)
	m, err := Open(Options{DB: db, Logf: func(string, ...any) {}})
	if err != nil {
		t.Fatalf("Open: %v", err)
	}
	t.Cleanup(func() { m.Close() })
	return m
}

// TestSnapshotAtFleetScaleCompletesPromptly is the timing floor the resolver's
// (workspace, state, at) index and the de-duplicated composite resolution buy.
// The bound is deliberately generous — this asserts the mechanism did not
// regress to a full log re-walk per axis, not a wall-clock target.
func TestSnapshotAtFleetScaleCompletesPromptly(t *testing.T) {
	if raceDetector {
		t.Skip("the race detector's cgo instrumentation dominates this SQL cost measurement")
	}
	const (
		workspaces = 161
		rowsPer    = 140
		budget     = 3 * time.Second
	)

	m := openFleetTest(t, workspaces, rowsPer)

	start := time.Now()
	out, err := m.Snapshot()
	elapsed := time.Since(start)

	if err != nil {
		t.Fatalf("Snapshot: %v", err)
	}
	if len(out) != workspaces {
		t.Fatalf("snapshot len = %d, want %d", len(out), workspaces)
	}
	t.Logf("snapshot of %d workspaces x %d rows took %s", workspaces, rowsPer, elapsed)
	if elapsed > budget {
		t.Fatalf("snapshot took %s, want <= %s", elapsed, budget)
	}
}

// TestSnapshotDoesNotBlockPromptAcceptance is the defect itself: a resolve in
// flight must not queue a user's prompt behind it. The gate holds the
// (lock-free) resolve open until the prompt path has completed.
func TestSnapshotDoesNotBlockPromptAcceptance(t *testing.T) {
	m, _, _ := openTest(t, fakeResolver{"s1": "ws1"})
	gateEntered := make(chan struct{})
	promptFinished := make(chan struct{})
	m.snapshotGate = func() {
		close(gateEntered)
		// The prompt path must complete while the snapshot is held open here.
		<-promptFinished
	}

	snapshotDone := make(chan error, 1)
	go func() {
		_, err := m.Snapshot()
		snapshotDone <- err
	}()
	<-gateEntered
	promptErr := m.MarkPromptAccepted("ws1", "s1", "req-1", PromptAdmissionUser, func(*frontendv1.WorkspaceState) {})
	close(promptFinished)

	if promptErr != nil {
		t.Fatalf("MarkPromptAccepted while a snapshot was in flight: %v", promptErr)
	}
	if err := <-snapshotDone; err != nil {
		t.Fatalf("Snapshot: %v", err)
	}
}

// TestSnapshotCoalescesConcurrentCallers proves the single flight: the joiners
// share the leader's resolve instead of each running a fleet-wide one.
func TestSnapshotCoalescesConcurrentCallers(t *testing.T) {
	const joiners = 4

	m, _, _ := openTest(t, fakeResolver{"s1": "ws1"})
	joined := make(chan struct{}, joiners)
	release := make(chan struct{})
	m.snapshotJoined = func() { joined <- struct{}{} }
	m.snapshotGate = func() { <-release }

	results := make([][]*frontendv1.WorkspaceState, joiners+1)
	var wg sync.WaitGroup
	wg.Add(1)
	go func() {
		defer wg.Done()
		out, err := m.Snapshot()
		if err != nil {
			t.Errorf("leader Snapshot: %v", err)
		}
		results[0] = out
	}()
	for i := 0; i < joiners; i++ {
		wg.Add(1)
		go func(i int) {
			defer wg.Done()
			out, err := m.Snapshot()
			if err != nil {
				t.Errorf("joiner %d Snapshot: %v", i, err)
			}
			results[i+1] = out
		}(i)
	}
	for i := 0; i < joiners; i++ {
		<-joined
	}
	close(release)
	wg.Wait()

	if got := m.snapshotResolves.Load(); got != 1 {
		t.Fatalf("full-fleet resolves = %d, want 1 (callers must coalesce)", got)
	}
	for i, out := range results {
		if len(out) != len(results[0]) {
			t.Fatalf("caller %d got %d workspaces, want the leader's %d", i, len(out), len(results[0]))
		}
	}
}

// TestSnapshotRefreshesAWorkspacePublishedDuringTheResolve covers the window
// dropping the lock opens: a frame published while the fleet was resolving must
// not be superseded by older snapshot content.
func TestSnapshotRefreshesAWorkspacePublishedDuringTheResolve(t *testing.T) {
	m, _, _ := openTest(t, fakeResolver{"s1": "ws1"})
	published := make(chan struct{})
	m.snapshotGate = func() {
		if err := m.MarkPromptAccepted("ws1", "s1", "req-1", PromptAdmissionUser, func(*frontendv1.WorkspaceState) {}); err != nil {
			t.Errorf("MarkPromptAccepted: %v", err)
		}
		close(published)
	}

	out, err := m.Snapshot()

	<-published
	if err != nil {
		t.Fatalf("Snapshot: %v", err)
	}
	var got *frontendv1.WorkspaceState
	for _, msg := range out {
		if msg.GetWorkspace() == "ws1" {
			got = msg
		}
	}
	if got == nil {
		t.Fatalf("snapshot has no ws1: %v", out)
	}
	if got.GetCauseKind() != causePromptAccepted {
		t.Fatalf("cause = %q, want the %q frame published during the resolve", got.GetCauseKind(), causePromptAccepted)
	}
}
