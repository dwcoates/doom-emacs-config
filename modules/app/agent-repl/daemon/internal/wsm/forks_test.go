package wsm

import (
	"context"
	"slices"
	"strings"
	"testing"
	"time"

	"agent-repld/internal/dlog"
)

// forkedPair answers a parent and a child with a session row, the child owing a
// fork of the parent.
func forkedPair(t *testing.T, s *store) (parent, child Workspace) {
	t.Helper()
	ctx := context.Background()
	parent, child = testWorkspace(t, s), testWorkspace(t, s)
	instant := time.Unix(1_700_000_000, 0)
	if err := s.PutSession(ctx, Session{Workspace: child.ID, HostSessionID: "host-1", StartedAt: instant, LastEngagementAt: instant}); err != nil {
		t.Fatalf("PutSession: %v", err)
	}
	if err := s.PutPendingFork(ctx, PendingFork{Workspace: child.ID, Parent: parent.ID}); err != nil {
		t.Fatalf("PutPendingFork: %v", err)
	}
	return parent, child
}

func TestPendingForkRoundTrips(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	parent, child := forkedPair(t, s)

	// Act
	got, found, err := s.PendingFork(context.Background(), child.ID)

	// Assert
	if err != nil || !found {
		t.Fatalf("PendingFork = (%v, %v, %v), want found", got, found, err)
	}
	if got != (PendingFork{Workspace: child.ID, Parent: parent.ID}) {
		t.Fatalf("PendingFork = %+v, want child %s of parent %s", got, child.ID, parent.ID)
	}
}

func TestPendingForkAnswersNotFoundForAWorkspaceThatOwesNone(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)

	// Act
	_, found, err := s.PendingFork(context.Background(), ws.ID)

	// Assert
	if err != nil || found {
		t.Fatalf("PendingFork = (found %v, %v), want not found", found, err)
	}
}

func TestPutPendingForkRefusesAnIncompleteOrSelfFork(t *testing.T) {
	cases := []struct {
		name string
		fork func(ws WorkspaceID) PendingFork
	}{
		{name: "no workspace", fork: func(ws WorkspaceID) PendingFork { return PendingFork{Parent: ws} }},
		{name: "no parent", fork: func(ws WorkspaceID) PendingFork { return PendingFork{Workspace: ws} }},
		{name: "forked from itself", fork: func(ws WorkspaceID) PendingFork { return PendingFork{Workspace: ws, Parent: ws} }},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			s, logs := testStore(t)
			ws := testWorkspace(t, s)

			// Act
			err := s.PutPendingFork(context.Background(), tc.fork(ws.ID))

			// Assert
			if err == nil {
				t.Fatal("PutPendingFork accepted it")
			}
			if n := scalar[int](t, s, `SELECT count(*) FROM pending_forks`); n != 0 {
				t.Fatalf("pending_forks holds %d rows after a refusal", n)
			}
			if !loggedError(logs.Records(), "daemon.wsm.put_pending_fork") {
				t.Fatalf("the refusal was not logged at error: %+v", logs.Records())
			}
		})
	}
}

func TestResolveForkRecordsTheForkInOneStep(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	parent, child := forkedPair(t, s)
	ctx := context.Background()

	// Act
	err := s.ResolveFork(ctx, "vendor-fork", ForkLineage{
		Workspace: child.ID, Parent: parent.ID, ParentBook: "book-p", ThroughMs: 1234, Uninherited: []TurnID{"t9"},
	})

	// Assert
	if err != nil {
		t.Fatalf("ResolveFork: %v", err)
	}
	if _, found, _ := s.PendingFork(ctx, child.ID); found {
		t.Fatal("the pending fork still stands after it was resolved")
	}
	if got := scalar[string](t, s, `SELECT vendor_session_id FROM sessions WHERE workspace_id = ?`, child.ID); got != "vendor-fork" {
		t.Fatalf("session vendor id = %q, want vendor-fork", got)
	}
	lineage, err := s.Lineage(ctx, child.ID)
	if err != nil {
		t.Fatalf("Lineage: %v", err)
	}
	want := []ForkLineage{{Workspace: child.ID, Parent: parent.ID, ParentBook: "book-p", ThroughMs: 1234, Uninherited: []TurnID{"t9"}}}
	if len(lineage) != 1 || lineage[0].Workspace != want[0].Workspace || lineage[0].Parent != want[0].Parent ||
		lineage[0].ParentBook != want[0].ParentBook || lineage[0].ThroughMs != want[0].ThroughMs ||
		!slices.Equal(lineage[0].Uninherited, want[0].Uninherited) {
		t.Fatalf("Lineage = %+v, want %+v", lineage, want)
	}
}

func TestResolveForkRefusesAForkNobodyOwesAndWritesNothing(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	parent, child := forkedPair(t, s)
	other := testWorkspace(t, s)
	ctx := context.Background()

	// Act
	err := s.ResolveFork(ctx, "vendor-fork", ForkLineage{Workspace: child.ID, Parent: other.ID, ParentBook: "b", ThroughMs: 1})

	// Assert
	if err == nil || !strings.Contains(err.Error(), "owes no fork") {
		t.Fatalf("ResolveFork = %v, want owes-no-fork", err)
	}
	if _, found, _ := s.PendingFork(ctx, child.ID); !found {
		t.Fatal("the refused resolve retired the real pending fork")
	}
	if n := scalar[int](t, s, `SELECT count(*) FROM fork_lineage`); n != 0 {
		t.Fatalf("fork_lineage holds %d rows after a refusal", n)
	}
	_ = parent
}

func TestResolveForkRefusesAnIncompleteLineage(t *testing.T) {
	cases := []struct {
		name    string
		vendor  string
		lineage func(parent, child WorkspaceID) ForkLineage
	}{
		{name: "no vendor id", vendor: "", lineage: func(p, c WorkspaceID) ForkLineage {
			return ForkLineage{Workspace: c, Parent: p, ParentBook: "b", ThroughMs: 1}
		}},
		{name: "no parent book", vendor: "v", lineage: func(p, c WorkspaceID) ForkLineage {
			return ForkLineage{Workspace: c, Parent: p, ThroughMs: 1}
		}},
		{name: "no bound", vendor: "v", lineage: func(p, c WorkspaceID) ForkLineage {
			return ForkLineage{Workspace: c, Parent: p, ParentBook: "b"}
		}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			s, logs := testStore(t)
			parent, child := forkedPair(t, s)

			// Act
			err := s.ResolveFork(context.Background(), tc.vendor, tc.lineage(parent.ID, child.ID))

			// Assert
			if err == nil {
				t.Fatal("ResolveFork accepted it")
			}
			if _, found, _ := s.PendingFork(context.Background(), child.ID); !found {
				t.Fatal("the refused resolve retired the pending fork")
			}
			if !loggedError(logs.Records(), "daemon.wsm.resolve_fork") {
				t.Fatalf("the refusal was not logged at error: %+v", logs.Records())
			}
		})
	}
}

func TestLineageWalksEveryGenerationNearestFirst(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ctx := context.Background()
	grandparent, parent := forkedPair(t, s)
	if err := s.ResolveFork(ctx, "v-parent", ForkLineage{Workspace: parent.ID, Parent: grandparent.ID, ParentBook: "book-g", ThroughMs: 10}); err != nil {
		t.Fatalf("resolve parent: %v", err)
	}
	child := testWorkspace(t, s)
	instant := time.Unix(1_700_000_000, 0)
	if err := s.PutSession(ctx, Session{Workspace: child.ID, HostSessionID: "h", StartedAt: instant, LastEngagementAt: instant}); err != nil {
		t.Fatalf("PutSession: %v", err)
	}
	if err := s.PutPendingFork(ctx, PendingFork{Workspace: child.ID, Parent: parent.ID}); err != nil {
		t.Fatalf("PutPendingFork: %v", err)
	}
	if err := s.ResolveFork(ctx, "v-child", ForkLineage{Workspace: child.ID, Parent: parent.ID, ParentBook: "book-p", ThroughMs: 20}); err != nil {
		t.Fatalf("resolve child: %v", err)
	}

	// Act
	got, err := s.Lineage(ctx, child.ID)

	// Assert
	if err != nil {
		t.Fatalf("Lineage: %v", err)
	}
	if len(got) != 2 || got[0].ParentBook != "book-p" || got[1].ParentBook != "book-g" || got[1].Parent != grandparent.ID {
		t.Fatalf("Lineage = %+v, want book-p then book-g", got)
	}
}

func TestLineageIsEmptyForAWorkspaceNeverForked(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)

	// Act
	got, err := s.Lineage(context.Background(), ws.ID)

	// Assert
	if err != nil || len(got) != 0 {
		t.Fatalf("Lineage = (%+v, %v), want empty", got, err)
	}
}

func TestLineageRefusesACycle(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	a, b := testWorkspace(t, s), testWorkspace(t, s)
	corrupt(t, s, `INSERT INTO fork_lineage (workspace_id, parent_workspace_id, parent_book, through_ms) VALUES (?, ?, 'x', 1)`, a.ID, b.ID)
	corrupt(t, s, `INSERT INTO fork_lineage (workspace_id, parent_workspace_id, parent_book, through_ms) VALUES (?, ?, 'y', 1)`, b.ID, a.ID)

	// Act
	_, err := s.Lineage(context.Background(), a.ID)

	// Assert
	if err == nil || !strings.Contains(err.Error(), "loops back") {
		t.Fatalf("Lineage = %v, want a loop refusal", err)
	}
}

// loggedError reports whether RECORDS hold an error-level record of OPERATION.
func loggedError(records []dlog.Record, operation string) bool {
	for _, r := range records {
		if r.Level == dlog.LevelError && r.Operation == operation {
			return true
		}
	}
	return false
}

func TestBindSessionForgetsTheForkTheWorkspaceWas(t *testing.T) {
	cases := []struct {
		name  string
		owned func(t *testing.T, s *store, parent, child Workspace)
	}{
		{name: "a fork still owed", owned: func(*testing.T, *store, Workspace, Workspace) {}},
		{name: "a made fork", owned: func(t *testing.T, s *store, parent, child Workspace) {
			if err := s.ResolveFork(context.Background(), "v", ForkLineage{Workspace: child.ID, Parent: parent.ID, ParentBook: "b", ThroughMs: 1, Uninherited: []TurnID{"t"}}); err != nil {
				t.Fatalf("ResolveFork: %v", err)
			}
		}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			parent, child := forkedPair(t, s)
			tc.owned(t, s, parent, child)
			instant := time.Unix(1_700_000_000, 0)

			// Act
			err := s.BindSession(context.Background(), Session{Workspace: child.ID, HostSessionID: "h", VendorSessionID: "chosen", StartedAt: instant, LastEngagementAt: instant})

			// Assert
			if err != nil {
				t.Fatalf("BindSession: %v", err)
			}
			for _, table := range []string{"pending_forks", "fork_lineage", "fork_uninherited_turns"} {
				if n := scalar[int](t, s, `SELECT count(*) FROM `+table+` WHERE workspace_id = ?`, child.ID); n != 0 {
					t.Fatalf("%s holds %d rows after the bind", table, n)
				}
			}
			if got := scalar[string](t, s, `SELECT vendor_session_id FROM sessions WHERE workspace_id = ?`, child.ID); got != "chosen" {
				t.Fatalf("vendor id = %q, want chosen", got)
			}
		})
	}
}

func TestBindSessionRefusesASessionWithNoIdentityAndWritesNothing(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	_, child := forkedPair(t, s)

	// Act
	err := s.BindSession(context.Background(), Session{Workspace: child.ID, VendorSessionID: "chosen"})

	// Assert
	if err == nil {
		t.Fatal("BindSession accepted a session with no host identity")
	}
	if _, found, _ := s.PendingFork(context.Background(), child.ID); !found {
		t.Fatal("the refused bind forgot the pending fork")
	}
}
