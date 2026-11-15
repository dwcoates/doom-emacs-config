package wsm

import (
	"context"
	"path/filepath"
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
		{name: "a copied conversation of another workspace", fork: func(ws WorkspaceID) PendingFork {
			return PendingFork{Workspace: ws, Parent: "ws-other", ParentVendorSessionID: "vendor-queue"}
		}},
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
	instant := time.Unix(1_700_000_000, 0)
	for ws, conv := range map[WorkspaceID]string{a.ID: "va", b.ID: "vb"} {
		if err := s.PutSession(context.Background(), Session{Workspace: ws, HostSessionID: "h", VendorSessionID: conv, StartedAt: instant, LastEngagementAt: instant}); err != nil {
			t.Fatalf("PutSession: %v", err)
		}
	}
	corrupt(t, s, `INSERT INTO fork_lineage (workspace_id, vendor_session_id, parent_workspace_id, parent_book, through_ms) VALUES (?, 'va', ?, 'x', 1)`, a.ID, b.ID)
	corrupt(t, s, `INSERT INTO fork_lineage (workspace_id, vendor_session_id, parent_workspace_id, parent_book, through_ms) VALUES (?, 'vb', ?, 'y', 1)`, b.ID, a.ID)

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

// TestBindSessionForgetsAnOwedForkAndKeepsAMadeForksLineage pins what a bind
// forgets: the fork the workspace still owed, never the lineage of a fork that
// was made, which stays with its conversation and is not drawn under the
// conversation bound.
func TestBindSessionForgetsAnOwedForkAndKeepsAMadeForksLineage(t *testing.T) {
	cases := []struct {
		name        string
		owned       func(t *testing.T, s *store, parent, child Workspace)
		wantLineage int
	}{
		{name: "a fork still owed", owned: func(*testing.T, *store, Workspace, Workspace) {}, wantLineage: 0},
		{name: "a made fork", owned: func(t *testing.T, s *store, parent, child Workspace) {
			if err := s.ResolveFork(context.Background(), "v", ForkLineage{Workspace: child.ID, Parent: parent.ID, ParentBook: "b", ThroughMs: 1, Uninherited: []TurnID{"t"}}); err != nil {
				t.Fatalf("ResolveFork: %v", err)
			}
		}, wantLineage: 1},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			parent, child := forkedPair(t, s)
			tc.owned(t, s, parent, child)
			instant := time.Unix(1_700_000_000, 0)

			// Act
			err := s.BindSession(context.Background(), Session{Workspace: child.ID, HostSessionID: "h", VendorSessionID: "chosen", StartedAt: instant, LastEngagementAt: instant}, nil)

			// Assert
			if err != nil {
				t.Fatalf("BindSession: %v", err)
			}
			if n := scalar[int](t, s, `SELECT count(*) FROM pending_forks WHERE workspace_id = ?`, child.ID); n != 0 {
				t.Fatalf("pending_forks holds %d rows after the bind", n)
			}
			for _, table := range []string{"fork_lineage", "fork_uninherited_turns"} {
				if n := scalar[int](t, s, `SELECT count(*) FROM `+table+` WHERE workspace_id = ? AND vendor_session_id = 'v'`, child.ID); n != tc.wantLineage {
					t.Fatalf("%s holds %d rows for the made fork's conversation after the bind, want %d", table, n, tc.wantLineage)
				}
			}
			if got, err := s.Lineage(context.Background(), child.ID); err != nil || len(got) != 0 {
				t.Fatalf("Lineage = (%+v, %v), want none drawn under the chosen conversation", got, err)
			}
			if got := scalar[string](t, s, `SELECT vendor_session_id FROM sessions WHERE workspace_id = ?`, child.ID); got != "chosen" {
				t.Fatalf("vendor id = %q, want chosen", got)
			}
		})
	}
}

// TestABindBackOntoAForkedConversationDrawsItsLineageAgain pins the fix for
// the queue-bypass switch back (2026-11-10): a conversation that was a fork
// draws its inherited past again once it is bound again.
func TestABindBackOntoAForkedConversationDrawsItsLineageAgain(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ctx := context.Background()
	parent, child := forkedPair(t, s)
	if err := s.ResolveFork(ctx, "v-queue", ForkLineage{Workspace: child.ID, Parent: parent.ID, ParentBook: "book-p", ThroughMs: 7, Uninherited: []TurnID{"t-open"}}); err != nil {
		t.Fatalf("ResolveFork: %v", err)
	}
	instant := time.Unix(1_700_000_000, 0)
	away := Session{Workspace: child.ID, HostSessionID: "h", VendorSessionID: "v-other", StartedAt: instant, LastEngagementAt: instant}
	if err := s.BindSession(ctx, away, nil); err != nil {
		t.Fatalf("BindSession away: %v", err)
	}

	// Act
	back := away
	back.VendorSessionID = "v-queue"
	if err := s.BindSession(ctx, back, nil); err != nil {
		t.Fatalf("BindSession back: %v", err)
	}
	got, err := s.Lineage(ctx, child.ID)

	// Assert
	if err != nil {
		t.Fatalf("Lineage: %v", err)
	}
	if len(got) != 1 || got[0].Parent != parent.ID || got[0].ParentBook != "book-p" || got[0].ThroughMs != 7 || !slices.Equal(got[0].Uninherited, []TurnID{"t-open"}) {
		t.Fatalf("Lineage = %+v, want the queue conversation's fork of the parent", got)
	}
}

// TestSetVendorSessionIDCarriesTheLineageToTheRotatedConversation pins the
// rotation: a /clear continues the conversation under a new id, and its
// lineage moves with it.
func TestSetVendorSessionIDCarriesTheLineageToTheRotatedConversation(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ctx := context.Background()
	parent, child := forkedPair(t, s)
	if err := s.ResolveFork(ctx, "v-before", ForkLineage{Workspace: child.ID, Parent: parent.ID, ParentBook: "book-p", ThroughMs: 3, Uninherited: []TurnID{"t"}}); err != nil {
		t.Fatalf("ResolveFork: %v", err)
	}

	// Act
	if _, err := s.SetVendorSessionID(ctx, child.ID, "v-after"); err != nil {
		t.Fatalf("SetVendorSessionID: %v", err)
	}
	got, err := s.Lineage(ctx, child.ID)

	// Assert
	if err != nil || len(got) != 1 || got[0].ParentBook != "book-p" || !slices.Equal(got[0].Uninherited, []TurnID{"t"}) {
		t.Fatalf("Lineage = (%+v, %v), want the fork carried to the rotated conversation", got, err)
	}
	for _, table := range []string{"fork_lineage", "fork_uninherited_turns"} {
		if n := scalar[int](t, s, `SELECT count(*) FROM `+table+` WHERE workspace_id = ? AND vendor_session_id = 'v-before'`, child.ID); n != 0 {
			t.Fatalf("%s still holds %d rows under the id the rotation replaced", table, n)
		}
	}
}

func TestBindSessionRefusesASessionWithNoIdentityAndWritesNothing(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	_, child := forkedPair(t, s)

	// Act
	err := s.BindSession(context.Background(), Session{Workspace: child.ID, VendorSessionID: "chosen"}, nil)

	// Assert
	if err == nil {
		t.Fatal("BindSession accepted a session with no host identity")
	}
	if _, found, _ := s.PendingFork(context.Background(), child.ID); !found {
		t.Fatal("the refused bind forgot the pending fork")
	}
}

// ownForkStore answers a store holding one workspace with a session row.
func ownForkStore(t *testing.T) (*store, *dlog.TestLogger, Workspace) {
	t.Helper()
	s, logs := testStore(t)
	ws := testWorkspace(t, s)
	instant := time.Unix(1_700_000_000, 0)
	if err := s.PutSession(context.Background(), Session{
		Workspace: ws.ID, HostSessionID: "host-1", VendorSessionID: "vendor-queue", ConfigDir: "/root",
		StartedAt: instant, LastEngagementAt: instant,
	}); err != nil {
		t.Fatalf("PutSession: %v", err)
	}
	return s, logs, ws
}

func TestBindSessionToOwnForkBindsNoConversationAndOwesTheFork(t *testing.T) {
	// Arrange
	s, _, ws := ownForkStore(t)
	ctx := context.Background()
	sess, _, _ := s.Session(ctx, ws.ID)
	sess.VendorSessionID = ""

	// Act
	err := s.BindSessionToOwnFork(ctx, sess, "vendor-queue", nil)

	// Assert
	if err != nil {
		t.Fatalf("BindSessionToOwnFork: %v", err)
	}
	if got := scalar[string](t, s, `SELECT vendor_session_id FROM sessions WHERE workspace_id = ?`, ws.ID); got != "" {
		t.Fatalf("the session names %q, want no conversation until the fork is made", got)
	}
	got, found, err := s.PendingFork(ctx, ws.ID)
	want := PendingFork{Workspace: ws.ID, Parent: ws.ID, ParentVendorSessionID: "vendor-queue"}
	if err != nil || !found || got != want {
		t.Fatalf("PendingFork = (%+v, %v, %v), want %+v", got, found, err, want)
	}
}

// TestAnOwnForkOfAForkedConversationWalksThroughTheConversationItCopied pins
// the queue-bypass fork of a conversation that was itself a fork: the bypass
// conversation's lineage is its own fork, then the queue conversation's fork
// of the parent.
func TestAnOwnForkOfAForkedConversationWalksThroughTheConversationItCopied(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	parent, child := forkedPair(t, s)
	ctx := context.Background()
	if err := s.ResolveFork(ctx, "vendor-queue", ForkLineage{Workspace: child.ID, Parent: parent.ID, ParentBook: "book-p", ThroughMs: 1}); err != nil {
		t.Fatalf("ResolveFork: %v", err)
	}
	sess, _, _ := s.Session(ctx, child.ID)
	sess.VendorSessionID = ""
	if err := s.BindSessionToOwnFork(ctx, sess, "vendor-queue", nil); err != nil {
		t.Fatalf("BindSessionToOwnFork: %v", err)
	}

	// Act
	if err := s.ResolveFork(ctx, "vendor-bypass", ForkLineage{Workspace: child.ID, Parent: child.ID, ParentBook: "book-queue", ThroughMs: 5}); err != nil {
		t.Fatalf("ResolveFork own: %v", err)
	}
	got, err := s.Lineage(ctx, child.ID)

	// Assert
	if err != nil {
		t.Fatalf("Lineage: %v", err)
	}
	if len(got) != 2 ||
		got[0].Parent != child.ID || got[0].ParentVendorSessionID != "vendor-queue" || got[0].ParentBook != "book-queue" ||
		got[1].Parent != parent.ID || got[1].ParentVendorSessionID != "" || got[1].ParentBook != "book-p" {
		t.Fatalf("Lineage = %+v, want the own fork then the queue conversation's fork of the parent", got)
	}
}

func TestBindSessionToOwnForkRefusesAndWritesNothing(t *testing.T) {
	cases := []struct {
		name   string
		vendor string
		parent string
	}{
		{name: "a session that already names a conversation", vendor: "vendor-other", parent: "vendor-queue"},
		{name: "no conversation to copy", vendor: "", parent: ""},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			s, logs, ws := ownForkStore(t)
			ctx := context.Background()
			sess, _, _ := s.Session(ctx, ws.ID)
			sess.VendorSessionID = tc.vendor

			// Act
			err := s.BindSessionToOwnFork(ctx, sess, tc.parent, nil)

			// Assert
			if err == nil {
				t.Fatal("BindSessionToOwnFork accepted it")
			}
			if got := scalar[string](t, s, `SELECT vendor_session_id FROM sessions WHERE workspace_id = ?`, ws.ID); got != "vendor-queue" {
				t.Fatalf("the session names %q after a refusal, want it unchanged", got)
			}
			if n := scalar[int](t, s, `SELECT count(*) FROM pending_forks`); n != 0 {
				t.Fatalf("pending_forks holds %d rows after a refusal", n)
			}
			if !loggedError(logs.Records(), "daemon.wsm.bind_session_to_own_fork") {
				t.Fatalf("the refusal was not logged at error: %+v", logs.Records())
			}
		})
	}
}

// TestLineageOfAnOwnForkOfAnUnforkedConversationIsTheOwnForkAlone pins the
// walk's end at a copied conversation that was no fork.
func TestLineageOfAnOwnForkOfAnUnforkedConversationIsTheOwnForkAlone(t *testing.T) {
	// Arrange
	s, _, ws := ownForkStore(t)
	ctx := context.Background()
	sess, _, _ := s.Session(ctx, ws.ID)
	sess.VendorSessionID = ""
	if err := s.BindSessionToOwnFork(ctx, sess, "vendor-queue", nil); err != nil {
		t.Fatalf("BindSessionToOwnFork: %v", err)
	}
	if err := s.ResolveFork(ctx, "vendor-bypass", ForkLineage{Workspace: ws.ID, Parent: ws.ID, ParentBook: "book-queue", ThroughMs: 5}); err != nil {
		t.Fatalf("ResolveFork: %v", err)
	}

	// Act
	got, err := s.Lineage(ctx, ws.ID)

	// Assert
	if err != nil {
		t.Fatalf("Lineage: %v", err)
	}
	want := []ForkLineage{{Workspace: ws.ID, Parent: ws.ID, ParentBook: "book-queue", ThroughMs: 5}}
	if len(got) != 1 || got[0].Workspace != want[0].Workspace || got[0].Parent != want[0].Parent || got[0].ParentBook != want[0].ParentBook || got[0].ThroughMs != want[0].ThroughMs || got[0].ParentVendorSessionID != "vendor-queue" {
		t.Fatalf("Lineage = %+v, want %+v", got, want)
	}
}

// byConversationFixture answers a file at the layout before the
// by-conversation step, holding the fixture workspace and two more, ws-child
// and ws-own, with no lineage yet.
func byConversationFixture(t *testing.T) string {
	t.Helper()
	path := fixtureAt(t, stepNamed(t, "fork_lineage_by_conversation")-1)
	for i, ws := range []string{"ws-child", "ws-own"} {
		execRaw(t, path, `INSERT INTO workspaces (id, repo_id, dir, name, branch, parent_branch, closed, attention, is_current, created_at, place)
		VALUES (?, 'repo-1', ?, ?, ?, 'master', 0, 0, 0, 1, ?)`, ws, "/tmp/"+ws, ws, ws, 60+i)
	}
	return path
}

// TestTheByConversationStepKeysEachLineageByItsBoundConversation pins the
// layout-140 step: a standing lineage is carried to the conversation its
// workspace is bound to, and an own fork records the parked conversation it
// copied.
func TestTheByConversationStepKeysEachLineageByItsBoundConversation(t *testing.T) {
	// Arrange
	path := byConversationFixture(t)
	execRaw(t, path, `INSERT INTO sessions (workspace_id, host_session_id, vendor_session_id, config_dir, model, permission_mode, started_at, last_engagement_at)
		VALUES ('ws-child', 'h', 'v-child', '/c', 'opus', 'auto', 1, 1), ('ws-own', 'h', 'v-bypass', '/c', 'opus', 'auto', 1, 1)`)
	execRaw(t, path, `INSERT INTO workspace_parked_conversations (workspace_id, vendor_session_id) VALUES ('ws-own', 'v-queue')`)
	execRaw(t, path, `INSERT INTO fork_lineage (workspace_id, parent_workspace_id, parent_book, through_ms) VALUES ('ws-child', ?, 'b1', 1), ('ws-own', 'ws-own', 'b2', 2)`, string(fixtureWorkspaceID))
	execRaw(t, path, `INSERT INTO fork_uninherited_turns (workspace_id, turn_id) VALUES ('ws-child', 't1')`)

	// Act
	openMigrated(t, path)

	// Assert
	lineage := rawScalar[string](t, path, `SELECT group_concat(workspace_id || ':' || vendor_session_id || ':' || coalesce(parent_vendor_session_id, '-'), ',') FROM (SELECT * FROM fork_lineage ORDER BY workspace_id)`)
	if want := "ws-child:v-child:-,ws-own:v-bypass:v-queue"; lineage != want {
		t.Fatalf("fork_lineage = %q, want %q", lineage, want)
	}
	turns := rawScalar[string](t, path, `SELECT group_concat(workspace_id || ':' || vendor_session_id || ':' || turn_id, ',') FROM fork_uninherited_turns`)
	if want := "ws-child:v-child:t1"; turns != want {
		t.Fatalf("fork_uninherited_turns = %q, want %q", turns, want)
	}
}

// TestTheByConversationStepFailsALineageWhoseWorkspaceNamesNoConversation pins
// the step's invariant: a lineage row whose workspace is bound to no
// conversation fails the migration, never dropped.
func TestTheByConversationStepFailsALineageWhoseWorkspaceNamesNoConversation(t *testing.T) {
	// Arrange
	path := byConversationFixture(t)
	execRaw(t, path, `INSERT INTO fork_lineage (workspace_id, parent_workspace_id, parent_book, through_ms) VALUES ('ws-child', 'ws-own', 'b1', 1)`)

	// Act
	_, err := Open(context.Background(), path, WithUnsyncedWrites())

	// Assert
	if err == nil || !strings.Contains(err.Error(), "NOT NULL") {
		t.Fatalf("Open = %v, want the step failed on the lineage naming no conversation", err)
	}
	if got := rawScalar[int](t, path, `SELECT count(*) FROM fork_lineage WHERE workspace_id = 'ws-child'`); got != 1 {
		t.Fatalf("lineage rows after the refused step = %d, want the row left in place", got)
	}
}

// TestAMigratedForkLineageMatchesAFreshOne pins that both routes to the
// by-conversation layout leave the lineage tables in one shape.
func TestAMigratedForkLineageMatchesAFreshOne(t *testing.T) {
	for _, table := range []string{"fork_lineage", "fork_uninherited_turns"} {
		t.Run(table, func(t *testing.T) {
			// Arrange
			migrated := byConversationFixture(t)
			openMigrated(t, migrated)
			fresh := filepath.Join(t.TempDir(), "fresh.db")
			openMigrated(t, fresh)

			// Act
			got, want := columnSet(t, migrated, table), columnSet(t, fresh, table)

			// Assert
			if !slices.Equal(got, want) {
				t.Fatalf("migrated %s columns = %q, a fresh file's = %q", table, got, want)
			}
		})
	}
}
