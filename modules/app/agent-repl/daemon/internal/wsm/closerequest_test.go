package wsm

import (
	"context"
	"database/sql"
	"errors"
	"slices"
	"testing"
	"time"
)

func TestTheCloseRequestRoundTripsAndWithdrawsOnce(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	want := CloseRequest{At: instant, By: CloseByAgent}

	// Act
	setErr := s.RequestClose(context.Background(), ws.ID, want)
	stood, readErr := s.Workspace(context.Background(), ws.ID)
	first, firstErr := s.WithdrawCloseRequest(context.Background(), ws.ID)
	second, secondErr := s.WithdrawCloseRequest(context.Background(), ws.ID)
	cleared, clearedErr := s.Workspace(context.Background(), ws.ID)

	// Assert
	if err := errors.Join(setErr, readErr, firstErr, secondErr, clearedErr); err != nil {
		t.Fatalf("errors: %v", err)
	}
	if stood.CloseRequest == nil || *stood.CloseRequest != want {
		t.Errorf("stood = %+v, want %+v", stood.CloseRequest, want)
	}
	if !first || second {
		t.Errorf("withdrawals = %v, %v; want the first alone to find the request", first, second)
	}
	if cleared.CloseRequest != nil {
		t.Errorf("cleared = %+v, want none", cleared.CloseRequest)
	}
}

func TestARequestReplacesTheOneThatStood(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	later := CloseRequest{At: instant.Add(time.Minute), By: CloseByUser}
	if err := s.RequestClose(context.Background(), ws.ID, CloseRequest{At: instant, By: CloseByAgent}); err != nil {
		t.Fatalf("RequestClose: %v", err)
	}

	// Act
	err := s.RequestClose(context.Background(), ws.ID, later)

	// Assert
	got, readErr := s.Workspace(context.Background(), ws.ID)
	if err != nil || readErr != nil || got.CloseRequest == nil || *got.CloseRequest != later {
		t.Fatalf("request = %+v (%v, %v), want %+v", got.CloseRequest, err, readErr, later)
	}
}

func TestOnlyAGesturesCloseMayStand(t *testing.T) {
	for _, by := range []CloseCause{CloseByMerge, CloseByDaemon} {
		t.Run(string(by), func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			ws := testWorkspace(t, s)

			// Act
			err := s.RequestClose(context.Background(), ws.ID, CloseRequest{At: instant, By: by})

			// Assert
			got, readErr := s.Workspace(context.Background(), ws.ID)
			if err == nil || readErr != nil || got.CloseRequest != nil {
				t.Fatalf("RequestClose = %v, request %+v; want a refusal and nothing written", err, got.CloseRequest)
			}
		})
	}
}

func TestEveryCloseClearsTheCloseRequest(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	if err := s.RequestClose(context.Background(), ws.ID, CloseRequest{At: instant, By: CloseByAgent}); err != nil {
		t.Fatalf("RequestClose: %v", err)
	}

	// Act
	err := s.MarkClosed(context.Background(), ws.ID, Closure{At: instant, By: CloseByMerge})

	// Assert
	got, readErr := s.Workspace(context.Background(), ws.ID)
	if err != nil || readErr != nil || got.CloseRequest != nil {
		t.Fatalf("after the close: %+v (%v, %v); want no request", got.CloseRequest, err, readErr)
	}
}

func TestRequestingTheCloseOfAnUnregisteredWorkspaceFails(t *testing.T) {
	// Arrange
	s, _ := testStore(t)

	// Act
	err := s.RequestClose(context.Background(), "ws-none", CloseRequest{At: instant, By: CloseByUser})

	// Assert
	if err == nil {
		t.Fatal("RequestClose = nil, want a failure")
	}
}

func TestCloseRequestsListsOnlyOpenWorkspacesWhoseRequestStands(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	standing := testWorkspace(t, s)
	none := testWorkspaceNamed(t, s, "none")
	for _, ws := range []Workspace{standing} {
		if err := s.RequestClose(context.Background(), ws.ID, CloseRequest{At: instant, By: CloseByAgent}); err != nil {
			t.Fatalf("RequestClose: %v", err)
		}
	}

	// Act
	got, err := s.CloseRequests(context.Background())

	// Assert
	if err != nil || !slices.Equal(got, []WorkspaceID{standing.ID}) {
		t.Fatalf("CloseRequests = %v (%v), want only %s (not %s)", got, err, standing.ID, none.ID)
	}
}

func TestACorruptRequestCauseFailsTheRead(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	if _, err := s.q(context.Background()).ExecContext(context.Background(), `UPDATE workspaces SET close_requested_at = 1, close_requested_by = 'merge' WHERE id = ?`, string(ws.ID)); err != nil {
		t.Fatalf("corrupt the row: %v", err)
	}

	// Act
	_, err := s.Workspace(context.Background(), ws.ID)

	// Assert
	var decode *DecodeError
	if !errors.As(err, &decode) || decode.Field != "close_requested_by" {
		t.Fatalf("Workspace = %v, want a decode error naming close_requested_by", err)
	}
}

// closeRequestFixture is a file at the layout before the close request's
// columns, holding one workspace marked to close when its turn ends and one
// unmarked.
func closeRequestFixture(t *testing.T) string {
	t.Helper()
	path := fixtureAt(t, stepNamed(t, "close_requests")-1)
	execRaw(t, path, `INSERT INTO workspaces (id, repo_id, dir, name, branch, parent_branch, closed, attention, is_current, created_at, close_when_turn_ends)
		VALUES ('ws-marked', 'repo-1', '/tmp/ws-marked', 'marked', 'marked', 'master', 0, 0, 0, 1, 7)`)
	execRaw(t, path, `INSERT INTO workspaces (id, repo_id, dir, name, branch, parent_branch, closed, attention, is_current, created_at)
		VALUES ('ws-plain', 'repo-1', '/tmp/ws-plain', 'plain', 'plain', 'master', 0, 0, 0, 1)`)
	return path
}

func TestTheCloseRequestMigrationCarriesEverySelfCloseMark(t *testing.T) {
	tests := []struct {
		name  string
		query string
		want  string
	}{
		{name: "a marked row's request stands from its mark", query: `SELECT close_requested_at FROM workspaces WHERE id = 'ws-marked'`, want: "7"},
		{name: "a marked row's request is an agent's", query: `SELECT close_requested_by FROM workspaces WHERE id = 'ws-marked'`, want: "agent"},
		{name: "an unmarked row has no request", query: `SELECT coalesce(close_requested_at, 'null') FROM workspaces WHERE id = 'ws-plain'`, want: "null"},
		{name: "the mark's column is retired", query: `SELECT count(*) FROM pragma_table_info('workspaces') WHERE name = 'close_when_turn_ends'`, want: "0"},
		{name: "the mirroring triggers are retired", query: `SELECT count(*) FROM sqlite_master WHERE type = 'trigger' AND (name LIKE '%self_close%' OR name LIKE '%close_request%')`, want: "0"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			path := closeRequestFixture(t)

			// Act
			handle, err := Open(context.Background(), path, WithUnsyncedWrites())
			if err != nil {
				t.Fatalf("Open: %v", err)
			}
			handle.Close()

			// Assert
			if got := rawScalar[string](t, path, tt.query); got != tt.want {
				t.Fatalf("%s = %q, want %q", tt.query, got, tt.want)
			}
		})
	}
}

// THE TRIGGERS KEEP A HANDOVER'S TWO BUILDS ON ONE REQUEST: between layout 215
// and its contract, the incumbent writes the self-close mark and this build
// the request, and each write reaches the other.
func TestTheCloseRequestMirrorsKeepTheMarkAndTheRequestEqual(t *testing.T) {
	tests := []struct {
		name  string
		write string
		read  string
		want  string
	}{
		{name: "a mark the incumbent writes is an agent's request",
			write: `UPDATE workspaces SET close_when_turn_ends = 9 WHERE id = 'ws-plain'`,
			read:  `SELECT close_requested_at || ',' || close_requested_by FROM workspaces WHERE id = 'ws-plain'`, want: "9,agent"},
		{name: "a mark the incumbent takes withdraws the request",
			write: `UPDATE workspaces SET close_when_turn_ends = NULL WHERE id = 'ws-marked'`,
			read:  `SELECT coalesce(close_requested_at, 'null') || ',' || coalesce(close_requested_by, 'null') FROM workspaces WHERE id = 'ws-marked'`, want: "null,null"},
		{name: "a request this build writes is a mark for the incumbent",
			write: `UPDATE workspaces SET close_requested_at = 9, close_requested_by = 'user' WHERE id = 'ws-plain'`,
			read:  `SELECT close_when_turn_ends || ',' || close_requested_by FROM workspaces WHERE id = 'ws-plain'`, want: "9,user"},
		{name: "a request this build withdraws takes the incumbent's mark",
			write: `UPDATE workspaces SET close_requested_at = NULL, close_requested_by = NULL WHERE id = 'ws-marked'`,
			read:  `SELECT coalesce(close_when_turn_ends, 'null') FROM workspaces WHERE id = 'ws-marked'`, want: "null"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange: the file as a joining successor leaves it, the additive
			// step applied and its contract still owed.
			path := closeRequestFixture(t)
			for _, m := range migrations {
				if m.To == stepNamed(t, "close_requests") {
					execRaw(t, path, m.DDL)
				}
			}

			// Act
			execRaw(t, path, tt.write)

			// Assert
			if got := rawScalar[sql.NullString](t, path, tt.read); got.String != tt.want {
				t.Fatalf("%s = %q, want %q", tt.read, got.String, tt.want)
			}
		})
	}
}
