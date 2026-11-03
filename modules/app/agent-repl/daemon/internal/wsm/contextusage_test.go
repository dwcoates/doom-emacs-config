package wsm

import (
	"context"
	"errors"
	"testing"

	"google.golang.org/protobuf/proto"

	conversationv1 "agentrepl/proto/conversation/v1"

	"agent-repld/internal/dlog"
)

func readingOf(total, window int64) *conversationv1.SessionContextUsage {
	return &conversationv1.SessionContextUsage{TotalTokens: total, MaxTokens: window}
}

func TestWorkspaceContextUsage(t *testing.T) {
	tests := []struct {
		name      string
		put       func(context.Context, *store, WorkspaceID) error
		want      ContextUsageRecord
		wantFound bool
	}{
		{name: "a workspace with none recorded has none", put: func(context.Context, *store, WorkspaceID) error { return nil }},
		{name: "a reading is read back whole", put: func(ctx context.Context, s *store, ws WorkspaceID) error {
			return s.PutWorkspaceContextUsage(ctx, ws, readingOf(42_000, 200_000))
		}, want: ContextUsageRecord{Usage: readingOf(42_000, 200_000)}, wantFound: true},
		{name: "a later reading replaces the earlier", put: func(ctx context.Context, s *store, ws WorkspaceID) error {
			return errors.Join(s.PutWorkspaceContextUsage(ctx, ws, readingOf(1, 200_000)), s.PutWorkspaceContextUsage(ctx, ws, readingOf(2, 1_000_000)))
		}, want: ContextUsageRecord{Usage: readingOf(2, 1_000_000)}, wantFound: true},
		{name: "a cut retires the reading", put: func(ctx context.Context, s *store, ws WorkspaceID) error {
			return errors.Join(s.PutWorkspaceContextUsage(ctx, ws, readingOf(1, 200_000)), s.PutWorkspaceContextCut(ctx, ws))
		}, want: ContextUsageRecord{Cut: true}, wantFound: true},
		{name: "a fresh reading retires the cut", put: func(ctx context.Context, s *store, ws WorkspaceID) error {
			return errors.Join(s.PutWorkspaceContextCut(ctx, ws), s.PutWorkspaceContextUsage(ctx, ws, readingOf(3, 200_000)))
		}, want: ContextUsageRecord{Usage: readingOf(3, 200_000)}, wantFound: true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			ws := testWorkspace(t, s).ID
			if err := tt.put(context.Background(), s, ws); err != nil {
				t.Fatalf("put: %v", err)
			}

			// Act
			got, found, err := s.WorkspaceContextUsage(context.Background(), ws)

			// Assert
			if err != nil || found != tt.wantFound || got.Cut != tt.want.Cut || !proto.Equal(got.Usage, tt.want.Usage) {
				t.Fatalf("WorkspaceContextUsage = %+v, %v, %v; want %+v, %v", got, found, err, tt.want, tt.wantFound)
			}
		})
	}
}

func TestANilContextReadingIsRefused(t *testing.T) {
	// Arrange
	s, log := testStore(t)
	ws := testWorkspace(t, s).ID

	// Act
	err := s.PutWorkspaceContextUsage(context.Background(), ws, nil)

	// Assert
	if err == nil {
		t.Fatal("PutWorkspaceContextUsage(nil) = nil, want a refusal")
	}
	if n := scalar[int](t, s, `SELECT count(*) FROM workspace_context_usage`); n != 0 {
		t.Fatalf("%d rows after the refusal, want none", n)
	}
	records := dlog.FilterRecords(log.Records(), dlog.RecordQuery{Level: dlog.LevelError, Operation: "daemon.wsm.put_workspace_context_usage"})
	if len(records) != 1 || records[0].Context["workspace"] != string(ws) {
		t.Fatalf("records = %+v, want one ERROR naming the workspace", log.Records())
	}
}

func TestARowThatIsBothOrNeitherIsUnrepresentable(t *testing.T) {
	tests := []struct {
		name string
		stmt string
	}{
		{"a reading and a cut", `INSERT INTO workspace_context_usage (workspace_id, usage, cut) VALUES (?, x'08', 1)`},
		{"neither a reading nor a cut", `INSERT INTO workspace_context_usage (workspace_id, usage, cut) VALUES (?, NULL, 0)`},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			ws := testWorkspace(t, s).ID

			// Act
			_, err := s.db().ExecContext(context.Background(), tt.stmt, ws)

			// Assert
			if err == nil {
				t.Fatalf("%s was stored, want the CHECK to refuse it", tt.name)
			}
		})
	}
}

func TestAStoredContextReadingThatDoesNotDecodeIsADecodeError(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s).ID
	corrupt(t, s, `INSERT INTO workspace_context_usage (workspace_id, usage, cut) VALUES (?, x'ff', 0)`, ws)

	// Act
	_, _, err := s.WorkspaceContextUsage(context.Background(), ws)

	// Assert
	var decode *DecodeError
	if !errors.As(err, &decode) || decode.Field != "usage" {
		t.Fatalf("err = %v, want a DecodeError on usage", err)
	}
}

func TestAWorkspacesContextUsageGoesWithIt(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s).ID
	if err := s.PutWorkspaceContextUsage(context.Background(), ws, readingOf(1, 200_000)); err != nil {
		t.Fatalf("PutWorkspaceContextUsage: %v", err)
	}

	// Act
	if _, err := s.db().ExecContext(context.Background(), `DELETE FROM workspaces WHERE id = ?`, ws); err != nil {
		t.Fatalf("delete: %v", err)
	}

	// Assert
	if n := scalar[int](t, s, `SELECT count(*) FROM workspace_context_usage`); n != 0 {
		t.Fatalf("%d context rows, want none after the workspace went", n)
	}
}

// TestTheMigrationAddsTheContextUsageTable pins the layout-109 step: a file
// written before the context fact was durable carries the table afterwards,
// with nothing recorded.
func TestTheMigrationAddsTheContextUsageTable(t *testing.T) {
	// Arrange
	path := layout3Fixture(t)

	// Act
	handle, err := Open(context.Background(), path, WithUnsyncedWrites())
	if err != nil {
		t.Fatalf("Open on a layout-3 database: %v", err)
	}
	defer handle.Close()

	// Assert
	if got := scalar[int](t, handle.(*store), `SELECT count(*) FROM workspace_context_usage`); got != 0 {
		t.Fatalf("%d migrated rows, want none", got)
	}
}
