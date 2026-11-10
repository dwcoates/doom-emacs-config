package wsm

import (
	"context"
	"database/sql"
	"encoding/json"
	"testing"

	"agent-repld/internal/dlog"
)

func TestOnlyRetiredFailures(t *testing.T) {
	tests := []struct {
		name      string
		sentences []string
		want      bool
	}{
		{name: "an empty notice is no failure notice", want: false},
		{name: "reads failing", sentences: []string{"The daemon's reads of this pull request are failing: dns. It keeps reading on its schedule."}, want: true},
		{name: "a refused login", sentences: []string{"The daemon can no longer read this pull request: GitHub refused the `gh` login's credentials."}, want: true},
		{name: "a failure beside real activity", sentences: []string{"The daemon's reads of this pull request are failing: dns.", "@bob commented."}, want: false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act, Assert
			if got := onlyRetiredFailures(tt.sentences); got != tt.want {
				t.Fatalf("onlyRetiredFailures = %v, want %v", got, tt.want)
			}
		})
	}
}

// waitingNotice stores a standing held prompt and the pull-request notice it
// carries, saying SENTENCES, and answers the prompt's turn.
func waitingNotice(t *testing.T, s *store, ws WorkspaceID, number string, sentences []string) TurnID {
	t.Helper()
	turn := NewTurnID()
	if err := s.PutHeldPrompt(context.Background(), HeldPrompt{Workspace: ws, Turn: turn, Said: said("notice"), Origin: "workspace_notice", QueuedAt: instant}); err != nil {
		t.Fatalf("PutHeldPrompt: %v", err)
	}
	raw, _ := json.Marshal(sentences)
	if err := s.write(context.Background(), "test", nil, func(ctx context.Context, tx *sql.Tx) error {
		_, err := tx.ExecContext(ctx, `INSERT INTO pull_request_untold_notices (subscriber, pull_request, turn_id, sentences) VALUES (?, ?, ?, ?)`,
			ws, "o/r#"+number, turn, string(raw))
		return err
	}); err != nil {
		t.Fatalf("insert the untold notice: %v", err)
	}
	return turn
}

func TestTheRewriteDropsOnlyNoticesMadeOfRetiredFailures(t *testing.T) {
	// Arrange
	s, logs := testStore(t)
	ws := testWorkspace(t, s)
	failing := waitingNotice(t, s, ws.ID, "1", []string{"The daemon's reads of this pull request are failing: dns. It keeps reading on its schedule."})
	mixed := waitingNotice(t, s, ws.ID, "2", []string{"The daemon's reads of this pull request are failing: dns.", "@bob commented."})

	// Act
	err := s.write(context.Background(), "test", nil, func(ctx context.Context, tx *sql.Tx) error {
		return retireGithubFailureNotices(ctx, tx, s.log)
	})

	// Assert
	if err != nil {
		t.Fatalf("retireGithubFailureNotices: %v", err)
	}
	for _, tc := range []struct {
		turn        TurnID
		wantDropped bool
	}{{failing, true}, {mixed, false}} {
		got, _, err := s.HeldPromptByTurn(context.Background(), tc.turn)
		if err != nil {
			t.Fatalf("HeldPromptByTurn: %v", err)
		}
		if dropped := got.Tombstone != nil; dropped != tc.wantDropped {
			t.Fatalf("prompt %s dropped = %v, want %v", tc.turn, dropped, tc.wantDropped)
		}
	}
	if n := scalar[int](t, s, `SELECT count(*) FROM pull_request_untold_notices`); n != 1 {
		t.Fatalf("untold notices = %d, want only the mixed one left", n)
	}
	if _, ok := dlog.FindRecord(logs.Records(), dlog.RecordQuery{Level: "info", Operation: "daemon.wsm.open", Message: "dropped the waiting notices made only of retired GitHub failure sentences"}); !ok {
		t.Fatalf("records = %+v, want the rewrite recorded", logs.Records())
	}
}
