package wsm

import (
	"context"
	"testing"

	"agent-repld/internal/dlog"
)

func TestTheParkedTerminalStepRewritesBothOldParks(t *testing.T) {
	tests := []struct {
		name       string
		kind       string
		wantKind   string
		wantDetail string
	}{
		{name: "the idle sweep's park becomes the one park", kind: "hibernated", wantKind: "parked", wantDetail: "idle past the 24h0m0s cutoff"},
		{name: "the keep-alive cycle's park becomes the one park", kind: "keepalive_cycle", wantKind: "parked", wantDetail: "stood down between keep-alives"},
		{name: "a death is left as it is", kind: "killed", wantKind: "killed", wantDetail: "KillWorkspace"},
		{name: "a deletion is left as it is", kind: "deleted", wantKind: "deleted", wantDetail: "DeleteWorkspace"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			step := migrationTo(t, 107)
			path := fixtureAt(t, 106)
			execRaw(t, path, `INSERT INTO sessions (workspace_id, host_session_id, vendor_session_id, config_dir, model, permission_mode, started_at, last_engagement_at, terminal_kind, terminal_detail, terminal_at)
				VALUES ('ws-1', 'h', 'v', '/c', 'opus', 'auto', 1, 1, ?, ?, 7)`, tt.kind, tt.wantDetail)

			// Act
			execRaw(t, path, step.DDL)

			// Assert
			if got := rawScalar[string](t, path, `SELECT terminal_kind FROM sessions WHERE workspace_id = 'ws-1'`); got != tt.wantKind {
				t.Errorf("terminal_kind = %q, want %q", got, tt.wantKind)
			}
			if got := rawScalar[string](t, path, `SELECT terminal_detail FROM sessions WHERE workspace_id = 'ws-1'`); got != tt.wantDetail {
				t.Errorf("terminal_detail = %q, want the trigger kept as %q", got, tt.wantDetail)
			}
			if got := rawScalar[int](t, path, `SELECT terminal_at FROM sessions WHERE workspace_id = 'ws-1'`); got != 7 {
				t.Errorf("terminal_at = %d, want the instant kept as 7", got)
			}
		})
	}
}

func TestTheParkedTerminalStepLeavesALiveSessionLive(t *testing.T) {
	// Arrange
	step := migrationTo(t, 107)
	path := fixtureAt(t, 106)
	execRaw(t, path, `INSERT INTO sessions (workspace_id, host_session_id, vendor_session_id, config_dir, model, permission_mode, started_at, last_engagement_at)
		VALUES ('ws-1', 'h', 'v', '/c', 'opus', 'auto', 1, 1)`)

	// Act
	execRaw(t, path, step.DDL)

	// Assert
	if got := rawScalar[int](t, path, `SELECT count(*) FROM sessions WHERE terminal_kind IS NULL`); got != 1 {
		t.Fatalf("live sessions = %d, want the one live session left live", got)
	}
}

func TestTheParkedTerminalStepIsBreaking(t *testing.T) {
	// Act
	step := migrationTo(t, 107)

	// Assert
	if step.Name != "parked_terminal" || step.Kind != MigrationBreaking {
		t.Fatalf("migration 107 = %s (%s), want parked_terminal, breaking", step.Name, step.Kind)
	}
}

func TestAMigratedParkReadsAsTheOnePark(t *testing.T) {
	tests := []struct {
		name string
		kind string
	}{
		{name: "a hibernated row", kind: "hibernated"},
		{name: "a keep-alive cycle's row", kind: "keepalive_cycle"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			path := fixtureAt(t, 106)
			execRaw(t, path, `INSERT INTO sessions (workspace_id, host_session_id, vendor_session_id, config_dir, model, permission_mode, started_at, last_engagement_at, terminal_kind, terminal_detail, terminal_at)
				VALUES (?, 'h', 'v', '/c', 'opus', 'auto', 1, 1, ?, 'why', 7)`, string(fixtureWorkspaceID), tt.kind)
			db := openStoreAt(t, path, dlog.NewTestLogger())
			t.Cleanup(func() { db.Close() })

			// Act
			session, found, err := db.Session(context.Background(), fixtureWorkspaceID)

			// Assert
			if err != nil || !found {
				t.Fatalf("Session() = found %v, err %v; want the migrated row", found, err)
			}
			if !session.Parked() {
				t.Fatalf("the migrated session's terminal = %+v, want it read as the one park", session.Terminal)
			}
		})
	}
}
