package wsm

import (
	"context"
	"errors"
	"testing"
)

// testLinkedWorktree registers a repository's main worktree and then a SECOND
// worktree of it, answering the second: the workspace whose remote control is
// off by default.
func testLinkedWorktree(t *testing.T, s *store) Workspace {
	t.Helper()
	main := testWorkspace(t, s)
	ws, _, err := s.RegisterWorkspace(context.Background(), t.TempDir(), RegisterFacts{
		Name: "linked", Branch: "topic", ParentBranch: "master", RepoDir: main.Dir,
	})
	if err != nil {
		t.Fatalf("RegisterWorkspace: %v", err)
	}
	return ws
}

func TestTheRemoteControlDefault(t *testing.T) {
	tests := []struct {
		name    string
		arrange func(*testing.T, *store) Workspace
		want    RemoteControl
	}{
		{name: "a repository's main worktree is on", arrange: testWorkspace, want: RemoteControl{On: true}},
		{name: "a linked worktree is off", arrange: testLinkedWorktree, want: RemoteControl{}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			ws := tt.arrange(t, s)

			// Act
			got, err := s.RemoteControl(context.Background(), ws.ID)

			// Assert
			if err != nil || got != tt.want {
				t.Fatalf("RemoteControl = %+v, %v; want %+v", got, err, tt.want)
			}
		})
	}
}

func TestSetRemoteControlMode(t *testing.T) {
	tests := []struct {
		name    string
		arrange func(*testing.T, *store) Workspace
		changes []RemoteControlChange
		want    bool
	}{
		{name: "off is recorded over a main worktree's default", arrange: testWorkspace, changes: []RemoteControlChange{RemoteControlOff}, want: false},
		{name: "on is recorded over a linked worktree's default", arrange: testLinkedWorktree, changes: []RemoteControlChange{RemoteControlOn}, want: true},
		{name: "a toggle turns a main worktree's default over", arrange: testWorkspace, changes: []RemoteControlChange{RemoteControlToggle}, want: false},
		{name: "a toggle turns a linked worktree's default over", arrange: testLinkedWorktree, changes: []RemoteControlChange{RemoteControlToggle}, want: true},
		{name: "a toggle turns a recorded mode over", arrange: testLinkedWorktree, changes: []RemoteControlChange{RemoteControlOn, RemoteControlToggle}, want: false},
		{name: "two toggles come back to the start", arrange: testWorkspace, changes: []RemoteControlChange{RemoteControlToggle, RemoteControlToggle}, want: true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			ws := tt.arrange(t, s)

			// Act
			var answered bool
			for _, change := range tt.changes {
				var err error
				if answered, err = s.SetRemoteControlMode(context.Background(), ws.ID, change); err != nil {
					t.Fatalf("SetRemoteControlMode(%s): %v", change, err)
				}
			}

			// Assert
			got, err := s.RemoteControl(context.Background(), ws.ID)
			if err != nil || answered != tt.want || got.On != tt.want || !got.Recorded {
				t.Fatalf("answered %v, RemoteControl = %+v, %v; want %v recorded", answered, got, err, tt.want)
			}
		})
	}
}

func TestARemoteControlChangeNamingNoModeIsRefused(t *testing.T) {
	// Arrange
	s, log := testStore(t)
	ws := testWorkspace(t, s)

	// Act
	_, err := s.SetRemoteControlMode(context.Background(), ws.ID, RemoteControlChange(0))

	// Assert
	if err == nil || !loggedOperation(log, "daemon.wsm.set_remote_control_mode", "error") {
		t.Fatalf("err = %v, want a refusal recorded at error", err)
	}
	if got, _ := s.RemoteControl(context.Background(), ws.ID); got.Recorded {
		t.Fatalf("RemoteControl = %+v, want nothing recorded", got)
	}
}

func TestTheRemoteControlOfAnUnknownWorkspaceIsNotFound(t *testing.T) {
	// Arrange
	s, _ := testStore(t)

	// Act
	_, err := s.RemoteControl(context.Background(), "no-such-workspace")

	// Assert
	if !errors.Is(err, ErrNotFound) {
		t.Fatalf("err = %v, want ErrNotFound", err)
	}
}

func TestSettingTheModeOfAnUnknownWorkspaceIsNotFound(t *testing.T) {
	// Arrange
	s, _ := testStore(t)

	// Act
	_, err := s.SetRemoteControlMode(context.Background(), "no-such-workspace", RemoteControlOn)

	// Assert
	if !errors.Is(err, ErrNotFound) {
		t.Fatalf("err = %v, want ErrNotFound", err)
	}
}

func TestABridgeSessionIsReadBackBesideTheDefault(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)

	// Act
	if err := s.SetRemoteControlBridgeSession(context.Background(), ws.ID, "bridge-1"); err != nil {
		t.Fatalf("SetRemoteControlBridgeSession: %v", err)
	}

	// Assert: a bridge alone records no mode; the default still decides it.
	got, err := s.RemoteControl(context.Background(), ws.ID)
	if want := (RemoteControl{On: true, BridgeSessionID: "bridge-1"}); err != nil || got != want {
		t.Fatalf("RemoteControl = %+v, %v; want %+v", got, err, want)
	}
}

func TestAModeChangeKeepsTheBridgeSession(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	if err := s.SetRemoteControlBridgeSession(context.Background(), ws.ID, "bridge-1"); err != nil {
		t.Fatalf("SetRemoteControlBridgeSession: %v", err)
	}

	// Act
	if _, err := s.SetRemoteControlMode(context.Background(), ws.ID, RemoteControlOff); err != nil {
		t.Fatalf("SetRemoteControlMode: %v", err)
	}

	// Assert
	if got, _ := s.RemoteControl(context.Background(), ws.ID); got.BridgeSessionID != "bridge-1" {
		t.Fatalf("RemoteControl = %+v, want the bridge session kept", got)
	}
}

func TestABridgeSessionKeepsTheMode(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	if _, err := s.SetRemoteControlMode(context.Background(), ws.ID, RemoteControlOff); err != nil {
		t.Fatalf("SetRemoteControlMode: %v", err)
	}

	// Act
	if err := s.SetRemoteControlBridgeSession(context.Background(), ws.ID, "bridge-2"); err != nil {
		t.Fatalf("SetRemoteControlBridgeSession: %v", err)
	}

	// Assert
	if got, _ := s.RemoteControl(context.Background(), ws.ID); got.On || !got.Recorded {
		t.Fatalf("RemoteControl = %+v, want the recorded off kept", got)
	}
}

func TestABlankBridgeSessionIsRefused(t *testing.T) {
	// Arrange
	s, log := testStore(t)
	ws := testWorkspace(t, s)

	// Act
	err := s.SetRemoteControlBridgeSession(context.Background(), ws.ID, " ")

	// Assert
	if err == nil || !loggedOperation(log, "daemon.wsm.set_remote_control_bridge_session", "error") {
		t.Fatalf("err = %v, want a refusal recorded at error", err)
	}
}

func TestAWorkspacesRemoteControlGoesWithIt(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	if _, err := s.SetRemoteControlMode(context.Background(), ws.ID, RemoteControlOff); err != nil {
		t.Fatalf("SetRemoteControlMode: %v", err)
	}

	// Act
	corrupt(t, s, `DELETE FROM workspaces WHERE id = ?`, ws.ID)

	// Assert
	if rows := scalar[int](t, s, `SELECT COUNT(*) FROM workspace_remote_control`); rows != 0 {
		t.Fatalf("remote-control rows = %d, want none after the workspace went", rows)
	}
}

// TestTheMigrationAddsTheRemoteControlTable pins the layout-91 step: a file
// written before the mode was durable carries the table afterwards, and every
// migrated workspace reads its default.
func TestTheMigrationAddsTheRemoteControlTable(t *testing.T) {
	// Arrange
	path := layout3Fixture(t)

	// Act
	handle, err := Open(context.Background(), path, WithUnsyncedWrites())
	if err != nil {
		t.Fatalf("Open on a layout-3 database: %v", err)
	}
	defer handle.Close()

	// Assert
	s := handle.(*store)
	if got := scalar[int](t, s, `SELECT count(*) FROM sqlite_master WHERE type = 'table' AND name = 'workspace_remote_control'`); got != 1 {
		t.Fatalf("workspace_remote_control exists %d times after the migration, want 1", got)
	}
}

func TestTheMigratedWorkspaceRecordsNoMode(t *testing.T) {
	// Arrange
	path := layout3Fixture(t)

	// Act
	handle, err := Open(context.Background(), path, WithUnsyncedWrites())
	if err != nil {
		t.Fatalf("Open on a layout-3 database: %v", err)
	}
	defer handle.Close()

	// Assert
	got, err := handle.RemoteControl(context.Background(), fixtureWorkspaceID)
	if err != nil || got.Recorded || got.BridgeSessionID != "" {
		t.Fatalf("RemoteControl = %+v, %v; want the default, nothing recorded", got, err)
	}
}
