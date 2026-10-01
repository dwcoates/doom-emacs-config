package wsm

import (
	"context"
	"errors"
	"testing"
	"time"
)

func TestPutSessionRoundTripsTheSpawnIdentity(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	session := Session{
		Workspace: ws.ID, HostSessionID: "host-1", VendorSessionID: "vendor-1", ConfigDir: "/root/.claude",
		Model: "opus", PermissionMode: "default", StartedAt: instant, LastEngagementAt: instant,
	}

	// Act
	if err := s.PutSession(context.Background(), session); err != nil {
		t.Fatalf("PutSession: %v", err)
	}
	got, found, err := s.Session(context.Background(), ws.ID)

	// Assert
	if err != nil {
		t.Fatalf("Session: %v", err)
	}
	if !found {
		t.Fatalf("found = false for a recorded session")
	}
	if got.VendorSessionID != "vendor-1" || got.ConfigDir != "/root/.claude" || got.Model != "opus" || got.PermissionMode != "default" {
		t.Fatalf("session = %+v, want %+v", got, session)
	}
	if got.Terminal != nil {
		t.Fatalf("terminal = %+v on a live session, want nil", got.Terminal)
	}
}

func TestSessionReportsAbsence(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)

	// Act
	_, found, err := s.Session(context.Background(), ws.ID)

	// Assert
	if err != nil {
		t.Fatalf("Session: %v", err)
	}
	if found {
		t.Fatalf("found = true with no session recorded")
	}
}

func TestSetSessionTerminalPersistsTheCause(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	if err := s.PutSession(context.Background(), Session{Workspace: ws.ID, HostSessionID: "host-1", StartedAt: instant, LastEngagementAt: instant}); err != nil {
		t.Fatalf("PutSession: %v", err)
	}
	terminal := SessionTerminal{Kind: "shim_died", Detail: "exit status 1", At: instant}

	// Act
	if err := s.SetSessionTerminal(context.Background(), ws.ID, terminal); err != nil {
		t.Fatalf("SetSessionTerminal: %v", err)
	}

	// Assert
	got, _, err := s.Session(context.Background(), ws.ID)
	if err != nil {
		t.Fatalf("Session: %v", err)
	}
	if got.Terminal == nil || got.Terminal.Kind != "shim_died" || got.Terminal.Detail != "exit status 1" || !got.Terminal.At.Equal(instant) {
		t.Fatalf("terminal = %+v, want %+v", got.Terminal, terminal)
	}
}

func TestSetSessionTerminalRefusesAnUnknownSession(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)

	// Act
	err := s.SetSessionTerminal(context.Background(), ws.ID, SessionTerminal{Kind: "killed", At: instant})

	// Assert
	if !errors.Is(err, ErrNotFound) {
		t.Fatalf("SetSessionTerminal = %v, want ErrNotFound", err)
	}
}

func TestPutSessionRefusesResurrectionOfADeletedSession(t *testing.T) {
	// Arrange
	s, log := testStore(t)
	ws := testWorkspace(t, s)
	if err := s.PutSession(context.Background(), Session{Workspace: ws.ID, HostSessionID: "host-1", StartedAt: instant, LastEngagementAt: instant}); err != nil {
		t.Fatalf("PutSession: %v", err)
	}
	if err := s.SetSessionTerminal(context.Background(), ws.ID, SessionTerminal{Kind: "deleted", At: instant}); err != nil {
		t.Fatalf("SetSessionTerminal: %v", err)
	}

	// Act
	err := s.PutSession(context.Background(), Session{Workspace: ws.ID, HostSessionID: "host-2", VendorSessionID: "second", StartedAt: instant, LastEngagementAt: instant})

	// Assert
	if !errors.Is(err, ErrSessionDeleted) {
		t.Fatalf("PutSession = %v, want ErrSessionDeleted", err)
	}
	if !loggedOperation(log, "daemon.wsm.put_session", "error") {
		t.Fatalf("the refusal was not logged at error: %v", log.Records())
	}
}

func TestSetSessionTerminalRefusesReterminatingADeletedSession(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	if err := s.PutSession(context.Background(), Session{Workspace: ws.ID, HostSessionID: "host-1", StartedAt: instant, LastEngagementAt: instant}); err != nil {
		t.Fatalf("PutSession: %v", err)
	}
	if err := s.SetSessionTerminal(context.Background(), ws.ID, SessionTerminal{Kind: "deleted", At: instant}); err != nil {
		t.Fatalf("SetSessionTerminal: %v", err)
	}

	// Act
	err := s.SetSessionTerminal(context.Background(), ws.ID, SessionTerminal{Kind: "killed", At: instant})

	// Assert
	if !errors.Is(err, ErrSessionDeleted) {
		t.Fatalf("SetSessionTerminal = %v, want ErrSessionDeleted", err)
	}
}

func TestClearSessionTerminalRetiresTheCause(t *testing.T) {
	// Arrange: a killed session whose shim is live again.
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	if err := s.PutSession(context.Background(), Session{Workspace: ws.ID, HostSessionID: "host-1", StartedAt: instant, LastEngagementAt: instant}); err != nil {
		t.Fatalf("PutSession: %v", err)
	}
	if err := s.SetSessionTerminal(context.Background(), ws.ID, SessionTerminal{Kind: "killed", Detail: "KillWorkspace", At: instant}); err != nil {
		t.Fatalf("SetSessionTerminal: %v", err)
	}

	// Act
	if err := s.ClearSessionTerminal(context.Background(), ws.ID); err != nil {
		t.Fatalf("ClearSessionTerminal: %v", err)
	}

	// Assert
	got, found, err := s.Session(context.Background(), ws.ID)
	if err != nil {
		t.Fatalf("Session: %v", err)
	}
	if !found {
		t.Fatalf("found = false after retiring a terminal, want the session kept")
	}
	if got.Terminal != nil {
		t.Fatalf("terminal = %+v after the retirement, want nil", got.Terminal)
	}
}

func TestClearSessionTerminalAcceptsAWorkspaceWithNoSession(t *testing.T) {
	// Arrange: nothing was ever recorded, so no terminal stands.
	s, _ := testStore(t)
	ws := testWorkspace(t, s)

	// Act
	err := s.ClearSessionTerminal(context.Background(), ws.ID)

	// Assert
	if err != nil {
		t.Fatalf("ClearSessionTerminal = %v, want the absent record accepted", err)
	}
}

func TestClearSessionTerminalRefusesToResurrectADeletedSession(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	if err := s.PutSession(context.Background(), Session{Workspace: ws.ID, HostSessionID: "host-1", StartedAt: instant, LastEngagementAt: instant}); err != nil {
		t.Fatalf("PutSession: %v", err)
	}
	if err := s.SetSessionTerminal(context.Background(), ws.ID, SessionTerminal{Kind: "deleted", At: instant}); err != nil {
		t.Fatalf("SetSessionTerminal: %v", err)
	}

	// Act
	err := s.ClearSessionTerminal(context.Background(), ws.ID)

	// Assert
	if !errors.Is(err, ErrSessionDeleted) {
		t.Fatalf("ClearSessionTerminal = %v, want ErrSessionDeleted", err)
	}
}

func TestClearSessionTerminalKeepsADeletedSessionsCause(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	if err := s.PutSession(context.Background(), Session{Workspace: ws.ID, HostSessionID: "host-1", StartedAt: instant, LastEngagementAt: instant}); err != nil {
		t.Fatalf("PutSession: %v", err)
	}
	if err := s.SetSessionTerminal(context.Background(), ws.ID, SessionTerminal{Kind: "deleted", Detail: "forget", At: instant}); err != nil {
		t.Fatalf("SetSessionTerminal: %v", err)
	}

	// Act
	_ = s.ClearSessionTerminal(context.Background(), ws.ID)

	// Assert
	got, _, err := s.Session(context.Background(), ws.ID)
	if err != nil {
		t.Fatalf("Session: %v", err)
	}
	if got.Terminal == nil || got.Terminal.Kind != "deleted" {
		t.Fatalf("terminal = %+v after a refused retirement, want the deletion kept", got.Terminal)
	}
}

func TestTouchEngagementStampsTheIdleSweepsInput(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	if err := s.PutSession(context.Background(), Session{Workspace: ws.ID, HostSessionID: "host-1", StartedAt: instant, LastEngagementAt: instant}); err != nil {
		t.Fatalf("PutSession: %v", err)
	}
	later := instant.Add(time.Hour)

	// Act
	if err := s.TouchEngagement(context.Background(), ws.ID, later); err != nil {
		t.Fatalf("TouchEngagement: %v", err)
	}

	// Assert
	got, _, err := s.Session(context.Background(), ws.ID)
	if err != nil {
		t.Fatalf("Session: %v", err)
	}
	if !got.LastEngagementAt.Equal(later) {
		t.Fatalf("last engagement = %v, want %v", got.LastEngagementAt, later)
	}
}

func TestTouchEngagementRefusesAnUnknownSession(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)

	// Act
	err := s.TouchEngagement(context.Background(), ws.ID, instant)

	// Assert
	if !errors.Is(err, ErrNotFound) {
		t.Fatalf("TouchEngagement = %v, want ErrNotFound", err)
	}
}

func TestSessionFailsWholeOnAHalfWrittenTerminal(t *testing.T) {
	// Arrange
	s, log := testStore(t)
	ws := testWorkspace(t, s)
	if err := s.PutSession(context.Background(), Session{Workspace: ws.ID, HostSessionID: "host-1", StartedAt: instant, LastEngagementAt: instant}); err != nil {
		t.Fatalf("PutSession: %v", err)
	}
	corrupt(t, s, `UPDATE sessions SET terminal_at = 1 WHERE workspace_id = ?`, ws.ID)

	// Act
	_, found, err := s.Session(context.Background(), ws.ID)

	// Assert
	var refusal *DecodeError
	if !errors.As(err, &refusal) || refusal.Table != "sessions" || refusal.Field != "terminal" {
		t.Fatalf("Session = %v, want a *DecodeError naming sessions.terminal", err)
	}
	if found {
		t.Fatalf("found = true alongside the refusal")
	}
	if !loggedOperation(log, "daemon.wsm.session", "error") {
		t.Fatalf("the decode failure was not logged at error: %v", log.Records())
	}
}

func TestSetShimPIDRoundTripsTheManifestPid(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	if err := s.PutSession(context.Background(), Session{
		Workspace: ws.ID, HostSessionID: "host-1", VendorSessionID: "vendor-1", ConfigDir: "/root/.claude",
		Model: "opus", PermissionMode: "default", StartedAt: instant, LastEngagementAt: instant,
	}); err != nil {
		t.Fatalf("PutSession: %v", err)
	}
	pid := 4242

	// Act
	if err := s.SetShimPID(context.Background(), ws.ID, &pid); err != nil {
		t.Fatalf("SetShimPID: %v", err)
	}
	got, _, err := s.Session(context.Background(), ws.ID)

	// Assert
	if err != nil {
		t.Fatalf("Session: %v", err)
	}
	if got.ShimPID == nil || *got.ShimPID != pid {
		t.Fatalf("shim pid = %v, want %d", got.ShimPID, pid)
	}
}

func TestSetShimPIDClearsThePidAtStandDown(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	pid := 4242
	if err := s.PutSession(context.Background(), Session{
		Workspace: ws.ID, HostSessionID: "host-1", VendorSessionID: "vendor-1", ConfigDir: "/root/.claude",
		Model: "opus", PermissionMode: "default", StartedAt: instant, LastEngagementAt: instant,
		ShimPID: &pid,
	}); err != nil {
		t.Fatalf("PutSession: %v", err)
	}

	// Act
	if err := s.SetShimPID(context.Background(), ws.ID, nil); err != nil {
		t.Fatalf("SetShimPID: %v", err)
	}
	got, _, err := s.Session(context.Background(), ws.ID)

	// Assert
	if err != nil {
		t.Fatalf("Session: %v", err)
	}
	if got.ShimPID != nil {
		t.Fatalf("shim pid = %d, want nil after a stand-down", *got.ShimPID)
	}
}

func TestSetShimPIDRefusesANonPositivePid(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	if err := s.PutSession(context.Background(), Session{
		Workspace: ws.ID, HostSessionID: "host-1", VendorSessionID: "vendor-1", ConfigDir: "/root/.claude",
		Model: "opus", PermissionMode: "default", StartedAt: instant, LastEngagementAt: instant,
	}); err != nil {
		t.Fatalf("PutSession: %v", err)
	}
	zero := 0

	// Act
	err := s.SetShimPID(context.Background(), ws.ID, &zero)

	// Assert
	if err == nil {
		t.Fatalf("SetShimPID accepted a non-positive pid")
	}
}

func TestSessionRefusesACorruptShimPid(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	if err := s.PutSession(context.Background(), Session{
		Workspace: ws.ID, HostSessionID: "host-1", VendorSessionID: "vendor-1", ConfigDir: "/root/.claude",
		Model: "opus", PermissionMode: "default", StartedAt: instant, LastEngagementAt: instant,
	}); err != nil {
		t.Fatalf("PutSession: %v", err)
	}
	corrupt(t, s, `UPDATE sessions SET shim_pid = -1 WHERE workspace_id = ?`, ws.ID)

	// Act
	_, _, err := s.Session(context.Background(), ws.ID)

	// Assert
	var decode *DecodeError
	if !errors.As(err, &decode) {
		t.Fatalf("Session error = %v, want a DecodeError for a corrupt shim pid", err)
	}
}

func TestPutSessionRefusesARowWithNoHostIdentity(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)

	// Act
	err := s.PutSession(context.Background(), Session{
		Workspace: ws.ID, VendorSessionID: "vendor-1", ConfigDir: "/root/.claude",
		Model: "opus", PermissionMode: "default", StartedAt: instant, LastEngagementAt: instant,
	})

	// Assert
	if !errors.Is(err, ErrSessionIdentityMissing) {
		t.Fatalf("PutSession = %v, want ErrSessionIdentityMissing", err)
	}
}

func TestPutSessionWritesNothingWhenItRefusesAnIdentitylessRow(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)

	// Act
	_ = s.PutSession(context.Background(), Session{
		Workspace: ws.ID, StartedAt: instant, LastEngagementAt: instant,
	})

	// Assert
	_, found, err := s.Session(context.Background(), ws.ID)
	if err != nil {
		t.Fatalf("Session: %v", err)
	}
	if found {
		t.Fatalf("a refused identityless session was persisted anyway")
	}
}

func TestPutSessionRoundTripsTheSelectedAccountRoot(t *testing.T) {
	// Arrange — a workspace whose user CHOSE a root that is not the one the
	// session last came up under.
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	session := Session{
		Workspace: ws.ID, HostSessionID: "host-1", VendorSessionID: "vendor-1",
		ConfigDir: "/root/.claude", SelectedConfigDir: "/root/.claude-chesscom",
		Model: "opus", PermissionMode: "default", StartedAt: instant, LastEngagementAt: instant,
	}

	// Act
	if err := s.PutSession(context.Background(), session); err != nil {
		t.Fatalf("PutSession: %v", err)
	}
	got, _, err := s.Session(context.Background(), ws.ID)

	// Assert
	if err != nil {
		t.Fatalf("Session: %v", err)
	}
	if got.SelectedConfigDir != "/root/.claude-chesscom" {
		t.Fatalf("selected config dir = %q, want %q", got.SelectedConfigDir, "/root/.claude-chesscom")
	}
	if got.ConfigDir != "/root/.claude" {
		t.Fatalf("config dir = %q, want the root the session came up under", got.ConfigDir)
	}
}

func TestPutSessionRecordsNoSelectedRootWhenNobodyChoseOne(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	session := Session{
		Workspace: ws.ID, HostSessionID: "host-1", VendorSessionID: "vendor-1", ConfigDir: "/root/.claude",
		Model: "opus", PermissionMode: "default", StartedAt: instant, LastEngagementAt: instant,
	}

	// Act
	if err := s.PutSession(context.Background(), session); err != nil {
		t.Fatalf("PutSession: %v", err)
	}
	got, _, err := s.Session(context.Background(), ws.ID)

	// Assert
	if err != nil {
		t.Fatalf("Session: %v", err)
	}
	if got.SelectedConfigDir != "" {
		t.Fatalf("selected config dir = %q with no choice recorded, want empty", got.SelectedConfigDir)
	}
}

func TestSetVendorSessionIDRecordsTheIdInForceAndAnswersTheOneItReplaced(t *testing.T) {
	// Arrange
	db, _ := testStore(t)
	ws := vendorSession(t, db, "vendor-start")

	// Act
	previous, err := db.SetVendorSessionID(context.Background(), ws, "vendor-rotated")

	// Assert
	if err != nil || previous != "vendor-start" {
		t.Fatalf("SetVendorSessionID = (%q, %v), want the start's id answered", previous, err)
	}
	session, _, err := db.Session(context.Background(), ws)
	if err != nil || session.VendorSessionID != "vendor-rotated" {
		t.Fatalf("Session = (%+v, %v), want the rotated id recorded", session, err)
	}
}

func TestSetVendorSessionIDRefusesAnEmptyId(t *testing.T) {
	// Arrange
	db, log := testStore(t)
	ws := vendorSession(t, db, "vendor-start")

	// Act
	_, err := db.SetVendorSessionID(context.Background(), ws, "")

	// Assert
	if err == nil {
		t.Fatal("an empty vendor session id was recorded")
	}
	if session, _, _ := db.Session(context.Background(), ws); session.VendorSessionID != "vendor-start" {
		t.Fatalf("vendor session = %q, want the record untouched", session.VendorSessionID)
	}
	if _, ok := recordFor(log, "daemon.wsm.set_vendor_session_id", "error", "refused an empty vendor session id"); !ok {
		t.Fatalf("records = %+v, want the refusal at ERROR", log.Records())
	}
}

func TestSetVendorSessionIDRefusesAWorkspaceWithNoSession(t *testing.T) {
	// Arrange
	db, _ := testStore(t)

	// Act
	_, err := db.SetVendorSessionID(context.Background(), "no-such-workspace", "vendor-1")

	// Assert
	if !errors.Is(err, ErrNotFound) {
		t.Fatalf("SetVendorSessionID = %v, want ErrNotFound", err)
	}
}

// vendorSession registers a workspace whose session record names vendor.
func vendorSession(t *testing.T, s *store, vendor string) WorkspaceID {
	t.Helper()
	ws := testWorkspace(t, s)
	if err := s.PutSession(context.Background(), Session{
		Workspace: ws.ID, HostSessionID: "host-1", VendorSessionID: vendor, StartedAt: time.Unix(1, 0),
	}); err != nil {
		t.Fatalf("PutSession: %v", err)
	}
	return ws.ID
}
