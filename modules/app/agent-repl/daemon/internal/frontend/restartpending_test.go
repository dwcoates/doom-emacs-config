package frontend

import (
	"testing"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"
)

// The GUI carrier reaches the rendering webview — the client that paints the
// severed banner and therefore the one the notice exists for.
func TestPushRestartPendingToGUIReachesAStreamClient(t *testing.T) {
	// Arrange.
	s, _ := newTestServer(t, 0)
	defer s.Close()
	stream, _ := dialScoped(t, s, Scope{Workspace: "w1", SessionID: "s1"}, ClientKindGUIStream)

	// Act.
	n := s.PushRestartPendingToGUI(&frontendv1.RestartPendingView{
		Cause: "deploy-all rebuilt the daemon", ExpectedOutageSeconds: 60, AnnouncedAtMs: 1234,
	})

	// Assert.
	if n != 1 {
		t.Fatalf("want the stream client counted, got %d", n)
	}
	if got := readWSFrame(t, stream).GetRestartPending().GetCause(); got != "deploy-all rebuilt the daemon" {
		t.Fatalf("cause = %q, want the announced cause", got)
	}
}

// The host carrier reaches Emacs, which renders its own degraded-link segment
// on an unexplained disconnect.
func TestPushRestartPendingToHostReachesTheHostClient(t *testing.T) {
	// Arrange.
	s, _ := newTestServer(t, 0)
	defer s.Close()
	host, _ := dialScoped(t, s, Scope{}, ClientKindHost)

	// Act.
	n := s.PushRestartPendingToHost(&frontendv1.RestartPendingView{
		Cause: "SIGTERM", ExpectedOutageSeconds: 30, AnnouncedAtMs: 99,
	})

	// Assert.
	if n != 1 {
		t.Fatalf("want the host client counted, got %d", n)
	}
	if got := readWSFrame(t, host).GetRestartPending().GetCause(); got != "SIGTERM" {
		t.Fatalf("cause = %q, want SIGTERM", got)
	}
}

// The two carriers are DISJOINT. If the GUI push also reached the host, one
// failed carrier would silently look like a delivered one and the shutdown log
// could no longer say which frontend was missed.
func TestPushRestartPendingToGUIExcludesTheHostClient(t *testing.T) {
	// Arrange.
	s, _ := newTestServer(t, 0)
	defer s.Close()
	dialScoped(t, s, Scope{}, ClientKindHost)

	// Act.
	n := s.PushRestartPendingToGUI(&frontendv1.RestartPendingView{
		Cause: "deploy", ExpectedOutageSeconds: 60, AnnouncedAtMs: 1,
	})

	// Assert.
	if n != 0 {
		t.Fatalf("the GUI carrier must not count the host client, got %d", n)
	}
}

// The mirror of the above: the host carrier never counts a GUI client.
func TestPushRestartPendingToHostExcludesGUIClients(t *testing.T) {
	// Arrange.
	s, _ := newTestServer(t, 0)
	defer s.Close()
	dialScoped(t, s, Scope{Workspace: "w1", SessionID: "s1"}, ClientKindGUIStream)

	// Act.
	n := s.PushRestartPendingToHost(&frontendv1.RestartPendingView{
		Cause: "deploy", ExpectedOutageSeconds: 60, AnnouncedAtMs: 1,
	})

	// Assert.
	if n != 0 {
		t.Fatalf("the host carrier must not count a GUI client, got %d", n)
	}
}

// A session-SCOPED webview must receive the notice even though the frame names
// no workspace: the daemon going down takes that client's session with it.
func TestRestartPendingSurvivesSessionScoping(t *testing.T) {
	// Arrange.
	frame := RestartPendingFrame(&frontendv1.RestartPendingView{
		Cause: "deploy", ExpectedOutageSeconds: 60, AnnouncedAtMs: 1,
	})

	// Act.
	_, keep := scopeFrame(frame, Scope{Workspace: "w1", SessionID: "s1"})

	// Assert.
	if !keep {
		t.Fatal("a scoped webview must still receive the restart announcement")
	}
}
