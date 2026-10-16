//go:build integration

package integration

import (
	"testing"
	"time"

	frontendv1 "agentrepl/proto/frontend/v1"

	"agent-repld/integration/harness"
)

// hasSession reports whether a topbar's connectivity indicator carries
// agent-repl's session.
func hasSession(v *frontendv1.TopbarView) bool {
	return v.GetConnectivity().GetSession() != nil
}

func TestANewEmacsBeginsAgentReplsSessionOnTheTopbar(t *testing.T) {
	t.Parallel()
	// Arrange.
	f := newOpened(t, harness.Opts{})
	topbar := f.d.WatchTopbar(f.ws)
	before := time.Now()

	// Act.
	f.d.WatchDaemonStream()

	// Assert.
	view := awaitTopbar(t, f, topbar, "the session a new Emacs began", hasSession)
	session := view.GetConnectivity().GetSession()
	started := time.UnixMilli(session.GetStartedAtMs())
	if session.GetEditorStart() == nil || started.Before(before.Truncate(time.Millisecond)) || started.After(time.Now()) {
		t.Fatalf("session = %v, want an editor-start session begun during the test", session)
	}
}

func TestAgentReplsSessionSurvivesADaemonRestart(t *testing.T) {
	t.Parallel()
	// Arrange: a new Emacs began a session on the first daemon.
	f := newOpened(t, harness.Opts{DurableState: true})
	topbar := f.d.WatchTopbar(f.ws)
	f.d.WatchDaemonStream()
	began := awaitTopbar(t, f, topbar, "the session a new Emacs began", hasSession).GetConnectivity().GetSession()

	// Act.
	f2 := restarted(f, harness.ShimProfile{})

	// Assert.
	view := awaitTopbar(t, f2, f2.d.WatchTopbar(f2.ws), "the carried session", hasSession)
	if got := view.GetConnectivity().GetSession(); got.GetStartedAtMs() != began.GetStartedAtMs() || got.GetEditorStart() == nil {
		t.Fatalf("session after the restart = %v, want the one begun at %d", got, began.GetStartedAtMs())
	}
}
