package main

import (
	"bytes"
	"io"
	"os"
	"strings"
	"testing"
	"time"

	"claude-repld/internal/dlog"
	"claude-repld/internal/restartannounce"
	"claude-repld/internal/sessioncontroller"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"
)

// fakeRestartPublisher records what each carrier was handed, so a test can
// assert the announcement reached BOTH client kinds rather than one.
type fakeRestartPublisher struct {
	gui       []*frontendv1.RestartPendingView
	host      []*frontendv1.RestartPendingView
	guiCount  int
	hostCount int
}

func (f *fakeRestartPublisher) PushRestartPendingToGUI(v *frontendv1.RestartPendingView) int {
	f.gui = append(f.gui, v)
	return f.guiCount
}

func (f *fakeRestartPublisher) PushRestartPendingToHost(v *frontendv1.RestartPendingView) int {
	f.host = append(f.host, v)
	return f.hostCount
}

// The announcement must reach BOTH carriers. Delivering to one is the exact
// failure the two-sink split exists to make visible: whichever frontend was
// missed renders the deliberate bounce as an unexplained disconnect.
func TestAnnounceIntentionalRestartFansOutToBothClientKinds(t *testing.T) {
	// Arrange.
	var durable bytes.Buffer
	logger := dlog.New(&durable, io.Discard, false)
	pub := &fakeRestartPublisher{guiCount: 2, hostCount: 1}

	// Act.
	announceIntentionalRestart(logger, pub, shutdownRequest{
		stopShims: true,
		cause:     sessioncontroller.StopCauseDaemonShutdown(),
	})

	// Assert.
	if len(pub.gui) != 1 || len(pub.host) != 1 {
		t.Fatalf("want one announcement on each carrier, got gui=%d host=%d", len(pub.gui), len(pub.host))
	}
}

// stop_shims is a wire fact the client renders a longer settle from, so it
// must survive the hop from the shutdown request.
func TestAnnounceIntentionalRestartCarriesStopShims(t *testing.T) {
	// Arrange.
	logger := dlog.New(io.Discard, io.Discard, false)
	pub := &fakeRestartPublisher{guiCount: 1, hostCount: 1}

	// Act.
	announceIntentionalRestart(logger, pub, shutdownRequest{
		stopShims: true,
		cause:     sessioncontroller.StopCauseDaemonShutdown(),
	})

	// Assert.
	if !pub.gui[0].GetStopShims() {
		t.Fatal("want stop_shims carried onto the wire, got false")
	}
}

// The mint time is what lets a late-delivered announcement SHORTEN the
// client's window instead of restarting its clock, so it may never be zero.
func TestAnnounceIntentionalRestartCarriesAMintTime(t *testing.T) {
	// Arrange.
	logger := dlog.New(io.Discard, io.Discard, false)
	pub := &fakeRestartPublisher{guiCount: 1, hostCount: 1}

	// Act.
	announceIntentionalRestart(logger, pub, shutdownRequest{
		cause: sessioncontroller.StopCauseDaemonShutdown(),
	})

	// Assert.
	if pub.gui[0].GetAnnouncedAtMs() <= 0 {
		t.Fatalf("want a positive mint time, got %d", pub.gui[0].GetAnnouncedAtMs())
	}
}

// A missing publisher is a REAL failure: there is no carrier at all, so every
// connected client loses the notice. It must be recorded, not shrugged off.
func TestRestartAnnouncementSinksRefuseAMissingPublisher(t *testing.T) {
	// Arrange / Act.
	sinks, err := restartAnnouncementSinks(func(string, ...any) {}, nil)

	// Assert.
	if err == nil {
		t.Fatalf("want a refusal without a publisher, got %d sinks", len(sinks))
	}
}

// A sink that cannot report what it did is refused, because the delivery count
// is the only evidence the shutdown log gets.
func TestRestartAnnouncementSinksRefuseAMissingLogger(t *testing.T) {
	// Arrange / Act.
	_, err := restartAnnouncementSinks(nil, &fakeRestartPublisher{})

	// Assert.
	if err == nil {
		t.Fatal("want a refusal without a logger, got nil")
	}
}

// Zero connected clients of a kind is a COMPLETE delivery, not a failure: a
// frontend that is not connected renders no alarm to suppress. It is still
// logged, since it explains a degraded link appearing in the editor.
func TestAnnounceIntentionalRestartRecordsAnAbsentEditor(t *testing.T) {
	// Arrange.
	var durable bytes.Buffer
	logger := dlog.New(&durable, io.Discard, false)
	pub := &fakeRestartPublisher{guiCount: 1, hostCount: 0}

	// Act.
	announceIntentionalRestart(logger, pub, shutdownRequest{
		cause: sessioncontroller.StopCauseDaemonShutdown(),
	})

	// Assert.
	if !strings.Contains(durable.String(), "host_clients=0") {
		t.Fatalf("want the absent editor recorded, got %q", durable.String())
	}
}

// The wire field is whole seconds, so a sub-second hint would truncate to a
// zero the proto forbids. It floors at one second instead.
func TestRestartPendingViewFloorsASubSecondOutageHint(t *testing.T) {
	// Arrange.
	ann := restartannounce.Announcement{
		Cause:          "deploy",
		ExpectedOutage: 400 * time.Millisecond,
		AtMs:           1,
	}

	// Act.
	view := restartPendingView(ann)

	// Assert.
	if view.GetExpectedOutageSeconds() != 1 {
		t.Fatalf("want a sub-second hint floored to 1s, got %d", view.GetExpectedOutageSeconds())
	}
}

// The ordinary hint converts to whole seconds unchanged.
func TestRestartPendingViewConvertsTheOutageHintToSeconds(t *testing.T) {
	// Arrange.
	ann := restartannounce.Announcement{
		Cause:          "deploy",
		ExpectedOutage: 45 * time.Second,
		AtMs:           1,
	}

	// Act.
	view := restartPendingView(ann)

	// Assert.
	if view.GetExpectedOutageSeconds() != 45 {
		t.Fatalf("want 45s carried onto the wire, got %d", view.GetExpectedOutageSeconds())
	}
}

// THE ORDERING IS THE WHOLE MECHANISM. The announcement travels over the very
// sockets the teardown closes, so announcing after any listener is torn down
// is announcing to nobody — the frame would be marshaled, enqueued to a dead
// connection, and dropped, leaving every client with exactly the unexplained
// disconnect this feature exists to prevent.
//
// The sequence lives inline in main's shutdown goroutine, which cannot be
// called from a test without booting a daemon, so the guard reads the source
// and asserts the announcement precedes every teardown step. A reordering that
// broke the feature would otherwise pass every other test in this package,
// because the announcement would still be composed, still be delivered, and
// still be logged as delivered.
func TestAnnouncementPrecedesEveryShutdownTeardownStep(t *testing.T) {
	// Arrange.
	src, err := os.ReadFile("main.go")
	if err != nil {
		t.Fatalf("read main.go: %v", err)
	}
	text := string(src)
	announce := strings.Index(text, "announceIntentionalRestart(daemonLog")
	if announce < 0 {
		t.Fatal("the shutdown path no longer announces an intentional restart")
	}
	// Each step is anchored on its own shutdown-step operation tag rather than
	// on the call itself: the tags appear exactly once and only inside this
	// sequence, whereas a call like logins.CloseAll() also appears elsewhere in
	// the file and would anchor the guard on the wrong line.
	teardown := []struct {
		name string
		mark string
	}{
		{"workspace creation cancel", `"shutdown-stop-workspace-creation"`},
		{"session shutdown", `"shutdown-all-sessions"`},
		{"login terminal close", `"shutdown-close-logins"`},
		{"http listener close", `"shutdown-close-http"`},
	}

	for _, tc := range teardown {
		t.Run(tc.name, func(t *testing.T) {
			// Act.
			at := strings.Index(text, tc.mark)

			// Assert.
			if at < 0 {
				t.Fatalf("shutdown step %q not found; the ordering guard is now blind to it", tc.mark)
			}
			if announce > at {
				t.Fatalf("the restart announcement runs AFTER %q; it would reach no client", tc.mark)
			}
		})
	}
}

// The announcement must also run after the daemon stops declaring itself
// ready, so nothing reconnects into a daemon that is on its way out.
func TestAnnouncementFollowsTheReadinessFlip(t *testing.T) {
	// Arrange.
	src, err := os.ReadFile("main.go")
	if err != nil {
		t.Fatalf("read main.go: %v", err)
	}
	text := string(src)

	// Act.
	notReady := strings.Index(text, "ready.ready.Store(false)")
	announce := strings.Index(text, "announceIntentionalRestart(daemonLog")

	// Assert.
	if notReady < 0 || announce < 0 {
		t.Fatalf("shutdown markers missing: ready=%d announce=%d", notReady, announce)
	}
	if announce < notReady {
		t.Fatal("the announcement runs before the daemon stops declaring itself ready")
	}
}
