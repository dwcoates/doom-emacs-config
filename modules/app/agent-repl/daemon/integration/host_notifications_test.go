//go:build integration

package integration

import (
	"context"
	"runtime"
	"slices"
	"strings"
	"testing"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	conversationv1 "agentrepl/proto/conversation/v1"

	"agent-repld/integration/harness"
	"agent-repld/internal/desktopnotify"
)

// ==========================================================================
// Desktop banners. The daemon posts every banner itself, through the Zettel
// app on macOS (the harness's fake app) or the platform's banner program
// elsewhere (the harness's recorder), decided on Emacs's focus: the focus
// an Emacs WatchDaemon stream connects with, moved by ReportEditorFocus, and
// forgotten when the stream ends. A banner click reaches Emacs as the host
// stream's notification_clicked.
// ==========================================================================

// bannerSettled is the daemon's record that a banner's program returned with
// no click to relay: a read-back banner dismissed or timed out, or a banner
// the platform now holds (macOS), whose click is reported on its own.
func bannerSettled(r harness.LogRecord) bool {
	return r.Operation == "daemon.desktopnotify.post" &&
		(r.Message == "the banner was dismissed or timed out" ||
			r.Message == "the platform holds the banner; its click is reported on its own")
}

// awaitBannerSettled waits for a banner to settle, and answers the words of
// every banner posted so far (harness.Daemon.PostedBanners).
func awaitBannerSettled(t *testing.T, f *fixture, what string) [][]string {
	t.Helper()
	f.d.AwaitOwnRunLogRecord(what, bannerSettled)
	return f.d.PostedBannersFor(f.ws.GetId())
}

// bannerCarrying reports whether any posted banner carries every one of words
// in one of its words, whatever the platform's spelling.
func bannerCarrying(argvs [][]string, words ...string) bool {
	for _, argv := range argvs {
		matched := true
		for _, word := range words {
			if !slices.ContainsFunc(argv, func(arg string) bool { return strings.Contains(arg, word) }) {
				matched = false
				break
			}
		}
		if matched {
			return true
		}
	}
	return false
}

// TestACompletedTurnRaisesTheCompletedBanner pins the turn-end banner: a turn
// that concluded raises "✅ <name>" over the summary the
// daemon's headless call wrote (the fake vendor answers a question that is
// neither the classifier's nor an abbreviation with its minted name), set in
// italics.
func TestACompletedTurnRaisesTheCompletedBanner(t *testing.T) {
	t.Parallel()
	// Arrange
	f := newInGroup(t, groupDesktopBanner)
	f.submit("go", "k-turn-banner", conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT)

	// Act
	pushConcludedTurn(f.shim, mainAgent, "answer-1")

	// Assert
	argvs := awaitBannerSettled(t, f, "the completed-turn banner")
	if !bannerCarrying(argvs, "✅", desktopnotify.Italic(harness.FakeClaudeMintedName)) {
		t.Fatalf("banners %q, want the completed title over the model's summary", argvs)
	}
}

// TestReportEditorFocusMovesTheBannerDecision pins that a report moves the
// focus the next banner is decided on.
func TestReportEditorFocusMovesTheBannerDecision(t *testing.T) {
	t.Parallel()
	// Arrange
	f := newInGroup(t, groupDesktopBanner)
	f.d.WatchEmacsDaemonStream(harness.UnfocusedEditor())
	resp, err := f.d.Client().ReportEditorFocus(context.Background(),
		connect.NewRequest(&agentreplv1.ReportEditorFocusRequest{Focus: harness.FocusedEditor()}))
	if err != nil || resp.Msg.GetSuccess() == nil {
		t.Fatalf("ReportEditorFocus = (%v, %v), want success", resp, err)
	}
	f.submit("go", "k-reported", conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT)

	// Act
	pushConcludedTurn(f.shim, mainAgent, "answer-1")

	// Assert
	f.d.AwaitOwnRunLogRecord("the suppressed banner", func(r harness.LogRecord) bool {
		return r.Operation == "daemon.desktopnotify.post" && r.Message == "Emacs is focused; no desktop banner"
	})
	if got := f.d.PostedBannersFor(f.ws.GetId()); len(got) != 0 {
		t.Fatalf("a reported-focused Emacs got %d banners, want none", len(got))
	}
}

// TestAClickedBannerAsksEmacsToSelectTheWorkspace pins a READ-BACK click
// (Linux): the banner program reports a click, and the Emacs daemon stream
// carries notification_clicked naming the workspace. The case stands an Emacs
// daemon stream, whose focus is daemon-wide, so it takes its own daemon.
func TestAClickedBannerAsksEmacsToSelectTheWorkspace(t *testing.T) {
	if runtime.GOOS == "darwin" {
		t.Skip("a macOS banner is handed to the platform; TestAHandedBannersClickSelectsTheWorkspace pins its click")
	}
	t.Parallel()
	// Arrange
	f := newOpened(t, harness.Opts{})
	editor := f.d.WatchEmacsDaemonStream(harness.UnfocusedEditor())
	f.d.Notifier.SetStdout("default")
	f.submit("go", "k-clicked", conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT)

	// Act
	pushConcludedTurn(f.shim, mainAgent, "answer-1")

	// Assert
	harness.AwaitView(t, f.d.Ctx(), editor, "the banner click on the Emacs daemon stream",
		func(r *agentreplv1.WatchDaemonResponse) bool {
			return r.GetNotificationClicked().GetWorkspace().GetId() == f.ws.GetId()
		})
}

// TestAHandedBannersClickSelectsTheWorkspace pins a HANDED click (macOS): the
// launcher reports the click with the banner's own token, and the Emacs daemon
// stream carries notification_clicked naming the banner's workspace. The case
// stands an Emacs daemon stream, whose focus is daemon-wide, so it takes its
// own daemon.
func TestAHandedBannersClickSelectsTheWorkspace(t *testing.T) {
	if runtime.GOOS != "darwin" {
		t.Skip("only a macOS banner is handed to the launcher")
	}
	t.Parallel()
	// Arrange
	f := newOpened(t, harness.Opts{})
	editor := f.d.WatchEmacsDaemonStream(harness.UnfocusedEditor())
	f.submit("go", "k-handed", conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT)
	pushConcludedTurn(f.shim, mainAgent, "answer-1")
	awaitBannerSettled(t, f, "the handed turn banner")
	banners := f.d.Launcher.BannersFor("agent-repl:" + f.ws.GetId())
	if len(banners) != 1 {
		t.Fatalf("banners %v, want exactly one", banners)
	}

	// Act
	resp, err := f.d.Client().ReportDesktopBannerClicked(f.d.Ctx(), connect.NewRequest(&agentreplv1.ReportDesktopBannerClickedRequest{
		Click: banners[0].GetClick(),
	}))

	// Assert
	if err != nil || resp.Msg.GetSuccess() == nil {
		t.Fatalf("ReportDesktopBannerClicked = (%v, %v), want success", resp, err)
	}
	harness.AwaitView(t, f.d.Ctx(), editor, "the banner click on the Emacs daemon stream",
		func(r *agentreplv1.WatchDaemonResponse) bool {
			return r.GetNotificationClicked().GetWorkspace().GetId() == f.ws.GetId()
		})
}
