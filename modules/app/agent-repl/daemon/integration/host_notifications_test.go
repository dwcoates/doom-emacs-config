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
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/integration/harness"
)

// ==========================================================================
// Desktop banners. The daemon posts every banner itself, through the platform's
// banner program (the harness's recorder), decided on Emacs's focus: the focus
// an Emacs WatchDaemon stream connects with, moved by ReportEditorFocus, and
// forgotten when the stream ends. A banner click reaches Emacs as the host
// stream's notification_clicked.
// ==========================================================================

// awaitBannerSettled waits for the daemon's record that a banner's program
// returned without a click, and answers every banner argv posted so far.
func awaitBannerSettled(t *testing.T, f *fixture, what string) [][]string {
	t.Helper()
	f.d.AwaitLogRecord(f.d.RunLogPath(), what, func(r harness.LogRecord) bool {
		return r.Operation == "daemon.desktopnotify.post" && r.Message == "the banner was dismissed or timed out"
	})
	var argvs [][]string
	for _, inv := range f.d.Notifier.Invocations() {
		argvs = append(argvs, inv.Argv)
	}
	return argvs
}

// bannerCarrying reports whether any posted argv carries every one of words
// as an argument, whatever the platform's flag spelling.
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

// clickToken is what this platform's banner program prints for a click.
func clickToken() string {
	if runtime.GOOS == "darwin" {
		return "@CONTENTCLICKED"
	}
	return "default"
}

// TestQuestionAskedRaisesABannerAndSetsAttention pins that a question batch
// raises the workspace's desktop banner — the question's line under the
// workspace's name — and the roster's attention marker.
func TestQuestionAskedRaisesABannerAndSetsAttention(t *testing.T) {
	t.Parallel()
	// Arrange
	f := newOpened(t, harness.Opts{})
	f.submit("go", "k-notify-question", conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT)
	roster := f.d.WatchRoster()

	// Act
	f.shim.PushAgentFrame(mainAgent, updateFrame(mainAgent, &conversationv1.AgentUpdate{
		Update: &conversationv1.AgentUpdate_Question{Question: &conversationv1.AgentQuestion{
			Id: &conversationv1.AgentQuestionId{Value: "q-notify"},
			Result: &conversationv1.AgentQuestion_Start{Start: &conversationv1.AgentQuestionStart{
				Batch: &conversationv1.AgentQuestionBatch{Questions: []*conversationv1.AgentQuestionAsked{{
					Question: &conversationv1.AgentQuestionText{Text: "Which auth method?"},
					Header:   "Auth method",
					Choices: &conversationv1.AgentQuestionAsked_SingleSelect{SingleSelect: &conversationv1.AgentQuestionSingleSelect{
						Options: []*conversationv1.AgentQuestionOption{{Label: &conversationv1.AgentQuestionOptionLabel{Label: "OAuth"}}},
					}},
				}}},
				StartedAt: startedAt(1),
			}},
		}},
	}))

	// Assert
	argvs := awaitBannerSettled(t, f, "the question banner")
	if !bannerCarrying(argvs, "Auth method") {
		t.Fatalf("banners %q, want one carrying the question's line", argvs)
	}
	awaitRoster(t, f.d, roster, "the attention marker set by the question", func(r *frontendv1.WorkspaceRoster) bool {
		row := rosterRow(r, f.ws.GetId())
		return row != nil && row.GetAttention() != nil
	})
}

// TestAgentAddressedRaisesABannerWithItsText pins that an agent push
// notification raises a banner carrying the pushed message verbatim.
func TestAgentAddressedRaisesABannerWithItsText(t *testing.T) {
	t.Parallel()
	// Arrange
	f := newOpened(t, harness.Opts{})
	f.submit("go", "k-notify-addressed", conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT)

	// Act
	f.shim.PushAgentFrame(mainAgent, activityFrame(mainAgent, &conversationv1.AgentActivity{
		ActivityId: activityID("push-1"),
		Item: &conversationv1.AgentActivity_PushNotification{PushNotification: &conversationv1.AgentPushNotification{
			State: &conversationv1.AgentPushNotification_Start{Start: &conversationv1.AgentPushNotificationStart{
				Message:   "the deploy finished",
				StartedAt: startedAt(1),
			}},
		}},
	}))

	// Assert
	if argvs := awaitBannerSettled(t, f, "the agent push banner"); !bannerCarrying(argvs, "the deploy finished") {
		t.Fatalf("banners %q, want one carrying the pushed message", argvs)
	}
}

// TestACompletedTurnRaisesTheCompletedBanner pins the turn-end banner: a turn
// that concluded raises ✅ "<name> turn completed <time>" over the summary the
// daemon's headless call wrote (the fake vendor answers ROUTE_HOLD to text).
func TestACompletedTurnRaisesTheCompletedBanner(t *testing.T) {
	t.Parallel()
	// Arrange
	f := newOpened(t, harness.Opts{})
	f.submit("go", "k-turn-banner", conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT)

	// Act
	pushConcludedTurn(f.shim, mainAgent, "answer-1")

	// Assert
	argvs := awaitBannerSettled(t, f, "the completed-turn banner")
	if !bannerCarrying(argvs, "✅", "turn completed", "ROUTE_HOLD") {
		t.Fatalf("banners %q, want the completed title over the model's summary", argvs)
	}
}

// TestAFailedTurnRaisesTheErroredBanner pins that a failed turn raises ❌
// "<name> turn errored <time>" over the errored ending's line.
func TestAFailedTurnRaisesTheErroredBanner(t *testing.T) {
	t.Parallel()
	// Arrange
	f := newOpened(t, harness.Opts{})
	f.submit("go", "k-turn-failed-banner", conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT)

	// Act
	f.shim.PushAgentFrame(mainAgent, failureFrame(mainAgent, &conversationv1.AgentFailure{
		Failure: &conversationv1.AgentFailure_MaxTurns{MaxTurns: &conversationv1.AgentMaxTurnsReached{}},
	}))

	// Assert
	if argvs := awaitBannerSettled(t, f, "the errored-turn banner"); !bannerCarrying(argvs, "❌", "turn errored") {
		t.Fatalf("banners %q, want the errored title", argvs)
	}
}

// TestAFocusedEmacsGetsNoBanner pins the focus rule: an Emacs stream that
// connects focused suppresses every banner.
func TestAFocusedEmacsGetsNoBanner(t *testing.T) {
	t.Parallel()
	// Arrange
	f := newOpened(t, harness.Opts{})
	f.d.WatchEmacsDaemonStream(harness.FocusedEditor())
	f.submit("go", "k-focused", conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT)

	// Act
	pushConcludedTurn(f.shim, mainAgent, "answer-1")

	// Assert
	f.d.AwaitLogRecord(f.d.RunLogPath(), "the suppressed banner", func(r harness.LogRecord) bool {
		return r.Operation == "daemon.desktopnotify.post" && r.Message == "Emacs is focused; no desktop banner"
	})
	if got := f.d.Notifier.Invocations(); len(got) != 0 {
		t.Fatalf("a focused Emacs got %d banners, want none", len(got))
	}
}

// TestReportEditorFocusMovesTheBannerDecision pins that a report moves the
// focus the next banner is decided on.
func TestReportEditorFocusMovesTheBannerDecision(t *testing.T) {
	t.Parallel()
	// Arrange
	f := newOpened(t, harness.Opts{})
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
	f.d.AwaitLogRecord(f.d.RunLogPath(), "the suppressed banner", func(r harness.LogRecord) bool {
		return r.Operation == "daemon.desktopnotify.post" && r.Message == "Emacs is focused; no desktop banner"
	})
	if got := f.d.Notifier.Invocations(); len(got) != 0 {
		t.Fatalf("a reported-focused Emacs got %d banners, want none", len(got))
	}
}

// TestReportEditorFocusWithNoEmacsStreamIsRefused pins the refusal arm.
func TestReportEditorFocusWithNoEmacsStreamIsRefused(t *testing.T) {
	t.Parallel()
	// Arrange
	d := harness.StartDaemon(t, harness.Opts{})

	// Act
	resp, err := d.Client().ReportEditorFocus(context.Background(),
		connect.NewRequest(&agentreplv1.ReportEditorFocusRequest{Focus: harness.FocusedEditor()}))

	// Assert
	if err != nil {
		t.Fatalf("ReportEditorFocus: %v", err)
	}
	if resp.Msg.GetError().GetNoEmacsStream() == nil {
		t.Fatalf("response = %v, want the no_emacs_stream arm", resp.Msg)
	}
}

// TestAClickedBannerAsksEmacsToSelectTheWorkspace pins the click: the banner
// program reports a click, and the host stream carries notification_clicked.
func TestAClickedBannerAsksEmacsToSelectTheWorkspace(t *testing.T) {
	t.Parallel()
	// Arrange
	f := newOpened(t, harness.Opts{})
	f.d.Notifier.SetStdout(clickToken())
	f.submit("go", "k-clicked", conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT)

	// Act
	pushConcludedTurn(f.shim, mainAgent, "answer-1")

	// Assert
	harness.AwaitView(t, f.d.Ctx(), f.host, "the banner click on the host stream",
		func(r *agentreplv1.WatchHostWorkspaceResponse) bool { return r.GetNotificationClicked() != nil })
}
