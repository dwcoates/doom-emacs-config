//go:build integration

package integration

import (
	"context"
	"os/exec"
	"runtime"
	"slices"
	"strings"
	"testing"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"agent-repld/integration/harness"
	"agent-repld/internal/desktopnotify"
)

// ==========================================================================
// Desktop banners. The daemon posts every banner itself, through the platform's
// banner program (the harness's recorder), decided on Emacs's focus: the focus
// an Emacs WatchDaemon stream connects with, moved by ReportEditorFocus, and
// forgotten when the stream ends. A banner click reaches Emacs as the host
// stream's notification_clicked.
// ==========================================================================

// bannerSettled is the daemon's record that a banner's program returned with
// no click to relay: a read-back banner dismissed or timed out, or a banner
// the platform now holds (macOS), whose click selects the workspace itself.
func bannerSettled(r harness.LogRecord) bool {
	return r.Operation == "daemon.desktopnotify.post" &&
		(r.Message == "the banner was dismissed or timed out" ||
			r.Message == "the platform holds the banner; its click selects the workspace itself")
}

// awaitBannerSettled waits for a banner to settle, and answers every banner
// argv posted so far.
func awaitBannerSettled(t *testing.T, f *fixture, what string) [][]string {
	t.Helper()
	f.d.AwaitLogRecord(f.d.RunLogPath(), what, bannerSettled)
	var argvs [][]string
	for _, inv := range bannerRuns(f) {
		argvs = append(argvs, inv.Argv)
	}
	return argvs
}

// bannerRuns is every run of the banner program that posted a banner: the
// boot's version query set aside.
func bannerRuns(f *fixture) []harness.Invocation {
	return f.d.Notifier.InvocationsExcept(harness.NotifierVersionQuery)
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
// that concluded raises "✅ <name>" over the summary the
// daemon's headless call wrote (the fake vendor answers ROUTE_HOLD to text),
// set in italics.
func TestACompletedTurnRaisesTheCompletedBanner(t *testing.T) {
	t.Parallel()
	// Arrange
	f := newOpened(t, harness.Opts{})
	f.submit("go", "k-turn-banner", conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT)

	// Act
	pushConcludedTurn(f.shim, mainAgent, "answer-1")

	// Assert
	argvs := awaitBannerSettled(t, f, "the completed-turn banner")
	if !bannerCarrying(argvs, "✅", desktopnotify.Italic("ROUTE_HOLD")) {
		t.Fatalf("banners %q, want the completed title over the model's summary", argvs)
	}
}

// TestAFailedTurnRaisesTheErroredBanner pins that a failed turn raises
// "❌ <name>" over the errored ending's line.
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
	if argvs := awaitBannerSettled(t, f, "the errored-turn banner"); !bannerCarrying(argvs, "❌") {
		t.Fatalf("banners %q, want the errored title", argvs)
	}
}

// awaitChimePlayed waits for the daemon's record that a chime finished, and
// answers every chime argv played so far.
func awaitChimePlayed(t *testing.T, f *fixture, what string) [][]string {
	t.Helper()
	f.d.AwaitLogRecord(f.d.RunLogPath(), what, func(r harness.LogRecord) bool {
		return r.Operation == "daemon.desktopnotify.chime" && r.Message == "the chime played"
	})
	var argvs [][]string
	for _, inv := range f.d.Chime.Invocations() {
		argvs = append(argvs, inv.Argv)
	}
	return argvs
}

// TestABannerRingsOneChimeAndCarriesNoSoundOfItsOwn pins that an unfocused
// Emacs hears a turn end once: its banner is made silent, and the chime rings
// as it is made.
func TestABannerRingsOneChimeAndCarriesNoSoundOfItsOwn(t *testing.T) {
	t.Parallel()
	// Arrange
	f := newOpened(t, harness.Opts{})
	f.submit("go", "k-chime-unfocused", conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT)

	// Act
	pushConcludedTurn(f.shim, mainAgent, "answer-1")

	// Assert
	if argvs := awaitChimePlayed(t, f, "the turn banner's chime"); len(argvs) != 1 {
		t.Fatalf("chimes %q, want exactly one", argvs)
	}
	argvs := awaitBannerSettled(t, f, "the silent turn banner")
	if !bannerCarrying(argvs, "✅") || slices.ContainsFunc(argvs, func(argv []string) bool {
		return slices.Contains(argv, "-sound") || slices.ContainsFunc(argv, func(arg string) bool { return strings.HasPrefix(arg, "--sound") })
	}) {
		t.Fatalf("banners %q, want the turn's banner with no sound flag", argvs)
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
	if got := bannerRuns(f); len(got) != 0 {
		t.Fatalf("a reported-focused Emacs got %d banners, want none", len(got))
	}
}

// claimLiveNotification makes the claim an Emacs that just came forward makes.
func claimLiveNotification(t *testing.T, d *harness.Daemon) *agentreplv1.ClaimLiveNotificationResponse {
	t.Helper()
	resp, err := d.Client().ClaimLiveNotification(context.Background(),
		connect.NewRequest(&agentreplv1.ClaimLiveNotificationRequest{}))
	if err != nil {
		t.Fatalf("ClaimLiveNotification: %v", err)
	}
	return resp.Msg
}

// TestAFocusGainClaimsTheBannerThePlatformStillHolds pins the claim on macOS:
// a banner handed to the platform while Emacs was away, and still listed by
// it, names its workspace to the Emacs that came forward, once.
func TestAFocusGainClaimsTheBannerThePlatformStillHolds(t *testing.T) {
	if runtime.GOOS != "darwin" {
		t.Skip("only a macOS banner is handed to the platform and listed by it")
	}
	t.Parallel()
	// Arrange
	f := newOpened(t, harness.Opts{})
	f.d.WatchEmacsDaemonStream(harness.UnfocusedEditor())
	f.submit("go", "k-claimed", conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT)
	pushConcludedTurn(f.shim, mainAgent, "answer-1")
	awaitBannerSettled(t, f, "the handed turn banner")
	f.d.Notifier.SetStdout("GroupID\tTitle\tSubtitle\tMessage\tDelivered At\n" +
		"agent-repl:" + f.ws.GetId() + "\tT\t\tB\t2026-10-15 16:16:08 +0000\n")
	resp, err := f.d.Client().ReportEditorFocus(context.Background(),
		connect.NewRequest(&agentreplv1.ReportEditorFocusRequest{Focus: harness.FocusedEditor()}))
	if err != nil || resp.Msg.GetSuccess() == nil {
		t.Fatalf("ReportEditorFocus = (%v, %v), want success", resp, err)
	}

	// Act
	first := claimLiveNotification(t, f.d)
	second := claimLiveNotification(t, f.d)

	// Assert
	got := first.GetSuccess().GetLive().GetWorkspace()
	if got.GetId() != f.ws.GetId() || got.GetDir() != f.ws.GetDir() {
		t.Fatalf("first claim = %v, want live naming %v", first, f.ws)
	}
	if second.GetSuccess().GetNone() == nil {
		t.Fatalf("second claim = %v, want none: the first consumed the banner", second)
	}
	if !slices.ContainsFunc(bannerRuns(f), func(inv harness.Invocation) bool {
		return slices.Equal(inv.Argv, []string{"-list", "ALL"})
	}) {
		t.Fatalf("banner program runs %v, want the platform asked -list ALL", bannerRuns(f))
	}
}

// TestAClaimWhileEmacsIsUnfocusedIsRefused pins the refusal arm: no stream
// stands, so Emacs reads as unfocused.
func TestAClaimWhileEmacsIsUnfocusedIsRefused(t *testing.T) {
	t.Parallel()
	// Arrange
	d := harness.StartDaemon(t, harness.Opts{})

	// Act
	resp := claimLiveNotification(t, d)

	// Assert
	if resp.GetError().GetEditorUnfocused() == nil {
		t.Fatalf("response = %v, want the editor_unfocused arm", resp)
	}
}

// TestAClickedBannerAsksEmacsToSelectTheWorkspace pins a READ-BACK click
// (Linux): the banner program reports a click, and the host stream carries
// notification_clicked.
func TestAClickedBannerAsksEmacsToSelectTheWorkspace(t *testing.T) {
	if runtime.GOOS == "darwin" {
		t.Skip("a macOS banner is handed to the platform; TestAHandedBannersClickSelectsTheWorkspace pins its click")
	}
	t.Parallel()
	// Arrange
	f := newOpened(t, harness.Opts{})
	f.d.Notifier.SetStdout("default")
	f.submit("go", "k-clicked", conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT)

	// Act
	pushConcludedTurn(f.shim, mainAgent, "answer-1")

	// Assert
	harness.AwaitView(t, f.d.Ctx(), f.host, "the banner click on the host stream",
		func(r *agentreplv1.WatchHostWorkspaceResponse) bool { return r.GetNotificationClicked() != nil })
}

// TestAHandedBannersClickSelectsTheWorkspace pins a HANDED click (macOS): the
// banner brings Emacs forward and stores the daemon's own SelectWorkspace
// call, and running that stored command -- what the platform does on the
// click -- makes the workspace the roster's current.
func TestAHandedBannersClickSelectsTheWorkspace(t *testing.T) {
	if runtime.GOOS != "darwin" {
		t.Skip("only a macOS banner is handed to the platform")
	}
	t.Parallel()
	// Arrange
	f := newOpened(t, harness.Opts{})
	roster := f.d.WatchRoster()
	f.submit("go", "k-handed", conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT)
	pushConcludedTurn(f.shim, mainAgent, "answer-1")
	argvs := awaitBannerSettled(t, f, "the handed turn banner")
	if len(argvs) != 1 {
		t.Fatalf("banners %q, want exactly one", argvs)
	}
	argv := argvs[0]
	activate := slices.Index(argv, "-activate")
	execute := slices.Index(argv, "-execute")
	if activate < 0 || activate+1 >= len(argv) || argv[activate+1] != "org.gnu.Emacs" {
		t.Fatalf("banner %q, want -activate org.gnu.Emacs", argv)
	}
	if execute < 0 || execute+1 >= len(argv) || !strings.HasPrefix(argv[execute+1], `\`) {
		t.Fatalf("banner %q, want a backslash-escaped -execute command", argv)
	}
	click := strings.TrimPrefix(argv[execute+1], `\`)

	// Act
	if out, err := exec.CommandContext(f.d.Ctx(), "/bin/sh", "-c", click).CombinedOutput(); err != nil {
		t.Fatalf("the stored click %q failed: %v: %s", click, err, out)
	}

	// Assert
	awaitRoster(t, f.d, roster, "the click's SelectWorkspace stamped current", func(r *frontendv1.WorkspaceRoster) bool {
		return r.GetCurrent().GetWorkspace().GetId() == f.ws.GetId()
	})
}
