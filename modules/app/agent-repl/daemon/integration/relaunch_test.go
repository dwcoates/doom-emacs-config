//go:build integration

package integration

import (
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/integration/harness"

	agentreplv1 "agentrepl/proto/agentrepl/v1"

	"connectrpc.com/connect"
)

// relaunchTurn is the ONE turn the relaunch tests run before the daemon is
// stopped: the delivered prompt and the settled answer, which are the two
// bubbles the conversation must still be drawing after the relaunch.
const (
	relaunchTurnID = "turn-relaunch"
	relaunchPrompt = "what did we decide"
	relaunchAnswer = "we decided to rehydrate"
)

// relaunchPromptEntry is the store's own record of the delivered prompt.
func relaunchPromptEntry() *conversationv1.HistoryEntry {
	return &conversationv1.HistoryEntry{
		Entry: &conversationv1.HistoryEntry_UserPrompt{UserPrompt: &conversationv1.AgentPrompt{
			Id:     &conversationv1.TurnId{Value: relaunchTurnID},
			Agent:  &conversationv1.AgentId{Value: mainAgent},
			Origin: conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT,
			Said:   said(relaunchPrompt),
		}},
	}
}

// relaunchAnswerEntry is the store's own record of the settled answer.
func relaunchAnswerEntry() *conversationv1.HistoryEntry {
	return &conversationv1.HistoryEntry{
		Entry: &conversationv1.HistoryEntry_AgentFrame{
			AgentFrame: feedResponseFrames("resp-relaunch", relaunchAnswer)[1],
		},
	}
}

// pageProse reports whether a page carries a settled answer with this prose.
func pageProse(page *frontendv1.FeedPage, markdown string) bool {
	for _, row := range page.GetSuccess().GetRows() {
		if row.GetActivity().GetResponse().GetSuccess().GetProse().GetMarkdown() == markdown {
			return true
		}
	}
	return false
}

// pagePrompt reports whether a page carries the delivered prompt of a turn.
func pagePrompt(page *frontendv1.FeedPage, turn string) bool {
	for _, row := range page.GetSuccess().GetRows() {
		if row.GetUserPrompt() != nil && row.GetTurn().GetValue() == turn {
			return true
		}
	}
	return false
}

// TestARelaunchedDaemonRehydratesAnAnnouncedWorkspacesConversation is J58's
// defect, end to end against a real daemon process.
//
// A stop stands every session down, and Emacs then re-announces every
// workspace it holds to the daemon that replaces it. For a workspace whose
// panel was ALREADY mounted that announcement is the only edge the fresh
// daemon gets — no OpenWorkspace follows it — so before the fix the second
// daemon registered the workspace, served its page and its WatchFeed, and
// answered ZERO rows for a conversation the store still held whole, under a
// footer reading `ready`.
//
// The fake shim serves a resumed session's book and an empty floor to a fresh
// one, so the rows below are proof the conversation RESUMED and not merely
// that some session came up.
func TestARelaunchedDaemonRehydratesAnAnnouncedWorkspacesConversation(t *testing.T) {
	t.Parallel()
	// Arrange: one turn run to its terminal on the first daemon.
	f := newOpened(t, harness.Opts{})
	footer := f.d.WatchFooter(f.ws)
	f.submit(relaunchPrompt, "k-relaunch", conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT)
	f.shim.PushAgentFrame(mainAgent, feedResponseFrames("resp-relaunch", relaunchAnswer)[1])
	f.shim.PushAgentFrame(mainAgent, successFrame(mainAgent, nil))
	awaitFooter(t, f, footer, "idle.done once the pre-stop turn concludes", func(v *frontendv1.FooterView) bool {
		return v.GetStrip().GetStatus().GetIdle().GetDone() != nil
	})

	// Arrange: the daemon is asked to shut down NOW, which stands every
	// session down, and a second daemon boots on the SAME state root and the
	// SAME account root — the two facts a relaunch keeps.
	expectSessionKillRecords(f.d)
	if _, err := f.d.Client().UpdateShutdownSchedule(f.d.Ctx(), connect.NewRequest(&agentreplv1.UpdateShutdownScheduleRequest{
		Action: &agentreplv1.UpdateShutdownScheduleRequest_Now{Now: &agentreplv1.UpdateShutdownScheduleNow{
			Reason: drainReasonOperator("the relaunch under test"),
		}},
	})); err != nil {
		t.Fatalf("UpdateShutdownSchedule{now} = %v, want the immediate shutdown accepted", err)
	}
	f.d.AwaitExit()

	// The store's book, as the resumed shim will serve it: newest first. IT IS
	// WRITTEN BEFORE THE SUCCESSOR STARTS, because the successor brings its
	// open workspaces' sessions up during its own boot: a profile written
	// afterwards would be read by nothing, and the resumed shim would serve
	// the empty floor a fresh one serves.
	f.d.WriteShimProfile(f.repo.Dir, harness.ShimProfile{
		ResumeHistory: harness.EncodeHistory(t, relaunchAnswerEntry(), relaunchPromptEntry()),
	})
	d2 := harness.StartDaemon(t, harness.Opts{
		StateDir:   f.d.StateDir,
		ProfileDir: f.d.ProfileDir,
		ExtraArgs:  []string{"--default-config-dir", f.d.DefaultConfigDir},
	})
	f2 := &fixture{d: d2, repo: f.repo, ws: f.ws, t: t}

	// Act: Emacs announces the workspace it still holds.
	again := harness.Register(t, d2, f.repo.Dir)
	if again.GetId() != f.ws.GetId() {
		t.Fatalf("RegisterWorkspace after the relaunch = %q, want the same workspace %q", again.GetId(), f.ws.GetId())
	}
	f2.host = d2.WatchHost(f.ws)
	f2.web = d2.WatchWeb(f.ws)

	// Assert: the conversation's two bubbles are on the page the relaunched
	// daemon serves.
	page, _ := f2.openFeedOnceCarrying("the pre-stop turn's two bubbles", func(p *frontendv1.FeedPage) bool {
		return pagePrompt(p, relaunchTurnID) && pageProse(p, relaunchAnswer)
	})
	if !pagePrompt(page, relaunchTurnID) || !pageProse(page, relaunchAnswer) {
		t.Fatalf("the relaunched daemon's feed page = %v, want the prompt and the answer of the pre-stop turn", page)
	}

	// Assert: and the footer reconciles to the terminal the turn recorded,
	// rather than reporting a conversation that has never run.
	awaitFooter(t, f2, d2.WatchFooter(f.ws), "idle.done after the relaunch", func(v *frontendv1.FooterView) bool {
		return v.GetStrip().GetStatus().GetIdle().GetDone() != nil
	})
}
