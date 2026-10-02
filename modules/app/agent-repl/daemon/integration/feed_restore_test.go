//go:build integration

package integration

import (
	"testing"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/integration/harness"
)

// FEED RESTORATION AFTER A RESTART (owner principle, 2026-10-02: the vendor
// and its state never gate agent-repl's own functions). A restarted daemon
// serves a workspace's conversation from the store through its shim, whatever
// the vendor session is doing: parked at its cold gate, being retried, or
// refused. Only a workspace with no shim at all serves what it holds, and the
// newest page reaches its reader the moment a shim comes up.

// restartedWith stops an opened workspace's daemon, writes the shim profile the
// successor's bring-up spawns its shim under (the store's book is the
// relaunch fixture's turn: a prompt and its settled answer), and boots the
// successor on the same state and account roots. It answers the successor's
// fixture with the workspace announced and both client hops held.
func restartedWith(t *testing.T, profile harness.ShimProfile) *fixture {
	t.Helper()
	f := newOpened(t, harness.Opts{})
	expectSessionKillRecords(f.d)
	if _, err := f.d.Client().UpdateShutdownSchedule(f.d.Ctx(), connect.NewRequest(&agentreplv1.UpdateShutdownScheduleRequest{
		Action: &agentreplv1.UpdateShutdownScheduleRequest_Now{Now: &agentreplv1.UpdateShutdownScheduleNow{
			Reason: drainReasonOperator("the restart under test"),
		}},
	})); err != nil {
		t.Fatalf("UpdateShutdownSchedule{now} = %v, want the immediate shutdown accepted", err)
	}
	f.d.AwaitExit()

	// WRITTEN BEFORE THE SUCCESSOR STARTS: its boot spawns the shim that reads it.
	profile.ResumeHistory = harness.EncodeHistory(t, relaunchAnswerEntry(), relaunchPromptEntry())
	f.d.WriteShimProfile(f.repo.Dir, profile)
	d2 := harness.StartDaemon(t, harness.Opts{
		StateDir:   f.d.StateDir,
		ProfileDir: f.d.ProfileDir,
		ExtraArgs:  []string{"--default-config-dir", f.d.DefaultConfigDir},
	})
	f2 := &fixture{d: d2, repo: f.repo, ws: f.ws, t: t}
	again := harness.Register(t, d2, f.repo.Dir)
	if again.GetId() != f.ws.GetId() {
		t.Fatalf("RegisterWorkspace after the restart = %q, want the same workspace %q", again.GetId(), f.ws.GetId())
	}
	f2.host = d2.WatchHost(f.ws)
	f2.web = d2.WatchWeb(f.ws)
	return f2
}

// pageCarriesTheConversation reports whether a page carries the store's turn:
// its prompt and its settled answer.
func pageCarriesTheConversation(p *frontendv1.FeedPage) bool {
	return pagePrompt(p, relaunchTurnID) && pageProse(p, relaunchAnswer)
}

// rowIndex answers the position of the first row of a page satisfying PRED, -1
// when none does.
func rowIndex(p *frontendv1.FeedPage, pred func(*frontendv1.FeedRow) bool) int {
	for i, row := range p.GetSuccess().GetRows() {
		if pred(row) {
			return i
		}
	}
	return -1
}

func TestARestartedColdGatedWorkspaceOpensWithItsConversation(t *testing.T) {
	t.Parallel()
	// Arrange.
	f := restartedWith(t, harness.ShimProfile{ColdOnResume: &harness.ShimColdFacts{
		ContextTokens: 123456, LastRequestAtMS: 1_700_000_000_000, RequestedModel: "sonnet", CacheTTLMS: 300_000,
	}})
	f.d.AwaitWorkspaceLogRecord(f.ws.GetDir(), "the session parked at its cold gate", func(r harness.LogRecord) bool {
		return r.PID == f.d.PID() && r.Message == "the session is parked at its cold gate"
	})

	// Act.
	page, _ := f.openFeed(nil)

	// Assert.
	if !pageCarriesTheConversation(page) {
		t.Fatalf("the cold-gated workspace's newest page = %v, want the store's prompt and answer", page)
	}
}

func TestARestartedColdGatedWorkspacesGateStandsBelowItsConversation(t *testing.T) {
	t.Parallel()
	// Arrange.
	f := restartedWith(t, harness.ShimProfile{ColdOnResume: &harness.ShimColdFacts{
		ContextTokens: 123456, LastRequestAtMS: 1_700_000_000_000, RequestedModel: "sonnet", CacheTTLMS: 300_000,
	}})
	isGate := func(r *frontendv1.FeedRow) bool { return r.GetColdGate().GetStanding() != nil }

	// Act.
	page, _ := f.openFeedOnceCarrying("the conversation and the cold gate", func(p *frontendv1.FeedPage) bool {
		return pageCarriesTheConversation(p) && rowIndex(p, isGate) >= 0
	})

	// Assert: the gate, made after every entry of the conversation, sorts below it.
	answer := rowIndex(page, func(r *frontendv1.FeedRow) bool {
		return r.GetActivity().GetResponse().GetSuccess().GetProse().GetMarkdown() == relaunchAnswer
	})
	if gate := rowIndex(page, isGate); gate < answer {
		t.Fatalf("the cold gate stands at row %d above the answer at row %d, want it below the conversation it postdates", gate, answer)
	}
}

func TestARestartedWorkspaceWhoseVendorStartIsRetriedOpensWithItsConversation(t *testing.T) {
	t.Parallel()
	// Arrange: every StartSession is refused retryable, so the run is still
	// retrying when the reader opens.
	f := restartedWith(t, harness.ShimProfile{VendorStartFailed: "the vendor is overloaded", VendorStartRetryable: true})
	f.d.AwaitWorkspaceLogRecord(f.ws.GetDir(), "a retryable vendor-start refusal", func(r harness.LogRecord) bool {
		return r.PID == f.d.PID() && r.Message == "the vendor did not start; retrying on the backoff"
	})

	// Act.
	page, _ := f.openFeed(nil)

	// Assert.
	if !pageCarriesTheConversation(page) {
		t.Fatalf("the retrying workspace's newest page = %v, want the store's prompt and answer", page)
	}
}

func TestARestartedWorkspaceWhoseVendorStartFailedOpensWithItsConversation(t *testing.T) {
	t.Parallel()
	// Arrange: the vendor start is rejected, so its shim is stopped; the reader
	// opens only after the failed start has secured the newest page.
	f := restartedWith(t, harness.ShimProfile{VendorStartFailed: "invalid api key"})
	// The sweep covers every test; the declared records are evidence of the rejection the test scripts.
	f.d.ExpectWarnings("daemon.workspace.bring_up", "daemon.boot.bring_up")
	f.d.AwaitWorkspaceLogRecord(f.ws.GetDir(), "the newest page secured before the failed start's shim stopped", func(r harness.LogRecord) bool {
		return r.PID == f.d.PID() && r.Operation == "daemon.feed.newest_page_kept"
	})

	// Act.
	page, _ := f.openFeed(nil)

	// Assert.
	if !pageCarriesTheConversation(page) {
		t.Fatalf("the failed workspace's newest page = %v, want the store's prompt and answer", page)
	}
}

func TestARestartedWorkspaceWithNoShimServesWhatItHolds(t *testing.T) {
	t.Parallel()
	// Arrange: the successor's shim withholds its first diagnostics, so the
	// bring-up holds no client yet.
	f := restartedWith(t, harness.ShimProfile{DelayDiagnostics: true})

	// Act.
	page, _ := f.openFeed(nil)

	// Assert.
	if pageCarriesTheConversation(page) {
		t.Fatalf("the page = %v, want no history read with no shim up", page)
	}
}

func TestARestartedWorkspacesNewestPageReachesItsReaderWhenTheShimComesUp(t *testing.T) {
	t.Parallel()
	// Arrange: the reader opens while no shim is up.
	f := restartedWith(t, harness.ShimProfile{DelayDiagnostics: true})
	tail := f.watchRootFeed()

	// Act.
	f.d.Shim(f.ws).PushHealthyWhenSubscribed()

	// Assert.
	awaitRow(t, f, tail, "the store's answer pushed to the waiting reader", func(r *frontendv1.FeedRow) bool {
		return r.GetActivity().GetResponse().GetSuccess().GetProse().GetMarkdown() == relaunchAnswer
	})
}
