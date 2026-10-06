//go:build integration

package integration

import (
	"testing"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/integration/harness"
)

// FEED RESTORATION AFTER A RESTART (owner principle, 2026-10-02: the vendor
// and its state never gate agent-repl's own functions). A restarted daemon
// serves a workspace's conversation from the store through its shim, whatever
// the vendor session is doing: parked at its cold gate, being retried, or
// refused. Only a workspace with no shim at all serves what it holds, and the
// newest page reaches its reader the moment a shim comes up.

// restartedWith restarts a freshly opened workspace (restarted) onto a book
// holding the relaunch fixture's turn: a prompt and its settled answer.
func restartedWith(t *testing.T, profile harness.ShimProfile) *fixture {
	t.Helper()
	profile.ResumeHistory = harness.EncodeHistory(t, relaunchAnswerEntry(), relaunchPromptEntry())
	return restarted(newOpened(t, harness.Opts{}), profile)
}

// restarted stops a fixture's daemon, writes the shim profile its successor's
// bring-up spawns the shim under — the book it resumes is the profile's own
// ResumeHistory — and boots the successor on the same state and account
// roots, answering the successor's fixture with the workspace announced and
// both client hops held. A fixture restarted is restartable again.
func restarted(f *fixture, profile harness.ShimProfile) *fixture {
	t := f.t
	t.Helper()
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
	f.d.WriteShimProfile(f.repo.Dir, profile)
	d2 := harness.StartDaemon(t, harness.Opts{
		StateDir:   f.d.StateDir,
		ProfileDir: f.d.ProfileDir,
		ExtraArgs:  []string{"--default-config-dir", f.d.DefaultConfigDir},
	})
	// THE SUCCESSOR RUNS ON ITS PREDECESSOR'S ACCOUNT ROOT — the flag above
	// overrides the one the harness minted for it — so its fixture names that
	// root, and a further restart hands the same one on. Naming the minted one
	// sent the next successor to a root holding no transcript, and it came up
	// on a FRESH conversation instead of resuming this one.
	d2.DefaultConfigDir = f.d.DefaultConfigDir
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
	// Arrange: the vendor start is rejected; its shim stays held with no
	// session, and the reader opens once the failure is recorded.
	f := restartedWith(t, harness.ShimProfile{VendorStartFailed: "invalid api key"})
	// The sweep covers every test; the declared records are evidence of the rejection the test scripts.
	f.d.ExpectWarnings("daemon.workspace.bring_up", "daemon.boot.bring_up")
	f.d.AwaitWorkspaceLogRecord(f.ws.GetDir(), "the failed start's shim held with no session", func(r harness.LogRecord) bool {
		return r.PID == f.d.PID() && r.Message == "the failed start's shim stays held with no session; the next start reuses it"
	})

	// Act.
	page, _ := f.openFeed(nil)

	// Assert: the conversation is read through the held shim.
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

// THE SHAPE THE OWNER'S WORKSPACE REPLAYED ON EVERY BOOT (2026-10-02): an
// ADOPTED turn — the vendor started it on its own (a stopped background task's
// notification, answered with no reply) — whose terminal names an answer no
// plane stored, because the shim's fold had kept the keep-alive's "." from the
// turn before and named it again.
const (
	adoptedReplayTurn = "adopted-d8d296ab-6fde-45f2-ae82-e0c896b0aaee"
	unstoredAnswer    = "msg_011CfcgDhgcB7pSBohLKm2Yp:0"
)

// adoptedTurnBook is the store's book for that turn, newest first: its
// terminal naming the unstored answer, then its VENDOR_STARTED prompt with no
// words said.
func adoptedTurnBook(t *testing.T) [][]byte {
	t.Helper()
	return harness.EncodeHistory(t,
		&conversationv1.HistoryEntry{Entry: &conversationv1.HistoryEntry_AgentFrame{
			AgentFrame: successFrame(mainAgent, activityID(unstoredAnswer)),
		}},
		&conversationv1.HistoryEntry{Entry: &conversationv1.HistoryEntry_UserPrompt{UserPrompt: &conversationv1.AgentPrompt{
			Id:     &conversationv1.TurnId{Value: adoptedReplayTurn},
			Agent:  &conversationv1.AgentId{Value: mainAgent},
			Origin: conversationv1.PromptOrigin_PROMPT_ORIGIN_VENDOR_STARTED,
			Said:   &conversationv1.UserSaid{Content: &conversationv1.UserContent{}},
		}}},
	)
}

// carriesAdoptedTerminal reports whether a page carries the adopted turn's
// terminal row: the page that replayed it is the one that judged its verdict.
func carriesAdoptedTerminal(p *frontendv1.FeedPage) bool {
	return rowIndex(p, func(r *frontendv1.FeedRow) bool {
		return r.GetTurnEnded() != nil && r.GetTurn().GetValue() == adoptedReplayTurn
	}) >= 0
}

// adoptedVerdict matches one of this daemon's records about the adopted turn's
// verdict under OPERATION.
func adoptedVerdict(operation string) func(harness.LogRecord) bool {
	return func(r harness.LogRecord) bool {
		return r.Operation == operation && r.Context["turn"] == adoptedReplayTurn && r.Context["unit"] == unstoredAnswer
	}
}

func TestAnAdoptedTurnsUnresolvedAnswerIsRaisedOnceAcrossRestarts(t *testing.T) {
	t.Parallel()
	// Arrange: the first daemon to replay the book — its reader opening the
	// newest page — judges the turn and raises its verdict.
	profile := harness.ShimProfile{ResumeHistory: adoptedTurnBook(t)}
	first := restarted(newOpened(t, harness.Opts{}), profile)
	first.d.ExpectWarnings("daemon.feed.final_answer_unresolved")
	first.openFeedOnceCarrying("the adopted turn's terminal", carriesAdoptedTerminal)
	first.d.AwaitWorkspaceLogRecordInState("the first replay raising the adopted turn's verdict",
		adoptedVerdict("daemon.feed.final_answer_unresolved"))

	// Act: the next daemon's reader replays the same book.
	second := restarted(first, profile)
	second.openFeedOnceCarrying("the adopted turn's terminal", carriesAdoptedTerminal)
	// Assert: the verdict is found in the record, and raised nowhere again —
	// the cleanup sweep fails the test on any undeclared ERROR, and this one
	// is not declared on the second daemon.
	second.d.AwaitWorkspaceLogRecordInState("the second replay finding the verdict recorded",
		adoptedVerdict("daemon.feed.final_answer_verdict_recorded"))
	for _, r := range second.d.WorkspaceLogRecords() {
		if adoptedVerdict("daemon.feed.final_answer_unresolved")(r) {
			t.Fatalf("the second daemon raised the recorded verdict again: %s", r.Raw)
		}
	}
}

// THE WHOLE TRANSCRIPT IS READABLE BEHIND A STANDING GATE (owner, 2026-10-03):
// the gate's choice — pay, compact or clear — is about the conversation, so the
// reader must be able to walk every page of it before answering.
func TestAColdGatedWorkspacesWholeConversationIsWalkableBeforeTheGateIsAnswered(t *testing.T) {
	t.Parallel()
	// Arrange: a book of several store pages, resumed into a cold gate.
	entries := make([]*conversationv1.HistoryEntry, 0, pagingRows+1)
	for i := pagingRows - 1; i >= 0; i-- {
		entries = append(entries, &conversationv1.HistoryEntry{
			Entry: &conversationv1.HistoryEntry_AgentFrame{AgentFrame: feedRowLabeledResponse(i)},
		})
	}
	entries = append(entries, relaunchPromptEntry())
	f := restarted(newOpened(t, harness.Opts{}), harness.ShimProfile{
		HistoryPageSize: pagingStorePage,
		ResumeHistory:   harness.EncodeHistory(t, entries...),
		ColdOnResume: &harness.ShimColdFacts{
			ContextTokens: 123456, LastRequestAtMS: 1_700_000_000_000, RequestedModel: "sonnet", CacheTTLMS: 300_000,
		},
	})
	f.d.AwaitWorkspaceLogRecord(f.ws.GetDir(), "the session parked at its cold gate", func(r harness.LogRecord) bool {
		return r.PID == f.d.PID() && r.Message == "the session is parked at its cold gate"
	})
	f.openFeed(nil)

	// Act: the newest page, then next until the conversation's start.
	pages := []*frontendv1.FeedPage{f.feedPage(true)}
	for pages[len(pages)-1].GetSuccess().GetHasMore() != nil && len(pages) <= pagingRows+1 {
		pages = append(pages, f.feedPage(false))
	}

	// Assert: every response and the opening prompt were reached.
	responses, prompt := 0, false
	for _, page := range pages {
		for _, row := range page.GetSuccess().GetRows() {
			if row.GetActivity().GetResponse() != nil {
				responses++
			}
		}
		prompt = prompt || pagePrompt(page, relaunchTurnID)
	}
	if len(pages) < 3 || responses != pagingRows || !prompt {
		t.Fatalf("walked %d pages: %d of %d responses, prompt reached = %v; want the whole conversation behind the gate",
			len(pages), responses, pagingRows, prompt)
	}
}

// A NEWEST PAGE THAT DRAWS NO ROW NEVER LEAVES THE FEED EMPTY (owner's report,
// 2026-10-06: after a restart two workspaces' feeds stayed empty). The reader
// opens before any shim is up, so the newest page is loaded for it by the
// kick when one comes up, and a page that draws nothing must read on to the
// page that does.

// emptyNewestPageStore is a store page of two, so the newest pages of the
// books below draw nothing.
const emptyNewestPageStore = 2

// olderTurnPrompt is a prompt older than the relaunch turn, so the page that
// draws the relaunch turn is not the conversation's start.
func olderTurnPrompt() *conversationv1.HistoryEntry {
	return &conversationv1.HistoryEntry{Entry: &conversationv1.HistoryEntry_UserPrompt{UserPrompt: &conversationv1.AgentPrompt{
		Id:     &conversationv1.TurnId{Value: "turn-older"},
		Agent:  &conversationv1.AgentId{Value: mainAgent},
		Origin: conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT,
		Said:   said("an older question"),
	}}}
}

// succeededHook is a settled, succeeded SessionStart hook: the entry every
// restart writes, which draws no row.
func succeededHook(id string) *conversationv1.HistoryEntry {
	return &conversationv1.HistoryEntry{Entry: &conversationv1.HistoryEntry_AgentFrame{AgentFrame: activityFrame(mainAgent, &conversationv1.AgentActivity{
		ActivityId: activityID(id),
		Item: &conversationv1.AgentActivity_Hook{Hook: &conversationv1.AgentHook{
			Result: &conversationv1.AgentHook_Succeeded{Succeeded: &conversationv1.AgentHookSucceeded{Command: "session-start"}},
		}},
	})}}
}

func TestARestartWhoseNewestPageDrawsNoRowPushesTheConversation(t *testing.T) {
	t.Parallel()
	tests := []struct {
		name  string
		book  []*conversationv1.HistoryEntry
		turns []string
	}{
		{
			// agent-repl-streaming: the last turn is longer than the newest
			// page, every entry of which waits on the turn's prompt.
			name: "a last turn longer than a page",
			book: []*conversationv1.HistoryEntry{
				relaunchAnswerEntry(), relaunchAnswerEntry(), relaunchAnswerEntry(),
				relaunchPromptEntry(), olderTurnPrompt(),
			},
			turns: []string{relaunchTurnID, relaunchTurnID, relaunchTurnID},
		},
		{
			// definitions: the newest page is the restarts' succeeded
			// SessionStart hooks.
			name: "a newest page of succeeded hooks",
			book: []*conversationv1.HistoryEntry{
				succeededHook("hook-3"), succeededHook("hook-2"), succeededHook("hook-1"),
				relaunchAnswerEntry(), relaunchPromptEntry(), olderTurnPrompt(),
			},
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			t.Parallel()
			// Arrange: the reader opens while no shim is up.
			f := restarted(newOpened(t, harness.Opts{}), harness.ShimProfile{
				DelayDiagnostics: true,
				HistoryPageSize:  emptyNewestPageStore,
				ResumeHistory:    harness.EncodeHistory(t, tt.book...),
				ResumeTurns:      tt.turns,
			})
			tail := f.watchRootFeed()

			// Act.
			f.d.Shim(f.ws).PushHealthyWhenSubscribed()

			// Assert: the conversation reaches the reader.
			awaitRow(t, f, tail, "the relaunch turn's answer pushed to the waiting reader", func(r *frontendv1.FeedRow) bool {
				return r.GetActivity().GetResponse().GetSuccess().GetProse().GetMarkdown() == relaunchAnswer
			})
		})
	}
}
