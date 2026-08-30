package sessionwatcher

import (
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"

	"claude-repld/internal/ids"
	"claude-repld/internal/wsm"
)

// TestRouteActivityGoesToFeedAndFooterOnly covers the ordinary activity: the
// feed draws the row and the footer advances its status tree, and the topbar
// must NOT see it — the topbar sees an activity for one reason only.
func TestRouteActivityGoesToFeedAndFooterOnly(t *testing.T) {
	// Arrange.
	h := newHarness(t, Session{Started: sessionStarted("")})
	h.quiet()

	// Act.
	got := h.route(h.main, entryFrame(frameUpdate("main-1", activityUpdate(readActivity("act-1")))))

	// Assert.
	assertNames(t, got, []string{"feed.OnActivity", "footer.OnActivity"})
}

// TestRouteUnmodeledActivityAlsoWarnsTheTopbar covers the one reason the
// topbar sees an activity: an activity the schema does not model is a warning
// it shows.
func TestRouteUnmodeledActivityAlsoWarnsTheTopbar(t *testing.T) {
	// Arrange.
	h := newHarness(t, Session{Started: sessionStarted("")})
	h.quiet()

	// Act.
	got := h.route(h.main, entryFrame(frameUpdate("main-1", activityUpdate(unmodeledActivity("act-1", "mcp__thing__do")))))

	// Assert.
	assertNames(t, got, []string{"feed.OnActivity", "footer.OnActivity", "topbar.OnActivity"})
	if !h.hasRecord("warn", "daemon.sessionwatcher.unmodeled_activity") {
		t.Fatal("an unmodeled activity was not warned about")
	}
}

// TestRouteContextInjectedActivity covers a FILE-PLANE-ONLY fact: injected
// context reaches the daemon only through an agent watch's replay or follow,
// never on the session stream, and it must route as any other activity does
// when it arrives there.
func TestRouteContextInjectedActivity(t *testing.T) {
	// Arrange.
	h := newHarness(t, Session{Started: sessionStarted("")})
	h.quiet()

	// Act.
	got := h.route(h.main, entryFrame(frameUpdate("main-1", activityUpdate(contextInjectedActivity("act-1")))))

	// Assert.
	assertNames(t, got, []string{"feed.OnActivity", "footer.OnActivity"})
}

// TestRouteQuestion covers a blocked question: the feed draws it, the footer
// moves to waiting, and the host is notified so the roster's attention marker
// rises. The sidebar has no question method and must not be reached.
func TestRouteQuestion(t *testing.T) {
	// Arrange.
	h := newHarness(t, Session{Started: sessionStarted("")})
	h.quiet()

	// Act.
	got := h.route(h.main, entryFrame(frameUpdate("main-1", questionUpdate("q-1", "Pick a branch", "Which branch should I cut from?"))))

	// Assert.
	assertNames(t, got, []string{"feed.OnQuestion", "footer.OnQuestion", "lifecycle.OnNotification"})
	note := requireEvent(t, got, "lifecycle.OnNotification").note
	if note.Kind != NotificationQuestionAsked {
		t.Fatalf("notification kind = %q, want %q", note.Kind, NotificationQuestionAsked)
	}
	if note.Header != "Pick a branch" {
		t.Fatalf("notification header = %q, want the first question's chip label", note.Header)
	}
}

// TestRouteQuestionFallsBackToTheQuestionText covers a batch whose first
// question carries no chip label: the notification still has to say something,
// and the question's own text is the only other thing that describes it.
func TestRouteQuestionFallsBackToTheQuestionText(t *testing.T) {
	// Arrange.
	h := newHarness(t, Session{Started: sessionStarted("")})
	h.quiet()

	// Act.
	got := h.route(h.main, entryFrame(frameUpdate("main-1", questionUpdate("q-1", "", "Which branch should I cut from?"))))

	// Assert.
	note := requireEvent(t, got, "lifecycle.OnNotification").note
	if note.Text != "Which branch should I cut from?" {
		t.Fatalf("notification text = %q, want the question's own text", note.Text)
	}
}

// TestRoutePermission covers a blocked permission: it reaches the feed, the
// footer and the roster row, and raises the host notification that names the
// gated tool.
func TestRoutePermission(t *testing.T) {
	// Arrange.
	h := newHarness(t, Session{Started: sessionStarted("")})
	h.quiet()

	// Act.
	got := h.route(h.main, entryFrame(frameUpdate("main-1", permissionUpdate("p-1", "act-1", "Claude wants to read foo.txt", "Read file"))))

	// Assert.
	assertNames(t, got, []string{"feed.OnPermission", "footer.OnPermission", "sidebar.OnPermission", "lifecycle.OnNotification"})
	note := requireEvent(t, got, "lifecycle.OnNotification").note
	if note.Kind != NotificationPermissionRequested {
		t.Fatalf("notification kind = %q, want %q", note.Kind, NotificationPermissionRequested)
	}
}

// TestPermissionToolNameComesFromTheGatedCall covers the tool name's best
// source: the permission names an ACTIVITY it gates, and that unit's own
// recorded tool is the real name — the vendor's display name is a phrase.
func TestPermissionToolNameComesFromTheGatedCall(t *testing.T) {
	// Arrange: the gated call streams first, so its tool name is known.
	h := newHarness(t, Session{Started: sessionStarted("")})
	h.quiet()
	h.route(h.main, entryFrame(frameUpdate("main-1", activityUpdate(readActivity("act-1")))))

	// Act.
	got := h.route(h.main, entryFrame(frameUpdate("main-1", permissionUpdate("p-1", "act-1", "Claude wants to read foo.txt", "Read file"))))

	// Assert.
	note := requireEvent(t, got, "lifecycle.OnNotification").note
	if note.ToolName != "Read" {
		t.Fatalf("tool name = %q, want the gated call's own tool", note.ToolName)
	}
}

// TestPermissionToolNameFallsBackToTheDisplayName covers a gate on a call this
// watcher never saw: the vendor's short phrase is the last resort, and naming
// nothing would leave the notification unable to say what is being asked.
func TestPermissionToolNameFallsBackToTheDisplayName(t *testing.T) {
	// Arrange.
	h := newHarness(t, Session{Started: sessionStarted("")})
	h.quiet()

	// Act.
	got := h.route(h.main, entryFrame(frameUpdate("main-1", permissionUpdate("p-1", "never-seen", "Claude wants to read foo.txt", "Read file"))))

	// Assert.
	note := requireEvent(t, got, "lifecycle.OnNotification").note
	if note.ToolName != "Read file" {
		t.Fatalf("tool name = %q, want the vendor's display name", note.ToolName)
	}
}

// TestRouteContextCut covers the cut: the feed draws the separation divider
// and the footer needs the same record, because it is the END signal for the
// compacting state SessionUpdate.compacting opened.
func TestRouteContextCut(t *testing.T) {
	// Arrange.
	h := newHarness(t, Session{Started: sessionStarted("")})
	h.quiet()

	// Act.
	got := h.routeNow(func(w *watcher) {
		w.routeUpdateLocked(agentID("main-1"), &conversationv1.AgentUpdate{
			Update: &conversationv1.AgentUpdate_ContextCut{ContextCut: &conversationv1.ContextCut{}},
		})
	})

	// Assert.
	assertNames(t, got, []string{"feed.OnContextCut", "footer.OnContextCut"})
}

// TestRouteContextBudgetWarning covers the arm's plane: the vendor's
// context-budget warning is a transcript attachment on the AGENT plane, and
// the footer's activity line is its only consumer.
func TestRouteContextBudgetWarning(t *testing.T) {
	// Arrange.
	h := newHarness(t, Session{Started: sessionStarted("")})
	h.quiet()

	// Act.
	got := h.routeNow(func(w *watcher) { w.routeUpdateLocked(agentID("main-1"), budgetWarningFrame()) })

	// Assert.
	assertNames(t, got, []string{"footer.OnContextBudgetWarning"})
}

// TestRouteApiError covers mid-turn evidence: it is a page line and a footer
// retry notice, and never a terminal — the turn goes on.
func TestRouteApiError(t *testing.T) {
	// Arrange.
	h := newHarness(t, Session{Started: sessionStarted("turn-1")})
	h.quiet()

	// Act.
	got := h.route(h.main, entryFrame(frameUpdate("main-1", apiErrorUpdate("529 overloaded"))))

	// Assert.
	assertNames(t, got, []string{"feed.OnApiError", "footer.OnApiError"})
	if h.w.TurnInFlight() == nil {
		t.Fatal("a mid-turn api error ended the turn; it is evidence, never a terminal")
	}
}

// TestRouteHistoryPage covers a watch's opening frame: the page goes to the
// feed whole, and its entries are NOT replayed as live frames.
func TestRouteHistoryPage(t *testing.T) {
	// Arrange.
	h := newHarness(t, Session{Started: sessionStarted("")})
	h.quiet()

	// Act.
	got := h.route(h.main, pageFrame(promptEntry("ptr-9", "turn-old", "main-1")))

	// Assert.
	assertNames(t, got, []string{"feed.OnHistoryPage"})
	if h.w.TurnInFlight() != nil {
		t.Fatal("a page's prompt opened a turn; a page is newest-first and its turns are already over")
	}
}

// TestRouteLivePromptOpensTheTurn covers the main watch's live prompt: it
// carries the daemon's minted TurnId and names its recipient, which is how a
// turn the watcher did not open through the queue becomes the tracked one.
func TestRouteLivePromptOpensTheTurn(t *testing.T) {
	// Arrange.
	h := newHarness(t, Session{Started: sessionStarted("")})
	h.quiet()

	// Act.
	got := h.route(h.main, entryPrompt("turn-7", "main-1"))

	// Assert.
	assertNames(t, got, []string{"feed.OnPrompt"})
	turn := h.w.TurnInFlight()
	if turn == nil || *turn != ids.TurnID("turn-7") {
		t.Fatalf("turn in flight = %v, want turn-7", turn)
	}
}

// TestRouteMainTerminalEndsTheTurn covers the turn's close: the three views
// see the terminal, and the lifecycle edge the prompt queue drains on carries
// the turn the watcher was tracking.
func TestRouteMainTerminalEndsTheTurn(t *testing.T) {
	// Arrange.
	h := newHarness(t, Session{Started: sessionStarted("turn-1")})
	h.w.SetMainAgent(agentID("main-1"))
	h.quiet()

	// Act.
	got := h.route(h.main, entryFrame(frameSuccess("main-1", completed())))

	// Assert.
	assertNames(t, got, []string{
		"feed.OnAgentTerminal", "footer.OnAgentTerminal", "sidebar.OnAgentTerminal", "lifecycle.OnTurnEnded",
	})
	ended := requireEvent(t, got, "lifecycle.OnTurnEnded")
	if ended.turn == nil || *ended.turn != ids.TurnID("turn-1") {
		t.Fatalf("turn ended = %v, want turn-1", ended.turn)
	}
	if ended.close != wsm.CloseCompleted {
		t.Fatalf("close = %v, want CloseCompleted", ended.close)
	}
	if h.w.TurnInFlight() != nil {
		t.Fatal("the turn is still in flight after its terminal")
	}
}

// TestTurnEndIsWithheldUntilTheMainAgentIsNamed covers the one thing the
// watcher refuses to guess: with no main agent named, a terminal cannot be
// attributed to the turn, and draining the prompt queue on a subagent's
// terminal is worse than waiting.
func TestTurnEndIsWithheldUntilTheMainAgentIsNamed(t *testing.T) {
	// Arrange: an adoption with a turn already in flight and no StartTurn yet.
	h := newHarness(t, Session{Started: sessionStarted("turn-1")})
	h.quiet()

	// Act.
	got := h.route(h.main, entryFrame(frameSuccess("main-1", completed())))

	// Assert.
	assertNames(t, got, []string{"feed.OnAgentTerminal", "footer.OnAgentTerminal", "sidebar.OnAgentTerminal"})
	if !h.hasRecord("warn", "daemon.sessionwatcher.turn_end_withheld") {
		t.Fatal("the withheld turn end was not warned about")
	}
	if h.w.TurnInFlight() == nil {
		t.Fatal("the turn was closed without being attributed")
	}
}

// TestSubagentTerminalIsNotTheTurnsEnd covers subagent parity's limit: a
// subagent's terminal is drawn like the main agent's, and closes no turn.
func TestSubagentTerminalIsNotTheTurnsEnd(t *testing.T) {
	// Arrange.
	h := newHarness(t, Session{Started: sessionStarted("turn-1")})
	h.w.SetMainAgent(agentID("main-1"))
	h.quiet()

	// Act.
	got := h.route(h.main, entryFrame(frameSuccess("sub-9", completed())))

	// Assert.
	assertNames(t, got, []string{"feed.OnAgentTerminal", "footer.OnAgentTerminal", "sidebar.OnAgentTerminal"})
	if h.w.TurnInFlight() == nil {
		t.Fatal("a subagent's terminal closed the session's turn")
	}
}

// TestTurnCloseOf covers how each terminal arm closes a turn, including the
// two that are answers rather than failures.
func TestTurnCloseOf(t *testing.T) {
	tests := []struct {
		name    string
		success *conversationv1.AgentSuccess
		failure *conversationv1.AgentFailure
		want    TurnClose
	}{
		{
			name:    "the agent finishing on its own completes the turn",
			success: completed(),
			want:    wsm.CloseCompleted,
		},
		{
			name:    "an acknowledged stop kills the turn",
			success: interrupted(),
			want:    wsm.CloseKilled,
		},
		{
			name:    "backgrounding completes the turn: it is what was asked for",
			success: backgrounded(),
			want:    wsm.CloseCompleted,
		},
		{
			name:    "a failure fails the turn",
			failure: &conversationv1.AgentFailure{},
			want:    wsm.CloseFailed,
		},
		{
			name:    "work lost while detached fails the turn like any other failure",
			failure: lostFailure(),
			want:    wsm.CloseFailed,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange / Act.
			got := turnCloseOf(tt.success, tt.failure)

			// Assert.
			if got != tt.want {
				t.Fatalf("turnCloseOf = %v, want %v", got, tt.want)
			}
		})
	}
}

// TestDetachedLostIsAnOrdinaryTerminal covers the DetachedLost arms: work the
// daemon lost track of ended, and it ends through the same terminal path as
// anything else — nothing about it is special to the watcher.
func TestDetachedLostIsAnOrdinaryTerminal(t *testing.T) {
	// Arrange: a detached subagent with its own watch.
	h := newHarness(t, Session{Started: sessionStarted("", createdWork("w-1", subagentWork("sub-1")))})
	open := h.client.nextAgentOpen(t)
	h.quiet()

	// Act.
	got := h.routeReaping(open.stream, entryFrame(&conversationv1.AgentFrame{
		AgentId: agentID("sub-1"),
		Result:  &conversationv1.AgentFrame_Failure{Failure: lostFailure()},
	}))

	// Assert.
	assertNames(t, got, []string{"feed.OnAgentTerminal", "footer.OnAgentTerminal", "sidebar.OnAgentTerminal"})
	if !h.w.LiveWork().Empty() {
		t.Fatal("lost work stayed in the live set")
	}
}

// TestRouteBashFrame covers a detached shell's progress: the feed draws the
// bubble and the footer advances the chip, and nothing else is involved.
func TestRouteBashFrame(t *testing.T) {
	// Arrange.
	h := newHarness(t, Session{Started: sessionStarted("", createdWork("w-1", bashWork()))})
	h.client.nextBashOpen(t)
	h.quiet()
	entry := h.shellWatchFor("w-1")

	// Act.
	got := h.routeNow(func(w *watcher) {
		w.routeBashLocked(entry, &conversationv1.AgentBash{
			Result: &conversationv1.AgentBash_Update{Update: &conversationv1.AgentBashUpdate{}},
		})
	})

	// Assert.
	assertNames(t, got, []string{"feed.OnBash", "footer.OnBash"})
}

// TestRouteDetachedWorkAnnouncement covers the announcement itself: the bubble
// head reaches the feed, the chip the footer, the roster row the sidebar.
func TestRouteDetachedWorkAnnouncement(t *testing.T) {
	// Arrange.
	h := newHarness(t, Session{Started: sessionStarted("")})
	h.quiet()

	// Act.
	got := h.route(h.main, entryFrame(frameDetached("main-1", createdWork("w-1", bashWork()))))

	// Assert.
	assertNames(t, got, []string{
		"feed.OnDetachedWork", "footer.OnDetachedWork", "sidebar.OnDetachedWork", "lifecycle.OnLiveWorkChanged",
	})
}

// TestRouteSessionUpdateArms covers the per-arm split of the session's
// standing stream. One case per arm family, because each names a different set
// of views and a wrong one is silent.
func TestRouteSessionUpdateArms(t *testing.T) {
	tests := []struct {
		name   string
		update *conversationv1.SessionUpdate
		want   []string
	}{
		{
			name:   "diagnostics is the topbar's",
			update: diagnosticsUpdate(),
			want:   []string{"topbar.OnSessionUpdate"},
		},
		{
			name:   "context usage is the topbar's",
			update: contextUsageUpdate(),
			want:   []string{"topbar.OnSessionUpdate"},
		},
		{
			name:   "identity rotation is the topbar's",
			update: identityRotatedUpdate(),
			want:   []string{"topbar.OnSessionUpdate"},
		},
		{
			name:   "fast mode is the topbar's",
			update: fastModeUpdate(),
			want:   []string{"topbar.OnSessionUpdate"},
		},
		{
			name:   "an mcp server's health is the topbar's",
			update: mcpServerUpdate(),
			want:   []string{"topbar.OnSessionUpdate"},
		},
		{
			name:   "a model change is the topbar's and the roster's",
			update: modelChangedUpdate(),
			want:   []string{"topbar.OnSessionUpdate", "sidebar.OnSessionUpdate"},
		},
		{
			name:   "a permission mode change is the topbar's and the roster's",
			update: permissionModeChangedUpdate(),
			want:   []string{"topbar.OnSessionUpdate", "sidebar.OnSessionUpdate"},
		},
		{
			name:   "account usage is the footer's",
			update: accountUsageUpdate(),
			want:   []string{"footer.OnSessionUpdate"},
		},
		{
			name:   "the rate-limit status is the footer's",
			update: rateLimitStatusUpdate(),
			want:   []string{"footer.OnSessionUpdate"},
		},
		{
			name:   "a beginning compaction is the footer's",
			update: compactingUpdate(),
			want:   []string{"footer.OnSessionUpdate"},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t, Session{Started: sessionStarted("")})
			h.quiet()

			// Act.
			got := h.routeNow(func(w *watcher) { w.routeSessionUpdateLocked(tt.update) })

			// Assert.
			assertNames(t, got, tt.want)
		})
	}
}

// TestRouteQueryDied covers the session's death: every view reflects it, and
// the daemon's own machinery is told, because an open turn will never get a
// terminal now and a lease holder waiting on freeness would wait forever.
func TestRouteQueryDied(t *testing.T) {
	// Arrange.
	h := newHarness(t, Session{Started: sessionStarted("turn-1", createdWork("w-1", bashWork()))})
	h.client.nextBashOpen(t)
	h.w.SetMainAgent(agentID("main-1"))
	h.quiet()

	// Act.
	got := h.routeNow(func(w *watcher) { w.routeSessionUpdateLocked(queryDiedUpdate()) })

	// Assert.
	assertNames(t, got, []string{
		"footer.OnSessionUpdate", "feed.OnSessionUpdate", "sidebar.OnSessionUpdate",
		"lifecycle.OnTurnEnded", "lifecycle.OnLiveWorkChanged",
	})
	if !h.w.Free() {
		t.Fatal("a dead session is not free; a lease holder would wait forever")
	}
}

// TestUnroutedSessionArmIsWarnedAbout covers an arm with no route: a frame the
// daemon silently drops is a fact nobody ever draws.
func TestUnroutedSessionArmIsWarnedAbout(t *testing.T) {
	// Arrange.
	h := newHarness(t, Session{Started: sessionStarted("")})
	h.quiet()

	// Act.
	h.routeNow(func(w *watcher) { w.routeSessionUpdateLocked(&conversationv1.SessionUpdate{}) })

	// Assert.
	if !h.hasRecord("warn", "daemon.sessionwatcher.session_update_unrouted") {
		t.Fatal("an unroutable SessionUpdate arm was dropped without a warning")
	}
}

// TestActivityToolName covers the one place a tool name is derived, since a
// permission notification names the tool and nothing else can supply it.
func TestActivityToolName(t *testing.T) {
	tests := []struct {
		name string
		act  *conversationv1.AgentActivity
		want string
	}{
		{
			name: "a modeled tool call is named by its arm",
			act:  readActivity("act-1"),
			want: "Read",
		},
		{
			name: "an unmodeled call is named by the tool it stated",
			act:  unmodeledActivity("act-1", "mcp__thing__do"),
			want: "mcp__thing__do",
		},
		{
			name: "prose is not a tool call and has no name",
			act:  responseActivity("act-1"),
			want: "",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange / Act.
			got := activityToolName(tt.act)

			// Assert.
			if got != tt.want {
				t.Fatalf("activityToolName = %q, want %q", got, tt.want)
			}
		})
	}
}

// TestSessionStreamReachesRouting covers the session stream's plumbing, which
// the per-arm table deliberately bypasses: a frame pushed on WatchSession has
// to reach the routing at all.
func TestSessionStreamReachesRouting(t *testing.T) {
	// Arrange.
	h := newHarness(t, Session{Started: sessionStarted("")})
	h.quiet()

	// Act.
	h.session.send(t, compactingUpdate())

	// Assert: the wait returns the moment the footer is called.
	h.rec.until(t, "footer.OnSessionUpdate")
}

// TestBashStreamReachesRouting covers the bash stream's plumbing for the same
// reason.
func TestBashStreamReachesRouting(t *testing.T) {
	// Arrange.
	h := newHarness(t, Session{Started: sessionStarted("", createdWork("w-1", bashWork()))})
	open := h.client.nextBashOpen(t)
	h.quiet()

	// Act.
	open.stream.send(t, &conversationv1.AgentBash{
		Result: &conversationv1.AgentBash_Update{Update: &conversationv1.AgentBashUpdate{}},
	})

	// Assert.
	h.rec.until(t, "footer.OnBash")
}
