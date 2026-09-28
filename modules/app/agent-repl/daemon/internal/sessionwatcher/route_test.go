package sessionwatcher

import (
	"slices"
	"testing"

	"connectrpc.com/connect"

	conversationv1 "agentrepl/proto/conversation/v1"
	shimv1 "agentrepl/proto/shim/v1"

	"claude-repld/internal/ids"
	"claude-repld/internal/shimclient"
	"claude-repld/internal/wsm"
)

// TestRouteActivityGoesToTheThreeSinksThatDrawFromIt covers the ordinary
// activity: the feed draws the row, the footer advances its status tree, and
// the topbar accumulates the session's token spend from the usage the frame
// carries.
func TestRouteActivityGoesToTheThreeSinksThatDrawFromIt(t *testing.T) {
	// Arrange.
	h := newHarness(t, Session{Started: sessionStarted("")})
	h.quiet()

	// Act.
	got := h.route(h.main, entryFrame(frameUpdate("main-1", activityUpdate(readActivity("act-1")))))

	// Assert.
	assertNames(t, got, []string{"feed.OnActivity", "footer.OnActivity", "topbar.OnActivity"})
}

// TestRouteUnmodeledActivityAlsoWarnsTheTopbar covers the extra thing an
// unmodeled activity earns beyond the ordinary routing: a warning the topbar
// shows.
func TestRouteUnmodeledActivityAlsoWarnsTheTopbar(t *testing.T) {
	// Arrange.
	h := newHarness(t, Session{Started: sessionStarted("")})
	h.quiet()

	// Act.
	got := h.route(h.main, entryFrame(frameUpdate("main-1", activityUpdate(unmodeledActivity("act-1", "StructuredOutput")))))

	// Assert.
	assertNames(t, got, []string{"feed.OnActivity", "footer.OnActivity", "topbar.OnActivity"})
	if !h.hasRecord("warn", "daemon.sessionwatcher.unmodeled_activity") {
		t.Fatal("an unmodeled activity was not warned about")
	}
}

// TestRouteMcpToolCallRoutesAsAnOrdinaryActivity covers an MCP server's tool:
// an ordinary tool call, routed like any other.
func TestRouteMcpToolCallRoutesAsAnOrdinaryActivity(t *testing.T) {
	// Arrange.
	h := newHarness(t, Session{Started: sessionStarted("")})
	h.quiet()

	// Act.
	got := h.route(h.main, entryFrame(frameUpdate("main-1", activityUpdate(mcpActivity("act-1", "mcp__claude-in-chrome__navigate")))))

	// Assert.
	assertNames(t, got, []string{"feed.OnActivity", "footer.OnActivity", "topbar.OnActivity"})
}

// TestRouteMcpToolCallIsNeverWarnedAboutAsUnmodeled covers the WARN that used
// to fire for every MCP call: it no longer does.
func TestRouteMcpToolCallIsNeverWarnedAboutAsUnmodeled(t *testing.T) {
	// Arrange.
	h := newHarness(t, Session{Started: sessionStarted("")})
	h.quiet()

	// Act.
	h.route(h.main, entryFrame(frameUpdate("main-1", activityUpdate(mcpActivity("act-1", "mcp__claude-in-chrome__navigate")))))

	// Assert.
	if h.hasRecord("warn", "daemon.sessionwatcher.unmodeled_activity") {
		t.Fatal("an MCP tool call was warned about as unmodeled")
	}
}

// TestRouteSubagentHandbackIsNeverWarnedAboutAsUnmodeled covers the WARN that
// used to fire once per finished subagent: the hand-back is modeled now.
func TestRouteSubagentHandbackIsNeverWarnedAboutAsUnmodeled(t *testing.T) {
	// Arrange.
	h := newHarness(t, Session{Started: sessionStarted("")})
	h.quiet()

	// Act.
	h.route(h.main, entryFrame(frameUpdate("main-1", activityUpdate(handbackActivity("act-1")))))

	// Assert.
	if h.hasRecord("warn", "daemon.sessionwatcher.unmodeled_activity") {
		t.Fatal("a subagent hand-back was warned about as unmodeled")
	}
}

// TestRouteSubagentHandbackRoutesAsAnOrdinaryActivity covers the hand-back
// reaching every activity sink.
func TestRouteSubagentHandbackRoutesAsAnOrdinaryActivity(t *testing.T) {
	// Arrange.
	h := newHarness(t, Session{Started: sessionStarted("")})
	h.quiet()

	// Act.
	got := h.route(h.main, entryFrame(frameUpdate("main-1", activityUpdate(handbackActivity("act-1")))))

	// Assert.
	assertNames(t, got, []string{"feed.OnActivity", "footer.OnActivity", "topbar.OnActivity"})
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
	assertNames(t, got, []string{"feed.OnActivity", "footer.OnActivity", "topbar.OnActivity"})
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

// TestAnsweredPermissionRetiresTheAttentionMarker covers the user's own
// answer: the ask that raised the marker is settled, so the notification is
// SEEN and the marker is cleared without waiting for a workspace switch.
func TestAnsweredPermissionRetiresTheAttentionMarker(t *testing.T) {
	// Arrange: the ask is open, so the marker stands.
	h := newHarness(t, Session{Started: sessionStarted("")})
	h.quiet()
	h.route(h.main, entryFrame(frameUpdate("main-1", permissionUpdate("p-1", "act-1", "Claude wants to read foo.txt", "Read file"))))

	// Act.
	got := h.route(h.main, entryFrame(frameUpdate("main-1", permissionSettledUpdate("p-1", allowedOnce()))))

	// Assert.
	if _, ok := find(got, "lifecycle.OnAsksSettled"); !ok {
		t.Fatalf("the answered ask did not clear the attention marker: %v", names(got))
	}
}

// TestPolicyDeniedPermissionRetiresTheAttentionMarker covers a gate DECIDED
// without the user: a deny rule settles the open ask, and an ask nobody can
// answer any more is no longer something unseen.
func TestPolicyDeniedPermissionRetiresTheAttentionMarker(t *testing.T) {
	// Arrange.
	h := newHarness(t, Session{Started: sessionStarted("")})
	h.quiet()
	h.route(h.main, entryFrame(frameUpdate("main-1", permissionUpdate("p-1", "act-1", "Claude wants to read foo.txt", "Read file"))))

	// Act.
	got := h.route(h.main, entryFrame(frameUpdate("main-1", permissionSettledUpdate("p-1", deniedByPolicy()))))

	// Assert.
	if _, ok := find(got, "lifecycle.OnAsksSettled"); !ok {
		t.Fatalf("the policy-decided ask did not clear the attention marker: %v", names(got))
	}
}

// TestAnOpenAskKeepsTheAttentionMarker covers two asks with one answered: the
// marker names UNSEEN notifications, and the second ask is still one.
func TestAnOpenAskKeepsTheAttentionMarker(t *testing.T) {
	// Arrange: two asks open.
	h := newHarness(t, Session{Started: sessionStarted("")})
	h.quiet()
	h.route(h.main, entryFrame(frameUpdate("main-1", permissionUpdate("p-1", "act-1", "Claude wants to read foo.txt", "Read file"))))
	h.route(h.main, entryFrame(frameUpdate("main-1", permissionUpdate("p-2", "act-2", "Claude wants to read bar.txt", "Read file"))))

	// Act.
	got := h.route(h.main, entryFrame(frameUpdate("main-1", permissionSettledUpdate("p-1", allowedOnce()))))

	// Assert.
	if _, ok := find(got, "lifecycle.OnAsksSettled"); ok {
		t.Fatalf("the marker was cleared with an ask still open: %v", names(got))
	}
}

// TestASettleForAnAskThatNeverOpenedClearsNothing covers the policy denial
// that never had an open ask: it raised no marker, so its settle must not
// retire one another ask raised.
func TestASettleForAnAskThatNeverOpenedClearsNothing(t *testing.T) {
	// Arrange: one ask open, and a second call denied without ever asking.
	h := newHarness(t, Session{Started: sessionStarted("")})
	h.quiet()
	h.route(h.main, entryFrame(frameUpdate("main-1", permissionUpdate("p-1", "act-1", "Claude wants to read foo.txt", "Read file"))))

	// Act.
	got := h.route(h.main, entryFrame(frameUpdate("main-1", permissionSettledUpdate("p-2", deniedByPolicy()))))

	// Assert.
	if _, ok := find(got, "lifecycle.OnAsksSettled"); ok {
		t.Fatalf("a settle for an ask that never opened cleared the marker: %v", names(got))
	}
}

// TestAnsweredQuestionRetiresTheAttentionMarker covers the question ask, which
// gets a permission ask's attention treatment and must lose it the same way.
func TestAnsweredQuestionRetiresTheAttentionMarker(t *testing.T) {
	// Arrange.
	h := newHarness(t, Session{Started: sessionStarted("")})
	h.quiet()
	h.route(h.main, entryFrame(frameUpdate("main-1", questionUpdate("q-1", "Pick a branch", "Which branch should I cut from?"))))

	// Act.
	got := h.route(h.main, entryFrame(frameUpdate("main-1", questionSettledUpdate("q-1"))))

	// Assert.
	if _, ok := find(got, "lifecycle.OnAsksSettled"); !ok {
		t.Fatalf("the answered question did not clear the attention marker: %v", names(got))
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

// TestRouteContextCut covers the cut: the feed draws the separation divider,
// the footer needs the same record because it is the END signal for the
// compacting state SessionUpdate.compacting opened, and the topbar needs it to
// drop the context figure the cut just invalidated.
func TestRouteContextCut(t *testing.T) {
	// Arrange.
	h := newHarness(t, Session{Started: sessionStarted("")})
	h.quiet()

	// Act.
	got := h.routeNow(func(w *watcher) {
		w.routeUpdateLocked(agentID("main-1"), &conversationv1.AgentUpdate{
			Update: &conversationv1.AgentUpdate_ContextCut{ContextCut: &conversationv1.ContextCut{}},
		}, &conversationv1.HistoryPointer{Value: "entry-1"}, nil)
	})

	// Assert.
	assertNames(t, got, []string{"feed.OnContextCut", "footer.OnContextCut", "topbar.OnContextCut"})
}

// TestRouteContextBudgetWarning covers the arm's plane: the vendor's
// context-budget warning is a transcript attachment on the AGENT plane, and
// the footer's activity line is its only consumer.
func TestRouteContextBudgetWarning(t *testing.T) {
	// Arrange.
	h := newHarness(t, Session{Started: sessionStarted("")})
	h.quiet()

	// Act.
	got := h.routeNow(func(w *watcher) { w.routeUpdateLocked(agentID("main-1"), budgetWarningFrame(), nil, nil) })

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
// feed AND the footer whole, and its entries are NOT replayed as live frames.
// The footer is on the list because a resumed conversation's prior turns reach
// this daemon only as the page.
func TestRouteHistoryPage(t *testing.T) {
	// Arrange.
	h := newHarness(t, Session{Started: sessionStarted("")})
	h.quiet()

	// Act.
	got := h.route(h.main, pageFrame(promptEntry("ptr-9", "turn-old", "main-1")))

	// Assert.
	assertNames(t, got, []string{"feed.OnHistoryPage", "footer.OnHistoryPage"})
	if h.w.TurnInFlight() != nil {
		t.Fatal("a page's prompt opened a turn; a page is newest-first and its turns are already over")
	}
}

// TestRouteLivePromptOpensNoTurn covers the main watch's live prompt: it is
// drawn and it names the main agent, but it opens no turn. The turn in flight
// is the queue's to state (or the session facts'), because a prompt row can be
// served again: after a store restart the shim re-served a finished turn's
// prompt row and stood that turn back up in flight in the watcher alone, so the
// queue held every later prompt behind a turn every other observer had closed.
func TestRouteLivePromptOpensNoTurn(t *testing.T) {
	tests := []struct {
		name string
		// before is what the watcher knew of the prompt's turn beforehand.
		before func(h *harness)
	}{
		{
			name:   "a prompt row for a turn the queue never stated",
			before: func(*harness) {},
		},
		{
			name: "a re-served prompt row for a turn that already ended",
			before: func(h *harness) {
				h.w.OnTurnOpening("ws-1", "turn-7")
				h.route(h.main, entryFrameAt(frameSuccess("main-1", completed()), "ptr-turn-7-terminal"))
			},
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t, Session{Started: sessionStarted("")})
			h.w.SetMainAgent(agentID("main-1"))
			h.quiet()
			tt.before(h)

			// Act.
			got := h.route(h.main, entryPrompt("turn-7", "main-1"))

			// Assert.
			assertNames(t, got, []string{"feed.OnPrompt"})
			if turn := h.w.TurnInFlight(); turn != nil {
				t.Fatalf("turn in flight = %v, want none: a prompt row opens no turn", *turn)
			}
		})
	}
}

// TestRouteLivePromptNamesTheMainAgent covers what a live prompt on the main
// watch still does: it names the main agent, which an adopted session has no
// other source for until its next StartTurn.
func TestRouteLivePromptNamesTheMainAgent(t *testing.T) {
	// Arrange.
	h := newHarness(t, Session{Started: sessionStarted("")})
	h.quiet()

	// Act.
	h.route(h.main, entryPrompt("turn-7", "main-1"))

	// Assert.
	h.w.mu.Lock()
	named := h.w.mainAgent.GetValue()
	h.w.mu.Unlock()
	if named != "main-1" {
		t.Fatalf("main agent = %q, want main-1", named)
	}
}

// TestRouteLivePeerMessageGoesToTheFeedAndOpensNoTurn covers a live peer
// message: it is routed to the feed as a peer message, and — unlike a prompt —
// it opens no turn, because it is not this agent's own work.
func TestRouteLivePeerMessageGoesToTheFeedAndOpensNoTurn(t *testing.T) {
	// Arrange.
	h := newHarness(t, Session{Started: sessionStarted("")})
	h.quiet()

	// Act.
	got := h.route(h.main, entryPeer("peer-1", "main-1", "Explore"))

	// Assert.
	assertNames(t, got, []string{"feed.OnPeerMessage"})
	if h.w.TurnInFlight() != nil {
		t.Fatal("a peer message opened a turn; it is not this agent's own work and drives no turn")
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
	if !h.hasRecord("info", "daemon.sessionwatcher.turn_ended") {
		t.Fatal("the turn end has no info lifecycle record")
	}
}

// TestTurnEndIsWithheldUntilTheMainAgentIsNamed covers the one thing the
// watcher refuses to guess: with no main agent named, a terminal cannot be
// attributed to the turn, and draining the prompt queue on a subagent's
// terminal is worse than waiting. The WHOLE terminal waits, views included,
// so its later replay is the routing it would have had all along.
func TestTurnEndIsWithheldUntilTheMainAgentIsNamed(t *testing.T) {
	// Arrange: an adoption with a turn already in flight and no StartTurn yet.
	h := newHarness(t, Session{Started: sessionStarted("turn-1")})
	h.quiet()

	// Act.
	got := h.route(h.main, entryFrame(frameSuccess("main-1", completed())))

	// Assert.
	assertNames(t, got, nil)
	if !h.hasRecord("debug", "daemon.sessionwatcher.turn_end_withheld") {
		t.Fatal("the withheld turn end was not recorded")
	}
	if h.w.TurnInFlight() == nil {
		t.Fatal("the turn was closed without being attributed")
	}
}

// TestAWithheldTerminalIsReleasedWhenTheMainAgentIsNamed covers the release:
// the shim's stream plane and StartTurn's answer have no ordering between
// them, so under load the terminal lands FIRST — and the turn it ends still
// has to end, because no second terminal is ever coming for it.
func TestAWithheldTerminalIsReleasedWhenTheMainAgentIsNamed(t *testing.T) {
	// Arrange: the terminal arrives before anything has named the main agent.
	h := newHarness(t, Session{Started: sessionStarted("turn-1")})
	h.quiet()
	h.route(h.main, entryFrame(frameSuccess("main-1", completed())))

	// Act: StartTurn's answer lands second, exactly as the queue hands it over.
	h.w.SetMainAgent(agentID("main-1"))
	h.w.OnTurnOpened("ws-1", &conversationv1.AgentPrompt{
		Id:    &conversationv1.TurnId{Value: "turn-1"},
		Agent: agentID("main-1"),
	}, nil)
	// The release's lifecycle edge is dispatched OFF the caller's goroutine
	// (see flushTurnEndsAsync); this is the join Close performs.
	h.w.dispatching.Wait()
	got := h.drainNow()

	// Assert: the views see the turn OPEN before its terminal, which is the
	// order they would have seen had the answer beaten the stream.
	assertNames(t, got, []string{
		"footer.OnTurnOpened", "feed.OnTurnOpened",
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
		t.Fatal("the turn is still in flight after its released terminal")
	}
}

// TestAWithheldTerminalIsRoutedUnattributedWhenTheSessionDies covers the
// release's other end: the query is dead, so the answer that would have named
// the main agent is never coming, and the terminal is routed rather than lost.
func TestAWithheldTerminalIsRoutedUnattributedWhenTheSessionDies(t *testing.T) {
	// Arrange.
	h := newHarness(t, Session{Started: sessionStarted("turn-1")})
	h.quiet()
	h.route(h.main, entryFrame(frameSuccess("main-1", completed())))

	// Act.
	got := h.routeNow(func(w *watcher) {
		w.routeQueryDiedLocked(queryDiedUpdate())
	})

	// Assert.
	requireEvent(t, got, "feed.OnAgentTerminal")
	if h.w.TurnInFlight() != nil {
		t.Fatal("the turn is still in flight after the session died")
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

// TestAReplayedTerminalNeverEndsTheOpenTurn covers the store-restart replay: the
// shim re-opens a book from a pointer that walked backward and re-serves rows
// it already served, among them the PREVIOUS turn's terminal. The frame names
// no turn, so routing it would end whichever turn is open now. A terminal row
// already served on the watch — live or on an opening page — is dropped whole.
func TestAReplayedTerminalNeverEndsTheOpenTurn(t *testing.T) {
	tests := []struct {
		name string
		// firstServing serves the old terminal row the first time.
		firstServing func(h *harness)
	}{
		{
			name: "a terminal first served live",
			firstServing: func(h *harness) {
				h.w.OnTurnOpening("ws-1", "turn-old")
				h.route(h.main, entryFrameAt(frameSuccess("main-1", completed()), "ptr-old-terminal"))
				h.w.dispatching.Wait()
			},
		},
		{
			name: "a terminal first served on an opening page",
			firstServing: func(h *harness) {
				h.route(h.main, pageFrame(frameEntryAt("ptr-old-terminal", frameSuccess("main-1", completed()))))
			},
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t, Session{Started: sessionStarted("")})
			h.w.SetMainAgent(agentID("main-1"))
			h.quiet()
			tt.firstServing(h)
			h.w.OnTurnOpening("ws-1", "turn-live")
			h.drainNow()

			// Act.
			got := h.route(h.main, entryFrameAt(frameSuccess("main-1", completed()), "ptr-old-terminal"))

			// Assert.
			assertNames(t, got, nil)
			turn := h.w.TurnInFlight()
			if turn == nil || *turn != ids.TurnID("turn-live") {
				t.Fatalf("turn in flight = %v, want turn-live still running", turn)
			}
			if !h.hasRecord("info", "daemon.sessionwatcher.terminal_replayed") {
				t.Fatal("the dropped replay has no info record")
			}
		})
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
	assertNames(t, got, []string{
		"feed.OnAgentTerminal", "footer.OnAgentTerminal", "sidebar.OnAgentTerminal",
		"lifecycle.OnLiveWorkChanged", "sidebar.OnLiveWorkChanged", "footer.OnLiveWorkChanged", "feed.OnLiveWorkChanged",
	})
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
			Result: &conversationv1.AgentBash_Tail{Tail: &conversationv1.AgentBashTail{}},
		})
	})

	// Assert.
	assertNames(t, got, []string{"feed.OnBash", "footer.OnBash"})
}

// TestRouteBashTerminalSettlesTheFeedBeforeTheSetDropsIt covers the order a
// shell's own terminal reaches the feed in: the frame first, then the set that
// no longer lists the run. The terminal is what settles the bubble with its
// exit; the set arriving first would settle it lost instead.
func TestRouteBashTerminalSettlesTheFeedBeforeTheSetDropsIt(t *testing.T) {
	// Arrange.
	h := newHarness(t, Session{Started: sessionStarted("", createdWork("w-1", bashWork()))})
	h.client.nextBashOpen(t)
	h.quiet()
	entry := h.shellWatchFor("w-1")

	// Act.
	got := h.routeNow(func(w *watcher) {
		w.routeBashLocked(entry, &conversationv1.AgentBash{
			Result: &conversationv1.AgentBash_Success{Success: &conversationv1.AgentBashSuccess{}},
		})
	})

	// Assert.
	assertNames(t, got, []string{
		"feed.OnBash", "footer.OnBash",
		"lifecycle.OnLiveWorkChanged", "sidebar.OnLiveWorkChanged", "footer.OnLiveWorkChanged", "feed.OnLiveWorkChanged",
	})
	feed := requireEvent(t, got, "feed.OnLiveWorkChanged")
	if feed.live == nil || len(feed.live.Shells) != 0 {
		t.Fatalf("the feed was told %+v, want a set without the ended shell", feed.live)
	}
}

// TestRouteDetachedSubagentFrame covers a DETACHED run's own subagent frame:
// it reaches the footer addressed by its HANDLE, so the chip retires at the
// terminal whichever book carried it. The counterpart of TestRouteBashFrame.
func TestRouteDetachedSubagentFrame(t *testing.T) {
	// Arrange.
	h := newHarness(t, Session{Started: sessionStarted("", createdWork("sub-1", subagentWork("sub-1")))})
	open := h.client.nextAgentOpen(t)
	h.quiet()

	// Act.
	// The frame is this run's terminal, so it REAPS its own stream: it is
	// bounded by the stream's end, never by a sentinel the reap would close
	// out from under (see routeReaping).
	got := h.routeReaping(open.stream, entryFrame(&conversationv1.AgentFrame{
		AgentId: agentID("sub-1"),
		Result: &conversationv1.AgentFrame_Update{Update: activityUpdate(&conversationv1.AgentActivity{
			ActivityId: &conversationv1.AgentActivityId{Value: "sub-unit-9"},
			Item: &conversationv1.AgentActivity_Subagent{Subagent: &conversationv1.AgentSubagent{
				Result: &conversationv1.AgentSubagent_Success{Success: &conversationv1.AgentSubagentSuccess{}},
			}},
		})},
	}))

	// Assert: the frame is this run's TERMINAL, so the live set loses it in the
	// same breath the chip retires.
	assertNames(t, got, []string{
		"feed.OnActivity", "footer.OnActivity", "topbar.OnActivity", "footer.OnSubagent",
		"lifecycle.OnLiveWorkChanged", "sidebar.OnLiveWorkChanged", "footer.OnLiveWorkChanged", "feed.OnLiveWorkChanged",
	})
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
		"feed.OnDetachedWork", "footer.OnDetachedWork", "sidebar.OnDetachedWork",
		"lifecycle.OnLiveWorkChanged", "sidebar.OnLiveWorkChanged", "footer.OnLiveWorkChanged", "feed.OnLiveWorkChanged",
	})
}

// TestAFreshAnnouncementReachesTheFooterWithItsAnnouncer is the other half of
// the footer's launch premise: work that has just started reaches the footer
// named by the agent that announced it, which is what separates it from an
// adoption.
func TestAFreshAnnouncementReachesTheFooterWithItsAnnouncer(t *testing.T) {
	// Arrange.
	h := newHarness(t, Session{Started: sessionStarted("")})
	h.quiet()

	// Act.
	got := h.route(h.main, entryFrame(frameDetached("main-1", createdWork("w-1", bashWork()))))

	// Assert.
	e := requireEvent(t, got, "footer.OnDetachedWork")
	if e.agent != "main-1" {
		t.Fatalf("the footer heard the announcement from %q, want main-1", e.agent)
	}
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
			// The ROSTER reads it too: an open degraded window is what the
			// row's `degraded` arm is made of, and the dot and the topbar must
			// not disagree about one push. The HEALTH REPORTER IS FIRST; see
			// TestRouteDiagnosticsRecordsTheHealthFactBeforePublishingTheView.
			name:   "diagnostics is the health reporter's, the topbar's and the roster's",
			update: diagnosticsUpdate(),
			want:   []string{"lifecycle.OnSessionDiagnostics", "topbar.OnSessionUpdate", "sidebar.OnSessionUpdate"},
		},
		{
			// The footer's tokens cell is the turn's growth of this same
			// reading, so both surfaces take the one update.
			name:   "context usage is the topbar's and the footer's",
			update: contextUsageUpdate(),
			want:   []string{"topbar.OnSessionUpdate", "footer.OnSessionUpdate"},
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
			// The vendor's ai-title was previously UNROUTED and fell to the
			// default WARN, so the topbar never drew it. It is the topbar's.
			name:   "the vendor's title is the topbar's",
			update: titleUpdate(),
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

// TestRouteDiagnosticsRecordsTheHealthFactBeforePublishingTheView pins the ONE
// ordering the diagnostics arm depends on: the health reporter is told before
// the topbar draws the warning strip.
//
// SessionHealth answers from the reporter's recorded faults, while the topbar's
// warning strip is drawn from the push itself. With the view published first, a
// client that saw the warning and immediately asked SessionHealth was answered
// "healthy" — observed at -parallel 16 as a 2.1ms window between a topbar
// republish and the matching `daemon.health.open_fault`, which cost
// TestSessionHealthReturnsToHealthyAfterAHealthyDiagnosticsPush a run.
func TestRouteDiagnosticsRecordsTheHealthFactBeforePublishingTheView(t *testing.T) {
	// Arrange.
	h := newHarness(t, Session{Started: sessionStarted("")})
	h.quiet()

	// Act.
	got := h.routeNow(func(w *watcher) { w.routeSessionUpdateLocked(diagnosticsUpdate()) })

	// Assert.
	routed := names(got)
	health, topbar := positionOf(routed, "lifecycle.OnSessionDiagnostics"), positionOf(routed, "topbar.OnSessionUpdate")
	if health < 0 || topbar < 0 {
		t.Fatalf("routed to %v, want both the health reporter and the topbar", routed)
	}
	if health > topbar {
		t.Fatalf("routed to %v, want the health reporter told before the topbar publishes the warning", routed)
	}
}

// positionOf is the position of a routed sink call, or -1.
func positionOf(routed []string, name string) int {
	for i, got := range routed {
		if got == name {
			return i
		}
	}
	return -1
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
	// OnTurnEnded comes LAST: it is handed over off the lock (the queue
	// delivers the next prompt from it, which opens a turn back on this
	// watcher), while the view sinks are told inside it.
	// feed.OnTurnOpened precedes feed.OnSessionUpdate: the feed draws the
	// death's terminal against the turn it believes is running, and this
	// watcher may be its only source for which turn that is.
	assertNames(t, got, []string{
		"footer.OnSessionUpdate", "feed.OnTurnOpened", "feed.OnSessionUpdate",
		"sidebar.OnSessionUpdate",
		"lifecycle.OnLiveWorkChanged", "sidebar.OnLiveWorkChanged", "footer.OnLiveWorkChanged", "feed.OnLiveWorkChanged",
		"lifecycle.OnTurnEnded",
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
			act:  unmodeledActivity("act-1", "StructuredOutput"),
			want: "StructuredOutput",
		},
		{
			name: "an MCP call is named by the tool as the agent named it",
			act:  mcpActivity("act-1", "mcp__claude-in-chrome__navigate"),
			want: "mcp__claude-in-chrome__navigate",
		},
		{
			name: "a subagent hand-back is named by its vendor tool",
			act:  handbackActivity("act-1"),
			want: "SubagentHandback",
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
	h.sendSessionUpdate(t, compactingUpdate())

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
		Result: &conversationv1.AgentBash_Tail{Tail: &conversationv1.AgentBashTail{}},
	})

	// Assert.
	h.rec.until(t, "footer.OnBash")
}

// TestASyncSubagentSpawnOpensItsOwnWatch covers the sub-feed's supply: a
// spawned subagent's frames are addressed to the created agent and only ever
// reach the daemon on a watch opened for it.
func TestASyncSubagentSpawnOpensItsOwnWatch(t *testing.T) {
	// Arrange.
	h := newHarness(t, Session{Started: sessionStarted("")})
	h.quiet()

	// Act.
	h.route(h.main, entryFrame(frameUpdate("main-1", &conversationv1.AgentUpdate{
		Update: &conversationv1.AgentUpdate_Activity{Activity: subagentActivity("spawn-1", "sub-1")},
	})))

	// Assert.
	open := h.client.nextAgentOpen(t)
	if open.req.GetTarget().GetValue() != "sub-1" {
		t.Fatalf("the spawn opened a watch on %q, want the created agent sub-1", open.req.GetTarget().GetValue())
	}
}

// TestASyncSubagentIsNotLiveWork covers freeness: an in-turn subagent is the
// turn's own progress, so its watch must not hold the workspace unfree.
func TestASyncSubagentIsNotLiveWork(t *testing.T) {
	// Arrange.
	h := newHarness(t, Session{Started: sessionStarted("")})
	h.quiet()

	// Act.
	h.route(h.main, entryFrame(frameUpdate("main-1", &conversationv1.AgentUpdate{
		Update: &conversationv1.AgentUpdate_Activity{Activity: subagentActivity("spawn-1", "sub-1")},
	})))

	// Assert.
	if live := h.w.LiveWork(); len(live.Agents) != 0 {
		t.Fatalf("live work = %v, want no agents: a sync subagent is not detached work", live.Agents)
	}
}

// TestDetachedAnnouncementPromotesTheSyncWatch covers the promotion: the spawn
// was already watched as the turn's own progress, so the announcement's only
// job is to hand that watch the handle the live set reports.
func TestDetachedAnnouncementPromotesTheSyncWatch(t *testing.T) {
	// Arrange: the spawn's own watch, opened with no handle.
	h := newHarness(t, Session{Started: sessionStarted("")})
	h.route(h.main, entryFrame(frameUpdate("main-1", activityUpdate(subagentActivity("spawn-1", "sub-1")))))
	h.client.nextAgentOpen(t)
	h.quiet()

	// Act.
	h.route(h.main, entryFrame(frameDetached("main-1", createdWork("w-1", subagentWork("sub-1")))))

	// Assert.
	live := h.w.LiveWork()
	if len(live.Agents) != 1 || live.Agents[0].GetValue() != "sub-1" {
		t.Fatalf("live work = %v, want the promoted subagent", live.Agents)
	}
}

// TestPromotedWatchIsNotOpenedTwice covers the other half of the promotion: the
// watch that already exists is reused, never replaced by a second stream.
func TestPromotedWatchIsNotOpenedTwice(t *testing.T) {
	// Arrange.
	h := newHarness(t, Session{Started: sessionStarted("")})
	h.route(h.main, entryFrame(frameUpdate("main-1", activityUpdate(subagentActivity("spawn-1", "sub-1")))))
	h.client.nextAgentOpen(t)
	h.quiet()

	// Act.
	h.route(h.main, entryFrame(frameDetached("main-1", createdWork("w-1", subagentWork("sub-1")))))

	// Assert.
	h.client.noAgentOpen(t)
}

// TestARefusedShellWatchOpenKeepsTheShellLive covers the refusal: a watch the
// shim would not open says nothing about whether the shell has ENDED, which
// only the shim's conclusion may say, so the shell stays in the live set. (Its
// stream-less entry is what the next announcement re-opens:
// TestRepeatedAnnouncementReopensARefusedShellWatch.)
func TestARefusedShellWatchOpenKeepsTheShellLive(t *testing.T) {
	// Arrange.
	h := newHarness(t, Session{Started: sessionStarted("")})
	h.quiet()
	h.client.setBashErr(refusedOpenError("WatchBash", connect.CodeNotFound, "no rows for the handle yet"))

	// Act.
	h.route(h.main, entryFrame(frameDetached("main-1", createdWork("w-1", bashWork()))))
	h.client.awaitRefusedOpen(t, "WatchBash")
	h.client.settleOpens()

	// Assert.
	assertLiveWork(t, h.w.LiveWork(), LiveWorkSet{Shells: []*conversationv1.DetachedWorkId{workID("w-1")}})
}

// TestARefusedShellWatchOpenNeverSeversTheLink pins the classification for a
// shell: the shim refuses WatchBash until its store holds the handle's rows,
// and that refusal says nothing about the link.
func TestARefusedShellWatchOpenNeverSeversTheLink(t *testing.T) {
	// Arrange.
	h := newHarness(t, Session{Started: sessionStarted("")})
	h.quiet()
	h.client.setBashErr(refusedOpenError("WatchBash", connect.CodeNotFound, "no rows for the handle yet"))

	// Act.
	h.route(h.main, entryFrame(frameDetached("main-1", createdWork("w-1", bashWork()))))
	h.client.awaitRefusedOpen(t, "WatchBash")

	// Assert.
	if got := h.w.Link(); got != shimclient.LinkConnected {
		t.Fatalf("the link after a refused shell open = %v, want LinkConnected", got)
	}
}

// TestRepeatedAnnouncementReopensARefusedSubagentWatch is the subagent's half
// of the retry: the shim refuses WatchAgent for a book it has not registered,
// and the repeated announcement is the occasion to open one.
func TestRepeatedAnnouncementReopensARefusedSubagentWatch(t *testing.T) {
	// Arrange: a first announcement the shim refused.
	h := newHarness(t, Session{Started: sessionStarted("")})
	h.quiet()
	h.client.setAgentErr(refusedOpenError("WatchAgent", connect.CodeNotFound, "no such agent"))
	h.route(h.main, entryFrame(frameDetached("main-1", createdWork("w-1", subagentWork("sub-1")))))
	h.client.awaitRefusedOpen(t, "WatchAgent")
	h.client.setAgentErr(nil)

	// Act.
	h.route(h.main, entryFrame(frameDetached("main-1", createdWork("w-1", subagentWork("sub-1")))))

	// Assert.
	if open := h.client.nextAgentOpen(t); open.req.GetTarget().GetValue() != "sub-1" {
		t.Fatalf("re-opened watch = %q, want sub-1", open.req.GetTarget().GetValue())
	}
}

// TestRepeatedAnnouncementReopensARefusedShellWatch covers the retry: the shim
// refuses WatchBash until its store holds the handle, and the repeat is what
// gets the shell watched and drawn.
func TestRepeatedAnnouncementReopensARefusedShellWatch(t *testing.T) {
	// Arrange: a first announcement the shim refused.
	h := newHarness(t, Session{Started: sessionStarted("")})
	h.quiet()
	h.client.setBashErr(refusedOpenError("WatchBash", connect.CodeNotFound, "no rows for the handle yet"))
	h.route(h.main, entryFrame(frameDetached("main-1", createdWork("w-1", bashWork()))))
	h.client.awaitRefusedOpen(t, "WatchBash")
	h.client.setBashErr(nil)

	// Act.
	h.route(h.main, entryFrame(frameDetached("main-1", createdWork("w-1", bashWork()))))

	// Assert.
	if open := h.client.nextBashOpen(t); open.work.GetValue() != "w-1" {
		t.Fatalf("re-opened watch = %q, want w-1", open.work.GetValue())
	}
	if live := h.w.LiveWork(); len(live.Shells) != 1 {
		t.Fatalf("live work = %v, want the re-opened shell", live.Shells)
	}
}

// TestDetachedSubagentSettlesOutOfTheLiveSet covers the settle a detached run
// actually gets: its own stream carries no agent terminal, so the SPAWN UNIT's
// success arm — addressed by the work handle — is what drops it from the live
// set.
func TestDetachedSubagentSettlesOutOfTheLiveSet(t *testing.T) {
	// Arrange.
	h := detachedSubagentHarness(t)

	// Act.
	h.route(h.main, entryFrame(frameUpdate("main-1", activityUpdate(settledSubagentActivity("spawn-1", false)))))

	// Assert.
	if live := h.w.LiveWork(); len(live.Agents) != 0 {
		t.Fatalf("live work = %v, want no agents once the detached run settled", live.Agents)
	}
}

// TestFailedDetachedSubagentSettlesOutOfTheLiveSet covers the other terminal
// arm: a run that ended without reporting is just as settled as one that
// reported, and holding it live would keep the workspace unfree over work that
// is over.
func TestFailedDetachedSubagentSettlesOutOfTheLiveSet(t *testing.T) {
	// Arrange.
	h := detachedSubagentHarness(t)

	// Act.
	h.route(h.main, entryFrame(frameUpdate("main-1", activityUpdate(settledSubagentActivity("spawn-1", true)))))

	// Assert.
	if live := h.w.LiveWork(); len(live.Agents) != 0 {
		t.Fatalf("live work = %v, want no agents once the detached run failed", live.Agents)
	}
}

// TestRunningDetachedSubagentStaysLive covers the half that is NOT a settle: an
// update arm is progress, and reaping on it would drop a run that is still
// going.
func TestRunningDetachedSubagentStaysLive(t *testing.T) {
	// Arrange.
	h := detachedSubagentHarness(t)

	// Act.
	h.route(h.main, entryFrame(frameUpdate("main-1", activityUpdate(runningSubagentActivity("spawn-1")))))

	// Assert.
	live := h.w.LiveWork()
	if len(live.Agents) != 1 || live.Agents[0].GetValue() != "sub-1" {
		t.Fatalf("live work = %v, want the still-running detached subagent", live.Agents)
	}
}

// TestOpeningPageOpensAWatchForAnInTurnSpawnedSubagent covers the resume
// wiring: a subagent that ran in a PRIOR session reaches this daemon only as a
// spawn frame on the main agent's opening page, and its own conversation lives
// on the child's book — so the page must open the child's watch, exactly as the
// live spawn path does. Without it the expanded bubble shows only the parent's
// commission.
func TestOpeningPageOpensAWatchForAnInTurnSpawnedSubagent(t *testing.T) {
	// Arrange.
	h := newHarness(t, Session{Started: sessionStarted("")})
	h.quiet()

	// Act: the main agent's opening page carries an in-turn spawn.
	h.route(h.main, pageFrame(frameEntryAt("ptr-1",
		frameUpdate("main-1", activityUpdate(subagentActivity("spawn-1", "sub-1"))))))

	// Assert.
	open := h.client.nextAgentOpen(t)
	if open.req.GetTarget().GetValue() != "sub-1" {
		t.Fatalf("the opening page opened a watch on %q, want the created agent sub-1", open.req.GetTarget().GetValue())
	}
}

// TestOpeningPageOpensAWatchForADetachedAnnouncedSubagent covers the second
// spawn shape a page carries: a detached-work announcement's created arm names
// the child agent the same way an in-turn spawn does, and a settled detached
// subagent is neither live work nor re-adopted on resume, so its book is
// fetched only if the page opens its watch.
func TestOpeningPageOpensAWatchForADetachedAnnouncedSubagent(t *testing.T) {
	// Arrange.
	h := newHarness(t, Session{Started: sessionStarted("")})
	h.quiet()

	// Act: the opening page carries a detached-subagent announcement.
	h.route(h.main, pageFrame(frameEntryAt("ptr-1",
		frameDetached("main-1", createdWork("w-1", subagentWork("sub-1"))))))

	// Assert.
	open := h.client.nextAgentOpen(t)
	if open.req.GetTarget().GetValue() != "sub-1" {
		t.Fatalf("the opening page opened a watch on %q, want the created agent sub-1", open.req.GetTarget().GetValue())
	}
}

// A SPAWN ABOVE THE PAGE'S NEWEST CUT opens no watch: the feed begins at the
// cut, so its bubble is withheld and its child's book has no reader. Each case
// is one cut standing between the spawn and the page's head.
func TestOpeningPageSpawnAboveItsNewestCutOpensNoWatch(t *testing.T) {
	cases := []struct {
		name string
		cut  *conversationv1.ContextCut
	}{
		{name: "above a compaction", cut: compactedCut()},
		{name: "above a clear", cut: clearedCut()},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t, Session{Started: sessionStarted("")})
			h.quiet()

			// Act: newest first — the cut, then the older spawn behind it.
			h.route(h.main, pageFrame(
				frameEntryAt("ptr-2", frameUpdate("main-1", cutUpdate(tc.cut))),
				frameEntryAt("ptr-1", frameUpdate("main-1", activityUpdate(subagentActivity("spawn-1", "sub-1")))),
			))

			// Assert.
			h.client.noAgentOpen(t)
		})
	}
}

func TestOpeningPageSpawnBelowItsNewestCutStillOpensAWatch(t *testing.T) {
	// Arrange.
	h := newHarness(t, Session{Started: sessionStarted("")})
	h.quiet()

	// Act: newest first — the spawn AFTER the cut.
	h.route(h.main, pageFrame(
		frameEntryAt("ptr-2", frameUpdate("main-1", activityUpdate(subagentActivity("spawn-1", "sub-1")))),
		frameEntryAt("ptr-1", frameUpdate("main-1", cutUpdate(compactedCut()))),
	))

	// Assert.
	open := h.client.nextAgentOpen(t)
	if open.req.GetTarget().GetValue() != "sub-1" {
		t.Fatalf("the page opened a watch on %q, want sub-1", open.req.GetTarget().GetValue())
	}
}

// TestOpeningPageDoesNotReopenAnAlreadyWatchedSubagent covers idempotency: a
// spawn already watched — here by a live frame that arrived before the page —
// opens no second stream when the page restates the same spawn.
func TestOpeningPageDoesNotReopenAnAlreadyWatchedSubagent(t *testing.T) {
	// Arrange: the live spawn opened sub-1's watch.
	h := newHarness(t, Session{Started: sessionStarted("")})
	h.route(h.main, entryFrame(frameUpdate("main-1", activityUpdate(subagentActivity("spawn-1", "sub-1")))))
	h.client.nextAgentOpen(t)
	h.quiet()

	// Act: the opening page restates the same spawn.
	h.route(h.main, pageFrame(frameEntryAt("ptr-1",
		frameUpdate("main-1", activityUpdate(subagentActivity("spawn-1", "sub-1"))))))

	// Assert.
	h.client.noAgentOpen(t)
}

// TestOpeningPageOpensWatchesForNestedSpawnsRecursively covers the whole
// subtree: a child's own opening page flows through the same routeOpeningPage
// path, so a spawn ON the child's page opens the grandchild's watch too. This
// is what carries a NESTED subagent's head-settle home on resume — that settle
// is a line in the parent-subagent's book, not the main agent's, so without the
// child watch the nested bubble restored unsettled.
func TestOpeningPageOpensWatchesForNestedSpawnsRecursively(t *testing.T) {
	// Arrange: the main page names child sub-1, whose watch opens.
	h := newHarness(t, Session{Started: sessionStarted("")})
	h.quiet()
	h.route(h.main, pageFrame(frameEntryAt("ptr-1",
		frameUpdate("main-1", activityUpdate(subagentActivity("spawn-1", "sub-1"))))))
	child := h.client.nextAgentOpen(t)
	if child.req.GetTarget().GetValue() != "sub-1" {
		t.Fatalf("first open = %q, want the child sub-1", child.req.GetTarget().GetValue())
	}

	// Act: the child's OWN opening page names grandchild sub-2.
	child.stream.send(t, pageFrame(frameEntryAt("ptr-2",
		frameUpdate("sub-1", activityUpdate(subagentActivity("spawn-2", "sub-2"))))))

	// Assert.
	grandchild := h.client.nextAgentOpen(t)
	if grandchild.req.GetTarget().GetValue() != "sub-2" {
		t.Fatalf("second open = %q, want the grandchild sub-2", grandchild.req.GetTarget().GetValue())
	}
}

// TestACutOnACatchUpPageReachesEveryViewOnce covers the cut a re-opened watch
// finds on its catch-up page: it was written while no stream stood, so it is an
// edge the views missed, and it reaches the footer and the topbar exactly as a
// live cut does — once, and after the page itself. The feed takes no second
// call: it draws the divider from the page it was just handed.
func TestACutOnACatchUpPageReachesEveryViewOnce(t *testing.T) {
	tests := []struct {
		name string
		cut  *conversationv1.ContextCut
		// titleReset is whether the cut moved the digest boundary.
		titleReset bool
	}{
		{name: "a completed compaction", cut: compactedCut(), titleReset: true},
		{name: "a clear", cut: clearedCut(), titleReset: true},
		{name: "a failed compaction", cut: failedCompactionCut(), titleReset: false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t, Session{Started: sessionStarted("")})
			h.quiet()
			h.relink(t)
			h.quiet()
			ts := h.withTitle()

			// Act.
			got := h.route(h.main, pageFrame(cutEntryAt("ptr-cut", tt.cut)))

			// Assert.
			assertNames(t, got, []string{"feed.OnHistoryPage", "footer.OnHistoryPage", "footer.OnContextCut", "topbar.OnContextCut"})
			if ts.has("OnContextReset") != tt.titleReset {
				t.Fatalf("title reset = %v, want %v", ts.has("OnContextReset"), tt.titleReset)
			}
			ctx := h.recordContext(t, "info", "daemon.sessionwatcher.context_cut")
			if ctx["source"] != "caught_up" || ctx["pointer"] != "ptr-cut" {
				t.Fatalf("the caught-up cut's record = %v, want source caught_up at ptr-cut", ctx)
			}
		})
	}
}

// TestACutIsRoutedOncePerPointer covers the cut's identity: whichever serving
// comes first — live, or on a page — is the only one routed, and a second
// serving of the same row is a replay, dropped whole and recorded.
func TestACutIsRoutedOncePerPointer(t *testing.T) {
	tests := []struct {
		name string
		// first serves the cut row the first time, and leaves the harness quiet.
		first func(t *testing.T, h *harness)
		// again serves the same row a second time and returns what it provoked.
		again func(t *testing.T, h *harness) []event
		want  []string
	}{
		{
			name: "seen live, then re-served on a catch-up page",
			first: func(t *testing.T, h *harness) {
				h.route(h.main, liveCutAt("ptr-cut", compactedCut()))
				h.relink(t)
				h.quiet()
			},
			again: func(t *testing.T, h *harness) []event {
				return h.route(h.main, pageFrame(cutEntryAt("ptr-cut", compactedCut())))
			},
			want: []string{"feed.OnHistoryPage", "footer.OnHistoryPage"},
		},
		{
			name: "caught up on a page, then re-served live",
			first: func(t *testing.T, h *harness) {
				h.relink(t)
				h.quiet()
				h.route(h.main, pageFrame(cutEntryAt("ptr-cut", compactedCut())))
			},
			again: func(t *testing.T, h *harness) []event {
				return h.route(h.main, liveCutAt("ptr-cut", compactedCut()))
			},
			want: nil,
		},
		{
			name: "seen live, then re-served live",
			first: func(t *testing.T, h *harness) {
				h.route(h.main, liveCutAt("ptr-cut", compactedCut()))
			},
			again: func(t *testing.T, h *harness) []event {
				return h.route(h.main, liveCutAt("ptr-cut", compactedCut()))
			},
			want: nil,
		},
		{
			name: "history on a first page, then re-served live",
			first: func(t *testing.T, h *harness) {
				h.route(h.main, pageFrame(cutEntryAt("ptr-cut", compactedCut())))
			},
			again: func(t *testing.T, h *harness) []event {
				return h.route(h.main, liveCutAt("ptr-cut", compactedCut()))
			},
			want: nil,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t, Session{Started: sessionStarted("")})
			h.quiet()
			tt.first(t, h)

			// Act.
			got := tt.again(t, h)

			// Assert.
			assertNames(t, got, tt.want)
			ctx := h.recordContext(t, "info", "daemon.sessionwatcher.context_cut_replayed")
			if ctx["pointer"] != "ptr-cut" {
				t.Fatalf("the replay's record names pointer %v, want ptr-cut", ctx["pointer"])
			}
		})
	}
}

// TestACutOnAFirstPageIsHistory covers a watch's FIRST page, a repaint: the
// cuts on it ended acts this daemon never saw begin, so none reaches the
// footer or the topbar — routed, it would end the turn the footer is drawing
// and drop the context figure the topbar read after it.
func TestACutOnAFirstPageIsHistory(t *testing.T) {
	// Arrange.
	h := newHarness(t, Session{Started: sessionStarted("")})
	h.quiet()

	// Act.
	got := h.route(h.main, pageFrame(cutEntryAt("ptr-cut", compactedCut())))

	// Assert.
	assertNames(t, got, []string{"feed.OnHistoryPage", "footer.OnHistoryPage"})
	if !h.hasRecord("debug", "daemon.sessionwatcher.context_cut_history") {
		t.Fatal("the history cut left no debug record")
	}
}

// TestACompactingEndedByACaughtUpCutLeavesNothingStanding covers the defect:
// the vendor's `compacting` is seen live, the watch re-opens, and the cut that
// ends the compaction is on the catch-up page. The footer must take the cut —
// the compaction's end — and nothing on the page may raise `compacting` again.
func TestACompactingEndedByACaughtUpCutLeavesNothingStanding(t *testing.T) {
	// Arrange.
	h := newHarness(t, Session{Started: sessionStarted("turn-1")})
	h.quiet()
	h.routeNow(func(w *watcher) { w.routeSessionUpdateLocked(compactingUpdate()) })
	h.relink(t)
	h.quiet()

	// Act.
	got := h.route(h.main, pageFrame(
		cutEntryAt("ptr-cut", compactedCut()),
		frameEntryAt("ptr-before", frameUpdate("main-1", activityUpdate(readActivity("act-1")))),
	))

	// Assert.
	assertNames(t, got, []string{"feed.OnHistoryPage", "footer.OnHistoryPage", "footer.OnContextCut", "topbar.OnContextCut"})
}

// TestAPagesClosingsAreTakenOldestFirst covers two cuts on one catch-up page:
// the page is newest first, and the views take the cuts in the order they were
// written, so the one left standing is the newest.
func TestAPagesClosingsAreTakenOldestFirst(t *testing.T) {
	// Arrange.
	h := newHarness(t, Session{Started: sessionStarted("")})
	h.quiet()
	h.relink(t)
	h.quiet()

	// Act.
	h.route(h.main, pageFrame(
		cutEntryAt("ptr-newer", clearedCut()),
		cutEntryAt("ptr-older", compactedCut()),
	))

	// Assert.
	var pointers []any
	for _, r := range h.log.Records() {
		if r.Level == "info" && r.Operation == "daemon.sessionwatcher.context_cut" {
			pointers = append(pointers, r.Context["pointer"])
		}
	}
	if len(pointers) != 2 || pointers[0] != "ptr-older" || pointers[1] != "ptr-newer" {
		t.Fatalf("cuts routed at %v, want ptr-older then ptr-newer", pointers)
	}
}

// TestACutOnAStartTurnPageIsLeftToTheMainWatch covers the page StartTurn's
// answer carries: the standing main watch serves the same rows live, so a cut
// on that page is neither routed nor counted served, and the live row that
// follows still reaches the views.
func TestACutOnAStartTurnPageIsLeftToTheMainWatch(t *testing.T) {
	tests := []struct {
		name string
		// act is the serving under test, returning what it provoked.
		act  func(h *harness) []event
		want []string
	}{
		{
			name: "the page itself routes no cut",
			act: func(h *harness) []event {
				h.w.OnTurnOpened("ws-1", &conversationv1.AgentPrompt{
					Id: &conversationv1.TurnId{Value: "turn-1"}, Agent: agentID("main-1"),
				}, &conversationv1.HistoryPage{Entries: []*conversationv1.HistoryEntryAt{cutEntryAt("ptr-cut", clearedCut())}})
				return h.drainNow()
			},
			want: []string{"footer.OnTurnOpened", "feed.OnTurnOpened", "feed.OnHistoryPage", "footer.OnHistoryPage"},
		},
		{
			name: "the live row that follows is routed",
			act: func(h *harness) []event {
				h.w.OnTurnOpened("ws-1", &conversationv1.AgentPrompt{
					Id: &conversationv1.TurnId{Value: "turn-1"}, Agent: agentID("main-1"),
				}, &conversationv1.HistoryPage{Entries: []*conversationv1.HistoryEntryAt{cutEntryAt("ptr-cut", clearedCut())}})
				h.drainNow()
				return h.route(h.main, liveCutAt("ptr-cut", clearedCut()))
			},
			want: []string{"feed.OnContextCut", "footer.OnContextCut", "topbar.OnContextCut"},
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t, Session{Started: sessionStarted("")})
			h.quiet()
			h.w.OnTurnOpening("ws-1", "turn-1")
			h.drainNow()

			// Act.
			got := tt.act(h)

			// Assert.
			assertNames(t, got, tt.want)
			if !h.hasRecord("debug", "daemon.sessionwatcher.context_cut_on_turn_page") {
				t.Fatal("the turn page's cut left no debug record")
			}
		})
	}
}

// TestAClosingRowWithNoPointerIsRoutedAndRecorded covers a producer defect: a
// row closing an act that carries no pointer has no identity, so it can never
// be recognized as a replay. It is routed rather than lost, and the missing
// pointer is recorded at ERROR.
func TestAClosingRowWithNoPointerIsRoutedAndRecorded(t *testing.T) {
	// Arrange.
	h := newHarness(t, Session{Started: sessionStarted("")})
	h.quiet()
	h.relink(t)
	h.quiet()
	entry := cutEntryAt("", compactedCut())
	entry.At = nil

	// Act.
	got := h.route(h.main, pageFrame(entry))

	// Assert.
	assertNames(t, got, []string{"feed.OnHistoryPage", "footer.OnHistoryPage", "footer.OnContextCut", "topbar.OnContextCut"})
	ctx := h.recordContext(t, "error", "daemon.sessionwatcher.closing_unaddressed")
	if ctx["closing"] != "context_cut" || ctx["agent_id"] != "main-1" {
		t.Fatalf("the missing pointer's record = %v, want a context_cut of main-1", ctx)
	}
}

// acceptTurn hands the watcher an accepted turn-1 whose opening page carries
// the given entries under the given boundary.
func acceptTurn(h *harness, page *conversationv1.HistoryPage) {
	h.w.OnTurnOpening("ws-1", "turn-1")
	h.w.OnTurnOpened("ws-1", &conversationv1.AgentPrompt{
		Id: &conversationv1.TurnId{Value: "turn-1"}, Agent: agentID("main-1"),
	}, page)
}

// TestATurnPageNeverWalksTheMainMarkBack covers the high-water mark StartTurn's
// page may set: the main watch serves the turn's rows live and may be AHEAD of
// the page, so the page moves the mark only when the watch holds none.
func TestATurnPageNeverWalksTheMainMarkBack(t *testing.T) {
	tests := []struct {
		name string
		// served is what the main watch was served before the page.
		served func(w *watcher)
		want   string
	}{
		{
			name: "a mark the main watch holds stands",
			served: func(w *watcher) {
				w.routeAgentResponseLocked(w.main, entryFrameAt(frameUpdate("main-1", activityUpdate(readActivity("act-1"))), "ptr-live-2"))
			},
			want: "ptr-live-2",
		},
		{
			name:   "a main watch served nothing takes the turn's row",
			served: func(*watcher) {},
			want:   "ptr-prompt-1",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t, Session{Started: sessionStarted("")})
			h.routeNow(tt.served)

			// Act.
			acceptTurn(h, &conversationv1.HistoryPage{Entries: []*conversationv1.HistoryEntryAt{
				promptEntry("ptr-prompt-1", "turn-1", "main-1"),
			}})

			// Assert.
			if got := h.w.MainKnownThrough().GetValue(); got != tt.want {
				t.Fatalf("main mark = %q, want %q", got, tt.want)
			}
		})
	}
}

// TestATurnOpeningReFiresNoHistory covers the owner's rule on the turn path at
// the watcher: the accepted turn's page is the turn's own prompt row, and
// handing it over draws that row and nothing else — no cut, no terminal, no
// activity, no detached work re-resolved.
func TestATurnOpeningReFiresNoHistory(t *testing.T) {
	// Arrange.
	h := newHarness(t, Session{Started: sessionStarted("")})
	h.quiet()

	// Act.
	acceptTurn(h, &conversationv1.HistoryPage{Entries: []*conversationv1.HistoryEntryAt{
		promptEntry("ptr-prompt-1", "turn-1", "main-1"),
	}})
	got := h.drainNow()

	// Assert.
	assertNames(t, got, []string{"footer.OnTurnOpened", "feed.OnTurnOpened", "feed.OnHistoryPage", "footer.OnHistoryPage"})
	if page := requireEvent(t, got, "feed.OnHistoryPage"); page.detail != "1" {
		t.Fatalf("the turn page drew %s entries, want the prompt row alone", page.detail)
	}
}

// TestTheFeedIsHandedABoundaryOnlyOnAFirstPage covers what a page's boundary
// means to the feed: whether older history remains below the TOP of the book.
// Only a watch's first page is read from there; a catch-up is bounded by its
// pointer and a turn page by its one-row budget, so theirs say nothing.
func TestTheFeedIsHandedABoundaryOnlyOnAFirstPage(t *testing.T) {
	floor := &conversationv1.HistoryPage_Floor{Floor: &conversationv1.HistoryFloor{}}
	more := &conversationv1.HistoryPage_More{More: &conversationv1.HistoryMore{}}
	tests := []struct {
		name string
		// act serves the page under test and answers what it provoked.
		act  func(h *harness) []event
		want string
	}{
		{
			name: "a watch's first page keeps its floor",
			act: func(h *harness) []event {
				return h.route(h.main, &shimv1.WatchAgentResponse{Frame: &shimv1.WatchAgentResponse_Page{
					Page: &conversationv1.HistoryPage{Boundary: floor},
				}})
			},
			want: "floor",
		},
		{
			name: "a catch-up page's floor is withheld",
			act: func(h *harness) []event {
				h.quiet()
				h.relink(t)
				h.drainNow()
				return h.route(h.main, &shimv1.WatchAgentResponse{Frame: &shimv1.WatchAgentResponse_Page{
					Page: &conversationv1.HistoryPage{Boundary: floor},
				}})
			},
		},
		{
			name: "a turn page's more is withheld",
			act: func(h *harness) []event {
				acceptTurn(h, &conversationv1.HistoryPage{
					Entries:  []*conversationv1.HistoryEntryAt{promptEntry("ptr-prompt-1", "turn-1", "main-1")},
					Boundary: more,
				})
				return h.drainNow()
			},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t, Session{Started: sessionStarted("")})
			h.drainNow()

			// Act.
			got := tt.act(h)

			// Assert.
			if page := requireEvent(t, got, "feed.OnHistoryPage"); page.boundary != tt.want {
				t.Fatalf("the feed was handed boundary %q, want %q", page.boundary, tt.want)
			}
		})
	}
}

// TestARetiredDetachedHandleIsNeverReadmitted covers the "added agent" churn:
// the vendor's end-of-run notification upserts the announcement row, and that
// row rides the live watch AFTER the run settled. It still reaches the views,
// but the work stays out of the live set and no watch is re-opened for it.
func TestARetiredDetachedHandleIsNeverReadmitted(t *testing.T) {
	tests := []struct {
		name string
		// settle brings the harness to a settled handle and answers the
		// re-served announcement.
		settle func(t *testing.T) (*harness, *conversationv1.AgentDetachedWork)
	}{
		{
			name: "a settled subagent",
			settle: func(t *testing.T) (*harness, *conversationv1.AgentDetachedWork) {
				h := detachedSubagentHarness(t)
				h.route(h.main, entryFrame(frameUpdate("main-1", activityUpdate(settledSubagentActivity("spawn-1", false)))))
				return h, detachedWork("w-1", "spawn-1", subagentKind("sub-1"))
			},
		},
		{
			name: "a settled shell",
			settle: func(t *testing.T) (*harness, *conversationv1.AgentDetachedWork) {
				h := newHarness(t, Session{Started: sessionStarted("", createdWork("w-2", bashWork()))})
				h.client.nextBashOpen(t)
				h.quiet()
				entry := h.shellWatchFor("w-2")
				h.routeNow(func(w *watcher) {
					w.routeBashLocked(entry, &conversationv1.AgentBash{
						Result: &conversationv1.AgentBash_Success{Success: &conversationv1.AgentBashSuccess{}},
					})
				})
				return h, createdWork("w-2", bashWork())
			},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			h, again := tt.settle(t)
			if !h.w.LiveWork().Empty() {
				t.Fatalf("live work = %+v before the re-serving, want it settled", h.w.LiveWork())
			}

			// Act.
			got := h.route(h.main, entryFrame(frameDetached("main-1", again)))

			// Assert.
			requireEvent(t, got, "feed.OnDetachedWork")
			if _, published := find(got, "lifecycle.OnLiveWorkChanged"); published {
				t.Fatalf("the re-served announcement republished the live set: %v", names(got))
			}
			if !h.w.LiveWork().Empty() {
				t.Fatalf("live work = %+v, want the settled work kept out", h.w.LiveWork())
			}
			h.client.noAgentOpen(t)
			h.client.noBashOpen(t)
			if !h.hasRecord("debug", "daemon.sessionwatcher.detached_work_retired") {
				t.Fatal("the re-served announcement left no debug record")
			}
		})
	}
}

// A SPAWN MADE BY A DETACHED RUN IS NOT THE RUN. Its frames ride the run's own
// stream, and addressing them by the run's handle rewrote the parent's footer
// row with the child and retired it at the child's launch receipt.
func TestANestedSpawnOnADetachedRunsStreamIsNotAddressedByTheRunsHandle(t *testing.T) {
	tests := []struct {
		name  string
		frame *conversationv1.AgentActivity
	}{
		{name: "the nested spawn's start", frame: subagentActivity("nested-unit", "nested-agent")},
		{name: "the nested spawn's settle", frame: settledSubagentActivity("nested-unit", false)},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange: a detached run whose own stream has already started a
			// nested spawn (so the unit is known to name another agent).
			h := newHarness(t, Session{Started: sessionStarted("", createdWork("sub-1", subagentWork("sub-1")))})
			open := h.client.nextAgentOpen(t)
			h.route(open.stream, entryFrame(frameUpdate("sub-1", activityUpdate(subagentActivity("nested-unit", "nested-agent")))))
			h.quiet()

			// Act
			got := h.route(open.stream, entryFrame(frameUpdate("sub-1", activityUpdate(tt.frame))))

			// Assert
			if _, routed := find(got, "footer.OnSubagent"); routed {
				t.Fatalf("events = %v, want no footer.OnSubagent for a nested spawn's frame", names(got))
			}
			if live := h.w.LiveWork(); len(live.Agents) != 1 || live.Agents[0].GetValue() != "sub-1" {
				t.Fatalf("live work = %v, want the parent run still live", live.Agents)
			}
		})
	}
}

// TestTheMainWatchNamesTheRootsOwnerForTheViews pins the feed's one source for
// the root's owner: the feed used to latch the FIRST agent it ever saw as the
// main one, a default that a subagent's frame arriving first would have turned
// into the whole conversation drawn on a sub-feed. The main watch names it
// instead, before the frame that needs it is routed.
func TestTheMainWatchNamesTheRootsOwnerForTheViews(t *testing.T) {
	for _, tc := range []struct {
		name string
		// frames are routed on the main watch, in order.
		frames []*shimv1.WatchAgentResponse
		want   []string
	}{
		{
			name:   "a main-watch frame names its agent for the feed",
			frames: []*shimv1.WatchAgentResponse{entryFrame(frameSuccess("main-1", backgrounded()))},
			want:   []string{"feed:main-1"},
		},
		{
			name:   "a main-watch page names the agent of its first row",
			frames: []*shimv1.WatchAgentResponse{pageFrame(frameEntryAt("ptr-1", frameSuccess("main-1", completed())))},
			want:   []string{"feed:main-1"},
		},
		{
			name: "a later row naming another agent on the main watch is not a rename",
			frames: []*shimv1.WatchAgentResponse{
				entryFrame(frameSuccess("main-1", backgrounded())),
				entryFrame(frameSuccess("sub-9", backgrounded())),
			},
			want: []string{"feed:main-1"},
		},
	} {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t, Session{Started: sessionStarted("")})
			// NO quiet(): the harness's sentinel rides the main watch as its own
			// agent, and would itself be the first row to name the main agent.

			// Act.
			for _, frame := range tc.frames {
				h.route(h.main, frame)
			}

			// Assert.
			if got := h.rec.mainNamings(); !slices.Equal(got, tc.want) {
				t.Fatalf("main namings = %v, want %v", got, tc.want)
			}
		})
	}
}

// TestAMainWatchPageReachesTheFeedWithItsAgentBeforeStartTurnNamesIt pins the
// replay half: an adopted session's opening page arrives before any StartTurn,
// and the feed must still be told whose rows it is drawing rather than being
// handed no agent at all.
func TestAMainWatchPageReachesTheFeedWithItsAgentBeforeStartTurnNamesIt(t *testing.T) {
	// Arrange.
	h := newHarness(t, Session{Started: sessionStarted("")})
	// NO quiet(): the harness's sentinel rides the main watch as its own
	// agent, and would itself be the first row to name the main agent.

	// Act.
	got := h.route(h.main, pageFrame(frameEntryAt("ptr-1", frameSuccess("main-1", completed()))))

	// Assert.
	e := requireEvent(t, got, "feed.OnHistoryPage")
	if e.agent != "main-1" {
		t.Fatalf("the feed replayed the main page as %q, want main-1", e.agent)
	}
}

// stampedFrameAt wraps an AgentFrame as one live entry at a pointer, stamped
// with the turn it was produced within ("" leaves it unstamped).
func stampedFrameAt(frame *conversationv1.AgentFrame, pointer, turn string) *shimv1.WatchAgentResponse {
	resp := entryFrameAt(frame, pointer)
	if turn != "" {
		resp.GetEntry().Turn = &conversationv1.TurnId{Value: turn}
	}
	return resp
}

// A MAIN TERMINAL ENDS THE TURN ITS STAMP NAMES, never merely the open one.
// Each case routes one main-agent terminal while turn-1 is in flight.
func TestAMainTerminalIsChargedToTheTurnItsStampNames(t *testing.T) {
	tests := []struct {
		name      string
		stamp     string
		wantEnded bool
		level     string
		operation string
	}{
		{name: "stamped with the open turn, it ends it", stamp: "turn-1", wantEnded: true, level: "info", operation: "daemon.sessionwatcher.turn_ended"},
		{name: "stamped with a turn nobody opened, it ends nothing and is an error", stamp: "turn-ghost", wantEnded: false, level: "error", operation: "daemon.sessionwatcher.terminal_turn_unknown"},
		{name: "unstamped, it ends the turn in flight and says so at info", stamp: "", wantEnded: true, level: "info", operation: "daemon.sessionwatcher.terminal_unstamped"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t, Session{Started: sessionStarted("turn-1")})
			h.w.SetMainAgent(agentID("main-1"))
			h.quiet()

			// Act.
			got := h.route(h.main, stampedFrameAt(frameSuccess("main-1", completed()), "ptr-t", tt.stamp))

			// Assert.
			_, ended := find(got, "lifecycle.OnTurnEnded")
			if ended != tt.wantEnded {
				t.Fatalf("turn ended = %v, want %v (events %v)", ended, tt.wantEnded, got)
			}
			if !h.hasRecord(tt.level, tt.operation) {
				t.Fatalf("no %s record %q", tt.level, tt.operation)
			}
		})
	}
}

func TestATerminalOfAnEndedTurnDoesNotEndTheOpenOne(t *testing.T) {
	// Arrange: turn-1 ended, turn-2 is in flight.
	h := newHarness(t, Session{Started: sessionStarted("turn-1")})
	h.w.SetMainAgent(agentID("main-1"))
	h.quiet()
	h.route(h.main, stampedFrameAt(frameSuccess("main-1", completed()), "ptr-1", "turn-1"))
	h.w.dispatching.Wait()
	h.w.OnTurnOpening("ws-1", "turn-2")
	h.drainNow()

	// Act: a second terminal, at a new pointer, names turn-1.
	h.route(h.main, stampedFrameAt(frameSuccess("main-1", completed()), "ptr-2", "turn-1"))

	// Assert.
	turn := h.w.TurnInFlight()
	if turn == nil || *turn != ids.TurnID("turn-2") {
		t.Fatalf("turn in flight = %v, want turn-2 still running", turn)
	}
	if !h.hasRecord("debug", "daemon.sessionwatcher.terminal_turn_not_open") {
		t.Fatal("the terminal of an ended turn was not recorded")
	}
}

// TestAnAdoptedTurnEndsOnItsMainWatchTerminal covers the turn a PURE ATTACH
// finds running: no StartTurn of this watcher's will ever name the main agent
// for it, so the main watch's terminal names it, and the adopted turn ends
// rather than waiting forever for a name.
func TestAnAdoptedTurnEndsOnItsMainWatchTerminal(t *testing.T) {
	// Arrange: a pure attach whose shim re-announces a running turn.
	h := newHarnessAttachingPurely(t)
	h.sendSessionStarted(t, sessionStarted("turn-1"))
	open := h.client.nextAgentOpen(t)
	h.main, h.mainReq = open.stream, open.req
	h.quiet()

	// Act.
	got := h.route(h.main, entryFrame(frameSuccess("main-1", completed())))

	// Assert.
	ended := requireEvent(t, got, "lifecycle.OnTurnEnded")
	if ended.turn == nil || *ended.turn != ids.TurnID("turn-1") {
		t.Fatalf("turn ended = %v, want the adopted turn-1", ended.turn)
	}
}

// TestRouteRetiredEntryReachesItsFeedRemoval covers the `retired` arm: each
// retirable kind is handed to the feed method that removes what its live
// counterpart drew, and to nothing else.
func TestRouteRetiredEntryReachesItsFeedRemoval(t *testing.T) {
	tests := []struct {
		name  string
		entry *conversationv1.HistoryEntryAt
		want  string
	}{
		{
			name:  "a retired prompt",
			entry: promptEntry("ptr-1", "turn-7", "main-1"),
			want:  "feed.OnPromptRetired",
		},
		{
			name:  "a retired peer message",
			entry: peerEntryAt("ptr-2", "peer-1", "main-1"),
			want:  "feed.OnPeerMessageRetired",
		},
		{
			name:  "a retired api error",
			entry: frameEntryAt("ptr-3", frameUpdate("main-1", apiErrorUpdate("529 overloaded"))),
			want:  "feed.OnApiErrorRetired",
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t, Session{Started: sessionStarted("")})
			h.w.SetMainAgent(agentID("main-1"))
			h.quiet()

			// Act.
			got := h.route(h.main, retiredFrame(tt.entry))

			// Assert.
			assertNames(t, got, []string{tt.want})
		})
	}
}

// TestRouteRetiredEntryIsNeverUnrouted covers the arm's routing itself: it is
// a frame the watcher knows, so it never falls into the unrouted warning.
func TestRouteRetiredEntryIsNeverUnrouted(t *testing.T) {
	// Arrange.
	h := newHarness(t, Session{Started: sessionStarted("")})
	h.w.SetMainAgent(agentID("main-1"))
	h.quiet()

	// Act.
	h.route(h.main, retiredFrame(promptEntry("ptr-1", "turn-7", "main-1")))

	// Assert.
	if h.hasRecord("warn", "daemon.sessionwatcher.agent_frame_unrouted") {
		t.Fatal("a retired frame was reported unrouted")
	}
}

// TestRouteRetiredUnretirableKindDrawsNothing covers a producer breaking the
// contract: a retired entry of a kind the store never retires reaches no view.
func TestRouteRetiredUnretirableKindDrawsNothing(t *testing.T) {
	// Arrange.
	h := newHarness(t, Session{Started: sessionStarted("")})
	h.w.SetMainAgent(agentID("main-1"))
	h.quiet()

	// Act.
	got := h.route(h.main, retiredFrame(frameEntryAt("ptr-1", frameUpdate("main-1", activityUpdate(readActivity("act-1"))))))

	// Assert.
	assertNames(t, got, nil)
}

// TestRouteRetiredUnretirableKindIsRecordedAtError covers the same violation's
// record: it names the kind the producer sent.
func TestRouteRetiredUnretirableKindIsRecordedAtError(t *testing.T) {
	// Arrange.
	h := newHarness(t, Session{Started: sessionStarted("")})
	h.w.SetMainAgent(agentID("main-1"))
	h.quiet()

	// Act.
	h.route(h.main, retiredFrame(frameEntryAt("ptr-1", frameUpdate("main-1", activityUpdate(readActivity("act-1"))))))

	// Assert.
	ctx := h.recordContext(t, "error", "daemon.sessionwatcher.retired_unretirable")
	if ctx["kind"] != "agent_update" || ctx["pointer"] != "ptr-1" {
		t.Fatalf("the violation's record = %v, want an agent_update at ptr-1", ctx)
	}
}

// TestRouteRetiredPointerIsNotAdoptedAsTheMark covers the high-water mark: a
// retired row keeps its old position, so taking it would walk the watch's
// mark backwards and the next re-open would re-serve drawn rows.
func TestRouteRetiredPointerIsNotAdoptedAsTheMark(t *testing.T) {
	// Arrange.
	h := newHarness(t, Session{Started: sessionStarted("")})
	h.w.SetMainAgent(agentID("main-1"))
	h.quiet()
	h.route(h.main, entryFrameAt(frameUpdate("main-1", activityUpdate(readActivity("act-1"))), "ptr-newest"))

	// Act: routed directly, because the sentinel a streamed frame is bounded
	// by is itself an entry that moves the mark.
	h.routeNow(func(w *watcher) {
		w.routeAgentResponseLocked(w.main, retiredFrame(promptEntry("ptr-old", "turn-1", "main-1")))
	})

	// Assert.
	if got := h.w.Pointers().Main.GetValue(); got == "ptr-old" {
		t.Fatalf("main mark = %q, want the retired row's old pointer never adopted", got)
	}
}
