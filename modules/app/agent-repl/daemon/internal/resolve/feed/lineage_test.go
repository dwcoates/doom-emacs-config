package feed

import (
	"errors"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"

	"claude-repld/internal/ids"
)

// A FORK'S INHERITED PAST. The fork's book carries a copy of the parent's
// conversation, ingested while the fork runs; on a fork every main-agent entry
// of a turn the fork never opened (or of none) is that inherited past, drawn in
// a plane of its own, in a stance of its own, and never pushed live.

// forkTurn is the fork's own first turn in every case here.
const forkTurn = "fork-turn"

// newForkHarness is a harness for a FORK: it carries a ported conversation, and
// the turns it recorded as its own are forkTurn and the tail sentinel.
func newForkHarness(t *testing.T) *harness {
	t.Helper()
	h := newHarness(t)
	h.ported = []PortedPrompt{{Turn: "ported-turn", Text: "the parent's question", Origin: conversationv1.PromptOrigin_PROMPT_ORIGIN_USER_SENT}}
	h.owned = map[ids.TurnID]bool{forkTurn: true, "turn-sentinel": true}
	return h
}

// openForkTurn opens the fork's own first turn the way the daemon does: the
// turn is opened, then the queue's mirror draws its prompt.
func (h *harness) openForkTurn() {
	h.t.Helper()
	h.resolver.OnTurnOpened(testWorkspace, forkTurn)
	h.deliverPrompt(forkTurn, "the fork's own question")
}

// liveResponse sends one settled response of the main agent, stamped with
// TURN ("" unstamped), as the watch serves a live entry.
func (h *harness) liveResponse(unit, text, turn string) {
	h.t.Helper()
	var stamp *conversationv1.TurnId
	if turn != "" {
		stamp = &conversationv1.TurnId{Value: turn}
	}
	h.resolver.OnActivity(testWorkspace, mainAgent(), responseSuccessActivity(unit, text), stamp, nil)
}

// liveCut sends one unstamped compaction at pointer AT, the way the file plane
// serves an inherited one.
func (h *harness) liveCut(at string) {
	h.t.Helper()
	h.cutAt(at, compactedCut("summary at "+at))
}

// delivered answers every row id a reader is served, oldest first, with the
// ported conversation left out: what these cases pin is the store's copy and
// the fork's own rows.
func (h *harness) delivered() []string {
	h.t.Helper()
	h.resolver.mu.Lock()
	defer h.resolver.mu.Unlock()
	s := h.resolver.state(testWorkspace)
	f := h.resolver.feed(s, rootFeed())
	order, _ := h.resolver.deliverable(s, f, "test")
	var out []string
	for _, id := range order {
		if f.rank[id].plane == planePorted {
			continue
		}
		out = append(out, id)
	}
	return out
}

// root is the root feed's activity row id for UNIT.
func root(unit string) string { return activityRowID(rootFeed(), unit) }

func TestAForksInheritedPastArrivingLiveStandsAboveItsOwnTurnBoundedAtItsNewestCut(t *testing.T) {
	// Arrange: the fork's own turn opens, then the copied conversation streams
	// in live — two compactions among it — interleaved with the fork's own
	// responses, exactly as ship-gns received it.
	h := newForkHarness(t)
	h.openForkTurn()

	// Act.
	h.liveResponse("old-1", "inherited, before both cuts", "parent-turn-1")
	h.liveCut("cut-1")
	h.liveResponse("old-2", "inherited, between the cuts", "parent-turn-2")
	h.liveResponse("own-1", "the fork's first answer", forkTurn)
	h.liveCut("cut-2")
	h.liveResponse("old-3", "inherited, after the newest cut", "parent-turn-3")
	h.liveResponse("own-2", "the fork's second answer", forkTurn)

	// Assert: the newest inherited divider, the inherited turn after it, then
	// the fork's own prompt and its answers, contiguous.
	want := []string{
		h.separationRowID("cut-2"), root("old-3"),
		h.promptRowID(forkTurn), root("own-1"), root("own-2"),
	}
	if got := h.delivered(); !equalIDs(got, want) {
		t.Fatalf("delivered = %v, want %v", got, want)
	}
}

func TestAForksInheritedPastReplayedAroundItsOwnPromptStandsAboveIt(t *testing.T) {
	// Arrange: a restart replays the fork's book, which holds the fork's own
	// prompt BEFORE most of the copy (the book orders by first insert).
	h := newForkHarness(t)
	page := stampedPage(historyPage(&conversationv1.HistoryFloor{},
		frameEntry(mainAgent(), &conversationv1.AgentUpdate{Update: &conversationv1.AgentUpdate_Activity{Activity: responseSuccessActivity("own-1", "the fork's answer")}}),
		frameEntry(mainAgent(), &conversationv1.AgentUpdate{Update: &conversationv1.AgentUpdate_Activity{Activity: responseSuccessActivity("old-2", "after the cut")}}),
		cutEntry(compactedCut("what survived")),
		frameEntry(mainAgent(), &conversationv1.AgentUpdate{Update: &conversationv1.AgentUpdate_Activity{Activity: responseSuccessActivity("old-1", "before the cut")}}),
		promptEntry(forkTurn, "the fork's own question"),
	), forkTurn, "parent-turn-2", "", "parent-turn-1", forkTurn)
	cutPointer := page.Entries[2].GetAt().GetValue()

	// Act.
	h.replay(page)

	// Assert.
	want := []string{h.separationRowID(cutPointer), root("old-2"), h.promptRowID(forkTurn), root("own-1")}
	if got := h.delivered(); !equalIDs(got, want) {
		t.Fatalf("delivered = %v, want %v", got, want)
	}
}

func TestAForksInheritedPastIsNeverPushedLive(t *testing.T) {
	// Arrange: a reader following the fork's feed through its first turn.
	h := newForkHarness(t)
	rows := h.follow(rootFeed(), "reader-1")
	h.openForkTurn()

	// Act.
	h.liveResponse("old-1", "inherited", "parent-turn-1")
	h.liveCut("cut-1")
	h.liveResponse("own-1", "the fork's answer", forkTurn)
	got := pushedBefore(t, rows, h.sendSentinel())

	// Assert: only the fork's own rows reached the wire, so nothing the reader
	// holds is ever truncated by an inherited divider.
	want := []string{h.promptRowID(forkTurn), root("own-1")}
	if !equalIDs(got, want) {
		t.Fatalf("pushed = %v, want %v", got, want)
	}
}

func TestAnInheritedPromptNeverBecomesTheForksTurnInFlight(t *testing.T) {
	// Arrange.
	h := newForkHarness(t)
	h.openForkTurn()

	// Act: the copy carries one of the parent's prompts.
	h.deliverPrompt("parent-turn-1", "the parent's old question")

	// Assert.
	h.resolver.mu.Lock()
	defer h.resolver.mu.Unlock()
	if turn := h.resolver.state(testWorkspace).turnInFlight; turn == nil || *turn != forkTurn {
		t.Fatalf("turn in flight = %v, want the fork's own %q", turn, forkTurn)
	}
}

func TestAnInheritedPromptIsDrawnSettled(t *testing.T) {
	// Arrange.
	h := newForkHarness(t)
	h.openForkTurn()

	// Act.
	h.deliverPrompt("parent-turn-1", "the parent's old question")

	// Assert: the parent's turn ended in the parent; nothing here runs it.
	if h.promptWorking("parent-turn-1") {
		t.Fatal("an inherited prompt is drawn working")
	}
}

func TestAForksOwnCutHidesItsWholeInheritedPast(t *testing.T) {
	// Arrange: inherited rows, then the fork's own compaction during its turn.
	h := newForkHarness(t)
	h.openForkTurn()
	h.liveResponse("old-1", "inherited", "parent-turn-1")

	// Act: the shim stamps the fork's own cut with the fork's turn.
	h.resolver.OnContextCut(testWorkspace, mainAgent(), compactedCut("the fork's own"),
		&conversationv1.HistoryPointer{Value: "own-cut"}, &conversationv1.TurnId{Value: forkTurn}, nil)

	// Assert: the feed begins at the fork's own divider.
	want := []string{h.separationRowID("own-cut")}
	if got := h.delivered(); !equalIDs(got, want) {
		t.Fatalf("delivered = %v, want %v", got, want)
	}
}

func TestANonForkDrawsAnUnrecordedTurnAsItsOwn(t *testing.T) {
	// Arrange: no ported conversation, so the workspace inherited nothing.
	h := newHarness(t)
	h.owned = map[ids.TurnID]bool{}
	rows := h.follow(rootFeed(), "reader-1")

	// Act.
	h.liveResponse("unit-1", "an adopted turn's answer", "never-recorded")
	got := pushedBefore(t, rows, h.sendSentinel())

	// Assert: drawn live and pushed, as before forks were told apart.
	want := []string{root("unit-1")}
	if !equalIDs(got, want) {
		t.Fatalf("pushed = %v, want %v", got, want)
	}
}

func TestAnUnreadableTurnOwnershipIsRecordedAndDrawnAsTheForksOwn(t *testing.T) {
	// Arrange.
	h := newForkHarness(t)
	h.ownedErr = errors.New("the state database is unreadable")

	// Act.
	h.liveResponse("unit-1", "an answer", "parent-turn-1")

	// Assert.
	if !h.hasRecord("error", "daemon.feed.turn_ownership_unreadable") {
		t.Fatalf("records = %+v, want the failed ownership read at ERROR", h.records())
	}
	h.resolver.mu.Lock()
	plane := h.resolver.feed(h.resolver.state(testWorkspace), rootFeed()).rank[root("unit-1")].plane
	h.resolver.mu.Unlock()
	if plane != planeLive {
		t.Fatalf("plane = %s, want the entry drawn as the fork's own (live)", plane)
	}
}

func TestAnUnreadableForkFactIsRecorded(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.portedErr = errors.New("the state database is unreadable")

	// Act.
	h.liveResponse("unit-1", "an answer", "turn-1")

	// Assert.
	if !h.hasRecord("error", "daemon.feed.fork_unreadable") {
		t.Fatalf("records = %+v, want the failed fork read at ERROR", h.records())
	}
}

// inherits is the ONE classification; each case is one entry's standing.
func TestInheritsClassifiesByForkAgentAndTurn(t *testing.T) {
	cases := []struct {
		name  string
		fork  bool
		agent string
		turn  ids.TurnID
		want  bool
	}{
		{name: "a non-fork inherits nothing", fork: false, agent: "agent-main", turn: "parent-turn", want: false},
		{name: "a fork's unstamped main entry is inherited", fork: true, agent: "agent-main", turn: "", want: true},
		{name: "a fork's own turn is not inherited", fork: true, agent: "agent-main", turn: forkTurn, want: false},
		{name: "a fork's main entry of a turn it never opened is inherited", fork: true, agent: "agent-main", turn: "parent-turn", want: true},
		{name: "a subagent's entry is never inherited", fork: true, agent: "agent-sub", turn: "parent-turn", want: false},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)
			if tc.fork {
				h = newForkHarness(t)
			}
			h.resolver.learnLineage(testWorkspace, tc.turn)

			// Act.
			h.resolver.mu.Lock()
			got := h.resolver.inherits(h.resolver.state(testWorkspace), &conversationv1.AgentId{Value: tc.agent}, tc.turn)
			h.resolver.mu.Unlock()

			// Assert.
			if got != tc.want {
				t.Fatalf("inherits = %v, want %v", got, tc.want)
			}
		})
	}
}

// entryClass reads the agent and turn off each arm of an entry.
func TestEntryClassNamesEachArmsAgentAndTurn(t *testing.T) {
	sub := &conversationv1.AgentId{Value: "agent-sub"}
	cases := []struct {
		name      string
		at        *conversationv1.HistoryEntryAt
		wantAgent string
		wantTurn  ids.TurnID
	}{
		{
			name:      "a prompt is its recipient's, under its own turn",
			at:        &conversationv1.HistoryEntryAt{Entry: promptEntry("turn-p", "hi"), Turn: &conversationv1.TurnId{Value: "ignored"}},
			wantAgent: "agent-main", wantTurn: "turn-p",
		},
		{
			name:      "a frame is its own agent's, under its stamp",
			at:        &conversationv1.HistoryEntryAt{Entry: frameEntry(sub, completed("")), Turn: &conversationv1.TurnId{Value: "turn-f"}},
			wantAgent: "agent-sub", wantTurn: "turn-f",
		},
		{
			name: "a peer message is its recipient's, under its stamp",
			at: &conversationv1.HistoryEntryAt{Entry: &conversationv1.HistoryEntry{Entry: &conversationv1.HistoryEntry_PeerMessage{
				PeerMessage: &conversationv1.PeerMessage{Agent: mainAgent(), Id: "peer-1"},
			}}},
			wantAgent: "agent-main", wantTurn: "",
		},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act.
			agent, turn := entryClass(tc.at, mainAgent())

			// Assert.
			if agent.GetValue() != tc.wantAgent || turn != tc.wantTurn {
				t.Fatalf("entryClass = (%q, %q), want (%q, %q)", agent.GetValue(), turn, tc.wantAgent, tc.wantTurn)
			}
		})
	}
}
