package sessionwatcher

import (
	"slices"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
	"google.golang.org/protobuf/proto"
)

// openQuestion is the question q-ID as the feed stands it, asked by main-1.
func openQuestion(id string) *conversationv1.SessionOpenAsk {
	return &conversationv1.SessionOpenAsk{Agent: agentID("main-1"), Ask: &conversationv1.SessionOpenAsk_Question{
		Question: questionUpdate(id, "Pick", "Which?").GetQuestion()}}
}

// openPermission is the consent ask ID as the feed stands it, asked by main-1.
func openPermission(id string) *conversationv1.SessionOpenAsk {
	return &conversationv1.SessionOpenAsk{Agent: agentID("main-1"), Ask: &conversationv1.SessionOpenAsk_Permission{
		Permission: permissionUpdate(id, "act-"+id, "Claude wants to read foo.txt", "Read file").GetPermission()}}
}

// attachWith starts a watcher over a feed standing BEFORE, then hands it the
// facts naming OPEN, and reads up to a sentinel so every edge the facts
// provoked has landed.
func attachWith(t *testing.T, before, open []*conversationv1.SessionOpenAsk, afterStart func(*harness)) *harness {
	t.Helper()
	h := startHarnessFeed(t, Session{}, nil, nil, &mergeSink{}, &feedSink{standing: before})
	h.session = h.client.nextSessionOpen(t)
	if afterStart != nil {
		afterStart(h)
	}
	started := sessionStarted("turn-1")
	started.OpenAsks = open
	h.sendSessionStarted(t, started)
	h.sendSessionUpdate(t, compactingUpdate())
	h.rec.until(t, "footer.OnSessionUpdate")
	return h
}

func TestAttachSettlesEveryStandingAskTheFactsDoNotNameAsAbandoned(t *testing.T) {
	tests := []struct {
		name   string
		before []*conversationv1.SessionOpenAsk
		open   []*conversationv1.SessionOpenAsk
		want   []string
	}{
		{name: "a question the facts drop", before: []*conversationv1.SessionOpenAsk{openQuestion("q-1")}, want: []string{"question:q-1:abandoned"}},
		{name: "a consent ask the facts drop", before: []*conversationv1.SessionOpenAsk{openPermission("p-1")}, want: []string{"permission:p-1:abandoned"}},
		{name: "an ask the facts still name stands", before: []*conversationv1.SessionOpenAsk{openQuestion("q-1")}, open: []*conversationv1.SessionOpenAsk{openQuestion("q-1")}},
		{name: "only the dropped ask of two settles", before: []*conversationv1.SessionOpenAsk{openQuestion("q-1"), openPermission("p-1")}, open: []*conversationv1.SessionOpenAsk{openPermission("p-1")}, want: []string{"question:q-1:abandoned"}},
		{name: "a feed standing nothing settles nothing"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			h := attachWith(t, tt.before, tt.open, nil)

			// Assert
			if got := h.feed.settledAsks(); !slices.Equal(got, tt.want) {
				t.Fatalf("feed settles = %v, want %v", got, tt.want)
			}
		})
	}
}

func TestAttachLeavesAnAskTheViewsStoodOnlyAfterTheWatcherStarted(t *testing.T) {
	// Act: q-2 is stood after start, as a live start would be.
	h := attachWith(t, nil, nil, func(h *harness) { h.feed.stand(openQuestion("q-2")) })

	// Assert
	if got := h.feed.settledAsks(); len(got) != 0 {
		t.Fatalf("feed settles = %v, want none: an ask stood after start may be newer than the facts", got)
	}
}

func TestAttachLeavesAnAskThatSettledBeforeTheFactsArrived(t *testing.T) {
	// Act: q-1 stood at start and settled on its own before the facts.
	h := attachWith(t, []*conversationv1.SessionOpenAsk{openQuestion("q-1")}, nil, func(h *harness) {
		h.feed.settled("question:q-1", "answered")
	})

	// Assert
	if got := h.feed.settledAsks(); !slices.Equal(got, []string{"question:q-1:answered"}) {
		t.Fatalf("feed settles = %v, want only the ask's own answer", got)
	}
	if !h.hasRecord("debug", "daemon.sessionwatcher.ask_settled_before_facts") {
		t.Fatalf("records = %v, want a debug ask_settled_before_facts", h.log.Records())
	}
}

func TestAttachRoutesTheAbandonedSettleDownTheLiveSettlesPath(t *testing.T) {
	tests := []struct {
		name string
		ask  *conversationv1.SessionOpenAsk
		want []string
	}{
		{name: "a question", ask: openQuestion("q-1"), want: []string{"feed.OnQuestion", "footer.OnQuestion", "merge.OnQuestion", "asks.NeedsInput"}},
		{name: "a consent ask", ask: openPermission("p-1"), want: []string{"feed.OnPermission", "footer.OnPermission", "sidebar.OnPermission", "merge.OnPermission", "asks.NeedsInput"}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := startHarnessFeed(t, Session{}, nil, nil, &mergeSink{}, &feedSink{standing: []*conversationv1.SessionOpenAsk{tt.ask}})
			h.session = h.client.nextSessionOpen(t)

			// Act
			h.sendSessionStarted(t, sessionStarted("turn-1"))
			h.sendSessionUpdate(t, compactingUpdate())
			seen := h.rec.until(t, "footer.OnSessionUpdate")

			// Assert
			var asks []string
			for _, ev := range seen {
				switch ev.method {
				case "OnPermission", "OnQuestion", "NeedsInput":
					asks = append(asks, ev.name())
				}
			}
			if !slices.Equal(asks, tt.want) {
				t.Fatalf("ask edges = %v, want %v", asks, tt.want)
			}
			if !h.hasRecord("info", "daemon.sessionwatcher.ask_abandoned_at_attach") {
				t.Fatalf("records = %v, want an info ask_abandoned_at_attach", h.log.Records())
			}
		})
	}
}

func TestAbandonRefusedAskSettlesAnAskTheFeedStillStands(t *testing.T) {
	tests := []struct {
		name string
		kind AskKind
		id   string
		ask  *conversationv1.SessionOpenAsk
		want string
	}{
		{name: "a question", kind: AskKindQuestion, id: "q-1", ask: openQuestion("q-1"), want: "question:q-1:abandoned"},
		{name: "a consent ask", kind: AskKindPermission, id: "p-1", ask: openPermission("p-1"), want: "permission:p-1:abandoned"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := attachWith(t, nil, nil, nil)
			h.feed.stand(tt.ask)

			// Act
			settled := h.w.AbandonRefusedAsk(tt.kind, tt.id)

			// Assert
			if !settled || !slices.Equal(h.feed.settledAsks(), []string{tt.want}) {
				t.Fatalf("AbandonRefusedAsk = %v with settles %v, want true and %q", settled, h.feed.settledAsks(), tt.want)
			}
			if !h.hasRecord("info", "daemon.sessionwatcher.ask_abandoned_on_refusal") {
				t.Fatalf("records = %v, want an info ask_abandoned_on_refusal", h.log.Records())
			}
		})
	}
}

func TestAbandonRefusedAskTouchesNothingTheFeedNoLongerStands(t *testing.T) {
	// Arrange
	h := attachWith(t, nil, nil, nil)

	// Act
	settled := h.w.AbandonRefusedAsk(AskKindQuestion, "q-gone")

	// Assert
	if settled || len(h.feed.settledAsks()) != 0 {
		t.Fatalf("AbandonRefusedAsk = %v with settles %v, want false and none", settled, h.feed.settledAsks())
	}
	if !h.hasRecord("debug", "daemon.sessionwatcher.ask_abandoned_on_refusal") {
		t.Fatalf("records = %v, want a debug ask_abandoned_on_refusal", h.log.Records())
	}
}

func TestOpenAskKeyNamesEachKindInItsOwnSpace(t *testing.T) {
	tests := []struct {
		name string
		ask  *conversationv1.SessionOpenAsk
		want string
	}{
		{name: "a question", ask: openQuestion("x"), want: "question:x"},
		{name: "a consent ask", ask: openPermission("x"), want: "permission:x"},
		{name: "neither", ask: &conversationv1.SessionOpenAsk{}, want: ""},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if got := openAskKey(tt.ask); got != tt.want {
				t.Fatalf("openAskKey = %q, want %q", got, tt.want)
			}
		})
	}
}

func TestAbandonedQuestionRestatesTheBatchItSettles(t *testing.T) {
	// Arrange
	start := openQuestion("q-1").GetQuestion()

	// Act
	settle := abandonedQuestion(start)

	// Assert
	if settle.GetId().GetValue() != "q-1" || settle.GetSuccess().GetAbandoned() == nil ||
		!proto.Equal(settle.GetSuccess().GetBatch(), start.GetStart().GetBatch()) {
		t.Fatalf("abandonedQuestion = %v, want q-1 settled abandoned restating its batch", settle)
	}
}

func TestAbandonedPermissionNamesTheCallItGated(t *testing.T) {
	// Arrange
	start := openPermission("p-1").GetPermission()

	// Act
	settle := abandonedPermission(start)

	// Assert
	if settle.GetId().GetValue() != "p-1" || settle.GetSuccess().GetAbandoned() == nil ||
		settle.GetGatedCall().GetValue() != "act-p-1" {
		t.Fatalf("abandonedPermission = %v, want p-1 settled abandoned naming act-p-1", settle)
	}
}

func TestSettleAbandonedAskPanicsOnAnAskOfNeitherKind(t *testing.T) {
	// Arrange
	h := attachWith(t, nil, nil, nil)
	defer func() {
		if recover() == nil {
			t.Fatalf("settleAbandonedAskLocked returned, want a panic on an ask of neither kind")
		}
	}()

	// Act
	h.w.mu.Lock()
	defer h.w.mu.Unlock()
	h.w.settleAbandonedAskLocked(&conversationv1.SessionOpenAsk{}, "op", "msg")
}
