package sessioncontroller

import (
	"testing"

	datav1 "agentrepl/proto/agentshim/data/v1"
	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"claude-repld/internal/keepalive"
	"claude-repld/internal/tokenusage"
)

// mainAgentUtilization builds one MAIN-AGENT response's durable record with the
// three input buckets a context measurement is taken from.
func mainAgentUtilization(apiMessageID string, input, cacheCreation, cacheRead int64) *frontendv1.TokenUtilization {
	return &frontendv1.TokenUtilization{
		ApiMessageId: apiMessageID,
		Model:        "claude-opus-5",
		Actor:        &frontendv1.TokenUtilization_MainAgent{MainAgent: &frontendv1.TokenUtilizationMainAgent{}},
		Usage: &frontendv1.VendorTokenUsage{
			InputTokens:              input,
			CacheCreationInputTokens: cacheCreation,
			CacheReadInputTokens:     cacheRead,
		},
	}
}

// subagentUtilization is the same record attributed to a subagent, which runs a
// context of its own that this session's floor must not read.
func subagentUtilization(apiMessageID string, input, cacheCreation, cacheRead int64) *frontendv1.TokenUtilization {
	record := mainAgentUtilization(apiMessageID, input, cacheCreation, cacheRead)
	record.Actor = &frontendv1.TokenUtilization_Subagent{Subagent: &frontendv1.TokenUtilizationSubagent{
		AgentId:         "agent-1",
		ParentToolUseId: "toolu_1",
		SubagentType:    "general-purpose",
	}}
	return record
}

// contextSizeOf reads the remembered conversation size under the manager mutex.
func contextSizeOf(m *Manager, d *sessionController) int64 {
	m.mu.Lock()
	defer m.mu.Unlock()
	return d.lastContextInputTokens
}

// ONE RESPONSE'S THREE INPUT BUCKETS ARE THE CONVERSATION'S SIZE. The buckets are
// settled at message_start and name exactly the prompt that one request
// presented, so their sum is the context window's occupancy and nothing else.
func TestMainAgentContextTokensSumsEveryInputBucketOfOneResponse(t *testing.T) {
	tests := []struct {
		name          string
		input         int64
		cacheCreation int64
		cacheRead     int64
		want          int64
	}{
		{
			name:      "a warm response reads almost the whole conversation from cache",
			input:     12,
			cacheRead: 120_000,
			want:      120_012,
		},
		{
			name:          "a cold response writes the whole conversation fresh",
			input:         8,
			cacheCreation: 120_000,
			want:          120_008,
		},
		{
			name:          "all three buckets are standing context and all three count",
			input:         1_000,
			cacheCreation: 4_000,
			cacheRead:     45_000,
			want:          50_000,
		},
		{
			name: "a response that reported nothing measures zero",
			want: 0,
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			record := mainAgentUtilization("msg_1", tc.input, tc.cacheCreation, tc.cacheRead)

			// Act.
			usage, ok, err := mainAgentContextUsage(record)

			// Assert.
			if err != nil {
				t.Fatalf("mainAgentContextUsage = %v, want no error", err)
			}
			if !ok {
				t.Fatal("mainAgentContextUsage refused a main-agent record; the floor has nothing to judge against")
			}
			if got := tokenusage.ContextInput(usage); got != tc.want {
				t.Fatalf("context tokens = %d, want %d", got, tc.want)
			}
		})
	}
}

// A SUBAGENT MEASURES A DIFFERENT CONVERSATION. Its context is its own and is
// neither the session's nor a part of it, so reading it would judge the floor
// against a conversation a compaction would not rewrite.
func TestMainAgentContextTokensRefusesASubagentResponse(t *testing.T) {
	// Arrange.
	record := subagentUtilization("msg_1", 0, 0, 900_000)

	// Act.
	_, ok, err := mainAgentContextUsage(record)

	// Assert.
	if err != nil {
		t.Fatalf("mainAgentContextUsage on a subagent record = %v, want no error", err)
	}
	if ok {
		t.Fatal("mainAgentContextUsage accepted a subagent response; a subagent's context is not this conversation's size")
	}
}

// A NEGATIVE VENDOR COUNTER IS SURFACED, NOT CONVERTED. The canonical shape is
// unsigned, so converting one would hand the floor a conversation of nearly 2^64
// tokens and compact every session forever after.
func TestMainAgentContextTokensRejectsANegativeVendorCounter(t *testing.T) {
	// Arrange.
	record := mainAgentUtilization("msg_1", -1, 0, 120_000)

	// Act.
	_, ok, err := mainAgentContextUsage(record)

	// Assert.
	if err == nil {
		t.Fatal("mainAgentContextUsage accepted a negative counter; the floor would judge against a fabricated figure")
	}
	if ok {
		t.Fatal("mainAgentContextUsage reported a usable figure alongside its error")
	}
}

// THE SIZE IS REMEMBERED FROM ONE LIVE RESPONSE, which is what the
// warm-compaction floor is judged against. Nothing else in this daemon holds a
// figure for how big a live conversation is.
func TestNoteMainAgentContextSizeRemembersTheObservedConversationSize(t *testing.T) {
	// Arrange.
	m, _, _, _, _ := coldPingRig(t)
	d := controllerFor(t, m)

	// Act.
	m.noteMainAgentContextSize(d, mainAgentUtilization("msg_1", 10, 0, 120_000))

	// Assert.
	if got := contextSizeOf(m, d); got != 120_010 {
		t.Fatalf("remembered conversation size = %d, want %d", got, 120_010)
	}
}

// A RESPONSE THAT REPORTED NO INPUT AT ALL DOES NOT ERASE THE LAST REAL
// MEASUREMENT. An absent reading is not a reading of zero, and writing it would
// turn a known-large session into an unknown one — which the eligibility check
// reads as "do not compact", silently switching the feature off.
func TestNoteMainAgentContextSizeKeepsTheLastMeasurementWhenAResponseReportsNoInput(t *testing.T) {
	// Arrange.
	m, _, _, _, _ := coldPingRig(t)
	d := controllerFor(t, m)
	m.noteMainAgentContextSize(d, mainAgentUtilization("msg_1", 0, 0, keepalive.WarmCompactMinContextTokens*2))

	// Act.
	m.noteMainAgentContextSize(d, mainAgentUtilization("msg_2", 0, 0, 0))

	// Assert.
	if got := contextSizeOf(m, d); got != keepalive.WarmCompactMinContextTokens*2 {
		t.Fatalf("remembered conversation size after an empty response = %d, want the previous %d",
			got, keepalive.WarmCompactMinContextTokens*2)
	}
}

// A SUBAGENT'S RESPONSE DOES NOT MOVE THE FIGURE. A long subagent run would
// otherwise overwrite the session's own size with a conversation the floor has
// no business judging.
func TestNoteMainAgentContextSizeIgnoresASubagentResponse(t *testing.T) {
	// Arrange.
	m, _, _, _, _ := coldPingRig(t)
	d := controllerFor(t, m)
	m.noteMainAgentContextSize(d, mainAgentUtilization("msg_1", 0, 0, 120_000))

	// Act.
	m.noteMainAgentContextSize(d, subagentUtilization("msg_2", 0, 0, 900_000))

	// Assert.
	if got := contextSizeOf(m, d); got != 120_000 {
		t.Fatalf("remembered conversation size after a subagent response = %d, want the main agent's %d", got, 120_000)
	}
}

// THE MEASUREMENT IS NOT A HIGH-WATER MARK. A compaction's whole purpose is to
// make the conversation smaller, and a floor judged against the largest figure
// ever seen would read a compacted session as huge forever and compact it again
// every cache window.
func TestNoteMainAgentContextSizeFallsWhenTheConversationShrinks(t *testing.T) {
	// Arrange.
	m, _, _, _, _ := coldPingRig(t)
	d := controllerFor(t, m)
	m.noteMainAgentContextSize(d, mainAgentUtilization("msg_1", 0, 0, 400_000))

	// Act.
	m.noteMainAgentContextSize(d, mainAgentUtilization("msg_2", 0, 0, 20_000))

	// Assert.
	if got := contextSizeOf(m, d); got != 20_000 {
		t.Fatalf("remembered conversation size after a compaction = %d, want the smaller %d", got, 20_000)
	}
}

// A NEGATIVE COUNTER LEAVES THE LAST REAL MEASUREMENT STANDING AND SAYS SO. The
// figure is refused rather than recorded, and the refusal reaches the canonical
// error channel so a floor behaving oddly can be explained from the log alone.
func TestNoteMainAgentContextSizeReportsARejectedCounterAndKeepsTheLastMeasurement(t *testing.T) {
	// Arrange.
	m, _, _, _, capture := coldPingRig(t)
	d := controllerFor(t, m)
	m.noteMainAgentContextSize(d, mainAgentUtilization("msg_1", 0, 0, 120_000))

	// Act.
	m.noteMainAgentContextSize(d, mainAgentUtilization("msg_2", -1, 0, 500_000))

	// Assert.
	if got := contextSizeOf(m, d); got != 120_000 {
		t.Fatalf("remembered conversation size after a rejected counter = %d, want the previous %d", got, 120_000)
	}
	if !capture.contains("conversation size REJECTED") {
		t.Fatal("a rejected size measurement was not reported through the canonical log")
	}
}

// contextSizeConsumer is one consumer with the size hook recording everything it
// receives, which is the seam the manager binds in production.
func contextSizeConsumer(t *testing.T) (*consumer, *[]*frontendv1.TokenUtilization) {
	t.Helper()
	var seen []*frontendv1.TokenUtilization
	c := newConsumer("ws", "s", &fakePusher{}, &fakeApplier{}, nil, newFakeClearCompactStore(), emptyTurnAccountingStore{}, nil, nil, nil, nil, nil, nil)
	c.historicalUsageStore = &fakeHistoricalUsageStore{}
	c.accounting.activeTurnID = "t"
	c.accounting.turns["t"] = &accountingTurn{}
	c.onMainAgentContextSize = func(record *frontendv1.TokenUtilization) { seen = append(seen, record) }
	return c, &seen
}

// A LIVE RESPONSE REACHES THE FLOOR'S MEASURING POINT. Without this the hook is
// wired to nothing and the warm-compaction floor never learns a session's size,
// which reads as "context unknown" and declines every compaction forever.
func TestConsumeReportsALiveResponsesContextSize(t *testing.T) {
	// Arrange.
	c, seen := contextSizeConsumer(t)
	event := accountingVendorEvent(t, &datav1.ClaudeStreamMessage{Msg: &datav1.ClaudeStreamMessage_Assistant{Assistant: &datav1.AssistantMessage{
		Message: &datav1.ApiAssistantMessage{Id: "m", Model: "model", Usage: &datav1.ApiUsage{InputTokens: 10, CacheReadInputTokens: 120_000}},
	}}})

	// Act.
	if err := c.Consume(event); err != nil {
		t.Fatalf("Consume of a live response = %v", err)
	}

	// Assert.
	if len(*seen) != 1 {
		t.Fatalf("size reports = %d, want exactly the one live response", len(*seen))
	}
	usage, _, _ := mainAgentContextUsage((*seen)[0])
	if got := tokenusage.ContextInput(usage); got != 120_010 {
		t.Fatalf("reported context size = %d, want %d", got, 120_010)
	}
}

// A REPLAYED TRANSCRIPT RESPONSE IS NOT A MEASUREMENT OF NOW. It describes how
// big the conversation was at some past instant, and a floor judging the present
// against it would compact against history — including against a conversation
// that has since been compacted already.
func TestConsumeDoesNotReportAHistoricalResponsesContextSize(t *testing.T) {
	// Arrange.
	c, seen := contextSizeConsumer(t)
	event := historicalTranscriptEvent(t, "claude", "claude")

	// Act.
	if err := c.Consume(event); err != nil {
		t.Fatalf("Consume of a historical response = %v", err)
	}

	// Assert.
	if len(*seen) != 0 {
		t.Fatalf("size reports = %d, want none from replayed history", len(*seen))
	}
}
