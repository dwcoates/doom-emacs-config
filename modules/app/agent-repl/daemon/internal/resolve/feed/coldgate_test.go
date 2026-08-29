package feed

import (
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
)

// THE COLD GATE IS DATA, not prose: the daemon relays raw counts and instants
// and the CLIENT owns the wording, the formatting and the ticking. ONE row per
// workspace, so the resolved trace REPLACES the standing gate.

// coldGate finds the gate row on the root feed.
func (h *harness) coldGate() *frontendv1.FeedColdGate {
	h.t.Helper()
	for _, row := range h.rows(rootFeed()) {
		if gate := row.GetColdGate(); gate != nil {
			return gate
		}
	}
	h.t.Fatal("no cold gate on the root feed")
	return nil
}

// coldFacts is a lapsed cold context.
func coldFacts() *conversationv1.SessionCold {
	return &conversationv1.SessionCold{
		ContextTokens:   180_000,
		LastRequestAtMs: 1_699_000_000_000,
		RequestedModel:  &conversationv1.AgentModel{Name: "claude-opus-5"},
		Reason: &conversationv1.SessionCold_Lapsed{
			Lapsed: &conversationv1.SessionColdLapsed{CacheTtlMs: 300_000},
		},
	}
}

// everyScope is the compaction set the daemon normally offers.
func everyScope() []conversationv1.SessionCompactScope {
	return []conversationv1.SessionCompactScope{
		conversationv1.SessionCompactScope_SESSION_COMPACT_SCOPE_ALL,
		conversationv1.SessionCompactScope_SESSION_COMPACT_SCOPE_PROMPTS,
		conversationv1.SessionCompactScope_SESSION_COMPACT_SCOPE_RESPONSES,
	}
}

func TestTheStandingGateRelaysRawCountsAndInstants(t *testing.T) {
	// Arrange, Act.
	h := newHarness(t)
	h.resolver.RaiseColdGate(testWorkspace, coldFacts(),
		[]*conversationv1.AgentModel{{Name: "claude-haiku-4"}}, everyScope())

	// Assert: raw, not formatted — a deliberate departure, because these are
	// counts and instants rather than resolved presentation.
	standing := h.coldGate().GetStanding()
	if standing.GetContextTokens().GetTokens() != 180_000 {
		t.Fatalf("context_tokens = %d, want the raw count", standing.GetContextTokens().GetTokens())
	}
	if standing.GetLastRequest().GetAtMs() != 1_699_000_000_000 {
		t.Fatalf("last_request = %d, want the raw instant", standing.GetLastRequest().GetAtMs())
	}
	if standing.GetModel().GetModel().GetName() != "claude-opus-5" {
		t.Fatalf("model = %q, want the model the re-read would run on", standing.GetModel().GetModel().GetName())
	}
}

func TestTheCompactMenuServesExactlyWhatTheAnswerWillAccept(t *testing.T) {
	// Arrange, Act.
	h := newHarness(t)
	h.resolver.RaiseColdGate(testWorkspace, coldFacts(),
		[]*conversationv1.AgentModel{{Name: "claude-haiku-4"}, {Name: "claude-sonnet-4"}}, everyScope())

	// Assert: the SERVED values are the contract — a client can only echo one
	// back.
	menu := h.coldGate().GetStanding().GetCompact()
	if len(menu.GetModels()) != 2 {
		t.Fatalf("summarizers = %d, want 2", len(menu.GetModels()))
	}
	if menu.GetModels()[0].GetModel().GetName() != "claude-haiku-4" {
		t.Fatalf("first summarizer = %q", menu.GetModels()[0].GetModel().GetName())
	}
	if len(menu.GetScopes()) != 3 {
		t.Fatalf("scopes = %v, want all three", menu.GetScopes())
	}
}

func TestTheGateLogsWhyTheCacheWouldNotServe(t *testing.T) {
	tests := []struct {
		name   string
		reason any
		want   string
	}{
		{
			name:   "the cache lifetime passed",
			reason: &conversationv1.SessionColdLapsed{CacheTtlMs: 300_000},
			want:   "lapsed",
		},
		{
			name:   "the model is changing, and a cache is per model",
			reason: &conversationv1.SessionColdModelSwitch{},
			want:   "model_switch",
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)
			cold := coldFacts()
			switch r := tc.reason.(type) {
			case *conversationv1.SessionColdLapsed:
				cold.Reason = &conversationv1.SessionCold_Lapsed{Lapsed: r}
			case *conversationv1.SessionColdModelSwitch:
				cold.Reason = &conversationv1.SessionCold_ModelSwitch{ModelSwitch: r}
			}

			// Act.
			h.resolver.RaiseColdGate(testWorkspace, cold, nil, everyScope())

			// Assert.
			var reason string
			for _, record := range h.records() {
				if record.Operation == "daemon.feed.cold_gate_standing" {
					reason, _ = record.Context["reason"].(string)
				}
			}
			if reason != tc.want {
				t.Fatalf("logged reason = %q, want %q", reason, tc.want)
			}
		})
	}
}

func TestTheResolvedTraceReplacesTheStandingGateOnTheSameRow(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	standingID := h.resolver.RaiseColdGate(testWorkspace, coldFacts(), nil, everyScope())

	// Act.
	resolvedID := h.resolver.ResolveColdGate(testWorkspace, &conversationv1.SessionColdRemediation{
		Remediation: &conversationv1.SessionColdRemediation_Pay{Pay: &conversationv1.SessionColdPay{}},
	})

	// Assert: ONE row — the gate stays in history as the trace of the choice.
	if standingID.GetValue() != resolvedID.GetValue() {
		t.Fatalf("ids = (%q, %q), want the same row", standingID.GetValue(), resolvedID.GetValue())
	}
	if rows := h.rows(rootFeed()); len(rows) != 1 {
		t.Fatalf("rows = %d, want the one gate row", len(rows))
	}
	if h.coldGate().GetResolved().GetPay() == nil {
		t.Fatalf("choice = %T, want pay", h.coldGate().GetResolved().GetChoice())
	}
}

func TestEveryRemediationHasItsOwnTrace(t *testing.T) {
	tests := []struct {
		name        string
		remediation any
		want        string
	}{
		{name: "paid the cold read", remediation: &conversationv1.SessionColdPay{}, want: "pay"},
		{name: "cleared the context", remediation: &conversationv1.SessionColdClear{}, want: "clear"},
		{
			name: "compacted first",
			remediation: &conversationv1.SessionColdCompact{
				Model: &conversationv1.AgentModel{Name: "claude-haiku-4"},
				Scope: conversationv1.SessionCompactScope_SESSION_COMPACT_SCOPE_PROMPTS,
			},
			want: "compact",
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)
			h.resolver.RaiseColdGate(testWorkspace, coldFacts(), nil, everyScope())
			remediation := &conversationv1.SessionColdRemediation{}
			switch r := tc.remediation.(type) {
			case *conversationv1.SessionColdPay:
				remediation.Remediation = &conversationv1.SessionColdRemediation_Pay{Pay: r}
			case *conversationv1.SessionColdClear:
				remediation.Remediation = &conversationv1.SessionColdRemediation_Clear{Clear: r}
			case *conversationv1.SessionColdCompact:
				remediation.Remediation = &conversationv1.SessionColdRemediation_Compact{Compact: r}
			}

			// Act.
			h.resolver.ResolveColdGate(testWorkspace, remediation)

			// Assert.
			got := resolvedChoiceWord(h.coldGate().GetResolved())
			if got != tc.want {
				t.Fatalf("choice = %q, want %q", got, tc.want)
			}
		})
	}
}

// resolvedChoiceWord names a resolved gate's choice arm.
func resolvedChoiceWord(resolved *frontendv1.FeedColdGateResolved) string {
	switch resolved.GetChoice().(type) {
	case *frontendv1.FeedColdGateResolved_Pay:
		return "pay"
	case *frontendv1.FeedColdGateResolved_Clear:
		return "clear"
	case *frontendv1.FeedColdGateResolved_Compact:
		return "compact"
	}
	return "unset"
}

func TestAResolvedCompactionNamesItsSummarizerAndItsScope(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.resolver.RaiseColdGate(testWorkspace, coldFacts(), nil, everyScope())

	// Act.
	h.resolver.ResolveColdGate(testWorkspace, &conversationv1.SessionColdRemediation{
		Remediation: &conversationv1.SessionColdRemediation_Compact{
			Compact: &conversationv1.SessionColdCompact{
				Model: &conversationv1.AgentModel{Name: "claude-haiku-4"},
				Scope: conversationv1.SessionCompactScope_SESSION_COMPACT_SCOPE_RESPONSES,
			},
		},
	})

	// Assert: a compaction of the responses alone bought something different
	// from a compaction of everything, and the trace is where that is
	// recoverable.
	compact := h.coldGate().GetResolved().GetCompact()
	if compact.GetModel().GetModel().GetName() != "claude-haiku-4" {
		t.Fatalf("summarizer = %q", compact.GetModel().GetModel().GetName())
	}
	if compact.GetScope() != conversationv1.SessionCompactScope_SESSION_COMPACT_SCOPE_RESPONSES {
		t.Fatalf("scope = %v, want the responses scope", compact.GetScope())
	}
}

func TestAResolutionWithNoRemediationLeavesTheGateStanding(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.resolver.RaiseColdGate(testWorkspace, coldFacts(), nil, everyScope())

	// Act.
	id := h.resolver.ResolveColdGate(testWorkspace, &conversationv1.SessionColdRemediation{})

	// Assert.
	if id != nil {
		t.Fatalf("id = %+v, want nil for an unset remediation", id)
	}
	if h.coldGate().GetStanding() == nil {
		t.Fatalf("state = %T, want the gate still standing", h.coldGate().GetState())
	}
	if !h.hasRecord("warn", "daemon.feed.cold_gate_unset_remediation") {
		t.Fatalf("records = %+v, want a WARN daemon.feed.cold_gate_unset_remediation", h.records())
	}
}
