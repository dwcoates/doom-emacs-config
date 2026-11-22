package vendorspend

import (
	"errors"
	"testing"

	"agent-repld/internal/sourcescan"
	conversationv1 "agentrepl/proto/conversation/v1"
)

func TestBucketOf(t *testing.T) {
	tests := []struct {
		name    string
		tokens  uint64
		want    uint64
		inBound bool
	}{
		{name: "at a bound", tokens: 300_000, want: 300_000, inBound: true},
		{name: "just above a bound", tokens: 300_001, want: 400_000, inBound: true},
		{name: "above every bound", tokens: 1_000_001, inBound: false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got, ok := BucketOf(tt.tokens)
			// Assert
			if ok != tt.inBound || got != tt.want {
				t.Errorf("BucketOf(%d) = %d, %v; want %d, %v", tt.tokens, got, ok, tt.want, tt.inBound)
			}
		})
	}
}

func TestLabels(t *testing.T) {
	tests := []struct {
		name string
		got  string
		want string
	}{
		{name: "a cause", got: CauseLabel(conversationv1.VendorCallCause_VENDOR_CALL_CAUSE_COLD_GATE_ANSWER), want: "cold_gate_answer"},
		{name: "an agent", got: AgentLabel(conversationv1.VendorCallAgent_VENDOR_CALL_AGENT_SIDE_QUERY), want: "side_query"},
		{name: "an outcome", got: OutcomeLabel(conversationv1.VendorCacheOutcome_VENDOR_CACHE_OUTCOME_UNCACHED_NO_WRITE), want: "uncached_no_write"},
		{name: "a reason", got: ReasonLabel(conversationv1.VendorColdReason_VENDOR_COLD_REASON_LAPSE_1H), want: "lapse_1h"},
		{name: "a warm call's reason", got: ReasonLabel(conversationv1.VendorColdReason_VENDOR_COLD_REASON_UNSPECIFIED), want: ""},
		{name: "a bucket", got: ContextLabel(&conversationv1.VendorSpend{ContextLeTokens: ptr(uint64(25_000))}), want: "25000"},
		{name: "above every bucket", got: ContextLabel(&conversationv1.VendorSpend{}), want: AboveEveryBound},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if tt.got != tt.want {
				t.Errorf("got %q, want %q", tt.got, tt.want)
			}
		})
	}
}

func TestTokensOfAndInputOf(t *testing.T) {
	// Arrange
	tokens := &conversationv1.VendorTokens{UncachedInput: 1, CacheRead: 2, CacheWrite_5M: 3, CacheWrite_1H: 4, Output: 5}
	// Act
	var got []uint64
	for _, kind := range Kinds {
		got = append(got, TokensOf(tokens, kind))
	}
	// Assert
	want := []uint64{1, 2, 3, 4, 5}
	for i := range want {
		if got[i] != want[i] {
			t.Errorf("kind %s: got %d, want %d", Kinds[i], got[i], want[i])
		}
	}
	if InputOf(tokens) != 10 {
		t.Errorf("InputOf = %d, want 10", InputOf(tokens))
	}
}

func TestTokensOfPanicsOnAnUnknownKind(t *testing.T) {
	defer func() {
		if recover() == nil {
			t.Error("TokensOf did not panic on an unknown kind")
		}
	}()
	TokensOf(&conversationv1.VendorTokens{}, "thinking")
}

func ptr[T any](v T) *T { return &v }

// valid is an entry Validate accepts; each case breaks one part of it.
func valid() *conversationv1.VendorSpend {
	return &conversationv1.VendorSpend{
		Model:        "claude-opus-5-5",
		Agent:        conversationv1.VendorCallAgent_VENDOR_CALL_AGENT_MAIN,
		Cause:        conversationv1.VendorCallCause_VENDOR_CALL_CAUSE_USER_PROMPT,
		CacheOutcome: conversationv1.VendorCacheOutcome_VENDOR_CACHE_OUTCOME_WARM,
		Calls:        1,
		Tokens:       &conversationv1.VendorTokens{},
	}
}

func TestValidate(t *testing.T) {
	tests := []struct {
		name   string
		mutate func(e *conversationv1.VendorSpend)
		ok     bool
	}{
		{name: "a whole entry", mutate: func(*conversationv1.VendorSpend) {}, ok: true},
		{name: "a subagent with its type", mutate: func(e *conversationv1.VendorSpend) {
			e.Agent = conversationv1.VendorCallAgent_VENDOR_CALL_AGENT_SUBAGENT
			e.SubagentType = "Explore"
		}, ok: true},
		{name: "a cold entry with its reason", mutate: func(e *conversationv1.VendorSpend) {
			e.CacheOutcome = conversationv1.VendorCacheOutcome_VENDOR_CACHE_OUTCOME_COLD_REWRITE
			e.ColdReason = conversationv1.VendorColdReason_VENDOR_COLD_REASON_FIRST_CALL
		}, ok: true},
		{name: "no model", mutate: func(e *conversationv1.VendorSpend) { e.Model = "" }},
		{name: "no agent", mutate: func(e *conversationv1.VendorSpend) { e.Agent = 0 }},
		{name: "an undeclared agent", mutate: func(e *conversationv1.VendorSpend) { e.Agent = 99 }},
		{name: "no cause", mutate: func(e *conversationv1.VendorSpend) { e.Cause = 0 }},
		{name: "no outcome", mutate: func(e *conversationv1.VendorSpend) { e.CacheOutcome = 0 }},
		{name: "an undeclared reason", mutate: func(e *conversationv1.VendorSpend) {
			e.CacheOutcome = conversationv1.VendorCacheOutcome_VENDOR_CACHE_OUTCOME_PARTIAL
			e.ColdReason = 99
		}},
		{name: "a warm entry with a reason", mutate: func(e *conversationv1.VendorSpend) {
			e.ColdReason = conversationv1.VendorColdReason_VENDOR_COLD_REASON_UNSTATED
		}},
		{name: "a cold entry with no reason", mutate: func(e *conversationv1.VendorSpend) {
			e.CacheOutcome = conversationv1.VendorCacheOutcome_VENDOR_CACHE_OUTCOME_PARTIAL
		}},
		{name: "a subagent with no type", mutate: func(e *conversationv1.VendorSpend) {
			e.Agent = conversationv1.VendorCallAgent_VENDOR_CALL_AGENT_SUBAGENT
		}},
		{name: "a main loop with a type", mutate: func(e *conversationv1.VendorSpend) { e.SubagentType = "Explore" }},
		{name: "a bound the vocabulary does not name", mutate: func(e *conversationv1.VendorSpend) { e.ContextLeTokens = ptr(uint64(12_345)) }},
		{name: "no call", mutate: func(e *conversationv1.VendorSpend) { e.Calls = 0 }},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			e := valid()
			tt.mutate(e)
			// Act
			err := Validate(e)
			// Assert
			if tt.ok != (err == nil) {
				t.Fatalf("Validate = %v, want ok %v", err, tt.ok)
			}
			if err != nil && !errors.Is(err, ErrInvalid) {
				t.Errorf("the error %v does not wrap ErrInvalid", err)
			}
		})
	}
}

func TestTheCachePricesHaveOneDefinition(t *testing.T) {
	// Act
	sites := sourcescan.Sites(t, "PriceTenths  = ")
	// Assert: the run cap reads them through cachelapse's alias, which
	// names this package's constants, never a figure of its own.
	if len(sites) != 1 || sites[0] != "vendorspend.go" {
		t.Fatalf("the cache prices are defined in %v, want vendorspend.go alone", sites)
	}
}
