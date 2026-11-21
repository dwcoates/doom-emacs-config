package headless

import (
	"context"
	"errors"
	"path/filepath"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
)

// spendSink records every run's spend it is told.
type spendSink struct {
	configDirs []string
	entries    [][]*conversationv1.VendorSpend
}

func (s *spendSink) sink(configDir string, entries []*conversationv1.VendorSpend) {
	s.configDirs = append(s.configDirs, configDir)
	s.entries = append(s.entries, entries)
}

// assistantLine is one stream line of message ID on MODEL with USAGE.
func assistantLine(id, model, usage string) string {
	return `{"type":"assistant","message":{"id":"` + id + `","model":"` + model + `","usage":` + usage + `}}` + "\n"
}

const coldUsage = `{"input_tokens":3,"output_tokens":5,"cache_read_input_tokens":0,"cache_creation_input_tokens":2000,"cache_creation":{"ephemeral_1h_input_tokens":0,"ephemeral_5m_input_tokens":2000}}`

func scriptedSpend(t *testing.T, out string, runErr error) (*Client, *spendSink) {
	t.Helper()
	sink := &spendSink{}
	c, _ := scripted(t, permissiveGuard(t), "fake-claude", out, runErr)
	c.spend = sink.sink
	return c, sink
}

func TestARunsCallsAreCountedOncePerMessage(t *testing.T) {
	// Arrange: one call restated as it streamed, its output grown.
	out := assistantLine("m1", "claude-haiku-5-5", coldUsage) +
		assistantLine("m1", "claude-haiku-5-5", `{"input_tokens":3,"output_tokens":9,"cache_read_input_tokens":0,"cache_creation_input_tokens":2000}`) +
		okStream
	c, sink := scriptedSpend(t, out, nil)
	// Act
	if _, err := c.Run(context.Background(), Request{Kind: KindInference, Site: "classifier", ConfigDir: "/roots/multi", Prompt: "q"}); err != nil {
		t.Fatalf("Run: %v", err)
	}
	// Assert
	if len(sink.entries) != 1 || len(sink.entries[0]) != 1 || sink.configDirs[0] != "/roots/multi" {
		t.Fatalf("spend told = %v for %v, want one run's one entry billed to its root", sink.entries, sink.configDirs)
	}
	e := sink.entries[0][0]
	if e.GetCalls() != 1 || e.GetTokens().GetOutput() != 9 || e.GetTokens().GetCacheWrite_5M() != 2000 ||
		e.GetAgent() != conversationv1.VendorCallAgent_VENDOR_CALL_AGENT_HEADLESS ||
		e.GetCause() != conversationv1.VendorCallCause_VENDOR_CALL_CAUSE_HEADLESS_QUESTION ||
		e.GetCacheOutcome() != conversationv1.VendorCacheOutcome_VENDOR_CACHE_OUTCOME_COLD_REWRITE ||
		e.GetColdReason() != conversationv1.VendorColdReason_VENDOR_COLD_REASON_FIRST_CALL || e.GetContextLeTokens() != 25_000 {
		t.Fatalf("entry = %v, want one cold first call of a headless question, its output grown to 9", e)
	}
}

func TestARunsLaterColdCallStatesNoReason(t *testing.T) {
	// Arrange
	out := assistantLine("m1", "claude-sonnet-5-5", coldUsage) + assistantLine("m2", "claude-sonnet-5-5", coldUsage) + okStream
	c, sink := scriptedSpend(t, out, nil)
	// Act
	if _, err := c.Run(context.Background(), Request{Kind: KindToolCalls, Site: "news_digest_ipo", Tools: []string{"WebSearch"}, Prompt: "q"}); err != nil {
		t.Fatalf("Run: %v", err)
	}
	// Assert
	var reasons []conversationv1.VendorColdReason
	for _, e := range sink.entries[0] {
		reasons = append(reasons, e.GetColdReason())
	}
	if len(reasons) != 2 || reasons[0] != conversationv1.VendorColdReason_VENDOR_COLD_REASON_FIRST_CALL || reasons[1] != conversationv1.VendorColdReason_VENDOR_COLD_REASON_UNSTATED {
		t.Fatalf("reasons = %v, want the first call's and then unstated", reasons)
	}
}

func TestAFailedRunsCallsAreCountedToo(t *testing.T) {
	// Arrange
	out := assistantLine("m1", "claude-haiku-5-5", coldUsage) + `{"type":"result","subtype":"error_during_execution","is_error":true,"result":"overloaded"}`
	c, sink := scriptedSpend(t, out, errors.New("exit status 1"))
	// Act
	_, err := c.Run(context.Background(), Request{Kind: KindInference, Site: "classifier", Prompt: "q"})
	// Assert
	if err == nil || len(sink.entries) != 1 {
		t.Fatalf("Run = %v and spend told %d times; want the failure and its call counted", err, len(sink.entries))
	}
}

func TestARunStatingNoCallTellsNoSpend(t *testing.T) {
	tests := []struct {
		name string
		out  string
	}{
		{name: "a result alone", out: okStream},
		{name: "the CLI's own message", out: assistantLine("m1", syntheticModel, coldUsage) + okStream},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			c, sink := scriptedSpend(t, tt.out, nil)
			// Act
			if _, err := c.Run(context.Background(), Request{Kind: KindInference, Site: "classifier", Prompt: "q"}); err != nil {
				t.Fatalf("Run: %v", err)
			}
			// Assert
			if len(sink.entries) != 0 {
				t.Fatalf("spend told %v, want none", sink.entries)
			}
		})
	}
}

func TestEffectiveConfigDir(t *testing.T) {
	tests := []struct {
		name, named, env, home, want string
	}{
		{name: "the run's own root", named: "/roots/multi", env: "/env/root", want: "/roots/multi"},
		{name: "the environment's root", env: "/env/root", want: "/env/root"},
		{name: "the CLI's default under the home", home: "/home/u", want: filepath.Join("/home/u", ".claude")},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			t.Setenv("CLAUDE_CONFIG_DIR", tt.env)
			if tt.home != "" {
				t.Setenv("HOME", tt.home)
			}
			// Act
			got := effectiveConfigDir(tt.named)
			// Assert
			if got != tt.want {
				t.Fatalf("effectiveConfigDir(%q) = %q, want %q", tt.named, got, tt.want)
			}
		})
	}
}

func TestNewPanicsWithNoSpendSink(t *testing.T) {
	defer func() {
		if recover() == nil {
			t.Fatal("New with no spend sink did not panic")
		}
	}()
	New(permissiveGuard(t), "fake-claude", nil)
}
