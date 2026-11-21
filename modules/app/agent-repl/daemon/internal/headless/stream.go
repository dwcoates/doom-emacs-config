package headless

import (
	"encoding/json"
	"fmt"
	"strings"

	"agent-repld/internal/vendorspend"
	conversationv1 "agentrepl/proto/conversation/v1"
)

// EVERY RUN IS READ AS THE CLI'S EVENT STREAM (`--output-format stream-json
// --verbose`): one JSON object per line, the vendor's every assistant message
// with its own usage, and the result envelope last. The stream is the one
// shape that states each vendor call's usage, which the spend series count
// (vendorspend), and its result envelope is the answer a run returns.

// streamFormat is the one output format every run asks the CLI for.
const streamFormat = "stream-json"

// syntheticModel is the model the CLI names a message it wrote itself with
// (conversation.v1.ModelMarker SYNTHETIC): no vendor call.
const syntheticModel = "<synthetic>"

// streamLine is the slice of one stream line this package reads.
type streamLine struct {
	Type    string `json:"type"`
	Message *struct {
		ID    string       `json:"id"`
		Model string       `json:"model"`
		Usage *streamUsage `json:"usage"`
	} `json:"message"`
	Result  string `json:"result"`
	IsError bool   `json:"is_error"`
	Subtype string `json:"subtype"`
}

// streamUsage is one assistant message's usage.
type streamUsage struct {
	InputTokens   uint64 `json:"input_tokens"`
	OutputTokens  uint64 `json:"output_tokens"`
	CacheRead     uint64 `json:"cache_read_input_tokens"`
	CacheCreation uint64 `json:"cache_creation_input_tokens"`
	Split         *struct {
		OneHour uint64 `json:"ephemeral_1h_input_tokens"`
	} `json:"cache_creation"`
}

// call is one vendor call's usage, by message id.
type call struct {
	model  string
	tokens *conversationv1.VendorTokens
}

// readStream reads a run's stdout: the result envelope, when the stream
// carried one, and every vendor call it states, each counted once (one per
// message id, its largest restatement). A line that does not parse is a
// broken stream, answered as an error with whatever was read before it.
func readStream(stdout string) (*envelope, []call, error) {
	var result *envelope
	var calls []call
	byID := map[string]int{}
	for n, line := range strings.Split(stdout, "\n") {
		line = strings.TrimSpace(line)
		if line == "" {
			continue
		}
		var l streamLine
		if err := json.Unmarshal([]byte(line), &l); err != nil {
			return result, calls, fmt.Errorf("stream line %d did not parse: %w", n+1, err)
		}
		switch l.Type {
		case "result":
			result = &envelope{Result: l.Result, IsError: l.IsError, Subtype: l.Subtype}
		case "assistant":
			if l.Message == nil || l.Message.Usage == nil || l.Message.ID == "" || l.Message.Model == syntheticModel {
				continue
			}
			c := call{model: l.Message.Model, tokens: tokensOf(l.Message.Usage)}
			if i, seen := byID[l.Message.ID]; seen {
				calls[i] = larger(calls[i], c)
				continue
			}
			byID[l.Message.ID] = len(calls)
			calls = append(calls, c)
		}
	}
	return result, calls, nil
}

// tokensOf answers a usage's tokens by kind: a write with no tier split is
// the five-minute tier's, as the shim reads it.
func tokensOf(u *streamUsage) *conversationv1.VendorTokens {
	var oneHour uint64
	if u.Split != nil {
		oneHour = u.Split.OneHour
	}
	fiveMin := uint64(0)
	if u.CacheCreation > oneHour {
		fiveMin = u.CacheCreation - oneHour
	}
	return &conversationv1.VendorTokens{UncachedInput: u.InputTokens, CacheRead: u.CacheRead, CacheWrite_5M: fiveMin, CacheWrite_1H: oneHour, Output: u.OutputTokens}
}

// larger is a call's restatement: each figure the larger of the two.
func larger(a, b call) call {
	pick := func(x, y uint64) uint64 { return max(x, y) }
	return call{model: a.model, tokens: &conversationv1.VendorTokens{
		UncachedInput: pick(a.tokens.GetUncachedInput(), b.tokens.GetUncachedInput()),
		CacheRead:     pick(a.tokens.GetCacheRead(), b.tokens.GetCacheRead()),
		CacheWrite_5M: pick(a.tokens.GetCacheWrite_5M(), b.tokens.GetCacheWrite_5M()),
		CacheWrite_1H: pick(a.tokens.GetCacheWrite_1H(), b.tokens.GetCacheWrite_1H()),
		Output:        pick(a.tokens.GetOutput(), b.tokens.GetOutput()),
	}}
}

// spendOf tallies a run's calls as spend entries under CAUSE: the run's first
// call is its agent's first, and a later one the cache did not serve is
// filed with no reason this package can name.
func spendOf(calls []call, cause conversationv1.VendorCallCause) []*conversationv1.VendorSpend {
	entries := map[string]*conversationv1.VendorSpend{}
	var order []string
	for i, c := range calls {
		t := c.tokens
		outcome := vendorspend.OutcomeOf(t.GetCacheRead(), t.GetCacheWrite_5M()+t.GetCacheWrite_1H(), t.GetUncachedInput())
		reason := conversationv1.VendorColdReason_VENDOR_COLD_REASON_UNSPECIFIED
		if outcome != conversationv1.VendorCacheOutcome_VENDOR_CACHE_OUTCOME_WARM {
			reason = conversationv1.VendorColdReason_VENDOR_COLD_REASON_UNSTATED
			if i == 0 {
				reason = conversationv1.VendorColdReason_VENDOR_COLD_REASON_FIRST_CALL
			}
		}
		bound, inBound := vendorspend.BucketOf(vendorspend.InputOf(t))
		key := fmt.Sprintf("%s|%d|%d|%d|%v", c.model, outcome, reason, bound, inBound)
		e := entries[key]
		if e == nil {
			e = &conversationv1.VendorSpend{
				Model: c.model, Agent: conversationv1.VendorCallAgent_VENDOR_CALL_AGENT_HEADLESS, Cause: cause,
				CacheOutcome: outcome, ColdReason: reason, Tokens: &conversationv1.VendorTokens{},
			}
			if inBound {
				e.ContextLeTokens = &bound
			}
			entries[key] = e
			order = append(order, key)
		}
		e.Calls++
		e.Tokens.UncachedInput += t.GetUncachedInput()
		e.Tokens.CacheRead += t.GetCacheRead()
		e.Tokens.CacheWrite_5M += t.GetCacheWrite_5M()
		e.Tokens.CacheWrite_1H += t.GetCacheWrite_1H()
		e.Tokens.Output += t.GetOutput()
	}
	out := make([]*conversationv1.VendorSpend, len(order))
	for i, key := range order {
		out[i] = entries[key]
	}
	return out
}
