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
func readStream(stdout string) (*envelope, []vendorspend.Call, error) {
	var result *envelope
	var calls []vendorspend.Call
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
			c := vendorspend.Call{Model: l.Message.Model, Tokens: tokensOf(l.Message.Usage)}
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
func larger(a, b vendorspend.Call) vendorspend.Call {
	return vendorspend.Call{Model: a.Model, Tokens: &conversationv1.VendorTokens{
		UncachedInput: max(a.Tokens.GetUncachedInput(), b.Tokens.GetUncachedInput()),
		CacheRead:     max(a.Tokens.GetCacheRead(), b.Tokens.GetCacheRead()),
		CacheWrite_5M: max(a.Tokens.GetCacheWrite_5M(), b.Tokens.GetCacheWrite_5M()),
		CacheWrite_1H: max(a.Tokens.GetCacheWrite_1H(), b.Tokens.GetCacheWrite_1H()),
		Output:        max(a.Tokens.GetOutput(), b.Tokens.GetOutput()),
	}}
}
