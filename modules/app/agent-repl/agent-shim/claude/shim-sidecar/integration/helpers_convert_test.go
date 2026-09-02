package integration

import (
	"testing"

	storev1 "agentrepl/proto/store/v1"
)

// helpers_convert_test.go — shared helpers for the CONVERSION-side subjects of
// the sidecar audit's remainder. They live in their own file so the settled
// helpers_test.go is neither reordered nor reformatted by this work.

// setToolUseInput replaces the sole tool_use block's `input` object, so a real
// assistant record can be re-pointed at a different tool's arguments without
// inventing a record shape around it.
func setToolUseInput(t *testing.T, obj map[string]any, input map[string]any) map[string]any {
	t.Helper()
	msg, ok := obj["message"].(map[string]any)
	if !ok {
		t.Fatalf("record carries no message object: %v", obj)
	}
	blocks, ok := msg["content"].([]any)
	if !ok || len(blocks) == 0 {
		t.Fatalf("record's message carries no content blocks: %v", msg)
	}
	newBlocks := make([]any, 0, len(blocks))
	var found bool
	for _, raw := range blocks {
		b, ok := raw.(map[string]any)
		if !ok {
			newBlocks = append(newBlocks, raw)
			continue
		}
		nb := make(map[string]any, len(b))
		for k, v := range b {
			nb[k] = v
		}
		if b["type"] == "tool_use" {
			nb["input"] = input
			found = true
		}
		newBlocks = append(newBlocks, nb)
	}
	if !found {
		t.Fatalf("record carries no tool_use block to re-point: %v", msg)
	}
	newMsg := make(map[string]any, len(msg))
	for k, v := range msg {
		newMsg[k] = v
	}
	newMsg["content"] = newBlocks
	return withFields(t, obj, map[string]any{"message": newMsg})
}

// corpusToolUseInput answers the `input` object of a tool-inputs fixture, so a
// call built from a captured assistant line carries the REAL arguments the
// vendor writes for that tool rather than the ones it was cloned from.
func corpusToolUseInput(t *testing.T, rel string) map[string]any {
	t.Helper()
	block := corpusRecord(t, rel, 0)
	input, ok := block["input"].(map[string]any)
	if !ok {
		t.Fatalf("corpus tool input %s carries no input object: %v", rel, block)
	}
	return input
}

// unitEntries answers every entry written under one unit's activity key.
func unitEntries(entries []*storev1.StoreEntry, unit string) []*storev1.StoreEntry {
	var out []*storev1.StoreEntry
	for _, e := range entries {
		if e.GetUpsertKey() == "activity:"+unit {
			out = append(out, e)
		}
	}
	return out
}

// upsertKeysOf lists every key a batch stream carried, for a failure message.
func upsertKeysOf(entries []*storev1.StoreEntry) []string {
	out := make([]string, 0, len(entries))
	for _, e := range entries {
		out = append(out, e.GetUpsertKey())
	}
	return sortedStrings(out)
}
