package heldingress

import (
	"strings"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
)

const validSaid = `{"content":{"blocks":[{"text":{"text":"hello"}}]}}`

// entryJSON builds an entry document with one field overridden by a raw
// JSON fragment ("" leaves the field out).
func entryJSON(overrides map[string]string) string {
	fields := []struct{ name, value string }{
		{"version", "1"},
		{"project_dir", `"/work/one"`},
		{"idempotency_key", `"k-1"`},
		{"origin", `"PROMPT_ORIGIN_USER_SENT"`},
		{"said", validSaid},
		{"queued_at", `"2026-09-28T12:00:00Z"`},
	}
	var parts []string
	for _, f := range fields {
		value := f.value
		if o, ok := overrides[f.name]; ok {
			value = o
		}
		if value == "" {
			continue
		}
		parts = append(parts, `"`+f.name+`":`+value)
	}
	for name, value := range overrides {
		known := false
		for _, f := range fields {
			known = known || f.name == name
		}
		if !known {
			parts = append(parts, `"`+name+`":`+value)
		}
	}
	return "{" + strings.Join(parts, ",") + "}"
}

func TestParseDecodesAWellFormedEntry(t *testing.T) {
	// Arrange
	data := []byte(entryJSON(nil))

	// Act
	got, err := parse(data)

	// Assert
	if err != nil {
		t.Fatalf("parse = %v, want a decoded entry", err)
	}
	if got.origin != conversationv1.PromptOrigin_PROMPT_ORIGIN_USER_SENT {
		t.Fatalf("origin = %v, want PROMPT_ORIGIN_USER_SENT", got.origin)
	}
	if text := got.said.GetContent().GetBlocks()[0].GetText().GetText(); text != "hello" {
		t.Fatalf("said text = %q, want hello", text)
	}
	if got.IdempotencyKey != "k-1" || got.ProjectDir != "/work/one" {
		t.Fatalf("entry = %+v, want the written key and directory", got.Entry)
	}
}

func TestParseRefusesAnEntryItCouldOnlyActOnByGuessing(t *testing.T) {
	tests := []struct {
		name      string
		body      string
		wantCause string
	}{
		{name: "another format version", body: entryJSON(map[string]string{"version": "2"}), wantCause: "version 2"},
		{name: "a relative project dir", body: entryJSON(map[string]string{"project_dir": `"work/one"`}), wantCause: "absolute"},
		{name: "no project dir", body: entryJSON(map[string]string{"project_dir": ""}), wantCause: "absolute"},
		{name: "no idempotency key", body: entryJSON(map[string]string{"idempotency_key": ""}), wantCause: "idempotency_key"},
		{name: "no said", body: entryJSON(map[string]string{"said": ""}), wantCause: "said is required"},
		{name: "an unknown origin", body: entryJSON(map[string]string{"origin": `"PROMPT_ORIGIN_NOPE"`}), wantCause: "not a prompt origin"},
		{name: "the unspecified origin", body: entryJSON(map[string]string{"origin": `"PROMPT_ORIGIN_UNSPECIFIED"`}), wantCause: "not a prompt origin"},
		{name: "a said with no blocks", body: entryJSON(map[string]string{"said": `{"content":{"blocks":[]}}`}), wantCause: "no content blocks"},
		{name: "a said that is not a UserSaid", body: entryJSON(map[string]string{"said": `{"words":"hi"}`}), wantCause: "decode said"},
		{name: "an unknown field", body: entryJSON(map[string]string{"priority": `"high"`}), wantCause: "unknown field"},
		{name: "trailing data", body: entryJSON(nil) + `{}`, wantCause: "trailing data"},
		{name: "a truncated document", body: entryJSON(nil)[:20], wantCause: "decode the entry"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			_, err := parse([]byte(tc.body))

			// Assert
			if err == nil || !strings.Contains(err.Error(), tc.wantCause) {
				t.Fatalf("parse(%s) = %v, want a refusal naming %q", tc.body, err, tc.wantCause)
			}
		})
	}
}
