package server

import (
	"errors"
	"strings"
	"testing"
)

// stubBeforeActionSource is a MergeBeforeActionSource that answers from a map,
// modeling the creation records the real bridge reads.
type stubBeforeActionSource struct {
	prompts map[string]string
	err     error
}

func (s stubBeforeActionSource) BeforeWSMergePrompt(ws string) (string, error) {
	if s.err != nil {
		return "", s.err
	}
	return s.prompts[ws], nil
}

func TestMergeBeforeActionsReadsTheCreationRecord(t *testing.T) {
	// Arrange — a workspace created with a before_ws_merge action.
	source := mergeBeforeActions{creation: stubBeforeActionSource{prompts: map[string]string{"/ws/a": "run the gate"}}}

	// Act.
	got, err := source.BeforeAction("/ws/a")

	// Assert.
	if err != nil {
		t.Fatalf("BeforeAction() error = %v", err)
	}
	if got != "run the gate" {
		t.Fatalf("BeforeAction() = %q, want the recorded creation-time action", got)
	}
}

func TestMergeBeforeActionsReportsNoneForAWorkspaceWithoutOne(t *testing.T) {
	// Arrange — a workspace the creation records know nothing about.
	source := mergeBeforeActions{creation: stubBeforeActionSource{prompts: map[string]string{}}}

	// Act.
	got, err := source.BeforeAction("/ws/handmade")

	// Assert — the common case passes straight through rather than failing.
	if err != nil {
		t.Fatalf("BeforeAction() error = %v, want no error for a workspace with no action", err)
	}
	if got != "" {
		t.Fatalf("BeforeAction() = %q, want none", got)
	}
}

func TestMergeBeforeActionsSurfacesAnUnreadableRecord(t *testing.T) {
	// Arrange — records that cannot be read at all.
	boom := errors.New("records unreadable")
	source := mergeBeforeActions{creation: stubBeforeActionSource{err: boom}}

	// Act.
	_, err := source.BeforeAction("/ws/a")

	// Assert — surfaced, never collapsed into "this workspace has none".
	if !errors.Is(err, boom) {
		t.Fatalf("BeforeAction() error = %v, want the read failure surfaced", err)
	}
}

func TestMergeBeforeActionsRefusesAnUnwiredCreationSource(t *testing.T) {
	// Arrange — no creation source bound at all.
	source := mergeBeforeActions{}

	// Act.
	_, err := source.BeforeAction("/ws/a")

	// Assert — a loud refusal, because "no source" is not "no action".
	if err == nil || !strings.Contains(err.Error(), "workspace-creation source") {
		t.Fatalf("BeforeAction() error = %v, want a refusal naming the missing creation source", err)
	}
}
