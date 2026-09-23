package sessionwatcher

import (
	"errors"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
)

// TestOpeningStatesWhetherItReplays covers the three openings and the refused
// zero value: only the open and the select replay history.
func TestOpeningStatesWhetherItReplays(t *testing.T) {
	tests := []struct {
		name        string
		opening     Opening
		wantReplays bool
		wantName    string
		wantErr     error
	}{
		{name: "a workspace's opening replays", opening: WorkspaceOpened(), wantReplays: true, wantName: "workspace_opened"},
		{name: "a transcript selection replays", opening: TranscriptSelected(), wantReplays: true, wantName: "transcript_selected"},
		{name: "a resume does not replay", opening: ResumeFrom(Pointers{}), wantName: "resumed"},
		{name: "the zero opening is undecided and refused", opening: Opening{}, wantName: "undecided", wantErr: errUndecidedOpening},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange in the table. Act.
			replays, name, err := tt.opening.Replays(), tt.opening.String(), tt.opening.validate()

			// Assert.
			if replays != tt.wantReplays {
				t.Fatalf("Replays() = %v, want %v", replays, tt.wantReplays)
			}
			if name != tt.wantName {
				t.Fatalf("String() = %q, want %q", name, tt.wantName)
			}
			if !errors.Is(err, tt.wantErr) {
				t.Fatalf("validate() = %v, want %v", err, tt.wantErr)
			}
		})
	}
}

// TestResumeFromHoldsItsOwnCopy covers the aliasing the opening must not have:
// a caller writing to the map it handed over after the fact must not reach
// the pointers a watcher resumes from.
func TestResumeFromHoldsItsOwnCopy(t *testing.T) {
	// Arrange.
	agents := map[string]*conversationv1.HistoryPointer{"sub-1": {Value: "ptr-sub-1"}}
	opening := ResumeFrom(Pointers{Agents: agents})

	// Act.
	agents["sub-1"] = &conversationv1.HistoryPointer{Value: "ptr-later"}

	// Assert.
	if got := opening.From().Agents["sub-1"].GetValue(); got != "ptr-sub-1" {
		t.Fatalf("resumed pointer = %q, want the one handed over, ptr-sub-1", got)
	}
}
