package e2e

import (
	"testing"

	storev1 "agentrepl/proto/store/v1"
)

func TestCursorIngested(t *testing.T) {
	tests := []struct {
		name    string
		cursors []*storev1.CursorState
		want    bool
	}{
		{name: "a cursor at the file's size has ingested it",
			cursors: []*storev1.CursorState{{Path: "/p/agent-1.jsonl", Offset: 40}}, want: true},
		{name: "a cursor past the file's size has ingested it",
			cursors: []*storev1.CursorState{{Path: "/p/agent-1.jsonl", Offset: 41}}, want: true},
		{name: "a cursor short of the file's size has not",
			cursors: []*storev1.CursorState{{Path: "/p/agent-1.jsonl", Offset: 39}}},
		{name: "no cursor for the file has not",
			cursors: nil},
		{name: "another file's cursor at the size does not count",
			cursors: []*storev1.CursorState{{Path: "/p/main.jsonl", Offset: 40}}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act.
			got := cursorIngested(tt.cursors, "/p/agent-1.jsonl", 40)

			// Assert.
			if got != tt.want {
				t.Fatalf("cursorIngested = %v, want %v", got, tt.want)
			}
		})
	}
}
