package recorded_test

import (
	"testing"

	"agentrepl/shim-claude-sidecar/internal/recorded"
)

func TestExpand(t *testing.T) {
	tests := []struct {
		name string
		text string
		want string
	}{
		{name: "the token becomes the home", text: `{"cwd":"${HOME}/p"}`, want: `{"cwd":"/Users/bo/p"}`},
		{name: "the token's slug becomes the home's slug", text: "projects/--HOME---p/s.jsonl", want: "projects/-Users-bo--p/s.jsonl"},
		{name: "text with no token is unchanged", text: "/private/tmp/x", want: "/private/tmp/x"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Act.
			got := recorded.Expand(tc.text, "/Users/bo")

			// Assert.
			if got != tc.want {
				t.Fatalf("Expand(%q) = %q, want %q", tc.text, got, tc.want)
			}
		})
	}
}
