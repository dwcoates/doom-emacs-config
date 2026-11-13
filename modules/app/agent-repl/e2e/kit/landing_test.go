package kit

import (
	"strings"
	"testing"
)

// landingStateOf composes a state string the way `landingStateForm` does, so
// these cases cannot drift from the field names the elisp writes.
func landingStateOf(ws, current, landing, pending, window, webview string) string {
	return strings.Join([]string{
		"ws=" + ws,
		"current=" + current,
		"landing=" + landing,
		"pending=" + pending,
		"window=" + window,
		"webview=" + webview,
	}, LandingStateSeparator)
}

func TestLandingSettled(t *testing.T) {
	t.Parallel()
	tests := []struct {
		name  string
		state string
		want  bool
	}{
		{
			name:  "a landed workspace is settled",
			state: landingStateOf("harbor-lantern", "harbor-lantern", "nil", "nil", "t", "*agent-repl web: harbor-lantern*"),
			want:  true,
		},
		{
			name:  "a directory with no workspace at it is not settled",
			state: landingStateOf("nil", "nil", "nil", "nil", "nil", "nil"),
			want:  false,
		},
		{
			name:  "a minted ref still waiting for its tab is not settled",
			state: landingStateOf("harbor-lantern", "harbor-lantern", "t", "nil", "t", "*agent-repl web: harbor-lantern*"),
			want:  false,
		},
		{
			name:  "a workspace the landing has not stood on yet is not settled",
			state: landingStateOf("harbor-lantern", "quay-signal", "nil", "nil", "t", "*agent-repl web: harbor-lantern*"),
			want:  false,
		},
		{
			name:  "a show still armed is not settled",
			state: landingStateOf("harbor-lantern", "harbor-lantern", "nil", "t", "t", "*agent-repl web: harbor-lantern*"),
			want:  false,
		},
		{
			name:  "a panel with no window on the frame is not settled",
			state: landingStateOf("harbor-lantern", "harbor-lantern", "nil", "nil", "nil", "nil"),
			want:  false,
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			if got := LandingSettled(tc.state); got != tc.want {
				t.Fatalf("LandingSettled(%q) = %v, want %v", tc.state, got, tc.want)
			}
		})
	}
}

// TestPlaytestLandingStateKeepsBufferNamesWhole is the reason the state
// string is separated by ` | ` and not by a space: one of its fields is a
// BUFFER NAME, and Emacs writes spaces into those. Split on spaces and the
// tail of a buffer name becomes the next field's value, which is a predicate
// that answers about the wrong fact.
func TestLandingStateKeepsBufferNamesWhole(t *testing.T) {
	t.Parallel()
	state := landingStateOf("harbor-lantern", "harbor-lantern", "nil", "nil", "t", "*agent-repl web: harbor-lantern*")

	fields := ParseLandingState(state)

	if got, want := fields["webview"], "*agent-repl web: harbor-lantern*"; got != want {
		t.Fatalf("the webview field parsed as %q, want %q", got, want)
	}
	if got, want := fields["window"], "t"; got != want {
		t.Fatalf("the window field parsed as %q, want %q", got, want)
	}
}
