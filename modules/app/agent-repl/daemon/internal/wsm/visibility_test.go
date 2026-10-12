package wsm

import (
	"testing"
	"time"
)

func TestWithheld(t *testing.T) {
	merged := time.Unix(100, 0)
	cases := []struct {
		name       string
		ws         Workspace
		repoFolded bool
		want       Withholding
	}{
		{name: "an ordinary workspace is drawn", ws: Workspace{}, want: Drawn},
		{name: "a hidden workspace is withheld hidden", ws: Workspace{Hidden: true}, want: WithheldHidden},
		{name: "a workspace in a collapsed section is withheld collapsed", ws: Workspace{}, repoFolded: true, want: WithheldCollapsed},
		{name: "a hidden workspace in a collapsed section is withheld hidden", ws: Workspace{Hidden: true}, repoFolded: true, want: WithheldHidden},
		{name: "a merged workspace ignores its hidden flag", ws: Workspace{Hidden: true, MergedAt: &merged}, want: Drawn},
		{name: "a merged workspace follows its repository's fold", ws: Workspace{MergedAt: &merged}, repoFolded: true, want: WithheldCollapsed},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange / Act
			got := tc.ws.Withheld(tc.repoFolded)

			// Assert
			if got != tc.want {
				t.Fatalf("Withheld = %v, want %v", got, tc.want)
			}
		})
	}
}

func TestRuns(t *testing.T) {
	of := MergeOrigin{}
	cases := []struct {
		name       string
		ws         Workspace
		repoFolded bool
		want       bool
	}{
		{name: "a drawn workspace runs", ws: Workspace{}, want: true},
		{name: "a hidden workspace does not run", ws: Workspace{Hidden: true}, want: false},
		{name: "a collapsed workspace does not run", ws: Workspace{}, repoFolded: true, want: false},
		{name: "a hidden merge workspace runs in a collapsed section", ws: Workspace{MergeOf: &of}, repoFolded: true, want: true},
		{name: "a revealed merge workspace the user hid does not run", ws: Workspace{MergeOf: &of, MergeRevealed: true, Hidden: true}, want: false},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange / Act
			got := tc.ws.Runs(tc.repoFolded)

			// Assert
			if got != tc.want {
				t.Fatalf("Runs = %v, want %v", got, tc.want)
			}
		})
	}
}
