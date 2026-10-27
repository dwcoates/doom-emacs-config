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
		{name: "a reopened merged workspace the user hid is withheld hidden", ws: Workspace{Hidden: true, MergedAt: &merged}, want: WithheldHidden},
		{name: "a settled merge ignores its hidden flag", ws: Workspace{Hidden: true, Closed: true, MergedAt: &merged}, want: Drawn},
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

func TestInHiddenSection(t *testing.T) {
	merged := time.Unix(100, 0)
	cases := []struct {
		name string
		ws   Workspace
		want bool
	}{
		{name: "an ordinary workspace is not", ws: Workspace{}, want: false},
		{name: "a hidden workspace is", ws: Workspace{Hidden: true}, want: true},
		{name: "a reopened merged hidden workspace is", ws: Workspace{Hidden: true, MergedAt: &merged}, want: true},
		{name: "a settled merge that was hidden is not", ws: Workspace{Hidden: true, Closed: true, MergedAt: &merged}, want: false},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got := tc.ws.InHiddenSection()

			// Assert
			if got != tc.want {
				t.Fatalf("InHiddenSection = %v, want %v", got, tc.want)
			}
		})
	}
}

func TestLetsCacheLapse(t *testing.T) {
	merged := time.Unix(100, 0)
	cases := []struct {
		name string
		ws   Workspace
		want bool
	}{
		{name: "an open visible workspace does not", ws: Workspace{}, want: false},
		{name: "a hidden workspace does", ws: Workspace{Hidden: true}, want: true},
		{name: "a closed workspace does", ws: Workspace{Closed: true}, want: true},
		{name: "a closed hidden workspace does", ws: Workspace{Hidden: true, Closed: true}, want: true},
		{name: "a reopened merged hidden workspace does", ws: Workspace{Hidden: true, MergedAt: &merged}, want: true},
		{name: "a merged closed workspace does", ws: Workspace{Closed: true, MergedAt: &merged}, want: true},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got := tc.ws.LetsCacheLapse()

			// Assert
			if got != tc.want {
				t.Fatalf("LetsCacheLapse = %v, want %v", got, tc.want)
			}
		})
	}
}

// TestTheHiddenSectionIsOneStatement pins that Withheld's hidden arm IS
// InHiddenSection: every workspace it calls hidden, and no other, is in the
// hidden section, so the tab bar, the start path and the cache keeper cannot
// disagree about which workspaces are hidden.
func TestTheHiddenSectionIsOneStatement(t *testing.T) {
	merged := time.Unix(100, 0)
	for _, ws := range []Workspace{{}, {Hidden: true}, {MergedAt: &merged}, {Hidden: true, MergedAt: &merged}} {
		for _, folded := range []bool{false, true} {
			// Act
			withheldHidden := ws.Withheld(folded) == WithheldHidden

			// Assert
			if withheldHidden != ws.InHiddenSection() {
				t.Fatalf("Withheld(%v) of %+v = %v, InHiddenSection = %v", folded, ws, ws.Withheld(folded), ws.InHiddenSection())
			}
		}
	}
}

func TestRuns(t *testing.T) {
	of := MergeOrigin{}
	cases := []struct {
		name        string
		ws          Workspace
		repoFolded  bool
		mergeQueued bool
		want        bool
	}{
		{name: "a drawn workspace runs", ws: Workspace{}, want: true},
		{name: "a hidden workspace does not run", ws: Workspace{Hidden: true}, want: false},
		{name: "a collapsed workspace does not run", ws: Workspace{}, repoFolded: true, want: false},
		{name: "a hidden merge workspace runs in a collapsed section", ws: Workspace{MergeOf: &of}, repoFolded: true, mergeQueued: true, want: true},
		{name: "a hidden merge workspace whose merge left every queue does not run", ws: Workspace{MergeOf: &of}, want: false},
		{name: "a hidden merge workspace whose merge left every queue runs while the user looks at it", ws: Workspace{MergeOf: &of, Current: true}, want: true},
		{name: "a revealed merge workspace the user hid does not run", ws: Workspace{MergeOf: &of, MergeRevealed: true, Hidden: true}, want: false},
		{name: "a hidden workspace the user is looking at runs", ws: Workspace{Hidden: true, Current: true}, want: true},
		{name: "a collapsed workspace the user is looking at runs", ws: Workspace{Current: true}, repoFolded: true, want: true},
		{name: "a hidden workspace with a merge queued runs", ws: Workspace{Hidden: true}, mergeQueued: true, want: true},
		{name: "a closed workspace with a merge queued runs", ws: Workspace{Closed: true}, mergeQueued: true, want: true},
		{name: "a collapsed workspace with a merge queued runs", ws: Workspace{}, repoFolded: true, mergeQueued: true, want: true},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange / Act
			got := tc.ws.Runs(tc.repoFolded, tc.mergeQueued)

			// Assert
			if got != tc.want {
				t.Fatalf("Runs = %v, want %v", got, tc.want)
			}
		})
	}
}

func TestKeepaliveHeld(t *testing.T) {
	merged := time.Unix(100, 0)
	cases := []struct {
		name string
		ws   Workspace
		want bool
	}{
		{name: "an ordinary workspace keeps its cache warm", ws: Workspace{}, want: false},
		{name: "a hidden workspace lets its cache lapse", ws: Workspace{Hidden: true}, want: true},
		{name: "a hidden workspace the user is looking at keeps its cache warm", ws: Workspace{Hidden: true, Current: true}, want: false},
		{name: "a reopened merged hidden workspace lets its cache lapse", ws: Workspace{Hidden: true, MergedAt: &merged}, want: true},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got := tc.ws.KeepaliveHeld()

			// Assert
			if got != tc.want {
				t.Fatalf("KeepaliveHeld = %v, want %v", got, tc.want)
			}
		})
	}
}

// TestAHeldCacheNeverRuns pins that a workspace whose cache is left to lapse
// is one the daemon does not run, so the keeper and the stand-down it asks for
// read the same verdict.
func TestAHeldCacheNeverRuns(t *testing.T) {
	merged := time.Unix(100, 0)
	for _, ws := range []Workspace{{}, {Hidden: true}, {Hidden: true, Current: true}, {Current: true}, {Hidden: true, MergedAt: &merged}} {
		for _, folded := range []bool{false, true} {
			// Act / Assert
			if ws.KeepaliveHeld() && ws.Runs(folded, false) {
				t.Fatalf("%+v (folded %v) holds its cache yet runs", ws, folded)
			}
		}
	}
}

func TestLetsCacheLapseOfASelectedWorkspace(t *testing.T) {
	cases := []struct {
		name string
		ws   Workspace
		want bool
	}{
		{name: "a selected hidden workspace keeps its cache warm", ws: Workspace{Hidden: true, Current: true}, want: false},
		{name: "a selected closed workspace lets its cache lapse", ws: Workspace{Closed: true, Current: true}, want: true},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got := tc.ws.LetsCacheLapse()

			// Assert
			if got != tc.want {
				t.Fatalf("LetsCacheLapse = %v, want %v", got, tc.want)
			}
		})
	}
}
