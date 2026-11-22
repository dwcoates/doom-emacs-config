package wsm

import (
	"io/fs"
	"os"
	"path/filepath"
	"regexp"
	"strings"
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
		{name: "the selection in a collapsed section is withheld collapsed by the sections alone", ws: Workspace{Current: true}, repoFolded: true, want: WithheldCollapsed},
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

func TestMergeSettled(t *testing.T) {
	merged := time.Unix(100, 0)
	cases := []struct {
		name string
		ws   Workspace
		want bool
	}{
		{name: "an open workspace has not", ws: Workspace{}, want: false},
		{name: "a closed workspace that never merged has not", ws: Workspace{Closed: true}, want: false},
		{name: "a reopened merged workspace has not", ws: Workspace{MergedAt: &merged}, want: false},
		{name: "a closed merged workspace has", ws: Workspace{Closed: true, MergedAt: &merged}, want: true},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got := tc.ws.MergeSettled()

			// Assert
			if got != tc.want {
				t.Fatalf("MergeSettled = %v, want %v", got, tc.want)
			}
		})
	}
}

func TestInMergedBand(t *testing.T) {
	merged := time.Unix(100, 0)
	cases := []struct {
		name string
		ws   Workspace
		want bool
	}{
		{name: "a settled merge is", ws: Workspace{Closed: true, MergedAt: &merged}, want: true},
		{name: "a reopened merged workspace is not", ws: Workspace{MergedAt: &merged}, want: false},
		{name: "a closed workspace that never merged is not", ws: Workspace{Closed: true}, want: false},
		{name: "a settled chat thread is not", ws: Workspace{Kind: WorkspaceKindThread, Closed: true, MergedAt: &merged}, want: false},
		{name: "a landed merge workspace is", ws: Workspace{MergeOf: &MergeOrigin{Requester: "requester"}, Closed: true, MergedAt: &merged}, want: true},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got := tc.ws.InMergedBand()

			// Assert
			if got != tc.want {
				t.Fatalf("InMergedBand = %v, want %v", got, tc.want)
			}
		})
	}
}

func TestNoSourceSpellsASettledMergeByHand(t *testing.T) {
	// Arrange
	settled := regexp.MustCompile(`\.Closed\s*&&\s*\w+\.MergedAt\s*!=\s*nil`)
	var offenders []string

	// Act
	err := filepath.WalkDir("..", func(path string, d fs.DirEntry, err error) error {
		if err != nil {
			return err
		}
		if d.IsDir() || !strings.HasSuffix(path, ".go") || strings.HasSuffix(path, "_test.go") || path == filepath.Join("..", "wsm", "visibility.go") {
			return nil
		}
		src, err := os.ReadFile(path)
		if err != nil {
			return err
		}
		if settled.Match(src) {
			offenders = append(offenders, path)
		}
		return nil
	})

	// Assert
	if err != nil {
		t.Fatalf("walking internal/: %v", err)
	}
	if len(offenders) > 0 {
		t.Fatalf("these sources spell a settled merge by hand instead of through Workspace.MergeSettled: %v", offenders)
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

// TestEveryVisibleWorkspaceRuns pins the run rule as a superset of the
// persisted visibility: over every combination of the facts Runs reads, a
// visible workspace runs.
func TestEveryVisibleWorkspaceRuns(t *testing.T) {
	of := MergeOrigin{}
	merged := time.Unix(100, 0)
	for mask := 0; mask < 1<<7; mask++ {
		bit := func(i int) bool { return mask&(1<<i) != 0 }
		ws := Workspace{Hidden: bit(0), Closed: bit(1), Current: bit(2), OneShot: bit(3), DeployMediator: bit(4)}
		if bit(5) {
			ws.MergeOf = &of
		}
		if bit(6) {
			ws.MergedAt = &merged
		}
		for _, v := range []Visibility{UnselectedVisible, Selected} {
			ws.Visibility = v
			for _, folded := range []bool{false, true} {
				// Act / Assert
				if !ws.Runs(folded, false) {
					t.Fatalf("workspace %+v folded=%v is %v but does not run", ws, folded, v)
				}
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
		{name: "a visible workspace runs", ws: Workspace{Visibility: UnselectedVisible}, want: true},
		{name: "the selection runs", ws: Workspace{Visibility: Selected}, want: true},
		{name: "a hidden selection runs", ws: Workspace{Hidden: true, Visibility: Selected}, want: true},
		{name: "a workspace that is not visible does not run", ws: Workspace{}, want: false},
		{name: "a collapsed workspace that is not visible does not run", ws: Workspace{}, repoFolded: true, want: false},
		{name: "a hidden merge workspace runs in a collapsed section", ws: Workspace{MergeOf: &of}, repoFolded: true, mergeQueued: true, want: true},
		{name: "a hidden merge workspace whose merge left every queue does not run", ws: Workspace{MergeOf: &of}, want: false},
		{name: "a selected hidden merge workspace whose merge left every queue runs", ws: Workspace{MergeOf: &of, Visibility: Selected}, want: true},
		{name: "a hidden workspace with a merge queued runs", ws: Workspace{Hidden: true}, mergeQueued: true, want: true},
		{name: "a closed workspace with a merge queued runs", ws: Workspace{Closed: true}, mergeQueued: true, want: true},
		{name: "a collapsed one-shot runs, as the fold spares it", ws: Workspace{OneShot: true}, repoFolded: true, want: true},
		{name: "a collapsed workspace with a merge pending runs, as the fold spares it", ws: Workspace{MergePending: true}, repoFolded: true, want: true},
		{name: "a hidden one-shot does not run, as only a fold spares", ws: Workspace{OneShot: true, Hidden: true}, want: false},
		{name: "an unfolded one-shot that is not visible does not run", ws: Workspace{OneShot: true}, want: false},
		{name: "a hidden standing deploy check runs", ws: Workspace{Hidden: true, DeployMediator: true}, want: true},
		{name: "a thread that is not visible does not run", ws: Workspace{Kind: WorkspaceKindThread}, want: false},
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
		{name: "a hidden standing deploy check keeps its cache warm", ws: Workspace{Hidden: true, DeployMediator: true}, want: false},
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

func TestKeptWarmIgnoresCollapsingAndHonorsAHide(t *testing.T) {
	hiddenAt := time.Date(2026, 11, 16, 12, 0, 0, 0, time.UTC)
	tests := []struct {
		name   string
		ws     Workspace
		queued bool
		want   bool
	}{
		{name: "a drawn workspace is kept warm", ws: Workspace{ID: "w"}, want: true},
		{name: "a hidden workspace lets its cache lapse", ws: Workspace{ID: "w", Hidden: true, HiddenAt: &hiddenAt}},
		{name: "a hidden workspace the user is looking at is kept warm", ws: Workspace{ID: "w", Hidden: true, HiddenAt: &hiddenAt, Current: true}, want: true},
		{name: "a hidden workspace with a merge queued is kept warm", ws: Workspace{ID: "w", Hidden: true, HiddenAt: &hiddenAt}, queued: true, want: true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got := tt.ws.KeptWarm(tt.queued)

			// Assert
			if got != tt.want {
				t.Fatalf("KeptWarm = %v, want %v", got, tt.want)
			}
		})
	}
}

// TestACollapsedWorkspaceThatRunsNothingIsStillKeptWarm pins the ruling's
// difference from Runs: a collapse withholds the session, never the cache.
func TestACollapsedWorkspaceThatRunsNothingIsStillKeptWarm(t *testing.T) {
	// Arrange
	ws := Workspace{ID: "w", Visibility: NotVisible}

	// Act
	runs, warm := ws.Runs(true, false), ws.KeptWarm(false)

	// Assert
	if runs || !warm {
		t.Fatalf("Runs collapsed = %v, KeptWarm = %v; want a collapsed workspace withheld from running but kept warm", runs, warm)
	}
}
