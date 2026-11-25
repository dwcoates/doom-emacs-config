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
		{name: "a backlogged workspace is withheld backlogged", ws: Workspace{Backlogged: true}, want: WithheldBacklogged},
		{name: "a workspace in a collapsed section is withheld collapsed", ws: Workspace{}, repoFolded: true, want: WithheldCollapsed},
		{name: "a backlogged workspace in a collapsed section is withheld backlogged", ws: Workspace{Backlogged: true}, repoFolded: true, want: WithheldBacklogged},
		{name: "a reopened merged workspace the user hid is withheld backlogged", ws: Workspace{Backlogged: true, MergedAt: &merged}, want: WithheldBacklogged},
		{name: "a settled merge ignores its backlogged flag", ws: Workspace{Backlogged: true, Closed: true, MergedAt: &merged}, want: Drawn},
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

func TestInBacklog(t *testing.T) {
	merged := time.Unix(100, 0)
	cases := []struct {
		name string
		ws   Workspace
		want bool
	}{
		{name: "an ordinary workspace is not", ws: Workspace{}, want: false},
		{name: "a backlogged workspace is", ws: Workspace{Backlogged: true}, want: true},
		{name: "a reopened merged backlogged workspace is", ws: Workspace{Backlogged: true, MergedAt: &merged}, want: true},
		{name: "a settled merge that was backlogged is not", ws: Workspace{Backlogged: true, Closed: true, MergedAt: &merged}, want: false},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got := tc.ws.InBacklog()

			// Assert
			if got != tc.want {
				t.Fatalf("InBacklog = %v, want %v", got, tc.want)
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
		{name: "a backlogged workspace does", ws: Workspace{Backlogged: true}, want: true},
		{name: "a closed workspace does", ws: Workspace{Closed: true}, want: true},
		{name: "a closed backlogged workspace does", ws: Workspace{Backlogged: true, Closed: true}, want: true},
		{name: "a reopened merged backlogged workspace does", ws: Workspace{Backlogged: true, MergedAt: &merged}, want: true},
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

// TestTheBacklogIsOneStatement pins that Withheld's backlogged arm IS
// InBacklog: every workspace it calls backlogged, and no other, is in the
// backlog, so the tab bar, the start path and the cache keeper cannot
// disagree about which workspaces are backlogged.
func TestTheBacklogIsOneStatement(t *testing.T) {
	merged := time.Unix(100, 0)
	for _, ws := range []Workspace{{}, {Backlogged: true}, {MergedAt: &merged}, {Backlogged: true, MergedAt: &merged}} {
		for _, folded := range []bool{false, true} {
			// Act
			withheldHidden := ws.Withheld(folded) == WithheldBacklogged

			// Assert
			if withheldHidden != ws.InBacklog() {
				t.Fatalf("Withheld(%v) of %+v = %v, InBacklog = %v", folded, ws, ws.Withheld(folded), ws.InBacklog())
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
		ws := Workspace{Backlogged: bit(0), Closed: bit(1), Current: bit(2), OneShot: bit(3)}
		if bit(4) {
			ws.Mediator = MediatorDeploy
		}
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
		{name: "a backlogged selection runs", ws: Workspace{Backlogged: true, Visibility: Selected}, want: true},
		{name: "a workspace that is not visible does not run", ws: Workspace{}, want: false},
		{name: "a collapsed workspace that is not visible does not run", ws: Workspace{}, repoFolded: true, want: false},
		{name: "a hidden merge workspace runs in a collapsed section", ws: Workspace{MergeOf: &of}, repoFolded: true, mergeQueued: true, want: true},
		{name: "a hidden merge workspace whose merge left every queue does not run", ws: Workspace{MergeOf: &of}, want: false},
		{name: "a selected hidden merge workspace whose merge left every queue runs", ws: Workspace{MergeOf: &of, Visibility: Selected}, want: true},
		{name: "a backlogged workspace with a merge queued runs", ws: Workspace{Backlogged: true}, mergeQueued: true, want: true},
		{name: "a closed workspace with a merge queued runs", ws: Workspace{Closed: true}, mergeQueued: true, want: true},
		{name: "a collapsed one-shot runs, as the fold spares it", ws: Workspace{OneShot: true}, repoFolded: true, want: true},
		{name: "a collapsed workspace with a merge pending runs, as the fold spares it", ws: Workspace{MergePending: true}, repoFolded: true, want: true},
		{name: "a backlogged one-shot does not run, as only a fold spares", ws: Workspace{OneShot: true, Backlogged: true}, want: false},
		{name: "an unfolded one-shot that is not visible does not run", ws: Workspace{OneShot: true}, want: false},
		{name: "a backlogged standing deploy check runs", ws: Workspace{Backlogged: true, Mediator: MediatorDeploy}, want: true},
		{name: "a backlogged production mediator runs", ws: Workspace{Backlogged: true, Mediator: MediatorProduction}, want: true},
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
		{name: "a backlogged workspace lets its cache lapse", ws: Workspace{Backlogged: true}, want: true},
		{name: "a backlogged workspace the user is looking at keeps its cache warm", ws: Workspace{Backlogged: true, Current: true}, want: false},
		{name: "a reopened merged backlogged workspace lets its cache lapse", ws: Workspace{Backlogged: true, MergedAt: &merged}, want: true},
		{name: "a backlogged standing deploy check keeps its cache warm", ws: Workspace{Backlogged: true, Mediator: MediatorDeploy}, want: false},
		{name: "a backlogged production mediator keeps its cache warm", ws: Workspace{Backlogged: true, Mediator: MediatorProduction}, want: false},
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
	for _, ws := range []Workspace{{}, {Backlogged: true}, {Backlogged: true, Current: true}, {Current: true}, {Backlogged: true, MergedAt: &merged}} {
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
		{name: "a selected backlogged workspace keeps its cache warm", ws: Workspace{Backlogged: true, Current: true}, want: false},
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

func TestKeptWarmIgnoresCollapsingAndKeepsABacklogForItsLastBeat(t *testing.T) {
	backloggedAt := time.Date(2026, 11, 16, 12, 0, 0, 0, time.UTC)
	tests := []struct {
		name   string
		ws     Workspace
		queued bool
		want   bool
	}{
		{name: "a drawn workspace is kept warm", ws: Workspace{ID: "w"}, want: true},
		{name: "a backlogged workspace is kept warm for its last beat, which the keeper spends", ws: Workspace{ID: "w", Backlogged: true, BackloggedAt: &backloggedAt}, want: true},
		{name: "a hidden merge workspace is not kept warm", ws: Workspace{ID: "w", MergeOf: &MergeOrigin{}}},
		{name: "a backlogged workspace the user is looking at is kept warm", ws: Workspace{ID: "w", Backlogged: true, BackloggedAt: &backloggedAt, Current: true}, want: true},
		{name: "a backlogged workspace with a merge queued is kept warm", ws: Workspace{ID: "w", Backlogged: true, BackloggedAt: &backloggedAt}, queued: true, want: true},
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

func TestTopPriority(t *testing.T) {
	tests := []struct {
		name     string
		priority *Priority
		want     bool
	}{
		{name: "P0.5", priority: &[]Priority{PriorityP05}[0], want: true},
		{name: "P1", priority: &[]Priority{PriorityP1}[0], want: true},
		{name: "P2", priority: &[]Priority{PriorityP2}[0], want: false},
		{name: "P3", priority: &[]Priority{PriorityP3}[0], want: false},
		{name: "unset", priority: nil, want: false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			ws := Workspace{Priority: tt.priority}

			// Act
			got := ws.TopPriority()

			// Assert
			if got != tt.want {
				t.Fatalf("TopPriority = %v, want %v", got, tt.want)
			}
		})
	}
}

func TestShimHoldLater(t *testing.T) {
	at := time.Date(2026, 11, 24, 12, 0, 0, 0, time.UTC)
	tests := []struct {
		name string
		h, o ShimHold
		want ShimHold
	}{
		{name: "an indefinite hold wins", h: ShimHold{Until: at}, o: ShimHold{Indefinite: true}, want: ShimHold{Indefinite: true}},
		{name: "the later instant wins", h: ShimHold{Until: at}, o: ShimHold{Until: at.Add(time.Minute)}, want: ShimHold{Until: at.Add(time.Minute)}},
		{name: "an earlier instant loses", h: ShimHold{Until: at.Add(time.Minute)}, o: ShimHold{Until: at}, want: ShimHold{Until: at.Add(time.Minute)}},
		{name: "two empty holds stay empty", want: ShimHold{}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act / Assert
			if got := tt.h.Later(tt.o); got != tt.want {
				t.Fatalf("Later = %+v, want %+v", got, tt.want)
			}
		})
	}
}
