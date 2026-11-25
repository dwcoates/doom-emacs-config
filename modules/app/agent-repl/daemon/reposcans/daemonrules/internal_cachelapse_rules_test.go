package daemonrules

import (
	"go/ast"
	"go/token"
	"os"
	"path/filepath"
	"regexp"
	"slices"
	"testing"

	"agent-repld/reposcans/internal/sourcescan"
)

// internalCachelapseDir is the package these rules hold, under the daemon module's root.
const internalCachelapseDir = "internal/cachelapse"

// TestColdMarkWritesHaveOneHome fails when a source file in this package
// writes the cold mark other than through Keeper.markCold.
func TestColdMarkWritesHaveOneHome(t *testing.T) {
	sourcescan.InPackage(t, internalCachelapseDir)
	// Act
	sites := sourcescan.Sites(t, internalCachelapseDir, ".MarkCacheCold(ctx")

	// Assert
	if len(sites) != 1 || sites[0] != "coldmark.go" {
		t.Fatalf("the cold mark is written at %v, want only Keeper.markCold in coldmark.go", sites)
	}
}

// TestColdParkAsksHaveOneHome fails when a source file in this package asks
// for the cold park other than through Keeper.parkCold.
func TestColdParkAsksHaveOneHome(t *testing.T) {
	sourcescan.InPackage(t, internalCachelapseDir)
	// Act
	sites := sourcescan.Sites(t, internalCachelapseDir, "k.deps.ColdDown(")

	// Assert
	if len(sites) != 1 || sites[0] != "coldmark.go" {
		t.Fatalf("the cold park is asked at %v, want only Keeper.parkCold in coldmark.go", sites)
	}
}

// TestTheUndeliveredFaultHasOneOpen fails when a source of this package opens
// the undelivered fault other than through Keeper.raiseUndelivered.
func TestTheUndeliveredFaultHasOneOpen(t *testing.T) {
	sourcescan.InPackage(t, internalCachelapseDir)
	// Act
	sites := sourcescan.Sites(t, internalCachelapseDir, "Kind:      health.KindKeepaliveUndelivered")

	// Assert
	if len(sites) != 1 || sites[0] != "delivery.go" {
		t.Fatalf("the undelivered fault is opened at %v, want only Keeper.raiseUndelivered in delivery.go", sites)
	}
}

func TestAGateIsDatedOnlyByGateLapsedAt(t *testing.T) {
	sourcescan.InPackage(t, internalCachelapseDir)
	// Arrange: a cache lifetime ADDED to an instant is a gate being dated; naming the
	// lifetime's tier (telemetry's coldGateOutcome) dates nothing.
	dating := regexp.MustCompile(`(\+=|\.Add\().*GetCacheTtlMs\(\)`)

	// Act
	sites := sourcescan.SitesMatching(t, internalCachelapseDir, dating)

	// Assert
	if len(sites) != 1 || sites[0] != "keeper.go" {
		t.Fatalf("a gate is dated by its cache lifetime at %v, want only GateLapsedAt in keeper.go", sites)
	}
}

// TestLapseWritesHaveOneHome fails when a source file in this package writes
// a cache lapse other than through Keeper.store.
func TestLapseWritesHaveOneHome(t *testing.T) {
	sourcescan.InPackage(t, internalCachelapseDir)
	// Act
	sites := sourcescan.Sites(t, internalCachelapseDir, ".SetCacheLapse(ctx")

	// Assert
	if len(sites) != 1 || sites[0] != "keeper.go" {
		t.Fatalf("a cache lapse is written at %v, want only Keeper.store in keeper.go", sites)
	}
}

// TestQueuedEdgesAreAppliedOnlyThroughApplyQueued fails when a test of this
// package hand-rolls the take-and-apply loop applyQueued owns.
func TestQueuedEdgesAreAppliedOnlyThroughApplyQueued(t *testing.T) {
	sourcescan.InPackage(t, internalCachelapseDir)
	// Arrange
	files, err := filepath.Glob("*_test.go")
	if err != nil {
		t.Fatalf("Glob: %v", err)
	}

	// Act
	var sites []string
	for _, name := range files {
		source, err := os.ReadFile(name)
		if err != nil {
			t.Fatalf("ReadFile %s: %v", name, err)
		}
		for range regexp.MustCompile(`range \w+\.keeper\.take\(\)`).FindAllIndex(source, -1) {
			sites = append(sites, name)
		}
	}

	// Assert
	if !slices.Equal(sites, []string{"keeper_test.go"}) {
		t.Fatalf("the take-and-apply loop is at %v, want only harness.applyQueued in keeper_test.go", sites)
	}
}

// TestTheVerbAndTheScheduledBeatShareOneSubroutine fails when the keep-live
// verb or the scheduled beat reaches the shim's keep-alive other than through
// sendBeat, so the two can never drift apart.
func TestTheVerbAndTheScheduledBeatShareOneSubroutine(t *testing.T) {
	sourcescan.InPackage(t, internalCachelapseDir)
	// Arrange
	callers := map[string][]string{}
	sourcescan.EachSourceIn(t, ".", false, func(_ *token.FileSet, f *ast.File) {
		for _, decl := range f.Decls {
			fn, ok := decl.(*ast.FuncDecl)
			if !ok || fn.Body == nil {
				continue
			}
			ast.Inspect(fn.Body, func(n ast.Node) bool {
				if call, ok := n.(*ast.CallExpr); ok {
					if sel, ok := call.Fun.(*ast.SelectorExpr); ok {
						callers[sel.Sel.Name] = append(callers[sel.Sel.Name], fn.Name.Name)
					}
				}
				return true
			})
		}
	})

	// Assert
	if got, want := callers["sendBeat"], []string{"beginKeepLive", "beatsDue"}; !sameSet(got, want) {
		t.Fatalf("sendBeat is called from %v, want only %v", got, want)
	}
	// SendBackloggedKeepalives' beat (`SPC j R`) keeps its cadence held and owns
	// no stand-down, so it is the one other caller of the shim's keep-alive.
	if got, want := callers["Beat"], []string{"sendBeat", "beat"}; !sameSet(got, want) {
		t.Fatalf("the shim's keep-alive is asked from %v, want only %v", got, want)
	}
	if got, want := callers["settleAnswer"], []string{"answered", "answeredFailed", "answersDue"}; !sameSet(got, want) {
		t.Fatalf("the answer is settled from %v, want only %v", got, want)
	}
}

func sameSet(got, want []string) bool {
	got, want = slices.Clone(got), slices.Clone(want)
	slices.Sort(got)
	slices.Sort(want)
	return slices.Equal(got, want)
}

func TestProjectedGateWritesHaveOneHome(t *testing.T) {
	sourcescan.InPackage(t, internalCachelapseDir)
	// Act
	sites := sourcescan.Sites(t, internalCachelapseDir, ".SetProjectedColdGate(ctx")

	// Assert
	if len(sites) != 1 || sites[0] != "projectedgate.go" {
		t.Fatalf("a projected gate is written at %v, want only Keeper.projectGate in projectedgate.go", sites)
	}
}

// TestTheKeepaliveScheduleNeverReadsTheFolds pins the ruling structurally: the
// beat plan judges a workspace by wsm.Workspace.KeptWarm alone, and reads no
// section's fold, so collapsing can never withhold a keep-alive.
func TestTheKeepaliveScheduleNeverReadsTheFolds(t *testing.T) {
	sourcescan.InPackage(t, internalCachelapseDir)
	// Act
	folds := sourcescan.Sites(t, internalCachelapseDir, "bringup.OnlyVisible(")
	warm := sourcescan.Sites(t, internalCachelapseDir, ".KeptWarm(")

	// Assert
	if len(folds) != 0 || !slices.Equal(warm, []string{"schedule.go"}) {
		t.Fatalf("OnlyVisible read in %v, KeptWarm read in %v; want the folds read nowhere and KeptWarm in schedule.go", folds, warm)
	}
}

// TestShimCallFailureTriageHasOneHome fails when a source file in this
// package triages a dead shim by hand instead of through shimCallFailed.
func TestShimCallFailureTriageHasOneHome(t *testing.T) {
	sourcescan.InPackage(t, internalCachelapseDir)
	// Act
	sites := sourcescan.Sites(t, internalCachelapseDir, "shimclient.ErrShimDied")

	// Assert
	if len(sites) != 1 || sites[0] != "keeper.go" {
		t.Fatalf("shimclient.ErrShimDied is triaged at %v, want only shimCallFailed in keeper.go", sites)
	}
}
