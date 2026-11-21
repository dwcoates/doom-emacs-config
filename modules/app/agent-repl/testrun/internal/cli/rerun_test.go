package cli

import (
	"bytes"
	"errors"
	"os"
	"path/filepath"
	"strings"
	"testing"

	testrunv1 "agentrepl/proto/testrun/v1"

	"google.golang.org/protobuf/proto"

	"agentrepl/testrun/internal/events"
	"agentrepl/testrun/internal/suites"
	"agentrepl/testrun/roster"
)

// rerunLister lists no case at all, or fails every listing with err.
type rerunLister struct{ err error }

func (l rerunLister) GoTests(string, string) ([]string, error) { return nil, l.err }
func (l rerunLister) VitestNames(string, string, string, []string) (map[string][]string, error) {
	return nil, l.err
}
func (l rerunLister) ERTNames(string, string, []string) (map[string][]string, error) {
	return nil, l.err
}
func (l rerunLister) HarnessGroups(suites.Layout, roster.Suite) ([]string, error) { return nil, l.err }

// rerunOf is a rerun of SELS.
func rerunOf(sels ...*testrunv1.TestSelection) *testrunv1.TestRunRequest {
	return &testrunv1.TestRunRequest{Selections: sels, DropMissing: true}
}

// retiredSuite is a case the roster no longer holds.
var retiredSuite = &testrunv1.TestSelection{Target: &testrunv1.TestSelection_Suite{Suite: &testrunv1.TestSelectionSuite{Name: "retired-suite"}}}

// rerunHarness is a harness whose tree holds the test-all harness script.
func rerunHarness(t *testing.T, list rerunLister) (harness, *bytes.Buffer) {
	t.Helper()
	h := newHarness(t, nil, nil)
	script := filepath.Join(h.module, "bin", "test-test-all.sh")
	if err := os.MkdirAll(filepath.Dir(script), 0o755); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(script, nil, 0o755); err != nil {
		t.Fatal(err)
	}
	if err := os.MkdirAll(filepath.Join(h.module, "daemon", "internal", "merge"), 0o755); err != nil {
		t.Fatal(err)
	}
	h.deps.Lister = list
	var stream bytes.Buffer
	h.deps.Events = events.New(&stream, h.deps.Log)
	return h, &stream
}

func TestARerunStatesWhatItDroppedAndRunsTheRest(t *testing.T) {
	// Arrange
	h, stream := rerunHarness(t, rerunLister{})
	kept := harnessSelection().GetSelections()[0]

	// Act
	code := h.run(t, "--selection", writeSelection(t, rerunOf(kept, retiredSuite)))

	// Assert
	evs := eventsOf(t, stream)
	if code != 0 || len(evs) < 3 {
		t.Fatalf("exit = %d, events = %v; want a passing run", code, evs)
	}
	dropped := evs[0].GetDropped().GetCases()
	if len(dropped) != 1 || !proto.Equal(dropped[0].GetSelection(), retiredSuite) || !strings.Contains(dropped[0].GetDetail(), "no such suite any more") {
		t.Fatalf("first event = %v; want the retired suite dropped as sent", evs[0])
	}
	if evs[1].GetPlanned() == nil || evs[len(evs)-1].GetEnded().GetPassed() == nil {
		t.Fatalf("events = %v; want dropped, then planned, ..., ended passed", evs)
	}
	if !strings.Contains(h.out.String(), "the rerun dropped suite retired-suite") {
		t.Fatalf("log = %s; want the dropped case recorded", h.out)
	}
}

func TestARerunWhoseEveryCaseIsGoneIsRefusedAsNothingLeft(t *testing.T) {
	// Arrange
	h, stream := rerunHarness(t, rerunLister{})

	// Act
	code := h.run(t, "--selection", writeSelection(t, rerunOf(retiredSuite)))

	// Assert
	evs := eventsOf(t, stream)
	if code != 1 || len(h.exec.ran) != 0 || len(evs) != 2 || evs[0].GetDropped() == nil || evs[1].GetRefused().GetNothingLeft() == nil {
		t.Fatalf("exit = %d, ran %v, events = %v; want 1, nothing run, dropped then refused nothing left", code, h.exec.ran, evs)
	}
	if !strings.Contains(h.errOut.String(), "the tree holds none of the rerun's 1 cases any more") {
		t.Fatalf("stderr = %s; want the refusal recorded", h.errOut)
	}
}

func TestARerunSelectionNamingSeveralCasesIsRefused(t *testing.T) {
	// Arrange
	h, stream := rerunHarness(t, rerunLister{})

	// Act
	code := h.run(t, "--selection", writeSelection(t, rerunOf(harnessSelection("a", "b").GetSelections()[0])))

	// Assert
	evs := eventsOf(t, stream)
	if code != 1 || len(h.exec.ran) != 0 || len(evs) != 1 || evs[0].GetRefused() == nil || evs[0].GetRefused().GetNothingLeft() != nil {
		t.Fatalf("exit = %d, ran %v, events = %v; want 1, nothing run, one plain refusal", code, h.exec.ran, evs)
	}
	if !strings.Contains(h.errOut.String(), "names exactly one case") {
		t.Fatalf("stderr = %s; want the refusal recorded", h.errOut)
	}
}

func TestARerunWhoseListingFailsIsAbortedRunningAndDroppingNothing(t *testing.T) {
	// Arrange
	h, stream := rerunHarness(t, rerunLister{err: errors.New("go/build broke")})
	sel := &testrunv1.TestSelection{Target: &testrunv1.TestSelection_GoPackage{GoPackage: &testrunv1.TestSelectionGoPackage{
		PackageDir: "modules/app/agent-repl/daemon/internal/merge", Tests: &testrunv1.TestSelectionGoPackage_All{All: &testrunv1.TestSelectionGoAllTests{}}}}}

	// Act
	code := h.run(t, "--selection", writeSelection(t, rerunOf(sel)))

	// Assert
	evs := eventsOf(t, stream)
	if code != 1 || len(h.exec.ran) != 0 || len(evs) != 1 || !strings.Contains(evs[0].GetAborted().GetDetail(), "go/build broke") {
		t.Fatalf("exit = %d, ran %v, events = %v; want 1, nothing run, one abort naming the cause", code, h.exec.ran, evs)
	}
	if !strings.Contains(h.errOut.String(), "the rerun could not list the cases the tree holds") {
		t.Fatalf("stderr = %s; want the listing's failure recorded", h.errOut)
	}
}
