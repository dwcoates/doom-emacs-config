package merge

import (
	"context"
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"sync"
	"testing"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"
	"claude-repld/internal/gitexec"
)

func TestMain(m *testing.M) {
	// The package executes both fixture Git commands and the production merge
	// driver. A parent pre-commit hook exports its live repository bindings,
	// which must never escape into either path.
	for _, key := range []string{"GIT_DIR", "GIT_WORK_TREE", "GIT_INDEX_FILE", "GIT_PREFIX"} {
		_ = os.Unsetenv(key)
	}
	os.Exit(m.Run())
}

// --- fakes & fixtures ---------------------------------------------------

// recordingSink captures every transition the driver emits, in order. When
// failOn is set, RecordMergeTransition returns an error for that phase (to
// exercise sink-error propagation).
type recordingSink struct {
	got      []transition
	statuses []*frontendv1.MergeStatus
	failOn   Phase
}

type transition struct {
	ws    string
	phase Phase
	cause string
}

func (s *recordingSink) RecordMergeTransition(ws string, phase Phase, cause string) error {
	s.got = append(s.got, transition{ws, phase, cause})
	if s.failOn != "" && phase == s.failOn {
		return errFakeSink
	}
	return nil
}

// RecordMergeStatus makes the double the StatusSink too, exactly as the
// production sink is both: one call carries the axis row and the phase status,
// so a test cannot observe one without the other any more than production can.
func (s *recordingSink) RecordMergeStatus(ws string, phase Phase, cause string, status *frontendv1.MergeStatus) error {
	s.statuses = append(s.statuses, status)
	return s.RecordMergeTransition(ws, phase, cause)
}

var (
	_ StateSink  = (*recordingSink)(nil)
	_ StatusSink = (*recordingSink)(nil)
)

// withRun attaches a run to a driver-level request. merge.Driver refuses a
// request that carries none — a merge that publishes no phase status lands
// commits no frontend can watch — and in production merge.Coordinator mints it
// at admission. These tests drive the driver directly, so they mint their own.
func withRun(t *testing.T, sink StatusSink, req Request) Request {
	t.Helper()
	run, err := NewRunStatus(sink, t.Logf, req.Workspace, testClock())
	if err != nil {
		t.Fatalf("NewRunStatus: %v", err)
	}
	req.Run = run
	return req
}

// testClock is a monotonically advancing millisecond clock. It is deliberately
// not time.Now: a test asserting on the phase timestamps must not depend on how
// fast the machine ran.
func testClock() func() int64 {
	var at int64 = 1_000_000
	var mu sync.Mutex
	return func() int64 {
		mu.Lock()
		defer mu.Unlock()
		at++
		return at
	}
}

func (s *recordingSink) phases() []Phase {
	ps := make([]Phase, len(s.got))
	for i, t := range s.got {
		ps[i] = t.phase
	}
	return ps
}

type sentinelError string

func (e sentinelError) Error() string { return string(e) }

const errFakeSink = sentinelError("sink write failed")

// fakeSuite stands in for merge.SuiteRunner. It records every target it was
// asked to test, in order, and replays a scripted list of verdicts — the last
// verdict repeats, so a test that only cares about "the gate never fails"
// scripts nothing at all.
type fakeSuite struct {
	targets  []string
	verdicts []SuiteResult
	err      error
	calls    int
}

// skipping is the harness default: a target repository with no test entrypoint,
// which is what every fixture repo in this file actually is.
func skippingSuite() *fakeSuite {
	return &fakeSuite{verdicts: []SuiteResult{{Skipped: true, Reason: "fixture repo declares no test entrypoint"}}}
}

func (f *fakeSuite) RunSuite(_ context.Context, targetDir string) (SuiteResult, error) {
	f.targets = append(f.targets, targetDir)
	f.calls++
	if f.err != nil {
		return SuiteResult{}, f.err
	}
	if len(f.verdicts) == 0 {
		return SuiteResult{Passed: true}, nil
	}
	if f.calls <= len(f.verdicts) {
		return f.verdicts[f.calls-1], nil
	}
	return f.verdicts[len(f.verdicts)-1], nil
}

func newTestDriver(t *testing.T, sink StateSink) *Driver {
	t.Helper()
	return newTestDriverWithSuite(t, sink, skippingSuite())
}

func newTestDriverWithSuite(t *testing.T, sink StateSink, suite SuiteRunner) *Driver {
	t.Helper()
	e, err := NewDriver(Config{Logf: t.Logf, Sink: sink, Suite: suite})
	if err != nil {
		t.Fatalf("NewDriver: %v", err)
	}
	return e
}

// The env-shape assertions on the shared builder live in internal/gitexec.
// What this package owns is that merge.Driver's OWN git goes through that
// builder: a daemon (or this suite) running under the repo's pre-commit hook
// inherits GIT_DIR and would otherwise cherry-pick into the HOOK'S repository
// instead of the fixture's.
func TestMergeIgnoresAnInheritedGitDir(t *testing.T) {
	// Arrange — a real fixture merge, plus a hook-shaped leak pointing elsewhere.
	target := initTarget(t)
	featureDir := addFeatureWorktree(t, target)
	writeFile(t, featureDir, "feature.txt", "hello\n")
	gitRun(t, featureDir, "add", ".")
	gitRun(t, featureDir, "commit", "-q", "-m", "add feature.txt")

	sink := &recordingSink{}
	e := newTestDriver(t, sink)
	req := withRun(t, sink, Request{Workspace: "/ws/leak-ws", Name: "leak-ws", SourceBranch: "feature", SourceDir: featureDir, TargetDir: target})

	elsewhere := t.TempDir()
	t.Setenv("GIT_DIR", filepath.Join(elsewhere, ".git"))
	t.Setenv("GIT_WORK_TREE", elsewhere)
	t.Setenv("GIT_INDEX_FILE", filepath.Join(elsewhere, ".git", "index"))

	// Act.
	res, err := e.Merge(context.Background(), req)

	// Assert — the pick landed in the -C target, not the leaked repository.
	if err != nil {
		t.Fatalf("Merge() err = %v; the inherited GIT_DIR reached git", err)
	}
	if res.Outcome != OutcomeMerged {
		t.Fatalf("Merge() outcome = %s, want merged", res.Outcome)
	}
	if _, statErr := os.Stat(filepath.Join(target, "feature.txt")); statErr != nil {
		t.Errorf("feature.txt did not land on the -C target: %v", statErr)
	}
}

// gitRun runs a git command in dir, failing the test on error. Test-driver
// git only; the driver runs its own git. It uses the same env-stripped builder
// the driver does, so a leaked GIT_DIR cannot make a fixture's `git add` /
// `git commit` rewrite the caller's real repository.
func gitRun(t *testing.T, dir string, args ...string) string {
	t.Helper()
	gitArgs := append([]string{"-c", "core.hooksPath=/dev/null"}, args...)
	cmd := gitexec.Command(context.Background(), dir, gitArgs...)
	out, err := cmd.CombinedOutput()
	if err != nil {
		t.Fatalf("git %v in %s: %v\n%s", args, dir, err, out)
	}
	return string(out)
}

// writeFile writes content to a file under dir.
func writeFile(t *testing.T, dir, name, content string) {
	t.Helper()
	if err := os.WriteFile(filepath.Join(dir, name), []byte(content), 0o644); err != nil {
		t.Fatalf("write %s: %v", name, err)
	}
}

// initTarget creates a fresh repo on `main` with one commit (A: base.txt) and
// repo-local identity so the driver's cherry-pick can commit hermetically.
func initTarget(t *testing.T) string {
	t.Helper()
	dir := t.TempDir()
	gitRun(t, dir, "init", "-q", "-b", "main")
	gitRun(t, dir, "config", "user.email", "test@example.com")
	gitRun(t, dir, "config", "user.name", "Test")
	gitRun(t, dir, "config", "commit.gpgsign", "false")
	// Driver.Merge runs production Git commands directly. Persist the fixture
	// hook isolation so cherry-pick commits cannot recurse into the parent
	// repository's pre-commit suite.
	gitRun(t, dir, "config", "core.hooksPath", "/dev/null")
	writeFile(t, dir, "base.txt", "base\n")
	gitRun(t, dir, "add", ".")
	gitRun(t, dir, "commit", "-q", "-m", "A")
	return dir
}

// addFeatureWorktree adds a linked worktree checked out on a new `feature`
// branch cut from main, modeling the sibling source/target worktrees of the
// real system (shared object store + refs). Returns the feature worktree dir.
func addFeatureWorktree(t *testing.T, target string) string {
	t.Helper()
	featureDir := filepath.Join(t.TempDir(), "wt")
	gitRun(t, target, "worktree", "add", "-q", "-b", "feature", featureDir, "main")
	return featureDir
}

// --- construction -------------------------------------------------------

func TestNewDriverRequiresDependencies(t *testing.T) {
	tests := []struct {
		name    string
		cfg     Config
		wantErr bool
	}{
		{"nil logf", Config{Sink: &recordingSink{}, Suite: skippingSuite()}, true},
		{"nil sink", Config{Logf: func(string, ...any) {}, Suite: skippingSuite()}, true},
		{"nil suite", Config{Logf: func(string, ...any) {}, Sink: &recordingSink{}}, true},
		{"all present", Config{Logf: func(string, ...any) {}, Sink: &recordingSink{}, Suite: skippingSuite()}, false},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Act.
			_, err := NewDriver(tc.cfg)
			// Assert.
			if (err != nil) != tc.wantErr {
				t.Fatalf("NewDriver err = %v, wantErr = %v", err, tc.wantErr)
			}
		})
	}
}

func TestMergeRejectsIncompleteRequest(t *testing.T) {
	base := withRun(t, &recordingSink{}, Request{Workspace: "/ws/ws", Name: "ws", SourceBranch: "feature", SourceDir: "/s", TargetDir: "/t"})
	tests := []struct {
		name  string
		mutat func(*Request)
	}{
		{"no workspace", func(r *Request) { r.Workspace = "" }},
		{"no branch", func(r *Request) { r.SourceBranch = "" }},
		{"no source dir", func(r *Request) { r.SourceDir = "" }},
		{"no target dir", func(r *Request) { r.TargetDir = "" }},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			sink := &recordingSink{}
			e := newTestDriver(t, sink)
			req := base
			tc.mutat(&req)
			// Act.
			_, err := e.Merge(context.Background(), req)
			// Assert.
			if err == nil {
				t.Fatalf("Merge() err = nil, want validation error")
			}
			if len(sink.got) != 0 {
				t.Fatalf("Merge() emitted %v on invalid request; want none", sink.phases())
			}
		})
	}
}

// --- clean cherry-pick end-to-end ---------------------------------------

func TestMergeCleanCherryPick(t *testing.T) {
	// Arrange: target at A; feature adds an orthogonal file.
	target := initTarget(t)
	featureDir := addFeatureWorktree(t, target)
	writeFile(t, featureDir, "feature.txt", "hello\n")
	gitRun(t, featureDir, "add", ".")
	gitRun(t, featureDir, "commit", "-q", "-m", "add feature.txt")

	sink := &recordingSink{}
	e := newTestDriver(t, sink)
	req := withRun(t, sink, Request{Workspace: "/ws/clean-ws", Name: "clean-ws", SourceBranch: "feature", SourceDir: featureDir, TargetDir: target})

	// Act.
	res, err := e.Merge(context.Background(), req)

	// Assert.
	if err != nil {
		t.Fatalf("Merge() err = %v", err)
	}
	if res.Outcome != OutcomeMerged || res.AlreadyIncorporated {
		t.Fatalf("Merge() res = %+v, want merged (not already-incorporated)", res)
	}
	if _, statErr := os.Stat(filepath.Join(target, "feature.txt")); statErr != nil {
		t.Errorf("feature.txt did not land on target: %v", statErr)
	}
	// The landed commit carries the -x annotation.
	if body := gitRun(t, target, "log", "-1", "--pretty=%B"); !strings.Contains(body, "cherry picked from commit") {
		t.Errorf("landed commit missing -x annotation:\n%s", body)
	}
	// Finalization: the merge/<ws> tag exists at HEAD.
	if tags := gitRun(t, target, "tag", "-l", "merge/clean-ws"); !strings.Contains(tags, "merge/clean-ws") {
		t.Errorf("tag merge/clean-ws not created; git tag -l = %q", tags)
	}
	if res.Tag != "merge/clean-ws" {
		t.Errorf("res.Tag = %q, want merge/clean-ws", res.Tag)
	}
	// A cherry_picking and a testing phase for each commit the replay picks,
	// then the terminal merged. Nothing precedes them: a cherry_picking status
	// published before the plan exists could only ever report 0 of 0.
	assertPhases(t, sink.phases(), PhaseMerging, PhaseMerging, PhaseMerged)
}

// --- conflict detection -------------------------------------------------

func TestMergeTransitionsAreEmittedOnTheStateKeyNotTheName(t *testing.T) {
	// The two identities must not be conflated: state rows filed under the
	// display name land on a workspace key the SSM knows nothing else about,
	// so the composite carries no connectivity verdict and the frontend drops
	// the frame. The tag is the one place the name belongs.
	// Arrange.
	target := initTarget(t)
	featureDir := addFeatureWorktree(t, target)
	writeFile(t, featureDir, "feature.txt", "hello\n")
	gitRun(t, featureDir, "add", ".")
	gitRun(t, featureDir, "commit", "-q", "-m", "add feature.txt")

	sink := &recordingSink{}
	e := newTestDriver(t, sink)
	req := withRun(t, sink, Request{Workspace: "/ws/keyed-ws", Name: "keyed-ws", SourceBranch: "feature", SourceDir: featureDir, TargetDir: target})

	// Act.
	res, err := e.Merge(context.Background(), req)

	// Assert.
	if err != nil {
		t.Fatalf("Merge() err = %v", err)
	}
	if len(sink.got) == 0 {
		t.Fatal("no merge transitions recorded")
	}
	for _, tr := range sink.got {
		if tr.ws != "/ws/keyed-ws" {
			t.Errorf("transition %s keyed on %q, want the state key /ws/keyed-ws", tr.phase, tr.ws)
		}
	}
	if res.Tag != "merge/keyed-ws" {
		t.Errorf("res.Tag = %q, want the DISPLAY name in the tag", res.Tag)
	}
}

func TestMergeConflictDetection(t *testing.T) {
	// Arrange: feature and target both edit the same line — a real conflict.
	target := initTarget(t)
	featureDir := addFeatureWorktree(t, target)
	writeFile(t, featureDir, "base.txt", "feature\n")
	gitRun(t, featureDir, "add", ".")
	gitRun(t, featureDir, "commit", "-q", "-m", "feature edit")
	writeFile(t, target, "base.txt", "target\n")
	gitRun(t, target, "add", ".")
	gitRun(t, target, "commit", "-q", "-m", "target edit")

	sink := &recordingSink{}
	e := newTestDriver(t, sink)
	req := withRun(t, sink, Request{Workspace: "/ws/cf-ws", Name: "cf-ws", SourceBranch: "feature", SourceDir: featureDir, TargetDir: target})

	// Act.
	res, err := e.Merge(context.Background(), req)

	// Assert.
	if err != nil {
		t.Fatalf("Merge() err = %v", err)
	}
	if res.Outcome != OutcomeConflict {
		t.Fatalf("Merge() outcome = %s, want conflict", res.Outcome)
	}
	if res.ConflictCommit == "" {
		t.Errorf("res.ConflictCommit empty; want the conflicting short SHA")
	}
	// The pick is left IN TREE (never aborted): CHERRY_PICK_HEAD survives.
	if !cherryPickHeadPresent(t, target) {
		t.Errorf("CHERRY_PICK_HEAD absent — driver aborted the conflict instead of leaving it in tree")
	}
	assertPhases(t, sink.phases(), PhaseMerging, PhaseMergeConflict)
}

// --- resume-after-resolve completing the merge --------------------------

func TestResumeCompletesMergeAndOrdersTransitions(t *testing.T) {
	// Arrange: drive a real conflict via Merge, then a human resolves it.
	target := initTarget(t)
	featureDir := addFeatureWorktree(t, target)
	writeFile(t, featureDir, "base.txt", "feature\n")
	gitRun(t, featureDir, "add", ".")
	gitRun(t, featureDir, "commit", "-q", "-m", "feature edit")
	writeFile(t, target, "base.txt", "target\n")
	gitRun(t, target, "add", ".")
	gitRun(t, target, "commit", "-q", "-m", "target edit")

	sink := &recordingSink{}
	e := newTestDriver(t, sink)
	req := withRun(t, sink, Request{Workspace: "/ws/rz-ws", Name: "rz-ws", SourceBranch: "feature", SourceDir: featureDir, TargetDir: target})

	if res, err := e.Merge(context.Background(), req); err != nil || res.Outcome != OutcomeConflict {
		t.Fatalf("setup Merge() res=%+v err=%v, want conflict", res, err)
	}
	// Human resolves the conflict and stages the file.
	writeFile(t, target, "base.txt", "resolved\n")
	gitRun(t, target, "add", "base.txt")

	// Act.
	res, err := e.Resume(context.Background(), req)

	// Assert.
	if err != nil {
		t.Fatalf("Resume() err = %v", err)
	}
	if res.Outcome != OutcomeMerged {
		t.Fatalf("Resume() outcome = %s, want merged", res.Outcome)
	}
	if cherryPickHeadPresent(t, target) {
		t.Errorf("CHERRY_PICK_HEAD still present after a completed Resume")
	}
	if got := readFile(t, target, "base.txt"); got != "resolved\n" {
		t.Errorf("base.txt = %q, want the resolved content", got)
	}
	if tags := gitRun(t, target, "tag", "-l", "merge/rz-ws"); !strings.Contains(tags, "merge/rz-ws") {
		t.Errorf("tag merge/rz-ws not created after Resume")
	}
	// The full ordered sequence across Merge + Resume: the opening pick, the
	// conflict, the resume, the test gate on the RESUMED commit, the terminal.
	assertPhases(t, sink.phases(), PhaseMerging, PhaseMergeConflict, PhaseMerging, PhaseMerging, PhaseMerged)
}

func TestResumeWithoutInProgressPickFails(t *testing.T) {
	// Arrange: a clean repo with no paused cherry-pick.
	target := initTarget(t)
	featureDir := addFeatureWorktree(t, target)
	sink := &recordingSink{}
	e := newTestDriver(t, sink)
	req := withRun(t, sink, Request{Workspace: "/ws/no-pick", Name: "no-pick", SourceBranch: "feature", SourceDir: featureDir, TargetDir: target})

	// Act.
	_, err := e.Resume(context.Background(), req)

	// Assert.
	if err == nil {
		t.Fatalf("Resume() err = nil; want a loud failure when nothing is in progress")
	}
	if len(sink.got) != 0 {
		t.Fatalf("Resume() emitted %v with no pick in progress; want none", sink.phases())
	}
}

// --- a range containing a merge commit ---------------------------------

// Per-commit replay FLATTENS a branch that has an internal merge: `--no-merges`
// drops the merge commit (which carries no patch of its own) and both of its
// sides are picked individually, because both are already in the range.
//
// The whole-range driver this replaced could not do that — `git cherry-pick`
// refuses a range containing a merge outright — so a branch with an internal
// merge used to fail the entire attempt.
func TestMergeFlattensAMergeCommitInTheRange(t *testing.T) {
	// Arrange: feature = A -> (side S) -> merge M -> F2.
	target := initTarget(t)
	featureDir := addFeatureWorktree(t, target)
	gitRun(t, featureDir, "checkout", "-q", "-b", "side")
	writeFile(t, featureDir, "s.txt", "s\n")
	gitRun(t, featureDir, "add", ".")
	gitRun(t, featureDir, "commit", "-q", "-m", "S")
	gitRun(t, featureDir, "checkout", "-q", "feature")
	gitRun(t, featureDir, "merge", "-q", "--no-ff", "-m", "merge side", "side")
	writeFile(t, featureDir, "f2.txt", "f2\n")
	gitRun(t, featureDir, "add", ".")
	gitRun(t, featureDir, "commit", "-q", "-m", "F2")

	sink := &recordingSink{}
	e := newTestDriver(t, sink)
	req := withRun(t, sink, Request{Workspace: "/ws/flat-ws", Name: "flat-ws", SourceBranch: "feature", SourceDir: featureDir, TargetDir: target})

	// Act.
	res, err := e.Merge(context.Background(), req)

	// Assert — both sides of the merge landed, under one merged outcome.
	if err != nil {
		t.Fatalf("Merge() err = %v", err)
	}
	if res.Outcome != OutcomeMerged {
		t.Fatalf("Merge() outcome = %s, want merged", res.Outcome)
	}
	for _, name := range []string{"s.txt", "f2.txt"} {
		if _, statErr := os.Stat(filepath.Join(target, name)); statErr != nil {
			t.Errorf("%s did not land on the target: %v", name, statErr)
		}
	}
	assertPhases(t, sink.phases(), PhaseMerging, PhaseMerging, PhaseMerging, PhaseMerging, PhaseMerged)
}

// --- preconditions abort with state intact ------------------------------

func TestMergeDirtySourceAbortsWithStateIntact(t *testing.T) {
	// Arrange: uncommitted change to a tracked file in the source worktree.
	target := initTarget(t)
	featureDir := addFeatureWorktree(t, target)
	writeFile(t, featureDir, "feature.txt", "hello\n")
	gitRun(t, featureDir, "add", ".")
	gitRun(t, featureDir, "commit", "-q", "-m", "add feature.txt")
	writeFile(t, featureDir, "base.txt", "dirty edit\n") // tracked, unstaged

	sink := &recordingSink{}
	e := newTestDriver(t, sink)
	req := withRun(t, sink, Request{Workspace: "/ws/dirty-ws", Name: "dirty-ws", SourceBranch: "feature", SourceDir: featureDir, TargetDir: target})

	// Act.
	_, err := e.Merge(context.Background(), req)

	// Assert.
	if err == nil {
		t.Fatalf("Merge() err = nil; want a dirty-source precondition failure")
	}
	if len(sink.got) != 0 {
		t.Fatalf("Merge() emitted %v on a failed precondition; want state intact (none)", sink.phases())
	}
}

func TestMergeMissingBranchAbortsWithStateIntact(t *testing.T) {
	// Arrange.
	target := initTarget(t)
	featureDir := addFeatureWorktree(t, target)
	sink := &recordingSink{}
	e := newTestDriver(t, sink)
	req := withRun(t, sink, Request{Workspace: "/ws/nb-ws", Name: "nb-ws", SourceBranch: "does-not-exist", SourceDir: featureDir, TargetDir: target})

	// Act.
	_, err := e.Merge(context.Background(), req)

	// Assert.
	if err == nil {
		t.Fatalf("Merge() err = nil; want a missing-branch failure")
	}
	if len(sink.got) != 0 {
		t.Fatalf("Merge() emitted %v for a missing branch; want none", sink.phases())
	}
}

func TestMergeDirtyStagedSourceAbortsWithStateIntact(t *testing.T) {
	// Arrange: a STAGED (not merely unstaged) change in the source worktree.
	// It belongs to no commit either, so the merge would drop it just the same.
	target := initTarget(t)
	featureDir := addFeatureWorktree(t, target)
	writeFile(t, featureDir, "feature.txt", "hello\n")
	gitRun(t, featureDir, "add", ".")
	gitRun(t, featureDir, "commit", "-q", "-m", "add feature.txt")
	writeFile(t, featureDir, "base.txt", "staged edit\n")
	gitRun(t, featureDir, "add", "base.txt")

	sink := &recordingSink{}
	e := newTestDriver(t, sink)
	req := withRun(t, sink, Request{Workspace: "/ws/staged-ws", Name: "staged-ws", SourceBranch: "feature", SourceDir: featureDir, TargetDir: target})

	// Act.
	_, err := e.Merge(context.Background(), req)

	// Assert.
	if err == nil {
		t.Fatalf("Merge() err = nil; want a staged-dirty-source precondition failure")
	}
	if !strings.Contains(err.Error(), "staged=true") {
		t.Errorf("Merge() err = %v; want it to name the staged dirtiness", err)
	}
	if len(sink.got) != 0 {
		t.Fatalf("Merge() emitted %v on a failed precondition; want state intact (none)", sink.phases())
	}
}

// --- no-op merges: the work is already on the target --------------------

func TestMergeEmptyRangeReportsAlreadyIncorporated(t *testing.T) {
	// Arrange: a source branch that never committed anything, so the range is
	// empty by ancestry.
	target := initTarget(t)
	featureDir := addFeatureWorktree(t, target)

	sink := &recordingSink{}
	e := newTestDriver(t, sink)
	req := withRun(t, sink, Request{Workspace: "/ws/empty-ws", Name: "empty-ws", SourceBranch: "feature", SourceDir: featureDir, TargetDir: target})

	// Act.
	res, err := e.Merge(context.Background(), req)

	// Assert: a successful no-op, and the phases still say so out loud.
	if err != nil {
		t.Fatalf("Merge() err = %v", err)
	}
	if res.Outcome != OutcomeMerged || !res.AlreadyIncorporated {
		t.Fatalf("Merge() res = %+v, want merged + AlreadyIncorporated", res)
	}
	assertPhases(t, sink.phases(), PhaseMerged)
}

func TestMergeRangeAlreadyIncorporatedByCherryPickBase(t *testing.T) {
	// Arrange: a PRIOR `cherry-pick -x` already landed the work on the target
	// under a new SHA. The -x annotation is what lets the base probe see it, so
	// the range collapses to empty rather than replaying the patch twice.
	target := initTarget(t)
	featureDir := addFeatureWorktree(t, target)
	writeFile(t, featureDir, "feature.txt", "hello\n")
	gitRun(t, featureDir, "add", ".")
	gitRun(t, featureDir, "commit", "-q", "-m", "add feature.txt")
	gitRun(t, target, "cherry-pick", "-x", "feature")
	headBefore := strings.TrimSpace(gitRun(t, target, "rev-parse", "HEAD"))

	sink := &recordingSink{}
	e := newTestDriver(t, sink)
	req := withRun(t, sink, Request{Workspace: "/ws/inc-ws", Name: "inc-ws", SourceBranch: "feature", SourceDir: featureDir, TargetDir: target})

	// Act.
	res, err := e.Merge(context.Background(), req)

	// Assert.
	if err != nil {
		t.Fatalf("Merge() err = %v", err)
	}
	if res.Outcome != OutcomeMerged || !res.AlreadyIncorporated {
		t.Fatalf("Merge() res = %+v, want merged + AlreadyIncorporated", res)
	}
	if head := strings.TrimSpace(gitRun(t, target, "rev-parse", "HEAD")); head != headBefore {
		t.Errorf("target HEAD moved from %s to %s; a no-op merge must not replay the patch", headBefore, head)
	}
	assertPhases(t, sink.phases(), PhaseMerged)
}

func TestMergeRangeAlreadyIncorporatedByPatchID(t *testing.T) {
	// Arrange: the range's only non-merge commit is already on the target under
	// a DIFFERENT sha and with NO -x annotation, so the base probe cannot see
	// it. The range leads with a merge commit, which `cherry-pick` refuses
	// outright — a non-zero exit with no CHERRY_PICK_HEAD. Only the patch-id
	// probe can tell that apart from real un-applied work.
	target := initTarget(t)
	featureDir := addFeatureWorktree(t, target)
	// feature: side commit S, merged back as M, then F2.
	gitRun(t, featureDir, "checkout", "-q", "-b", "side")
	writeFile(t, featureDir, "s.txt", "s\n")
	gitRun(t, featureDir, "add", ".")
	gitRun(t, featureDir, "commit", "-q", "-m", "S")
	gitRun(t, featureDir, "checkout", "-q", "feature")
	gitRun(t, featureDir, "merge", "-q", "--no-ff", "-m", "merge side", "side")
	writeFile(t, featureDir, "f2.txt", "f2\n")
	gitRun(t, featureDir, "add", ".")
	gitRun(t, featureDir, "commit", "-q", "-m", "F2")
	// target: fast-forward onto S (so the merge-base is S and the range leads
	// with M), then apply F2's patch by hand under its own sha.
	gitRun(t, target, "merge", "-q", "--ff-only", "side")
	writeFile(t, target, "f2.txt", "f2\n")
	gitRun(t, target, "add", ".")
	gitRun(t, target, "commit", "-q", "-m", "target already carries f2")

	sink := &recordingSink{}
	e := newTestDriver(t, sink)
	req := withRun(t, sink, Request{Workspace: "/ws/pid-ws", Name: "pid-ws", SourceBranch: "feature", SourceDir: featureDir, TargetDir: target})

	// Act.
	res, err := e.Merge(context.Background(), req)

	// Assert.
	if err != nil {
		t.Fatalf("Merge() err = %v", err)
	}
	if res.Outcome != OutcomeMerged || !res.AlreadyIncorporated {
		t.Fatalf("Merge() res = %+v, want merged + AlreadyIncorporated via the patch-id probe", res)
	}
	assertPhases(t, sink.phases(), PhaseMerged)
}

// --- a dirty or colliding TARGET tree -----------------------------------

func TestMergeTargetDirtyUnstagedFails(t *testing.T) {
	// Arrange: the target has an uncommitted edit to a file the pick rewrites.
	// Git refuses rather than clobbering it, and there is no conflict to
	// resolve, so the merge is failed and NOT parked.
	target := initTarget(t)
	featureDir := addFeatureWorktree(t, target)
	writeFile(t, featureDir, "base.txt", "feature\n")
	gitRun(t, featureDir, "add", ".")
	gitRun(t, featureDir, "commit", "-q", "-m", "feature edits base")
	writeFile(t, target, "base.txt", "local uncommitted work\n")

	sink := &recordingSink{}
	e := newTestDriver(t, sink)
	req := withRun(t, sink, Request{Workspace: "/ws/tdirty-ws", Name: "tdirty-ws", SourceBranch: "feature", SourceDir: featureDir, TargetDir: target})

	// Act.
	res, err := e.Merge(context.Background(), req)

	// Assert.
	if err != nil {
		t.Fatalf("Merge() err = %v", err)
	}
	if res.Outcome != OutcomeFailed {
		t.Fatalf("Merge() outcome = %s, want failed", res.Outcome)
	}
	if got := readFile(t, target, "base.txt"); got != "local uncommitted work\n" {
		t.Errorf("target base.txt = %q; the refused merge overwrote uncommitted work", got)
	}
	assertPhases(t, sink.phases(), PhaseMerging, PhaseMergeFailed)
}

func TestMergeTargetDirtyStagedFails(t *testing.T) {
	// Arrange: same collision, but the target's edit is STAGED. Git reports it
	// differently ("your local changes would be overwritten by cherry-pick"),
	// and the classification must land the same way.
	target := initTarget(t)
	featureDir := addFeatureWorktree(t, target)
	writeFile(t, featureDir, "base.txt", "feature\n")
	gitRun(t, featureDir, "add", ".")
	gitRun(t, featureDir, "commit", "-q", "-m", "feature edits base")
	writeFile(t, target, "base.txt", "staged local work\n")
	gitRun(t, target, "add", "base.txt")

	sink := &recordingSink{}
	e := newTestDriver(t, sink)
	req := withRun(t, sink, Request{Workspace: "/ws/tstaged-ws", Name: "tstaged-ws", SourceBranch: "feature", SourceDir: featureDir, TargetDir: target})

	// Act.
	res, err := e.Merge(context.Background(), req)

	// Assert.
	if err != nil {
		t.Fatalf("Merge() err = %v", err)
	}
	if res.Outcome != OutcomeFailed {
		t.Fatalf("Merge() outcome = %s, want failed", res.Outcome)
	}
	if cherryPickHeadPresent(t, target) {
		t.Errorf("CHERRY_PICK_HEAD present; a refused pick is not a resolvable conflict")
	}
	assertPhases(t, sink.phases(), PhaseMerging, PhaseMergeFailed)
}

func TestMergeUntrackedCollisionFails(t *testing.T) {
	// Arrange: an UNTRACKED target file with the same name a later commit in
	// the range adds. Git applies the earlier commit and then refuses, so the
	// merge is partially applied — and must still be reported failed rather
	// than merged, because the rest of the range never landed.
	target := initTarget(t)
	featureDir := addFeatureWorktree(t, target)
	writeFile(t, featureDir, "base.txt", "feature\n")
	gitRun(t, featureDir, "add", ".")
	gitRun(t, featureDir, "commit", "-q", "-m", "feature edits base")
	writeFile(t, featureDir, "new.txt", "from feature\n")
	gitRun(t, featureDir, "add", ".")
	gitRun(t, featureDir, "commit", "-q", "-m", "feature adds new.txt")
	writeFile(t, target, "new.txt", "untracked local file\n")

	sink := &recordingSink{}
	e := newTestDriver(t, sink)
	req := withRun(t, sink, Request{Workspace: "/ws/untracked-ws", Name: "untracked-ws", SourceBranch: "feature", SourceDir: featureDir, TargetDir: target})

	// Act.
	res, err := e.Merge(context.Background(), req)

	// Assert.
	if err != nil {
		t.Fatalf("Merge() err = %v", err)
	}
	if res.Outcome != OutcomeFailed {
		t.Fatalf("Merge() outcome = %s, want failed", res.Outcome)
	}
	if got := readFile(t, target, "new.txt"); got != "untracked local file\n" {
		t.Errorf("target new.txt = %q; the refused merge clobbered an untracked file", got)
	}
	if tags := strings.TrimSpace(gitRun(t, target, "tag", "-l", "merge/untracked-ws")); tags != "" {
		t.Errorf("merge/untracked-ws tagged on a failed merge; git tag -l = %q", tags)
	}
	// The first commit landed and was tested; the second is what git refused.
	assertPhases(t, sink.phases(), PhaseMerging, PhaseMerging, PhaseMerging, PhaseMergeFailed)
}

// --- the resumed run's commit cursor ------------------------------------

// lastMergedStatus returns the terminal merged arm the sink recorded.
func lastMergedStatus(t *testing.T, sink *recordingSink) *frontendv1.MergeStatusMerged {
	t.Helper()
	for i := len(sink.statuses) - 1; i >= 0; i-- {
		if merged := sink.statuses[i].GetMerged(); merged != nil {
			return merged
		}
	}
	t.Fatalf("no merged status was published; statuses = %v", sink.phases())
	return nil
}

// THE REGRESSION THIS PINS: a resume recomputes its cherry-pick base, which has
// advanced past the commit the resolution just landed, so the range it reads is
// EMPTY. Recording that as the run's plan made the terminal `merged` status
// report commits_total=0 for a merge that plainly landed a commit.
func TestResumeReportsTheRunsWholeRangeRatherThanTheEmptyRemainder(t *testing.T) {
	// Arrange — a single-commit branch parked on a conflict, then resolved.
	target := initTarget(t)
	featureDir := addFeatureWorktree(t, target)
	writeFile(t, featureDir, "base.txt", "feature\n")
	gitRun(t, featureDir, "add", ".")
	gitRun(t, featureDir, "commit", "-q", "-m", "feature edit")
	writeFile(t, target, "base.txt", "target\n")
	gitRun(t, target, "add", ".")
	gitRun(t, target, "commit", "-q", "-m", "target edit")

	sink := &recordingSink{}
	e := newTestDriver(t, sink)
	req := withRun(t, sink, Request{Workspace: "/ws/rz-total", Name: "rz-total", SourceBranch: "feature", SourceDir: featureDir, TargetDir: target})
	if res, err := e.Merge(context.Background(), req); err != nil || res.Outcome != OutcomeConflict {
		t.Fatalf("setup Merge() res=%+v err=%v, want conflict", res, err)
	}
	writeFile(t, target, "base.txt", "resolved\n")
	gitRun(t, target, "add", "base.txt")

	// Act.
	res, err := e.Resume(context.Background(), req)

	// Assert.
	if err != nil {
		t.Fatalf("Resume() err = %v", err)
	}
	if res.Outcome != OutcomeMerged {
		t.Fatalf("Resume() outcome = %s, want merged", res.Outcome)
	}
	if got := lastMergedStatus(t, sink).GetCommitsTotal(); got != 1 {
		t.Fatalf("the merged status reports commits_total = %d, want 1 (the branch's single commit)", got)
	}
}

// The bounce edge: a resume driven by a publisher REBUILT from nothing (the
// process that held the cursor is gone) has no total to keep, so it counts the
// workspace's whole contribution off git rather than reporting zero.
func TestResumeRebuildsTheCursorForAPublisherThatLostIt(t *testing.T) {
	// Arrange — two commits, the FIRST clean and the second colliding, so a
	// rebuilt cursor that merely counted the remainder would report 1, not 2.
	target := initTarget(t)
	featureDir := addFeatureWorktree(t, target)
	writeFile(t, featureDir, "feature.txt", "hello\n")
	gitRun(t, featureDir, "add", ".")
	gitRun(t, featureDir, "commit", "-q", "-m", "feature adds its own file")
	writeFile(t, featureDir, "base.txt", "feature\n")
	gitRun(t, featureDir, "add", ".")
	gitRun(t, featureDir, "commit", "-q", "-m", "feature edit")
	writeFile(t, target, "base.txt", "target\n")
	gitRun(t, target, "add", ".")
	gitRun(t, target, "commit", "-q", "-m", "target edit")

	sink := &recordingSink{}
	e := newTestDriver(t, sink)
	req := withRun(t, sink, Request{Workspace: "/ws/rz-bounce", Name: "rz-bounce", SourceBranch: "feature", SourceDir: featureDir, TargetDir: target})
	if res, err := e.Merge(context.Background(), req); err != nil || res.Outcome != OutcomeConflict {
		t.Fatalf("setup Merge() res=%+v err=%v, want conflict", res, err)
	}
	writeFile(t, target, "base.txt", "resolved\n")
	gitRun(t, target, "add", "base.txt")
	// The daemon that was publishing this run is gone; its successor rebuilds
	// the publisher from the durable run id alone.
	rebuilt, err := ResumeRunStatus(sink, t.Logf, req.Workspace, testClock(), req.Run.RunID())
	if err != nil {
		t.Fatalf("ResumeRunStatus: %v", err)
	}
	req.Run = rebuilt

	// Act.
	res, err := e.Resume(context.Background(), req)

	// Assert.
	if err != nil {
		t.Fatalf("Resume() err = %v", err)
	}
	if res.Outcome != OutcomeMerged {
		t.Fatalf("Resume() outcome = %s, want merged", res.Outcome)
	}
	if got := lastMergedStatus(t, sink).GetCommitsTotal(); got != 2 {
		t.Fatalf("the rebuilt run reports commits_total = %d, want 2 (the branch's whole range)", got)
	}
}

// --- resume that hits a further conflict --------------------------------

func TestResumeSecondConflictReEmitsConflict(t *testing.T) {
	// Arrange: two feature commits that both touch the contended line, so
	// resolving the first only exposes the second. Resume must park again
	// rather than report the merge finished.
	target := initTarget(t)
	featureDir := addFeatureWorktree(t, target)
	writeFile(t, featureDir, "base.txt", "feature one\n")
	gitRun(t, featureDir, "add", ".")
	gitRun(t, featureDir, "commit", "-q", "-m", "feature edit 1")
	writeFile(t, featureDir, "base.txt", "feature two\n")
	gitRun(t, featureDir, "add", ".")
	gitRun(t, featureDir, "commit", "-q", "-m", "feature edit 2")
	writeFile(t, target, "base.txt", "target\n")
	gitRun(t, target, "add", ".")
	gitRun(t, target, "commit", "-q", "-m", "target edit")

	sink := &recordingSink{}
	e := newTestDriver(t, sink)
	req := withRun(t, sink, Request{Workspace: "/ws/rz2-ws", Name: "rz2-ws", SourceBranch: "feature", SourceDir: featureDir, TargetDir: target})

	first, err := e.Merge(context.Background(), req)
	if err != nil || first.Outcome != OutcomeConflict {
		t.Fatalf("setup Merge() res=%+v err=%v, want conflict", first, err)
	}
	// A human resolves the FIRST conflict only.
	writeFile(t, target, "base.txt", "resolved one\n")
	gitRun(t, target, "add", "base.txt")

	// Act.
	res, err := e.Resume(context.Background(), req)

	// Assert.
	if err != nil {
		t.Fatalf("Resume() err = %v", err)
	}
	if res.Outcome != OutcomeConflict {
		t.Fatalf("Resume() outcome = %s, want conflict on the next commit", res.Outcome)
	}
	if res.ConflictCommit == first.ConflictCommit {
		t.Errorf("Resume() re-reported the first conflict %s; want the NEXT commit", res.ConflictCommit)
	}
	if !cherryPickHeadPresent(t, target) {
		t.Errorf("CHERRY_PICK_HEAD absent — the second conflict was not left in tree")
	}
	if tags := strings.TrimSpace(gitRun(t, target, "tag", "-l", "merge/rz2-ws")); tags != "" {
		t.Errorf("merge/rz2-ws tagged while still conflicted; git tag -l = %q", tags)
	}
	assertPhases(t, sink.phases(), PhaseMerging, PhaseMergeConflict, PhaseMerging, PhaseMerging, PhaseMerging, PhaseMergeConflict)
}

// --- sink-error propagation ---------------------------------------------

func TestMergeSinkFailureOnMergingAbortsBeforeThePick(t *testing.T) {
	// Arrange: the sink rejects the OPENING merging transition. The driver must
	// surface it and never run the cherry-pick, so the target is untouched.
	target := initTarget(t)
	featureDir := addFeatureWorktree(t, target)
	writeFile(t, featureDir, "feature.txt", "hello\n")
	gitRun(t, featureDir, "add", ".")
	gitRun(t, featureDir, "commit", "-q", "-m", "add feature.txt")
	headBefore := strings.TrimSpace(gitRun(t, target, "rev-parse", "HEAD"))

	sink := &recordingSink{failOn: PhaseMerging}
	e := newTestDriver(t, sink)
	req := withRun(t, sink, Request{Workspace: "/ws/sink0-ws", Name: "sink0-ws", SourceBranch: "feature", SourceDir: featureDir, TargetDir: target})

	// Act.
	_, err := e.Merge(context.Background(), req)

	// Assert.
	if err == nil {
		t.Fatalf("Merge() err = nil; want the sink error surfaced")
	}
	if !strings.Contains(err.Error(), string(errFakeSink)) {
		t.Errorf("Merge() err = %v; want it to wrap the sink error", err)
	}
	if head := strings.TrimSpace(gitRun(t, target, "rev-parse", "HEAD")); head != headBefore {
		t.Errorf("target HEAD moved to %s; the pick ran despite the aborted transition", head)
	}
	if _, statErr := os.Stat(filepath.Join(target, "feature.txt")); statErr == nil {
		t.Errorf("feature.txt landed despite the aborted transition")
	}
	assertPhases(t, sink.phases(), PhaseMerging)
}

func TestResumeSurfacesSinkError(t *testing.T) {
	// Arrange: a resolved conflict whose resume-time merging transition the
	// sink rejects. Resume must abort with the error rather than continue the
	// pick behind an unrecorded state.
	target := initTarget(t)
	featureDir := addFeatureWorktree(t, target)
	writeFile(t, featureDir, "base.txt", "feature\n")
	gitRun(t, featureDir, "add", ".")
	gitRun(t, featureDir, "commit", "-q", "-m", "feature edit")
	writeFile(t, target, "base.txt", "target\n")
	gitRun(t, target, "add", ".")
	gitRun(t, target, "commit", "-q", "-m", "target edit")

	sink := &recordingSink{}
	e := newTestDriver(t, sink)
	req := withRun(t, sink, Request{Workspace: "/ws/rsink-ws", Name: "rsink-ws", SourceBranch: "feature", SourceDir: featureDir, TargetDir: target})
	if res, err := e.Merge(context.Background(), req); err != nil || res.Outcome != OutcomeConflict {
		t.Fatalf("setup Merge() res=%+v err=%v, want conflict", res, err)
	}
	writeFile(t, target, "base.txt", "resolved\n")
	gitRun(t, target, "add", "base.txt")
	sink.failOn = PhaseMerging

	// Act.
	_, err := e.Resume(context.Background(), req)

	// Assert.
	if err == nil {
		t.Fatalf("Resume() err = nil; want the sink error surfaced")
	}
	if !strings.Contains(err.Error(), string(errFakeSink)) {
		t.Errorf("Resume() err = %v; want it to wrap the sink error", err)
	}
	if !cherryPickHeadPresent(t, target) {
		t.Errorf("CHERRY_PICK_HEAD gone; Resume continued the pick despite the aborted transition")
	}
}

func TestMergeSurfacesSinkError(t *testing.T) {
	// Arrange: a clean cherry-pick, but the sink rejects the terminal merged
	// transition. The driver must surface (not swallow) it.
	target := initTarget(t)
	featureDir := addFeatureWorktree(t, target)
	writeFile(t, featureDir, "feature.txt", "hello\n")
	gitRun(t, featureDir, "add", ".")
	gitRun(t, featureDir, "commit", "-q", "-m", "add feature.txt")

	sink := &recordingSink{failOn: PhaseMerged}
	e := newTestDriver(t, sink)
	req := withRun(t, sink, Request{Workspace: "/ws/sink-ws", Name: "sink-ws", SourceBranch: "feature", SourceDir: featureDir, TargetDir: target})

	// Act.
	_, err := e.Merge(context.Background(), req)

	// Assert.
	if err == nil {
		t.Fatalf("Merge() err = nil; want the sink error surfaced")
	}
	if !strings.Contains(err.Error(), string(errFakeSink)) {
		t.Errorf("Merge() err = %v; want it to wrap the sink error", err)
	}
}

// --- helpers ------------------------------------------------------------

// --- what the published statuses say about the picks --------------------

// firstStatusOnArm returns the first recorded status whose oneof arm matches
// the predicate, or nil.
func firstCherryPicking(statuses []*frontendv1.MergeStatus) *frontendv1.MergeStatusCherryPicking {
	for _, s := range statuses {
		if pick := s.GetCherryPicking(); pick != nil {
			return pick
		}
	}
	return nil
}

func firstConflictStatus(statuses []*frontendv1.MergeStatus) *frontendv1.MergeStatusConflict {
	for _, s := range statuses {
		if c := s.GetConflict(); c != nil {
			return c
		}
	}
	return nil
}

// THE GUARANTEE: the FIRST cherry_picking status a run publishes already carries
// the plan's real size. The run used to open with one published before the plan
// was computed, so the first thing any frontend learned about a three-commit
// merge was that it was picking 0 of 0.
func TestTheFirstCherryPickingStatusCarriesThePlanSize(t *testing.T) {
	// Arrange — a branch with two commits, so the denominator is not 1 by luck.
	target := initTarget(t)
	featureDir := addFeatureWorktree(t, target)
	for _, name := range []string{"one.txt", "two.txt"} {
		writeFile(t, featureDir, name, "hello\n")
		gitRun(t, featureDir, "add", ".")
		gitRun(t, featureDir, "commit", "-q", "-m", "add "+name)
	}
	sink := &recordingSink{}
	e := newTestDriver(t, sink)
	req := withRun(t, sink, Request{Workspace: "/ws/plan-ws", Name: "plan-ws", SourceBranch: "feature", SourceDir: featureDir, TargetDir: target})

	// Act.
	if _, err := e.Merge(context.Background(), req); err != nil {
		t.Fatalf("Merge() err = %v", err)
	}

	// Assert.
	pick := firstCherryPicking(sink.statuses)
	if pick == nil {
		t.Fatal("the run published no cherry_picking status at all")
	}
	if pick.GetCommitsTotal() != 2 {
		t.Fatalf("the first cherry_picking status reports commits_total=%d, want 2 (the plan's real size)", pick.GetCommitsTotal())
	}
	if pick.GetCurrentSha() == "" {
		t.Fatal("the first cherry_picking status carries an empty current_sha, so a frontend cannot say which commit is in flight")
	}
}

// A NO-OP merge publishes no cherry_picking status. There is no commit in
// flight and no denominator, so the only honest cherry_picking status would be
// 0 of 0 — which reads to a frontend as a stalled merge rather than as one that
// had nothing to do.
func TestANoOpMergePublishesNoCherryPickingStatus(t *testing.T) {
	// Arrange — a source branch that never committed anything.
	target := initTarget(t)
	featureDir := addFeatureWorktree(t, target)
	sink := &recordingSink{}
	e := newTestDriver(t, sink)
	req := withRun(t, sink, Request{Workspace: "/ws/noop-ws", Name: "noop-ws", SourceBranch: "feature", SourceDir: featureDir, TargetDir: target})

	// Act.
	if _, err := e.Merge(context.Background(), req); err != nil {
		t.Fatalf("Merge() err = %v", err)
	}

	// Assert.
	if pick := firstCherryPicking(sink.statuses); pick != nil {
		t.Fatalf("a no-op merge published a cherry_picking status reporting %d of %d",
			pick.GetCommitsLanded(), pick.GetCommitsTotal())
	}
}

// THE GUARANTEE: conflicted_sha is the FULL sha. It is the one field a human
// uses to go find the collision, and an abbreviated sha is only unique in the
// repository that abbreviated it — a frontend cannot address the commit with it
// anywhere else.
func TestTheConflictStatusNamesTheFullShaOfTheCollidingCommit(t *testing.T) {
	// Arrange — a feature edit that collides with a target edit to one file.
	target := initTarget(t)
	featureDir := addFeatureWorktree(t, target)
	writeFile(t, featureDir, "base.txt", "feature\n")
	gitRun(t, featureDir, "add", ".")
	gitRun(t, featureDir, "commit", "-q", "-m", "feature edit")
	wantSHA := strings.TrimSpace(gitRun(t, featureDir, "rev-parse", "HEAD"))
	writeFile(t, target, "base.txt", "target\n")
	gitRun(t, target, "add", ".")
	gitRun(t, target, "commit", "-q", "-m", "target edit")

	sink := &recordingSink{}
	e := newTestDriver(t, sink)
	req := withRun(t, sink, Request{Workspace: "/ws/full-sha-ws", Name: "full-sha-ws", SourceBranch: "feature", SourceDir: featureDir, TargetDir: target})

	// Act.
	res, err := e.Merge(context.Background(), req)
	if err != nil {
		t.Fatalf("Merge() err = %v", err)
	}
	if res.Outcome != OutcomeConflict {
		t.Fatalf("Merge() outcome = %s, want conflict", res.Outcome)
	}

	// Assert.
	conflict := firstConflictStatus(sink.statuses)
	if conflict == nil {
		t.Fatal("the parked run published no conflict status")
	}
	if got := conflict.GetConflictedSha(); got != wantSHA {
		t.Fatalf("conflicted_sha = %q, want the full sha %q", got, wantSHA)
	}
}

func assertPhases(t *testing.T, got []Phase, want ...Phase) {
	t.Helper()
	if len(got) != len(want) {
		t.Fatalf("transition phases = %v, want %v", got, want)
	}
	for i := range want {
		if got[i] != want[i] {
			t.Fatalf("transition phases = %v, want %v", got, want)
		}
	}
}

func cherryPickHeadPresent(t *testing.T, dir string) bool {
	t.Helper()
	gitDir := strings.TrimSpace(gitRun(t, dir, "rev-parse", "--absolute-git-dir"))
	_, err := os.Stat(filepath.Join(gitDir, "CHERRY_PICK_HEAD"))
	return err == nil
}

func readFile(t *testing.T, dir, name string) string {
	t.Helper()
	b, err := os.ReadFile(filepath.Join(dir, name))
	if err != nil {
		t.Fatalf("read %s: %v", name, err)
	}
	return string(b)
}

// A target carrying only SEQUENCER residue (a multi-commit pick interrupted
// between commits — no CHERRY_PICK_HEAD) makes git refuse every new pick with
// an opaque exit 128. The driver must reject it as a NAMED precondition,
// before any transition, with the operator's way out in the message.
func TestMergeRefusesATargetWithStaleSequencerState(t *testing.T) {
	// Arrange — a normal fixture pair, plus hand-planted sequencer residue.
	target := initTarget(t)
	featureDir := addFeatureWorktree(t, target)
	writeFile(t, featureDir, "feature.txt", "hello\n")
	gitRun(t, featureDir, "add", ".")
	gitRun(t, featureDir, "commit", "-q", "-m", "add feature.txt")
	seqDir := strings.TrimSpace(gitRun(t, target, "rev-parse", "--git-path", "sequencer"))
	if !filepath.IsAbs(seqDir) {
		seqDir = filepath.Join(target, seqDir)
	}
	if err := os.MkdirAll(seqDir, 0o755); err != nil {
		t.Fatalf("plant sequencer dir: %v", err)
	}

	sink := &recordingSink{}
	e := newTestDriver(t, sink)
	req := withRun(t, sink, Request{Workspace: "/ws/seq-ws", Name: "seq-ws", SourceBranch: "feature", SourceDir: featureDir, TargetDir: target})

	// Act.
	_, err := e.Merge(context.Background(), req)

	// Assert — refused pre-transition, naming the unfinished pick.
	if err == nil || !strings.Contains(err.Error(), "unfinished cherry-pick") {
		t.Fatalf("Merge() err = %v, want the unfinished-cherry-pick precondition", err)
	}
	if len(sink.got) != 0 {
		t.Fatalf("Merge() emitted %v before the precondition; want none", sink.phases())
	}
}

// --- the per-commit test gate -------------------------------------------

func TestMergeRunsTheSuiteOncePerLandedCommit(t *testing.T) {
	// Arrange: three orthogonal commits on the source branch.
	target := initTarget(t)
	featureDir := addFeatureWorktree(t, target)
	for _, name := range []string{"one.txt", "two.txt", "three.txt"} {
		writeFile(t, featureDir, name, name+"\n")
		gitRun(t, featureDir, "add", ".")
		gitRun(t, featureDir, "commit", "-q", "-m", "add "+name)
	}

	sink := &recordingSink{}
	suite := &fakeSuite{}
	e := newTestDriverWithSuite(t, sink, suite)
	req := withRun(t, sink, Request{Workspace: "/ws/gate-ws", Name: "gate-ws", SourceBranch: "feature", SourceDir: featureDir, TargetDir: target})

	// Act.
	res, err := e.Merge(context.Background(), req)

	// Assert — one run per commit, all against the target worktree.
	if err != nil {
		t.Fatalf("Merge() err = %v", err)
	}
	if res.Outcome != OutcomeMerged {
		t.Fatalf("Merge() outcome = %s, want merged", res.Outcome)
	}
	if suite.calls != 3 {
		t.Fatalf("suite runs = %d, want 3 (one per landed commit)", suite.calls)
	}
	for i, dir := range suite.targets {
		if dir != target {
			t.Errorf("suite run %d ran against %q, want the target %q", i, dir, target)
		}
	}
}

func TestMergeOrdersEachSuiteRunAfterItsOwnCommit(t *testing.T) {
	// The gate is only a gate if it runs on the tree the pick produced, so the
	// interleaving (pick, test, pick, test) is pinned rather than merely the
	// count.
	// Arrange.
	target := initTarget(t)
	featureDir := addFeatureWorktree(t, target)
	for _, name := range []string{"one.txt", "two.txt"} {
		writeFile(t, featureDir, name, name+"\n")
		gitRun(t, featureDir, "add", ".")
		gitRun(t, featureDir, "commit", "-q", "-m", "add "+name)
	}

	sink := &recordingSink{}
	var seen []string
	suite := &fakeSuite{}
	observing := suiteFunc(func(ctx context.Context, dir string) (SuiteResult, error) {
		files, err := os.ReadDir(dir)
		if err != nil {
			t.Fatalf("read target: %v", err)
		}
		var landed int
		for _, f := range files {
			if f.Name() == "one.txt" || f.Name() == "two.txt" {
				landed++
			}
		}
		seen = append(seen, fmt.Sprintf("%d", landed))
		return suite.RunSuite(ctx, dir)
	})
	e := newTestDriverWithSuite(t, sink, observing)
	req := withRun(t, sink, Request{Workspace: "/ws/order-ws", Name: "order-ws", SourceBranch: "feature", SourceDir: featureDir, TargetDir: target})

	// Act.
	if _, err := e.Merge(context.Background(), req); err != nil {
		t.Fatalf("Merge() err = %v", err)
	}

	// Assert — the first run saw one commit's file, the second saw both.
	if len(seen) != 2 || seen[0] != "1" || seen[1] != "2" {
		t.Fatalf("files present at each suite run = %v, want [1 2]", seen)
	}
}

func TestMergeSkipsTheGateWhenTheTargetDeclaresNoEntrypoint(t *testing.T) {
	// Arrange — the real runner against a fixture repo, which has no
	// modules/app/agent-repl/bin/test-all.sh.
	target := initTarget(t)
	featureDir := addFeatureWorktree(t, target)
	writeFile(t, featureDir, "feature.txt", "hello\n")
	gitRun(t, featureDir, "add", ".")
	gitRun(t, featureDir, "commit", "-q", "-m", "add feature.txt")

	var logged []string
	runner, err := NewRepoSuiteRunner(func(format string, args ...any) {
		logged = append(logged, fmt.Sprintf(format, args...))
	})
	if err != nil {
		t.Fatalf("NewRepoSuiteRunner: %v", err)
	}
	sink := &recordingSink{}
	e := newTestDriverWithSuite(t, sink, runner)
	req := withRun(t, sink, Request{Workspace: "/ws/noentry-ws", Name: "noentry-ws", SourceBranch: "feature", SourceDir: featureDir, TargetDir: target})

	// Act.
	res, err := e.Merge(context.Background(), req)

	// Assert — the merge lands, and the absence is named out loud.
	if err != nil {
		t.Fatalf("Merge() err = %v", err)
	}
	if res.Outcome != OutcomeMerged {
		t.Fatalf("Merge() outcome = %s, want merged", res.Outcome)
	}
	var skipped bool
	for _, line := range logged {
		if strings.Contains(line, "suite SKIPPED") && strings.Contains(line, "no test entrypoint") {
			skipped = true
		}
	}
	if !skipped {
		t.Fatalf("no loud skip log naming the missing entrypoint; got %v", logged)
	}
}

func TestMergeReportsATestFailureWithTheFailingCommitAndTail(t *testing.T) {
	// Arrange — the suite fails on the first landed commit.
	target := initTarget(t)
	featureDir := addFeatureWorktree(t, target)
	writeFile(t, featureDir, "feature.txt", "hello\n")
	gitRun(t, featureDir, "add", ".")
	gitRun(t, featureDir, "commit", "-q", "-m", "add feature.txt")
	head := strings.TrimSpace(gitRun(t, target, "rev-parse", "HEAD"))

	sink := &recordingSink{}
	suite := &fakeSuite{verdicts: []SuiteResult{{Passed: false, Tail: "FAIL: agent-repl-suite"}}}
	e := newTestDriverWithSuite(t, sink, suite)
	req := withRun(t, sink, Request{Workspace: "/ws/tf-ws", Name: "tf-ws", SourceBranch: "feature", SourceDir: featureDir, TargetDir: target})

	// Act.
	res, err := e.Merge(context.Background(), req)

	// Assert — a non-terminal test failure carrying everything the coordinator
	// needs to prompt a fix and, failing that, to roll back.
	if err != nil {
		t.Fatalf("Merge() err = %v", err)
	}
	if res.Outcome != OutcomeTestFailed {
		t.Fatalf("Merge() outcome = %s, want test_failed", res.Outcome)
	}
	if res.FailingCommit == "" {
		t.Errorf("res.FailingCommit empty; want the landed commit's short SHA")
	}
	if res.TestFailureTail != "FAIL: agent-repl-suite" {
		t.Errorf("res.TestFailureTail = %q, want the suite's tail", res.TestFailureTail)
	}
	if res.PreMergeHead != head {
		t.Errorf("res.PreMergeHead = %q, want the pre-merge HEAD %q", res.PreMergeHead, head)
	}
	// No terminal transition: the coordinator classifies this one.
	assertPhases(t, sink.phases(), PhaseMerging, PhaseMerging)
}

func TestMergeStopsPickingAfterATestFailure(t *testing.T) {
	// Arrange — two commits, the suite fails after the first.
	target := initTarget(t)
	featureDir := addFeatureWorktree(t, target)
	for _, name := range []string{"one.txt", "two.txt"} {
		writeFile(t, featureDir, name, name+"\n")
		gitRun(t, featureDir, "add", ".")
		gitRun(t, featureDir, "commit", "-q", "-m", "add "+name)
	}

	sink := &recordingSink{}
	suite := &fakeSuite{verdicts: []SuiteResult{{Passed: false, Tail: "boom"}}}
	e := newTestDriverWithSuite(t, sink, suite)
	req := withRun(t, sink, Request{Workspace: "/ws/stop-ws", Name: "stop-ws", SourceBranch: "feature", SourceDir: featureDir, TargetDir: target})

	// Act.
	if _, err := e.Merge(context.Background(), req); err != nil {
		t.Fatalf("Merge() err = %v", err)
	}

	// Assert — the second commit never landed.
	if _, statErr := os.Stat(filepath.Join(target, "two.txt")); statErr == nil {
		t.Errorf("the second commit landed after the first broke the suite")
	}
}

func TestMergeSurfacesAnUnrunnableSuite(t *testing.T) {
	// Arrange — the runner cannot classify the run at all.
	target := initTarget(t)
	featureDir := addFeatureWorktree(t, target)
	writeFile(t, featureDir, "feature.txt", "hello\n")
	gitRun(t, featureDir, "add", ".")
	gitRun(t, featureDir, "commit", "-q", "-m", "add feature.txt")

	sink := &recordingSink{}
	suite := &fakeSuite{err: sentinelError("suite binary vanished")}
	e := newTestDriverWithSuite(t, sink, suite)
	req := withRun(t, sink, Request{Workspace: "/ws/unrun-ws", Name: "unrun-ws", SourceBranch: "feature", SourceDir: featureDir, TargetDir: target})

	// Act.
	_, err := e.Merge(context.Background(), req)

	// Assert — surfaced, never reported as a passing (or failing) gate.
	if err == nil {
		t.Fatalf("Merge() err = nil; want the unrunnable suite surfaced")
	}
	if !strings.Contains(err.Error(), "suite binary vanished") {
		t.Errorf("Merge() err = %v; want it to wrap the runner's error", err)
	}
}

// --- resuming after a fix -----------------------------------------------

func TestContinueAfterTestFixCommitsTheStagedFixAndFinishesTheRange(t *testing.T) {
	// Arrange — one commit lands, the suite fails, an agent stages a fix.
	target := initTarget(t)
	featureDir := addFeatureWorktree(t, target)
	writeFile(t, featureDir, "feature.txt", "hello\n")
	gitRun(t, featureDir, "add", ".")
	gitRun(t, featureDir, "commit", "-q", "-m", "add feature.txt")

	sink := &recordingSink{}
	suite := &fakeSuite{verdicts: []SuiteResult{{Passed: false, Tail: "boom"}, {Passed: true}}}
	e := newTestDriverWithSuite(t, sink, suite)
	req := withRun(t, sink, Request{Workspace: "/ws/fix-ws", Name: "fix-ws", SourceBranch: "feature", SourceDir: featureDir, TargetDir: target})
	first, err := e.Merge(context.Background(), req)
	if err != nil || first.Outcome != OutcomeTestFailed {
		t.Fatalf("setup Merge() res=%+v err=%v, want test_failed", first, err)
	}
	headAfterPick := strings.TrimSpace(gitRun(t, target, "rev-parse", "HEAD"))
	writeFile(t, target, "feature.txt", "fixed\n")
	gitRun(t, target, "add", "feature.txt")

	// Act.
	res, err := e.ContinueAfterTestFix(context.Background(), req, first.FailingCommit)

	// Assert — the fix is a FOLLOW-UP commit (the picked commit's SHA stands),
	// and the merge completes.
	if err != nil {
		t.Fatalf("ContinueAfterTestFix() err = %v", err)
	}
	if res.Outcome != OutcomeMerged {
		t.Fatalf("ContinueAfterTestFix() outcome = %s, want merged", res.Outcome)
	}
	if parent := strings.TrimSpace(gitRun(t, target, "rev-parse", "HEAD^")); parent != headAfterPick {
		t.Errorf("HEAD^ = %s, want the untouched picked commit %s (the fix must not amend it)", parent, headAfterPick)
	}
	if body := gitRun(t, target, "log", "-1", "--pretty=%s"); !strings.Contains(body, "fix tests after cherry-pick of "+first.FailingCommit) {
		t.Errorf("follow-up commit subject = %q, want it to name the failing commit", body)
	}
}

func TestContinueAfterTestFixReportsAStillFailingSuite(t *testing.T) {
	// Arrange — the agent's attempt does not fix anything.
	target := initTarget(t)
	featureDir := addFeatureWorktree(t, target)
	writeFile(t, featureDir, "feature.txt", "hello\n")
	gitRun(t, featureDir, "add", ".")
	gitRun(t, featureDir, "commit", "-q", "-m", "add feature.txt")

	sink := &recordingSink{}
	suite := &fakeSuite{verdicts: []SuiteResult{{Passed: false, Tail: "still boom"}}}
	e := newTestDriverWithSuite(t, sink, suite)
	req := withRun(t, sink, Request{Workspace: "/ws/nofix-ws", Name: "nofix-ws", SourceBranch: "feature", SourceDir: featureDir, TargetDir: target})
	first, err := e.Merge(context.Background(), req)
	if err != nil || first.Outcome != OutcomeTestFailed {
		t.Fatalf("setup Merge() res=%+v err=%v, want test_failed", first, err)
	}

	// Act.
	res, err := e.ContinueAfterTestFix(context.Background(), req, first.FailingCommit)

	// Assert.
	if err != nil {
		t.Fatalf("ContinueAfterTestFix() err = %v", err)
	}
	if res.Outcome != OutcomeTestFailed {
		t.Fatalf("ContinueAfterTestFix() outcome = %s, want test_failed", res.Outcome)
	}
}

func TestResumeGatesTheResolvedCommitOnTheSuite(t *testing.T) {
	// A conflict a human resolved is exactly the kind of landing that breaks a
	// suite, so the resumed commit is gated like every other one.
	// Arrange.
	target := initTarget(t)
	featureDir := addFeatureWorktree(t, target)
	writeFile(t, featureDir, "base.txt", "feature\n")
	gitRun(t, featureDir, "add", ".")
	gitRun(t, featureDir, "commit", "-q", "-m", "feature edit")
	writeFile(t, target, "base.txt", "target\n")
	gitRun(t, target, "add", ".")
	gitRun(t, target, "commit", "-q", "-m", "target edit")

	sink := &recordingSink{}
	suite := &fakeSuite{verdicts: []SuiteResult{{Passed: false, Tail: "resolved badly"}}}
	e := newTestDriverWithSuite(t, sink, suite)
	req := withRun(t, sink, Request{Workspace: "/ws/rzgate-ws", Name: "rzgate-ws", SourceBranch: "feature", SourceDir: featureDir, TargetDir: target})
	if res, err := e.Merge(context.Background(), req); err != nil || res.Outcome != OutcomeConflict {
		t.Fatalf("setup Merge() res=%+v err=%v, want conflict", res, err)
	}
	writeFile(t, target, "base.txt", "resolved\n")
	gitRun(t, target, "add", "base.txt")

	// Act.
	res, err := e.Resume(context.Background(), req)

	// Assert.
	if err != nil {
		t.Fatalf("Resume() err = %v", err)
	}
	if res.Outcome != OutcomeTestFailed {
		t.Fatalf("Resume() outcome = %s, want test_failed on the resumed commit", res.Outcome)
	}
	if suite.calls != 1 {
		t.Errorf("suite runs = %d, want 1 (the resumed commit)", suite.calls)
	}
}

// --- rollback -----------------------------------------------------------

func TestRollbackReturnsTheTargetToItsPreMergeHead(t *testing.T) {
	// Arrange — a merge that landed a commit and then broke the suite.
	target := initTarget(t)
	featureDir := addFeatureWorktree(t, target)
	writeFile(t, featureDir, "feature.txt", "hello\n")
	gitRun(t, featureDir, "add", ".")
	gitRun(t, featureDir, "commit", "-q", "-m", "add feature.txt")

	sink := &recordingSink{}
	suite := &fakeSuite{verdicts: []SuiteResult{{Passed: false, Tail: "boom"}}}
	e := newTestDriverWithSuite(t, sink, suite)
	req := withRun(t, sink, Request{Workspace: "/ws/rb-ws", Name: "rb-ws", SourceBranch: "feature", SourceDir: featureDir, TargetDir: target})
	res, err := e.Merge(context.Background(), req)
	if err != nil || res.Outcome != OutcomeTestFailed {
		t.Fatalf("setup Merge() res=%+v err=%v, want test_failed", res, err)
	}

	// Act.
	if err := e.Rollback(context.Background(), req, res.PreMergeHead); err != nil {
		t.Fatalf("Rollback() err = %v", err)
	}

	// Assert — the target is exactly where the merge found it.
	if head := strings.TrimSpace(gitRun(t, target, "rev-parse", "HEAD")); head != res.PreMergeHead {
		t.Errorf("target HEAD = %s, want the pre-merge %s", head, res.PreMergeHead)
	}
	if _, statErr := os.Stat(filepath.Join(target, "feature.txt")); statErr == nil {
		t.Errorf("feature.txt survived the rollback")
	}
}

func TestRollbackWithoutAHeadIsRefused(t *testing.T) {
	// Arrange.
	target := initTarget(t)
	featureDir := addFeatureWorktree(t, target)
	e := newTestDriver(t, &recordingSink{})
	req := withRun(t, &recordingSink{}, Request{Workspace: "/ws/rb0-ws", Name: "rb0-ws", SourceBranch: "feature", SourceDir: featureDir, TargetDir: target})

	// Act.
	err := e.Rollback(context.Background(), req, "")

	// Assert — a rollback with no point to roll back to is a loud failure, not
	// a no-op that leaves the target broken in silence.
	if err == nil {
		t.Fatalf("Rollback() err = nil; want a refusal")
	}
}

// --- boot replay --------------------------------------------------------

func TestMergeReplayedAfterABounceSkipsTheCommitsThatAlreadyLanded(t *testing.T) {
	// A daemon that dies mid-loop leaves its durable queue entry behind, and
	// the next boot's Drain re-runs Merge from the top. The commits the dead
	// process landed must be no-ops rather than a second replay.
	// Arrange — two commits; the first "already landed" before the bounce.
	target := initTarget(t)
	featureDir := addFeatureWorktree(t, target)
	for _, name := range []string{"one.txt", "two.txt"} {
		writeFile(t, featureDir, name, name+"\n")
		gitRun(t, featureDir, "add", ".")
		gitRun(t, featureDir, "commit", "-q", "-m", "add "+name)
	}
	sink := &recordingSink{}
	suite := &fakeSuite{verdicts: []SuiteResult{{Passed: false, Tail: "the daemon died here"}}}
	e := newTestDriverWithSuite(t, sink, suite)
	req := withRun(t, sink, Request{Workspace: "/ws/replay-ws", Name: "replay-ws", SourceBranch: "feature", SourceDir: featureDir, TargetDir: target})
	if res, err := e.Merge(context.Background(), req); err != nil || res.Outcome != OutcomeTestFailed {
		t.Fatalf("setup Merge() res=%+v err=%v, want test_failed", res, err)
	}
	landedBefore := strings.TrimSpace(gitRun(t, target, "rev-parse", "HEAD"))

	// Act — the next boot replays the same request against a green suite.
	replaySink := &recordingSink{}
	replaySuite := &fakeSuite{}
	replay := newTestDriverWithSuite(t, replaySink, replaySuite)
	res, err := replay.Merge(context.Background(), req)

	// Assert — only the SECOND commit was picked and tested.
	if err != nil {
		t.Fatalf("replay Merge() err = %v", err)
	}
	if res.Outcome != OutcomeMerged {
		t.Fatalf("replay Merge() outcome = %s, want merged", res.Outcome)
	}
	if replaySuite.calls != 1 {
		t.Fatalf("replay suite runs = %d, want 1 (only the commit that had not landed)", replaySuite.calls)
	}
	if parent := strings.TrimSpace(gitRun(t, target, "rev-parse", "HEAD^")); parent != landedBefore {
		t.Errorf("HEAD^ = %s, want the pre-bounce landing %s; the replay re-applied a commit", parent, landedBefore)
	}
}

// suiteFunc adapts a plain function to SuiteRunner.
type suiteFunc func(ctx context.Context, targetDir string) (SuiteResult, error)

func (f suiteFunc) RunSuite(ctx context.Context, targetDir string) (SuiteResult, error) {
	return f(ctx, targetDir)
}
