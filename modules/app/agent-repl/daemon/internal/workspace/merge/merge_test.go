package merge

import (
	"context"
	"fmt"
	"os"
	"path/filepath"
	"reflect"
	"strings"
	"sync"
	"testing"
	"time"

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

// causes returns every recorded transition's cause, in order.
func (s *recordingSink) causes() []string {
	cs := make([]string, len(s.got))
	for i, t := range s.got {
		cs[i] = t.cause
	}
	return cs
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
	runs     []SuiteRun
	verdicts []SuiteResult
	err      error
	calls    int
}

// skipping is the harness default: a target repository with no test entrypoint,
// which is what every fixture repo in this file actually is.
func skippingSuite() *fakeSuite {
	return &fakeSuite{verdicts: []SuiteResult{{Skipped: true, Reason: "fixture repo declares no test entrypoint"}}}
}

func (f *fakeSuite) RunSuite(_ context.Context, targetDir string, run SuiteRun) (SuiteResult, error) {
	f.targets = append(f.targets, targetDir)
	f.runs = append(f.runs, run)
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

// parkedAt threads a parked Result's rebase worktree back onto the request,
// which is exactly what merge.Coordinator does before Resume and
// ContinueAfterTestFix. A driver step that works INSIDE an existing rebase
// cannot invent the tree it works in, and the alternative — falling back to the
// target — is the target-mutating behavior this design removed.
//
// It also registers the cleanup the coordinator owns in production, so a parked
// fixture never leaves a worktree registered in the repository.
func parkedAt(t *testing.T, e *Driver, req Request, res Result) Request {
	t.Helper()
	if res.WorkDir == "" || res.BaseHead == "" {
		t.Fatalf("parked Result names no rebase worktree: %+v", res)
	}
	req.WorkDir, req.BaseHead = res.WorkDir, res.BaseHead
	t.Cleanup(func() { _ = e.Cleanup(context.Background(), req) })
	return req
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
	// The landed commit carries the -x annotation naming the branch commit it
	// was replayed from. It is the merge commit's SECOND PARENT now, because the
	// target's own head is reached by a --no-ff merge rather than by the pick.
	if body := gitRun(t, target, "log", "-1", "--pretty=%B", "HEAD^2"); !strings.Contains(body, "cherry picked from commit") {
		t.Errorf("landed commit missing -x annotation:\n%s", body)
	}
	// Finalization: the merge/<ws> tag exists at HEAD.
	if tags := gitRun(t, target, "tag", "-l", "merge/clean-ws"); !strings.Contains(tags, "merge/clean-ws") {
		t.Errorf("tag merge/clean-ws not created; git tag -l = %q", tags)
	}
	if res.Tag != "merge/clean-ws" {
		t.Errorf("res.Tag = %q, want merge/clean-ws", res.Tag)
	}
	// A cherry_picking and a testing phase for each commit the replay picks, and
	// NOTHING ELSE. Nothing precedes them: a cherry_picking status published
	// before the plan exists could only ever report 0 of 0. Nothing follows them
	// either -- the terminal `merged` belongs to merge.Coordinator, which
	// publishes it once the workspace's after-action has run and can therefore
	// carry the action's outcome on it.
	assertPhases(t, sink.phases(), PhaseMerging, PhaseMerging)
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
	// The replay is left IN THE REBASE WORKTREE (never aborted), and the target
	// is untouched: CHERRY_PICK_HEAD is there and not here.
	req = parkedAt(t, e, req, res)
	if !cherryPickHeadPresent(t, req.WorkDir) {
		t.Errorf("CHERRY_PICK_HEAD absent — driver aborted the conflict instead of leaving it in the rebase worktree")
	}
	if cherryPickHeadPresent(t, target) {
		t.Errorf("CHERRY_PICK_HEAD present in the TARGET; the rebase must never park there")
	}
	assertPhases(t, sink.phases(), PhaseMerging, PhaseMergeConflict)
}

// --- empty picks are skipped, not parked as conflicts -------------------

// A pick whose change is ALREADY in the target's tree goes empty, and git parks
// CHERRY_PICK_HEAD over an empty pick exactly as it does over a conflict. The
// patch-id probes cannot see this one coming: the change reached the target
// inside a LARGER commit, whose patch-id differs from the commit being picked.
// Reporting it as a conflict parks the merge on work no resolver can do and
// holds the target worktree against every later merge in the repository.
func TestMergeSkipsAPickEmptiedByALargerCommitOnTheTarget(t *testing.T) {
	// Arrange: feature edits base.txt, and the target already carries that same
	// edit inside a commit that also adds an unrelated file.
	target := initTarget(t)
	featureDir := addFeatureWorktree(t, target)
	writeFile(t, featureDir, "base.txt", "feature\n")
	gitRun(t, featureDir, "add", ".")
	gitRun(t, featureDir, "commit", "-q", "-m", "feature edit")
	writeFile(t, target, "base.txt", "feature\n")
	writeFile(t, target, "unrelated.txt", "landed alongside the same edit\n")
	gitRun(t, target, "add", ".")
	gitRun(t, target, "commit", "-q", "-m", "the same edit inside a larger commit")

	sink := &recordingSink{}
	e := newTestDriver(t, sink)
	req := withRun(t, sink, Request{Workspace: "/ws/empty-ws", Name: "empty-ws", SourceBranch: "feature", SourceDir: featureDir, TargetDir: target})

	// Act.
	res, err := e.Merge(context.Background(), req)

	// Assert.
	if err != nil {
		t.Fatalf("Merge() err = %v", err)
	}
	if res.Outcome != OutcomeMerged {
		t.Fatalf("Merge() outcome = %s, want merged — an empty pick is the change already being on the target", res.Outcome)
	}
	if cherryPickHeadPresent(t, target) {
		t.Errorf("CHERRY_PICK_HEAD still parked — the empty pick was left wedging the target worktree")
	}
	for _, phase := range sink.phases() {
		if phase == PhaseMergeConflict {
			t.Errorf("transition phases = %v, want no conflict for an empty pick", sink.phases())
			break
		}
	}
}

// A resolution that discards the whole change empties the pick too, and
// `--continue` refuses an empty pick while leaving CHERRY_PICK_HEAD exactly
// where it was — the fixpoint that re-parks the merge on every resume.
func TestResumeSkipsAPickTheResolutionEmptied(t *testing.T) {
	// Arrange: a real conflict, resolved by keeping the target's own content.
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
	req := withRun(t, sink, Request{Workspace: "/ws/rs-empty-ws", Name: "rs-empty-ws", SourceBranch: "feature", SourceDir: featureDir, TargetDir: target})

	parked, err := e.Merge(context.Background(), req)
	if err != nil || parked.Outcome != OutcomeConflict {
		t.Fatalf("setup Merge() res=%+v err=%v, want conflict", parked, err)
	}
	// The conflict is parked in the REBASE WORKTREE, so that is where the
	// resolution happens and what the resume must be pointed at.
	req = parkedAt(t, e, req, parked)
	writeFile(t, req.WorkDir, "base.txt", "target\n")
	gitRun(t, req.WorkDir, "add", "base.txt")

	// Act.
	res, err := e.Resume(context.Background(), req)

	// Assert.
	if err != nil {
		t.Fatalf("Resume() err = %v", err)
	}
	if res.Outcome != OutcomeMerged {
		t.Fatalf("Resume() outcome = %s, want merged", res.Outcome)
	}
	if cherryPickHeadPresent(t, req.WorkDir) {
		t.Errorf("CHERRY_PICK_HEAD still parked after resuming an emptied replay")
	}
	if got := readFile(t, target, "base.txt"); got != "target\n" {
		t.Errorf("base.txt = %q, want the resolution's content", got)
	}
}

// A pick finished as empty is ACCOUNTED FOR, not silently dropped: it counts
// toward the run's landed total exactly as a pick that carried a change does.
// A run whose landed figure skipped it would count toward a total it could never
// reach, and a frontend would render a merge that finished as one stuck one
// commit short.
func TestAnEmptyPickCountsTowardTheRunsLandedTotal(t *testing.T) {
	// Arrange: two feature commits; the target already carries the FIRST one's
	// edit inside a larger commit, so that pick goes empty and the second lands.
	target := initTarget(t)
	featureDir := addFeatureWorktree(t, target)
	writeFile(t, featureDir, "base.txt", "feature\n")
	gitRun(t, featureDir, "add", ".")
	gitRun(t, featureDir, "commit", "-q", "-m", "feature edit")
	writeFile(t, featureDir, "second.txt", "second\n")
	gitRun(t, featureDir, "add", ".")
	gitRun(t, featureDir, "commit", "-q", "-m", "second commit")
	writeFile(t, target, "base.txt", "feature\n")
	writeFile(t, target, "unrelated.txt", "landed alongside the same edit\n")
	gitRun(t, target, "add", ".")
	gitRun(t, target, "commit", "-q", "-m", "the same edit inside a larger commit")

	sink := &recordingSink{}
	e := newTestDriver(t, sink)
	req := withRun(t, sink, Request{Workspace: "/ws/count-ws", Name: "count-ws", SourceBranch: "feature", SourceDir: featureDir, TargetDir: target})

	// Act.
	res, err := e.Merge(context.Background(), req)

	// Assert.
	if err != nil {
		t.Fatalf("Merge() err = %v", err)
	}
	if res.Outcome != OutcomeMerged {
		t.Fatalf("Merge() outcome = %s, want merged", res.Outcome)
	}
	picks := cherryPickingStatuses(sink.statuses)
	if len(picks) != 2 {
		t.Fatalf("cherry_picking statuses = %d, want one per planned commit", len(picks))
	}
	if got := picks[1].GetCommitsLanded(); got != 1 {
		t.Fatalf("the second pick reports commits_landed=%d of %d, want 1 — the empty pick was not counted",
			got, picks[1].GetCommitsTotal())
	}
}

// The empty pick's accounting is DURABLE, which is what `git cherry-pick --skip`
// could not give it: the replay derives its remaining work from the `-x`
// annotations on the target, so a pick that left no commit would be planned
// again by the very next re-entry — and one emptied by a resolution that dropped
// the change would then be re-picked, conflict for real, and loop forever.
func TestAnEmptyPickLeavesTheReplayNothingToPlanAgain(t *testing.T) {
	// Arrange: the same fixture, merged once.
	target := initTarget(t)
	featureDir := addFeatureWorktree(t, target)
	writeFile(t, featureDir, "base.txt", "feature\n")
	gitRun(t, featureDir, "add", ".")
	gitRun(t, featureDir, "commit", "-q", "-m", "feature edit")
	writeFile(t, target, "base.txt", "feature\n")
	writeFile(t, target, "unrelated.txt", "landed alongside the same edit\n")
	gitRun(t, target, "add", ".")
	gitRun(t, target, "commit", "-q", "-m", "the same edit inside a larger commit")

	sink := &recordingSink{}
	e := newTestDriver(t, sink)
	req := withRun(t, sink, Request{Workspace: "/ws/replay-ws", Name: "replay-ws", SourceBranch: "feature", SourceDir: featureDir, TargetDir: target})
	if res, err := e.Merge(context.Background(), req); err != nil || res.Outcome != OutcomeMerged {
		t.Fatalf("setup Merge() res=%+v err=%v, want merged", res, err)
	}

	// Act — a re-entry, exactly as a daemon bounce or a resume produces.
	replay := withRun(t, sink, req)
	res, err := e.Merge(context.Background(), replay)

	// Assert.
	if err != nil {
		t.Fatalf("replay Merge() err = %v", err)
	}
	if res.Outcome != OutcomeMerged {
		t.Fatalf("replay Merge() outcome = %s, want merged", res.Outcome)
	}
	if !res.AlreadyIncorporated {
		t.Fatal("the replay planned the empty pick again — its empty commit's -x annotation did not advance the cherry-pick base")
	}
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

	parked, err := e.Merge(context.Background(), req)
	if err != nil || parked.Outcome != OutcomeConflict {
		t.Fatalf("setup Merge() res=%+v err=%v, want conflict", parked, err)
	}
	// The conflict is parked in the REBASE WORKTREE, so that is where the
	// resolution happens and what the resume must be pointed at.
	req = parkedAt(t, e, req, parked)
	// Human resolves the conflict and stages the file, in the rebase worktree.
	writeFile(t, req.WorkDir, "base.txt", "resolved\n")
	gitRun(t, req.WorkDir, "add", "base.txt")

	// Act.
	res, err := e.Resume(context.Background(), req)

	// Assert.
	if err != nil {
		t.Fatalf("Resume() err = %v", err)
	}
	if res.Outcome != OutcomeMerged {
		t.Fatalf("Resume() outcome = %s, want merged", res.Outcome)
	}
	if cherryPickHeadPresent(t, req.WorkDir) {
		t.Errorf("CHERRY_PICK_HEAD still present after a completed Resume")
	}
	if got := readFile(t, target, "base.txt"); got != "resolved\n" {
		t.Errorf("base.txt = %q, want the resolved content", got)
	}
	if tags := gitRun(t, target, "tag", "-l", "merge/rz-ws"); !strings.Contains(tags, "merge/rz-ws") {
		t.Errorf("tag merge/rz-ws not created after Resume")
	}
	// The full ordered sequence across Merge + Resume: the opening pick, the
	// conflict, the resume, the test gate on the RESUMED commit. The terminal
	// `merged` is merge.Coordinator's, not the driver's.
	assertPhases(t, sink.phases(), PhaseMerging, PhaseMergeConflict, PhaseMerging, PhaseMerging)
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
	// Two picks and ONE gate: the suite runs once, at the head the replay
	// reached.
	assertPhases(t, sink.phases(), PhaseMerging, PhaseMerging, PhaseMerging)
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

	// Assert: a successful no-op. It publishes NO phase of its own -- there was
	// no pick to report -- and the terminal `merged` is merge.Coordinator's,
	// which publishes it with the after-action's outcome already on it.
	if err != nil {
		t.Fatalf("Merge() err = %v", err)
	}
	if res.Outcome != OutcomeMerged || !res.AlreadyIncorporated {
		t.Fatalf("Merge() res = %+v, want merged + AlreadyIncorporated", res)
	}
	assertPhases(t, sink.phases())
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
	assertPhases(t, sink.phases())
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
	assertPhases(t, sink.phases())
}

// --- a dirty or colliding TARGET tree -----------------------------------

func TestMergeTargetDirtyUnstagedFails(t *testing.T) {
	// Arrange: the target has an uncommitted edit to a file the merge rewrites.
	// AMENDED for the rebase pipeline: the rebase itself runs in a clean temp
	// worktree and succeeds, so the refusal now comes from the ONE merge commit
	// at the end. The guarantee under test is unchanged and asserted unchanged —
	// git refuses rather than clobbering the uncommitted work, and the merge is
	// failed rather than parked — only the phase count grew, because the commits
	// were genuinely replayed and gated before the target was asked to take
	// them.
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
	assertPhases(t, sink.phases(), PhaseMerging, PhaseMerging, PhaseMergeFailed)
}

func TestMergeTargetDirtyStagedFails(t *testing.T) {
	// Arrange: same collision, but the target's edit is STAGED. Git reports it
	// differently, and the classification must land the same way. AMENDED for
	// the rebase pipeline exactly as its unstaged sibling was.
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
		t.Errorf("CHERRY_PICK_HEAD present; a refused merge is not a resolvable conflict")
	}
	if got := readFile(t, target, "base.txt"); got != "staged local work\n" {
		t.Errorf("target base.txt = %q; the refused merge overwrote staged work", got)
	}
	assertPhases(t, sink.phases(), PhaseMerging, PhaseMerging, PhaseMergeFailed)
}

func TestMergeUntrackedCollisionFails(t *testing.T) {
	// Arrange: an UNTRACKED target file with the same name a commit in the range
	// adds. AMENDED for the rebase pipeline: the range now replays in full
	// against a clean temp worktree, and the refusal comes from the ONE merge
	// commit — so the merge is no longer PARTIALLY applied to the target, it is
	// not applied at all. The assertions are strictly stronger: the untracked
	// file survives, nothing is tagged, and the target keeps its own head.
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
	// Both commits replayed (one phase each) and the head was gated once; the
	// target then refused to take them and kept its own head.
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
	parked, err := e.Merge(context.Background(), req)
	if err != nil || parked.Outcome != OutcomeConflict {
		t.Fatalf("setup Merge() res=%+v err=%v, want conflict", parked, err)
	}
	// The conflict is parked in the REBASE WORKTREE, so that is where the
	// resolution happens and what the resume must be pointed at.
	req = parkedAt(t, e, req, parked)
	writeFile(t, req.WorkDir, "base.txt", "resolved\n")
	gitRun(t, req.WorkDir, "add", "base.txt")

	// Act.
	res, err := e.Resume(context.Background(), req)

	// Assert.
	if err != nil {
		t.Fatalf("Resume() err = %v", err)
	}
	if res.Outcome != OutcomeMerged {
		t.Fatalf("Resume() outcome = %s, want merged", res.Outcome)
	}
	// The driver reports OutcomeMerged without publishing the terminal arm —
	// that publish is the coordinator's, after the after-action — so this test
	// performs the coordinator's terminal publish to read the run's totals.
	if err := req.Run.Merged("", "the merge landed"); err != nil {
		t.Fatalf("Merged(): %v", err)
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
	parked, err := e.Merge(context.Background(), req)
	if err != nil || parked.Outcome != OutcomeConflict {
		t.Fatalf("setup Merge() res=%+v err=%v, want conflict", parked, err)
	}
	// The conflict is parked in the REBASE WORKTREE, so that is where the
	// resolution happens and what the resume must be pointed at.
	req = parkedAt(t, e, req, parked)
	writeFile(t, req.WorkDir, "base.txt", "resolved\n")
	gitRun(t, req.WorkDir, "add", "base.txt")
	// The daemon that was publishing this run is gone; its successor rebuilds
	// the publisher from the durable run id alone.
	rebuilt, err := ResumeRunStatus(sink, t.Logf, req.Workspace, testClock(), req.Run.RunID(), 0)
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
	// The terminal merged publish moved to the coordinator (after the
	// after-action), so the test performs it to read the rebuilt run's totals.
	if err := req.Run.Merged("", "the merge landed"); err != nil {
		t.Fatalf("Merged(): %v", err)
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
	req = parkedAt(t, e, req, first)
	// A human resolves the FIRST conflict only, in the rebase worktree.
	writeFile(t, req.WorkDir, "base.txt", "resolved one\n")
	gitRun(t, req.WorkDir, "add", "base.txt")

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
	if !cherryPickHeadPresent(t, req.WorkDir) {
		t.Errorf("CHERRY_PICK_HEAD absent — the second conflict was not left in the rebase worktree")
	}
	if tags := strings.TrimSpace(gitRun(t, target, "tag", "-l", "merge/rz2-ws")); tags != "" {
		t.Errorf("merge/rz2-ws tagged while still conflicted; git tag -l = %q", tags)
	}
	// pick 1 -> conflict -> resume -> pick 2 -> conflict. No `testing` phase
	// anywhere: the merge's one gate runs at the head, and this replay never
	// reached one.
	assertPhases(t, sink.phases(), PhaseMerging, PhaseMergeConflict, PhaseMerging, PhaseMerging, PhaseMergeConflict)
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
	parked, err := e.Merge(context.Background(), req)
	if err != nil || parked.Outcome != OutcomeConflict {
		t.Fatalf("setup Merge() res=%+v err=%v, want conflict", parked, err)
	}
	// The conflict is parked in the REBASE WORKTREE, so that is where the
	// resolution happens and what the resume must be pointed at.
	req = parkedAt(t, e, req, parked)
	writeFile(t, req.WorkDir, "base.txt", "resolved\n")
	gitRun(t, req.WorkDir, "add", "base.txt")
	sink.failOn = PhaseMerging

	// Act.
	_, err = e.Resume(context.Background(), req)

	// Assert.
	if err == nil {
		t.Fatalf("Resume() err = nil; want the sink error surfaced")
	}
	if !strings.Contains(err.Error(), string(errFakeSink)) {
		t.Errorf("Resume() err = %v; want it to wrap the sink error", err)
	}
	if !cherryPickHeadPresent(t, req.WorkDir) {
		t.Errorf("CHERRY_PICK_HEAD gone; Resume continued the replay despite the aborted transition")
	}
}

func TestMergeSurfacesSinkError(t *testing.T) {
	// Arrange: a clean cherry-pick, but the sink rejects the per-pick `merging`
	// transition. The driver must surface (not swallow) it.
	target := initTarget(t)
	featureDir := addFeatureWorktree(t, target)
	writeFile(t, featureDir, "feature.txt", "hello\n")
	gitRun(t, featureDir, "add", ".")
	gitRun(t, featureDir, "commit", "-q", "-m", "add feature.txt")

	sink := &recordingSink{failOn: PhaseMerging}
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

// cherryPickingStatuses is every cherry_picking status a run published, in
// order, which is what an accounting assertion reads: the run's landed figure is
// only ever visible on the NEXT commit's status.
func cherryPickingStatuses(statuses []*frontendv1.MergeStatus) []*frontendv1.MergeStatusCherryPicking {
	var out []*frontendv1.MergeStatusCherryPicking
	for _, s := range statuses {
		if pick := s.GetCherryPicking(); pick != nil {
			out = append(out, pick)
		}
	}
	return out
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

// --- the single head test gate ------------------------------------------

// AMENDED FROM TestMergeRunsTheSuiteOncePerLandedCommit. The gate moved out of
// the replay loop to the rebased head, so the guarantee inverted: a three-commit
// merge now costs ONE suite run, not three. The rest of the assertion (every run
// happens in the rebase worktree, never in the target) is unchanged, because
// that part of the design did not move.
func TestMergeRunsTheSuiteOnceForTheWholeMerge(t *testing.T) {
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

	// Assert — exactly one run, against the REBASE WORKTREE. That is the tree the
	// proposed merge exists in; running the gate in the target would mean the
	// target already carried the untested work.
	if err != nil {
		t.Fatalf("Merge() err = %v", err)
	}
	if res.Outcome != OutcomeMerged {
		t.Fatalf("Merge() outcome = %s, want merged", res.Outcome)
	}
	if suite.calls != 1 {
		t.Fatalf("suite runs = %d, want 1 (the whole merge is gated once, at the rebased head)", suite.calls)
	}
	if res.WorkDir == "" {
		t.Fatalf("merged Result names no rebase worktree")
	}
	for i, dir := range suite.targets {
		if dir != res.WorkDir {
			t.Errorf("suite run %d ran against %q, want the rebase worktree %q", i, dir, res.WorkDir)
		}
		if dir == target {
			t.Errorf("suite run %d ran in the TARGET checkout, which this merge must never write to", i)
		}
	}
}

// AMENDED FROM TestMergeOrdersEachSuiteRunAfterItsOwnCommit, which pinned the
// (pick, test, pick, test) interleaving. The interleaving is gone; what replaces
// it is the property that made it worth pinning — the suite judges the tree that
// is about to reach the target — expressed as "the one run sees the WHOLE range
// already replayed", not a prefix of it.
func TestTheGateRunsOnlyOnceTheWholeRangeHasReplayed(t *testing.T) {
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
	observing := suiteFunc(func(ctx context.Context, dir string, run SuiteRun) (SuiteResult, error) {
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
		return suite.RunSuite(ctx, dir, run)
	})
	e := newTestDriverWithSuite(t, sink, observing)
	req := withRun(t, sink, Request{Workspace: "/ws/order-ws", Name: "order-ws", SourceBranch: "feature", SourceDir: featureDir, TargetDir: target})

	// Act.
	if _, err := e.Merge(context.Background(), req); err != nil {
		t.Fatalf("Merge() err = %v", err)
	}

	// Assert — one run, and it saw both commits' files.
	if len(seen) != 1 || seen[0] != "2" {
		t.Fatalf("files present at each suite run = %v, want [2] (one run, on the fully replayed head)", seen)
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
	// needs to prompt a fix and, failing that, to discard the rebase worktree.
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
	if res.WorkDir == "" {
		t.Errorf("res.WorkDir empty; the coordinator has no tree to point a fix turn at")
	}
	if res.BaseHead != head {
		t.Errorf("res.BaseHead = %q, want the target head %q the rebase based itself on", res.BaseHead, head)
	}
	// No terminal transition: the coordinator classifies this one. The third
	// `merging` is the gate's single re-run, which a scripted failure repeats.
	assertPhases(t, sink.phases(), PhaseMerging, PhaseMerging, PhaseMerging)
}

// AMENDED FROM TestMergeStopsPickingAfterATestFailure, whose premise (a failing
// gate stops the replay part-way) cannot occur now that the gate is the replay's
// tail. What it asserted about the TARGET is what survives, and it is asserted
// here about the whole range rather than about one commit.
func TestAFailedHeadGateLandsNoneOfTheRangeOnTheTarget(t *testing.T) {
	// Arrange — two commits, and a suite that fails at the head.
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

	// Assert — neither commit reached the target.
	for _, name := range []string{"one.txt", "two.txt"} {
		if _, statErr := os.Stat(filepath.Join(target, name)); statErr == nil {
			t.Errorf("%s reached the target though the head gate failed", name)
		}
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
	// The failure is scripted TWICE because the gate re-runs a failing suite
	// once before believing it; the third verdict is the fixed tree's.
	suite := &fakeSuite{verdicts: []SuiteResult{{Passed: false, Tail: "boom"}, {Passed: false, Tail: "boom"}, {Passed: true}}}
	e := newTestDriverWithSuite(t, sink, suite)
	req := withRun(t, sink, Request{Workspace: "/ws/fix-ws", Name: "fix-ws", SourceBranch: "feature", SourceDir: featureDir, TargetDir: target})
	first, err := e.Merge(context.Background(), req)
	if err != nil || first.Outcome != OutcomeTestFailed {
		t.Fatalf("setup Merge() res=%+v err=%v, want test_failed", first, err)
	}
	req = parkedAt(t, e, req, first)
	headAfterPick := strings.TrimSpace(gitRun(t, req.WorkDir, "rev-parse", "HEAD"))
	writeFile(t, req.WorkDir, "feature.txt", "fixed\n")
	gitRun(t, req.WorkDir, "add", "feature.txt")

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
	// The target's HEAD is the merge commit; the rebased line hangs off its
	// second parent, and that is where the fix's ancestry is read.
	if parent := strings.TrimSpace(gitRun(t, target, "rev-parse", "HEAD^2^")); parent != headAfterPick {
		t.Errorf("HEAD^2^ = %s, want the untouched replayed commit %s (the fix must not amend it)", parent, headAfterPick)
	}
	// AMENDED: the message used to name the failing SHA. It names the workspace
	// now, by the ruling that no copy a human reads identifies a commit by sha —
	// and a commit message is read in `git log` right beside the commit it fixes,
	// which is where the sha it carried was least necessary of all.
	if body := gitRun(t, target, "log", "-1", "--pretty=%s", "HEAD^2"); !strings.Contains(body, "fix tests after rebasing fix-ws") {
		t.Errorf("follow-up commit subject = %q, want it to name the workspace being rebased", body)
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

	req = parkedAt(t, e, req, first)

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

// AMENDED FROM TestResumeGatesTheResolvedCommitOnTheSuite. The resume no longer
// gates the commit it just continued; it re-enters the replay, and the merge's
// one gate runs at the head that replay reaches. Here the resolved commit IS the
// whole range, so the head gate judges it — which is the guarantee that mattered:
// a hand-resolved conflict never reaches the target ungated.
func TestResumeGatesTheHeadItsReplayReaches(t *testing.T) {
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
	parked, err := e.Merge(context.Background(), req)
	if err != nil || parked.Outcome != OutcomeConflict {
		t.Fatalf("setup Merge() res=%+v err=%v, want conflict", parked, err)
	}
	// The conflict is parked in the REBASE WORKTREE, so that is where the
	// resolution happens and what the resume must be pointed at.
	req = parkedAt(t, e, req, parked)
	writeFile(t, req.WorkDir, "base.txt", "resolved\n")
	gitRun(t, req.WorkDir, "add", "base.txt")

	// Act.
	res, err := e.Resume(context.Background(), req)

	// Assert.
	if err != nil {
		t.Fatalf("Resume() err = %v", err)
	}
	if res.Outcome != OutcomeTestFailed {
		t.Fatalf("Resume() outcome = %s, want test_failed on the resumed commit", res.Outcome)
	}
	// Two runs, one gate: the gate re-runs a failing suite once on the same tree
	// before it believes the failure.
	if suite.calls != 2 {
		t.Errorf("suite runs = %d, want 2 (the resumed head, gated and re-run)", suite.calls)
	}
}

// --- the ONE target move, and the absence of a rollback -----------------

// Rollback is RETIRED. Nothing of a merge reaches the target until the whole
// rebase has landed and passed, so there is nothing a failed merge could undo —
// and the reset that used to do the undoing is the hazard the rebase design
// removed. The tests that pinned its behavior are replaced by the ones below,
// which pin the guarantee that made it unnecessary (the target is untouched) and
// the migrated refusal guard (the target must not have moved).

func TestAFailedGateLeavesTheTargetExactlyAsItWasFound(t *testing.T) {
	// AMENDED FROM TestRollbackReturnsTheTargetToItsPreMergeHead. That test
	// asserted the target came BACK to its pre-merge head after a reset; this
	// asserts it never left, which is the same guarantee without the window.
	// Arrange — a merge whose first landed commit breaks the suite twice.
	target := initTarget(t)
	featureDir := addFeatureWorktree(t, target)
	writeFile(t, featureDir, "feature.txt", "hello\n")
	gitRun(t, featureDir, "add", ".")
	gitRun(t, featureDir, "commit", "-q", "-m", "add feature.txt")
	before := strings.TrimSpace(gitRun(t, target, "rev-parse", "HEAD"))

	sink := &recordingSink{}
	suite := &fakeSuite{verdicts: []SuiteResult{{Passed: false, Tail: "boom"}}}
	e := newTestDriverWithSuite(t, sink, suite)
	req := withRun(t, sink, Request{Workspace: "/ws/rb-ws", Name: "rb-ws", SourceBranch: "feature", SourceDir: featureDir, TargetDir: target})

	// Act.
	res, err := e.Merge(context.Background(), req)

	// Assert — the target never moved and never saw the work.
	if err != nil || res.Outcome != OutcomeTestFailed {
		t.Fatalf("Merge() res=%+v err=%v, want test_failed", res, err)
	}
	if head := strings.TrimSpace(gitRun(t, target, "rev-parse", "HEAD")); head != before {
		t.Errorf("target HEAD = %s, want it untouched at %s", head, before)
	}
	if _, statErr := os.Stat(filepath.Join(target, "feature.txt")); statErr == nil {
		t.Errorf("feature.txt reached the target, which a failing gate must never let happen")
	}
}

func TestATestFailureParksInTheRebaseWorktreeNotTheTarget(t *testing.T) {
	// Arrange — a merge whose landed commit breaks the suite.
	target := initTarget(t)
	featureDir := addFeatureWorktree(t, target)
	writeFile(t, featureDir, "feature.txt", "hello\n")
	gitRun(t, featureDir, "add", ".")
	gitRun(t, featureDir, "commit", "-q", "-m", "add feature.txt")
	base := strings.TrimSpace(gitRun(t, target, "rev-parse", "HEAD"))

	sink := &recordingSink{}
	suite := &fakeSuite{verdicts: []SuiteResult{{Passed: false, Tail: "boom"}}}
	e := newTestDriverWithSuite(t, sink, suite)
	req := withRun(t, sink, Request{Workspace: "/ws/rb3-ws", Name: "rb3-ws", SourceBranch: "feature", SourceDir: featureDir, TargetDir: target})

	// Act.
	res, err := e.Merge(context.Background(), req)
	if err != nil {
		t.Fatalf("Merge() err = %v", err)
	}
	t.Cleanup(func() {
		_ = e.Cleanup(context.Background(), Request{Name: req.Name, TargetDir: target, WorkDir: res.WorkDir})
	})

	// Assert — the Result names the tree the failure lives in and the target
	// head the whole rebase was based on.
	if res.WorkDir == "" {
		t.Fatalf("res.WorkDir empty; the coordinator has no tree to point the fix turn at")
	}
	if res.BaseHead != base {
		t.Errorf("res.BaseHead = %q, want the target head %q the rebase based itself on", res.BaseHead, base)
	}
	if _, statErr := os.Stat(filepath.Join(res.WorkDir, "feature.txt")); statErr != nil {
		t.Errorf("the failing commit is not in the rebase worktree: %v", statErr)
	}
}

func TestARebaseLandsARealMergeCommitWhoseSecondParentIsTheBranch(t *testing.T) {
	// Arrange.
	target := initTarget(t)
	featureDir := addFeatureWorktree(t, target)
	writeFile(t, featureDir, "feature.txt", "hello\n")
	gitRun(t, featureDir, "add", ".")
	gitRun(t, featureDir, "commit", "-q", "-m", "add feature.txt")
	base := strings.TrimSpace(gitRun(t, target, "rev-parse", "HEAD"))

	sink := &recordingSink{}
	e := newTestDriver(t, sink)
	req := withRun(t, sink, Request{Workspace: "/ws/topo-ws", Name: "topo-ws", SourceBranch: "feature", SourceDir: featureDir, TargetDir: target})

	// Act.
	res, err := e.Merge(context.Background(), req)

	// Assert — a merge commit with the target's old head first and the BRANCH
	// second, which is what makes the workspace visible in git porcelain.
	if err != nil || res.Outcome != OutcomeMerged {
		t.Fatalf("Merge() res=%+v err=%v, want merged", res, err)
	}
	parents := strings.Fields(gitRun(t, target, "log", "-1", "--pretty=%P"))
	if len(parents) != 2 {
		t.Fatalf("target HEAD has %d parents (%v), want a 2-parent merge commit", len(parents), parents)
	}
	if parents[0] != base {
		t.Errorf("first parent = %s, want the target's own head %s", parents[0], base)
	}
	branch := strings.TrimSpace(gitRun(t, target, "rev-parse", "refs/heads/feature"))
	if parents[1] != branch {
		t.Errorf("second parent = %s, want the branch tip %s", parents[1], branch)
	}
}

func TestTheBranchRefIsForceMovedToTheRebasedLine(t *testing.T) {
	// Arrange — the target advances after the branch was cut, so the rebase
	// genuinely rewrites the branch's commits onto a new base.
	target := initTarget(t)
	featureDir := addFeatureWorktree(t, target)
	writeFile(t, featureDir, "feature.txt", "hello\n")
	gitRun(t, featureDir, "add", ".")
	gitRun(t, featureDir, "commit", "-q", "-m", "add feature.txt")
	writeFile(t, target, "target-moved.txt", "moved\n")
	gitRun(t, target, "add", ".")
	gitRun(t, target, "commit", "-q", "-m", "B")
	oldBranch := strings.TrimSpace(gitRun(t, target, "rev-parse", "refs/heads/feature"))
	newBase := strings.TrimSpace(gitRun(t, target, "rev-parse", "HEAD"))

	sink := &recordingSink{}
	e := newTestDriver(t, sink)
	req := withRun(t, sink, Request{Workspace: "/ws/mv-ws", Name: "mv-ws", SourceBranch: "feature", SourceDir: featureDir, TargetDir: target})

	// Act.
	if _, err := e.Merge(context.Background(), req); err != nil {
		t.Fatalf("Merge() err = %v", err)
	}

	// Assert — the branch moved off its old tip onto a line that descends from
	// the target's new head, and the source worktree came with it.
	moved := strings.TrimSpace(gitRun(t, target, "rev-parse", "refs/heads/feature"))
	if moved == oldBranch {
		t.Fatalf("branch ref = %s, want it force-moved off the pre-rebase tip", moved)
	}
	if got := strings.TrimSpace(gitRun(t, target, "merge-base", moved, newBase)); got != newBase {
		t.Errorf("the moved branch does not descend from the target head %s", newBase)
	}
	if head := strings.TrimSpace(gitRun(t, featureDir, "rev-parse", "HEAD")); head != moved {
		t.Errorf("source worktree HEAD = %s, want the moved branch %s", head, moved)
	}
	if _, statErr := os.Stat(filepath.Join(featureDir, "target-moved.txt")); statErr != nil {
		t.Errorf("the source worktree was not re-synced onto the rebased line: %v", statErr)
	}
}

func TestAnEmptyBranchNoOpsWithoutAnEmptyMergeCommit(t *testing.T) {
	// Arrange — a branch that contributes nothing over the target.
	target := initTarget(t)
	featureDir := addFeatureWorktree(t, target)
	before := strings.TrimSpace(gitRun(t, target, "rev-parse", "HEAD"))

	sink := &recordingSink{}
	e := newTestDriver(t, sink)
	req := withRun(t, sink, Request{Workspace: "/ws/empty-ws", Name: "empty-ws", SourceBranch: "feature", SourceDir: featureDir, TargetDir: target})

	// Act.
	res, err := e.Merge(context.Background(), req)

	// Assert — merged as a no-op, and NO merge commit was fabricated for it.
	if err != nil || res.Outcome != OutcomeMerged || !res.AlreadyIncorporated {
		t.Fatalf("Merge() res=%+v err=%v, want a merged no-op", res, err)
	}
	if head := strings.TrimSpace(gitRun(t, target, "rev-parse", "HEAD")); head != before {
		t.Errorf("target HEAD = %s, want it unmoved at %s (an empty merge commit records work that never arrived)", head, before)
	}
}

func TestAMovedTargetIsAnsweredByOneAutomaticReRebase(t *testing.T) {
	// A target that moves while the gate runs invalidates everything the gate
	// certified, so the merge re-bases onto the new head rather than merging a
	// line nothing tested against it.
	// Arrange — the suite advances the target the FIRST time it runs, and never
	// again, so exactly one re-rebase is needed.
	target := initTarget(t)
	featureDir := addFeatureWorktree(t, target)
	writeFile(t, featureDir, "feature.txt", "hello\n")
	gitRun(t, featureDir, "add", ".")
	gitRun(t, featureDir, "commit", "-q", "-m", "add feature.txt")

	sink := &recordingSink{}
	var runs int
	suite := suiteFunc(func(_ context.Context, _ string, _ SuiteRun) (SuiteResult, error) {
		runs++
		if runs == 1 {
			writeFile(t, target, "external.txt", "somebody else\n")
			gitRun(t, target, "add", ".")
			gitRun(t, target, "commit", "-q", "-m", "an external commit")
		}
		return SuiteResult{Passed: true}, nil
	})
	e := newTestDriverWithSuite(t, sink, suite)
	req := withRun(t, sink, Request{Workspace: "/ws/moved-ws", Name: "moved-ws", SourceBranch: "feature", SourceDir: featureDir, TargetDir: target})

	// Act.
	res, err := e.Merge(context.Background(), req)

	// Assert — the merge landed on the SECOND attempt, over the external commit,
	// which is still reachable.
	if err != nil || res.Outcome != OutcomeMerged {
		t.Fatalf("Merge() res=%+v err=%v, want merged after one re-rebase", res, err)
	}
	if runs != 2 {
		t.Errorf("suite runs = %d, want 2 (the first attempt, then the re-rebase)", runs)
	}
	if _, statErr := os.Stat(filepath.Join(target, "external.txt")); statErr != nil {
		t.Errorf("the external commit was lost by the re-rebase: %v", statErr)
	}
	if _, statErr := os.Stat(filepath.Join(target, "feature.txt")); statErr != nil {
		t.Errorf("the merged work did not reach the target: %v", statErr)
	}
}

func TestATargetThatKeepsMovingFailsLoudlyAtTheBound(t *testing.T) {
	// Arrange — the suite moves the target on EVERY run, so no attempt can ever
	// land. The loop is bounded at rebaseAttempts and must fail rather than spin.
	target := initTarget(t)
	featureDir := addFeatureWorktree(t, target)
	writeFile(t, featureDir, "feature.txt", "hello\n")
	gitRun(t, featureDir, "add", ".")
	gitRun(t, featureDir, "commit", "-q", "-m", "add feature.txt")

	sink := &recordingSink{}
	var runs int
	suite := suiteFunc(func(_ context.Context, _ string, _ SuiteRun) (SuiteResult, error) {
		runs++
		writeFile(t, target, fmt.Sprintf("external-%d.txt", runs), "somebody else\n")
		gitRun(t, target, "add", ".")
		gitRun(t, target, "commit", "-q", "-m", "an external commit")
		return SuiteResult{Passed: true}, nil
	})
	e := newTestDriverWithSuite(t, sink, suite)
	req := withRun(t, sink, Request{Workspace: "/ws/spin-ws", Name: "spin-ws", SourceBranch: "feature", SourceDir: featureDir, TargetDir: target})

	// Act.
	_, err := e.Merge(context.Background(), req)

	// Assert — a loud failure naming the bound, and nothing of the merge on the
	// target.
	if err == nil {
		t.Fatalf("Merge() err = nil; want a loud failure after %d attempts", rebaseAttempts)
	}
	if runs != rebaseAttempts {
		t.Errorf("suite runs = %d, want exactly %d — the loop must be bounded", runs, rebaseAttempts)
	}
	if _, statErr := os.Stat(filepath.Join(target, "feature.txt")); statErr == nil {
		t.Errorf("the merge landed on a target it never tested against")
	}
}

func TestTheRebaseWorktreeIsRemovedAfterASuccessfulMerge(t *testing.T) {
	// Arrange.
	target := initTarget(t)
	featureDir := addFeatureWorktree(t, target)
	writeFile(t, featureDir, "feature.txt", "hello\n")
	gitRun(t, featureDir, "add", ".")
	gitRun(t, featureDir, "commit", "-q", "-m", "add feature.txt")

	sink := &recordingSink{}
	e := newTestDriver(t, sink)
	req := withRun(t, sink, Request{Workspace: "/ws/cl-ws", Name: "cl-ws", SourceBranch: "feature", SourceDir: featureDir, TargetDir: target})

	// Act.
	res, err := e.Merge(context.Background(), req)
	if err != nil {
		t.Fatalf("Merge() err = %v", err)
	}

	// Assert — the driver owns the tree it made on any path that does not park.
	if res.WorkDir == "" {
		t.Fatalf("res.WorkDir empty; a merged Result still names the tree it worked in")
	}
	if _, statErr := os.Stat(res.WorkDir); !os.IsNotExist(statErr) {
		t.Errorf("rebase worktree %s survived a successful merge (stat err = %v)", res.WorkDir, statErr)
	}
	if list := gitRun(t, target, "worktree", "list"); strings.Contains(list, res.WorkDir) {
		t.Errorf("the repository still lists the rebase worktree:\n%s", list)
	}
}

func TestTheRebaseWorktreeIsRemovedWhenTheMergeFailsOutright(t *testing.T) {
	// Arrange — a gate that cannot run at all, which is the driver's own error
	// path rather than a parked outcome.
	target := initTarget(t)
	featureDir := addFeatureWorktree(t, target)
	writeFile(t, featureDir, "feature.txt", "hello\n")
	gitRun(t, featureDir, "add", ".")
	gitRun(t, featureDir, "commit", "-q", "-m", "add feature.txt")

	sink := &recordingSink{}
	e := newTestDriverWithSuite(t, sink, &fakeSuite{err: sentinelError("the entrypoint blew up")})
	req := withRun(t, sink, Request{Workspace: "/ws/cl2-ws", Name: "cl2-ws", SourceBranch: "feature", SourceDir: featureDir, TargetDir: target})

	// Act.
	if _, err := e.Merge(context.Background(), req); err == nil {
		t.Fatalf("Merge() err = nil; want the unrunnable gate surfaced")
	}

	// Assert — no worktree is left behind for the repository to trip over.
	if list := gitRun(t, target, "worktree", "list"); strings.Contains(list, "agent-repl-merge-rebase-") {
		t.Errorf("a rebase worktree survived a failed merge:\n%s", list)
	}
}

func TestCleanupRemovesTheParkedRebaseWorktree(t *testing.T) {
	// A parked outcome hands the tree to merge.Coordinator, which discards it
	// when the run reaches its terminal — including a run nobody ever resolved.
	// Arrange — a conflict, parked.
	target := initTarget(t)
	featureDir := addFeatureWorktree(t, target)
	writeFile(t, target, "shared.txt", "target\n")
	gitRun(t, target, "add", ".")
	gitRun(t, target, "commit", "-q", "-m", "target writes shared.txt")
	writeFile(t, featureDir, "shared.txt", "feature\n")
	gitRun(t, featureDir, "add", ".")
	gitRun(t, featureDir, "commit", "-q", "-m", "feature writes shared.txt")

	sink := &recordingSink{}
	e := newTestDriver(t, sink)
	req := withRun(t, sink, Request{Workspace: "/ws/cl3-ws", Name: "cl3-ws", SourceBranch: "feature", SourceDir: featureDir, TargetDir: target})
	res, err := e.Merge(context.Background(), req)
	if err != nil || res.Outcome != OutcomeConflict {
		t.Fatalf("setup Merge() res=%+v err=%v, want a parked conflict", res, err)
	}
	req.WorkDir, req.BaseHead = res.WorkDir, res.BaseHead

	// Act.
	if err := e.Cleanup(context.Background(), req); err != nil {
		t.Fatalf("Cleanup() err = %v", err)
	}

	// Assert — gone from disk AND from the repository's worktree list, even
	// though it was parked mid-replay.
	if _, statErr := os.Stat(res.WorkDir); !os.IsNotExist(statErr) {
		t.Errorf("parked rebase worktree %s survived Cleanup (stat err = %v)", res.WorkDir, statErr)
	}
	if list := gitRun(t, target, "worktree", "list"); strings.Contains(list, res.WorkDir) {
		t.Errorf("the repository still lists the parked rebase worktree:\n%s", list)
	}
}

func TestAnAbandonedRebaseWorktreeIsPrunedByTheNextMerge(t *testing.T) {
	// A daemon killed mid-merge leaves its rebase worktree REGISTERED in the
	// repository. Merges are serialized per repository, so the next one to start
	// knows every such tree is abandoned — and left alone they accumulate one
	// per interrupted merge.
	// Arrange — a leftover rebase worktree, shaped exactly like a dead daemon's.
	target := initTarget(t)
	featureDir := addFeatureWorktree(t, target)
	writeFile(t, featureDir, "feature.txt", "hello\n")
	gitRun(t, featureDir, "add", ".")
	gitRun(t, featureDir, "commit", "-q", "-m", "add feature.txt")
	orphanParent, err := os.MkdirTemp("", rebaseWorktreeMarker)
	if err != nil {
		t.Fatalf("MkdirTemp: %v", err)
	}
	orphan := filepath.Join(orphanParent, "rebase")
	gitRun(t, target, "worktree", "add", "--detach", orphan, "HEAD")

	sink := &recordingSink{}
	e := newTestDriver(t, sink)
	req := withRun(t, sink, Request{Workspace: "/ws/prune-ws", Name: "prune-ws", SourceBranch: "feature", SourceDir: featureDir, TargetDir: target})

	// Act.
	if _, err := e.Merge(context.Background(), req); err != nil {
		t.Fatalf("Merge() err = %v", err)
	}

	// Assert — the leftover is gone from disk and from the repository's list.
	if _, statErr := os.Stat(orphan); !os.IsNotExist(statErr) {
		t.Errorf("the abandoned rebase worktree %s survived (stat err = %v)", orphan, statErr)
	}
	if list := gitRun(t, target, "worktree", "list"); strings.Contains(list, orphan) {
		t.Errorf("the repository still lists the abandoned rebase worktree:\n%s", list)
	}
}

func TestCleanupOfARequestWithNoWorktreeIsANoOp(t *testing.T) {
	// Arrange — the shape a run that never reached a rebase carries.
	target := initTarget(t)
	featureDir := addFeatureWorktree(t, target)
	e := newTestDriver(t, &recordingSink{})
	req := Request{Workspace: "/ws/cl4-ws", Name: "cl4-ws", SourceBranch: "feature", SourceDir: featureDir, TargetDir: target}

	// Act.
	err := e.Cleanup(context.Background(), req)

	// Assert.
	if err != nil {
		t.Fatalf("Cleanup() err = %v, want a silent no-op", err)
	}
}

// --- boot replay --------------------------------------------------------

func TestMergeReplayedAfterABounceRebasesAfreshOntoAnUntouchedTarget(t *testing.T) {
	// AMENDED FROM TestMergeReplayedAfterABounceSkipsTheCommitsThatAlreadyLanded.
	// That test's premise — a dead daemon leaves commits half-landed ON THE
	// TARGET, and the replay must skip them — cannot occur any more: a merge that
	// did not finish never wrote to the target at all, and its temp worktree died
	// with the process. So the replay's obligation changed from "skip what
	// landed" to "start clean and land the whole thing", and the assertion is
	// equal in strength: the same fixture, the same two commits, and an exact
	// count of what the replay replays.
	// Arrange — a first attempt that dies on a failing gate.
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
	failed, err := e.Merge(context.Background(), req)
	if err != nil || failed.Outcome != OutcomeTestFailed {
		t.Fatalf("setup Merge() res=%+v err=%v, want test_failed", failed, err)
	}
	req = parkedAt(t, e, req, failed)
	beforeReplay := strings.TrimSpace(gitRun(t, target, "rev-parse", "HEAD"))

	// Act — the next boot replays the same request against a green suite. It
	// carries no rebase worktree, because a temp tree does not survive a bounce.
	replaySink := &recordingSink{}
	replaySuite := &fakeSuite{}
	replay := newTestDriverWithSuite(t, replaySink, replaySuite)
	fresh := withRun(t, replaySink, Request{Workspace: req.Workspace, Name: req.Name, SourceBranch: "feature", SourceDir: featureDir, TargetDir: target})
	res, err := replay.Merge(context.Background(), fresh)

	// Assert — the whole range was replayed and gated afresh, and it landed on
	// the head the dead attempt left untouched.
	if err != nil {
		t.Fatalf("replay Merge() err = %v", err)
	}
	if res.Outcome != OutcomeMerged {
		t.Fatalf("replay Merge() outcome = %s, want merged", res.Outcome)
	}
	if replaySuite.calls != 1 {
		t.Fatalf("replay suite runs = %d, want 1 (both commits replayed onto a target that kept nothing, then one gate at the head)", replaySuite.calls)
	}
	if parent := strings.TrimSpace(gitRun(t, target, "rev-parse", "HEAD^")); parent != beforeReplay {
		t.Errorf("HEAD^ = %s, want the untouched pre-replay head %s", parent, beforeReplay)
	}
}

// suiteFunc adapts a plain function to SuiteRunner.
type suiteFunc func(ctx context.Context, targetDir string, run SuiteRun) (SuiteResult, error)

func (f suiteFunc) RunSuite(ctx context.Context, targetDir string, run SuiteRun) (SuiteResult, error) {
	return f(ctx, targetDir, run)
}

// --- the gate's suite selection -----------------------------------------

// writeNested writes a file at a repository-relative path, creating its parents.
func writeNested(t *testing.T, dir, rel, content string) {
	t.Helper()
	full := filepath.Join(dir, rel)
	if err := os.MkdirAll(filepath.Dir(full), 0o755); err != nil {
		t.Fatalf("mkdir for %s: %v", rel, err)
	}
	if err := os.WriteFile(full, []byte(content), 0o644); err != nil {
		t.Fatalf("write %s: %v", rel, err)
	}
}

func TestGateNarrowsTheSuitesToWhatTheRangeTouches(t *testing.T) {
	// Arrange — a webapp-only branch, which is exactly the merge the gate used
	// to run all eighteen suites for.
	target := initTarget(t)
	featureDir := addFeatureWorktree(t, target)
	writeNested(t, featureDir, "modules/app/agent-repl/webapp/src/App.tsx", "export const x = 1\n")
	gitRun(t, featureDir, "add", ".")
	gitRun(t, featureDir, "commit", "-q", "-m", "webapp only")

	sink := &recordingSink{}
	suite := &fakeSuite{}
	e := newTestDriverWithSuite(t, sink, suite)
	req := withRun(t, sink, Request{Workspace: "/ws/sel-ws", Name: "sel-ws", SourceBranch: "feature", SourceDir: featureDir, TargetDir: target})

	// Act.
	if _, err := e.Merge(context.Background(), req); err != nil {
		t.Fatalf("Merge() err = %v", err)
	}

	// Assert.
	if len(suite.runs) != 1 {
		t.Fatalf("suite runs = %d, want 1", len(suite.runs))
	}
	want := []string{"build-frontend-harness", "webapp"}
	if !reflect.DeepEqual(suite.runs[0].Suites, want) {
		t.Fatalf("gate ran suites %v, want %v", suite.runs[0].Suites, want)
	}
}

// UNKNOWN BEATS WRONG: a path the mapping does not recognize runs everything.
func TestGateRunsEverySuiteForAnUnmappedPath(t *testing.T) {
	// Arrange.
	target := initTarget(t)
	featureDir := addFeatureWorktree(t, target)
	writeFile(t, featureDir, "somewhere-else.txt", "hello\n")
	gitRun(t, featureDir, "add", ".")
	gitRun(t, featureDir, "commit", "-q", "-m", "unmapped")

	sink := &recordingSink{}
	suite := &fakeSuite{}
	e := newTestDriverWithSuite(t, sink, suite)
	req := withRun(t, sink, Request{Workspace: "/ws/unmapped-ws", Name: "unmapped-ws", SourceBranch: "feature", SourceDir: featureDir, TargetDir: target})

	// Act.
	if _, err := e.Merge(context.Background(), req); err != nil {
		t.Fatalf("Merge() err = %v", err)
	}

	// Assert — an empty selection is what the runner passes through as
	// "everything".
	if len(suite.runs) != 1 || len(suite.runs[0].Suites) != 0 {
		t.Fatalf("gate ran suites %v, want the empty (full) selection", suite.runs)
	}
}

// The selection is the merge's own record of which suites were asked to testify
// about it, so it lands on the workspace's status cause.
func TestGateRecordsTheSelectionOnTheWorkspaceStatus(t *testing.T) {
	// Arrange.
	target := initTarget(t)
	featureDir := addFeatureWorktree(t, target)
	writeNested(t, featureDir, "modules/app/agent-repl/webapp/src/App.tsx", "export const x = 1\n")
	gitRun(t, featureDir, "add", ".")
	gitRun(t, featureDir, "commit", "-q", "-m", "webapp only")

	sink := &recordingSink{}
	e := newTestDriverWithSuite(t, sink, &fakeSuite{})
	req := withRun(t, sink, Request{Workspace: "/ws/selrec-ws", Name: "selrec-ws", SourceBranch: "feature", SourceDir: featureDir, TargetDir: target})

	// Act.
	if _, err := e.Merge(context.Background(), req); err != nil {
		t.Fatalf("Merge() err = %v", err)
	}

	// Assert.
	var found bool
	for _, c := range sink.causes() {
		if strings.Contains(c, "testing ") && strings.Contains(c, "[build-frontend-harness,webapp]") {
			found = true
		}
	}
	if !found {
		t.Fatalf("status causes = %v, want a testing cause naming the selection", sink.causes())
	}
}

// The gate runs once, at the head, but it does NOT select from the head commit:
// the suites are the ones the WHOLE merge can affect — otherwise a branch whose
// last commit touches the webapp would be gated on the webapp suite alone while
// the daemon change it also carries goes untested.
func TestGateSelectsFromTheWholeRangeNotTheHeadCommit(t *testing.T) {
	// Arrange — commit one touches the webapp, commit two the daemon.
	target := initTarget(t)
	featureDir := addFeatureWorktree(t, target)
	writeNested(t, featureDir, "modules/app/agent-repl/webapp/src/App.tsx", "export const x = 1\n")
	gitRun(t, featureDir, "add", ".")
	gitRun(t, featureDir, "commit", "-q", "-m", "webapp")
	writeNested(t, featureDir, "modules/app/agent-repl/daemon/main.go", "package main\n")
	gitRun(t, featureDir, "add", ".")
	gitRun(t, featureDir, "commit", "-q", "-m", "daemon")

	sink := &recordingSink{}
	suite := &fakeSuite{}
	e := newTestDriverWithSuite(t, sink, suite)
	req := withRun(t, sink, Request{Workspace: "/ws/range-ws", Name: "range-ws", SourceBranch: "feature", SourceDir: featureDir, TargetDir: target})

	// Act.
	if _, err := e.Merge(context.Background(), req); err != nil {
		t.Fatalf("Merge() err = %v", err)
	}

	// Assert — the one gate carries the webapp suite as well as the daemon one,
	// though only the daemon commit is at the head.
	want := []string{"build-frontend-harness", "daemon", "webapp"}
	if len(suite.runs) != 1 {
		t.Fatalf("suite runs = %d, want 1", len(suite.runs))
	}
	if !reflect.DeepEqual(suite.runs[0].Suites, want) {
		t.Fatalf("the head gate ran suites %v, want the whole range's %v", suite.runs[0].Suites, want)
	}
}

// --- the flake re-run ---------------------------------------------------

func TestGateRerunsAFailingSuiteOnceAndProceedsOnAPass(t *testing.T) {
	// Arrange — the suite fails once and passes on the identical tree.
	target := initTarget(t)
	featureDir := addFeatureWorktree(t, target)
	writeFile(t, featureDir, "feature.txt", "hello\n")
	gitRun(t, featureDir, "add", ".")
	gitRun(t, featureDir, "commit", "-q", "-m", "add feature.txt")

	sink := &recordingSink{}
	suite := &fakeSuite{verdicts: []SuiteResult{
		{Passed: false, Tail: "flaked under load", OutputPath: "/tmp/first.log", Duration: 4 * time.Second},
		{Passed: true, OutputPath: "/tmp/second.log", Duration: 90 * time.Second},
	}}
	e := newTestDriverWithSuite(t, sink, suite)
	req := withRun(t, sink, Request{Workspace: "/ws/flake-ws", Name: "flake-ws", SourceBranch: "feature", SourceDir: featureDir, TargetDir: target})

	// Act.
	res, err := e.Merge(context.Background(), req)

	// Assert — the merge proceeds, and it took exactly two runs to get there.
	if err != nil {
		t.Fatalf("Merge() err = %v", err)
	}
	if res.Outcome != OutcomeMerged {
		t.Fatalf("Merge() outcome = %s, want merged (the failure was a flake)", res.Outcome)
	}
	if suite.calls != 2 {
		t.Fatalf("suite runs = %d, want exactly 2 (one gate, one re-run)", suite.calls)
	}
	if suite.runs[1].Attempt != 2 {
		t.Errorf("re-run Attempt = %d, want 2 so its output is archived", suite.runs[1].Attempt)
	}
	if !reflect.DeepEqual(suite.runs[0].Suites, suite.runs[1].Suites) {
		t.Errorf("re-run suites = %v, want the same selection as the first run %v", suite.runs[1].Suites, suite.runs[0].Suites)
	}
}

func TestGateFailsForGoodWhenTheSuiteFailsTwice(t *testing.T) {
	// Arrange — the same failure on both runs.
	target := initTarget(t)
	featureDir := addFeatureWorktree(t, target)
	writeFile(t, featureDir, "feature.txt", "hello\n")
	gitRun(t, featureDir, "add", ".")
	gitRun(t, featureDir, "commit", "-q", "-m", "add feature.txt")

	sink := &recordingSink{}
	suite := &fakeSuite{verdicts: []SuiteResult{
		{Passed: false, Tail: "first tail", OutputPath: "/tmp/first.log"},
		{Passed: false, Tail: "second tail", OutputPath: "/tmp/second.log"},
	}}
	e := newTestDriverWithSuite(t, sink, suite)
	req := withRun(t, sink, Request{Workspace: "/ws/genuine-ws", Name: "genuine-ws", SourceBranch: "feature", SourceDir: featureDir, TargetDir: target})

	// Act.
	res, err := e.Merge(context.Background(), req)

	// Assert — today's behavior, with BOTH archives reachable from the result.
	if err != nil {
		t.Fatalf("Merge() err = %v", err)
	}
	if res.Outcome != OutcomeTestFailed {
		t.Fatalf("Merge() outcome = %s, want test_failed", res.Outcome)
	}
	if suite.calls != 2 {
		t.Fatalf("suite runs = %d, want exactly 2; the gate never retries a second failure", suite.calls)
	}
	if res.TestFailureOutputPath != "/tmp/second.log" {
		t.Errorf("res.TestFailureOutputPath = %q, want the re-run's archive", res.TestFailureOutputPath)
	}
	if !strings.Contains(res.TestFailureTail, "second tail") {
		t.Errorf("res.TestFailureTail = %q, want the re-run's own output", res.TestFailureTail)
	}
	if !strings.Contains(res.TestFailureTail, "/tmp/first.log") {
		t.Errorf("res.TestFailureTail = %q, want it to name the first run's archive too", res.TestFailureTail)
	}
}

// A skipped suite is not a failure, so there is nothing to re-run.
func TestGateNeverRerunsASkippedSuite(t *testing.T) {
	// Arrange.
	target := initTarget(t)
	featureDir := addFeatureWorktree(t, target)
	writeFile(t, featureDir, "feature.txt", "hello\n")
	gitRun(t, featureDir, "add", ".")
	gitRun(t, featureDir, "commit", "-q", "-m", "add feature.txt")

	sink := &recordingSink{}
	suite := skippingSuite()
	e := newTestDriverWithSuite(t, sink, suite)
	req := withRun(t, sink, Request{Workspace: "/ws/skip-ws", Name: "skip-ws", SourceBranch: "feature", SourceDir: featureDir, TargetDir: target})

	// Act.
	if _, err := e.Merge(context.Background(), req); err != nil {
		t.Fatalf("Merge() err = %v", err)
	}

	// Assert.
	if suite.calls != 1 {
		t.Fatalf("suite runs = %d, want 1 (a skip is not a failure)", suite.calls)
	}
}

// A re-run that cannot be CLASSIFIED is an error, never a quiet pass.
func TestGateSurfacesAnUnrunnableRerun(t *testing.T) {
	// Arrange — the first run fails with a verdict, the second cannot run.
	target := initTarget(t)
	featureDir := addFeatureWorktree(t, target)
	writeFile(t, featureDir, "feature.txt", "hello\n")
	gitRun(t, featureDir, "add", ".")
	gitRun(t, featureDir, "commit", "-q", "-m", "add feature.txt")

	sink := &recordingSink{}
	calls := 0
	runner := suiteFunc(func(_ context.Context, _ string, _ SuiteRun) (SuiteResult, error) {
		calls++
		if calls == 1 {
			return SuiteResult{Passed: false, Tail: "boom"}, nil
		}
		return SuiteResult{}, sentinelError("suite binary vanished")
	})
	e := newTestDriverWithSuite(t, sink, runner)
	req := withRun(t, sink, Request{Workspace: "/ws/rerun-err-ws", Name: "rerun-err-ws", SourceBranch: "feature", SourceDir: featureDir, TargetDir: target})

	// Act.
	_, err := e.Merge(context.Background(), req)

	// Assert.
	if err == nil || !strings.Contains(err.Error(), "suite binary vanished") {
		t.Fatalf("Merge() err = %v, want the re-run's failure surfaced", err)
	}
}

// The first run produced a verdict, so the entrypoint was there. Its
// disappearance between the two runs is a contradiction, not a skip to wave
// through.
func TestGateRefusesARerunThatReportsTheSuiteSkipped(t *testing.T) {
	// Arrange.
	target := initTarget(t)
	featureDir := addFeatureWorktree(t, target)
	writeFile(t, featureDir, "feature.txt", "hello\n")
	gitRun(t, featureDir, "add", ".")
	gitRun(t, featureDir, "commit", "-q", "-m", "add feature.txt")

	sink := &recordingSink{}
	suite := &fakeSuite{verdicts: []SuiteResult{
		{Passed: false, Tail: "boom"},
		{Skipped: true, Reason: "the entrypoint vanished"},
	}}
	e := newTestDriverWithSuite(t, sink, suite)
	req := withRun(t, sink, Request{Workspace: "/ws/rerun-skip-ws", Name: "rerun-skip-ws", SourceBranch: "feature", SourceDir: featureDir, TargetDir: target})

	// Act.
	_, err := e.Merge(context.Background(), req)

	// Assert.
	if err == nil || !strings.Contains(err.Error(), "SKIPPED") {
		t.Fatalf("Merge() err = %v, want the contradiction refused", err)
	}
}

// --- the gate is the replay's tail --------------------------------------

// A no-op merge runs NO suite. There is no tree the merge proposes to make that
// the target is not already on, so a suite run could only spend the merge's
// whole cost to re-certify what is already there.
func TestANoOpMergeRunsNoSuiteAtAll(t *testing.T) {
	// Arrange — a source branch that never committed anything.
	target := initTarget(t)
	featureDir := addFeatureWorktree(t, target)
	sink := &recordingSink{}
	suite := &fakeSuite{}
	e := newTestDriverWithSuite(t, sink, suite)
	req := withRun(t, sink, Request{Workspace: "/ws/noop-gate-ws", Name: "noop-gate-ws", SourceBranch: "feature", SourceDir: featureDir, TargetDir: target})

	// Act.
	res, err := e.Merge(context.Background(), req)

	// Assert.
	if err != nil {
		t.Fatalf("Merge() err = %v", err)
	}
	if res.Outcome != OutcomeMerged || !res.AlreadyIncorporated {
		t.Fatalf("Merge() res = %+v, want a merged no-op", res)
	}
	if suite.calls != 0 {
		t.Fatalf("suite runs = %d, want 0 (a no-op merge proposes no new tree)", suite.calls)
	}
}

// A conflict parks BEFORE the gate, because the gate is the replay's tail and
// this replay never reached it.
func TestAConflictMidReplayParksWithoutRunningTheSuite(t *testing.T) {
	// Arrange — the first of two commits collides with a target edit.
	target := initTarget(t)
	featureDir := addFeatureWorktree(t, target)
	writeFile(t, featureDir, "base.txt", "feature\n")
	gitRun(t, featureDir, "add", ".")
	gitRun(t, featureDir, "commit", "-q", "-m", "feature edit")
	writeFile(t, featureDir, "two.txt", "two\n")
	gitRun(t, featureDir, "add", ".")
	gitRun(t, featureDir, "commit", "-q", "-m", "add two.txt")
	writeFile(t, target, "base.txt", "target\n")
	gitRun(t, target, "add", ".")
	gitRun(t, target, "commit", "-q", "-m", "target edit")

	sink := &recordingSink{}
	suite := &fakeSuite{}
	e := newTestDriverWithSuite(t, sink, suite)
	req := withRun(t, sink, Request{Workspace: "/ws/park-gate-ws", Name: "park-gate-ws", SourceBranch: "feature", SourceDir: featureDir, TargetDir: target})

	// Act.
	res, err := e.Merge(context.Background(), req)

	// Assert.
	if err != nil {
		t.Fatalf("Merge() err = %v", err)
	}
	if res.Outcome != OutcomeConflict {
		t.Fatalf("Merge() outcome = %s, want conflict", res.Outcome)
	}
	parkedAt(t, e, req, res)
	if suite.calls != 0 {
		t.Fatalf("suite runs = %d, want 0 (the replay never reached its tail)", suite.calls)
	}
}

// The resume finishes the range and THEN the merge's one gate runs — once, on
// the head that carries the resolution and every commit after it.
func TestTheGateRunsOnceAtTheHeadAResumeReaches(t *testing.T) {
	// Arrange — a parked conflict on the first of two commits, resolved in the
	// rebase worktree.
	target := initTarget(t)
	featureDir := addFeatureWorktree(t, target)
	writeFile(t, featureDir, "base.txt", "feature\n")
	gitRun(t, featureDir, "add", ".")
	gitRun(t, featureDir, "commit", "-q", "-m", "feature edit")
	writeFile(t, featureDir, "two.txt", "two\n")
	gitRun(t, featureDir, "add", ".")
	gitRun(t, featureDir, "commit", "-q", "-m", "add two.txt")
	writeFile(t, target, "base.txt", "target\n")
	gitRun(t, target, "add", ".")
	gitRun(t, target, "commit", "-q", "-m", "target edit")

	sink := &recordingSink{}
	suite := &fakeSuite{}
	e := newTestDriverWithSuite(t, sink, suite)
	req := withRun(t, sink, Request{Workspace: "/ws/resume-gate-ws", Name: "resume-gate-ws", SourceBranch: "feature", SourceDir: featureDir, TargetDir: target})
	parked, err := e.Merge(context.Background(), req)
	if err != nil || parked.Outcome != OutcomeConflict {
		t.Fatalf("setup Merge() res=%+v err=%v, want conflict", parked, err)
	}
	req = parkedAt(t, e, req, parked)
	writeFile(t, req.WorkDir, "base.txt", "resolved\n")
	gitRun(t, req.WorkDir, "add", "base.txt")

	// Act.
	res, err := e.Resume(context.Background(), req)

	// Assert — one gate for the whole merge, run after the last commit replayed.
	if err != nil {
		t.Fatalf("Resume() err = %v", err)
	}
	if res.Outcome != OutcomeMerged {
		t.Fatalf("Resume() outcome = %s, want merged", res.Outcome)
	}
	if suite.calls != 1 {
		t.Fatalf("suite runs = %d, want 1 (the head the resumed replay reached)", suite.calls)
	}
}

// The remediation loop's return edge: the re-gate judges the head the FIX
// produced, not the head that failed.
func TestTheReGateAfterATestFixJudgesTheHeadTheFixProduced(t *testing.T) {
	// Arrange — the head gate fails twice, an agent stages a fix, the re-gate
	// passes. Each run records the tree it saw.
	target := initTarget(t)
	featureDir := addFeatureWorktree(t, target)
	writeFile(t, featureDir, "feature.txt", "broken\n")
	gitRun(t, featureDir, "add", ".")
	gitRun(t, featureDir, "commit", "-q", "-m", "add feature.txt")

	var seen []string
	calls := 0
	runner := suiteFunc(func(_ context.Context, dir string, _ SuiteRun) (SuiteResult, error) {
		calls++
		seen = append(seen, readFile(t, dir, "feature.txt"))
		if calls <= 2 {
			return SuiteResult{Passed: false, Tail: "boom"}, nil
		}
		return SuiteResult{Passed: true}, nil
	})
	sink := &recordingSink{}
	e := newTestDriverWithSuite(t, sink, runner)
	req := withRun(t, sink, Request{Workspace: "/ws/regate-ws", Name: "regate-ws", SourceBranch: "feature", SourceDir: featureDir, TargetDir: target})
	first, err := e.Merge(context.Background(), req)
	if err != nil || first.Outcome != OutcomeTestFailed {
		t.Fatalf("setup Merge() res=%+v err=%v, want test_failed", first, err)
	}
	req = parkedAt(t, e, req, first)
	writeFile(t, req.WorkDir, "feature.txt", "fixed\n")
	gitRun(t, req.WorkDir, "add", "feature.txt")

	// Act.
	res, err := e.ContinueAfterTestFix(context.Background(), req, first.FailingCommit)

	// Assert — three runs (gate, flake re-run, re-gate) and the third saw the fix.
	if err != nil {
		t.Fatalf("ContinueAfterTestFix() err = %v", err)
	}
	if res.Outcome != OutcomeMerged {
		t.Fatalf("ContinueAfterTestFix() outcome = %s, want merged", res.Outcome)
	}
	want := []string{"broken\n", "broken\n", "fixed\n"}
	if !reflect.DeepEqual(seen, want) {
		t.Fatalf("the suite saw feature.txt as %v, want %v (the re-gate judges the fixed head)", seen, want)
	}
}

// The target moves only once the head gate has PASSED, and the move is the
// `--no-ff` merge commit over the head the merge found.
func TestTheTargetMovesOnlyAfterTheReGatePasses(t *testing.T) {
	// Arrange — as above, with the fix landing between the two gates.
	target := initTarget(t)
	featureDir := addFeatureWorktree(t, target)
	writeFile(t, featureDir, "feature.txt", "broken\n")
	gitRun(t, featureDir, "add", ".")
	gitRun(t, featureDir, "commit", "-q", "-m", "add feature.txt")
	before := strings.TrimSpace(gitRun(t, target, "rev-parse", "HEAD"))

	sink := &recordingSink{}
	suite := &fakeSuite{verdicts: []SuiteResult{{Passed: false, Tail: "boom"}, {Passed: false, Tail: "boom"}, {Passed: true}}}
	e := newTestDriverWithSuite(t, sink, suite)
	req := withRun(t, sink, Request{Workspace: "/ws/move-ws", Name: "move-ws", SourceBranch: "feature", SourceDir: featureDir, TargetDir: target})
	first, err := e.Merge(context.Background(), req)
	if err != nil || first.Outcome != OutcomeTestFailed {
		t.Fatalf("setup Merge() res=%+v err=%v, want test_failed", first, err)
	}
	if head := strings.TrimSpace(gitRun(t, target, "rev-parse", "HEAD")); head != before {
		t.Fatalf("target HEAD = %s before the gate passed, want it still at %s", head, before)
	}
	req = parkedAt(t, e, req, first)
	writeFile(t, req.WorkDir, "feature.txt", "fixed\n")
	gitRun(t, req.WorkDir, "add", "feature.txt")

	// Act.
	if _, err := e.ContinueAfterTestFix(context.Background(), req, first.FailingCommit); err != nil {
		t.Fatalf("ContinueAfterTestFix() err = %v", err)
	}

	// Assert — one merge commit, first parent the untouched pre-merge head.
	if parent := strings.TrimSpace(gitRun(t, target, "rev-parse", "HEAD^")); parent != before {
		t.Errorf("HEAD^ = %s, want the pre-merge head %s (the target moved exactly once)", parent, before)
	}
	if got := readFile(t, target, "feature.txt"); got != "fixed\n" {
		t.Errorf("target feature.txt = %q, want the fixed content the passing gate certified", got)
	}
}

// AMENDED FROM TestARepeatedHeadGateFailureKeepsTheFirstFailuresIdentity, which
// pinned the OPPOSITE: a re-gate had to report the FIRST failure's sha so that
// merge.Coordinator's one-attempt-per-failure map could not be fooled by a moving
// head. That rule is abolished — the remediation loop turns until the gate passes
// or the agent escalates — so nothing keys on a stable identity any more, and
// reporting a tree the suite did not run on would now simply be false. The new
// contract is asserted at the same edge and with the same strength.
func TestAReGateAfterAFixReportsTheHeadItActuallyJudged(t *testing.T) {
	// Arrange — a suite that never passes, and an agent that commits something
	// anyway (so the head genuinely moves between the two gates).
	target := initTarget(t)
	featureDir := addFeatureWorktree(t, target)
	writeFile(t, featureDir, "feature.txt", "broken\n")
	gitRun(t, featureDir, "add", ".")
	gitRun(t, featureDir, "commit", "-q", "-m", "add feature.txt")

	sink := &recordingSink{}
	suite := &fakeSuite{verdicts: []SuiteResult{{Passed: false, Tail: "boom"}}}
	e := newTestDriverWithSuite(t, sink, suite)
	req := withRun(t, sink, Request{Workspace: "/ws/identity-ws", Name: "identity-ws", SourceBranch: "feature", SourceDir: featureDir, TargetDir: target})
	first, err := e.Merge(context.Background(), req)
	if err != nil || first.Outcome != OutcomeTestFailed {
		t.Fatalf("setup Merge() res=%+v err=%v, want test_failed", first, err)
	}
	req = parkedAt(t, e, req, first)
	writeFile(t, req.WorkDir, "feature.txt", "still broken, but different\n")
	gitRun(t, req.WorkDir, "add", "feature.txt")
	headBeforeFix := strings.TrimSpace(gitRun(t, req.WorkDir, "rev-parse", "--short", "HEAD"))

	// Act.
	res, err := e.ContinueAfterTestFix(context.Background(), req, first.FailingCommit)

	// Assert.
	if err != nil {
		t.Fatalf("ContinueAfterTestFix() err = %v", err)
	}
	if res.Outcome != OutcomeTestFailed {
		t.Fatalf("ContinueAfterTestFix() outcome = %s, want test_failed", res.Outcome)
	}
	head := strings.TrimSpace(gitRun(t, req.WorkDir, "rev-parse", "--short", "HEAD"))
	if head == headBeforeFix {
		t.Fatalf("the fix did not move the rebase worktree's head off %s, so the test proves nothing", headBeforeFix)
	}
	if res.FailingCommit != head {
		t.Fatalf("re-gate FailingCommit = %q, want the head the suite ran on %q", res.FailingCommit, head)
	}
	if res.FailingSubject == "" {
		t.Fatalf("re-gate FailingSubject is empty; every sentence about this failure names it")
	}
}

// RESTARTABILITY: a run whose replay is COMPLETE but whose gate never passed
// re-enters at the GATE. The re-entry replays nothing (there is nothing left)
// and publishes no further cherry_picking status — it derives that from git,
// exactly as the replay derives its remaining work.
func TestAReEntryWithTheReplayCompleteRunsTheGateAndNoPicks(t *testing.T) {
	// Arrange — a two-commit range fully replayed, whose head gate failed.
	target := initTarget(t)
	featureDir := addFeatureWorktree(t, target)
	for _, name := range []string{"one.txt", "two.txt"} {
		writeFile(t, featureDir, name, name+"\n")
		gitRun(t, featureDir, "add", ".")
		gitRun(t, featureDir, "commit", "-q", "-m", "add "+name)
	}
	sink := &recordingSink{}
	suite := &fakeSuite{verdicts: []SuiteResult{{Passed: false, Tail: "boom"}, {Passed: false, Tail: "boom"}, {Passed: true}}}
	e := newTestDriverWithSuite(t, sink, suite)
	req := withRun(t, sink, Request{Workspace: "/ws/reentry-ws", Name: "reentry-ws", SourceBranch: "feature", SourceDir: featureDir, TargetDir: target})
	first, err := e.Merge(context.Background(), req)
	if err != nil || first.Outcome != OutcomeTestFailed {
		t.Fatalf("setup Merge() res=%+v err=%v, want test_failed", first, err)
	}
	req = parkedAt(t, e, req, first)
	picksBefore := countStatusArm(sink.statuses, armCherryPicking)
	writeFile(t, req.WorkDir, "one.txt", "fixed\n")
	gitRun(t, req.WorkDir, "add", "one.txt")

	// Act.
	res, err := e.ContinueAfterTestFix(context.Background(), req, first.FailingCommit)

	// Assert — the gate ran again and not one commit was replayed.
	if err != nil {
		t.Fatalf("ContinueAfterTestFix() err = %v", err)
	}
	if res.Outcome != OutcomeMerged {
		t.Fatalf("ContinueAfterTestFix() outcome = %s, want merged", res.Outcome)
	}
	if suite.calls != 3 {
		t.Fatalf("suite runs = %d, want 3 (gate, flake re-run, re-gate at the new head)", suite.calls)
	}
	if got := countStatusArm(sink.statuses, armCherryPicking); got != picksBefore {
		t.Fatalf("the re-entry published %d cherry_picking statuses, want none: the replay was already complete", got-picksBefore)
	}
}

// countStatusArm counts the statuses carrying arm.
func countStatusArm(statuses []*frontendv1.MergeStatus, arm string) int {
	n := 0
	for _, s := range statuses {
		if statusArm(s) == arm {
			n++
		}
	}
	return n
}

// --- the gate's selection across a re-entry -----------------------------

// commitOnMain adds a file to the TARGET's main branch before any worktree is
// cut, so both sides of a merge inherit it as a tracked file.
func commitOnMain(t *testing.T, target, rel, content, subject string) {
	t.Helper()
	writeNested(t, target, rel, content)
	gitRun(t, target, "add", ".")
	gitRun(t, target, "commit", "-q", "-m", subject)
}

// containsSuite reports whether suites names one.
func containsSuite(suites []string, one string) bool {
	for _, s := range suites {
		if s == one {
			return true
		}
	}
	return false
}

// A FIX IS NOT BOUND BY THE SOURCE RANGE. The branch touches only the webapp, so
// a selection made from the range alone runs no daemon suite — and the fix the
// remediation turn commits is a daemon change, which would then land gated by
// nothing that covers it.
func TestTheReGateWidensTheSelectionToTheFixsOwnComponent(t *testing.T) {
	// Arrange — a webapp-only branch whose head gate fails.
	target := initTarget(t)
	commitOnMain(t, target, "modules/app/agent-repl/daemon/main.go", "package main\n", "seed the daemon")
	featureDir := addFeatureWorktree(t, target)
	writeNested(t, featureDir, "modules/app/agent-repl/webapp/src/App.tsx", "export const x = 1\n")
	gitRun(t, featureDir, "add", ".")
	gitRun(t, featureDir, "commit", "-q", "-m", "webapp only")

	sink := &recordingSink{}
	suite := &fakeSuite{verdicts: []SuiteResult{{Passed: false, Tail: "boom"}, {Passed: false, Tail: "boom"}, {Passed: true}}}
	e := newTestDriverWithSuite(t, sink, suite)
	req := withRun(t, sink, Request{Workspace: "/ws/widen-fix", Name: "widen-fix", SourceBranch: "feature", SourceDir: featureDir, TargetDir: target})
	first, err := e.Merge(context.Background(), req)
	if err != nil || first.Outcome != OutcomeTestFailed {
		t.Fatalf("setup Merge() res=%+v err=%v, want test_failed", first, err)
	}
	if got := suite.runs[0].Suites; containsSuite(got, "daemon") {
		t.Fatalf("the FIRST gate already selected the daemon suite (%v), so the widening this test is about would be invisible", got)
	}
	req = parkedAt(t, e, req, first)
	// The remediation turn fixes a daemon file, which no replayed commit touches.
	writeNested(t, req.WorkDir, "modules/app/agent-repl/daemon/main.go", "package main // fixed\n")

	// Act.
	res, err := e.ContinueAfterTestFix(context.Background(), req, first.FailingCommit)

	// Assert.
	if err != nil {
		t.Fatalf("ContinueAfterTestFix() err = %v", err)
	}
	if res.Outcome != OutcomeMerged {
		t.Fatalf("ContinueAfterTestFix() outcome = %s, want merged", res.Outcome)
	}
	if got := suite.runs[len(suite.runs)-1].Suites; !containsSuite(got, "daemon") {
		t.Fatalf("the re-gate ran suites %v, want the fix's own daemon suite among them", got)
	}
}

// THE REGRESSION THIS PINS: Resume re-entered the replay with an EMPTY head
// gate, so a resolution that edited a file outside the merge's own range was
// gated by suites that cover none of it. The widening is derived from git on
// every re-entry now, so the paths a resolution took reach the selection whether
// the re-entry came through a test fix or through a resume.
func TestAResumeWidensTheSelectionToWhatTheResolutionTouched(t *testing.T) {
	// Arrange — a webapp-only branch parked on a webapp conflict.
	target := initTarget(t)
	commitOnMain(t, target, "modules/app/agent-repl/daemon/main.go", "package main\n", "seed the daemon")
	commitOnMain(t, target, "modules/app/agent-repl/webapp/src/App.tsx", "export const x = 0\n", "seed the webapp")
	featureDir := addFeatureWorktree(t, target)
	writeNested(t, featureDir, "modules/app/agent-repl/webapp/src/App.tsx", "export const x = 1\n")
	gitRun(t, featureDir, "add", ".")
	gitRun(t, featureDir, "commit", "-q", "-m", "webapp edit")
	writeNested(t, target, "modules/app/agent-repl/webapp/src/App.tsx", "export const x = 2\n")
	gitRun(t, target, "add", ".")
	gitRun(t, target, "commit", "-q", "-m", "target webapp edit")

	sink := &recordingSink{}
	suite := &fakeSuite{}
	e := newTestDriverWithSuite(t, sink, suite)
	req := withRun(t, sink, Request{Workspace: "/ws/widen-resume", Name: "widen-resume", SourceBranch: "feature", SourceDir: featureDir, TargetDir: target})
	parked, err := e.Merge(context.Background(), req)
	if err != nil || parked.Outcome != OutcomeConflict {
		t.Fatalf("setup Merge() res=%+v err=%v, want conflict", parked, err)
	}
	req = parkedAt(t, e, req, parked)
	// The resolution settles the webapp collision AND edits a daemon file, which
	// no commit of the merge's range touches.
	writeNested(t, req.WorkDir, "modules/app/agent-repl/webapp/src/App.tsx", "export const x = 3\n")
	writeNested(t, req.WorkDir, "modules/app/agent-repl/daemon/main.go", "package main // resolved\n")

	// Act.
	res, err := e.Resume(context.Background(), req)

	// Assert.
	if err != nil {
		t.Fatalf("Resume() err = %v", err)
	}
	if res.Outcome != OutcomeMerged {
		t.Fatalf("Resume() outcome = %s, want merged", res.Outcome)
	}
	if len(suite.runs) == 0 {
		t.Fatalf("the resumed replay ran no gate at all")
	}
	if got := suite.runs[len(suite.runs)-1].Suites; !containsSuite(got, "daemon") {
		t.Fatalf("the resumed gate ran suites %v, want the daemon suite the resolution's own edit needs", got)
	}
}

// NOTHING LANDED MEANS NOTHING TO TESTIFY ABOUT. Every planned commit was
// already on the rebased line by patch-id, so the tree the gate would judge is
// the tree the target already carries — and a full suite run for a merge that
// changes nothing is the merge's whole cost spent on nothing.
func TestAMergeWhoseEveryPlannedPickWentEmptyRunsNoGate(t *testing.T) {
	// Arrange — the target already carries the branch's change, committed
	// separately so the SHAs differ and only the patch-id probe can see it.
	target := initTarget(t)
	featureDir := addFeatureWorktree(t, target)
	writeFile(t, featureDir, "feature.txt", "hello\n")
	gitRun(t, featureDir, "add", ".")
	gitRun(t, featureDir, "commit", "-q", "-m", "feature adds the file")
	writeFile(t, target, "feature.txt", "hello\n")
	gitRun(t, target, "add", ".")
	gitRun(t, target, "commit", "-q", "-m", "the target adds the same file")

	sink := &recordingSink{}
	suite := &fakeSuite{}
	e := newTestDriverWithSuite(t, sink, suite)
	req := withRun(t, sink, Request{Workspace: "/ws/empty-ws", Name: "empty-ws", SourceBranch: "feature", SourceDir: featureDir, TargetDir: target})

	// Act.
	res, err := e.Merge(context.Background(), req)

	// Assert.
	if err != nil {
		t.Fatalf("Merge() err = %v", err)
	}
	if res.Outcome != OutcomeMerged || !res.AlreadyIncorporated {
		t.Fatalf("Merge() res = %+v, want a merged no-op", res)
	}
	if suite.calls != 0 {
		t.Fatalf("suite runs = %d, want 0: nothing landed, so there is nothing for a suite to testify about", suite.calls)
	}
}

// --- the remediation loop's escalation exit -----------------------------

// The loop's only exit that is not a passing gate: the agent's own judgement.
func TestAnEscalationRecordEndsTheRemediationWithTheAgentsExplanation(t *testing.T) {
	// Arrange — a failed gate the agent answers with an escalation.
	target := initTarget(t)
	featureDir := addFeatureWorktree(t, target)
	writeFile(t, featureDir, "feature.txt", "broken\n")
	gitRun(t, featureDir, "add", ".")
	gitRun(t, featureDir, "commit", "-q", "-m", "add feature.txt")

	sink := &recordingSink{}
	suite := &fakeSuite{verdicts: []SuiteResult{{Passed: false, Tail: "boom"}}}
	e := newTestDriverWithSuite(t, sink, suite)
	req := withRun(t, sink, Request{Workspace: "/ws/esc-ws", Name: "esc-ws", SourceBranch: "feature", SourceDir: featureDir, TargetDir: target})
	first, err := e.Merge(context.Background(), req)
	if err != nil || first.Outcome != OutcomeTestFailed {
		t.Fatalf("setup Merge() res=%+v err=%v, want test_failed", first, err)
	}
	req = parkedAt(t, e, req, first)
	writeFile(t, req.WorkDir, mergeEscalationFile,
		mergeEscalationMarker+"\nthe suite assumes one writer per store; fixing it means moving the lease\n")
	runsBefore := suite.calls

	// Act.
	res, err := e.ContinueAfterTestFix(context.Background(), req, first.FailingCommit)

	// Assert — the agent's words come back, and no further suite ran.
	if err != nil {
		t.Fatalf("ContinueAfterTestFix() err = %v", err)
	}
	if res.Outcome != OutcomeTestFailed {
		t.Fatalf("ContinueAfterTestFix() outcome = %s, want test_failed", res.Outcome)
	}
	if res.TestFailureEscalation != "the suite assumes one writer per store; fixing it means moving the lease" {
		t.Fatalf("escalation = %q, want the agent's own explanation", res.TestFailureEscalation)
	}
	if suite.calls != runsBefore {
		t.Fatalf("suite runs = %d, want no further run after an escalation (was %d)", suite.calls, runsBefore)
	}
}

// The record is scratch, not work: it must never reach the fix commit, and it
// must never survive to escalate a later turn that did not write it.
func TestAnEscalationRecordIsRemovedFromTheRebaseWorktree(t *testing.T) {
	// Arrange.
	target := initTarget(t)
	featureDir := addFeatureWorktree(t, target)
	writeFile(t, featureDir, "feature.txt", "broken\n")
	gitRun(t, featureDir, "add", ".")
	gitRun(t, featureDir, "commit", "-q", "-m", "add feature.txt")

	sink := &recordingSink{}
	suite := &fakeSuite{verdicts: []SuiteResult{{Passed: false, Tail: "boom"}}}
	e := newTestDriverWithSuite(t, sink, suite)
	req := withRun(t, sink, Request{Workspace: "/ws/esc-rm", Name: "esc-rm", SourceBranch: "feature", SourceDir: featureDir, TargetDir: target})
	first, err := e.Merge(context.Background(), req)
	if err != nil || first.Outcome != OutcomeTestFailed {
		t.Fatalf("setup Merge() res=%+v err=%v, want test_failed", first, err)
	}
	req = parkedAt(t, e, req, first)
	writeFile(t, req.WorkDir, mergeEscalationFile, mergeEscalationMarker+"\nneeds a redesign\n")

	// Act.
	if _, err := e.ContinueAfterTestFix(context.Background(), req, first.FailingCommit); err != nil {
		t.Fatalf("ContinueAfterTestFix() err = %v", err)
	}

	// Assert.
	if _, statErr := os.Stat(filepath.Join(req.WorkDir, mergeEscalationFile)); !os.IsNotExist(statErr) {
		t.Fatalf("the escalation record is still in the rebase worktree (stat err = %v)", statErr)
	}
}

// A FILE WITHOUT THE MARKER IS NOT AN ESCALATION. Anything else would let a
// scratch file an agent happened to leave behind terminate somebody's merge.
func TestAFileWithoutTheMarkerIsNotAnEscalation(t *testing.T) {
	// Arrange.
	target := initTarget(t)
	featureDir := addFeatureWorktree(t, target)
	writeFile(t, featureDir, "feature.txt", "broken\n")
	gitRun(t, featureDir, "add", ".")
	gitRun(t, featureDir, "commit", "-q", "-m", "add feature.txt")

	sink := &recordingSink{}
	suite := &fakeSuite{verdicts: []SuiteResult{{Passed: false, Tail: "boom"}, {Passed: false, Tail: "boom"}, {Passed: true}}}
	e := newTestDriverWithSuite(t, sink, suite)
	req := withRun(t, sink, Request{Workspace: "/ws/esc-bad", Name: "esc-bad", SourceBranch: "feature", SourceDir: featureDir, TargetDir: target})
	first, err := e.Merge(context.Background(), req)
	if err != nil || first.Outcome != OutcomeTestFailed {
		t.Fatalf("setup Merge() res=%+v err=%v, want test_failed", first, err)
	}
	req = parkedAt(t, e, req, first)
	writeFile(t, req.WorkDir, mergeEscalationFile, "I am just a note\n")
	writeFile(t, req.WorkDir, "feature.txt", "fixed\n")

	// Act.
	res, err := e.ContinueAfterTestFix(context.Background(), req, first.FailingCommit)

	// Assert — the merge went on as one more iteration.
	if err != nil {
		t.Fatalf("ContinueAfterTestFix() err = %v", err)
	}
	if res.TestFailureEscalation != "" {
		t.Fatalf("escalation = %q, want none: the file does not open with the marker", res.TestFailureEscalation)
	}
	if res.Outcome != OutcomeMerged {
		t.Fatalf("ContinueAfterTestFix() outcome = %s, want merged (the re-gate passed)", res.Outcome)
	}
}

// --- the copy a user reads ----------------------------------------------

// NO SHA APPEARS IN ANY PUBLISHED CAUSE. The shas ride the status's own sha
// fields, where a tool reads them; every sentence names the commit's subject.
func TestNoPublishedCauseNamesASha(t *testing.T) {
	// Arrange — a merge that picks a commit and then fails its gate, so both the
	// picking and the testing prose are on the wire.
	target := initTarget(t)
	featureDir := addFeatureWorktree(t, target)
	writeFile(t, featureDir, "feature.txt", "broken\n")
	gitRun(t, featureDir, "add", ".")
	gitRun(t, featureDir, "commit", "-q", "-m", "add feature.txt")

	sink := &recordingSink{}
	suite := &fakeSuite{verdicts: []SuiteResult{{Passed: false, Tail: "boom"}}}
	e := newTestDriverWithSuite(t, sink, suite)
	req := withRun(t, sink, Request{Workspace: "/ws/nosha-ws", Name: "nosha-ws", SourceBranch: "feature", SourceDir: featureDir, TargetDir: target})

	// Act.
	res, err := e.Merge(context.Background(), req)

	// Assert.
	if err != nil || res.Outcome != OutcomeTestFailed {
		t.Fatalf("Merge() res=%+v err=%v, want test_failed", res, err)
	}
	picked := strings.TrimSpace(gitRun(t, featureDir, "rev-parse", "--short=7", "HEAD"))
	for _, cause := range sink.causes() {
		for _, sha := range []string{res.FailingCommit, picked} {
			if strings.Contains(cause, sha) {
				t.Errorf("published cause %q names the sha %s; the copy a user reads names subjects", cause, sha)
			}
		}
	}
}
