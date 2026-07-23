package merge

import (
	"context"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"
)

// --- fakes & fixtures ---------------------------------------------------

// recordingSink captures every transition the engine emits, in order. When
// failOn is set, RecordMergeTransition returns an error for that phase (to
// exercise sink-error propagation).
type recordingSink struct {
	got    []transition
	failOn Phase
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

func newTestEngine(t *testing.T, sink StateSink) *Engine {
	t.Helper()
	e, err := NewEngine(Config{Logf: t.Logf, Sink: sink})
	if err != nil {
		t.Fatalf("NewEngine: %v", err)
	}
	return e
}

// gitRun runs a git command in dir, failing the test on error. Test-driver
// git only; the engine runs its own git.
func gitRun(t *testing.T, dir string, args ...string) string {
	t.Helper()
	cmd := exec.Command("git", append([]string{"-C", dir}, args...)...)
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
// repo-local identity so the engine's cherry-pick can commit hermetically.
func initTarget(t *testing.T) string {
	t.Helper()
	dir := t.TempDir()
	gitRun(t, dir, "init", "-q", "-b", "main")
	gitRun(t, dir, "config", "user.email", "test@example.com")
	gitRun(t, dir, "config", "user.name", "Test")
	gitRun(t, dir, "config", "commit.gpgsign", "false")
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

func TestNewEngineRequiresDependencies(t *testing.T) {
	tests := []struct {
		name    string
		cfg     Config
		wantErr bool
	}{
		{"nil logf", Config{Sink: &recordingSink{}}, true},
		{"nil sink", Config{Logf: func(string, ...any) {}}, true},
		{"both present", Config{Logf: func(string, ...any) {}, Sink: &recordingSink{}}, false},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Act.
			_, err := NewEngine(tc.cfg)
			// Assert.
			if (err != nil) != tc.wantErr {
				t.Fatalf("NewEngine err = %v, wantErr = %v", err, tc.wantErr)
			}
		})
	}
}

func TestMergeRejectsIncompleteRequest(t *testing.T) {
	base := Request{Workspace: "ws", SourceBranch: "feature", SourceDir: "/s", TargetDir: "/t"}
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
			e := newTestEngine(t, sink)
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
	e := newTestEngine(t, sink)
	req := Request{Workspace: "clean-ws", SourceBranch: "feature", SourceDir: featureDir, TargetDir: target}

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
	assertPhases(t, sink.phases(), PhaseMerging, PhaseMerged)
}

// --- conflict detection -------------------------------------------------

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
	e := newTestEngine(t, sink)
	req := Request{Workspace: "cf-ws", SourceBranch: "feature", SourceDir: featureDir, TargetDir: target}

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
		t.Errorf("CHERRY_PICK_HEAD absent — engine aborted the conflict instead of leaving it in tree")
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
	e := newTestEngine(t, sink)
	req := Request{Workspace: "rz-ws", SourceBranch: "feature", SourceDir: featureDir, TargetDir: target}

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
	// The full ordered sequence across Merge + Resume.
	assertPhases(t, sink.phases(), PhaseMerging, PhaseMergeConflict, PhaseMerging, PhaseMerged)
}

func TestResumeWithoutInProgressPickFails(t *testing.T) {
	// Arrange: a clean repo with no paused cherry-pick.
	target := initTarget(t)
	featureDir := addFeatureWorktree(t, target)
	sink := &recordingSink{}
	e := newTestEngine(t, sink)
	req := Request{Workspace: "no-pick", SourceBranch: "feature", SourceDir: featureDir, TargetDir: target}

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

// --- merge_failed on a non-cherry-pick (non-conflict) error -------------

func TestMergeFailedOnNonConflictError(t *testing.T) {
	// Arrange: put a merge commit early in the range so `cherry-pick -x`
	// aborts on it (a merge with no -m) leaving a later commit unapplied and
	// no CHERRY_PICK_HEAD — the elisp silent-failure sentinel.
	target := initTarget(t)
	featureDir := addFeatureWorktree(t, target)
	// side branch off feature(=A) with commit S.
	gitRun(t, featureDir, "checkout", "-q", "-b", "side")
	writeFile(t, featureDir, "s.txt", "s\n")
	gitRun(t, featureDir, "add", ".")
	gitRun(t, featureDir, "commit", "-q", "-m", "S")
	// back on feature, merge side (creates merge commit M), then commit F2.
	gitRun(t, featureDir, "checkout", "-q", "feature")
	gitRun(t, featureDir, "merge", "-q", "--no-ff", "-m", "merge side", "side")
	writeFile(t, featureDir, "f2.txt", "f2\n")
	gitRun(t, featureDir, "add", ".")
	gitRun(t, featureDir, "commit", "-q", "-m", "F2")

	sink := &recordingSink{}
	e := newTestEngine(t, sink)
	req := Request{Workspace: "fail-ws", SourceBranch: "feature", SourceDir: featureDir, TargetDir: target}

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
		t.Errorf("CHERRY_PICK_HEAD present on a non-conflict failure")
	}
	if tags := gitRun(t, target, "tag", "-l", "merge/fail-ws"); strings.TrimSpace(tags) != "" {
		t.Errorf("merge/fail-ws tagged on a failed merge; git tag -l = %q", tags)
	}
	assertPhases(t, sink.phases(), PhaseMerging, PhaseMergeFailed)
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
	e := newTestEngine(t, sink)
	req := Request{Workspace: "dirty-ws", SourceBranch: "feature", SourceDir: featureDir, TargetDir: target}

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
	e := newTestEngine(t, sink)
	req := Request{Workspace: "nb-ws", SourceBranch: "does-not-exist", SourceDir: featureDir, TargetDir: target}

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

// --- sink-error propagation ---------------------------------------------

func TestMergeSurfacesSinkError(t *testing.T) {
	// Arrange: a clean cherry-pick, but the sink rejects the terminal merged
	// transition. The engine must surface (not swallow) it.
	target := initTarget(t)
	featureDir := addFeatureWorktree(t, target)
	writeFile(t, featureDir, "feature.txt", "hello\n")
	gitRun(t, featureDir, "add", ".")
	gitRun(t, featureDir, "commit", "-q", "-m", "add feature.txt")

	sink := &recordingSink{failOn: PhaseMerged}
	e := newTestEngine(t, sink)
	req := Request{Workspace: "sink-ws", SourceBranch: "feature", SourceDir: featureDir, TargetDir: target}

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
