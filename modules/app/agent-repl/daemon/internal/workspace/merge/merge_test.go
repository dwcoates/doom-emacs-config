package merge

import (
	"context"
	"os"
	"os/exec"
	"path/filepath"
	"slices"
	"strings"
	"testing"
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

func newTestDriver(t *testing.T, sink StateSink) *Driver {
	t.Helper()
	e, err := NewDriver(Config{Logf: t.Logf, Sink: sink})
	if err != nil {
		t.Fatalf("NewDriver: %v", err)
	}
	return e
}

// A Git hook exports repository bindings (GIT_DIR, GIT_INDEX_FILE, ...) into
// every child it runs. merge.Driver's git must operate on `-C dir` and
// nothing else, so gitCmd strips those bindings — otherwise a daemon (or this
// suite) running under the repo's own pre-commit hook cherry-picks into the
// HOOK'S repository instead of the fixture's.
func TestGitCmdStripsInheritedRepositoryBindings(t *testing.T) {
	// Arrange — a hook-shaped environment.
	t.Setenv("GIT_DIR", "/somewhere/.git")
	t.Setenv("GIT_INDEX_FILE", "/somewhere/.git/index")
	t.Setenv("GIT_WORK_TREE", "/somewhere")

	// Act.
	cmd := gitCmd(context.Background(), t.TempDir(), "status")

	// Assert — the bindings are gone and unrelated variables survive.
	for _, entry := range cmd.Env {
		for _, banned := range []string{"GIT_DIR=", "GIT_INDEX_FILE=", "GIT_WORK_TREE="} {
			if strings.HasPrefix(entry, banned) {
				t.Fatalf("gitCmd env kept %q; the driver's git would target the hook's repository", entry)
			}
		}
	}
	if os.Getenv("PATH") != "" && !slices.ContainsFunc(cmd.Env, func(e string) bool { return strings.HasPrefix(e, "PATH=") }) {
		t.Fatalf("gitCmd env dropped PATH; only repository bindings may be stripped")
	}
}

// gitRun runs a git command in dir, failing the test on error. Test-driver
// git only; the driver runs its own git.
func gitRun(t *testing.T, dir string, args ...string) string {
	t.Helper()
	gitArgs := append([]string{"-c", "core.hooksPath=/dev/null", "-C", dir}, args...)
	cmd := exec.Command("git", gitArgs...)
	cmd.Env = gitFixtureEnv()
	out, err := cmd.CombinedOutput()
	if err != nil {
		t.Fatalf("git %v in %s: %v\n%s", args, dir, err, out)
	}
	return string(out)
}

// gitFixtureEnv removes repository bindings exported by a parent Git hook.
// Without this boundary, scratch `git add` and `git commit` commands can
// rewrite the caller's real staging index instead of the temporary repo.
func gitFixtureEnv() []string {
	env := make([]string, 0, len(os.Environ()))
	for _, entry := range os.Environ() {
		switch {
		case strings.HasPrefix(entry, "GIT_DIR="),
			strings.HasPrefix(entry, "GIT_WORK_TREE="),
			strings.HasPrefix(entry, "GIT_INDEX_FILE="),
			strings.HasPrefix(entry, "GIT_PREFIX="):
			continue
		default:
			env = append(env, entry)
		}
	}
	return env
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
		{"nil logf", Config{Sink: &recordingSink{}}, true},
		{"nil sink", Config{Logf: func(string, ...any) {}}, true},
		{"both present", Config{Logf: func(string, ...any) {}, Sink: &recordingSink{}}, false},
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
	base := Request{Workspace: "/ws/ws", Name: "ws", SourceBranch: "feature", SourceDir: "/s", TargetDir: "/t"}
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
	req := Request{Workspace: "/ws/clean-ws", Name: "clean-ws", SourceBranch: "feature", SourceDir: featureDir, TargetDir: target}

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
	req := Request{Workspace: "/ws/keyed-ws", Name: "keyed-ws", SourceBranch: "feature", SourceDir: featureDir, TargetDir: target}

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
	req := Request{Workspace: "/ws/cf-ws", Name: "cf-ws", SourceBranch: "feature", SourceDir: featureDir, TargetDir: target}

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
	req := Request{Workspace: "/ws/rz-ws", Name: "rz-ws", SourceBranch: "feature", SourceDir: featureDir, TargetDir: target}

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
	e := newTestDriver(t, sink)
	req := Request{Workspace: "/ws/no-pick", Name: "no-pick", SourceBranch: "feature", SourceDir: featureDir, TargetDir: target}

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
	e := newTestDriver(t, sink)
	req := Request{Workspace: "/ws/fail-ws", Name: "fail-ws", SourceBranch: "feature", SourceDir: featureDir, TargetDir: target}

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
	e := newTestDriver(t, sink)
	req := Request{Workspace: "/ws/dirty-ws", Name: "dirty-ws", SourceBranch: "feature", SourceDir: featureDir, TargetDir: target}

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
	req := Request{Workspace: "/ws/nb-ws", Name: "nb-ws", SourceBranch: "does-not-exist", SourceDir: featureDir, TargetDir: target}

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
	// transition. The driver must surface (not swallow) it.
	target := initTarget(t)
	featureDir := addFeatureWorktree(t, target)
	writeFile(t, featureDir, "feature.txt", "hello\n")
	gitRun(t, featureDir, "add", ".")
	gitRun(t, featureDir, "commit", "-q", "-m", "add feature.txt")

	sink := &recordingSink{failOn: PhaseMerged}
	e := newTestDriver(t, sink)
	req := Request{Workspace: "/ws/sink-ws", Name: "sink-ws", SourceBranch: "feature", SourceDir: featureDir, TargetDir: target}

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
