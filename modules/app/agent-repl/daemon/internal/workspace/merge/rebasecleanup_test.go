package merge

import (
	"context"
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"sync"
	"testing"
)

// THE TEARDOWN FUNNEL. One function owns the removal of a rebase worktree, it
// judges itself on the postcondition (the tree AND its temp parent are gone,
// and git no longer registers them), and a second pass over a path it already
// removed says nothing at all.

// rebaseLog is a Logf that records every line, so a test can assert on what the
// funnel did and did NOT say. A silent no-op is a behavior here, not a detail:
// the double-cleanup noise this file exists to end was a log line.
type rebaseLog struct {
	mu    sync.Mutex
	lines []string
}

func (l *rebaseLog) logf(format string, args ...any) {
	l.mu.Lock()
	defer l.mu.Unlock()
	l.lines = append(l.lines, strings.TrimSpace(fmt.Sprintf(format, args...)))
}

func (l *rebaseLog) matching(needle string) []string {
	l.mu.Lock()
	defer l.mu.Unlock()
	var out []string
	for _, line := range l.lines {
		if strings.Contains(line, needle) {
			out = append(out, line)
		}
	}
	return out
}

// newLoggingDriver builds a driver whose log this test can read back, rooted in
// this test's own temp dir.
func newLoggingDriver(t *testing.T, log *rebaseLog) *Driver {
	t.Helper()
	return newLoggingDriverRooted(t, log, t.TempDir())
}

// newLoggingDriverRooted is newLoggingDriver with the injected rebase root the
// caller names — the sweep's summary tests plant orphans in it.
func newLoggingDriverRooted(t *testing.T, log *rebaseLog, root string) *Driver {
	t.Helper()
	e, err := NewDriver(Config{Logf: log.logf, Sink: &recordingSink{}, Suite: skippingSuite(), RebaseRoot: root})
	if err != nil {
		t.Fatalf("NewDriver: %v", err)
	}
	return e
}

// arrangeRebaseWorktree makes a real rebase worktree of target exactly the way
// createRebaseWorktree does, and returns the request that names it.
func arrangeRebaseWorktree(t *testing.T, e *Driver, target string) Request {
	t.Helper()
	req := Request{Name: "ws", Workspace: "/ws/ws", TargetDir: target}
	head := strings.TrimSpace(gitRun(t, target, "rev-parse", "HEAD"))
	work, err := e.createRebaseWorktree(context.Background(), req, head)
	if err != nil {
		t.Fatalf("createRebaseWorktree: %v", err)
	}
	req.WorkDir = work
	return req
}

func TestCleanupRemovesTheWorktreeAndItsTempParent(t *testing.T) {
	// Arrange
	target := initTarget(t)
	e := newTestDriver(t, &recordingSink{})
	req := arrangeRebaseWorktree(t, e, target)

	// Act
	if err := e.Cleanup(context.Background(), req); err != nil {
		t.Fatalf("Cleanup: %v", err)
	}

	// Assert — the leaf is what git owned; the PARENT is what os.MkdirTemp made,
	// and leaving it behind is the leak that filled $TMPDIR.
	parent := filepath.Dir(req.WorkDir)
	if _, err := os.Lstat(parent); !os.IsNotExist(err) {
		t.Fatalf("Lstat(%s) = %v, want the temp parent removed", parent, err)
	}
}

func TestCleanupDeregistersTheWorktreeFromTheRepository(t *testing.T) {
	// Arrange
	target := initTarget(t)
	e := newTestDriver(t, &recordingSink{})
	req := arrangeRebaseWorktree(t, e, target)

	// Act
	if err := e.Cleanup(context.Background(), req); err != nil {
		t.Fatalf("Cleanup: %v", err)
	}

	// Assert — a registration that outlives its directory is what grew the
	// repository's `git worktree list` to ~190 entries.
	if got := gitRun(t, target, "worktree", "list", "--porcelain"); strings.Contains(got, rebaseWorktreeMarker) {
		t.Fatalf("worktree list still carries a rebase worktree:\n%s", got)
	}
}

func TestASecondCleanupOfTheSamePathIsASilentNoOp(t *testing.T) {
	// Arrange — the exact production shape: merge.Driver tears its own cycle's
	// tree down, and merge.Coordinator's terminal defer then fires for the same
	// WorkDir the Result still carries.
	target := initTarget(t)
	log := &rebaseLog{}
	e := newLoggingDriver(t, log)
	req := arrangeRebaseWorktree(t, e, target)
	if err := e.Cleanup(context.Background(), req); err != nil {
		t.Fatalf("first Cleanup: %v", err)
	}

	// Act
	err := e.Cleanup(context.Background(), req)

	// Assert — no error, and not one further word about a directory that was
	// already gone.
	if err != nil {
		t.Fatalf("second Cleanup: %v, want a silent no-op", err)
	}
	if got := log.matching("REMOVED"); len(got) != 1 {
		t.Fatalf("REMOVED lines = %v, want exactly the one the first pass logged", got)
	}
}

func TestCleanupSurfacesATempDirectoryThatWillNotDelete(t *testing.T) {
	// Arrange — the temp parent sits under a directory nothing may unlink from,
	// so its removal genuinely cannot succeed.
	target := initTarget(t)
	e := newTestDriver(t, &recordingSink{})
	grand := t.TempDir()
	parent := filepath.Join(grand, rebaseWorktreeMarker+"stuck")
	if err := os.Mkdir(parent, 0o755); err != nil {
		t.Fatalf("Mkdir: %v", err)
	}
	work := filepath.Join(parent, "rebase")
	gitRun(t, target, "worktree", "add", "-q", "--detach", work)
	if err := os.Chmod(grand, 0o555); err != nil {
		t.Fatalf("Chmod: %v", err)
	}
	t.Cleanup(func() { _ = os.Chmod(grand, 0o755) })

	// Act
	err := e.Cleanup(context.Background(), Request{Name: "ws", TargetDir: target, WorkDir: work})

	// Assert — a removal that did not happen is REPORTED. Tolerating it here is
	// how a leak becomes invisible.
	if err == nil || !strings.Contains(err.Error(), parent) {
		t.Fatalf("Cleanup error = %v, want the undeletable temp directory %s named", err, parent)
	}
}

func TestCleanupSurfacesAWorktreeThatSurvivesEveryRemoval(t *testing.T) {
	// Arrange — git cannot remove it (the repository knows nothing about it) and
	// the filesystem cannot either, so the tree is still standing at the end.
	target := initTarget(t)
	e := newTestDriver(t, &recordingSink{})
	parent := filepath.Join(t.TempDir(), rebaseWorktreeMarker+"live")
	work := filepath.Join(parent, "rebase")
	if err := os.MkdirAll(work, 0o755); err != nil {
		t.Fatalf("MkdirAll: %v", err)
	}
	if err := os.Chmod(parent, 0o555); err != nil {
		t.Fatalf("Chmod: %v", err)
	}
	t.Cleanup(func() { _ = os.Chmod(parent, 0o755) })

	// Act
	err := e.Cleanup(context.Background(), Request{Name: "ws", TargetDir: target, WorkDir: work})

	// Assert
	if err == nil || !strings.Contains(err.Error(), work) {
		t.Fatalf("Cleanup error = %v, want the surviving worktree %s named", err, work)
	}
}

func TestCleanupSaysNothingForARequestWithNoWorktree(t *testing.T) {
	// Arrange — a run that never got as far as making one.
	log := &rebaseLog{}
	e := newLoggingDriver(t, log)

	// Act
	err := e.Cleanup(context.Background(), Request{Name: "ws", TargetDir: t.TempDir()})

	// Assert
	if err != nil {
		t.Fatalf("Cleanup: %v, want a no-op", err)
	}
	if got := log.matching("rebase worktree"); len(got) != 0 {
		t.Fatalf("log = %v, want nothing said about a worktree that never existed", got)
	}
}

func TestRebaseRemovalRootRefusesAParentThisPipelineDoesNotOwn(t *testing.T) {
	tests := []struct {
		name    string
		workDir string
		want    string
	}{
		{
			name:    "the temp parent createRebaseWorktree made",
			workDir: "/tmp/" + rebaseWorktreeMarker + "123/rebase",
			want:    "/tmp/" + rebaseWorktreeMarker + "123",
		},
		{
			// The answer goes straight to os.RemoveAll, so a path that is not
			// shaped the way this pipeline makes them must never widen the
			// removal to somebody else's directory.
			name:    "a worktree somewhere else entirely",
			workDir: "/Users/someone/checkout",
			want:    "/Users/someone/checkout",
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if got := rebaseRemovalRoot(tt.workDir); got != tt.want {
				t.Fatalf("rebaseRemovalRoot(%q) = %q, want %q", tt.workDir, got, tt.want)
			}
		})
	}
}

// movingTargetSuite moves the TARGET's head the first time the gate runs, which
// is what an external writer landing mid-merge looks like from inside the
// rebase. It records the rebase worktree each gate ran in, so a test can name
// the cycle that was superseded.
type movingTargetSuite struct {
	t      *testing.T
	target string
	moved  bool
	seen   []string
}

func (s *movingTargetSuite) RunSuite(_ context.Context, dir string, _ SuiteRun) (SuiteResult, error) {
	s.seen = append(s.seen, dir)
	if !s.moved {
		s.moved = true
		writeFile(s.t, s.target, "external.txt", "somebody else landed first\n")
		gitRun(s.t, s.target, "add", ".")
		gitRun(s.t, s.target, "commit", "-q", "-m", "external commit")
	}
	return SuiteResult{Skipped: true, Reason: "fixture repo declares no test entrypoint"}, nil
}

func TestATargetMovedRestartTearsDownTheSupersededCyclesWorktree(t *testing.T) {
	// Arrange — the re-rebase loop is UNBOUNDED, so a busy target can supersede
	// cycle after cycle. Each one made a temp worktree.
	target := initTarget(t)
	featureDir := addFeatureWorktree(t, target)
	writeFile(t, featureDir, "feature.txt", "hello\n")
	gitRun(t, featureDir, "add", ".")
	gitRun(t, featureDir, "commit", "-q", "-m", "add feature.txt")
	sink := &recordingSink{}
	suite := &movingTargetSuite{t: t, target: target}
	e := newTestDriverWithSuite(t, sink, suite)
	req := withRun(t, sink, Request{Workspace: "/ws/moved", Name: "moved", SourceBranch: "feature", SourceDir: featureDir, TargetDir: target})

	// Act
	res, err := e.Merge(context.Background(), req)

	// Assert
	if err != nil {
		t.Fatalf("Merge: %v", err)
	}
	if res.Outcome != OutcomeMerged {
		t.Fatalf("outcome = %s, want merged", res.Outcome)
	}
	if len(suite.seen) < 2 {
		t.Fatalf("gate ran in %v, want a superseded cycle and a restarted one", suite.seen)
	}
	superseded := filepath.Dir(suite.seen[0])
	if _, err := os.Lstat(superseded); !os.IsNotExist(err) {
		t.Fatalf("Lstat(%s) = %v, want the superseded cycle's temp directory gone", superseded, err)
	}
}

func TestASuccessfulMergeLeavesNoTempDirectoryBehind(t *testing.T) {
	// Arrange — a whole real merge, which is the path that ran 893 times and
	// left 893 directories.
	target := initTarget(t)
	featureDir := addFeatureWorktree(t, target)
	writeFile(t, featureDir, "feature.txt", "hello\n")
	gitRun(t, featureDir, "add", ".")
	gitRun(t, featureDir, "commit", "-q", "-m", "add feature.txt")
	sink := &recordingSink{}
	e := newTestDriver(t, sink)
	req := withRun(t, sink, Request{Workspace: "/ws/leak", Name: "leak", SourceBranch: "feature", SourceDir: featureDir, TargetDir: target})

	// Act
	res, err := e.Merge(context.Background(), req)

	// Assert
	if err != nil {
		t.Fatalf("Merge: %v", err)
	}
	if res.Outcome != OutcomeMerged {
		t.Fatalf("outcome = %s, want merged", res.Outcome)
	}
	parent := filepath.Dir(res.WorkDir)
	if _, err := os.Lstat(parent); !os.IsNotExist(err) {
		t.Fatalf("Lstat(%s) = %v, want the merge's temp directory gone", parent, err)
	}
}
