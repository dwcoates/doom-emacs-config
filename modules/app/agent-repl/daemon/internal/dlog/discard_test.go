package dlog

import (
	"errors"
	"os"
	"path/filepath"
	"slices"
	"testing"

	"golang.org/x/sys/unix"
)

// discardFixture is real surfaces with one workspace whose daemon sink has
// been written, so its target and canonical link exist.
type discardFixture struct {
	s       *surfaces
	logsDir string
	dir     string
	id      string
	link    string
	target  string
}

func newDiscardFixture(t *testing.T) discardFixture {
	t.Helper()
	s, runLog := testSurfaces(t)
	dir := sinkKey(t, t.TempDir())
	log, err := s.Workspace(dir)
	if err != nil {
		t.Fatalf("Workspace: %v", err)
	}
	log.Info("daemon.workspace.opened", "opened", nil)
	link := filepath.Join(dir, ".claude", "emacs", "daemon.log")
	target, err := os.Readlink(link)
	if err != nil {
		t.Fatalf("read the canonical link: %v", err)
	}
	return discardFixture{s: s, logsDir: filepath.Dir(runLog), dir: dir, id: mintedTestID(dir), link: link, target: target}
}

// present reports whether path names anything, a dangling link included.
func present(t *testing.T, path string) bool {
	t.Helper()
	_, err := os.Lstat(path)
	if err != nil && !errors.Is(err, os.ErrNotExist) {
		t.Fatalf("lstat %s: %v", path, err)
	}
	return err == nil
}

func TestDiscardWorkspaceRemovesItsTargetsAndTheLinksThatNamedThem(t *testing.T) {
	// Arrange.
	f := newDiscardFixture(t)
	info, err := os.Stat(f.target)
	if err != nil {
		t.Fatalf("stat the target: %v", err)
	}

	// Act.
	report, err := f.s.DiscardWorkspace(f.dir, f.id, false)

	// Assert.
	if err != nil {
		t.Fatalf("DiscardWorkspace: %v", err)
	}
	if len(report.Removed) != 1 || report.Removed[0].Path != f.target || report.Removed[0].Bytes != info.Size() {
		t.Fatalf("Removed = %+v, want the one target %s with %d bytes", report.Removed, f.target, info.Size())
	}
	if len(report.Links) != 1 || report.Links[0] != f.link {
		t.Fatalf("Links = %v, want %s", report.Links, f.link)
	}
	if present(t, f.target) || present(t, f.link) {
		t.Fatalf("the target or its canonical link is still on disk")
	}
}

func TestDiscardWorkspaceLeavesOtherWorkspacesAndEmacsFilesAlone(t *testing.T) {
	// Arrange: another workspace's target, and this workspace's Emacs file.
	f := newDiscardFixture(t)
	other := filepath.Join(f.logsDir, "agent-repl-ffffffffffffffff-daemon-1.log")
	emacs := filepath.Join(f.logsDir, "agent-repl-"+f.id+"-emacs-AbCdEf.log")
	for _, path := range []string{other, emacs} {
		if err := os.WriteFile(path, []byte("x\n"), 0o644); err != nil {
			t.Fatalf("write %s: %v", path, err)
		}
	}

	// Act.
	if _, err := f.s.DiscardWorkspace(f.dir, f.id, false); err != nil {
		t.Fatalf("DiscardWorkspace: %v", err)
	}

	// Assert.
	if !present(t, other) || !present(t, emacs) {
		t.Fatalf("a file that is not this workspace's daemon-written log was removed")
	}
}

func TestDiscardWorkspaceSkipsAFamilyAnotherWriterHolds(t *testing.T) {
	// Arrange: a second descriptor holds the target's shared lock, as a shim
	// that inherited it does.
	f := newDiscardFixture(t)
	holder, err := os.Open(f.target)
	if err != nil {
		t.Fatalf("open the target: %v", err)
	}
	t.Cleanup(func() { holder.Close() })
	if err := unix.Flock(int(holder.Fd()), unix.LOCK_SH); err != nil {
		t.Fatalf("hold the target: %v", err)
	}

	// Act.
	report, err := f.s.DiscardWorkspace(f.dir, f.id, false)

	// Assert.
	if err != nil {
		t.Fatalf("DiscardWorkspace: %v", err)
	}
	if len(report.Removed) != 0 || len(report.Links) != 0 || len(report.Held) != 1 || report.Held[0] != f.target {
		t.Fatalf("report = %+v, want only the held target %s", report, f.target)
	}
	if !present(t, f.target) || !present(t, f.link) {
		t.Fatalf("a held family or its canonical link was removed")
	}
}

func TestDiscardWorkspaceJudgingOnlyRemovesNothing(t *testing.T) {
	// Arrange.
	f := newDiscardFixture(t)

	// Act.
	report, err := f.s.DiscardWorkspace(f.dir, f.id, true)

	// Assert.
	if err != nil {
		t.Fatalf("DiscardWorkspace: %v", err)
	}
	if len(report.Removed) != 1 || report.Removed[0].Path != f.target || len(report.Links) != 1 {
		t.Fatalf("report = %+v, want the target and its link judged removable", report)
	}
	if !present(t, f.target) || !present(t, f.link) {
		t.Fatalf("a judgement removed the target or its canonical link")
	}
}

func TestDiscardWorkspaceKeepsALinkNamingSomethingElse(t *testing.T) {
	// Arrange: the canonical link is re-pointed at a file that is not removed.
	f := newDiscardFixture(t)
	elsewhere := filepath.Join(t.TempDir(), "kept.log")
	if err := os.WriteFile(elsewhere, nil, 0o644); err != nil {
		t.Fatalf("write: %v", err)
	}
	if err := replaceLink(f.link, elsewhere); err != nil {
		t.Fatalf("re-point the link: %v", err)
	}

	// Act.
	report, err := f.s.DiscardWorkspace(f.dir, f.id, false)

	// Assert.
	if err != nil {
		t.Fatalf("DiscardWorkspace: %v", err)
	}
	if len(report.Links) != 0 || !present(t, f.link) {
		t.Fatalf("Links = %v; a link naming a file that was not removed must stay", report.Links)
	}
}

func TestARecordAfterADiscardOpensAFreshTargetAndLink(t *testing.T) {
	// Arrange.
	f := newDiscardFixture(t)
	if _, err := f.s.DiscardWorkspace(f.dir, f.id, false); err != nil {
		t.Fatalf("DiscardWorkspace: %v", err)
	}

	// Act.
	log, err := f.s.Workspace(f.dir)
	if err != nil {
		t.Fatalf("Workspace: %v", err)
	}
	log.Info("daemon.workspace.later", "later", nil)

	// Assert: only the later record is readable through the link.
	records := workspaceRecords(t, f.dir, "daemon")
	if !hasOperation(records, "daemon.workspace.later") || hasOperation(records, "daemon.workspace.opened") {
		t.Fatalf("records after the discard = %v, want only the later one", records)
	}
}

func TestDiscardWorkspaceOfAGoneDirectoryRemovesItsTargets(t *testing.T) {
	// Arrange: the workspace's directory is removed, as an expired worktree's is.
	f := newDiscardFixture(t)
	if err := os.RemoveAll(f.dir); err != nil {
		t.Fatalf("remove the workspace directory: %v", err)
	}

	// Act.
	report, err := f.s.DiscardWorkspace(f.dir, f.id, false)

	// Assert.
	if err != nil {
		t.Fatalf("DiscardWorkspace: %v", err)
	}
	if len(report.Removed) != 1 || len(report.Links) != 0 || present(t, f.target) {
		t.Fatalf("report = %+v, want the target removed and no link named", report)
	}
}

func TestDiscardWorkspaceRefusals(t *testing.T) {
	cases := []struct {
		name    string
		id      string
		arrange func(s *surfaces)
		want    error
	}{
		{name: "no workspace id", id: "", arrange: func(*surfaces) {}},
		{name: "closed surfaces", id: "0123456789abcdef", arrange: func(s *surfaces) { s.Close() }, want: errSurfacesClosed},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			s, _ := testSurfaces(t)
			tc.arrange(s)

			// Act.
			_, err := s.DiscardWorkspace(t.TempDir(), tc.id, false)

			// Assert.
			if err == nil || (tc.want != nil && !errors.Is(err, tc.want)) {
				t.Fatalf("DiscardWorkspace = %v, want a refusal (%v)", err, tc.want)
			}
		})
	}
}

func TestDiscardWorkspaceRunsTheBeforePruneFunctionBeforeItRemoves(t *testing.T) {
	tests := []struct {
		name        string
		judgeOnly   bool
		wantReasons []string
	}{
		{name: "a discard tallies its files first", wantReasons: []string{"discard"}},
		{name: "judging only removes nothing, so tallies nothing", judgeOnly: true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			f := newDiscardFixture(t)
			var reasons []string
			existed := false
			f.s.BindBeforePrune(func(reason string) {
				reasons = append(reasons, reason)
				existed = present(t, f.target)
				// The function runs with no lock of the surfaces held: it logs.
				f.s.Global().Info("daemon.dlog.test", "tallied", nil)
			})

			// Act.
			if _, err := f.s.DiscardWorkspace(f.dir, f.id, tt.judgeOnly); err != nil {
				t.Fatalf("DiscardWorkspace: %v", err)
			}

			// Assert.
			if !slices.Equal(reasons, tt.wantReasons) || (len(reasons) > 0 && !existed) {
				t.Fatalf("before-prune ran with %v, the target existing %v; want %v, before the target went", reasons, existed, tt.wantReasons)
			}
		})
	}
}
