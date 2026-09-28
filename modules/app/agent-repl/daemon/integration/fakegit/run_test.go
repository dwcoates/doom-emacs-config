package fakegit

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
)

// world builds a one-repository fixture rooted at a real directory, because the
// worktree commands have filesystem effects the daemon reads back.
func world(t *testing.T) (*State, *Repo, string) {
	t.Helper()
	dir := filepath.Join(t.TempDir(), "repo")
	if err := os.MkdirAll(filepath.Join(dir, ".git"), 0o755); err != nil {
		t.Fatalf("mkdir: %v", err)
	}
	s := NewState()
	repo := &Repo{
		Dir:           dir,
		CommonDir:     filepath.Join(Canon(dir), ".git"),
		DefaultBranch: "main",
		BranchHeads:   map[string]string{},
		Worktrees:     []*Worktree{{Dir: dir, Branch: "main"}},
	}
	repo.AddBranch("main", "")
	s.Repos = append(s.Repos, repo)
	c := s.AddCommit(repo, "main", "add README.md", nil, []string{"README.md"})
	repo.Worktrees[0].Head = c.SHA
	return s, repo, dir
}

func TestRunRefusesADirectoryThatIsNotAFakeRepository(t *testing.T) {
	// Arrange.
	s, _, _ := world(t)

	// Act.
	got := Run(s, "/", []string{"-C", t.TempDir(), "status", "--porcelain"})

	// Assert.
	if got.Exit != 128 || !strings.Contains(got.Stderr, "not a git repository") {
		t.Fatalf("git in an unregistered directory = %+v, want a loud 128", got)
	}
}

func TestRunRefusesACommandWithNoFixture(t *testing.T) {
	// Arrange.
	s, _, dir := world(t)

	// Act.
	got := Run(s, "/", []string{"-C", dir, "fetch", "origin"})

	// Assert.
	if got.Exit == 0 || !strings.Contains(got.Stderr, "no fixture") {
		t.Fatalf("`git fetch` = %+v, want a loud refusal", got)
	}
}

func TestRunRecordsEveryInvocation(t *testing.T) {
	// Arrange.
	s, _, dir := world(t)

	// Act.
	Run(s, "/tmp", []string{"-C", dir, "status", "--porcelain"})

	// Assert.
	if len(s.Calls) != 1 || s.Calls[0].Cwd != "/tmp" {
		t.Fatalf("recorded calls = %+v, want the one invocation with its cwd", s.Calls)
	}
}

func TestRunRecordsWhatAnAnsweredCommandPrinted(t *testing.T) {
	// Arrange.
	s, _, dir := world(t)

	// Act.
	Run(s, "/tmp", []string{"-C", dir, "rev-parse", "--show-toplevel"})

	// Assert.
	if len(s.Calls) != 1 {
		t.Fatalf("recorded calls = %+v, want one", s.Calls)
	}
	if got, want := s.Calls[0].Stdout, Canon(dir)+"\n"; got != want {
		t.Fatalf("recorded stdout = %q, want %q", got, want)
	}
	if s.Calls[0].Exit != 0 {
		t.Fatalf("recorded exit = %d, want 0", s.Calls[0].Exit)
	}
}

func TestRunRecordsWhyARefusedCommandFailed(t *testing.T) {
	// Arrange.
	s, _, _ := world(t)
	absent := t.TempDir()

	// Act.
	Run(s, "/tmp", []string{"-C", absent, "rev-parse", "--show-toplevel"})

	// Assert.
	if len(s.Calls) != 1 {
		t.Fatalf("recorded calls = %+v, want one", s.Calls)
	}
	if s.Calls[0].Exit != 128 || !strings.Contains(s.Calls[0].Stderr, "not a git repository") {
		t.Fatalf("recorded answer = exit %d stderr %q, want a loud 128",
			s.Calls[0].Exit, s.Calls[0].Stderr)
	}
}

func TestClipRecordedBoundsAnAnswerTooLongToKeep(t *testing.T) {
	// Arrange.
	long := strings.Repeat("x", recordedOutputLimit+1)

	// Act.
	got := clipRecorded(long)

	// Assert.
	if want := strings.Repeat("x", recordedOutputLimit) + "...(clipped)"; got != want {
		t.Fatalf("clipRecorded of an over-long answer = %q, want it clipped at %d with the marker",
			got, recordedOutputLimit)
	}
}

func TestClipRecordedKeepsAnAnswerThatFits(t *testing.T) {
	// Arrange.
	short := strings.Repeat("x", recordedOutputLimit)

	// Act.
	got := clipRecorded(short)

	// Assert.
	if got != short {
		t.Fatalf("clipRecorded of an answer at the limit = %d bytes, want it kept whole", len(got))
	}
}

func TestSymbolicRefIsUnsetWithoutAnOriginHead(t *testing.T) {
	// Arrange.
	s, _, dir := world(t)

	// Act.
	got := Run(s, "/", []string{"-C", dir, "symbolic-ref", "--short", "refs/remotes/origin/HEAD"})

	// Assert.
	if got.Exit == 0 {
		t.Fatalf("symbolic-ref = %+v, want the unset answer", got)
	}
}

func TestSymbolicRefAnswersTheScriptedOriginHead(t *testing.T) {
	// Arrange.
	s, repo, dir := world(t)
	repo.OriginHead = "origin/trunk"

	// Act.
	got := Run(s, "/", []string{"-C", dir, "symbolic-ref", "--short", "refs/remotes/origin/HEAD"})

	// Assert.
	if got.Exit != 0 || strings.TrimSpace(got.Stdout) != "origin/trunk" {
		t.Fatalf("symbolic-ref = %+v, want origin/trunk", got)
	}
}

func TestConfigAnswersTheDefaultBranch(t *testing.T) {
	// Arrange.
	s, _, dir := world(t)

	// Act.
	got := Run(s, "/", []string{"-C", dir, "config", "--get", "init.defaultBranch"})

	// Assert.
	if strings.TrimSpace(got.Stdout) != "main" {
		t.Fatalf("config --get init.defaultBranch = %+v, want main", got)
	}
}

func TestShowRefVerifiesAnExistingBranch(t *testing.T) {
	// Arrange.
	s, _, dir := world(t)

	// Act.
	got := Run(s, "/", []string{"-C", dir, "show-ref", "--verify", "--quiet", "refs/heads/main"})

	// Assert.
	if got.Exit != 0 {
		t.Fatalf("show-ref of an existing branch = %+v, want exit 0", got)
	}
}

func TestShowRefRefusesAnAbsentBranch(t *testing.T) {
	// Arrange.
	s, _, dir := world(t)

	// Act.
	got := Run(s, "/", []string{"-C", dir, "show-ref", "--verify", "--quiet", "refs/heads/absent"})

	// Assert.
	if got.Exit == 0 {
		t.Fatalf("show-ref of an absent branch = %+v, want a nonzero exit", got)
	}
}

func TestRevParseResolvesABranchToItsHead(t *testing.T) {
	// Arrange.
	s, repo, dir := world(t)

	// Act.
	got := Run(s, "/", []string{"-C", dir, "rev-parse", "--verify", "--end-of-options", "main^{commit}"})

	// Assert.
	if strings.TrimSpace(got.Stdout) != repo.BranchHeads["main"] {
		t.Fatalf("rev-parse main = %+v, want %q", got, repo.BranchHeads["main"])
	}
}

func TestRevParseRefusesAnUnknownRef(t *testing.T) {
	// Arrange.
	s, _, dir := world(t)

	// Act.
	got := Run(s, "/", []string{"-C", dir, "rev-parse", "--verify", "--end-of-options", "nope^{commit}"})

	// Assert.
	if got.Exit == 0 {
		t.Fatalf("rev-parse of an unknown ref = %+v, want a loud failure", got)
	}
}

func TestRevParseAnswersTheCommonDir(t *testing.T) {
	// Arrange.
	s, repo, dir := world(t)

	// Act.
	got := Run(s, "/", []string{"-C", dir, "rev-parse", "--git-common-dir"})

	// Assert.
	if strings.TrimSpace(got.Stdout) != repo.CommonDir {
		t.Fatalf("rev-parse --git-common-dir = %+v, want %q", got, repo.CommonDir)
	}
}

func TestRevParseAbbrevRefAnswersTheCheckedOutBranch(t *testing.T) {
	// Arrange.
	s, _, dir := world(t)

	// Act.
	got := Run(s, "/", []string{"-C", dir, "rev-parse", "--abbrev-ref", "HEAD"})

	// Assert.
	if strings.TrimSpace(got.Stdout) != "main" {
		t.Fatalf("rev-parse --abbrev-ref HEAD = %+v, want main", got)
	}
}

func TestWorktreeAddCreatesTheTreeAndTheBranch(t *testing.T) {
	// Arrange.
	s, repo, dir := world(t)
	target := filepath.Join(filepath.Dir(dir), "wt")

	// Act.
	got := Run(s, "/", []string{"-C", dir, "worktree", "add", "-b", "feature", target, "main"})

	// Assert.
	if got.Exit != 0 {
		t.Fatalf("worktree add = %+v, want a success", got)
	}
	if _, err := os.Stat(target); err != nil {
		t.Fatalf("worktree add left no directory at %s: %v", target, err)
	}
	if !repo.HasBranch("feature") || repo.Worktree(target) == nil {
		t.Fatalf("worktree add registered branches=%v worktrees=%v", repo.Branches, repo.Worktrees)
	}
}

func TestWorktreeAddRefusesAnExistingBranch(t *testing.T) {
	// Arrange.
	s, _, dir := world(t)

	// Act.
	got := Run(s, "/", []string{"-C", dir, "worktree", "add", "-b", "main", filepath.Join(filepath.Dir(dir), "wt"), "main"})

	// Assert.
	if got.Exit == 0 || !strings.Contains(got.Stderr, "already exists") {
		t.Fatalf("worktree add of an existing branch = %+v, want a refusal", got)
	}
}

func TestWorktreeRemoveTakesTheTreeOffDiskAndOutOfTheRegistry(t *testing.T) {
	// Arrange.
	s, repo, dir := world(t)
	target := filepath.Join(filepath.Dir(dir), "wt")
	Run(s, "/", []string{"-C", dir, "worktree", "add", "-b", "feature", target, "main"})

	// Act.
	got := Run(s, "/", []string{"-C", dir, "worktree", "remove", "--force", target})

	// Assert.
	if got.Exit != 0 {
		t.Fatalf("worktree remove = %+v, want a success", got)
	}
	if _, err := os.Stat(target); !os.IsNotExist(err) {
		t.Fatalf("worktree remove left %s behind (%v)", target, err)
	}
	if repo.Worktree(target) != nil {
		t.Fatalf("worktree remove left the registration %v", repo.Worktrees)
	}
}

func TestWorktreePruneDropsARegistrationWhoseTreeIsGone(t *testing.T) {
	// Arrange.
	s, repo, dir := world(t)
	target := filepath.Join(filepath.Dir(dir), "wt")
	Run(s, "/", []string{"-C", dir, "worktree", "add", "-b", "feature", target, "main"})
	if err := os.RemoveAll(target); err != nil {
		t.Fatalf("removing the tree: %v", err)
	}

	// Act.
	Run(s, "/", []string{"-C", dir, "worktree", "prune"})

	// Assert.
	if repo.Worktree(target) != nil {
		t.Fatalf("prune kept %v", repo.Worktrees)
	}
}

func TestBranchDeleteRemovesTheBranch(t *testing.T) {
	// Arrange.
	s, repo, dir := world(t)
	repo.AddBranch("feature", repo.BranchHeads["main"])

	// Act.
	got := Run(s, "/", []string{"-C", dir, "branch", "-D", "feature"})

	// Assert.
	if got.Exit != 0 || repo.HasBranch("feature") {
		t.Fatalf("branch -D = %+v with branches %v", got, repo.Branches)
	}
}

func TestMergeNoFFLandsAMergeCommitWithTwoParents(t *testing.T) {
	// Arrange.
	s, repo, dir := world(t)
	base := repo.BranchHeads["main"]
	repo.AddBranch("feature", base)
	s.AddCommit(repo, "feature", "feature work", []string{base}, []string{"f.txt"})

	// Act.
	got := Run(s, "/", []string{"-C", dir, "merge", "--no-ff", "--no-edit", "-m", "merge feature", "feature"})

	// Assert.
	if got.Exit != 0 {
		t.Fatalf("merge = %+v, want a landing", got)
	}
	head := s.Commits[repo.BranchHeads["main"]]
	if head == nil || len(head.Parents) != 2 {
		t.Fatalf("the merge commit = %+v, want two parents", head)
	}
}

func TestMergeNoFFLeavesAScriptedConflictStaged(t *testing.T) {
	// Arrange.
	s, repo, dir := world(t)
	repo.AddBranch("feature", repo.BranchHeads["main"])
	s.Conflicts = append(s.Conflicts, &Conflict{Dir: dir, Branch: "feature", Paths: []string{"a.txt"}})

	// Act.
	got := Run(s, "/", []string{"-C", dir, "merge", "--no-ff", "--no-edit", "-m", "merge feature", "feature"})

	// Assert.
	if got.Exit == 0 {
		t.Fatalf("a scripted conflict merged cleanly: %+v", got)
	}
	if paths := repo.Worktree(dir).Conflicted; len(paths) != 1 || paths[0] != "a.txt" {
		t.Fatalf("the conflicted index = %v, want a.txt", paths)
	}
}

func TestDiffDiffFilterUListsTheConflictedPaths(t *testing.T) {
	// Arrange.
	s, repo, dir := world(t)
	repo.Worktree(dir).Conflicted = []string{"a.txt", "b.txt"}

	// Act.
	got := Run(s, "/", []string{"-C", dir, "diff", "--name-only", "--diff-filter=U", "-z"})

	// Assert.
	if got.Stdout != "a.txt\x00b.txt\x00" {
		t.Fatalf("conflicted files = %q, want the NUL-separated pair", got.Stdout)
	}
}

func TestMergeAbortClearsTheConflictedIndex(t *testing.T) {
	// Arrange.
	s, repo, dir := world(t)
	repo.Worktree(dir).Conflicted = []string{"a.txt"}

	// Act.
	Run(s, "/", []string{"-C", dir, "merge", "--abort"})

	// Assert.
	if len(repo.Worktree(dir).Conflicted) != 0 {
		t.Fatalf("merge --abort left %v", repo.Worktree(dir).Conflicted)
	}
}

func TestCommitRecordsTheStagedResolution(t *testing.T) {
	// Arrange.
	s, repo, dir := world(t)
	repo.Worktree(dir).Conflicted = []string{"a.txt"}

	// Act.
	got := Run(s, "/", []string{"-C", dir, "commit", "--no-edit", "-m", "resolve"})

	// Assert.
	if got.Exit != 0 {
		t.Fatalf("commit = %+v, want a success", got)
	}
	if head := s.Commits[repo.BranchHeads["main"]]; head == nil || head.Subject != "resolve" {
		t.Fatalf("the new head = %+v, want the resolution commit", head)
	}
	if len(repo.Worktree(dir).Conflicted) != 0 {
		t.Fatalf("commit left the index conflicted: %v", repo.Worktree(dir).Conflicted)
	}
}

func TestRevertRecordsARevertCommit(t *testing.T) {
	// Arrange.
	s, repo, dir := world(t)
	target := repo.BranchHeads["main"]

	// Act.
	got := Run(s, "/", []string{"-C", dir, "revert", "-m", "1", "--no-edit", target})

	// Assert.
	if got.Exit != 0 {
		t.Fatalf("revert = %+v, want a success", got)
	}
	if head := s.Commits[repo.BranchHeads["main"]]; head == nil || !strings.HasPrefix(head.Subject, "Revert") {
		t.Fatalf("the new head = %+v, want a revert commit", head)
	}
}

func TestRevListWalksTheMergesSecondParentOldestFirst(t *testing.T) {
	// Arrange: two commits on feature, then a merge.
	s, repo, dir := world(t)
	base := repo.BranchHeads["main"]
	repo.AddBranch("feature", base)
	first := s.AddCommit(repo, "feature", "first", []string{base}, []string{"a.txt"})
	second := s.AddCommit(repo, "feature", "second", []string{first.SHA}, []string{"b.txt"})
	Run(s, "/", []string{"-C", dir, "merge", "--no-ff", "--no-edit", "-m", "merge", "feature"})
	mergeSHA := repo.BranchHeads["main"]

	// Act.
	got := Run(s, "/", []string{"-C", dir, "rev-list", "--reverse",
		"--format=%H" + FieldSep + "%s" + FieldSep + "%an" + FieldSep + "%aI",
		"--no-commit-header", mergeSHA + "^1.." + mergeSHA + "^2"})

	// Assert.
	lines := strings.Split(strings.TrimRight(got.Stdout, "\n"), "\n")
	if len(lines) != 2 {
		t.Fatalf("rev-list = %q, want the two feature commits", got.Stdout)
	}
	if !strings.HasPrefix(lines[0], first.SHA) || !strings.HasPrefix(lines[1], second.SHA) {
		t.Fatalf("rev-list order = %q, want oldest first", lines)
	}
}

func TestDiffOverARangeListsTheChangedPaths(t *testing.T) {
	// Arrange.
	s, repo, dir := world(t)
	base := repo.BranchHeads["main"]
	repo.AddBranch("feature", base)
	s.AddCommit(repo, "feature", "first", []string{base}, []string{"a.txt"})
	Run(s, "/", []string{"-C", dir, "merge", "--no-ff", "--no-edit", "-m", "merge", "feature"})
	mergeSHA := repo.BranchHeads["main"]

	// Act.
	got := Run(s, "/", []string{"-C", dir, "diff", "--name-only", "-z", mergeSHA + "^1.." + mergeSHA + "^2"})

	// Assert.
	if got.Stdout != "a.txt\x00" {
		t.Fatalf("changed paths = %q, want a.txt", got.Stdout)
	}
}

func TestStatusIsSilentForACleanTree(t *testing.T) {
	// Arrange.
	s, _, dir := world(t)

	// Act.
	got := Run(s, "/", []string{"-C", dir, "status", "--porcelain"})

	// Assert.
	if strings.TrimSpace(got.Stdout) != "" {
		t.Fatalf("status of a clean tree = %q, want nothing", got.Stdout)
	}
}

func TestStatusReportsAScriptedDirtyTree(t *testing.T) {
	// Arrange.
	s, repo, dir := world(t)
	repo.Worktree(dir).Dirty = true

	// Act.
	got := Run(s, "/", []string{"-C", dir, "status", "--porcelain"})

	// Assert.
	if strings.TrimSpace(got.Stdout) == "" {
		t.Fatalf("status of a dirty tree = %q, want content", got.Stdout)
	}
}

func TestShowRendersTheCommitTemplate(t *testing.T) {
	// Arrange.
	s, repo, dir := world(t)
	sha := repo.BranchHeads["main"]

	// Act.
	got := Run(s, "/", []string{"-C", dir, "show", "--no-patch",
		"--format=%H" + FieldSep + "%s" + FieldSep + "%an" + FieldSep + "%aI", "HEAD"})

	// Assert.
	fields := strings.Split(strings.TrimRight(got.Stdout, "\n"), FieldSep)
	if len(fields) != 4 || fields[0] != sha {
		t.Fatalf("show = %q, want the four fields of %s", got.Stdout, sha)
	}
}

func TestAScriptedFailurePreemptsTheCommand(t *testing.T) {
	// Arrange.
	s, _, dir := world(t)
	s.Failures = append(s.Failures, &Failure{Match: []string{"status"}, Stderr: "scripted\n", Exit: 3})

	// Act.
	got := Run(s, "/", []string{"-C", dir, "status", "--porcelain"})

	// Assert.
	if got.Exit != 3 || got.Stderr != "scripted\n" {
		t.Fatalf("a scripted failure = %+v, want exit 3", got)
	}
}

func TestAScriptedFailureIsConsumedOnce(t *testing.T) {
	// Arrange.
	s, _, dir := world(t)
	s.Failures = append(s.Failures, &Failure{Match: []string{"status"}, Stderr: "scripted\n", Exit: 3})
	Run(s, "/", []string{"-C", dir, "status", "--porcelain"})

	// Act.
	got := Run(s, "/", []string{"-C", dir, "status", "--porcelain"})

	// Assert.
	if got.Exit != 0 {
		t.Fatalf("the second status = %+v, want the scripted failure spent", got)
	}
}

func TestRunAnswersGitVersionOutsideAnyRepository(t *testing.T) {
	tests := []struct {
		name string
		args []string
	}{
		{name: "subcommand", args: []string{"version"}},
		{name: "flag", args: []string{"--version"}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			s := NewState()

			// Act.
			got := Run(s, t.TempDir(), tt.args)

			// Assert.
			if got.Exit != 0 {
				t.Fatalf("exit %d, stderr %q; want 0", got.Exit, got.Stderr)
			}
			if got.Stdout != "git version 2.39.5\n" {
				t.Fatalf("stdout %q; want real git's shape", got.Stdout)
			}
		})
	}
}

// subdir registers a tracked subdirectory of the fixture worktree and answers
// its path, so the probes can be asked from below the top of the tree.
func subdir(t *testing.T, dir, name string) string {
	t.Helper()
	sub := filepath.Join(dir, name)
	if err := os.MkdirAll(sub, 0o755); err != nil {
		t.Fatalf("mkdir: %v", err)
	}
	return sub
}

func TestRevParseAnswersOneShapeFlag(t *testing.T) {
	// Arrange.
	s, repo, dir := world(t)
	tests := []struct {
		name string
		flag string
		want string
	}{
		{name: "toplevel", flag: "--show-toplevel", want: Canon(dir) + "\n"},
		{name: "cdup at the top", flag: "--show-cdup", want: "\n"},
		{name: "git dir at the top", flag: "--git-dir", want: ".git\n"},
		{name: "absolute git dir", flag: "--absolute-git-dir", want: repo.CommonDir + "\n"},
		{name: "common dir", flag: "--git-common-dir", want: repo.CommonDir + "\n"},
		{name: "inside work tree", flag: "--is-inside-work-tree", want: "true\n"},
		{name: "bare", flag: "--is-bare-repository", want: "false\n"},
		{name: "inside git dir", flag: "--is-inside-git-dir", want: "false\n"},
		{name: "prefix at the top", flag: "--show-prefix", want: "\n"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act.
			got := Run(s, dir, []string{"rev-parse", tt.flag})

			// Assert.
			if got.Exit != 0 || got.Stdout != tt.want {
				t.Fatalf("`rev-parse %s` = %+v, want stdout %q", tt.flag, got, tt.want)
			}
		})
	}
}

func TestRevParseAnswersEveryFlagOfOneCallInOrder(t *testing.T) {
	// Arrange.
	s, _, dir := world(t)

	// Act.
	got := Run(s, dir, []string{"rev-parse", "--is-inside-work-tree", "--is-bare-repository", "--show-toplevel"})

	// Assert.
	want := "true\nfalse\n" + Canon(dir) + "\n"
	if got.Stdout != want {
		t.Fatalf("the combined probe = %q, want one line per flag in order: %q", got.Stdout, want)
	}
}

func TestRevParseAnswersCdupFromASubdirectory(t *testing.T) {
	// Arrange.
	s, _, dir := world(t)
	sub := subdir(t, dir, filepath.Join("a", "b"))

	// Act.
	got := Run(s, sub, []string{"rev-parse", "--show-cdup"})

	// Assert.
	if got.Stdout != "../../\n" {
		t.Fatalf("`--show-cdup` two levels down = %q, want %q", got.Stdout, "../../\n")
	}
}

func TestRevParseAnswersPrefixFromASubdirectory(t *testing.T) {
	// Arrange.
	s, _, dir := world(t)
	sub := subdir(t, dir, filepath.Join("a", "b"))

	// Act.
	got := Run(s, sub, []string{"rev-parse", "--show-prefix"})

	// Assert.
	if got.Stdout != "a/b/\n" {
		t.Fatalf("`--show-prefix` = %q, want %q", got.Stdout, "a/b/\n")
	}
}

func TestRevParseAnswersAnAbsoluteGitDirBelowTheTop(t *testing.T) {
	// Arrange.
	s, repo, dir := world(t)
	sub := subdir(t, dir, "a")

	// Act.
	got := Run(s, sub, []string{"rev-parse", "--git-dir"})

	// Assert.
	if got.Stdout != repo.CommonDir+"\n" {
		t.Fatalf("`--git-dir` below the top = %q, want the absolute %q", got.Stdout, repo.CommonDir)
	}
}

func TestRevParseAnswersALinkedWorktreesOwnGitDir(t *testing.T) {
	// Arrange.
	s, repo, dir := world(t)
	linked := filepath.Join(filepath.Dir(dir), "wt")
	if got := Run(s, dir, []string{"worktree", "add", "-b", "feature", linked, "main"}); got.Exit != 0 {
		t.Fatalf("worktree add = %+v", got)
	}

	// Act.
	got := Run(s, linked, []string{"rev-parse", "--git-dir"})

	// Assert.
	want := filepath.Join(repo.CommonDir, "worktrees", "wt") + "\n"
	if got.Stdout != want {
		t.Fatalf("a linked worktree's `--git-dir` = %q, want %q", got.Stdout, want)
	}
}

func TestRevParseStillResolvesARevision(t *testing.T) {
	// Arrange.
	s, repo, dir := world(t)

	// Act.
	got := Run(s, dir, []string{"rev-parse", "--verify", "HEAD"})

	// Assert.
	if got.Stdout != repo.BranchHeads["main"]+"\n" {
		t.Fatalf("`rev-parse --verify HEAD` = %+v, want the head sha", got)
	}
}

func TestSymbolicRefAnswersTheShortBranch(t *testing.T) {
	// Arrange.
	s, _, dir := world(t)

	// Act.
	got := Run(s, dir, []string{"symbolic-ref", "--short", "HEAD"})

	// Assert.
	if got.Exit != 0 || got.Stdout != "main\n" {
		t.Fatalf("`symbolic-ref --short HEAD` = %+v, want main", got)
	}
}

func TestSymbolicRefAnswersTheFullBranchRef(t *testing.T) {
	// Arrange.
	s, _, dir := world(t)

	// Act.
	got := Run(s, dir, []string{"symbolic-ref", "HEAD"})

	// Assert.
	if got.Stdout != "refs/heads/main\n" {
		t.Fatalf("`symbolic-ref HEAD` = %+v, want refs/heads/main", got)
	}
}

func TestSymbolicRefRefusesADetachedHead(t *testing.T) {
	// Arrange.
	s, repo, dir := world(t)
	repo.Worktrees[0].Branch = ""

	// Act.
	got := Run(s, dir, []string{"symbolic-ref", "--short", "HEAD"})

	// Assert.
	if got.Exit != 128 || got.Stderr != "fatal: ref HEAD is not a symbolic ref\n" {
		t.Fatalf("a detached HEAD = %+v, want real git's 128", got)
	}
}

func TestSymbolicRefStillAnswersTheOriginHead(t *testing.T) {
	// Arrange.
	s, repo, dir := world(t)
	repo.OriginHead = "refs/remotes/origin/main"

	// Act.
	got := Run(s, dir, []string{"symbolic-ref", "refs/remotes/origin/HEAD"})

	// Assert.
	if got.Stdout != "refs/remotes/origin/main\n" {
		t.Fatalf("the origin head probe = %+v, want it unchanged", got)
	}
}

func TestConfigAnswersCoreBare(t *testing.T) {
	// Arrange.
	s, _, dir := world(t)

	// Act.
	got := Run(s, dir, []string{"config", "--get", "core.bare"})

	// Assert.
	if got.Exit != 0 || got.Stdout != "false\n" {
		t.Fatalf("`config --get core.bare` = %+v, want false", got)
	}
}

func TestConfigReportsAnUnsetKeyAsRealGitDoes(t *testing.T) {
	// Arrange.
	s, _, dir := world(t)

	// Act.
	got := Run(s, dir, []string{"config", "--get", "magit.nosuchkey"})

	// Assert.
	if got.Exit != 1 || got.Stdout != "" {
		t.Fatalf("an unset key = %+v, want an empty exit 1", got)
	}
}

func TestDescribeReportsNoNamesFound(t *testing.T) {
	// Arrange.
	s, _, dir := world(t)

	// Act.
	got := Run(s, dir, []string{"describe", "--tags", "--exact-match", "HEAD"})

	// Assert.
	if got.Exit != 128 || got.Stderr != "fatal: No names found, cannot describe anything.\n" {
		t.Fatalf("`describe` with no tags = %+v, want real git's fatal", got)
	}
}

func TestLsFilesListsTheTrackedPathsNulTerminated(t *testing.T) {
	// Arrange.
	s, repo, dir := world(t)
	repo.Worktrees[0].Files = []string{"README.md", "src/main.go"}

	// Act.
	got := Run(s, dir, []string{"ls-files", "-zco", "--exclude-standard"})

	// Assert.
	if got.Stdout != "README.md\x00src/main.go\x00" {
		t.Fatalf("projectile's listing = %q, want NUL-terminated tracked paths", got.Stdout)
	}
}

func TestLsFilesTerminatesWithNewlinesWithoutTheZFlag(t *testing.T) {
	// Arrange.
	s, repo, dir := world(t)
	repo.Worktrees[0].Files = []string{"README.md", "src/main.go"}

	// Act.
	got := Run(s, dir, []string{"ls-files"})

	// Assert.
	if got.Stdout != "README.md\nsrc/main.go\n" {
		t.Fatalf("`ls-files` = %q, want newline-terminated paths", got.Stdout)
	}
}

func TestLsFilesAnswersRelativeToTheDirectoryItRanIn(t *testing.T) {
	// Arrange.
	s, repo, dir := world(t)
	repo.Worktrees[0].Files = []string{"README.md", "src/main.go"}
	sub := subdir(t, dir, "src")

	// Act.
	got := Run(s, sub, []string{"ls-files", "-z"})

	// Assert.
	if got.Stdout != "main.go\x00" {
		t.Fatalf("`ls-files` in a subdirectory = %q, want the path relative to it", got.Stdout)
	}
}

func TestLsFilesAnswersNothingForAWorktreeWithNoFixedPaths(t *testing.T) {
	// Arrange.
	s, _, dir := world(t)

	// Act.
	got := Run(s, dir, []string{"ls-files", "-z"})

	// Assert.
	if got.Exit != 0 || got.Stdout != "" {
		t.Fatalf("an unpopulated worktree = %+v, want an empty success", got)
	}
}

func TestStatusTerminatesEntriesWithNulUnderTheZFlag(t *testing.T) {
	// Arrange.
	s, repo, dir := world(t)
	repo.Worktrees[0].Dirty = true

	// Act.
	got := Run(s, dir, []string{"status", "--porcelain", "-z"})

	// Assert.
	if got.Stdout != " M dirty.txt\x00" {
		t.Fatalf("`status --porcelain -z` = %q, want a NUL terminator", got.Stdout)
	}
}

func TestStatusPrependsTheBranchHeader(t *testing.T) {
	// Arrange.
	s, _, dir := world(t)

	// Act.
	got := Run(s, dir, []string{"status", "--porcelain", "--branch"})

	// Assert.
	if got.Stdout != "## main\n" {
		t.Fatalf("`status --porcelain --branch` = %q, want the branch header", got.Stdout)
	}
}

func TestStatusHeadsADetachedTreeAsRealGitDoes(t *testing.T) {
	// Arrange.
	s, repo, dir := world(t)
	repo.Worktrees[0].Branch = ""

	// Act.
	got := Run(s, dir, []string{"status", "--porcelain", "--branch"})

	// Assert.
	if got.Stdout != "## HEAD (no branch)\n" {
		t.Fatalf("a detached tree's header = %q, want real git's shape", got.Stdout)
	}
}

func TestStatusListsUntrackedFilesUnderUall(t *testing.T) {
	// Arrange.
	s, repo, dir := world(t)
	repo.Worktrees[0].Dirty = true

	// Act.
	got := Run(s, dir, []string{"status", "--porcelain", "-uall"})

	// Assert.
	if got.Stdout != " M dirty.txt\n" {
		t.Fatalf("`status --porcelain -uall` = %q, want the scripted dirty entry", got.Stdout)
	}
}

func TestRunSkipsGitGlobalOptionsBeforeTheSubcommand(t *testing.T) {
	tests := []struct {
		name string
		lead []string
	}{
		{name: "magit's prefix", lead: []string{"--no-pager", "--literal-pathspecs", "-c", "core.preloadIndex=true", "-c", "color.ui=false"}},
		{name: "-C after other options", lead: []string{"--no-pager", "-C"}},
		{name: "attached -c", lead: []string{"-ccolor.ui=false"}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			s, _, dir := world(t)
			args := append([]string(nil), tt.lead...)
			if args[len(args)-1] == "-C" {
				args = append(args, dir, "rev-parse", "--show-toplevel")
				dir = t.TempDir()
			} else {
				args = append(args, "rev-parse", "--show-toplevel")
			}

			// Act.
			got := Run(s, dir, args)

			// Assert.
			if got.Exit != 0 {
				t.Fatalf("exit %d, stderr %q; want the subcommand dispatched", got.Exit, got.Stderr)
			}
			if !strings.HasSuffix(got.Stdout, "/repo\n") {
				t.Fatalf("stdout %q; want the toplevel", got.Stdout)
			}
		})
	}
}

func TestUpdateIndexRefreshIsSilentOnACleanTree(t *testing.T) {
	// Arrange.
	s, _, dir := world(t)

	// Act.
	got := Run(s, dir, []string{"update-index", "--refresh"})

	// Assert.
	if got.Exit != 0 || got.Stdout != "" {
		t.Fatalf("`update-index --refresh` = %+v, want real git's silent success", got)
	}
}

func TestUpdateIndexRefreshNamesAnUnsettledPathOnADirtyTree(t *testing.T) {
	// Arrange.
	s, repo, dir := world(t)
	repo.Worktrees[0].Dirty = true

	// Act.
	got := Run(s, dir, []string{"update-index", "--refresh"})

	// Assert.
	if got.Exit != 1 || got.Stdout != "dirty.txt: needs update\n" {
		t.Fatalf("`update-index --refresh` = %+v, want the unsettled path and exit 1", got)
	}
}

func TestConfigListSeparatesRecordsWithNulUnderTheZFlag(t *testing.T) {
	// Arrange.
	s, _, dir := world(t)

	// Act.
	got := Run(s, dir, []string{"config", "--list", "-z"})

	// Assert.
	if !strings.Contains(got.Stdout, "core.bare\nfalse\x00") {
		t.Fatalf("`config --list -z` = %q, want key-newline-value records terminated by NUL", got.Stdout)
	}
}

func TestConfigListSeparatesRecordsWithEqualsWithoutTheZFlag(t *testing.T) {
	// Arrange.
	s, _, dir := world(t)

	// Act.
	got := Run(s, dir, []string{"config", "--list"})

	// Assert.
	if !strings.Contains(got.Stdout, "core.bare=false\n") {
		t.Fatalf("`config --list` = %q, want key=value lines", got.Stdout)
	}
}

func TestConfigListCarriesTheDefaultBranchLowercased(t *testing.T) {
	// Arrange.
	s, _, dir := world(t)

	// Act.
	got := Run(s, dir, []string{"config", "--list"})

	// Assert.
	if !strings.Contains(got.Stdout, "init.defaultbranch=main\n") {
		t.Fatalf("`config --list` = %q, want the default branch under real git's lowercased key", got.Stdout)
	}
}

func TestLogNoWalkPrintsOnlyTheNamedCommit(t *testing.T) {
	// Arrange.
	s, repo, dir := world(t)
	head := s.AddCommit(repo, "main", "add b.txt", []string{repo.BranchHeads["main"]}, []string{"b.txt"})

	// Act.
	got := Run(s, dir, []string{"log", "--no-walk", "--format=%h %s", "HEAD^{commit}", "--"})

	// Assert.
	if got.Stdout != s.Abbrev(head.SHA)+" add b.txt\n" {
		t.Fatalf("`log --no-walk` = %q, want only the named commit", got.Stdout)
	}
}

func TestLogWalksTheHistoryNewestFirst(t *testing.T) {
	// Arrange.
	s, repo, dir := world(t)
	s.AddCommit(repo, "main", "add b.txt", []string{repo.BranchHeads["main"]}, []string{"b.txt"})

	// Act.
	got := Run(s, dir, []string{"log", "--format=%s", "HEAD"})

	// Assert.
	if got.Stdout != "add b.txt\nadd README.md\n" {
		t.Fatalf("`log` = %q, want the history newest first", got.Stdout)
	}
}

func TestLogHonorsTheMaxCountCap(t *testing.T) {
	// Arrange.
	s, repo, dir := world(t)
	s.AddCommit(repo, "main", "add b.txt", []string{repo.BranchHeads["main"]}, []string{"b.txt"})

	// Act.
	got := Run(s, dir, []string{"log", "--format=%s", "-n1", "HEAD"})

	// Assert.
	if got.Stdout != "add b.txt\n" {
		t.Fatalf("`log -n1` = %q, want one commit", got.Stdout)
	}
}

func TestLogDecoratesTheCheckedOutBranchTip(t *testing.T) {
	// Arrange.
	s, _, dir := world(t)

	// Act.
	got := Run(s, dir, []string{"log", "--format=%D", "--decorate=full", "HEAD"})

	// Assert.
	if got.Stdout != "HEAD -> refs/heads/main\n" {
		t.Fatalf("`log --format=%%D` = %q, want real git's decoration for the checked-out tip", got.Stdout)
	}
}

func TestLogExpandsGitsLiteralByteEscape(t *testing.T) {
	// Arrange.
	s, _, dir := world(t)

	// Act.
	got := Run(s, dir, []string{"log", "--format=%x0c%s", "HEAD"})

	// Assert.
	if got.Stdout != "\x0cadd README.md\n" {
		t.Fatalf("`log --format=%%x0c` = %q, want the literal byte", got.Stdout)
	}
}

func TestLogRefusesAnUnknownRevision(t *testing.T) {
	// Arrange.
	s, _, dir := world(t)

	// Act.
	got := Run(s, dir, []string{"log", "--format=%s", "nope"})

	// Assert.
	if got.Exit != 128 || !strings.Contains(got.Stderr, "unknown revision") {
		t.Fatalf("`log nope` = %+v, want real git's unknown-revision refusal", got)
	}
}

func TestRevParseRefusesAnUpstreamTheFixtureHasNoRemoteFor(t *testing.T) {
	// Arrange.
	s, _, dir := world(t)

	// Act.
	got := Run(s, dir, []string{"rev-parse", "--verify", "--abbrev-ref", "main@{upstream}"})

	// Assert.
	if got.Exit != 128 || !strings.Contains(got.Stderr, "no upstream configured for branch 'main'") {
		t.Fatalf("`rev-parse main@{upstream}` = %+v, want real git's refusal", got)
	}
}

func TestRevParseAbbreviatesUnderTheShortFlag(t *testing.T) {
	// Arrange.
	s, repo, dir := world(t)

	// Act.
	got := Run(s, dir, []string{"rev-parse", "--short", "HEAD"})

	// Assert.
	if got.Stdout != s.Abbrev(repo.BranchHeads["main"])+"\n" {
		t.Fatalf("`rev-parse --short HEAD` = %q, want the abbreviated head", got.Stdout)
	}
}

func TestRevParseWalksAFirstParentAncestryStep(t *testing.T) {
	// Arrange.
	s, repo, dir := world(t)
	root := repo.BranchHeads["main"]
	s.AddCommit(repo, "main", "add b.txt", []string{root}, []string{"b.txt"})

	// Act.
	got := Run(s, dir, []string{"rev-parse", "HEAD~1"})

	// Assert.
	if got.Stdout != root+"\n" {
		t.Fatalf("`rev-parse HEAD~1` = %q, want the parent", got.Stdout)
	}
}

func TestRevParseRefusesAnAncestryStepPastTheRootCommit(t *testing.T) {
	// Arrange.
	s, _, dir := world(t)

	// Act.
	got := Run(s, dir, []string{"rev-parse", "--verify", "HEAD~10"})

	// Assert.
	if got.Exit != 128 || got.Stderr != "fatal: Needed a single revision\n" {
		t.Fatalf("`rev-parse HEAD~10` = %+v, want real git's refusal", got)
	}
}

func TestMergeBaseAcceptsAnAncestor(t *testing.T) {
	// Arrange.
	s, repo, dir := world(t)
	s.AddCommit(repo, "main", "add b.txt", []string{repo.BranchHeads["main"]}, []string{"b.txt"})

	// Act.
	got := Run(s, dir, []string{"merge-base", "--is-ancestor", "HEAD~1", "main"})

	// Assert.
	if got.Exit != 0 || got.Stdout != "" {
		t.Fatalf("`merge-base --is-ancestor` = %+v, want real git's silent success", got)
	}
}

func TestMergeBaseRejectsANonAncestor(t *testing.T) {
	// Arrange.
	s, repo, dir := world(t)
	side := s.AddCommit(repo, "", "side", nil, nil)
	repo.AddBranch("side", side.SHA)

	// Act.
	got := Run(s, dir, []string{"merge-base", "--is-ancestor", "side", "main"})

	// Assert.
	if got.Exit != 1 {
		t.Fatalf("`merge-base --is-ancestor` = %+v, want exit 1", got)
	}
}

func TestDescribeAlwaysFallsBackToTheAbbreviatedHead(t *testing.T) {
	// Arrange.
	s, repo, dir := world(t)

	// Act.
	got := Run(s, dir, []string{"describe", "--tags", "--dirty", "--always"})

	// Assert.
	if got.Exit != 0 || got.Stdout != s.Abbrev(repo.BranchHeads["main"])+"\n" {
		t.Fatalf("`describe --always` = %+v, want the abbreviated head", got)
	}
}

func TestDescribeContainsNamesTheCommitItCannotDescribe(t *testing.T) {
	// Arrange.
	s, repo, dir := world(t)

	// Act.
	got := Run(s, dir, []string{"describe", "--contains", "HEAD"})

	// Assert.
	want := "fatal: cannot describe '" + repo.BranchHeads["main"] + "'\n"
	if got.Exit != 128 || got.Stderr != want {
		t.Fatalf("`describe --contains HEAD` = %+v, want %q", got, want)
	}
}

func TestDiffOfACleanWorktreeIsEmpty(t *testing.T) {
	// Arrange.
	s, _, dir := world(t)

	// Act.
	got := Run(s, dir, []string{"diff", "--ita-visible-in-index", "--no-ext-diff", "--no-prefix", "--"})

	// Assert.
	if got.Exit != 0 || got.Stdout != "" {
		t.Fatalf("worktree `diff` = %+v, want no output for a clean tree", got)
	}
}

func TestDiffOfADirtyWorktreeIsAUnifiedDiff(t *testing.T) {
	// Arrange.
	s, repo, dir := world(t)
	repo.Worktrees[0].Dirty = true

	// Act.
	got := Run(s, dir, []string{"diff", "--ita-visible-in-index", "--no-ext-diff", "--no-prefix", "--"})

	// Assert.
	if !strings.HasPrefix(got.Stdout, "diff --git dirty.txt dirty.txt\n") {
		t.Fatalf("worktree `diff` = %q, want real git's unified diff header", got.Stdout)
	}
}

func TestDiffCachedIsEmptyForAScriptedDirtyTree(t *testing.T) {
	// Arrange.
	s, repo, dir := world(t)
	repo.Worktrees[0].Dirty = true

	// Act.
	got := Run(s, dir, []string{"diff", "--cached", "--no-ext-diff", "--no-prefix", "--"})

	// Assert.
	if got.Stdout != "" {
		t.Fatalf("`diff --cached` = %q, want nothing staged", got.Stdout)
	}
}

func TestAbbrevGrowsPastAnAmbiguousPrefix(t *testing.T) {
	// Arrange.
	s := NewState()
	s.Commits["aaaaaaaa1"] = &Commit{SHA: "aaaaaaaa1"}
	s.Commits["aaaaaaaa2"] = &Commit{SHA: "aaaaaaaa2"}

	// Act.
	got := s.Abbrev("aaaaaaaa1")

	// Assert.
	if got != "aaaaaaaa1" {
		t.Fatalf("Abbrev = %q, want a prefix grown until it is unique", got)
	}
}

// --- the landed-worktree reaper's commands -----------------------------------

// addTree cuts a worktree off main in the world and answers its directory.
func addTree(t *testing.T, s *State, dir, name string) string {
	t.Helper()
	target := filepath.Join(filepath.Dir(dir), name)
	if got := Run(s, "/", []string{"-C", dir, "worktree", "add", "-b", name, target, "main"}); got.Exit != 0 {
		t.Fatalf("worktree add = %+v", got)
	}
	return target
}

func TestWorktreeRemoveRefusesADirtyTreeUnlessForced(t *testing.T) {
	// Arrange.
	s, repo, dir := world(t)
	target := addTree(t, s, dir, "wt")
	repo.Worktree(target).Dirty = true

	// Act.
	got := Run(s, "/", []string{"-C", dir, "worktree", "remove", target})

	// Assert.
	if got.Exit != 128 || !strings.Contains(got.Stderr, "modified or untracked") || repo.Worktree(target) == nil {
		t.Fatalf("worktree remove of a dirty tree = %+v, want git's refusal and the tree kept", got)
	}
}

func TestWorktreeListWithDashZEndsEveryAttributeWithNUL(t *testing.T) {
	// Arrange.
	s, repo, dir := world(t)

	// Act.
	got := Run(s, "/", []string{"-C", dir, "worktree", "list", "--porcelain", "-z"})

	// Assert.
	want := "worktree " + dir + "\x00HEAD " + repo.BranchHeads["main"] + "\x00branch refs/heads/main\x00\x00"
	if got.Stdout != want {
		t.Fatalf("worktree list -z = %q, want %q", got.Stdout, want)
	}
}

func TestWorktreeListStatesADetachedHead(t *testing.T) {
	// Arrange.
	s, repo, dir := world(t)
	target := addTree(t, s, dir, "wt")
	repo.Worktree(target).Branch = ""

	// Act.
	got := Run(s, "/", []string{"-C", dir, "worktree", "list", "--porcelain"})

	// Assert.
	if !strings.Contains(got.Stdout, "worktree "+target+"\nHEAD "+repo.BranchHeads["main"]+"\ndetached\n") {
		t.Fatalf("worktree list = %q, want the detached tree stated", got.Stdout)
	}
}

func TestWorktreeListStatesALock(t *testing.T) {
	// Arrange.
	s, repo, dir := world(t)
	target := addTree(t, s, dir, "wt")
	repo.Worktree(target).Locked = true

	// Act.
	got := Run(s, "/", []string{"-C", dir, "worktree", "list", "--porcelain"})

	// Assert.
	if !strings.Contains(got.Stdout, "branch refs/heads/wt\nlocked\n") {
		t.Fatalf("worktree list = %q, want the lock stated", got.Stdout)
	}
}

func TestRevParseAnswersACommitsTree(t *testing.T) {
	// Arrange.
	s, repo, dir := world(t)
	head := repo.BranchHeads["main"]

	// Act.
	got := Run(s, "/", []string{"-C", dir, "rev-parse", "--verify", "--end-of-options", head + "^{tree}"})

	// Assert.
	if got.Exit != 0 || got.Stdout != s.Commits[head].TreeOf()+"\n" {
		t.Fatalf("rev-parse ^{tree} = %+v, want the commit's tree", got)
	}
}

func TestACommitsTreeIsItsOwnUnlessScripted(t *testing.T) {
	// Arrange.
	s, repo, _ := world(t)
	first := s.Commits[repo.BranchHeads["main"]]
	second := s.AddCommit(repo, "main", "more", []string{first.SHA}, nil)

	// Act, Assert.
	if first.TreeOf() == second.TreeOf() || first.TreeOf() == first.SHA {
		t.Fatalf("trees %q and %q, want each commit's tree distinct and not its sha", first.TreeOf(), second.TreeOf())
	}
	second.Tree = first.TreeOf()
	if second.TreeOf() != first.TreeOf() {
		t.Fatalf("a scripted tree = %q, want %q", second.TreeOf(), first.TreeOf())
	}
}

func TestMergeTreeOfAnAncestorIsTheBasesOwnTree(t *testing.T) {
	// Arrange: the branch's commit is already on main.
	s, repo, dir := world(t)
	branchHead := repo.BranchHeads["main"]
	next := s.AddCommit(repo, "main", "more", []string{branchHead}, nil)

	// Act.
	got := Run(s, "/", []string{"-C", dir, "merge-tree", "--write-tree", "--no-messages", next.SHA, branchHead})

	// Assert.
	if got.Exit != 0 || got.Stdout != next.TreeOf()+"\n" {
		t.Fatalf("merge-tree of a landed commit = %+v, want main's own tree", got)
	}
}

func TestMergeTreeOfADescendantIsItsTree(t *testing.T) {
	// Arrange: the branch is ahead of main.
	s, repo, dir := world(t)
	base := repo.BranchHeads["main"]
	ahead := s.AddCommit(nil, "", "ahead", []string{base}, nil)

	// Act.
	got := Run(s, "/", []string{"-C", dir, "merge-tree", "--write-tree", base, ahead.SHA})

	// Assert.
	if got.Exit != 0 || got.Stdout != ahead.TreeOf()+"\n" {
		t.Fatalf("merge-tree of an unlanded commit = %+v, want the branch's tree", got)
	}
}

func TestMergeTreeOfDivergedLinesIsNeitherSidesTree(t *testing.T) {
	// Arrange.
	s, repo, dir := world(t)
	root := repo.BranchHeads["main"]
	left := s.AddCommit(nil, "", "left", []string{root}, nil)
	right := s.AddCommit(nil, "", "right", []string{root}, nil)

	// Act.
	got := Run(s, "/", []string{"-C", dir, "merge-tree", "--write-tree", left.SHA, right.SHA})

	// Assert.
	tree := strings.TrimSpace(got.Stdout)
	if got.Exit != 0 || tree == left.TreeOf() || tree == right.TreeOf() || tree == "" {
		t.Fatalf("merge-tree of diverged lines = %+v, want a tree of its own", got)
	}
}

func TestMergeTreeRefusesWithoutWriteTree(t *testing.T) {
	// Arrange.
	s, repo, dir := world(t)
	head := repo.BranchHeads["main"]

	// Act.
	got := Run(s, "/", []string{"-C", dir, "merge-tree", head, head})

	// Assert.
	if got.Exit == 0 {
		t.Fatalf("merge-tree without --write-tree = %+v, want a refusal", got)
	}
}

func TestUpdateRefDeletesABranchAtItsHead(t *testing.T) {
	// Arrange.
	s, repo, dir := world(t)
	repo.AddBranch("feature", repo.BranchHeads["main"])

	// Act.
	got := Run(s, "/", []string{"-C", dir, "update-ref", "-d", "refs/heads/feature", repo.BranchHeads["main"]})

	// Assert.
	if got.Exit != 0 || repo.HasBranch("feature") {
		t.Fatalf("update-ref -d = %+v, branches %v, want the branch gone", got, repo.Branches)
	}
}

func TestUpdateRefRefusesABranchThatMoved(t *testing.T) {
	// Arrange.
	s, repo, dir := world(t)
	repo.AddBranch("feature", repo.BranchHeads["main"])

	// Act.
	got := Run(s, "/", []string{"-C", dir, "update-ref", "-d", "refs/heads/feature", "0123456789012345678901234567890123456789"})

	// Assert.
	if got.Exit != 128 || !strings.Contains(got.Stderr, "but expected") || !repo.HasBranch("feature") {
		t.Fatalf("update-ref -d of a moved branch = %+v, want git's refusal and the branch kept", got)
	}
}

func TestRevParseResolvesAFullBranchRef(t *testing.T) {
	// Arrange.
	s, repo, dir := world(t)

	// Act.
	got := Run(s, "/", []string{"-C", dir, "rev-parse", "--verify", "--end-of-options", "refs/heads/main^{commit}"})

	// Assert.
	if got.Exit != 0 || got.Stdout != repo.BranchHeads["main"]+"\n" {
		t.Fatalf("rev-parse refs/heads/main = %+v, want main's head", got)
	}
}

func TestGitDirOfIsWhatRevParseReports(t *testing.T) {
	// Arrange.
	s, _, dir := world(t)
	target := addTree(t, s, dir, "wt")
	reported := Run(s, "/", []string{"-C", target, "rev-parse", "--absolute-git-dir"})

	// Act.
	got, ok := s.GitDirOf(target)

	// Assert.
	if !ok || got+"\n" != reported.Stdout {
		t.Fatalf("GitDirOf = (%q, %v), want what rev-parse reported, %q", got, ok, reported.Stdout)
	}
}

// --- the merge queue's own tree ------------------------------------------

func TestWorktreeAddDetachChecksTheCommitOutWithNoBranch(t *testing.T) {
	// Arrange.
	s, repo, dir := world(t)
	tree := filepath.Join(t.TempDir(), "queue-tree")
	base := repo.BranchHeads["main"]

	// Act.
	got := Run(s, "/", []string{"-C", dir, "worktree", "add", "--detach", tree, base})

	// Assert.
	wt := repo.Worktree(tree)
	if got.Exit != 0 || wt == nil || wt.Branch != "" || wt.Head != base {
		t.Fatalf("worktree add --detach = %+v, tree %+v; want a detached tree at %s", got, wt, base)
	}
}

func TestAMergeInADetachedTreeMovesOnlyThatTree(t *testing.T) {
	// Arrange.
	s, repo, dir := world(t)
	tree := filepath.Join(t.TempDir(), "queue-tree")
	base := repo.BranchHeads["main"]
	repo.AddBranch("feature", base)
	s.AddCommit(repo, "feature", "feature work", []string{base}, []string{"f.txt"})
	Run(s, "/", []string{"-C", dir, "worktree", "add", "--detach", tree, base})

	// Act.
	got := Run(s, "/", []string{"-C", tree, "merge", "--no-ff", "--no-edit", "-m", "merge feature", "feature"})

	// Assert.
	if got.Exit != 0 || repo.Worktree(tree).Head == base || repo.BranchHeads["main"] != base {
		t.Fatalf("merge = %+v; tree head %s, main %s; want the tree moved and main untouched",
			got, repo.Worktree(tree).Head, repo.BranchHeads["main"])
	}
}

func TestMergeFFOnlyMovesTheBranchToADescendant(t *testing.T) {
	// Arrange.
	s, repo, dir := world(t)
	base := repo.BranchHeads["main"]
	ahead := s.AddCommit(repo, "", "the queue's merge", []string{base}, nil)

	// Act.
	got := Run(s, "/", []string{"-C", dir, "merge", "--ff-only", ahead.SHA})

	// Assert.
	if got.Exit != 0 || repo.BranchHeads["main"] != ahead.SHA || repo.Worktree(dir).Head != ahead.SHA {
		t.Fatalf("merge --ff-only = %+v; main %s; want main fast-forwarded to %s", got, repo.BranchHeads["main"], ahead.SHA)
	}
}

func TestMergeFFOnlyRefusesATargetThatMoved(t *testing.T) {
	// Arrange: the queue's merge was made on base; main moved on since.
	s, repo, dir := world(t)
	base := repo.BranchHeads["main"]
	queued := s.AddCommit(repo, "", "the queue's merge", []string{base}, nil)
	s.AddCommit(repo, "main", "someone else's commit", []string{base}, nil)

	// Act.
	got := Run(s, "/", []string{"-C", dir, "merge", "--ff-only", queued.SHA})

	// Assert.
	if got.Exit == 0 || !strings.Contains(got.Stderr, "Not possible to fast-forward") {
		t.Fatalf("merge --ff-only onto a moved branch = %+v, want git's refusal", got)
	}
}

func TestAScriptedConflictStandsWhileTheBranchDoesNotMove(t *testing.T) {
	// Arrange: a conflict met once.
	s, repo, dir := world(t)
	repo.AddBranch("feature", repo.BranchHeads["main"])
	s.Conflicts = append(s.Conflicts, &Conflict{Dir: dir, Branch: "feature", Paths: []string{"a.txt"}})
	Run(s, "/", []string{"-C", dir, "merge", "--no-ff", "--no-edit", "-m", "merge feature", "feature"})
	Run(s, "/", []string{"-C", dir, "merge", "--abort"})

	// Act: the same two histories merged again.
	got := Run(s, "/", []string{"-C", dir, "merge", "--no-ff", "--no-edit", "-m", "merge feature", "feature"})

	// Assert.
	if got.Exit == 0 {
		t.Fatalf("the same merge landed the second time: %+v, want the conflict again", got)
	}
}

func TestAScriptedConflictIsGoneOnceTheBranchMoved(t *testing.T) {
	// Arrange: a conflict met once, then resolved on the branch.
	s, repo, dir := world(t)
	base := repo.BranchHeads["main"]
	repo.AddBranch("feature", base)
	s.Conflicts = append(s.Conflicts, &Conflict{Dir: dir, Branch: "feature", Paths: []string{"a.txt"}})
	Run(s, "/", []string{"-C", dir, "merge", "--no-ff", "--no-edit", "-m", "merge feature", "feature"})
	Run(s, "/", []string{"-C", dir, "merge", "--abort"})
	s.AddCommit(repo, "feature", "resolve the conflict", []string{base}, []string{"a.txt"})

	// Act.
	got := Run(s, "/", []string{"-C", dir, "merge", "--no-ff", "--no-edit", "-m", "merge feature", "feature"})

	// Assert.
	if got.Exit != 0 {
		t.Fatalf("the merge after the branch moved = %+v, want it to land", got)
	}
}

func TestAScriptedConflictAppliesToAnyTreeOfItsRepository(t *testing.T) {
	// Arrange: a conflict scripted on the main tree, merged in the queue's.
	s, repo, dir := world(t)
	tree := filepath.Join(t.TempDir(), "queue-tree")
	base := repo.BranchHeads["main"]
	repo.AddBranch("feature", base)
	s.Conflicts = append(s.Conflicts, &Conflict{Dir: dir, Branch: "feature", Paths: []string{"a.txt"}})
	Run(s, "/", []string{"-C", dir, "worktree", "add", "--detach", tree, base})

	// Act.
	got := Run(s, "/", []string{"-C", tree, "merge", "--no-ff", "--no-edit", "-m", "merge feature", "feature"})

	// Assert.
	if got.Exit == 0 {
		t.Fatalf("the queue tree's merge landed: %+v, want the repository's scripted conflict", got)
	}
}

func TestDiffOverAThreeDotRangeListsWhatTheBranchBrought(t *testing.T) {
	// Arrange: main moved on its own; the branch changed one file.
	s, repo, dir := world(t)
	base := repo.BranchHeads["main"]
	repo.AddBranch("feature", base)
	s.AddCommit(repo, "feature", "the branch's work", []string{base}, []string{"b.txt"})
	s.AddCommit(repo, "main", "main's own work", []string{base}, []string{"m.txt"})

	// Act.
	got := Run(s, "/", []string{"-C", dir, "diff", "--name-only", "-z", "main...feature"})

	// Assert.
	if got.Stdout != "b.txt\x00" {
		t.Fatalf("diff main...feature = %q, want only what the branch brought", got.Stdout)
	}
}
