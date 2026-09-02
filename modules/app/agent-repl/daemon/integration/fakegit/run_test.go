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
