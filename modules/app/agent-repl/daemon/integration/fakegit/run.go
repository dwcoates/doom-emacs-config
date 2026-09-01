package fakegit

import (
	"fmt"
	"os"
	"path/filepath"
	"strings"
)

// Result is one fake invocation's answer.
type Result struct {
	Stdout string
	Stderr string
	Exit   int
}

// fieldSep and the format tokens mirror the git leaf's `--format` template.
const fieldSep = "\x1f"

// Run applies one `git` argument vector against the world and answers what the
// real binary would have printed. It is a pure function of the state plus the
// filesystem effects a worktree command has, so it is unit-testable without a
// process.
func Run(s *State, cwd string, args []string) Result {
	s.Calls = append(s.Calls, Call{Args: append([]string(nil), args...), Cwd: cwd})

	dir := cwd
	subject := args
	if len(args) >= 2 && args[0] == "-C" {
		dir, subject = args[1], args[2:]
	}
	if len(subject) == 0 {
		return Result{Stderr: "usage: git <command>\n", Exit: 129}
	}
	if f := s.takeFailure(dir, subject); f != nil {
		return Result{Stderr: f.Stderr, Exit: f.Exit}
	}

	repo, wt := s.FindWorktree(dir)
	if repo == nil {
		return Result{
			Stderr: fmt.Sprintf("fatal: not a git repository: %s\n", dir),
			Exit:   128,
		}
	}

	switch subject[0] {
	case "symbolic-ref":
		return symbolicRef(repo, subject)
	case "config":
		return config(repo, subject)
	case "show-ref":
		return showRef(repo, subject)
	case "rev-parse":
		return revParse(repo, wt, dir, subject)
	case "worktree":
		return worktree(s, repo, subject)
	case "branch":
		return branch(repo, subject)
	case "merge":
		return merge(s, repo, wt, subject)
	case "commit":
		return commit(s, repo, wt, subject)
	case "revert":
		return revert(s, repo, wt, subject)
	case "diff":
		return diff(s, repo, wt, subject)
	case "status":
		return status(wt, subject)
	case "rev-list":
		return revList(s, repo, subject)
	case "show":
		return show(s, repo, wt, subject)
	}
	return Result{Stderr: fmt.Sprintf("fatal: fakegit has no fixture for `git %s`\n", strings.Join(subject, " ")), Exit: 128}
}

// takeFailure pops the first scripted failure matching this command.
func (s *State) takeFailure(dir string, subject []string) *Failure {
	for i, f := range s.Failures {
		if f.Dir != "" && Canon(f.Dir) != Canon(dir) {
			continue
		}
		if !hasPrefix(subject, f.Match) {
			continue
		}
		s.Failures = append(s.Failures[:i], s.Failures[i+1:]...)
		if f.Exit == 0 {
			f.Exit = 1
		}
		return f
	}
	return nil
}

func hasPrefix(args, match []string) bool {
	if len(match) > len(args) {
		return false
	}
	for i := range match {
		if args[i] != match[i] {
			return false
		}
	}
	return true
}

func symbolicRef(repo *Repo, subject []string) Result {
	if repo.OriginHead == "" {
		return Result{Stderr: "fatal: ref refs/remotes/origin/HEAD is not a symbolic ref\n", Exit: 128}
	}
	return Result{Stdout: repo.OriginHead + "\n"}
}

func config(repo *Repo, subject []string) Result {
	if len(subject) >= 3 && subject[1] == "--get" && subject[2] == "init.defaultBranch" {
		if repo.DefaultBranch == "" {
			return Result{Exit: 1}
		}
		return Result{Stdout: repo.DefaultBranch + "\n"}
	}
	return Result{Exit: 1}
}

func showRef(repo *Repo, subject []string) Result {
	ref := subject[len(subject)-1]
	name := strings.TrimPrefix(ref, "refs/heads/")
	if repo.HasBranch(name) {
		return Result{}
	}
	return Result{Exit: 1}
}

func revParse(repo *Repo, wt *Worktree, dir string, subject []string) Result {
	switch {
	case contains(subject, "--git-common-dir"):
		return Result{Stdout: repo.CommonDir + "\n"}
	case contains(subject, "--abbrev-ref"):
		if wt == nil || wt.Branch == "" {
			return Result{Stdout: "HEAD\n"}
		}
		return Result{Stdout: wt.Branch + "\n"}
	}
	ref := strings.TrimSuffix(subject[len(subject)-1], "^{commit}")
	sha, ok := resolve(repo, wt, ref)
	if !ok {
		return Result{Stderr: fmt.Sprintf("fatal: Needed a single revision\n%s\n", ref), Exit: 128}
	}
	return Result{Stdout: sha + "\n"}
}

func resolve(repo *Repo, wt *Worktree, ref string) (string, bool) {
	if ref == "HEAD" {
		if wt == nil || wt.Head == "" {
			return "", false
		}
		return wt.Head, true
	}
	if sha, ok := repo.BranchHeads[ref]; ok {
		return sha, true
	}
	if len(ref) == 40 {
		return ref, true
	}
	return "", false
}

func contains(args []string, want string) bool {
	for _, a := range args {
		if a == want {
			return true
		}
	}
	return false
}

// worktree implements `worktree add|remove|prune`. The directory effects are
// real: the daemon reads the tree off disk, and the postcondition checks in the
// git leaf are exactly what a missing effect would trip.
func worktree(s *State, repo *Repo, subject []string) Result {
	switch {
	case len(subject) >= 2 && subject[1] == "add":
		var branchName, dir, base string
		for i := 2; i < len(subject); i++ {
			switch subject[i] {
			case "-b":
				if i+1 < len(subject) {
					branchName = subject[i+1]
					i++
				}
			default:
				if dir == "" {
					dir = subject[i]
				} else if base == "" {
					base = subject[i]
				}
			}
		}
		if branchName == "" || dir == "" {
			return Result{Stderr: "fatal: fakegit: `worktree add` needs -b <branch> <dir> <base>\n", Exit: 128}
		}
		if repo.HasBranch(branchName) {
			return Result{Stderr: fmt.Sprintf("fatal: a branch named '%s' already exists\n", branchName), Exit: 128}
		}
		head, ok := repo.BranchHeads[base]
		if !ok {
			if len(base) == 40 {
				head = base
			} else {
				return Result{Stderr: fmt.Sprintf("fatal: invalid reference: %s\n", base), Exit: 128}
			}
		}
		if err := os.MkdirAll(dir, 0o755); err != nil {
			return Result{Stderr: err.Error() + "\n", Exit: 128}
		}
		if err := os.WriteFile(filepath.Join(dir, ".git"), []byte("gitdir: "+repo.CommonDir+"\n"), 0o644); err != nil {
			return Result{Stderr: err.Error() + "\n", Exit: 128}
		}
		repo.AddBranch(branchName, head)
		repo.Worktrees = append(repo.Worktrees, &Worktree{Dir: dir, Branch: branchName, Head: head})
		return Result{Stdout: "Preparing worktree\n"}

	case len(subject) >= 2 && subject[1] == "remove":
		dir := subject[len(subject)-1]
		if repo.Worktree(dir) == nil {
			return Result{Stderr: fmt.Sprintf("fatal: '%s' is not a working tree\n", dir), Exit: 128}
		}
		if err := os.RemoveAll(dir); err != nil {
			return Result{Stderr: err.Error() + "\n", Exit: 128}
		}
		repo.RemoveWorktree(dir)
		return Result{}

	case len(subject) >= 2 && subject[1] == "prune":
		kept := repo.Worktrees[:0]
		for i, wt := range repo.Worktrees {
			if i == 0 {
				kept = append(kept, wt)
				continue
			}
			if _, err := os.Stat(wt.Dir); err == nil {
				kept = append(kept, wt)
			}
		}
		repo.Worktrees = kept
		return Result{}
	}
	return Result{Stderr: "fatal: fakegit: unsupported worktree command\n", Exit: 128}
}

func branch(repo *Repo, subject []string) Result {
	if len(subject) >= 3 && subject[1] == "-D" {
		name := subject[2]
		if !repo.HasBranch(name) {
			return Result{Stderr: fmt.Sprintf("error: branch '%s' not found.\n", name), Exit: 1}
		}
		repo.RemoveBranch(name)
		return Result{Stdout: "Deleted branch " + name + "\n"}
	}
	return Result{Stderr: "fatal: fakegit: unsupported branch command\n", Exit: 128}
}

func merge(s *State, repo *Repo, wt *Worktree, subject []string) Result {
	if contains(subject, "--abort") {
		if wt != nil {
			wt.Conflicted = nil
		}
		return Result{}
	}
	message := ""
	for i := 1; i < len(subject); i++ {
		if subject[i] == "-m" && i+1 < len(subject) {
			message = subject[i+1]
		}
	}
	source := subject[len(subject)-1]
	if wt == nil {
		return Result{Stderr: "fatal: not a working tree\n", Exit: 128}
	}
	for i, c := range s.Conflicts {
		if Canon(c.Dir) != Canon(wt.Dir) || c.Branch != source {
			continue
		}
		s.Conflicts = append(s.Conflicts[:i], s.Conflicts[i+1:]...)
		wt.Conflicted = c.Paths
		return Result{
			Stdout: "Auto-merging\nCONFLICT (content): Merge conflict\n",
			Stderr: "Automatic merge failed; fix conflicts and then commit the result.\n",
			Exit:   1,
		}
	}
	sourceHead, ok := repo.BranchHeads[source]
	if !ok {
		return Result{Stderr: fmt.Sprintf("merge: %s - not something we can merge\n", source), Exit: 1}
	}
	landedPaths := s.Commits[sourceHead].pathsOr(nil)
	c := s.AddCommit(repo, wt.Branch, message, []string{wt.Head, sourceHead}, landedPaths)
	return Result{Stdout: "Merge made by the 'ort' strategy.\n" + c.SHA + "\n"}
}

func (c *Commit) pathsOr(fallback []string) []string {
	if c == nil {
		return fallback
	}
	return c.Paths
}

func commit(s *State, repo *Repo, wt *Worktree, subject []string) Result {
	if wt == nil {
		return Result{Stderr: "fatal: not a working tree\n", Exit: 128}
	}
	message := ""
	for i := 1; i < len(subject); i++ {
		if subject[i] == "-m" && i+1 < len(subject) {
			message = subject[i+1]
		}
	}
	parents := []string{wt.Head}
	conflicted := wt.Conflicted
	wt.Conflicted = nil
	wt.Dirty = false
	c := s.AddCommit(repo, wt.Branch, message, parents, conflicted)
	return Result{Stdout: "[" + wt.Branch + " " + c.SHA[:7] + "] " + message + "\n"}
}

func revert(s *State, repo *Repo, wt *Worktree, subject []string) Result {
	if wt == nil {
		return Result{Stderr: "fatal: not a working tree\n", Exit: 128}
	}
	target := subject[len(subject)-1]
	c := s.AddCommit(repo, wt.Branch, `Revert "`+target+`"`, []string{wt.Head}, nil)
	return Result{Stdout: "[" + wt.Branch + " " + c.SHA[:7] + "] revert\n"}
}

func diff(s *State, repo *Repo, wt *Worktree, subject []string) Result {
	if contains(subject, "--diff-filter=U") {
		if wt == nil || len(wt.Conflicted) == 0 {
			return Result{}
		}
		return Result{Stdout: strings.Join(wt.Conflicted, "\x00") + "\x00"}
	}
	// `diff --name-only -z <range>`: the paths every commit in the range
	// touched, deduplicated in first-seen order.
	rangeSpec := subject[len(subject)-1]
	paths := s.rangePaths(repo, rangeSpec)
	if len(paths) == 0 {
		return Result{}
	}
	return Result{Stdout: strings.Join(paths, "\x00") + "\x00"}
}

// rangePaths answers the paths a `a..b` range touched.
func (s *State) rangePaths(repo *Repo, rangeSpec string) []string {
	commits := s.rangeCommits(repo, rangeSpec)
	seen := map[string]bool{}
	var out []string
	for _, c := range commits {
		for _, p := range c.Paths {
			if !seen[p] {
				seen[p] = true
				out = append(out, p)
			}
		}
	}
	return out
}

// rangeCommits walks `<a>..<b>`: every commit reachable from b and not from a,
// oldest first. `<sha>^1` and `<sha>^2` name a merge commit's parents.
func (s *State) rangeCommits(repo *Repo, rangeSpec string) []*Commit {
	left, right, found := strings.Cut(rangeSpec, "..")
	if !found {
		return nil
	}
	from := s.peel(repo, left)
	to := s.peel(repo, right)
	excluded := s.reachable(from)
	var walk func(sha string, out *[]*Commit, seen map[string]bool)
	walk = func(sha string, out *[]*Commit, seen map[string]bool) {
		if sha == "" || seen[sha] || excluded[sha] {
			return
		}
		seen[sha] = true
		c := s.Commits[sha]
		if c == nil {
			return
		}
		for _, p := range c.Parents {
			walk(p, out, seen)
		}
		*out = append(*out, c)
	}
	var out []*Commit
	walk(to, &out, map[string]bool{})
	return out
}

func (s *State) reachable(sha string) map[string]bool {
	seen := map[string]bool{}
	var walk func(string)
	walk = func(sha string) {
		if sha == "" || seen[sha] {
			return
		}
		seen[sha] = true
		if c := s.Commits[sha]; c != nil {
			for _, p := range c.Parents {
				walk(p)
			}
		}
	}
	walk(sha)
	return seen
}

// peel resolves `<ref>`, `<ref>^1` and `<ref>^2`.
func (s *State) peel(repo *Repo, ref string) string {
	base, suffix, found := strings.Cut(ref, "^")
	sha := base
	if repo != nil {
		if head, ok := repo.BranchHeads[base]; ok {
			sha = head
		}
	}
	if !found {
		return sha
	}
	c := s.Commits[sha]
	if c == nil {
		return ""
	}
	switch suffix {
	case "1", "":
		if len(c.Parents) >= 1 {
			return c.Parents[0]
		}
	case "2":
		if len(c.Parents) >= 2 {
			return c.Parents[1]
		}
	}
	return ""
}

func status(wt *Worktree, subject []string) Result {
	if wt == nil {
		return Result{Stderr: "fatal: not a working tree\n", Exit: 128}
	}
	if wt.Dirty {
		return Result{Stdout: " M dirty.txt\n"}
	}
	if len(wt.Conflicted) > 0 {
		out := ""
		for _, p := range wt.Conflicted {
			out += "UU " + p + "\n"
		}
		return Result{Stdout: out}
	}
	return Result{}
}

func revList(s *State, repo *Repo, subject []string) Result {
	format := formatOf(subject)
	rangeSpec := subject[len(subject)-1]
	commits := s.rangeCommits(repo, rangeSpec)
	out := ""
	for _, c := range commits {
		out += render(format, c) + "\n"
	}
	return Result{Stdout: out}
}

func show(s *State, repo *Repo, wt *Worktree, subject []string) Result {
	format := formatOf(subject)
	ref := subject[len(subject)-1]
	sha := ref
	if head, ok := repo.BranchHeads[ref]; ok {
		sha = head
	}
	if ref == "HEAD" && wt != nil {
		sha = wt.Head
	}
	c := s.Commits[sha]
	if c == nil {
		return Result{Stderr: fmt.Sprintf("fatal: bad object %s\n", ref), Exit: 128}
	}
	return Result{Stdout: render(format, c) + "\n"}
}

func formatOf(subject []string) string {
	for _, a := range subject {
		if f, ok := strings.CutPrefix(a, "--format="); ok {
			return f
		}
	}
	return "%H"
}

// render substitutes the four placeholders the git leaf's template uses.
func render(format string, c *Commit) string {
	out := format
	out = strings.ReplaceAll(out, "%H", c.SHA)
	out = strings.ReplaceAll(out, "%s", c.Subject)
	out = strings.ReplaceAll(out, "%an", c.Author)
	out = strings.ReplaceAll(out, "%aI", c.AtRFC)
	return out
}

// FieldSep is the unit separator the git leaf's commit template uses. Tests
// build expectations with it rather than repeating the byte.
const FieldSep = fieldSep
