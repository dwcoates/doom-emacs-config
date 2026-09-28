package fakegit

import (
	"fmt"
	"os"
	"path/filepath"
	"sort"
	"strings"
	"time"
)

// Result is one fake invocation's answer.
type Result struct {
	Stdout string
	Stderr string
	Exit   int
}

// fieldSep and the format tokens mirror the git leaf's `--format` template.
const fieldSep = "\x1f"

// fakeGitVersion is what `git version` answers, in real git's exact shape.
const fakeGitVersion = "git version 2.39.5"

// Run applies one `git` argument vector against the world and answers what the
// real binary would have printed. It is a pure function of the state plus the
// filesystem effects a worktree command has, so it is unit-testable without a
// process.
func Run(s *State, cwd string, args []string) Result {
	// THE ANSWER IS RECORDED, NOT ONLY THE QUESTION. A recorded conversation
	// that says `rev-parse --show-toplevel` ran but not what it answered
	// cannot tell a repository the fixture knows from one it does not -- and
	// that difference is the whole diagnosis when a caller reacts to the
	// answer by asking the user a question nobody can answer. The row is
	// reserved before the command runs and completed after, so a command that
	// records further calls of its own still leaves them in issue order.
	at := len(s.Calls)
	s.Calls = append(s.Calls, Call{Args: append([]string(nil), args...), Cwd: cwd, At: time.Now().UTC()})
	res := answer(s, cwd, args)
	s.Calls[at].Exit = res.Exit
	s.Calls[at].Stdout = clipRecorded(res.Stdout)
	s.Calls[at].Stderr = clipRecorded(res.Stderr)
	return res
}

// recordedOutputLimit bounds how much of one answer the fixture keeps. A `log`
// or a `status` can print a lot, and the fixture file is read and rewritten by
// every subsequent invocation; what a diagnosis needs is the first line or two,
// not the whole payload.
const recordedOutputLimit = 512

// clipRecorded bounds one recorded stream, saying so when it clips.
func clipRecorded(out string) string {
	if len(out) <= recordedOutputLimit {
		return out
	}
	return out[:recordedOutputLimit] + "...(clipped)"
}

// answer is Run's body: the pure question-to-answer mapping, with the
// recording left to Run.
func answer(s *State, cwd string, args []string) Result {
	dir, subject := splitGlobalOptions(cwd, args)
	if len(subject) == 0 {
		return Result{Stderr: "usage: git <command>\n", Exit: 129}
	}
	if f := s.takeFailure(dir, subject); f != nil {
		return Result{Stderr: f.Stderr, Exit: f.Exit}
	}
	// `git version` needs no repository: Emacs's vc-git and magit probe it
	// on the first project switch and feed the answer to version-to-list,
	// which signals on a missing one.
	if subject[0] == "version" || subject[0] == "--version" {
		return Result{Stdout: fakeGitVersion + "\n"}
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
		return symbolicRef(repo, wt, subject)
	case "config":
		return config(repo, subject)
	case "show-ref":
		return showRef(repo, subject)
	case "rev-parse":
		return revParse(s, repo, wt, dir, subject)
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
	case "ls-files":
		return lsFiles(wt, dir, subject)
	case "describe":
		return describe(s, repo, wt, subject)
	case "merge-base":
		return mergeBase(s, repo, wt, subject)
	case "rev-list":
		return revList(s, repo, subject)
	case "show":
		return show(s, repo, wt, subject)
	case "update-index":
		return updateIndex(wt, subject)
	case "log":
		return gitLog(s, repo, wt, subject)
	case "merge-tree":
		return mergeTree(s, repo, wt, subject)
	case "update-ref":
		return updateRef(repo, subject)
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

func symbolicRef(repo *Repo, wt *Worktree, subject []string) Result {
	// `symbolic-ref [--short] HEAD` is vc-git's branch probe.
	if subject[len(subject)-1] == "HEAD" {
		if wt == nil || wt.Branch == "" {
			return Result{Stderr: "fatal: ref HEAD is not a symbolic ref\n", Exit: 128}
		}
		if contains(subject, "--short") {
			return Result{Stdout: wt.Branch + "\n"}
		}
		return Result{Stdout: "refs/heads/" + wt.Branch + "\n"}
	}
	if repo.OriginHead == "" {
		return Result{Stderr: "fatal: ref refs/remotes/origin/HEAD is not a symbolic ref\n", Exit: 128}
	}
	return Result{Stdout: repo.OriginHead + "\n"}
}

func config(repo *Repo, subject []string) Result {
	if contains(subject, "--list") {
		return configList(repo, subject)
	}
	if len(subject) >= 3 && subject[1] == "--get" && subject[2] == "init.defaultBranch" {
		if repo.DefaultBranch == "" {
			return Result{Exit: 1}
		}
		return Result{Stdout: repo.DefaultBranch + "\n"}
	}
	// magit reads core.bare at startup; every other key is unset, which real
	// git reports as an empty exit 1.
	if len(subject) >= 3 && subject[1] == "--get" && subject[2] == "core.bare" {
		return Result{Stdout: "false\n"}
	}
	return Result{Exit: 1}
}

// configList answers `config --list [-z]`: every key of the fixture
// repository, lowercased the way real git prints them, as `key\nvalue`
// records. Magit reads the whole table once per refresh and looks its
// settings up out of it, so an empty answer makes every setting nil.
func configList(repo *Repo, subject []string) Result {
	pairs := [][2]string{
		{"core.repositoryformatversion", "0"},
		{"core.filemode", "true"},
		{"core.bare", "false"},
		{"core.logallrefupdates", "true"},
	}
	if repo.DefaultBranch != "" {
		pairs = append([][2]string{{"init.defaultbranch", repo.DefaultBranch}}, pairs...)
	}
	sep := "\n"
	if contains(subject, "-z") || contains(subject, "--null") {
		sep = "\x00"
	}
	var b strings.Builder
	for _, kv := range pairs {
		if sep == "\x00" {
			b.WriteString(kv[0] + "\n" + kv[1] + "\x00")
			continue
		}
		b.WriteString(kv[0] + "=" + kv[1] + "\n")
	}
	return Result{Stdout: b.String()}
}

// updateIndex is `update-index --refresh`, which magit runs before every
// status refresh to settle stat-dirty entries. Real git is silent and exits 0
// on a clean tree, and names each unsettled path and exits 1 on a dirty one.
func updateIndex(wt *Worktree, subject []string) Result {
	if wt == nil {
		return Result{Stderr: "fatal: not a working tree\n", Exit: 128}
	}
	if !contains(subject, "--refresh") {
		return Result{Stderr: "fatal: fakegit has no fixture for `git " + strings.Join(subject, " ") + "`\n", Exit: 128}
	}
	if wt.Dirty {
		return Result{Stdout: "dirty.txt: needs update\n", Exit: 1}
	}
	return Result{}
}

func showRef(repo *Repo, subject []string) Result {
	ref := subject[len(subject)-1]
	name := strings.TrimPrefix(ref, "refs/heads/")
	if repo.HasBranch(name) {
		return Result{}
	}
	return Result{Exit: 1}
}

// revParseInfo answers one of the repository-shape flags Emacs's magit and
// vc-git probe on the first project switch. A nil answer means the flag is not
// one of them.
func revParseInfo(repo *Repo, wt *Worktree, dir string, flag string) (string, bool) {
	switch flag {
	case "--show-toplevel":
		return Canon(wt.Dir), true
	case "--show-cdup":
		return cdup(wt.Dir, dir), true
	case "--git-dir":
		// Real git prints the bare `.git` only from the top of the main
		// worktree; anywhere else it prints the absolute path.
		if isMainWorktree(repo, wt) && Canon(dir) == Canon(wt.Dir) {
			return ".git", true
		}
		return gitDir(repo, wt), true
	case "--absolute-git-dir":
		return gitDir(repo, wt), true
	case "--git-common-dir":
		return repo.CommonDir, true
	case "--is-inside-work-tree":
		return "true", true
	case "--is-bare-repository":
		return "false", true
	case "--is-inside-git-dir":
		return "false", true
	case "--show-prefix":
		return prefix(wt.Dir, dir), true
	}
	return "", false
}

// isMainWorktree reports whether wt is the repository's main worktree, which
// git registers first.
func isMainWorktree(repo *Repo, wt *Worktree) bool {
	return len(repo.Worktrees) > 0 && repo.Worktrees[0] == wt
}

// gitDir is the absolute git directory of one worktree: the common dir for the
// main worktree, and the per-worktree subdirectory under it for a linked one,
// exactly as real git reports them.
func gitDir(repo *Repo, wt *Worktree) string {
	if isMainWorktree(repo, wt) {
		return repo.CommonDir
	}
	return filepath.Join(repo.CommonDir, "worktrees", filepath.Base(Canon(wt.Dir)))
}

// GitDirOf answers the absolute git directory `rev-parse --absolute-git-dir`
// reports for a worktree directory, without recording an invocation: a test
// that needs to write a worktree's admin files asks here.
func (s *State) GitDirOf(dir string) (string, bool) {
	repo, wt := s.FindWorktree(dir)
	if repo == nil || wt == nil {
		return "", false
	}
	return gitDir(repo, wt), true
}

// cdup is `--show-cdup`: the relative path back up to the top of the worktree,
// with a trailing separator, and empty at the top.
func cdup(top, dir string) string {
	depth := len(splitPrefix(top, dir))
	return strings.Repeat("../", depth)
}

// prefix is `--show-prefix`: the path from the top of the worktree down to the
// directory, with a trailing separator, and empty at the top.
func prefix(top, dir string) string {
	parts := splitPrefix(top, dir)
	if len(parts) == 0 {
		return ""
	}
	return strings.Join(parts, "/") + "/"
}

// splitPrefix is the path segments between the top of a worktree and a
// directory inside it.
func splitPrefix(top, dir string) []string {
	rel, err := filepath.Rel(Canon(top), Canon(dir))
	if err != nil || rel == "." || strings.HasPrefix(rel, "..") {
		return nil
	}
	return strings.Split(filepath.ToSlash(rel), "/")
}

func revParse(s *State, repo *Repo, wt *Worktree, dir string, subject []string) Result {
	// A fixture repository has no remotes, so every `@{upstream}` reference
	// is refused exactly as real git refuses it. This has to come first:
	// `rev-parse --verify --abbrev-ref <branch>@{upstream}` carries a flag
	// that would otherwise be answered with the branch itself, which would
	// tell magit the branch is tracking one.
	for _, a := range subject[1:] {
		if !strings.Contains(a, "@{u") {
			continue
		}
		return noUpstream(wt)
	}
	// magit passes several shape flags in ONE call and reads one line per
	// flag, in the order it passed them.
	if wt != nil {
		var b strings.Builder
		answered := false
		for _, flag := range subject[1:] {
			line, ok := revParseInfo(repo, wt, dir, flag)
			if !ok {
				continue
			}
			answered = true
			b.WriteString(line + "\n")
		}
		if answered {
			return Result{Stdout: b.String()}
		}
	}
	switch {
	case contains(subject, "--git-common-dir"):
		return Result{Stdout: repo.CommonDir + "\n"}
	case contains(subject, "--abbrev-ref"):
		if wt == nil || wt.Branch == "" {
			return Result{Stdout: "HEAD\n"}
		}
		return Result{Stdout: wt.Branch + "\n"}
	}
	if ref, ok := strings.CutSuffix(subject[len(subject)-1], "^{tree}"); ok {
		sha, found := s.resolve(repo, wt, ref)
		if c := s.Commits[sha]; found && c != nil {
			return Result{Stdout: c.TreeOf() + "\n"}
		}
		return Result{Stderr: "fatal: Needed a single revision\n", Exit: 128}
	}
	ref := strings.TrimSuffix(subject[len(subject)-1], "^{commit}")
	sha, ok := s.resolve(repo, wt, ref)
	if !ok {
		return Result{Stderr: "fatal: Needed a single revision\n", Exit: 128}
	}
	if contains(subject, "--short") {
		return Result{Stdout: s.Abbrev(sha) + "\n"}
	}
	return Result{Stdout: sha + "\n"}
}

// noUpstream is real git's refusal for a branch with no upstream configured.
func noUpstream(wt *Worktree) Result {
	if wt == nil || wt.Branch == "" {
		return Result{Stderr: "fatal: HEAD does not point to a branch\n", Exit: 128}
	}
	return Result{Stderr: fmt.Sprintf("fatal: no upstream configured for branch '%s'\n", wt.Branch), Exit: 128}
}

// cutAncestry splits `<ref>~<n>` and `<ref>^<n>` into the base ref and the
// number of first-parent steps back from it. `~` and `^` with no number mean
// one step.
func cutAncestry(ref string) (string, int, bool) {
	i := strings.LastIndexAny(ref, "~^")
	if i <= 0 {
		return "", 0, false
	}
	suffix := ref[i+1:]
	if suffix == "" {
		return ref[:i], 1, true
	}
	n := 0
	if _, err := fmt.Sscanf(suffix, "%d", &n); err != nil || n < 0 {
		return "", 0, false
	}
	return ref[:i], n, true
}

func (s *State) resolve(repo *Repo, wt *Worktree, ref string) (string, bool) {
	if base, n, ok := cutAncestry(ref); ok {
		sha, ok := s.resolve(repo, wt, base)
		if !ok {
			return "", false
		}
		for ; n > 0; n-- {
			c := s.Commits[sha]
			if c == nil || len(c.Parents) == 0 {
				return "", false
			}
			sha = c.Parents[0]
		}
		return sha, true
	}
	if ref == "HEAD" {
		if wt == nil || wt.Head == "" {
			return "", false
		}
		return wt.Head, true
	}
	if sha, ok := repo.BranchHeads[ref]; ok {
		return sha, true
	}
	// A FULL branch ref names the same branch its short name does.
	if name, full := strings.CutPrefix(ref, "refs/heads/"); full {
		if sha, ok := repo.BranchHeads[name]; ok {
			return sha, true
		}
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
	case len(subject) >= 2 && subject[1] == "add" && contains(subject, "--detach"):
		return addDetached(s, repo, subject)

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
		target := repo.Worktree(dir)
		if target == nil {
			return Result{Stderr: fmt.Sprintf("fatal: '%s' is not a working tree\n", dir), Exit: 128}
		}
		// Real git refuses a tree with modified or untracked content unless
		// it is forced.
		if target.Dirty && !contains(subject, "--force") {
			return Result{Stderr: fmt.Sprintf("fatal: '%s' contains modified or untracked files, use --force to delete it\n", dir), Exit: 128}
		}
		if err := os.RemoveAll(dir); err != nil {
			return Result{Stderr: err.Error() + "\n", Exit: 128}
		}
		repo.RemoveWorktree(dir)
		return Result{}

	case len(subject) >= 2 && subject[1] == "list":
		// git lists the MAIN worktree first, and the daemon reads exactly
		// that order, so the fixture keeps it: Worktrees[0] is the main one.
		// `-z` ends every attribute with NUL instead of a newline.
		sep := "\n"
		if contains(subject, "-z") {
			sep = "\x00"
		}
		var b strings.Builder
		for _, wt := range repo.Worktrees {
			b.WriteString("worktree " + wt.Dir + sep)
			b.WriteString("HEAD " + wt.Head + sep)
			if wt.Branch == "" {
				b.WriteString("detached" + sep)
			} else {
				b.WriteString("branch refs/heads/" + wt.Branch + sep)
			}
			if wt.Locked {
				b.WriteString("locked" + sep)
			}
			b.WriteString(sep)
		}
		return Result{Stdout: b.String()}

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

// addDetached implements `worktree add --detach <dir> <commit>`: the merge
// queue's scratch tree, which names no branch.
func addDetached(s *State, repo *Repo, subject []string) Result {
	var args []string
	for _, a := range subject[2:] {
		if a != "--detach" {
			args = append(args, a)
		}
	}
	if len(args) != 2 {
		return Result{Stderr: "fatal: fakegit: `worktree add --detach` needs <dir> <commit>\n", Exit: 128}
	}
	dir, base := args[0], args[1]
	head, ok := s.resolve(repo, nil, base)
	if !ok {
		return Result{Stderr: fmt.Sprintf("fatal: invalid reference: %s\n", base), Exit: 128}
	}
	if err := os.MkdirAll(dir, 0o755); err != nil {
		return Result{Stderr: err.Error() + "\n", Exit: 128}
	}
	if err := os.WriteFile(filepath.Join(dir, ".git"), []byte("gitdir: "+repo.CommonDir+"\n"), 0o644); err != nil {
		return Result{Stderr: err.Error() + "\n", Exit: 128}
	}
	repo.Worktrees = append(repo.Worktrees, &Worktree{Dir: dir, Head: head})
	return Result{Stdout: "Preparing worktree (detached HEAD)\n"}
}

// fastForward implements `merge --ff-only <commit>`: the branch checked out in
// wt moves to commit only when its head is an ancestor of it.
func fastForward(s *State, repo *Repo, wt *Worktree, target string) Result {
	sha, ok := s.resolve(repo, wt, target)
	if !ok {
		return Result{Stderr: fmt.Sprintf("merge: %s - not something we can merge\n", target), Exit: 1}
	}
	if !s.reachable(sha)[wt.Head] {
		return Result{Stderr: "fatal: Not possible to fast-forward, aborting.\n", Exit: 128}
	}
	wt.Head = sha
	if wt.Branch != "" {
		repo.BranchHeads[wt.Branch] = sha
		for _, other := range repo.Worktrees {
			if other.Branch == wt.Branch {
				other.Head = sha
			}
		}
	}
	return Result{Stdout: "Fast-forward\n"}
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
	if contains(subject, "--ff-only") {
		return fastForward(s, repo, wt, source)
	}
	for i, c := range s.Conflicts {
		// A CONFLICT SCRIPTED ON ANY TREE OF THE REPOSITORY applies to a merge
		// in any of its trees: the queue merges in a scratch tree of its own,
		// whose path a test does not know, and the conflict is a fact about
		// the two histories, not about the directory.
		if !sameRepoTree(repo, c.Dir, wt.Dir) || c.Branch != source {
			continue
		}
		head := repo.BranchHeads[source]
		if c.SourceHead != "" && c.SourceHead != head {
			// The branch moved since the conflict was met: resolved.
			s.Conflicts = append(s.Conflicts[:i], s.Conflicts[i+1:]...)
			break
		}
		c.SourceHead = head
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
	if wt.Branch == "" {
		wt.Head = c.SHA
	}
	return Result{Stdout: "Merge made by the 'ort' strategy.\n" + c.SHA + "\n"}
}

// sameRepoTree reports whether a scripted conflict's directory and a merging
// tree are trees of one repository.
func sameRepoTree(repo *Repo, scripted, merging string) bool {
	if Canon(scripted) == Canon(merging) {
		return true
	}
	return repo.Worktree(scripted) != nil || Canon(scripted) == Canon(repo.Dir)
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
	if wt.Branch == "" {
		wt.Head = c.SHA
	}
	return Result{Stdout: "[" + wt.Branch + " " + s.Abbrev(c.SHA) + "] " + message + "\n"}
}

func revert(s *State, repo *Repo, wt *Worktree, subject []string) Result {
	if wt == nil {
		return Result{Stderr: "fatal: not a working tree\n", Exit: 128}
	}
	target := subject[len(subject)-1]
	c := s.AddCommit(repo, wt.Branch, `Revert "`+target+`"`, []string{wt.Head}, nil)
	return Result{Stdout: "[" + wt.Branch + " " + s.Abbrev(c.SHA) + "] revert\n"}
}

func diff(s *State, repo *Repo, wt *Worktree, subject []string) Result {
	if contains(subject, "--diff-filter=U") {
		if wt == nil || len(wt.Conflicted) == 0 {
			return Result{}
		}
		return Result{Stdout: strings.Join(wt.Conflicted, "\x00") + "\x00"}
	}
	// With no `a..b` range, this is magit's worktree diff: `--cached` is the
	// index against HEAD and the bare form is the worktree against the index.
	rangeSpec, ranged := diffRange(subject)
	if !ranged {
		if wt == nil || !wt.Dirty || contains(subject, "--cached") {
			return Result{}
		}
		return Result{Stdout: dirtyDiff}
	}
	paths := s.rangePaths(repo, rangeSpec)
	if len(paths) == 0 {
		return Result{}
	}
	return Result{Stdout: strings.Join(paths, "\x00") + "\x00"}
}

// diffRange answers the `a..b` revision range of a diff, and whether the
// command carried one at all.
func diffRange(subject []string) (string, bool) {
	for _, a := range subject[1:] {
		if !strings.HasPrefix(a, "-") && strings.Contains(a, "..") {
			return a, true
		}
	}
	return "", false
}

// dirtyDiff is the unified diff a scripted dirty tree shows, in real git's
// exact shape under magit's `--no-prefix`. The fixture's one dirty path is
// `dirty.txt`, the same path `status --porcelain` reports.
const dirtyDiff = `diff --git dirty.txt dirty.txt
index 816a8fa..04848eb 100644
--- dirty.txt
+++ dirty.txt
@@ -1 +1,2 @@
 fake repository
+scripted local change
`

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
	// `<a>...<b>` is what b brought since the two diverged: every commit
	// reachable from b and not from a, which is the two-dot walk.
	right = strings.TrimPrefix(right, ".")
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
	var entries []string
	// `--branch` prepends the branch header, which vc-git reads.
	if contains(subject, "--branch") || contains(subject, "-b") {
		head := "HEAD (no branch)"
		if wt.Branch != "" {
			head = wt.Branch
		}
		entries = append(entries, "## "+head)
	}
	switch {
	case wt.Dirty:
		entries = append(entries, " M dirty.txt")
	case len(wt.Conflicted) > 0:
		for _, p := range wt.Conflicted {
			entries = append(entries, "UU "+p)
		}
	}
	if len(entries) == 0 {
		return Result{}
	}
	// `-z` terminates each entry with NUL instead of newline.
	sep := "\n"
	if contains(subject, "-z") {
		sep = "\x00"
	}
	return Result{Stdout: strings.Join(entries, sep) + sep}
}

// lsFiles answers the tracked paths of a worktree, relative to the directory
// the command ran in, which is what projectile and magit list a project with.
// `-o` adds untracked paths and the fixture models none.
func lsFiles(wt *Worktree, dir string, subject []string) Result {
	if wt == nil {
		return Result{Stderr: "fatal: not a working tree\n", Exit: 128}
	}
	under := prefix(wt.Dir, dir)
	var kept []string
	for _, f := range wt.Files {
		if !strings.HasPrefix(f, under) {
			continue
		}
		kept = append(kept, strings.TrimPrefix(f, under))
	}
	if len(kept) == 0 {
		return Result{}
	}
	sep := "\n"
	if hasZFlag(subject) {
		sep = "\x00"
	}
	return Result{Stdout: strings.Join(kept, sep) + sep}
}

// hasZFlag reports whether NUL termination was asked for, including inside a
// bundled short-flag cluster such as `-zco`.
func hasZFlag(subject []string) bool {
	for _, a := range subject {
		if strings.HasPrefix(a, "-") && !strings.HasPrefix(a, "--") && strings.Contains(a, "z") {
			return true
		}
	}
	return false
}

// describe answers what real git answers in a repository with no tags, which is
// how magit and vc-git learn there is no description to show. `--always` falls
// back to the abbreviated commit instead of failing, and `--contains` names the
// commit it could not describe.
func describe(s *State, repo *Repo, wt *Worktree, subject []string) Result {
	if contains(subject, "--contains") {
		sha, ok := s.resolve(repo, wt, subject[len(subject)-1])
		if !ok {
			return Result{Stderr: "fatal: Needed a single revision\n", Exit: 128}
		}
		return Result{Stderr: fmt.Sprintf("fatal: cannot describe '%s'\n", sha), Exit: 128}
	}
	if contains(subject, "--always") && wt != nil && wt.Head != "" {
		return Result{Stdout: s.Abbrev(wt.Head) + "\n"}
	}
	return Result{Stderr: "fatal: No names found, cannot describe anything.\n", Exit: 128}
}

// mergeBase answers `merge-base --is-ancestor <a> <b>`, which magit asks to
// decide whether HEAD is on the branch it is comparing against. Real git is
// silent and answers through the exit status alone: 0 for an ancestor, 1 for
// anything else.
func mergeBase(s *State, repo *Repo, wt *Worktree, subject []string) Result {
	if !contains(subject, "--is-ancestor") || len(subject) < 4 {
		return Result{Stderr: "fatal: fakegit has no fixture for `git " + strings.Join(subject, " ") + "`\n", Exit: 128}
	}
	ancestor, ok := s.resolve(repo, wt, subject[len(subject)-2])
	if !ok {
		return Result{Stderr: "fatal: Not a valid object name " + subject[len(subject)-2] + "\n", Exit: 128}
	}
	descendant, ok := s.resolve(repo, wt, subject[len(subject)-1])
	if !ok {
		return Result{Stderr: "fatal: Not a valid object name " + subject[len(subject)-1] + "\n", Exit: 128}
	}
	if s.reachable(descendant)[ancestor] {
		return Result{}
	}
	return Result{Exit: 1}
}

func revList(s *State, repo *Repo, subject []string) Result {
	format := formatOf(subject)
	rangeSpec := subject[len(subject)-1]
	commits := s.rangeCommits(repo, rangeSpec)
	out := ""
	for _, c := range commits {
		out += s.render(format, c) + "\n"
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
	return Result{Stdout: s.render(format, c) + "\n"}
}

// gitLog answers `git log`. Magit reads the HEAD line, the log section and
// every "unpulled/unpushed" section out of it, always with an explicit
// `--format=`. `--no-walk` prints only the named commits; otherwise the
// commits reachable from the named one are printed newest first, which for a
// fixture world is descending mint order.
func gitLog(s *State, repo *Repo, wt *Worktree, subject []string) Result {
	format := formatOf(subject)
	revs, limit := logRevs(subject)
	if len(revs) == 0 {
		revs = []string{"HEAD"}
	}
	var commits []*Commit
	for _, rev := range revs {
		sha, ok := s.resolve(repo, wt, strings.TrimSuffix(rev, "^{commit}"))
		if !ok {
			return Result{Stderr: fmt.Sprintf("fatal: ambiguous argument '%s': unknown revision or path not in the working tree.\n", rev), Exit: 128}
		}
		if contains(subject, "--no-walk") {
			if c := s.Commits[sha]; c != nil {
				commits = append(commits, c)
			}
			continue
		}
		commits = append(commits, s.history(sha)...)
	}
	if limit > 0 && len(commits) > limit {
		commits = commits[:limit]
	}
	var b strings.Builder
	for _, c := range commits {
		b.WriteString(s.renderCommit(format, c, decorations(repo, wt, c.SHA)) + "\n")
	}
	return Result{Stdout: b.String()}
}

// history is the commits reachable from a sha, newest first, which is the
// order `git log` prints. Fixture commits carry ascending timestamps, and the
// sha breaks a tie so the order is total and deterministic.
func (s *State) history(sha string) []*Commit {
	var out []*Commit
	for reached := range s.reachable(sha) {
		if c := s.Commits[reached]; c != nil {
			out = append(out, c)
		}
	}
	sort.Slice(out, func(i, j int) bool {
		if out[i].AtRFC != out[j].AtRFC {
			return out[i].AtRFC > out[j].AtRFC
		}
		return out[i].SHA > out[j].SHA
	})
	return out
}

// logRevs splits `git log`'s revisions from its options, stopping at the `--`
// that separates revisions from pathspecs, and answers the `-n`/`--max-count`
// cap alongside them.
func logRevs(subject []string) ([]string, int) {
	var revs []string
	limit := 0
	for i := 1; i < len(subject); i++ {
		a := subject[i]
		if a == "--" {
			break
		}
		if n, ok := strings.CutPrefix(a, "--max-count="); ok {
			fmt.Sscanf(n, "%d", &limit)
			continue
		}
		if a == "-n" && i+1 < len(subject) {
			fmt.Sscanf(subject[i+1], "%d", &limit)
			i++
			continue
		}
		if n, ok := strings.CutPrefix(a, "-n"); ok && n != "" && n[0] >= '0' && n[0] <= '9' {
			fmt.Sscanf(n, "%d", &limit)
			continue
		}
		if strings.HasPrefix(a, "-") {
			continue
		}
		revs = append(revs, a)
	}
	return revs, limit
}

func formatOf(subject []string) string {
	for _, a := range subject {
		if f, ok := strings.CutPrefix(a, "--format="); ok {
			return f
		}
		if f, ok := strings.CutPrefix(a, "--pretty=format:"); ok {
			return f
		}
	}
	return "%H"
}

// render substitutes the placeholders the git leaf's commit template uses.
// It carries no ref decorations, which only `git log --decorate` produces.
func (s *State) render(format string, c *Commit) string {
	return s.renderCommit(format, c, "")
}

// renderCommit substitutes git's pretty-format placeholders, including the
// ones magit's own log template uses. `decor` is the `%D` decoration line,
// which only a decorated log has.
func (s *State) renderCommit(format string, c *Commit, decor string) string {
	var b strings.Builder
	for i := 0; i < len(format); i++ {
		if format[i] != '%' || i+1 >= len(format) {
			b.WriteByte(format[i])
			continue
		}
		// `%x<hh>` is git's literal-byte escape; magit separates the fields
		// of one log line with it.
		if format[i+1] == 'x' && i+3 < len(format) {
			var v int
			if _, err := fmt.Sscanf(format[i+2:i+4], "%x", &v); err == nil {
				b.WriteByte(byte(v))
				i += 3
				continue
			}
		}
		token, width := s.commitToken(format[i+1:], c, decor)
		if width == 0 {
			b.WriteByte(format[i])
			continue
		}
		b.WriteString(token)
		i += width
	}
	return b.String()
}

// commitToken answers one placeholder's expansion and how many bytes of the
// format it consumed, or a zero width when the placeholder is not one the fake
// models.
func (s *State) commitToken(rest string, c *Commit, decor string) (string, int) {
	for _, tok := range []struct {
		name  string
		value string
	}{
		{"aN", c.Author},
		{"cN", c.Author},
		{"an", c.Author},
		{"cn", c.Author},
		{"ae", authorEmail},
		{"ce", authorEmail},
		{"aI", c.AtRFC},
		{"cI", c.AtRFC},
		{"ad", c.AtRFC},
		{"cd", c.AtRFC},
		{"at", epoch(c.AtRFC)},
		{"ct", epoch(c.AtRFC)},
		{"H", c.SHA},
		{"h", s.Abbrev(c.SHA)},
		{"P", strings.Join(c.Parents, " ")},
		{"p", s.abbrevAll(c.Parents)},
		{"D", decor},
		{"s", c.Subject},
		{"b", ""},
		{"B", c.Subject + "\n"},
		{"n", "\n"},
		{"%", "%"},
	} {
		if strings.HasPrefix(rest, tok.name) {
			return tok.value, len(tok.name)
		}
	}
	return "", 0
}

// authorEmail is the one address every fixture commit carries.
const authorEmail = "harness@example.com"

// epoch is the seconds-since-the-epoch form of a commit's RFC 3339 time, which
// is what `%at` and `%ct` print.
func epoch(rfc string) string {
	at, err := time.Parse(time.RFC3339, rfc)
	if err != nil {
		return "0"
	}
	return fmt.Sprintf("%d", at.Unix())
}

// decorations is `%D` under `--decorate=full`: every ref pointing at the
// commit, with the checked-out branch prefixed by `HEAD ->`.
func decorations(repo *Repo, wt *Worktree, sha string) string {
	var out []string
	for _, branch := range repo.Branches {
		if repo.BranchHeads[branch] != sha {
			continue
		}
		if wt != nil && wt.Branch == branch {
			out = append(out, "HEAD -> refs/heads/"+branch)
			continue
		}
		out = append(out, "refs/heads/"+branch)
	}
	return strings.Join(out, ", ")
}

// Abbrev is the short sha real git prints for `%h`: seven characters, grown
// until no other commit in the world shares the prefix. Fixture shas differ
// only in their last digits, so a fixed seven would print one ambiguous stem
// for every commit, which real git never does.
func (s *State) Abbrev(sha string) string {
	for n := 7; n < len(sha); n++ {
		if !s.ambiguous(sha[:n], sha) {
			return sha[:n]
		}
	}
	return sha
}

// ambiguous reports whether any commit other than sha starts with prefix.
func (s *State) ambiguous(prefix, sha string) bool {
	for other := range s.Commits {
		if other != sha && strings.HasPrefix(other, prefix) {
			return true
		}
	}
	return false
}

func (s *State) abbrevAll(shas []string) string {
	out := make([]string, 0, len(shas))
	for _, sha := range shas {
		out = append(out, s.Abbrev(sha))
	}
	return strings.Join(out, " ")
}

// FieldSep is the unit separator the git leaf's commit template uses. Tests
// build expectations with it rather than repeating the byte.
const FieldSep = fieldSep

// splitGlobalOptions consumes git's leading global options -- the ones that
// precede the subcommand -- and returns the working directory they select
// plus the subcommand vector. Magit prefixes every call with several
// (`--no-pager --literal-pathspecs -c key=value ...`), and real git accepts
// them in any order before the subcommand.
func splitGlobalOptions(cwd string, args []string) (string, []string) {
	dir := cwd
	i := 0
	for i < len(args) {
		switch a := args[i]; {
		case a == "-C" && i+1 < len(args):
			dir = args[i+1]
			i += 2
		case a == "-c" && i+1 < len(args):
			i += 2
		case a == "--no-pager", a == "-P", a == "--literal-pathspecs",
			a == "--no-optional-locks", a == "--no-replace-objects",
			strings.HasPrefix(a, "--git-dir="), strings.HasPrefix(a, "--work-tree="),
			strings.HasPrefix(a, "-c") && len(a) > 2:
			i++
		default:
			return dir, args[i:]
		}
	}
	return dir, nil
}

// mergeTree answers `merge-tree --write-tree [--no-messages] <base> <other>`:
// the tree merging other into base would record. An other already reachable
// from base changes nothing, so the answer is base's own tree; a base
// reachable from other is a fast-forward to other's tree; anything else is a
// merge of two lines, whose tree is neither side's.
func mergeTree(s *State, repo *Repo, wt *Worktree, subject []string) Result {
	var revs []string
	for _, a := range subject[1:] {
		if !strings.HasPrefix(a, "--") {
			revs = append(revs, a)
		}
	}
	if !contains(subject, "--write-tree") || len(revs) != 2 {
		return Result{Stderr: "fatal: fakegit: `merge-tree` needs --write-tree <base> <other>\n", Exit: 128}
	}
	base, okBase := s.resolve(repo, wt, revs[0])
	other, okOther := s.resolve(repo, wt, revs[1])
	baseCommit, otherCommit := s.Commits[base], s.Commits[other]
	if !okBase || !okOther || baseCommit == nil || otherCommit == nil {
		return Result{Stderr: "fatal: fakegit: merge-tree of an unknown commit\n", Exit: 128}
	}
	switch {
	case s.reachable(base)[other]:
		return Result{Stdout: baseCommit.TreeOf() + "\n"}
	case s.reachable(other)[base]:
		return Result{Stdout: otherCommit.TreeOf() + "\n"}
	}
	return Result{Stdout: "6" + base[1:20] + other[20:] + "\n"}
}

// updateRef answers `update-ref -d refs/heads/<branch> <old>`: git's
// compare-and-delete, refused when the branch no longer points at old.
func updateRef(repo *Repo, subject []string) Result {
	if len(subject) != 4 || subject[1] != "-d" || !strings.HasPrefix(subject[2], "refs/heads/") {
		return Result{Stderr: "fatal: fakegit: only `update-ref -d refs/heads/<branch> <old>` is modeled\n", Exit: 128}
	}
	name := strings.TrimPrefix(subject[2], "refs/heads/")
	head, ok := repo.BranchHeads[name]
	if !ok || !repo.HasBranch(name) {
		return Result{Stderr: fmt.Sprintf("error: cannot lock ref '%s': unable to resolve reference '%s'\n", subject[2], subject[2]), Exit: 128}
	}
	if head != subject[3] {
		return Result{Stderr: fmt.Sprintf("error: cannot lock ref '%s': is at %s but expected %s\n", subject[2], head, subject[3]), Exit: 128}
	}
	repo.RemoveBranch(name)
	return Result{}
}
