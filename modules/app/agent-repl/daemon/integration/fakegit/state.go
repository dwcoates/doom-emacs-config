// Package fakegit is the integration suite's scripted `git` executable and the
// fixture model behind it.
//
// WHY. The user directive is absolute: git is NEVER called during testing. The
// integration suite still needs a daemon that believes it is looking at real
// repositories, so this package supplies a `git` binary that is placed first on
// the daemon's PATH and answers every command the daemon's git leaf issues out
// of a FIXTURE TABLE held in one JSON file. Repositories are fake directories:
// a tree with a `.git` marker in it and a row in the fixture file. No `git
// init` runs anywhere, and no real git binary is ever reached.
//
// The fixture file is the whole conversation surface between a test and the
// fake: the test writes repositories, branches, commits, scripted conflicts and
// scripted dirty trees into it; the fake reads it on every invocation, applies
// the mutation the command asks for, and writes it back under an exclusive
// lock so two concurrent daemon spawns cannot lose an update.
package fakegit

import (
	"encoding/json"
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"sort"
	"strings"
	"syscall"
	"time"
)

// EnvStateFile names the fixture file the fake reads and writes. The daemon
// carries it into every child it spawns, so a shim-side git would see the same
// world (nothing spawns one today).
const EnvStateFile = "FAKEGIT_STATE"

// Commit is one commit in the fake world.
type Commit struct {
	SHA     string `json:"sha"`
	Subject string `json:"subject"`
	Author  string `json:"author"`
	AtRFC   string `json:"at"`
	// Parents are the commit's parents, first-parent first. A merge commit has
	// two, which is what the landed-range walk reads.
	Parents []string `json:"parents"`
	// Paths are the files the commit touched, the answer `diff --name-only`
	// gives for any range that includes it.
	Paths []string `json:"paths"`
	// Tree is the tree the commit records. Empty is a tree of its own,
	// distinct from every other commit's (TreeOf); a test sets it to script a
	// squash, whose one commit records exactly what merging a branch would.
	Tree string `json:"tree,omitempty"`
}

// TreeOf answers the tree a commit records.
func (c *Commit) TreeOf() string {
	if c.Tree != "" {
		return c.Tree
	}
	return "7" + c.SHA[1:]
}

// Worktree is one checked-out tree of a repository.
type Worktree struct {
	Dir    string `json:"dir"`
	Branch string `json:"branch"`
	Head   string `json:"head"`
	// Conflicted are the paths a scripted conflict left in the index.
	Conflicted []string `json:"conflicted"`
	// Dirty makes `status --porcelain` report content.
	Dirty bool `json:"dirty"`
	// Files are the tracked paths, relative to Dir, that `ls-files` answers.
	// Emacs's projectile and magit list a worktree the moment it is visited.
	Files []string `json:"files"`
	// Locked makes `worktree list` report the tree locked.
	Locked bool `json:"locked,omitempty"`
	// Rebase is the rebase standing in the tree, nil when none is.
	Rebase *Rebase `json:"rebase,omitempty"`
}

// Rebase is one interactive rebase in progress: the commits it replays onto
// Onto, how many it has replayed, and the head the replays have built.
type Rebase struct {
	Onto    string   `json:"onto"`
	Todo    []string `json:"todo"`
	Done    int      `json:"done"`
	NewHead string   `json:"new_head"`
	// Stopped reports that the current commit stopped on a conflict.
	Stopped bool `json:"stopped,omitempty"`
}

// Repo is one fake repository: a common dir every one of its worktrees reports.
type Repo struct {
	Dir           string   `json:"dir"`
	CommonDir     string   `json:"common_dir"`
	DefaultBranch string   `json:"default_branch"`
	OriginHead    string   `json:"origin_head"`
	Branches      []string `json:"branches"`
	// BranchHeads maps a branch to the commit it points at.
	BranchHeads map[string]string `json:"branch_heads"`
	// RemoteHeads maps a branch of the `origin` remote to the commit a fetch
	// brings in; `refs/remotes/origin/<branch>` resolves to it.
	RemoteHeads map[string]string `json:"remote_heads,omitempty"`
	Worktrees   []*Worktree       `json:"worktrees"`
}

// Conflict scripts a conflict between Branch and the repository Dir belongs
// to: a `merge --no-ff` of Branch leaves these paths conflicted instead of
// landing. It STANDS WHILE THE BRANCH DOES NOT MOVE -- merging the same two
// histories again conflicts again, as it would in git -- and is gone once
// the branch's head moved (its author resolved it on the branch).
type Conflict struct {
	Dir    string   `json:"dir"`
	Branch string   `json:"branch"`
	Paths  []string `json:"paths"`
	// SourceHead is the branch head the conflict was first met at, empty
	// until then.
	SourceHead string `json:"source_head,omitempty"`
	// RebaseCommit, when set, makes this a REBASE conflict instead: replaying
	// the branch's commit at that 1-based place stops on Paths, once.
	RebaseCommit int `json:"rebase_commit,omitempty"`
	// Met reports that a rebase conflict has stopped a replay already.
	Met bool `json:"met,omitempty"`
}

// Failure scripts one command failing: the next invocation whose subject starts
// with Match, in Dir when Dir is set, exits nonzero with Stderr.
type Failure struct {
	Dir    string   `json:"dir"`
	Match  []string `json:"match"`
	Stderr string   `json:"stderr"`
	Exit   int      `json:"exit"`
}

// Call is one recorded invocation, so a test can assert what the daemon ran.
type Call struct {
	Args []string `json:"args"`
	Cwd  string   `json:"cwd"`
	// At is when the fake answered this invocation. Every caller shares one
	// exclusive lock over the fixture file, so a run's git conversation is a
	// single ordered timeline and the GAPS in it are the only place a
	// scenario's own cost can be told apart from time spent waiting behind
	// somebody else's git. A bound derived from a run without it would be a
	// guess about which of the two was paying.
	At time.Time `json:"at"`
	// Exit, Stdout and Stderr are what the fake ANSWERED. Recorded because a
	// caller's next move is a reaction to the answer, and a conversation that
	// omits it cannot explain the reaction. Both streams are clipped; see
	// `recordedOutputLimit`.
	Exit   int    `json:"exit"`
	Stdout string `json:"stdout,omitempty"`
	Stderr string `json:"stderr,omitempty"`
}

// State is the whole fake world.
type State struct {
	Repos     []*Repo            `json:"repos"`
	Commits   map[string]*Commit `json:"commits"`
	Conflicts []*Conflict        `json:"conflicts"`
	Failures  []*Failure         `json:"failures"`
	Calls     []Call             `json:"calls"`
	// Seq mints deterministic commit shas.
	Seq int `json:"seq"`
}

// NewState is an empty world.
func NewState() *State {
	return &State{Commits: map[string]*Commit{}}
}

// Load reads the fixture file. A missing file is an empty world, so a test that
// never scripted anything still gets loud "not a git repository" answers rather
// than a crash.
func Load(path string) (*State, error) {
	body, err := os.ReadFile(path)
	if errors.Is(err, os.ErrNotExist) {
		return NewState(), nil
	}
	if err != nil {
		return nil, fmt.Errorf("fakegit: read %s: %w", path, err)
	}
	if len(body) == 0 {
		return NewState(), nil
	}
	s := NewState()
	if err := json.Unmarshal(body, s); err != nil {
		return nil, fmt.Errorf("fakegit: %s is malformed: %w", path, err)
	}
	if s.Commits == nil {
		s.Commits = map[string]*Commit{}
	}
	return s, nil
}

// Save writes the fixture file back.
func Save(path string, s *State) error {
	body, err := json.MarshalIndent(s, "", "  ")
	if err != nil {
		return fmt.Errorf("fakegit: encode state: %w", err)
	}
	if err := os.MkdirAll(filepath.Dir(path), 0o755); err != nil {
		return fmt.Errorf("fakegit: mkdir %s: %w", filepath.Dir(path), err)
	}
	if err := os.WriteFile(path, body, 0o644); err != nil {
		return fmt.Errorf("fakegit: write %s: %w", path, err)
	}
	return nil
}

// LoadLocked reads the fixture file under the SAME lock every write takes.
// Save truncates and rewrites in place, so an unlocked reader can see an empty
// or half-written file and read it as a world with no repositories at all.
func LoadLocked(path string) (*State, error) {
	lockPath := path + ".lock"
	if err := os.MkdirAll(filepath.Dir(lockPath), 0o755); err != nil {
		return nil, fmt.Errorf("fakegit: mkdir %s: %w", filepath.Dir(lockPath), err)
	}
	f, err := os.OpenFile(lockPath, os.O_CREATE|os.O_RDWR, 0o644)
	if err != nil {
		return nil, fmt.Errorf("fakegit: open %s: %w", lockPath, err)
	}
	defer f.Close()
	if err := syscall.Flock(int(f.Fd()), syscall.LOCK_EX); err != nil {
		return nil, fmt.Errorf("fakegit: lock %s: %w", lockPath, err)
	}
	defer syscall.Flock(int(f.Fd()), syscall.LOCK_UN)
	return Load(path)
}

// WithLock runs fn against the fixture file under an exclusive kernel lock, so
// a concurrent invocation can never lose an update. The lock is a sibling file
// rather than the state file itself, so a truncating write can never race the
// lock's own descriptor.
func WithLock(path string, fn func(*State) error) error {
	lockPath := path + ".lock"
	if err := os.MkdirAll(filepath.Dir(lockPath), 0o755); err != nil {
		return fmt.Errorf("fakegit: mkdir %s: %w", filepath.Dir(lockPath), err)
	}
	f, err := os.OpenFile(lockPath, os.O_CREATE|os.O_RDWR, 0o644)
	if err != nil {
		return fmt.Errorf("fakegit: open %s: %w", lockPath, err)
	}
	defer f.Close()
	if err := syscall.Flock(int(f.Fd()), syscall.LOCK_EX); err != nil {
		return fmt.Errorf("fakegit: lock %s: %w", lockPath, err)
	}
	defer syscall.Flock(int(f.Fd()), syscall.LOCK_UN)

	state, err := Load(path)
	if err != nil {
		return err
	}
	if err := fn(state); err != nil {
		return err
	}
	return Save(path, state)
}

// Canon canonicalizes a path the way the daemon's git leaf does, so a
// repository reached through /tmp and through /private/tmp is one repository.
func Canon(path string) string {
	abs, err := filepath.Abs(path)
	if err != nil {
		return filepath.Clean(path)
	}
	if resolved, err := filepath.EvalSymlinks(abs); err == nil {
		return filepath.Clean(resolved)
	}
	return filepath.Clean(abs)
}

// FindWorktree answers the worktree a directory is inside, and its repository.
// A directory that is not under any registered tree has no repository, which is
// how the fake answers "not a git repository".
func (s *State) FindWorktree(dir string) (*Repo, *Worktree) {
	want := Canon(dir)
	var (
		bestRepo *Repo
		bestWt   *Worktree
		bestLen  int
	)
	for _, repo := range s.Repos {
		for _, wt := range repo.Worktrees {
			have := Canon(wt.Dir)
			if want != have && !strings.HasPrefix(want, have+string(filepath.Separator)) {
				continue
			}
			if len(have) > bestLen {
				bestRepo, bestWt, bestLen = repo, wt, len(have)
			}
		}
	}
	return bestRepo, bestWt
}

// Repo answers a repository by any of its worktree directories.
func (s *State) Repo(dir string) *Repo {
	repo, _ := s.FindWorktree(dir)
	return repo
}

// MintSHA answers the next deterministic commit sha. The sequence number is
// scrambled into the LEADING digits rather than the trailing ones, because
// real git abbreviates a sha to the shortest unique prefix and magit prints
// that: shas differing only in their last digit would each abbreviate to the
// full forty characters, which no real repository ever shows.
func (s *State) MintSHA() string {
	s.Seq++
	// Knuth's multiplicative constant, taken modulo 2^32 by the cast, keeps
	// consecutive sequence numbers far apart in the leading digits.
	return fmt.Sprintf("%08x%032x", uint32(s.Seq)*2654435761, s.Seq)
}

// AddCommit records a commit and points a branch at it.
func (s *State) AddCommit(repo *Repo, branch, subject string, parents, paths []string) *Commit {
	c := &Commit{
		SHA:     s.MintSHA(),
		Subject: subject,
		Author:  "Integration Harness",
		AtRFC:   time.Unix(1_700_000_000+int64(s.Seq), 0).UTC().Format(time.RFC3339),
		Parents: parents,
		Paths:   paths,
	}
	s.Commits[c.SHA] = c
	if repo != nil && branch != "" {
		if repo.BranchHeads == nil {
			repo.BranchHeads = map[string]string{}
		}
		repo.BranchHeads[branch] = c.SHA
		for _, wt := range repo.Worktrees {
			if wt.Branch == branch {
				wt.Head = c.SHA
			}
		}
	}
	return c
}

// HasBranch reports whether a local branch exists.
func (r *Repo) HasBranch(name string) bool {
	for _, b := range r.Branches {
		if b == name {
			return true
		}
	}
	return false
}

// AddBranch adds a branch pointing at a commit.
func (r *Repo) AddBranch(name, head string) {
	if r.BranchHeads == nil {
		r.BranchHeads = map[string]string{}
	}
	if !r.HasBranch(name) {
		r.Branches = append(r.Branches, name)
		sort.Strings(r.Branches)
	}
	r.BranchHeads[name] = head
}

// RemoveBranch drops a branch.
func (r *Repo) RemoveBranch(name string) {
	out := r.Branches[:0]
	for _, b := range r.Branches {
		if b != name {
			out = append(out, b)
		}
	}
	r.Branches = out
	delete(r.BranchHeads, name)
}

// Worktree answers a registered worktree by directory.
func (r *Repo) Worktree(dir string) *Worktree {
	want := Canon(dir)
	for _, wt := range r.Worktrees {
		if Canon(wt.Dir) == want {
			return wt
		}
	}
	return nil
}

// RemoveWorktree drops a worktree registration.
func (r *Repo) RemoveWorktree(dir string) {
	want := Canon(dir)
	out := r.Worktrees[:0]
	for _, wt := range r.Worktrees {
		if Canon(wt.Dir) != want {
			out = append(out, wt)
		}
	}
	r.Worktrees = out
}
