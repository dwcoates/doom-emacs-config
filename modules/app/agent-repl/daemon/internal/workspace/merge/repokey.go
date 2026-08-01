package merge

import (
	"bytes"
	"context"
	"fmt"
	"path/filepath"
	"strings"

	"claude-repld/internal/dlog"
	"claude-repld/internal/gitexec"
)

// GitRepoKeyer resolves a worktree directory to its REPOSITORY identity, which
// is the key merge.Queue files a request under and the unit merge.Coordinator
// owns.
//
// The identity is `git -C <dir> rev-parse --git-common-dir` resolved to an
// absolute, symlink-free path. That is precisely the property the queue needs:
// every linked worktree of one repository shares one common git dir, so sibling
// worktrees collapse to a single key while unrelated repositories never
// collide. Deriving the key from the worktree path instead (a prefix, a
// basename, the toplevel) would give each sibling its own key and hand two
// cherry-picks the same target worktree — the race this key exists to make
// unrepresentable.
type GitRepoKeyer struct {
	logf dlog.Logf
}

var _ RepoKeyer = (*GitRepoKeyer)(nil)

// NewGitRepoKeyer validates its dependency and returns the keyer. A nil Logf is
// a hard construction error: a key resolution that cannot be logged makes an
// unexplainable queue split undiagnosable from the shared log.
func NewGitRepoKeyer(logf dlog.Logf) (*GitRepoKeyer, error) {
	if logf == nil {
		return nil, fmt.Errorf("merge: GitRepoKeyer needs a Logf")
	}
	return &GitRepoKeyer{logf: logf}, nil
}

// RepoKey implements merge.RepoKeyer.
//
// Every failure is surfaced as an error and never as a synthesized key: a
// guessed key would silently split one repository's queue in two and reinstate
// the concurrent-cherry-pick race.
func (k *GitRepoKeyer) RepoKey(ctx context.Context, worktreeDir string) (string, error) {
	if worktreeDir == "" {
		k.logf("merge: repo-key REFUSED empty worktree dir")
		return "", fmt.Errorf("merge: RepoKey needs a worktree directory")
	}
	common, err := gitCapture(ctx, worktreeDir, "rev-parse", "--git-common-dir")
	if err != nil {
		k.logf("merge: repo-key git FAILED {dir=%s}: %v", worktreeDir, err)
		return "", fmt.Errorf("merge: resolve repo key for %s: %w", worktreeDir, err)
	}
	if common == "" {
		k.logf("merge: repo-key EMPTY git-common-dir {dir=%s}", worktreeDir)
		return "", fmt.Errorf("merge: rev-parse --git-common-dir returned nothing for %s", worktreeDir)
	}
	// The main worktree answers with the relative ".git"; a linked worktree
	// answers with an absolute path. Resolve against the worktree so both land
	// on the same absolute location.
	if !filepath.IsAbs(common) {
		common = filepath.Join(worktreeDir, common)
	}
	// EvalSymlinks canonicalizes the pair of paths macOS reports for the same
	// directory (/var vs /private/var). Without it two siblings can resolve to
	// two spellings of one git dir and get two queues.
	resolved, err := filepath.EvalSymlinks(common)
	if err != nil {
		k.logf("merge: repo-key canonicalize FAILED {dir=%s common=%s}: %v", worktreeDir, common, err)
		return "", fmt.Errorf("merge: canonicalize repo key %s for %s: %w", common, worktreeDir, err)
	}
	key := filepath.Clean(resolved)
	k.logf("merge: repo-key {dir=%s key=%s}", worktreeDir, key)
	return key, nil
}

// gitCapture runs `git -C dir args...` and returns trimmed stdout, treating a
// non-zero exit as an error. It is the read-only probe half of the same git
// convention merge.Driver.gitString uses; the two are kept separate only
// because merge.Driver's is a method bound to that type's logger.
func gitCapture(ctx context.Context, dir string, args ...string) (string, error) {
	// gitexec.Command strips inherited repository bindings (GIT_DIR and
	// friends), which matters doubly here: with a leaked GIT_DIR every worktree
	// would resolve to the LEAKING repository's key, collapsing distinct repos
	// onto one queue.
	cmd := gitexec.Command(ctx, dir, args...)
	var out, errb bytes.Buffer
	cmd.Stdout = &out
	cmd.Stderr = &errb
	if err := cmd.Run(); err != nil {
		return "", fmt.Errorf("merge: git %v in %s: %w (stderr: %s)", args, dir, err, dlog.Clamp(errb.String(), 400))
	}
	return strings.TrimSpace(out.String()), nil
}
