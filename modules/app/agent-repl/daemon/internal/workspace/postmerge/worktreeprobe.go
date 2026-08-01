package postmerge

import (
	"bytes"
	"context"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"

	"claude-repld/internal/dlog"
)

// GitWorktreeProbe answers postmerge.WorktreeProbe from git itself.
//
// THE TEST IS git-dir VERSUS git-common-dir. Every worktree of a repository
// shares one COMMON git dir; only the main worktree's own git dir IS that
// common dir. A linked worktree's git dir is a per-worktree subdirectory of it
// (.git/worktrees/<name>). Comparing the two is therefore an exact answer to
// "is this checkout a linked worktree", with no path conventions, no naming
// assumptions, and no list to parse.
//
// It is deliberately the same fact merge.GitRepoKeyer derives its queue key
// from (git-common-dir), read from the other side: the keyer asks WHICH
// repository, this asks WHICH KIND of worktree within it.
type GitWorktreeProbe struct {
	logf dlog.Logf
}

var _ WorktreeProbe = (*GitWorktreeProbe)(nil)

// NewGitWorktreeProbe validates its dependency and returns the probe. A nil
// Logf is a hard construction error: an unlogged probe makes a phone-home that
// silently never fires undiagnosable from the shared log.
func NewGitWorktreeProbe(logf dlog.Logf) (*GitWorktreeProbe, error) {
	if logf == nil {
		return nil, fmt.Errorf("postmerge: GitWorktreeProbe needs a Logf")
	}
	return &GitWorktreeProbe{logf: logf}, nil
}

// IsLinkedWorktree implements postmerge.WorktreeProbe.
//
// Every failure is surfaced as an error and never as a guessed answer. Guessing
// "not linked" would silently swallow the parent handoff; guessing "linked"
// would prompt a workspace that is really the repository trunk.
func (p *GitWorktreeProbe) IsLinkedWorktree(ctx context.Context, dir string) (bool, error) {
	if dir == "" {
		p.logf("postmerge: worktree probe REFUSED an empty directory")
		return false, fmt.Errorf("postmerge: IsLinkedWorktree needs a worktree directory")
	}
	gitDir, err := gitCapture(ctx, dir, "rev-parse", "--absolute-git-dir")
	if err != nil {
		p.logf("postmerge: worktree probe git-dir FAILED {dir=%s}: %v", dir, err)
		return false, fmt.Errorf("postmerge: resolve git dir for %s: %w", dir, err)
	}
	common, err := gitCapture(ctx, dir, "rev-parse", "--git-common-dir")
	if err != nil {
		p.logf("postmerge: worktree probe git-common-dir FAILED {dir=%s}: %v", dir, err)
		return false, fmt.Errorf("postmerge: resolve common git dir for %s: %w", dir, err)
	}
	// The main worktree answers --git-common-dir with the relative ".git"; a
	// linked worktree answers with an absolute path. Resolve against the
	// worktree so both land on the same absolute location.
	if !filepath.IsAbs(common) {
		common = filepath.Join(dir, common)
	}
	// EvalSymlinks canonicalizes the two spellings macOS reports for one
	// directory (/var vs /private/var). Without it the main worktree's own
	// pair can differ textually and read as linked.
	canonicalGitDir, err := canonicalize(gitDir)
	if err != nil {
		p.logf("postmerge: worktree probe canonicalize git-dir FAILED {dir=%s git_dir=%s}: %v", dir, gitDir, err)
		return false, fmt.Errorf("postmerge: canonicalize git dir %s for %s: %w", gitDir, dir, err)
	}
	canonicalCommon, err := canonicalize(common)
	if err != nil {
		p.logf("postmerge: worktree probe canonicalize git-common-dir FAILED {dir=%s common=%s}: %v", dir, common, err)
		return false, fmt.Errorf("postmerge: canonicalize common git dir %s for %s: %w", common, dir, err)
	}
	linked := canonicalGitDir != canonicalCommon
	p.logf("postmerge: worktree probe {dir=%s git_dir=%s common_dir=%s linked=%t}", dir, canonicalGitDir, canonicalCommon, linked)
	return linked, nil
}

func canonicalize(path string) (string, error) {
	resolved, err := filepath.EvalSymlinks(path)
	if err != nil {
		return "", err
	}
	return filepath.Clean(resolved), nil
}

// gitCapture runs `git -C dir args...` and returns trimmed stdout, treating a
// non-zero exit as an error.
func gitCapture(ctx context.Context, dir string, args ...string) (string, error) {
	cmd := gitCmd(ctx, dir, args...)
	var out, errb bytes.Buffer
	cmd.Stdout = &out
	cmd.Stderr = &errb
	if err := cmd.Run(); err != nil {
		return "", fmt.Errorf("postmerge: git %v in %s: %w (stderr: %s)", args, dir, err, dlog.Clamp(errb.String(), 400))
	}
	return strings.TrimSpace(out.String()), nil
}

// gitCmd builds `git -C dir args...` with the inherited repository bindings
// STRIPPED.
//
// A leaked GIT_DIR is not a cosmetic problem here: it would make every probed
// directory report the LEAKING repository's git dir, so a linked worktree and
// the trunk would answer identically and the parent handoff would fire (or not
// fire) for the wrong reason entirely.
func gitCmd(ctx context.Context, dir string, args ...string) *exec.Cmd {
	full := append([]string{"-C", dir}, args...)
	cmd := exec.CommandContext(ctx, "git", full...)
	env := os.Environ()
	kept := env[:0]
	for _, entry := range env {
		switch {
		case strings.HasPrefix(entry, "GIT_DIR="),
			strings.HasPrefix(entry, "GIT_INDEX_FILE="),
			strings.HasPrefix(entry, "GIT_WORK_TREE="),
			strings.HasPrefix(entry, "GIT_OBJECT_DIRECTORY="),
			strings.HasPrefix(entry, "GIT_COMMON_DIR="),
			strings.HasPrefix(entry, "GIT_PREFIX="):
		default:
			kept = append(kept, entry)
		}
	}
	cmd.Env = kept
	return cmd
}
