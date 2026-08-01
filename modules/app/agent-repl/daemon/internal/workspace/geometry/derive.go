package geometry

import (
	"bytes"
	"context"
	"fmt"
	"os"
	"path/filepath"
	"strings"

	"claude-repld/internal/dlog"
	"claude-repld/internal/gitexec"
)

// Deriver reconstructs a workspace's merge geometry from GIT FACTS, for
// workspaces that predate the daemon owning the map. It is used by the boot
// backfill and by nothing else: a workspace the daemon created has its geometry
// recorded as an observed fact, and a derived answer must never displace one.
//
// The three derivation rules, each a single git fact and no inference:
//
//   - SourceDir    — the workspace directory itself, which must be a real
//     worktree of some repository.
//   - SourceBranch — the branch that worktree has CHECKED OUT
//     (`symbolic-ref --short HEAD`). A detached HEAD has no branch, so there is
//     nothing to cherry-pick a range from and derivation fails loudly.
//   - TargetDir    — the repository's MAIN worktree, which git reports as the
//     first entry of `worktree list --porcelain`. It is the only defensible
//     answer available after the fact: git records which repository a linked
//     worktree belongs to, never which sibling worktree a human spawned it from.
//
// Every failure is an error. Nothing here ever returns a partial or invented
// coordinate, because an invented target is a repository that gets written to.
type Deriver struct {
	logf dlog.Logf
}

// NewDeriver validates its dependency and returns the deriver.
func NewDeriver(logf dlog.Logf) (*Deriver, error) {
	if logf == nil {
		return nil, fmt.Errorf("geometry: Deriver needs a Logf")
	}
	return &Deriver{logf: logf}, nil
}

// Derive returns the backfilled geometry for the workspace at dir.
func (d *Deriver) Derive(ctx context.Context, workspace string) (Record, error) {
	key := Key(workspace)
	if key == "" {
		d.logf("geometry: derive REFUSED empty workspace")
		return Record{}, fmt.Errorf("geometry: derive needs a workspace directory")
	}
	info, err := os.Stat(key)
	if err != nil {
		d.logf("geometry: derive FAILED {workspace=%s stage=stat}: %v", key, err)
		return Record{}, fmt.Errorf("geometry: derive %s: %w", key, err)
	}
	if !info.IsDir() {
		d.logf("geometry: derive FAILED {workspace=%s stage=stat}: not a directory", key)
		return Record{}, fmt.Errorf("geometry: derive %s: not a directory", key)
	}
	branch, err := gitCapture(ctx, key, "symbolic-ref", "--quiet", "--short", "HEAD")
	if err != nil {
		// The loudest case this covers: a detached HEAD, where symbolic-ref
		// exits non-zero. Such a workspace has no branch to cherry-pick from,
		// so it gets NO record and its merge is refused with an explanation.
		d.logf("geometry: derive FAILED {workspace=%s stage=branch} — no checked-out branch (detached HEAD or not a worktree): %v", key, err)
		return Record{}, fmt.Errorf("geometry: derive %s: resolve checked-out branch: %w", key, err)
	}
	if branch == "" {
		d.logf("geometry: derive FAILED {workspace=%s stage=branch} — git reported an empty branch name", key)
		return Record{}, fmt.Errorf("geometry: derive %s: git reported an empty checked-out branch", key)
	}
	listing, err := gitCapture(ctx, key, "worktree", "list", "--porcelain")
	if err != nil {
		d.logf("geometry: derive FAILED {workspace=%s stage=worktree-list}: %v", key, err)
		return Record{}, fmt.Errorf("geometry: derive %s: list worktrees: %w", key, err)
	}
	main := mainWorktree(listing)
	if main == "" {
		d.logf("geometry: derive FAILED {workspace=%s stage=worktree-list} — no worktree entries in %q", key, dlog.Clamp(listing, 400))
		return Record{}, fmt.Errorf("geometry: derive %s: git listed no worktrees", key)
	}
	// Both directories are canonicalized before they are compared or recorded.
	// macOS reports the same directory as /var/... and /private/var/..., and
	// git answers in the resolved spelling while the daemon's workspace key
	// carries the unresolved one. Comparing the two spellings lexically would
	// declare the repository's OWN main worktree a mergeable workspace and
	// record a cherry-pick of a branch into itself.
	source, err := canonical(key)
	if err != nil {
		d.logf("geometry: derive FAILED {workspace=%s stage=canonicalize-source}: %v", key, err)
		return Record{}, fmt.Errorf("geometry: derive %s: %w", key, err)
	}
	target, err := canonical(main)
	if err != nil {
		d.logf("geometry: derive FAILED {workspace=%s stage=canonicalize-target target=%s}: %v", key, main, err)
		return Record{}, fmt.Errorf("geometry: derive %s: %w", key, err)
	}
	rec := Record{Workspace: key, SourceBranch: branch, SourceDir: source, TargetDir: target, Origin: OriginBackfilled}
	if err := rec.validate(); err != nil {
		// The live case: the workspace IS the repository's main worktree, so
		// source and target coincide. It has no merge geometry at all.
		d.logf("geometry: derive FAILED {workspace=%s stage=validate branch=%q source=%s target=%s}: %v", key, branch, source, target, err)
		return Record{}, fmt.Errorf("geometry: derive %s: %w", key, err)
	}
	d.logf("geometry: DERIVED {workspace=%s branch=%q source=%s target=%s}", key, branch, source, target)
	return rec, nil
}

// canonical resolves a directory to its symlink-free absolute spelling, the
// same normalization merge.GitRepoKeyer applies for the same reason. A path
// that cannot be resolved is an error, never a lexical guess.
func canonical(dir string) (string, error) {
	resolved, err := filepath.EvalSymlinks(dir)
	if err != nil {
		return "", fmt.Errorf("canonicalize %s: %w", dir, err)
	}
	return Key(resolved), nil
}

// mainWorktree returns the first `worktree ` path of a
// `git worktree list --porcelain` listing. Git documents the main worktree as
// the first entry, which is exactly the coordinate a cherry-pick lands in when
// nothing else recorded a parent.
func mainWorktree(listing string) string {
	for _, line := range strings.Split(listing, "\n") {
		if rest, ok := strings.CutPrefix(strings.TrimRight(line, "\r"), "worktree "); ok {
			return Key(rest)
		}
	}
	return ""
}

// gitCapture runs `git -C dir args...` and returns trimmed stdout, treating a
// non-zero exit as an error. The repository is ALWAYS the `-C dir` argument:
// gitexec.Command strips the inherited bindings that would otherwise make every
// workspace derive the LEAKING repository's branch and main worktree, and the
// backfill would then record a target that cherry-picks into the wrong checkout.
func gitCapture(ctx context.Context, dir string, args ...string) (string, error) {
	cmd := gitexec.Command(ctx, dir, args...)
	var out, errb bytes.Buffer
	cmd.Stdout = &out
	cmd.Stderr = &errb
	if err := cmd.Run(); err != nil {
		return "", fmt.Errorf("geometry: git %v in %s: %w (stderr: %s)", args, dir, err, dlog.Clamp(errb.String(), 400))
	}
	return strings.TrimSpace(out.String()), nil
}
