package workspace

import (
	"context"
	"fmt"
	"os"
	"path/filepath"

	"claude-repld/internal/dlog"
	"claude-repld/internal/wsm"
)

// opRegisterRepository is the operation every record this verb writes is filed
// under. It is its own operation rather than opRegister's: a repository
// registered on its own has no workspace, so its records cannot be read out of
// a workspace's sink, and a reader asking "what repositories were registered"
// wants exactly these lines.
const opRegisterRepository = "daemon.repository.register"

// RegisterRepository records a repository ON ITS OWN, resolved from ANY path
// inside it, with no workspace under it.
//
// THE PATH IS ANY PATH, a file as readily as a directory, because that is what
// the caller has: the editor's gesture is "pick a file". A file's directory is
// what git is asked about; a directory is asked about as it stands.
//
// THE REPOSITORY IS ITS MAIN WORKTREE, exactly as registration derives it for
// a workspace: workspace.v1's RepositoryRef.dir is "the repository's normalized
// main-worktree directory", and the two must agree or one repository would
// have two rows. So the resolution goes through the git client and the answer
// is normalized the same way.
//
// IDEMPOTENT BY THE RESOLVED DIR, and the bool says which it was: a repository
// the registry already holds is answered with its own ref and `true`, which is
// an answer and not a refusal.
func (v *verbs) RegisterRepository(ctx context.Context, path string) (wsm.Repository, bool, error) {
	global := v.deps.Log.Global().With(dlog.Context{"path": path})

	dir, err := repositoryProbeDir(path)
	if err != nil {
		return wsm.Repository{}, false, refuse(global, "RegisterRepository", ArmUnreadablePath,
			fmt.Sprintf("%q cannot be read: %v", path, err), false)
	}
	// THE PROBE, NOT THE RESOLUTION. A path outside every repository is the
	// answer this question was asked to get -- somebody picked the wrong file
	// -- so it must not be a fault in the log. gitclient.RepositoryOf answers
	// it as an ordinary false; MainWorktree, which records a refusal at error,
	// is for a directory already established as a worktree.
	main, inRepository, err := v.deps.Git.RepositoryOf(ctx, dir)
	if err != nil {
		global.Error(opRegisterRepository, "git could not be asked what repository the path is in", dlog.Context{
			"probe_dir": dir, "cause": err.Error(),
		})
		return wsm.Repository{}, false, fmt.Errorf("register repository %q: %w", path, err)
	}
	if !inRepository {
		return wsm.Repository{}, false, refuse(global, "RegisterRepository", ArmNotInARepository,
			fmt.Sprintf("%q is not inside a git repository with a main worktree", path), false)
	}
	normalized, err := normalizeDir(main)
	if err != nil {
		global.Error(opRegisterRepository, "the resolved main worktree cannot be normalized", dlog.Context{
			"main_worktree": main, "cause": err.Error(),
		})
		return wsm.Repository{}, false, fmt.Errorf("register repository %q: %w", path, err)
	}
	// The default branch is READ OFF GIT the way registration reads it, because
	// a repository row with no default branch is a merge target nobody can
	// resolve later, and the announcing caller is the only party that can look
	// it up.
	defaultBranch, err := v.deps.Git.DefaultBranch(ctx, normalized)
	if err != nil {
		global.Error(opRegisterRepository, "could not resolve the repository default branch", dlog.Context{
			"dir": normalized, "cause": err.Error(),
		})
		return wsm.Repository{}, false, fmt.Errorf("register repository %q: default branch: %w", path, err)
	}

	record, created, err := v.deps.DB.RegisterRepository(ctx, normalized, defaultBranch)
	if err != nil {
		global.Error(opRegisterRepository, "could not record the repository", dlog.Context{
			"dir": normalized, "cause": err.Error(),
		})
		return wsm.Repository{}, false, fmt.Errorf("register repository %q: %w", path, err)
	}
	global.Info(opRegisterRepository, "registered a repository", dlog.Context{
		"path": path, "dir": record.Dir, "repository": string(record.ID),
		"already_known": !created, "default_branch": record.DefaultBranch,
	})
	// THE ROSTER IS REPUBLISHED even though no workspace changed: the new
	// repository draws an EMPTY SECTION, and a registration the sidebar does
	// not show is a registration the user cannot act on.
	v.republishRegistry(ctx, global, opRegisterRepository)
	return record, !created, nil
}

// repositoryProbeDir answers the directory git is asked about for PATH: the
// path itself when it is a directory, its parent when it is a file. A path
// that cannot be stat'ed at all has no probe directory, which is the
// unreadable_path refusal.
func repositoryProbeDir(path string) (string, error) {
	if path == "" {
		return "", fmt.Errorf("a path is required")
	}
	abs, err := filepath.Abs(path)
	if err != nil {
		return "", err
	}
	info, err := os.Stat(abs)
	if err != nil {
		return "", err
	}
	if info.IsDir() {
		return abs, nil
	}
	return filepath.Dir(abs), nil
}
