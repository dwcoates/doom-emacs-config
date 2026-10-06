package workspace

import (
	"context"
	"fmt"
	"os"
	"path/filepath"

	"claude-repld/internal/dirpath"
	"claude-repld/internal/dlog"
	"claude-repld/internal/wsm"
)

// opRegisterRepository is the operation every record this verb writes is filed
// under. It is its own operation rather than opRegister's: a repository
// registered on its own has no workspace, so its records cannot be read out of
// a workspace's sink, and a reader asking "what repositories were registered"
// wants exactly these lines.
const opRegisterRepository = "daemon.repository.register"

// RegisteredRepository is what RegisterRepository answers: the repository, and
// the MAIN-WORKTREE WORKSPACE registered under it.
//
// The two already-known bools are INDEPENDENT and neither implies the other. A
// repository minted by an earlier RegisterWorkspace is already known while its
// workspace is too; a repository registered before the workspace half of this
// verb existed is already known while its workspace is freshly minted.
type RegisteredRepository struct {
	// Repository is the repository's record, keyed by its main-worktree dir.
	Repository wsm.Repository
	// RepositoryAlreadyKnown reports that the registry already held the
	// repository, so nothing was minted for it.
	RepositoryAlreadyKnown bool
	// Workspace is the main worktree, registered as an open workspace through
	// the SAME registration RegisterWorkspace runs. Always set on success.
	Workspace wsm.Workspace
	// WorkspaceAlreadyKnown reports that the registry already held a workspace
	// for the main-worktree dir, so this call adopted it.
	WorkspaceAlreadyKnown bool
}

// RegisterRepository records a repository resolved from ANY path inside it,
// AND registers that repository's main worktree as an open workspace.
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
// AND THE MAIN WORKTREE IS REGISTERED AS A WORKSPACE (owner ruling,
// 2026-09-14). The first landing stopped at the repository row, and a
// repository with NO WORKSPACE is not selectable: `SPC p p' completes over
// live workspaces, so the owner who registered the repository they were
// standing in still could not switch to it -- "I expect the main repo to be
// added as an actual workspace as well". The main worktree is a git worktree
// like any other, so it goes through `register', the SAME body
// RegisterWorkspace runs: same mint, same derived naming, same roster row,
// same session revival, same refusals. Nothing about this directory depends on
// which rpc announced it.
//
// IDEMPOTENT ON BOTH HALVES, and the bools say which each was: a repository or
// a workspace the registry already holds is answered with its own record and
// `true`, which is an answer and not a refusal.
func (v *verbs) RegisterRepository(ctx context.Context, path string) (RegisteredRepository, error) {
	global := v.deps.Log.Global().With(dlog.Context{"path": path})

	dir, err := repositoryProbeDir(path)
	if err != nil {
		return RegisteredRepository{}, refuse(global, "RegisterRepository", ArmUnreadablePath,
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
		return RegisteredRepository{}, fmt.Errorf("register repository %q: %w", path, err)
	}
	if !inRepository {
		return RegisteredRepository{}, refuse(global, "RegisterRepository", ArmNotInARepository,
			fmt.Sprintf("%q is not inside a git repository with a main worktree", path), false)
	}
	normalized, err := normalizeDir(main)
	if err != nil {
		global.Error(opRegisterRepository, "the resolved main worktree cannot be normalized", dlog.Context{
			"main_worktree": main, "cause": err.Error(),
		})
		return RegisteredRepository{}, fmt.Errorf("register repository %q: %w", path, err)
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
		return RegisteredRepository{}, fmt.Errorf("register repository %q: default branch: %w", path, err)
	}

	// THE REPOSITORY ROW IS WRITTEN FIRST, and on its own, for the one thing
	// the workspace registration cannot answer: whether the REPOSITORY was
	// already known. Registration mints the repository as a side effect and
	// says nothing about which it did, and that bool is half of what the user
	// is told.
	record, created, err := v.deps.DB.RegisterRepository(ctx, normalized, defaultBranch)
	if refused := temporaryRefusal(global, "RegisterRepository", err); refused != nil {
		return RegisteredRepository{}, refused
	}
	if err != nil {
		global.Error(opRegisterRepository, "could not record the repository", dlog.Context{
			"dir": normalized, "cause": err.Error(),
		})
		return RegisteredRepository{}, fmt.Errorf("register repository %q: %w", path, err)
	}
	global.Info(opRegisterRepository, "registered a repository", dlog.Context{
		"path": path, "dir": record.Dir, "repository": string(record.ID),
		"already_known": !created, "default_branch": record.DefaultBranch,
	})

	workspace, workspaceCreated, err := v.register(ctx, record.Dir, wsm.RegisterFacts{})
	if err != nil {
		// THE REPOSITORY STILL LANDED, so the roster is republished before the
		// failure is surfaced: the row is a durable fact the sidebar must
		// show, and `register' republishes only on the path it completes.
		v.republishRegistry(ctx, global, opRegisterRepository)
		global.Error(opRegisterRepository, "the repository's main worktree could not be registered as a workspace", dlog.Context{
			"dir": record.Dir, "repository": string(record.ID), "cause": err.Error(),
		})
		return RegisteredRepository{}, namedRefusal(err, "RegisterRepository")
	}
	global.Info(opRegisterRepository, "registered the repository's main worktree as a workspace", dlog.Context{
		"dir": workspace.Dir, "repository": string(record.ID), "workspace": string(workspace.ID),
		"workspace_already_known": !workspaceCreated,
	})
	// THE ROSTER IS ALREADY REPUBLISHED. `register' publishes it as the last
	// thing it does, and the repository row -- minted before that call -- is in
	// what it published. A second publish here would push the same roster
	// twice for one gesture.
	return RegisteredRepository{
		Repository:             record,
		RepositoryAlreadyKnown: !created,
		Workspace:              workspace,
		WorkspaceAlreadyKnown:  !workspaceCreated,
	}, nil
}

// repositoryProbeDir answers the directory git is asked about for PATH: the
// path itself when it is a directory, its parent when it is a file. A path
// that cannot be stat'ed at all has no probe directory, which is the
// unreadable_path refusal, and so is an empty or relative one: it is never
// resolved against the daemon's working directory (dirpath.Absolute).
func repositoryProbeDir(path string) (string, error) {
	abs, err := dirpath.Absolute(path, "")
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
