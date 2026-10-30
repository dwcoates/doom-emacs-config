package workspace

import (
	"context"
	"fmt"

	"agent-repld/internal/dlog"
)

// opHomeRepository is the operation every record EnsureHomeRepository writes
// is filed under.
const opHomeRepository = "daemon.repository.home"

// EnsureHomeRepository registers AGENT-REPL'S OWN REPOSITORY, the one its
// checkout DIR lives in, when the registry does not hold it (owner ruling,
// 2026-10-30): agent-repl is always a known project, so a first start with
// nothing registered has it, with its main worktree registered as an OPEN
// workspace, exactly as `repository register` would.
//
// An empty DIR is a daemon told to register nothing (a test daemon, whose
// registry is its suite's). A checkout in no git repository is an ordinary
// answer, recorded at INFO: there is nothing to register. A git failure, or a
// registration that fails, is surfaced.
func (v *verbs) EnsureHomeRepository(ctx context.Context, dir string) error {
	global := v.deps.Log.Global().With(dlog.Context{"checkout": dir})
	if dir == "" {
		global.Debug(opHomeRepository, "this daemon was told to register no home repository", nil)
		return nil
	}
	main, inRepository, err := v.deps.Git.RepositoryOf(ctx, dir)
	if err != nil {
		global.Error(opHomeRepository, "git could not be asked what repository agent-repl's checkout is in", dlog.Context{
			"cause": err.Error(),
		})
		return fmt.Errorf("ensure the home repository of %q: %w", dir, err)
	}
	if !inRepository {
		global.Info(opHomeRepository, "agent-repl's checkout is in no git repository; there is no home repository to register", nil)
		return nil
	}
	normalized, err := normalizeDir(main)
	if err != nil {
		global.Error(opHomeRepository, "the home repository's main worktree cannot be normalized", dlog.Context{
			"main_worktree": main, "cause": err.Error(),
		})
		return fmt.Errorf("ensure the home repository of %q: %w", dir, err)
	}
	known, err := v.repositoryRegisteredAt(ctx, normalized)
	if err != nil {
		global.Error(opHomeRepository, "could not read whether the home repository is registered", dlog.Context{
			"dir": normalized, "cause": err.Error(),
		})
		return fmt.Errorf("ensure the home repository of %q: %w", dir, err)
	}
	if known {
		global.Debug(opHomeRepository, "the home repository is already registered", dlog.Context{"dir": normalized})
		return nil
	}
	registered, err := v.RegisterRepository(ctx, dir)
	if err != nil {
		// RegisterRepository recorded its own failure or refusal.
		return fmt.Errorf("ensure the home repository of %q: %w", dir, err)
	}
	global.Info(opHomeRepository, "registered agent-repl's own repository, and opened its main worktree", dlog.Context{
		"dir": registered.Repository.Dir, "repository": string(registered.Repository.ID),
		"workspace": string(registered.Workspace.ID),
	})
	return nil
}
