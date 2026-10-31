package workspace

import (
	"context"
	"fmt"

	"agent-repld/internal/wsm"
)

// isMainWorkspace reports whether RECORD is its repository's main workspace:
// the workspace on the repository's main checkout (wsm.IsMainWorktree). It
// reads the repository registry for the repository's directory, and a record
// whose repository is not registered is an error, never "not main": a
// workspace with no registered repository is an invariant violation
// (wsm/repoinvariant.go), not a standing.
//
// It is the one place this package resolves the standing, so the one-shot
// refusal, the session's standing notice and the forget guard cannot
// disagree about which workspace is the main one.
func isMainWorkspace(ctx context.Context, db wsm.DB, record wsm.Workspace) (bool, error) {
	repoDir, err := repositoryRoot(ctx, db, record.Repo)
	if err != nil {
		return false, err
	}
	return wsm.IsMainWorktree(record.Dir, repoDir), nil
}

// repositoryRoot answers the directory of the registered repository REPO.
func repositoryRoot(ctx context.Context, db wsm.DB, repo wsm.RepoID) (string, error) {
	repositories, err := db.ListRepositories(ctx)
	if err != nil {
		return "", fmt.Errorf("read the repository registry: %w", err)
	}
	if repository, found := wsm.RepositoryWithID(repositories, repo); found {
		return repository.Dir, nil
	}
	return "", fmt.Errorf("no repository %q is registered", repo)
}
