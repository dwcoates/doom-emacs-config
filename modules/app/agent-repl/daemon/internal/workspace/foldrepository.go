package workspace

import (
	"context"
	"errors"
	"fmt"

	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
	"claude-repld/internal/wsm"
)

// opFoldRepository is the operation FoldRepository's records carry.
const opFoldRepository = "daemon.workspace.fold_repository"

// FoldRepository records whether a repository's roster section is collapsed,
// then republishes the roster: the sidebar hides a collapsed section's rows
// and the Emacs tab bar hides that repository's workspaces, both off the one
// pushed fold.
func (v *verbs) FoldRepository(ctx context.Context, repo ids.RepoID, folded bool) error {
	log := v.deps.Log.Global().With(dlog.Context{"repo_id": string(repo), "folded": folded})
	if err := v.deps.DB.SetRepositoryFolded(ctx, repo, folded); err != nil {
		if errors.Is(err, wsm.ErrNotFound) {
			return refuse(log, "FoldRepository", ArmUnknownRepository,
				fmt.Sprintf("no repository %q is registered", repo), true)
		}
		log.Error(opFoldRepository, "could not record the repository's fold", dlog.Context{"cause": err.Error()})
		return fmt.Errorf("fold repository %q: %w", repo, err)
	}
	log.Info(opFoldRepository, "recorded the repository's fold", nil)
	v.republishRegistry(ctx, log, opFoldRepository)
	return nil
}
