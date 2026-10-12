package workspace

import (
	"context"
	"errors"
	"fmt"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/wsm"
)

// opFoldRepository is the operation FoldRepository's records carry.
const opFoldRepository = "daemon.workspace.fold_repository"

// FoldRepository records whether a repository's roster section is collapsed,
// then republishes the roster: the sidebar hides a collapsed section's rows
// and the Emacs tab bar hides that repository's workspaces, both off the one
// pushed fold. COLLAPSING STANDS THE SECTION'S OPEN WORKSPACES' SESSIONS DOWN
// (only visible workspaces run), each as hiding it would; EXPANDING starts
// nothing, the editor asks with OpenTabs.
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
	if !folded {
		return nil
	}
	workspaces, err := v.deps.DB.ListWorkspaces(ctx)
	if err != nil {
		log.Error(opFoldRepository, "could not list the collapsed repository's workspaces; their sessions were not stood down", dlog.Context{"cause": err.Error()})
		return fmt.Errorf("fold repository %q: list its workspaces: %w", repo, err)
	}
	var withheld []string
	for _, ws := range workspaces {
		if ws.Repo != repo || ws.Closed {
			continue
		}
		withheld = append(withheld, string(ws.ID))
		v.deps.StandDowns.StandDownWithheld(ws.ID)
	}
	log.Info(opFoldRepository, "the collapsed repository's open workspaces are stood down", dlog.Context{"workspaces": withheld})
	return nil
}
