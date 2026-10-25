package wsm

import (
	"context"
	"errors"
	"fmt"

	"agent-repld/internal/dlog"
)

// SetName records the workspace's new name (agentrepl.v1.SetWorkspaceName).
// It writes the name column only: the branch and the worktree directory keep
// the names the workspace was created with, so after a rename the name and
// the branch may differ. Whether the name is legal and free is the caller's
// check (workspace.verbs.SetName), made under the naming lock. An empty name
// is a defect in the caller and is refused rather than stored, since every
// reader draws the name and the roster refuses a blank one.
func (s *store) SetName(ctx context.Context, id WorkspaceID, name string) error {
	const op = "daemon.wsm.set_name"
	if name == "" {
		err := fmt.Errorf("wsm: name of %s: %w", id, errEmptyName)
		s.log.Error(op, "refused an empty workspace name", withError(dlog.Context{"workspace": string(id)}, err))
		return err
	}
	return s.setWorkspaceColumns(ctx, op, id, dlog.Context{"name": name}, workspaceColumn{name: "name", value: name})
}

// errEmptyName is the empty-name refusal.
var errEmptyName = errors.New("a workspace name is never empty")
