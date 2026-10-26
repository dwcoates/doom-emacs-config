package account

import (
	"context"
	"fmt"

	"agent-repld/internal/ids"
)

// WorkspaceConfigDirs answers the account root a workspace's headless calls
// run under, so a call made about a workspace (its topbar title, its turn
// banner's summary, its chat questions' abbreviations) is billed to the
// account its directory routes to. It is THE ONE such interface: every
// consumer takes it rather than declaring its own.
type WorkspaceConfigDirs interface {
	// ConfigDirFor answers the workspace's account root, or the error that
	// kept the workspace's directory from being read. Each caller records
	// the failure at the level its own outcome warrants.
	ConfigDirFor(ctx context.Context, ws ids.WorkspaceID) (string, error)
}

// WorkspaceRoots is the production WorkspaceConfigDirs: the daemon's one
// workspace-directory lookup routed through the Resolver's one
// repo-under-root rule, so a headless call and the session's path routing
// can never disagree about which account a workspace's directory picks.
type WorkspaceRoots struct {
	// Dir answers a workspace's directory on the caller's context.
	Dir func(context.Context, ids.WorkspaceID) (string, error)
	// Accounts routes the directory to its root.
	Accounts Resolver
}

// ConfigDirFor implements WorkspaceConfigDirs.
func (r WorkspaceRoots) ConfigDirFor(ctx context.Context, ws ids.WorkspaceID) (string, error) {
	dir, err := r.Dir(ctx, ws)
	if err != nil {
		return "", fmt.Errorf("read the directory of workspace %q: %w", ws, err)
	}
	return r.Accounts.ConfigDirFor(dir), nil
}
