package account

import (
	"context"
	"fmt"

	"agent-repld/internal/ids"
)

// WorkspaceConfigDirs answers the account root a workspace's headless calls
// run under, so a call made about a workspace (its topbar title, its turn
// banner's summary, its chat questions' abbreviations) is billed to the
// account the workspace's session spends from (RootFor). It is THE ONE such
// interface: every consumer takes it rather than declaring its own.
type WorkspaceConfigDirs interface {
	// ConfigDirFor answers the workspace's account root, or the error that
	// kept the workspace's directory from being read. Each caller records
	// the failure at the level its own outcome warrants.
	ConfigDirFor(ctx context.Context, ws ids.WorkspaceID) (string, error)
}

// RootFor answers the account root a workspace spends from: the root the
// reader CHOSE for it (SelectAccount, empty when nobody has chosen), which
// outranks the path routing, and otherwise the root its directory routes to.
// It is THE ONE statement of that precedence, so a session's bring-up and a
// headless call about the same workspace can never bill different accounts.
func RootFor(accounts Resolver, dir, selected string) string {
	if selected != "" {
		return selected
	}
	return accounts.ConfigDirFor(dir)
}

// WorkspaceAccount is what RootFor reads of a workspace: its directory and
// the account root the reader chose for it, empty when nobody has chosen.
type WorkspaceAccount struct {
	Dir               string
	SelectedConfigDir string
}

// WorkspaceRoots is the production WorkspaceConfigDirs: the daemon's one
// workspace lookup decided by RootFor, so a headless call and the session's
// bring-up can never disagree about which account a workspace spends.
type WorkspaceRoots struct {
	// Read answers a workspace's directory and chosen root on the caller's
	// context.
	Read func(context.Context, ids.WorkspaceID) (WorkspaceAccount, error)
	// Accounts routes the directory to its root when nobody has chosen.
	Accounts Resolver
}

// ConfigDirFor implements WorkspaceConfigDirs.
func (r WorkspaceRoots) ConfigDirFor(ctx context.Context, ws ids.WorkspaceID) (string, error) {
	read, err := r.Read(ctx, ws)
	if err != nil {
		return "", fmt.Errorf("read the account of workspace %q: %w", ws, err)
	}
	return RootFor(r.Accounts, read.Dir, read.SelectedConfigDir), nil
}
