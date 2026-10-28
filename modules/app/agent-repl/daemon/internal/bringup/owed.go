package bringup

import (
	"context"
	"fmt"

	"agent-repld/internal/wsm"
)

// A WORKSPACE OWED A CUT TURN IS BROUGHT UP WHATEVER ITS VISIBILITY OR PARK.
// A turn this daemon's own stand-down cut (a restart, a bounce) is a prompt
// the daemon still owes the workspace, and a prompt is what brings a shim up
// (owner ruling, 2026-10-28). An open no longer starts a shim, so a hidden
// workspace owed one would otherwise wait for it forever; the session-up
// release of this bring-up pays it (promptqueue restartredrive).

// OwedReader reads the cut turns a workspace is owed a resumption for.
// wsm.DB is the production one.
type OwedReader interface {
	OwedTurns(ctx context.Context, ws wsm.WorkspaceID) ([]wsm.TurnID, error)
}

// OnlyOwed splits LEFT -- workspaces a boot or a takeover would otherwise not
// bring up -- into the ones owed a cut turn and the rest, each in LEFT's
// order. A read that fails splits nothing: the caller states the failure.
func OnlyOwed(ctx context.Context, db OwedReader, left []wsm.Workspace) (owed, rest []wsm.Workspace, err error) {
	for _, ws := range left {
		turns, err := db.OwedTurns(ctx, ws.ID)
		if err != nil {
			return nil, nil, fmt.Errorf("bringup: read the cut turns %s is owed: %w", ws.ID, err)
		}
		if len(turns) > 0 {
			owed = append(owed, ws)
			continue
		}
		rest = append(rest, ws)
	}
	return owed, rest, nil
}
