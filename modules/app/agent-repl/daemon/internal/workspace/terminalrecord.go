package workspace

import (
	"context"
	"errors"
	"fmt"

	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
	"claude-repld/internal/wsm"
)

// THE LIVE-SHIM INVARIANT: A WORKSPACE WHOSE SHIM IS LIVE IN THE FLEET CARRIES
// NO TERMINAL SESSION RECORD.
//
// retireTerminalRecord is the ONE way that invariant is restored, and every
// place the daemon begins holding a live client for a workspace passes through
// it — `Fleet.hold` (the bring-up's three arrivals: the adopted shim, the
// session parked at its cold gate, and the started session), `Fleet.Install`
// (the relaunch engine's rotation and the boot adoption), and `OpenWorkspace`,
// which reconciles rather than trusting that a live session's record already
// agrees with it.
//
// WHY IT HAD TO BECOME A STEP OF ITS OWN. Nothing retired a terminal record:
// the only write that ever cleared one was a successful PutSession, which
// clears it incidentally because the row it composes carries no terminal. A
// bring-up that parks at a cold gate records no session facts at all, so a
// workspace killed weeks earlier came up with a live shim behind a standing
// gate while its record still read `killed` — and the roster RECEDES a killed
// session's row (internal/resolve/sidebar's recedes), Emacs gives a tab only to
// a row that is not receded, and the gate the user had to answer lived in a
// workspace with no tab (workspace 3e2d9cadc6794e13, 2026-09-20T16:43:26).
//
// A DELETED SESSION IS NOT RESURRECTED. The store refuses the retirement of a
// deleted record, and that refusal is an ordinary outcome here rather than a
// failure: the workspace was forgotten, its cause of death is final, and the
// live client is not evidence against it. Every other failure is the store's,
// and it is recorded and returned.
func retireTerminalRecord(ctx context.Context, log dlog.Logger, db wsm.DB, operation string, ws ids.WorkspaceID) error {
	err := db.ClearSessionTerminal(ctx, ws)
	switch {
	case err == nil:
		return nil
	case errors.Is(err, wsm.ErrSessionDeleted):
		log.Info(operation, "the session record is deleted, so its terminal stands", nil)
		return nil
	default:
		log.Error(operation, "could not retire the session's terminal record", dlog.Context{"cause": err.Error()})
		return fmt.Errorf("retire the terminal session record for %q: %w", ws, err)
	}
}
