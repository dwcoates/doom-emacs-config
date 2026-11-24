package workspace

import (
	"context"
	"errors"
	"fmt"
	"time"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/shimlife"
	"agent-repld/internal/wsm"
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

// THE ENDED-SHIM INVARIANT: A SESSION RECORD WITH A CONVERSATION AND NO
// TERMINAL IS A SESSION THAT IS RUNNING, OR THAT WAS RUNNING WHEN ITS DAEMON
// STOPPED. It is the counterpart of the live-shim invariant above, and
// recordShimEnded is the ONE way it is kept: every place the fleet stops
// holding a client (Fleet.teardown, an ordered stop, and Fleet.retireReaped,
// a shim that is gone) records the session's end before it returns.
//
// WHY. A takeover and a boot start a workspace's session exactly when its
// record says it was running (bringup.OnlyWasUp; owner ruling, 2026-11-23: a
// handover starts nothing that was not already started). Only the drain's park
// and the kill verb wrote a terminal, and the park wrote none when its kill ran
// out of time with the shim already SIGKILLed: at the 2026-11-23 15:35
// handover five visible workspaces whose keep-alive parks had failed that way
// minutes before read as running, and the successor spawned their shims under
// the handover's load. A shim lost under a bounce wrote none either.
//
// THE END IS A PARK, whatever ended it, and its cause is the detail: a parked
// session is drawn and revived exactly as a stood-down one, which is what
// every surface already did with a shim that went away, so no view changes;
// only the record stops claiming a session that is not running. Two stops
// leave the record alone, because the session runs on past them: the daemon's
// own exit (shutdown), whose sessions the next daemon brings back, and a
// handover's, whose shim a successor serves. A kill records "killed", as the
// kill verb does.

// shimEndTerminal answers the terminal an ordered stop for REASON records, and
// false for a stop whose session runs on past it.
func shimEndTerminal(reason shimlife.StandDownReason) (kind string, record bool) {
	switch reason {
	case shimlife.StandDownShutdown, shimlife.StandDownHandover:
		return "", false
	case shimlife.StandDownKill:
		return "killed", true
	default:
		return wsm.TerminalParked, true
	}
}

// recordShimEnded records that WS's shim ended, as KIND with DETAIL at AT. A
// workspace with no session row has no session to end, and a deleted one
// keeps its final cause; both are ordinary outcomes. Every other failure is
// the store's, recorded at ERROR and returned.
func recordShimEnded(ctx context.Context, log dlog.Logger, db wsm.DB, operation string, ws ids.WorkspaceID, kind, detail string, at time.Time) error {
	fields := dlog.Context{"terminal": kind, "terminal_detail": detail}
	err := db.SetSessionTerminal(ctx, ws, wsm.SessionTerminal{Kind: kind, Detail: detail, At: at})
	switch {
	case err == nil:
		log.Info(operation, "recorded the end of the workspace's shim on its session record", fields)
		return nil
	case errors.Is(err, wsm.ErrNotFound):
		log.Debug(operation, "the workspace has no session record, so there is no end to record", fields)
		return nil
	case errors.Is(err, wsm.ErrSessionDeleted):
		log.Info(operation, "the session record is deleted, so its terminal stands", fields)
		return nil
	default:
		log.Error(operation, "could not record the end of the workspace's shim", dlog.WithCause(fields, err))
		return fmt.Errorf("record the end of the shim of %q: %w", ws, err)
	}
}
