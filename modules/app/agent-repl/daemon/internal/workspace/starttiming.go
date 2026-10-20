package workspace

import (
	"errors"
	"time"

	"agent-repld/internal/ids"
	"agent-repld/internal/restarttiming"
	"agent-repld/internal/shimclient"
)

// sessionTiming times the steps of ONE workspace's session start for the
// restart metrics (restarttiming.SessionStart). Each step is measured by the
// code that runs it, on the fleet's clock.
type sessionTiming struct {
	recorder restarttiming.Recorder
	now      func() time.Time
	ws       ids.WorkspaceID
	kind     restarttiming.SessionKind
}

// timing answers the timing of ws's start of KIND.
func (f *Fleet) timing(ws ids.WorkspaceID, kind restarttiming.SessionKind) sessionTiming {
	return sessionTiming{recorder: f.deps.Restarts, now: f.now, ws: ws, kind: kind}
}

// as answers the same timing, for a start now known to be of KIND.
func (t sessionTiming) as(kind restarttiming.SessionKind) sessionTiming {
	t.kind = kind
	return t
}

// since records phase P as having run from FROM until now and ended in ERR.
//
// A STEP CUT SHORT BY A STAND-DOWN THIS DAEMON ORDERED IS NOT TIMED: it says
// nothing about how long the step takes, exactly as the bring-up leaves such
// a start untimed.
func (t sessionTiming) since(p restarttiming.SessionPhase, from time.Time, err error) {
	if errors.Is(err, shimclient.ErrStandDownOrdered) || errors.Is(err, shimclient.ErrStandingDown) {
		return
	}
	t.recorder.ObserveRestart(restarttiming.OfSessionStart(t.ws, p, t.kind, restarttiming.OutcomeOf(err), t.now().Sub(from)))
}

// startKind is the kind of a fleet start: a fresh conversation, a hibernated
// one woken, or one resumed.
func startKind(fresh, hibernated bool) restarttiming.SessionKind {
	switch {
	case fresh:
		return restarttiming.SessionFresh
	case hibernated:
		return restarttiming.SessionWake
	default:
		return restarttiming.SessionResume
	}
}
