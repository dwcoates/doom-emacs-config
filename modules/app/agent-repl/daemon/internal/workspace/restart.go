package workspace

import (
	"context"
	"errors"
	"fmt"
	"time"

	"claude-repld/internal/bounce"
	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
	"claude-repld/internal/rollout"
)

// DefaultRestartStopBound bounds the ONE stop a restart asks of the shim before
// it bounces it: the forced end of the running turn.
//
// A RESTART IS ASKED FOR BECAUSE SOMETHING IS STUCK, and the vendor is usually
// unreachable when it is: a forced KillTurn that waits on the vendor to
// acknowledge can wait forever, and a restart that waits with it is the
// deadlock the 2026-10-02 incident was. The kill is a local rpc to the shim,
// which answers once it has signalled its vendor child, so a healthy one
// answers in milliseconds; 2s is generous for that and short enough that a
// stuck one costs the user a moment, not a hang. An overrun is ERROR and the
// bounce PROCEEDS: the forced stand-down that follows hard-kills whatever did
// not stop (endpoint_restart_workspace.proto).
const DefaultRestartStopBound = 2 * time.Second

// Restart bounces a workspace's shim IMMEDIATELY, always
// (endpoint_restart_workspace.proto: there is no graceful mode). The bounce is
// the ROLLOUT's one engine, asked through the prompt queue's bounce registry
// as a FORCED bounce, which the registry takes at once over whatever still
// runs. The verb adds only what the engine deliberately does not own:
//
//  1. a vendor-start retry run still in flight for the workspace is ENDED
//     first, so the restart begins a fresh run with the full window rather
//     than queueing behind the old one;
//  2. the running turn is force-ended with a BOUNDED call; a failure or an
//     overrun is recorded at ERROR and the bounce proceeds, because the
//     bounce's forced stand-down is the remedy for exactly a shim that will
//     not stop;
//  3. once the bounce is done, the webview is told to reload, so the user is
//     not left on a page built against a different shim.
//
// A workspace with no shim or no session is restarted by the same bounce: the
// engine prelaunches a shim and resumes the session on it.
func (v *verbs) Restart(ctx context.Context, ws ids.WorkspaceID) error {
	_, log, err := v.owned(ctx, "RestartWorkspace", ws)
	if err != nil {
		return err
	}
	if v.deps.Sessions.CancelVendorStart(ctx, ws) {
		log.Info(opRestart, "ended the vendor-start retry run in flight; the restart begins a fresh one", nil)
	}
	v.forceEndTurn(ctx, log, ws)
	return v.bounceShim(ctx, log, ws, true)
}

// bounceShim asks the bounce registry for the restart's shim replacement and
// hangs the webapp reload off its completion. force is false only for the
// account switch (SelectAccount), which bounces at the workspace's freeness
// rather than over its work.
func (v *verbs) bounceShim(ctx context.Context, log dlog.Logger, ws ids.WorkspaceID, force bool) error {
	// THE VERB ACCEPTS; THE BOUNCE RUNS BEHIND IT. Its completion is what the
	// reload and the record below hang off. The context is DETACHED from the
	// request: the bounce outlives the rpc.
	detached := context.WithoutCancel(ctx)
	decision, err := v.deps.Rollout.BounceShim(detached, ws, rollout.ReasonRestartVerb, force, func(err error) {
		v.finishRestart(detached, log, ws, force, err)
	})
	if errors.Is(err, bounce.ErrMovedAway) {
		// THE WORKSPACE IS MOVING TO ANOTHER DAEMON and its move has sealed
		// what it carries: the restart is the next daemon's to run, and the
		// transport answers `transferring_away` naming it, so the caller asks
		// there.
		log.Info(opRestart, "the workspace is moving to another daemon; the restart is refused so it is asked of that daemon", dlog.Context{"force": force})
		return fmt.Errorf("restart %q: %w", ws, err)
	}
	if err != nil {
		log.Error(opRestart, "the bounce registry refused the restart", dlog.Context{"force": force, "cause": err.Error()})
		return fmt.Errorf("restart %q: %w", ws, err)
	}
	log.Info(opRestart, "accepted the restart; the bounce runs behind it", dlog.Context{
		"force": force, "now": decision.Now, "turn_in_flight": decision.TurnInFlight, "detached_work": decision.DetachedWork,
	})
	return nil
}

// finishRestart is the restart's completion: the webapp reload that follows a
// bounce, or the bounce's failure. Nobody is waiting on it, so its failures
// are RECORDED rather than returned.
func (v *verbs) finishRestart(ctx context.Context, log dlog.Logger, ws ids.WorkspaceID, force bool, err error) {
	switch bounce.OutcomeOf(err) {
	case bounce.OutcomeUnregistered:
		// AN OUTCOME, NOT A FAILURE: the shim the restart would replace
		// departed first, and nothing is left to replace -- the session was
		// ended, the workspace closed, or a fresh shim already serves it.
		log.Info(opRestart, "the restart was unregistered: the shim it would replace departed and nothing is left to replace", dlog.Context{"force": force})
		return
	case bounce.OutcomeHandedAcross:
		// AN OUTCOME, NOT A FAILURE (owner ruling, 2026-09-27): the restart
		// raced a handover's move of the workspace, and the daemon that
		// adopted it runs the restart -- and the webapp reload after it.
		log.Info(opRestart, "the restart was handed to the daemon the workspace moved to, which runs it after its adoption", dlog.Context{"force": force})
		return
	case bounce.OutcomeDeferred:
		// The registry keeps a deferred bounce's Done for the rerun, so being
		// told a deferral is its contract broken; it is never read as a
		// finished restart.
		log.Error(opRestart, "the bounce registry told the restart a deferral; it owes only the rerun's outcome", dlog.Context{"force": force, "cause": err.Error()})
		return
	case bounce.OutcomeFailed:
		log.Error(opRestart, "the shim relaunch failed", dlog.Context{"force": force, "cause": err.Error()})
		return
	case bounce.OutcomeFinished:
		// The webapp reload below follows the finished bounce.
	}

	// The reload_webapp push follows the bounce, not the other way round: a
	// webview reloading against a shim that is still standing down would
	// re-mount onto a session that is about to be replaced.
	if err := v.deps.Rollout.ReloadWebapp(ctx, ws); err != nil {
		log.Error(opRestart, "could not push the webapp reload", dlog.Context{"cause": err.Error()})
	} else {
		log.Debug(opRestart, "pushed the webapp reload", nil)
	}

	log.Info(opRestart, "restarted the workspace", dlog.Context{"force": force})
}

// forceEndTurn ends whatever turn is running before the bounce, BOUNDED by
// restartStopBound. A workspace with no live session or no open turn has
// nothing to end.
//
// IT NEVER FAILS THE RESTART. A kill that fails or overruns its bound is
// recorded at ERROR -- surfaced, not swallowed -- and the restart proceeds to
// its forced bounce, whose stand-down hard-kills what this could not stop: a
// restart that gave up here would leave the stuck workspace exactly as stuck
// as it was.
func (v *verbs) forceEndTurn(ctx context.Context, log dlog.Logger, ws ids.WorkspaceID) {
	running, live := v.deps.Freeness(ws)
	if !live || running.Turn == nil {
		log.Debug(opRestart, "no turn to force-end before the relaunch", nil)
		return
	}
	shim, ok := v.deps.Shim(ws)
	if !ok {
		log.Debug(opRestart, "no shim to force-end the turn through", nil)
		return
	}
	stopCtx, cancel := context.WithTimeout(ctx, v.restartStopBound)
	defer cancel()
	if err := shim.KillTurn(stopCtx, *running.Turn, true, nil); err != nil {
		log.Error(opRestart, "could not force-end the running turn; the forced bounce ends it with the shim", dlog.Context{
			"turn": string(*running.Turn), "cause": err.Error(), "bound_ms": v.restartStopBound.Milliseconds(),
		})
		return
	}
	log.Debug(opRestart, "force-ended the running turn before the relaunch", dlog.Context{
		"turn": string(*running.Turn),
	})
}
