package workspace

import (
	"context"
	"errors"
	"fmt"
	"time"

	"agent-repld/internal/bounce"
	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/rollout"
	"agent-repld/internal/shimclient"
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
// A WORKSPACE WITH NO SHIM UP HAS ITS SESSION STARTED AT ONCE (owner ruling,
// 2026-10-28: the restart is an explicit start, like a prompt). The bounce
// would find nothing to replace, so the session is started through the one
// start path (Sessions.Start) and the restart answers its outcome.
func (v *verbs) Restart(ctx context.Context, ws ids.WorkspaceID) error {
	_, log, err := v.owned(ctx, "RestartWorkspace", ws)
	if err != nil {
		return err
	}
	if v.deps.Sessions.CancelVendorStart(ctx, ws) {
		log.Info(opRestart, "ended the vendor-start retry run in flight; the restart begins a fresh one", nil)
	}
	if _, up := v.deps.Shim(ws); !up {
		return v.startForRestart(ctx, log, ws)
	}
	// THE WORKSPACE'S MERGE LETS GO FIRST (owner ruling, 2026-10-08). A merge
	// drives this workspace's session, so it is brought to a stopping point
	// exactly as a handover's transfer brings it (merge.SuspendForRestart)
	// and taken back once the bounce finished, whatever its outcome. A merge
	// only WAITING for the workspace to fall free never blocks the restart --
	// the restart is also the escape hatch for live work that went stale --
	// and waits again on the fresh session. Measured 2026-10-08 18:55:03,
	// workspace `doom`: the relaunch could not take its hold over the waiting
	// merge's lease and the restart did nothing.
	if err := v.deps.Merge.SuspendForRestart(ctx, ws); err != nil {
		log.Error(opRestart, "the workspace's merge did not reach a stopping point; the restart is refused and the merge taken back", dlog.Context{"cause": err.Error()})
		v.resumeMerges(ctx, log, ws)
		return relaunchFailed(ws, "the workspace's merge did not reach a stopping point: "+err.Error())
	}
	defer v.resumeMerges(ctx, log, ws)
	v.forceEndTurn(ctx, log, ws)
	outcome := make(chan error, 1)
	if err := v.bounceShim(ctx, log, ws, true, func(err error) { outcome <- err }); err != nil {
		return err
	}
	// THE ANSWER IS THE OUTCOME (owner ruling, 2026-10-08: "i expect it to
	// just restart instantly (not 'work on it' when it 'gets around to it')").
	// A forced bounce is taken at once, so the restart's answer waits for it
	// to finish and says whether it happened; a relaunch that failed is the
	// `relaunch_failed` arm, which the editor surfaces. Measured 2026-10-08
	// 18:55:03, workspace `doom`: the answer said "under way", the relaunch
	// then failed on a merge's lease, and nothing told the user.
	select {
	case err := <-outcome:
		return restartAnswer(ws, err)
	case <-ctx.Done():
		log.Error(opRestart, "the restart's caller left before the restart finished; the bounce runs on and records its own outcome", dlog.Context{
			"cause": ctx.Err().Error(),
		})
		return fmt.Errorf("restart %q: the caller left before the restart finished: %w", ws, ctx.Err())
	}
}

// startForRestart starts the session of a workspace a restart found with no
// shim up, answering the outcome as the restart's.
func (v *verbs) startForRestart(ctx context.Context, log dlog.Logger, ws ids.WorkspaceID) error {
	log.Info(opRestart, "no shim is up; the restart starts the session", nil)
	if err := v.deps.Sessions.Start(ctx, ws); err != nil {
		if errors.Is(err, shimclient.ErrStandingDown) {
			log.Info(opRestart, "the session was not started: this daemon is standing down", nil)
		} else {
			log.Error(opRestart, "the restart could not start the session", dlog.Context{"cause": err.Error()})
		}
		return relaunchFailed(ws, "the session did not start: "+err.Error())
	}
	log.Info(opRestart, "the restart started the session", nil)
	return nil
}

// resumeMerges takes the workspace's merges back after a restart, on a context
// the restart's caller leaving cannot cancel. A failure is recorded at ERROR;
// the merge's own take-back has recorded its cause.
func (v *verbs) resumeMerges(ctx context.Context, log dlog.Logger, ws ids.WorkspaceID) {
	if err := v.deps.Merge.ResumeAfterRestart(context.WithoutCancel(ctx), ws); err != nil {
		log.Error(opRestart, "could not take the workspace's merges back after the restart", dlog.Context{"cause": err.Error()})
		return
	}
	log.Debug(opRestart, "took the workspace's merges back after the restart", nil)
}

// restartAnswer turns a finished bounce's outcome into the restart's answer:
// nil when the restart happened, or there was nothing left to replace, or the
// daemon the workspace moved to runs it; the `relaunch_failed` refusal, with
// the cause, otherwise. finishRestart has already recorded each outcome.
func restartAnswer(ws ids.WorkspaceID, err error) error {
	switch bounce.OutcomeOf(err) {
	case bounce.OutcomeFinished, bounce.OutcomeUnregistered, bounce.OutcomeHandedAcross:
		return nil
	}
	return relaunchFailed(ws, err.Error())
}

// relaunchFailed is THE one `RestartWorkspaceError.relaunch_failed` refusal:
// the restart did not happen, and detail says why.
func relaunchFailed(ws ids.WorkspaceID, detail string) *Refusal {
	return &Refusal{
		Rpc:    "RestartWorkspace",
		Arm:    "relaunch_failed",
		Reason: fmt.Sprintf("the restart of %s failed: %s", ws, detail),
		Fields: map[string]any{"detail": detail},
	}
}

// bounceShim asks the bounce registry for the restart's shim replacement and
// hangs the webapp reload off its completion. force is false only for the
// account switch (SelectAccount), which bounces at the workspace's freeness
// rather than over its work. finished, when set, is told the bounce's outcome
// after the completion's own record and reload.
func (v *verbs) bounceShim(ctx context.Context, log dlog.Logger, ws ids.WorkspaceID, force bool, finished func(error)) error {
	// THE BOUNCE RUNS ON A CONTEXT DETACHED FROM THE REQUEST: it outlives the
	// rpc. Its completion is what the reload and the record below hang off.
	detached := context.WithoutCancel(ctx)
	decision, err := v.deps.Rollout.BounceShim(detached, ws, rollout.ReasonRestartVerb, force, func(err error) {
		v.finishRestart(detached, log, ws, force, err)
		if finished != nil {
			finished(err)
		}
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
	log.Info(opRestart, "accepted the restart; the bounce is running", dlog.Context{
		"force": force, "now": decision.Now, "turn_in_flight": decision.TurnInFlight, "detached_work": decision.DetachedWork,
	})
	return nil
}

// finishRestart is the restart's completion: the webapp reload that follows a
// bounce, or the bounce's failure, RECORDED here once. The restart verb also
// answers the outcome to its caller (restartAnswer); the account switch's
// bounce has no caller waiting on it.
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
