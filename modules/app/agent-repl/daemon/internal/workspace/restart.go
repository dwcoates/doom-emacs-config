package workspace

import (
	"context"
	"errors"
	"fmt"

	"claude-repld/internal/bounce"
	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
	"claude-repld/internal/rollout"
)

// Restart bounces a workspace's shim. The bounce itself is the ROLLOUT's one
// engine, asked through the prompt queue's bounce registry — at once when the
// workspace is free, when its work ends otherwise — so this verb adds only the
// two things the engine deliberately does not own.
//
// force sends KillTurn{force:true} FIRST and then asks for a FORCED bounce,
// which the registry takes at once over whatever is still running. The
// interrupt is synchronous: the caller is told whether it could be sent at all.
//
// The verb also owns the WEBAPP's half of a restart: when the bounce is done,
// the webview is told to reload, so the user is not left on a page built
// against a different shim.
func (v *verbs) Restart(ctx context.Context, ws ids.WorkspaceID, force bool) error {
	_, log, err := v.owned(ctx, "RestartWorkspace", ws)
	if err != nil {
		return err
	}

	if force {
		if err := v.forceEndTurn(ctx, log, ws); err != nil {
			return err
		}
	}

	// THE VERB ACCEPTS; THE BOUNCE RUNS BEHIND IT. An unforced bounce waits
	// for FREENESS, forever if need be -- that wait is the whole design, and a
	// graceful restart asked for while a turn is running is precisely the
	// case it exists for. Answering only once it finished would make the
	// verb's answer a function of how long the agent takes; the registry's
	// completion is what the reload and the record below hang off instead.
	//
	// The context is DETACHED from the request: the bounce outlives the rpc.
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
	if errors.Is(err, bounce.ErrUnregistered) {
		// AN OUTCOME, NOT A FAILURE: the shim the restart would replace
		// departed first, and nothing is left to replace -- the session was
		// ended, the workspace closed, or a fresh shim already serves it.
		log.Info(opRestart, "the restart was unregistered: the shim it would replace departed and nothing is left to replace", dlog.Context{"force": force})
		return
	}
	if errors.Is(err, bounce.ErrHandedAcross) {
		// AN OUTCOME, NOT A FAILURE (owner ruling, 2026-09-27): the restart
		// raced a handover's move of the workspace, and the daemon that
		// adopted it runs the restart -- and the webapp reload after it.
		log.Info(opRestart, "the restart was handed to the daemon the workspace moved to, which runs it after its adoption", dlog.Context{"force": force})
		return
	}
	if err != nil {
		log.Error(opRestart, "the shim relaunch failed", dlog.Context{"force": force, "cause": err.Error()})
		return
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

// forceEndTurn ends whatever turn is running so the relaunch engine's freeness
// wait can complete. A workspace with no live session and no open turn is
// already free, which is success.
func (v *verbs) forceEndTurn(ctx context.Context, log dlog.Logger, ws ids.WorkspaceID) error {
	running, live := v.deps.Freeness(ws)
	if !live || running.Turn == nil {
		log.Debug(opRestart, "no turn to force-end before the relaunch", nil)
		return nil
	}
	shim, ok := v.deps.Shim(ws)
	if !ok {
		log.Debug(opRestart, "no shim to force-end the turn through", nil)
		return nil
	}
	if err := shim.KillTurn(ctx, *running.Turn, true, nil); err != nil {
		log.Error(opRestart, "could not force-end the running turn", dlog.Context{
			"turn": string(*running.Turn), "cause": err.Error(),
		})
		return fmt.Errorf("restart %q: force-end turn %q: %w", ws, *running.Turn, err)
	}
	log.Debug(opRestart, "force-ended the running turn before the relaunch", dlog.Context{
		"turn": string(*running.Turn),
	})
	return nil
}
