package workspace

import (
	"context"
	"fmt"

	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
	"claude-repld/internal/rollout"
)

// Restart bounces a workspace's shim. The bounce itself is the ROLLOUT's one
// relaunch engine — prelaunch inert, wait for freeness, take the restart
// hold, stand the old shim down gracefully, reap, resume, drain the holds — so
// this verb adds only the two things the engine deliberately does not own.
//
// force sends KillTurn{force:true} FIRST, because the engine waits for freeness
// and a wedged turn would otherwise never let it start.
//
// The verb also owns the WEBAPP's half of a restart: when the served webapp
// asset build changed, the webview is told to reload, so the user is not left
// on a page built against a different daemon.
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

	if err := v.deps.Rollout.RelaunchShim(ctx, ws, rollout.ReasonRestartVerb); err != nil {
		log.Error(opRestart, "the shim relaunch failed", dlog.Context{"force": force, "cause": err.Error()})
		return fmt.Errorf("restart %q: %w", ws, err)
	}

	// The reload_webapp push follows the relaunch, not the other way round: a
	// webview reloading against a shim that is still standing down would
	// re-mount onto a session that is about to be replaced.
	if err := v.deps.Rollout.ReloadWebapp(ctx, ws); err != nil {
		log.Warn(opRestart, "could not push the webapp reload", dlog.Context{"cause": err.Error()})
	} else {
		log.Debug(opRestart, "pushed the webapp reload", nil)
	}

	log.Info(opRestart, "restarted the workspace", dlog.Context{"force": force})
	return nil
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
	if err := shim.KillTurn(ctx, *running.Turn, true); err != nil {
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
