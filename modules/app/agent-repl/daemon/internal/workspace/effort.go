package workspace

import (
	"context"
	"errors"
	"fmt"
	"slices"

	conversationv1 "agentrepl/proto/conversation/v1"
	shimv1 "agentrepl/proto/shim/v1"

	"agent-repld/internal/claudesettings"
	"agent-repld/internal/dlog"
	"agent-repld/internal/effortlevel"
	"agent-repld/internal/ids"
	"agent-repld/internal/shimclient"
	"agent-repld/internal/wsm"
)

// THE EFFORT SELECTOR'S VERB AND ITS STARTING LEVEL (design record
// 2026-10-02 decision 2).
//
// THE SWITCH IS NOT A QUEUED ACT. A model or permission-mode change waiting on
// a turn is a held entry on the tray, shown as its slash command; an effort
// change must never put a row in front of the reader (owner ruling,
// 2026-10-01), so it goes straight to the shim, whose SetSessionEffort itself
// waits for the running turn to end. A prompt held behind that turn runs at
// the new level, which is what "from the next turn on" means.

const (
	opSetEffort      = "daemon.workspace.set_effort"
	opEffortSettings = "daemon.workspace.effort_settings"
	// effortSettingsWarning keys the warning strip's line for a settings file
	// the daemon could not read.
	effortSettingsWarning = "effort_settings"
)

// ArmEffortNotSupported is a level the selected model was not served as
// accepting, and the shim's own SetSessionEffortFailure.not_supported: the
// SAME spelling on both sides, so the shim's arm relays by name.
const ArmEffortNotSupported = "not_supported"

// SetEffort switches the session's reasoning effort to LEVEL, validated
// against exactly the levels the topbar's selector served.
func (v *verbs) SetEffort(ctx context.Context, ws ids.WorkspaceID, level conversationv1.AgentEffortLevel) error {
	_, log, err := v.owned(ctx, "SetEffort", ws)
	if err != nil {
		return err
	}
	served, ok := v.deps.Cards.EffortLevels(ws)
	if !ok {
		return refuse(log, "SetEffort", ArmEffortNotSupported,
			fmt.Sprintf("workspace %q serves no effort level for its model", ws), false)
	}
	if !slices.Contains(served, level) {
		return refuse(log, "SetEffort", ArmEffortNotSupported,
			fmt.Sprintf("the selector served %v and never offered %v", served, level), false)
	}
	err = v.deps.Sessions.SetEffort(ctx, log, ws, level)
	if refusedNoSession(err) {
		// A PICK WITH NO SHIM STARTS ONE (owner request, 2026-10-29). The
		// topbar holds the pick, which every start puts back in force
		// (reapplyEffort), so the shim started for it runs at the pick.
		v.deps.Topbar.SetPickedEffort(ws, level)
		if err := v.deps.Queue.ReviveForSettings(ctx, ws); err != nil {
			log.Error(opSetEffort, "the effort pick could not start the workspace's shim", dlog.Context{
				"effort": level.String(), "cause": err.Error(),
			})
			return fmt.Errorf("set effort on %q: start the shim: %w", ws, err)
		}
		log.Info(opSetEffort, "the workspace has no shim; the pick is kept and the shim started for it", dlog.Context{"effort": level.String()})
		return nil
	}
	if err != nil {
		return err
	}
	log.Info(opSetEffort, "the effort change landed", dlog.Context{"effort": level.String()})
	return nil
}

// SetEffort asks the workspace's shim for LEVEL and, once the shim confirms
// it, states it to the topbar, which keeps it for the rest of the session.
func (f *Fleet) SetEffort(ctx context.Context, log dlog.Logger, ws ids.WorkspaceID, level conversationv1.AgentEffortLevel) error {
	client, ok := f.Client(ws)
	if !ok {
		log.Info(opSetEffort, "the workspace has no session to set an effort on", dlog.Context{"effort": level.String()})
		return &ShimRefusal{Verb: "SetSessionEffort", Arm: ArmShimNoSession, Detail: "the workspace has no live session"}
	}
	effective, err := applyEffort(ctx, client, level)
	if switchFoundNoShim(err) {
		// A SHIM GONE OR UNREACHABLE HOLDS NO SESSION TO SWITCH, the same
		// answer as no shim at all: one this daemon stood down or that died
		// (recorded where that is observed), or one whose socket could not be
		// dialed (recorded at ERROR by the shim client). The session that
		// replaces it comes up at the pick (reapplyEffort). 2026-11-21
		// 12:56:28: a deploy mediator's shim, stood down by the rollout as
		// its effort was set, was recorded here at ERROR and the deploy's
		// check was never told.
		log.Info(opSetEffort, "the workspace's shim is gone or unreachable; it holds no session to set an effort on now", dlog.Context{
			"effort": level.String(), "cause": err.Error(),
		})
		return fmt.Errorf("set effort on %q: %w", ws, err)
	}
	if err != nil && ctx.Err() != nil {
		// A SWITCH ITS OWN CALLER ABANDONED IS NO REFUSAL: the shim answered
		// nothing (the daemon's exit, a cancelled request).
		log.Info(opSetEffort, "the effort switch was abandoned: its caller's context ended before the shim answered", dlog.Context{
			"effort": level.String(), "cause": err.Error(),
		})
		return fmt.Errorf("set effort on %q: %w", ws, err)
	}
	if refusedNoSession(err) {
		// A SHIM WITH NO SESSION YET (one prelaunched inert, one whose session
		// ended) is the same answer as no shim at all, which the caller turns
		// into a kept pick and a revived session (verbs.SetEffort); it is no
		// failure of the switch (2026-11-17 14:49:38, deploy-fixer: a deploy
		// check's pick met its replacement shim before its session started).
		log.Info(opSetEffort, "the workspace's shim holds no session yet to set an effort on", dlog.Context{
			"effort": level.String(), "cause": err.Error(),
		})
		return err
	}
	if err != nil {
		log.Error(opSetEffort, "the shim did not switch the effort", dlog.Context{
			"effort": level.String(), "cause": err.Error(),
		})
		return fmt.Errorf("set effort on %q: %w", ws, err)
	}
	f.deps.Topbar.SetPickedEffort(ws, effective)
	return nil
}

// switchFoundNoShim reports whether ERR, an effort switch's failure, is the
// switch finding no shim to take it: the shim died or this daemon stood it
// down (shimGone), or its socket could not be dialed.
func switchFoundNoShim(err error) bool {
	return shimGone(err) || errors.Is(err, shimclient.ErrShimUnreachable)
}

// switchFoundNoSession reports whether ERR, an effort switch's failure, means
// no live session took it: none held, a shim with no session yet, or no shim
// to take it (switchFoundNoShim). A recorded pick then stands for the
// session's next start (reapplyEffort).
func switchFoundNoSession(err error) bool {
	return refusedNoSession(err) || switchFoundNoShim(err)
}

// SwitchEffort switches WS's live session to LEVEL OFF THE CALLER'S
// GOROUTINE, under the fleet's lifetime (Detach), and answers at once. The
// caller has recorded LEVEL as the workspace's pick first, so every outcome
// leaves the session at it from its next start on.
//
// THE CALLER NEVER WAITS ON A TURN. The shim holds a switch asked mid-turn
// until that turn ends and answers only then (agent-shim engine/session.ts
// setSessionEffort, engine/boundary-change.ts), which has no bound: a deploy
// check's round that set its mediator's effort waited on the mediator's
// running turn for minutes (2026-11-21 13:55:20). The switch lands at the
// boundary, before the next turn, which is the turn a prompt queued behind
// the running one runs in.
//
// A REFUSAL IS LOUD, NEVER FATAL to the caller: it is recorded at ERROR
// (SetEffort) and stated on the warning strip, the reader's one error
// surface, as a failed put-back is. A switch that found no session, or that
// the daemon's exit cut, is recorded at INFO by SetEffort: the pick stands.
func (f *Fleet) SwitchEffort(log dlog.Logger, ws ids.WorkspaceID, level conversationv1.AgentEffortLevel) {
	f.Detach(ws, func(ctx context.Context) {
		err := f.SetEffort(ctx, log, ws, level)
		switch {
		case err == nil:
			log.Info(opSetEffort, "the effort switch landed", dlog.Context{"effort": level.String()})
		case switchFoundNoSession(err) || ctx.Err() != nil:
			log.Info(opSetEffort, "the effort switch found no session to take it; the pick stands for the session's next start", dlog.Context{"effort": level.String()})
		default:
			f.deps.Topbar.RaiseWarning(ws, opSetEffort,
				fmt.Sprintf("the session could not be switched to effort %s: %v", effortlevel.Word(level), err))
		}
	})
}

// reapplyEffort puts a freshly started shim back at the level a pick put in
// force: the vendor holds the level in the session-scoped flag layer of ONE
// process, and "for the rest of the session" outlives the process.
//
// THE PUSH DOES NOT REPLACE THIS. The shim's effort_changed push states the
// level a new process RUNS at, which is the vendor's default, not the pick;
// only this re-applies the pick. The push then states the result.
//
// A REFUSAL RETRACTS NOTHING SILENTLY. The selector keeps naming the pick,
// so the session would run at a level the strip does not show; the failure is
// logged and stated on the warning strip, the reader's one error surface.
//
// EACH PUT-BACK RECORDS HOW LONG THE SHIM TOOK (elapsed_ms): the call has no
// bound of its own yet, and its bound is sized from these measurements before
// the put-back is detached from its caller's cancel, as the remote-control
// put-back is (owner ruling, 2026-11-17).
func (f *Fleet) reapplyEffort(ctx context.Context, log dlog.Logger, ws ids.WorkspaceID, client shimclientEffort) {
	level, picked := f.deps.Topbar.PickedEffort(ws)
	if !picked {
		return
	}
	asked := f.now()
	_, err := applyEffort(ctx, client, level)
	elapsed := f.now().Sub(asked).Milliseconds()
	if err != nil {
		log.Error(opSetEffort, "the new shim could not be put back at the picked effort", dlog.Context{
			"effort": level.String(), "cause": err.Error(), "elapsed_ms": elapsed,
		})
		f.deps.Topbar.RaiseWarning(ws, opSetEffort,
			fmt.Sprintf("the session restarted and could not be put back at effort %s: %v", effortlevel.Word(level), err))
		return
	}
	log.Info(opSetEffort, "put the new shim back at the picked effort", dlog.Context{"effort": level.String(), "elapsed_ms": elapsed})
}

// shimclientEffort is the slice of the shim client an effort change uses.
type shimclientEffort interface {
	SetSessionEffort(ctx context.Context, req *shimv1.SetSessionEffortRequest) (*shimv1.SetSessionEffortResponse, error)
}

// applyEffort sends one SetSessionEffort and answers the level now in effect,
// or the shim's typed refusal.
func applyEffort(ctx context.Context, client shimclientEffort, level conversationv1.AgentEffortLevel) (conversationv1.AgentEffortLevel, error) {
	response, err := client.SetSessionEffort(ctx, &shimv1.SetSessionEffortRequest{Effort: level})
	if err != nil {
		return conversationv1.AgentEffortLevel_AGENT_EFFORT_LEVEL_UNSPECIFIED, err
	}
	if failure := response.GetFailure(); failure != nil {
		return conversationv1.AgentEffortLevel_AGENT_EFFORT_LEVEL_UNSPECIFIED,
			&ShimRefusal{Verb: "SetSessionEffort", Arm: setEffortArm(failure), Detail: failure.GetDetail()}
	}
	effective := response.GetSuccess().GetEffortChanged().GetEffectiveEffort()
	if effective == conversationv1.AgentEffortLevel_AGENT_EFFORT_LEVEL_UNSPECIFIED {
		// The contract: never UNSPECIFIED. A success that states no level is a
		// broken shim, and a level nobody confirmed is never drawn.
		return effective, fmt.Errorf("the shim answered SetSessionEffort success with no level in effect (response %v)", response)
	}
	return effective, nil
}

// setEffortArm names a SetSessionEffort refusal's arm.
func setEffortArm(failure *shimv1.SetSessionEffortFailure) string {
	switch {
	case failure.GetNotSupported() != nil:
		return ArmEffortNotSupported
	case failure.GetNoSession() != nil:
		return ArmShimNoSession
	case failure.GetVendorRefused() != nil:
		return "vendor_refused"
	default:
		return ArmShimUnspecified
	}
}

// publishEffortSettings reads what the session's config root persists for the
// effort level and states it to the topbar: the selector's current level
// before any pick. Run at registration and on every account switch, because
// the root decides which settings file the vendor reads.
//
// AN UNREADABLE FILE IS STATED, NOT FATAL. The vendor itself skips a settings
// file it cannot parse, so the session still runs; the daemon logs the cause,
// puts it on the warning strip, and states the root as naming no level.
func (v *verbs) publishEffortSettings(log dlog.Logger, record wsm.Workspace, configDir string) {
	settings, err := v.deps.ReadEffortSettings(configDir)
	if err != nil {
		log.Error(opEffortSettings, "the config root's settings file could not be read for its effort level", dlog.Context{
			"workspace": string(record.ID), "config_dir": configDir, "cause": err.Error(),
		})
		v.deps.Topbar.RaiseWarning(record.ID, effortSettingsWarning,
			fmt.Sprintf("could not read the effort level from %s: %v", settings.Path, err))
		settings = claudesettings.Effort{Path: settings.Path}
	}
	log.Info(opEffortSettings, "read the config root's persisted effort level", dlog.Context{
		"workspace":           string(record.ID),
		"path":                settings.Path,
		"effort_level":        effortlevel.Word(settings.Default),
		"model_settings_keys": len(settings.PerModel),
		"env_effort_level":    effortlevel.Word(settings.Env),
		"env_effort_unset":    settings.EnvUnset,
	})
	v.deps.Topbar.SetEffortSettings(record.ID, settings)
}
