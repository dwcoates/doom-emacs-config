package workspace

import (
	"context"
	"fmt"
	"slices"

	conversationv1 "agentrepl/proto/conversation/v1"
	shimv1 "agentrepl/proto/shim/v1"

	"claude-repld/internal/claudesettings"
	"claude-repld/internal/dlog"
	"claude-repld/internal/effortlevel"
	"claude-repld/internal/ids"
	"claude-repld/internal/wsm"
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
	if err := v.deps.Sessions.SetEffort(ctx, log, ws, level); err != nil {
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
	if err != nil {
		log.Error(opSetEffort, "the shim did not switch the effort", dlog.Context{
			"effort": level.String(), "cause": err.Error(),
		})
		return fmt.Errorf("set effort on %q: %w", ws, err)
	}
	f.deps.Topbar.SetPickedEffort(ws, effective)
	return nil
}

// reapplyEffort puts a freshly started shim back at the level a pick put in
// force: the vendor holds the level in the session-scoped flag layer of ONE
// process, and "for the rest of the session" outlives the process.
//
// A REFUSAL RETRACTS NOTHING SILENTLY. The selector keeps naming the pick,
// so the session would run at a level the strip does not show; the failure is
// logged and stated on the warning strip, the reader's one error surface.
func (f *Fleet) reapplyEffort(ctx context.Context, log dlog.Logger, ws ids.WorkspaceID, client shimclientEffort) {
	level, picked := f.deps.Topbar.PickedEffort(ws)
	if !picked {
		return
	}
	if _, err := applyEffort(ctx, client, level); err != nil {
		log.Error(opSetEffort, "the new shim could not be put back at the picked effort", dlog.Context{
			"effort": level.String(), "cause": err.Error(),
		})
		f.deps.Topbar.RaiseWarning(ws, opSetEffort,
			fmt.Sprintf("the session restarted and could not be put back at effort %s: %v", effortlevel.Word(level), err))
		return
	}
	log.Info(opSetEffort, "put the new shim back at the picked effort", dlog.Context{"effort": level.String()})
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
	})
	v.deps.Topbar.SetEffortSettings(record.ID, settings)
}
