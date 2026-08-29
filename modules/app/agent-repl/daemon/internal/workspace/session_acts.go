package workspace

import (
	"context"
	"fmt"

	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
	"claude-repld/internal/promptqueue"
)

// The session-act kinds, as the queue's one delivery path names them.
const (
	actSetModel          = "set_model"
	actSetPermissionMode = "set_permission_mode"
)

// SetModel switches the session's model THROUGH THE QUEUE, so the switch cannot
// overtake a prompt the user already queued: a model change that jumped the
// queue would run the queued prompt on a model the user did not choose for it.
func (v *verbs) SetModel(ctx context.Context, ws ids.WorkspaceID, model string) error {
	_, log, err := v.owned(ctx, "SetModel", ws)
	if err != nil {
		return err
	}
	if model == "" {
		return refuse(log, "SetModel", ArmUnservedAnswer, "no model was named", false)
	}
	if err := v.deps.Queue.SubmitSessionAct(ctx, ws, promptqueue.Act{Kind: actSetModel, Value: model}); err != nil {
		log.Error(opSetModel, "the model change was not accepted", dlog.Context{
			"model": model, "cause": err.Error(),
		})
		return fmt.Errorf("set model on %q: %w", ws, err)
	}
	log.Info(opSetModel, "queued the model change", dlog.Context{"model": model})
	return nil
}

// SetPermissionMode switches the permission mode down the same path, validating
// the mode against EXACTLY what the topbar's picker served — the daemon accepts
// only what it offered.
//
// A mode that DISABLES the consent gate additionally needs the consent recorded
// at creation: consenting once, at creation, is what buys an ungated session,
// and the gate is never dropped by a later switch nobody consented to.
func (v *verbs) SetPermissionMode(ctx context.Context, ws ids.WorkspaceID, mode string) error {
	_, log, err := v.owned(ctx, "SetPermissionMode", ws)
	if err != nil {
		return err
	}
	if mode == "" {
		return refuse(log, "SetPermissionMode", ArmModeNotServed, "no permission mode was named", false)
	}

	served, ok := v.deps.Cards.PermissionModes(ws)
	if !ok {
		return refuse(log, "SetPermissionMode", ArmModeNotServed,
			fmt.Sprintf("workspace %q has served no permission-mode picker", ws), false)
	}
	if !contains(served, mode) {
		return refuse(log, "SetPermissionMode", ArmModeNotServed,
			fmt.Sprintf("the picker served %v and never offered %q", served, mode), false)
	}

	if UngatedPermissionModes[mode] {
		job, ok, err := v.deps.DB.CreationJob(ctx, ws)
		if err != nil {
			log.Error(opSetMode, "could not read the recorded consent", dlog.Context{"cause": err.Error()})
			return fmt.Errorf("set permission mode on %q: read the creation job: %w", ws, err)
		}
		if !ok || job.ConsentedUngatedMode != mode {
			return refuse(log, "SetPermissionMode", ArmUngatedWithoutConsent,
				fmt.Sprintf("permission mode %q disables the consent gate and no consent was recorded at creation", mode), false)
		}
	}

	if err := v.deps.Queue.SubmitSessionAct(ctx, ws, promptqueue.Act{Kind: actSetPermissionMode, Value: mode}); err != nil {
		log.Error(opSetMode, "the permission-mode change was not accepted", dlog.Context{
			"mode": mode, "cause": err.Error(),
		})
		return fmt.Errorf("set permission mode on %q: %w", ws, err)
	}
	log.Info(opSetMode, "queued the permission-mode change", dlog.Context{"mode": mode})
	return nil
}

// contains reports membership in a served set.
func contains(set []string, want string) bool {
	for _, s := range set {
		if s == want {
			return true
		}
	}
	return false
}
