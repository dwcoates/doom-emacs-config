package workspace

import (
	"context"
	"errors"
	"fmt"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/wsm"
)

// opSetParent is the operation a retarget's records carry.
const opSetParent = "daemon.workspace.set_parent"

// rpcSetParent is the rpc a retarget's refusals are spelled under.
const rpcSetParent = "SetWorkspaceParent"

// SetParent retargets a workspace's merge: parent (nil: top-level) becomes its
// recorded parent and its merge lands in that parent's checkout, or in the
// repository's main checkout. The change is durable before this returns and
// the roster is republished with the new nesting.
//
// IT TELLS NOBODY. A retarget and its notice to the workspace's session are
// one operation, and internal/retarget is what makes them one; it is the only
// caller, and every surface (the rpc, the command file) goes through it.
func (v *verbs) SetParent(ctx context.Context, ws ids.WorkspaceID, parent *ids.WorkspaceID) (wsm.ParentChange, error) {
	_, log, err := v.owned(ctx, rpcSetParent, ws)
	if err != nil {
		return wsm.ParentChange{}, err
	}
	fields := dlog.Context{"parent": ids.Optional(parent)}
	change, err := v.deps.DB.SetParent(ctx, ws, parent)
	if refused, ok := wsm.AsParentRefused(err); ok {
		return wsm.ParentChange{}, parentRefusal(log, refused)
	}
	if refusedByFamily(log, opSetParent, err) {
		return wsm.ParentChange{}, err
	}
	if errors.Is(err, wsm.ErrNotFound) {
		// owned() read the record a moment ago: a forget raced the retarget.
		return wsm.ParentChange{}, refuse(log, rpcSetParent, ArmUnknownWorkspace,
			fmt.Sprintf("workspace %q was forgotten before its retarget was recorded", ws), true)
	}
	if err != nil {
		log.Error(opSetParent, "could not record the workspace's new parent", dlog.WithCause(fields, err))
		return wsm.ParentChange{}, fmt.Errorf("set parent of %q: %w", ws, err)
	}
	log.Info(opSetParent, "recorded the workspace's new parent and merge target", dlog.Context{
		"parent": ids.Optional(parent), "target_dir": change.After.Dir,
		"previous_parent": ids.Optional(change.Before.Parent), "previous_target_dir": change.Before.Dir,
	})
	v.republishRegistry(ctx, log, opSetParent)
	return change, nil
}

// RestoreParent undoes a retarget whose notice could not be queued, putting
// the prior parent and merge target back unless a later retarget has already
// superseded it, and republishes the roster.
func (v *verbs) RestoreParent(ctx context.Context, change wsm.ParentChange) error {
	_, log, err := v.record(ctx, rpcSetParent, change.Workspace)
	if err != nil {
		return err
	}
	fields := dlog.Context{"parent": ids.Optional(change.Before.Parent), "target_dir": change.Before.Dir,
		"undone_parent": ids.Optional(change.After.Parent), "undone_target_dir": change.After.Dir}
	stale, err := v.deps.DB.RestoreParent(ctx, change)
	if err != nil {
		log.Error(opSetParent, "could not undo a retarget whose notice was not queued", dlog.WithCause(fields, err))
		return fmt.Errorf("restore parent of %q: %w", change.Workspace, err)
	}
	if stale {
		log.Warn(opSetParent, "a retarget whose notice was not queued was superseded before it could be undone; the later retarget stands", fields)
		return nil
	}
	log.Info(opSetParent, "undid a retarget whose notice was not queued", fields)
	v.republishRegistry(ctx, log, opSetParent)
	return nil
}

// parentRefusal turns the store's retarget refusal into the rpc's arm.
func parentRefusal(log dlog.Logger, refused *wsm.ParentRefusedError) *Refusal {
	reason := refused.Error()
	switch refused.Refusal {
	case wsm.ParentUnknown:
		return refuse(log, rpcSetParent, ArmUnknownParent, reason, true)
	case wsm.ParentOtherRepository:
		return refuseWith(log, rpcSetParent, ArmParentInOtherRepository, reason, false,
			map[string]any{"parent_repository_dir": refused.ParentRepositoryDir})
	case wsm.ParentCycle:
		return refuse(log, rpcSetParent, ArmCycle, reason, false)
	case wsm.ParentNoMergeLayout:
		return refuse(log, rpcSetParent, ArmNoMergeLayout, reason, false)
	}
	panic(fmt.Sprintf("workspace: undeclared retarget refusal %s", refused.Refusal))
}

// UnknownParentRefusal is the unknown_parent refusal for a new parent named
// by a directory no workspace is registered at: the command file's spelling
// of a parent, which it resolves before the retarget is asked for. It is
// recorded at INFO, as every refusal is.
func UnknownParentRefusal(log dlog.Logger, dir string, cause error) *Refusal {
	return refuse(log, rpcSetParent, ArmUnknownParent,
		fmt.Sprintf("no workspace is registered at the new parent's directory %q: %v", dir, cause), true)
}
