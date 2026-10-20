package workspace

import (
	"context"
	"errors"
	"fmt"

	workspacev1 "agentrepl/proto/workspace/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/wsm"
)

// A workspace's STANDINGS are what its agent says about where its work
// stands, beside what the daemon observes: whether it is a one-shot, and
// whether it waits on another workspace.

const (
	opSetOneShot        = "daemon.workspace.set_one_shot"
	opSetCorrespondence = "daemon.workspace.set_correspondence"
)

// The standings' refusal arms.
const (
	// ArmClosed is a standing set on a closed workspace.
	ArmClosed = "closed"
	// ArmNoCreationJob is a one-shot mark on a registered workspace, which
	// records no creation to mark.
	ArmNoCreationJob = "no_creation_job"
	// ArmOwnerLands is a one-shot mark on a child its owner lands.
	ArmOwnerLands = "owner_lands"
	// ArmRequesterMayNotSet is a standing asked by a workspace the rule does
	// not let set it.
	ArmRequesterMayNotSet = "requester_may_not_set"
	// ArmPolicyMissing is a one-shot mark whose repository states no
	// completion directive.
	ArmPolicyMissing = "policy_missing"
	// ArmUnknownAwaited is a correspondence mark naming no registered
	// workspace, or the marked workspace itself.
	ArmUnknownAwaited = "unknown_awaited"
)

// SetOneShot marks WS a one-shot, or unmarks it, as ASKER asks (nil for the
// user), and answers the completion directive a marked workspace now follows
// (empty when unmarked). The workspace itself, its owner, or the user may
// ask (wsm.Workspace.MayAct); a child its owner lands is never a one-shot.
func (v *verbs) SetOneShot(ctx context.Context, ws ids.WorkspaceID, asker *ids.WorkspaceID, oneShot bool) (string, error) {
	const rpc = "SetWorkspaceOneShot"
	record, log, err := v.owned(ctx, rpc, ws)
	if err != nil {
		return "", err
	}
	fields := dlog.Context{"one_shot": oneShot, "asker": optionalAsker(asker)}
	if err := v.standingOpen(log, rpc, record); err != nil {
		return "", err
	}
	if !record.MayAct(optionalAsker(asker)) {
		return "", refuse(log, rpc, ArmRequesterMayNotSet, fmt.Sprintf("workspace %s is neither the workspace nor its owner", optionalAsker(asker)), false)
	}
	if oneShot && record.OwnerLands {
		return "", refuseWith(log, rpc, ArmOwnerLands, "a child its owner lands is landed by the owner, so it is never a one-shot", false,
			map[string]any{"owner": &workspacev1.WorkspaceRef{Id: string(*record.Parent)}})
	}
	var directive string
	if oneShot {
		repoDir, err := v.repoDirOf(ctx, record)
		if err != nil {
			log.Error(opSetOneShot, "could not resolve the repository whose completion directive a one-shot follows", dlog.WithCause(fields, err))
			return "", fmt.Errorf("set one-shot on %q: %w", ws, err)
		}
		policy := v.policySourceFor(repoDir)
		directive, err = v.completionDirective(policy)
		if err != nil {
			return "", refuseWith(log, rpc, ArmPolicyMissing, fmt.Sprintf("the repository states no usable completion directive: %v", err), false,
				map[string]any{"policy_dir": policy.Dir})
		}
	}
	err = v.deps.DB.SetOneShot(ctx, ws, oneShot)
	if errors.Is(err, wsm.ErrNotFound) {
		return "", refuse(log, rpc, ArmNoCreationJob, "the workspace was registered rather than created, so it records no creation to mark", false)
	}
	if err != nil {
		log.Error(opSetOneShot, "could not record the workspace's one-shot standing", dlog.WithCause(fields, err))
		return "", fmt.Errorf("set one-shot on %q: %w", ws, err)
	}
	log.Info(opSetOneShot, "recorded the workspace's one-shot standing", fields)
	v.republishRegistry(ctx, log, opSetOneShot)
	return directive, nil
}

// SetCorrespondence marks WS awaiting correspondence from AWAITED, or clears
// the mark with nil, as ASKER asks (nil for the user). Only the workspace
// itself or the user may ask. The footer is told the awaited workspace's
// name, which stands the background arm while no turn runs.
func (v *verbs) SetCorrespondence(ctx context.Context, ws ids.WorkspaceID, asker *ids.WorkspaceID, awaited *ids.WorkspaceID) error {
	const rpc = "SetWorkspaceCorrespondence"
	record, log, err := v.owned(ctx, rpc, ws)
	if err != nil {
		return err
	}
	fields := dlog.Context{"asker": optionalAsker(asker), "awaited": optionalAsker(awaited)}
	if err := v.standingOpen(log, rpc, record); err != nil {
		return err
	}
	if asker != nil && *asker != ws {
		return refuse(log, rpc, ArmRequesterMayNotSet, fmt.Sprintf("workspace %s is not the workspace", *asker), false)
	}
	name := ""
	if awaited != nil {
		target, err := v.deps.DB.Workspace(ctx, *awaited)
		switch {
		case *awaited == ws, errors.Is(err, wsm.ErrNotFound):
			return refuseWith(log, rpc, ArmUnknownAwaited, fmt.Sprintf("workspace %s is no other registered workspace to await", *awaited), true,
				map[string]any{"awaited": &workspacev1.WorkspaceRef{Id: string(*awaited)}})
		case err != nil:
			log.Error(opSetCorrespondence, "could not read the awaited workspace", dlog.WithCause(fields, err))
			return fmt.Errorf("set correspondence on %q: %w", ws, err)
		}
		name = target.Name
	}
	if err := v.deps.DB.SetAwaiting(ctx, ws, awaited); err != nil {
		log.Error(opSetCorrespondence, "could not record what the workspace awaits", dlog.WithCause(fields, err))
		return fmt.Errorf("set correspondence on %q: %w", ws, err)
	}
	v.deps.Footer.SetAwaiting(ws, name)
	log.Info(opSetCorrespondence, "recorded what the workspace awaits", fields)
	return nil
}

// restoreAwaiting tells the footer what a registering workspace awaits, so a
// restarted daemon draws a workspace still waiting in the background.
func (v *verbs) restoreAwaiting(ctx context.Context, log dlog.Logger, record wsm.Workspace) error {
	if record.Awaiting == nil {
		return nil
	}
	awaited, err := v.deps.DB.Workspace(ctx, *record.Awaiting)
	if err != nil {
		log.Error(opSetCorrespondence, "could not read the workspace a registering workspace awaits", dlog.WithCause(dlog.Context{"awaited": string(*record.Awaiting)}, err))
		return fmt.Errorf("restore what %q awaits: %w", record.ID, err)
	}
	v.deps.Footer.SetAwaiting(record.ID, awaited.Name)
	return nil
}

// standingOpen refuses a standing on a closed workspace.
func (v *verbs) standingOpen(log dlog.Logger, rpc string, record wsm.Workspace) error {
	if record.Closed {
		return refuse(log, rpc, ArmClosed, "a closed workspace runs no agent whose work could stand anywhere", false)
	}
	return nil
}

// optionalAsker is ASKER's id, empty for the user.
func optionalAsker(asker *ids.WorkspaceID) ids.WorkspaceID {
	if asker == nil {
		return ""
	}
	return *asker
}
