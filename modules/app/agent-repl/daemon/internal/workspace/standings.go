package workspace

import (
	"context"
	"errors"
	"fmt"

	conversationv1 "agentrepl/proto/conversation/v1"
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
	opTakeBackOneShot   = "daemon.workspace.take_back_one_shot"
	opSetCorrespondence = "daemon.workspace.set_correspondence"
)

// OneShotTakeBack names what took a one-shot back from its agent for the user
// (TakeBackOneShot).
type OneShotTakeBack string

const (
	// TakenBackByUserPrompt is a prompt the user submitted, from the editor
	// or the webapp (promptorigin.UserSent).
	TakenBackByUserPrompt OneShotTakeBack = "user_prompt"
	// TakenBackByReopen is a closed one-shot reopened (OpenWorkspace).
	TakenBackByReopen OneShotTakeBack = "reopen"
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
		// THE DIRECTIVE IS THE DAEMON'S OWN (completionDirective): a corpus
		// that cannot give it is the daemon's fault, not the repository's.
		var err error
		directive, err = v.completionDirective()
		if err != nil {
			log.Error(opSetOneShot, "could not read the daemon's one-shot completion directive", dlog.WithCause(fields, err))
			return "", fmt.Errorf("set one-shot on %q: %w", ws, err)
		}
	}
	// LEAVING THE ONE-SHOT STATE asks the workspace's own agent to file it
	// into a subsection (BriefSubsectionPlacement). The brief is composed
	// BEFORE the mark is written, so a corpus that cannot give it changes
	// nothing.
	var placement string
	if record.OneShot && !oneShot {
		if placement, err = v.oneShotExitPlacement(ctx, log, record); err != nil {
			return "", fmt.Errorf("set one-shot on %q: %w", ws, err)
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
	if placement != "" {
		if err := v.submitPlacement(ctx, log, ws, placement); err != nil {
			return "", fmt.Errorf("set one-shot on %q: %w", ws, err)
		}
	}
	return directive, nil
}

// opOneShotExitPlacement records the placement brief a workspace leaving its
// one-shot state is sent, or why it is sent none.
const opOneShotExitPlacement = "daemon.workspace.one_shot_exit_placement"

// oneShotExitPlacement answers the placement brief RECORD is owed as it leaves
// its one-shot state, "" when it is owed none: a child sits wherever its root
// does and a merge workspace has its own brief (filesItself), and a
// repository with nothing to file into asks nothing (placementOwed).
func (v *verbs) oneShotExitPlacement(ctx context.Context, log dlog.Logger, record wsm.Workspace) (string, error) {
	fields := dlog.Context{"brief": BriefSubsectionPlacement, "repo_id": string(record.Repo)}
	if !filesItself(record.Parent, record.MergeOf != nil) {
		log.Debug(opOneShotExitPlacement, "the workspace sits wherever its family's root does, so it is not asked to file itself", fields)
		return "", nil
	}
	owed, err := v.placementOwed(ctx, log, record)
	if err != nil || !owed {
		return "", err
	}
	return v.corpusNote(log, opOneShotExitPlacement, fields, BriefSubsectionPlacement, nil)
}

// submitPlacement queues the placement brief TEXT to workspace WS's own
// session as a deferred prompt: it waits behind a running turn and wakes an
// idle agent, so the workspace's agent files the workspace itself.
func (v *verbs) submitPlacement(ctx context.Context, log dlog.Logger, ws ids.WorkspaceID, text string) error {
	submission := deferredBrief(ws, text, conversationv1.PromptOrigin_PROMPT_ORIGIN_SUBSECTION_PLACEMENT)
	fields := dlog.Context{"turn": string(submission.Turn)}
	disposition, err := v.deps.Queue.Submit(ctx, submission)
	if err != nil {
		log.Error(opOneShotExitPlacement, "the workspace's queue did not accept its placement brief", dlog.WithCause(fields, err))
		return fmt.Errorf("submit the placement brief: %w", err)
	}
	if disposition.RefusedArm != "" {
		fields["refused_arm"] = disposition.RefusedArm
		log.Error(opOneShotExitPlacement, "the workspace's queue refused its placement brief", fields)
		return fmt.Errorf("submit the placement brief: the queue refused it: %s", disposition.RefusedArm)
	}
	fields["delivered"], fields["parked"] = disposition.Delivered, disposition.Parked()
	log.Info(opOneShotExitPlacement, "asked the workspace that left its one-shot state to file itself into a subsection", fields)
	return nil
}

// TakeBackOneShot clears WS's one-shot mark because the user took the
// workspace back, as BY says: the agent no longer finishes it unattended, so
// no agent has to remember to unmark itself. A workspace that is not marked is
// left as it is, and so is a closed one, whose mark records the one-shot it
// finished: a reopen clears it once the row is open. The clear IS SetOneShot(not one-shot) asked by the user, so
// it republishes the roster, and the sidebar's one-shot subsection lets the
// family go, with no code of its own.
func (v *verbs) TakeBackOneShot(ctx context.Context, ws ids.WorkspaceID, by OneShotTakeBack) error {
	record, log, err := v.owned(ctx, "TakeBackOneShot", ws)
	if err != nil {
		return err
	}
	if !record.OneShot || record.Closed {
		return nil
	}
	if _, err := v.SetOneShot(ctx, ws, nil, false); err != nil {
		log.Error(opTakeBackOneShot, "could not clear the one-shot mark of a workspace the user took back", dlog.Context{
			"cleared_by": string(by), "cause": err.Error()})
		return fmt.Errorf("take back one-shot %q: %w", ws, err)
	}
	log.Info(opTakeBackOneShot, "the user took the one-shot back; its mark is cleared", dlog.Context{"cleared_by": string(by)})
	return nil
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
