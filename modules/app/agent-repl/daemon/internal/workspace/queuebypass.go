package workspace

import (
	"context"
	"errors"
	"fmt"

	workspacev1 "agentrepl/proto/workspace/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/merge"
	"agent-repld/internal/resolve/topbar"
	"agent-repld/internal/wsm"
)

// opQueueBypass is the operation every queue-bypass record is filed under.
const opQueueBypass = "daemon.workspace.queue_bypass"

// The refusal arms UpdateQueueBypassMode answers beyond the shared ones.
const (
	// ArmWorkAccount refuses the mode for a workspace whose session spends
	// as a work account.
	ArmWorkAccount = "work_account"
	// ArmHeldElsewhere refuses the mode while another workspace of the same
	// repository holds it. It carries the holder as `workspace`.
	ArmHeldElsewhere = "held_elsewhere"
	// ArmSwitchFailed is a switch between the workspace's two conversations
	// that did not complete. It carries the failing step as `detail`.
	ArmSwitchFailed = "switch_failed"
)

// QueueBypassChange is what UpdateQueueBypassMode asks of the mode.
type QueueBypassChange int

const (
	// QueueBypassOn takes the mode.
	QueueBypassOn QueueBypassChange = iota + 1
	// QueueBypassOff releases it.
	QueueBypassOff
	// QueueBypassToggle takes it when the workspace does not hold it, and
	// releases it when it does.
	QueueBypassToggle
)

// UpdateQueueBypassMode takes or releases workspace WS's queue bypass mode and
// answers whether the workspace holds it afterwards.
//
// TAKING IT, IN ORDER: the hold is recorded (the store's key refuses a second
// holder of the repository, whatever two callers race), the repository's
// merge queue is HARD-STOPPED so whatever it was running stops at once, and the
// workspace switches to its bypass conversation, whose start reads the hold
// and so comes up with the bypass landing (sessionProfile).
//
// RELEASING IT is the reverse: the hold is cleared first, so the switch back
// starts the ordinary conversation with the queue landing, and the queue is
// resumed last.
//
// THE SWITCH SWAPS TWO CONVERSATIONS. The one the workspace steps out of is
// parked (wsm.SetParkedConversation) and the parked one is bound, through the
// same stop, record, feed reset and start a BindWorkspaceSession runs. A
// workspace with nothing parked yet is taking the mode for the first time:
// its bypass conversation is a FORK of the queue conversation it steps out of
// (owner request, 2026-11-10), so the bypass session continues the
// conversation under its own system prompt. Later switches resume the two
// parked conversations as they stand, so neither picks up the other's turns.
func (v *verbs) UpdateQueueBypassMode(ctx context.Context, ws ids.WorkspaceID, change QueueBypassChange) (bool, error) {
	const rpc = "UpdateQueueBypassMode"
	record, log, err := v.owned(ctx, rpc, ws)
	if err != nil {
		return false, err
	}
	standing, err := v.deps.DB.QueueBypass(ctx, ws)
	if err != nil {
		log.Error(opQueueBypass, "could not read the workspace's queue bypass standing", dlog.Context{"cause": err.Error()})
		return false, fmt.Errorf("queue bypass for %q: read the standing: %w", ws, err)
	}
	want := change == QueueBypassOn || (change == QueueBypassToggle && !standing.Holds)
	log = log.With(dlog.Context{"change": int(change), "holds": standing.Holds, "want": want})
	if want == standing.Holds {
		log.Info(opQueueBypass, "the workspace already stands where the change asks; nothing was changed", nil)
		return want, nil
	}
	if want {
		if standing.HeldBy != "" {
			return false, v.refuseHeldElsewhere(ctx, log, rpc, standing.HeldBy)
		}
		configDir, err := v.accountRoot(ctx, record)
		if err != nil {
			return false, err
		}
		if v.deps.Accounts.IsMultiRepo(configDir) {
			return false, refuseWith(log, rpc, ArmWorkAccount,
				"a work account's workspace never bypasses its repository's merge queue", false,
				map[string]any{"config_dir": configDir})
		}
	}
	if running, live := v.deps.Freeness(ws); live && running.Turn != nil {
		return standing.Holds, refuse(log, rpc, ArmTurnInFlight, "a turn is in flight; end it or interrupt it first", false)
	}

	// THE SWITCH OUTLIVES THE CALLER, exactly as a bind does: a client that
	// hangs up mid-way must not leave the hold and the conversation disagreeing.
	swap := context.WithoutCancel(ctx)
	scope := &merge.RepositoryScope{ID: standing.RepoID}
	pauser := merge.Pauser{By: merge.RequestedByUser}
	if want {
		if err := v.deps.DB.TakeQueueBypass(swap, ws, v.now()); err != nil {
			if errors.Is(err, wsm.ErrQueueBypassHeld) {
				fresh, readErr := v.deps.DB.QueueBypass(swap, ws)
				if readErr == nil && fresh.HeldBy != "" {
					return false, v.refuseHeldElsewhere(swap, log, rpc, fresh.HeldBy)
				}
			}
			log.Error(opQueueBypass, "could not record the workspace's hold on the queue bypass mode", dlog.Context{"cause": err.Error()})
			return false, fmt.Errorf("queue bypass for %q: take the hold: %w", ws, err)
		}
		if err := v.deps.Merge.HardStop(swap, scope, pauser); err != nil {
			log.Error(opQueueBypass, "the repository's merge queue could not be hard-stopped; the hold stands and the switch goes on", dlog.Context{"cause": err.Error()})
			return true, refuseWith(log, rpc, ArmSwitchFailed, fmt.Sprintf("the merge queue could not be hard-stopped: %s", err.Error()), false,
				map[string]any{"detail": "the merge queue could not be hard-stopped: " + err.Error()})
		}
	} else {
		if err := v.deps.DB.ReleaseQueueBypass(swap, ws); err != nil {
			log.Error(opQueueBypass, "could not clear the workspace's hold on the queue bypass mode", dlog.Context{"cause": err.Error()})
			return true, fmt.Errorf("queue bypass for %q: release the hold: %w", ws, err)
		}
	}
	if err := v.switchConversation(swap, log, rpc, ws, standing.ParkedVendorSessionID, want); err != nil {
		v.restateQueueBypass(swap, log, standing.RepoID)
		return want, err
	}
	if !want {
		if err := v.deps.Merge.Unpause(swap, scope); err != nil {
			log.Error(opQueueBypass, "the repository's merge queue could not be resumed after the mode was released", dlog.Context{"cause": err.Error()})
			v.restateQueueBypass(swap, log, standing.RepoID)
			return false, refuseWith(log, rpc, ArmSwitchFailed, fmt.Sprintf("the merge queue could not be resumed: %s", err.Error()), false,
				map[string]any{"detail": "the merge queue could not be resumed: " + err.Error()})
		}
	}
	v.restateQueueBypass(swap, log, standing.RepoID)
	log.Info(opQueueBypass, "the workspace switched its queue bypass mode", dlog.Context{"on": want})
	v.republishRegistry(swap, log, opQueueBypass)
	return want, nil
}

// switchConversation parks the conversation workspace WS runs and binds
// PARKED in its place. With PARKED empty, a switch INTO the mode (TAKING)
// binds a fork of the conversation parked, and any other switch, or one from a
// workspace that ran no conversation, starts fresh.
func (v *verbs) switchConversation(ctx context.Context, log dlog.Logger, rpc string, ws ids.WorkspaceID, parked string, taking bool) error {
	current, err := v.boundConversation(ctx, ws)
	if err != nil {
		log.Error(opQueueBypass, "could not read the conversation the workspace runs", dlog.Context{"cause": err.Error()})
		return fmt.Errorf("queue bypass for %q: %w", ws, err)
	}
	// THE CONVERSATION STEPPED OUT OF IS PARKED BEFORE THE SWAP STOPS IT: the
	// record names it until the swap rewrites the binding.
	if current != "" {
		if err := v.deps.DB.SetParkedConversation(ctx, ws, current); err != nil {
			return refuseWith(log, rpc, ArmSwitchFailed, fmt.Sprintf("the conversation stepped out of could not be kept: %s", err.Error()), false,
				map[string]any{"detail": "the conversation stepped out of could not be kept: " + err.Error()})
		}
	}
	to := conversationBinding{vendorSessionID: parked}
	if parked == "" && taking {
		to.ownForkOf = current
	}
	// AN OWN FORK STARTS AS A REBIND, so the shim adopts the fork's own id as
	// the workspace's book rather than keeping the parked conversation's.
	if err := v.swapConversation(ctx, log, ws, to, to.vendorSessionID != "" || to.ownForkOf != "", nil,
		"the workspace switched between its queue and bypass conversations",
		func(step swapStep, err error) error {
			what := "the other conversation's session would not come up"
			if step == swapStopping {
				what = "the current session would not end"
			}
			return refuseWith(log, rpc, ArmSwitchFailed, fmt.Sprintf("%s: %s", what, err.Error()), false,
				map[string]any{"detail": what + ": " + err.Error()})
		}); err != nil {
		return err
	}
	log.Info(opQueueBypass, "switched the workspace's conversation", dlog.Context{"parked": current, "bound": parked, "own_fork_of": to.ownForkOf})
	return nil
}

// refuseHeldElsewhere refuses the mode while HOLDER holds it.
func (v *verbs) refuseHeldElsewhere(ctx context.Context, log dlog.Logger, rpc string, holder ids.WorkspaceID) error {
	ref := &workspacev1.WorkspaceRef{Id: string(holder)}
	if record, err := v.deps.DB.Workspace(ctx, holder); err == nil {
		ref.Dir = record.Dir
	}
	return refuseWith(log, rpc, ArmHeldElsewhere,
		fmt.Sprintf("workspace %q holds the repository's queue bypass mode", holder), false,
		map[string]any{"workspace": ref})
}

// restateQueueBypass states the queue-bypass chip of every workspace of
// repository REPO to the topbar: taking or releasing the mode changes every
// sibling's chip, not only the holder's.
func (v *verbs) restateQueueBypass(ctx context.Context, log dlog.Logger, repo ids.RepoID) {
	workspaces, err := v.deps.DB.ListWorkspaces(ctx)
	if err != nil {
		log.Error(opQueueBypass, "could not list the workspaces whose queue-bypass chips changed", dlog.Context{"cause": err.Error()})
		return
	}
	for _, record := range workspaces {
		if record.Repo != repo || record.Closed {
			continue
		}
		chip, err := v.queueBypassChip(ctx, record)
		if err != nil {
			log.Error(opQueueBypass, "could not resolve a sibling's queue-bypass chip", dlog.Context{"workspace": string(record.ID), "cause": err.Error()})
			continue
		}
		v.deps.Topbar.SetQueueBypass(record.ID, chip)
	}
}

// queueBypassChip resolves workspace RECORD's queue-bypass chip: offered only
// to a personal account, held here, held by a sibling, or free.
func (v *verbs) queueBypassChip(ctx context.Context, record wsm.Workspace) (topbar.QueueBypass, error) {
	configDir, err := v.accountRoot(ctx, record)
	if err != nil {
		return topbar.QueueBypass{}, err
	}
	if v.deps.Accounts.IsMultiRepo(configDir) {
		return topbar.QueueBypass{}, nil
	}
	standing, err := v.deps.DB.QueueBypass(ctx, record.ID)
	if err != nil {
		return topbar.QueueBypass{}, fmt.Errorf("read the queue bypass standing of %q: %w", record.ID, err)
	}
	chip := topbar.QueueBypass{Offered: true, Holds: standing.Holds}
	if standing.HeldBy != "" {
		holder, err := v.deps.DB.Workspace(ctx, standing.HeldBy)
		if err != nil {
			return topbar.QueueBypass{}, fmt.Errorf("read the queue bypass holder %q: %w", standing.HeldBy, err)
		}
		chip.HeldBy = holder.Name
	}
	return chip, nil
}
