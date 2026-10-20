package workspace

import (
	"context"
	"fmt"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/resolve/footer"
	"agent-repld/internal/wsm"
)

// Close tears a workspace's editor state down. It REQUIRES QUIET — no turn in
// flight, no live work, no held prompts, no queued merge — because undelivered
// user intent is never silently discarded. A standing cold gate or a parked
// session does NOT block: neither holds anything the user still owes an answer
// to.
//
// A refusal MANIFESTS IN THE FOOTER as well as in the answer, so the user sees
// why the workspace would not close where they are looking.
//
// BY says who asked for the close (the user, an agent, a merge, the daemon),
// and the close record keeps it (wsm.MarkClosed).
//
// A WORKSPACE WITH OPEN CHILDREN IS NOT CLOSED: the close is refused with the
// children named (wsm.HasOpenChildrenError, has_open_children).
//
// Closing leaves the SESSION alone: Close is view-level, Kill is what ends a
// session, and the worktree is untouched either way.
func (v *verbs) Close(ctx context.Context, ws ids.WorkspaceID, by wsm.CloseCause) error {
	record, log, err := v.owned(ctx, "CloseWorkspace", ws)
	if err != nil {
		return err
	}

	blocked, err := v.closeBlocker(ctx, ws)
	if err != nil {
		log.Error(opClose, "could not judge whether the workspace is quiet", dlog.Context{"cause": err.Error()})
		return fmt.Errorf("close %q: %w", ws, err)
	}
	if blocked != nil {
		return v.refuseBlockedClose(log, ws, blocked)
	}

	if err := v.deps.DB.MarkClosed(ctx, ws, wsm.Closure{At: v.now(), By: by}); err != nil {
		// A PARENT WITH OPEN CHILDREN STAYS OPEN (wsm/family.go): the refusal
		// is the answer, mapped onto has_open_children.
		if refusedByFamily(log, opClose, err) {
			return err
		}
		log.Error(opClose, "could not record the close", dlog.Context{"cause": err.Error()})
		return fmt.Errorf("close %q: %w", ws, err)
	}
	// The refusal the footer may still be drawing is retired: the workspace
	// closed, so nothing is blocking it any more.
	v.deps.Footer.SetClosing(ws, nil)

	// THE CLOSE IS RECORDED BEFORE THE SINKS ARE EVICTED. This logger writes
	// through the workspace's own durable sink, and eviction releases exactly
	// that: recorded afterwards, the one record explaining why the workspace's
	// log ends here would be written into a sink nobody holds any more, and
	// the workspace's log would simply stop mid-sentence.
	log.Info(opClose, "closed the workspace", dlog.Context{"dir": record.Dir})

	// The workspace's durable log sinks are evicted, which releases the shared
	// descriptors the closed workspace no longer writes through; the canonical
	// links and their targets stay on disk. A failed eviction is a LEAK rather
	// than a correctness failure, so it warns instead of refusing a close the
	// record already says happened.
	if err := v.deps.Log.Evict(record.Dir); err != nil {
		log.Warn(opClose, "could not evict the workspace log sinks", dlog.Context{
			"dir": record.Dir, "cause": err.Error(),
		})
	}

	v.republishRegistry(ctx, log, opClose)
	return nil
}

// refuseBlockedClose stands BLOCKED in the footer and answers the close's
// `blocked` refusal.
func (v *verbs) refuseBlockedClose(log dlog.Logger, ws ids.WorkspaceID, blocked *footer.CloseBlocked) error {
	v.deps.Footer.SetClosing(ws, blocked)
	// `blocked` IS A LANDED CloseWorkspaceError ARM, so this refusal is an
	// ordinary answer the client reads, not a warning.
	log.Info(opClose, "refused a close that is not quiet", dlog.Context{
		"reason": blocked.Reason, "detail": blocked.Detail,
	})
	// THE EVIDENCE RIDES THE ARM (landing 7): a caller with no footer reads
	// why, and `summary` is the SAME composed sentence the footer's activity
	// line draws -- one composer, never two.
	return &Refusal{
		Rpc:    "CloseWorkspace",
		Arm:    "blocked",
		Reason: blocked.Detail,
		Fields: map[string]any{
			"turn_in_flight": blocked.TurnInFlight,
			"live_work":      blocked.LiveWork,
			"held_prompts":   blocked.HeldPrompts,
			"merge_queued":   blocked.MergeQueued,
			"summary":        blocked.Detail,
		},
	}
}

// CloseSelf is a workspace's agent closing its own workspace. The asking turn
// is never a blocker: when everything else is quiet and that turn still runs,
// the workspace is MARKED to close when the turn ends and the answer is true;
// a workspace with no turn running closes now, answering false.
//
// THE MARK IS TAKEN ONCE. The turn may end between the read that saw it
// running and the mark's write, so the turn is read again after the mark: a
// turn that has ended by then leaves the mark to whichever of this call and
// the turn-end edge (SelfCloses) takes it first, and only the taker closes.
func (v *verbs) CloseSelf(ctx context.Context, ws ids.WorkspaceID) (bool, error) {
	_, log, err := v.owned(ctx, "CloseWorkspace", ws)
	if err != nil {
		return false, err
	}
	blocked, err := v.blockedClose(ctx, ws, true)
	if err != nil {
		log.Error(opClose, "could not judge whether the workspace closing itself is quiet", dlog.Context{"cause": err.Error()})
		return false, fmt.Errorf("close %q: %w", ws, err)
	}
	if blocked != nil {
		return false, v.refuseBlockedClose(log, ws, blocked)
	}
	if !v.turnRunning(ws) {
		return false, v.Close(ctx, ws, wsm.CloseByAgent)
	}
	if err := v.deps.DB.MarkCloseWhenTurnEnds(ctx, ws, v.now()); err != nil {
		log.Error(opClose, "could not mark the workspace to close when its asking turn ends", dlog.Context{"cause": err.Error()})
		return false, fmt.Errorf("close %q: mark: %w", ws, err)
	}
	log.Info(opClose, "the workspace asked to close itself; it closes when its asking turn ends", nil)
	if v.turnRunning(ws) {
		return true, nil
	}
	return false, v.takeSelfClose(ctx, log, ws)
}

// takeSelfClose takes WS's self-close mark and, when this caller took it,
// carries the close out. A close that something else now blocks is
// abandoned into the footer's close-blocked state, as a refused close is.
func (v *verbs) takeSelfClose(ctx context.Context, log dlog.Logger, ws ids.WorkspaceID) error {
	taken, err := v.deps.DB.TakeCloseWhenTurnEnds(ctx, ws)
	if err != nil {
		log.Error(opClose, "could not take the workspace's self-close mark", dlog.Context{"cause": err.Error()})
		return fmt.Errorf("close %q: take the mark: %w", ws, err)
	}
	if !taken {
		return nil
	}
	err = v.Close(ctx, ws, wsm.CloseByAgent)
	if _, refused := AsRefusal(err); refused {
		log.Info(opClose, "the workspace's self-close was abandoned: something other than its asking turn now blocks it", dlog.Context{"cause": err.Error()})
		return nil
	}
	return err
}

// turnRunning reports whether a turn runs in WS now.
func (v *verbs) turnRunning(ws ids.WorkspaceID) bool {
	running, live := v.deps.Freeness(ws)
	return live && running.Turn != nil
}
