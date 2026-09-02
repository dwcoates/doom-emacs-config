package workspace

import (
	"context"
	"fmt"

	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
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
// Closing leaves the SESSION alone: Close is view-level, Kill is what ends a
// session, and the worktree is untouched either way.
func (v *verbs) Close(ctx context.Context, ws ids.WorkspaceID) error {
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
		v.deps.Footer.SetClosing(ws, blocked)
		// `blocked` IS A LANDED CloseWorkspaceError ARM, so this refusal is an
		// ordinary answer the client reads, not a warning.
		log.Info(opClose, "refused a close that is not quiet", dlog.Context{
			"reason": blocked.Reason, "detail": blocked.Detail,
		})
		// THE EVIDENCE RIDES THE ARM (landing 7): a caller with no footer
		// reads why, and `summary` is the SAME composed sentence the footer's
		// activity line draws — one composer, never two.
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

	if err := v.deps.DB.SetClosed(ctx, ws, true); err != nil {
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
