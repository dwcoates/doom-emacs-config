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
		log.Warn(opClose, "refused a close that is not quiet", dlog.Context{
			"reason": blocked.Reason, "detail": blocked.Detail,
		})
		return &Refusal{Rpc: "CloseWorkspace", Arm: "blocked", Reason: blocked.Detail}
	}

	if err := v.deps.DB.SetClosed(ctx, ws, true); err != nil {
		log.Error(opClose, "could not record the close", dlog.Context{"cause": err.Error()})
		return fmt.Errorf("close %q: %w", ws, err)
	}
	// The refusal the footer may still be drawing is retired: the workspace
	// closed, so nothing is blocking it any more.
	v.deps.Footer.SetClosing(ws, nil)

	// The workspace's durable log sink is evicted, which is what releases the
	// shared fd the closed workspace no longer writes through. Eviction is
	// injected because dlog.Surfaces does not expose it yet; a daemon wired
	// without it simply keeps the sink open, which is a leak and not a
	// correctness failure, so it is not worth refusing the close over.
	if v.deps.EvictLogSink != nil {
		if err := v.deps.EvictLogSink(record.Dir); err != nil {
			log.Warn(opClose, "could not evict the workspace log sink", dlog.Context{
				"dir": record.Dir, "cause": err.Error(),
			})
		} else {
			log.Debug(opClose, "evicted the workspace log sink", dlog.Context{"dir": record.Dir})
		}
	}

	log.Info(opClose, "closed the workspace", dlog.Context{"dir": record.Dir})
	v.republishRegistry(ctx, log, opClose)
	return nil
}
