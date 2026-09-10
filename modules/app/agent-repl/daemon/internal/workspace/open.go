package workspace

import (
	"context"
	"fmt"

	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
	"claude-repld/internal/resolve/footer"
	"claude-repld/internal/rollout"
)

// Open brings a registered workspace's session up. MOUNTING A PARKED
// WORKSPACE'S FRONTEND IS AN IMPLICIT REVIVAL: there is no shim-less read path,
// so opening spawns rather than serving history from anywhere else.
//
// It is idempotent: opening a workspace whose session is already live clears
// the closed flag and returns, because the mount it answers has already
// happened.
func (v *verbs) Open(ctx context.Context, ws ids.WorkspaceID) error {
	record, log, err := v.owned(ctx, "OpenWorkspace", ws)
	if err != nil {
		return err
	}

	if v.deps.Sessions.Live(ws) {
		log.Debug(opOpen, "the session is already live", nil)
	} else if err := v.deps.Sessions.Start(ctx, ws); err != nil {
		log.Error(opOpen, "the session did not come up", dlog.Context{"cause": err.Error()})
		return fmt.Errorf("open %q: start the session: %w", ws, err)
	}

	if record.Closed {
		if err := v.deps.DB.SetClosed(ctx, ws, false); err != nil {
			log.Error(opOpen, "could not clear the closed flag", dlog.Context{"cause": err.Error()})
			return fmt.Errorf("open %q: clear closed: %w", ws, err)
		}
		log.Debug(opOpen, "cleared the closed flag", nil)
	}

	// A close refusal is drawn in the footer; re-opening retires it, because
	// the state it described is gone.
	v.deps.Footer.SetClosing(ws, nil)

	// The build-staleness check belongs to the mount: a workspace coming up
	// against a shim older than the deployed build is bounced onto it now,
	// rather than discovering the mismatch mid-turn.
	if err := v.deps.Rollout.RelaunchShim(ctx, ws, rollout.ReasonBuildStale); err != nil {
		// A staleness bounce that will not run is a WARNING, not a failed
		// mount: the session is up and usable on the older build.
		log.Warn(opOpen, "the build-staleness check did not bounce the shim", dlog.Context{"cause": err.Error()})
	}

	log.Info(opOpen, "opened the workspace", dlog.Context{"dir": record.Dir})
	v.republishRegistry(ctx, log, opOpen)
	return nil
}

// closeBlocker names why a close is refused, or nil when the workspace is
// quiet. The four blockers are the ruled ones; a standing cold gate and a
// parked session are deliberately NOT among them.
//
// EVERY BLOCKER IS COMPUTED, not just the first one that fires: the refusal
// carries all four counts as evidence (CloseWorkspaceBlocked, landing 7) and
// the footer draws the same composed sentence, so the check answers the whole
// picture and the ORDER below only decides which one the sentence leads with.
func (v *verbs) closeBlocker(ctx context.Context, ws ids.WorkspaceID) (*footer.CloseBlocked, error) {
	blocked := footer.CloseBlocked{}
	var liveDetail string
	if running, live := v.deps.Freeness(ws); live {
		blocked.TurnInFlight = running.Turn != nil
		agents, shells := len(running.LiveWork.Agents), len(running.LiveWork.Shells)
		blocked.LiveWork = uint32(agents + shells)
		liveDetail = fmt.Sprintf("%d detached agents and %d detached shells are still live", agents, shells)
	}
	held, err := v.deps.DB.HeldPrompts(ctx, ws)
	if err != nil {
		return nil, fmt.Errorf("read the held prompts: %w", err)
	}
	blocked.HeldPrompts = uint32(len(held))
	mergeState := ""
	if facts, ok := v.deps.Merge.Facts(ws); ok && mergeIsPending(facts.State) {
		blocked.MergeQueued = true
		mergeState = facts.State
	}

	switch {
	case blocked.TurnInFlight:
		blocked.Reason = "turn_in_flight"
		blocked.Detail = "a turn is still running; interrupt it or wait for it to end"
	case blocked.LiveWork > 0:
		blocked.Reason = "live_work"
		blocked.Detail = liveDetail
	case blocked.HeldPrompts > 0:
		blocked.Reason = "held_prompts"
		blocked.Detail = fmt.Sprintf("%d held prompts have not been delivered; release or drop them first", len(held))
	case blocked.MergeQueued:
		blocked.Reason = "merge_queued"
		blocked.Detail = fmt.Sprintf("a merge is %s; evict it from the queue first", mergeState)
	default:
		return nil, nil
	}
	return &blocked, nil
}

// mergeIsPending reports whether a merge state still owes the workspace work.
// A merge that landed or failed is finished and blocks nothing.
func mergeIsPending(state string) bool {
	switch state {
	case "enqueuing", "queued", "merging", "conflict":
		return true
	default:
		return false
	}
}
