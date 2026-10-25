package server

import (
	"cmp"
	"context"
	"errors"
	"slices"
	"strings"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"

	"agent-repld/internal/worktreereap"
)

// WorktreeReaper is the landed-worktree reaper ReapWorktrees runs
// (internal/worktreereap).
type WorktreeReaper interface {
	// SweepWith runs one sweep shaped by opts; it refuses with
	// worktreereap.ErrSweepRunning or worktreereap.ErrHeldElsewhere, and any
	// other error ended the sweep early, already recorded at ERROR.
	SweepWith(ctx context.Context, opts worktreereap.Options) (worktreereap.Report, error)
}

// ReapWorktrees runs the landed-worktree reaper now, or judges one without
// changing anything, or takes closed workspaces' worktrees without waiting.
func (s *server) ReapWorktrees(
	ctx context.Context,
	req *connect.Request[agentreplv1.ReapWorktreesRequest],
) (*connect.Response[agentreplv1.ReapWorktreesResponse], error) {
	report, err := s.deps.Worktrees.SweepWith(ctx, worktreereap.Options{
		DryRun: req.Msg.GetDryRun(),
		Force:  req.Msg.GetForce(),
	})
	resp := &agentreplv1.ReapWorktreesResponse{}
	switch {
	case errors.Is(err, worktreereap.ErrSweepRunning):
		resp.Result = reapWorktreesError(&agentreplv1.ReapWorktreesError{Cause: &agentreplv1.ReapWorktreesError_AlreadyRunning{AlreadyRunning: &agentreplv1.ReapWorktreesAlreadyRunning{}}})
	case errors.Is(err, worktreereap.ErrHeldElsewhere):
		resp.Result = reapWorktreesError(&agentreplv1.ReapWorktreesError{Cause: &agentreplv1.ReapWorktreesError_HeldElsewhere{HeldElsewhere: &agentreplv1.ReapWorktreesHeldElsewhere{}}})
	case err != nil:
		resp.Result = reapWorktreesError(&agentreplv1.ReapWorktreesError{Cause: &agentreplv1.ReapWorktreesError_Failed{Failed: &agentreplv1.ReapWorktreesFailed{Detail: err.Error()}}})
	default:
		resp.Result = &agentreplv1.ReapWorktreesResponse_Success{Success: reapWorktreesSuccess(report)}
	}
	return connect.NewResponse(resp), nil
}

// reapWorktreesError is an error arm.
func reapWorktreesError(e *agentreplv1.ReapWorktreesError) *agentreplv1.ReapWorktreesResponse_Error {
	return &agentreplv1.ReapWorktreesResponse_Error{Error: e}
}

// reapWorktreesSuccess renders a sweep's report: its removals by rule, in the
// report's order, and its kept counts ordered by the contract's reason order,
// so the answer reads the same however the map iterates.
func reapWorktreesSuccess(r worktreereap.Report) *agentreplv1.ReapWorktreesSuccess {
	out := &agentreplv1.ReapWorktreesSuccess{
		DryRun:          r.DryRun,
		Repositories:    uint32(r.Repositories),
		Worktrees:       uint32(r.Worktrees),
		BranchesDeleted: uint32(r.BranchesDeleted),
		Pruned:          uint32(r.Pruned),
		Failures:        uint32(r.Failures),
	}
	for _, rule := range []struct {
		paths  []string
		reason agentreplv1.ReapWorktreesReason
	}{
		{r.Removed, agentreplv1.ReapWorktreesReason_REAP_WORKTREES_REASON_LANDED},
		{r.Expired, agentreplv1.ReapWorktreesReason_REAP_WORKTREES_REASON_EXPIRED},
		{r.Forced, agentreplv1.ReapWorktreesReason_REAP_WORKTREES_REASON_FORCED},
		{r.Leftovers, agentreplv1.ReapWorktreesReason_REAP_WORKTREES_REASON_LEFTOVER},
	} {
		for _, path := range rule.paths {
			out.Removed = append(out.Removed, &agentreplv1.ReapWorktreesRemoval{Path: path, Reason: rule.reason})
		}
	}
	for reason, n := range r.Kept {
		out.Kept = append(out.Kept, &agentreplv1.ReapWorktreesKept{Reason: reapWorktreesKeepReason(reason), Count: uint32(n)})
	}
	slices.SortFunc(out.Kept, func(a, b *agentreplv1.ReapWorktreesKept) int { return cmp.Compare(a.Reason, b.Reason) })
	return out
}

// reapWorktreesKeepReason is a keep reason's wire value: the reaper's
// snake_case reason (worktreereap.KeepReasons) is the enum value's suffix. One
// the contract does not draw is the reaper and the contract out of step, a
// programming error that TestEveryReaperKeepReasonIsDrawnByTheContract catches
// before it can reach here.
func reapWorktreesKeepReason(reason string) agentreplv1.ReapWorktreesKeepReason {
	wire, ok := agentreplv1.ReapWorktreesKeepReason_value["REAP_WORKTREES_KEEP_REASON_"+strings.ToUpper(reason)]
	if !ok || wire == 0 {
		panic("server: the worktree reaper kept an entry for a reason the contract does not draw: " + reason)
	}
	return agentreplv1.ReapWorktreesKeepReason(wire)
}
