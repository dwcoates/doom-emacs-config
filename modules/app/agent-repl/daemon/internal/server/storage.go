package server

import (
	"cmp"
	"context"
	"errors"
	"slices"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"

	"agent-repld/internal/storagesweep"
)

// StorageSweep is the storage sweep PruneStorage runs (internal/storagesweep).
type StorageSweep interface {
	// Sweep runs one sweep, or with dryRun judges one; it refuses with
	// storagesweep.ErrSweepRunning or storagesweep.ErrHeldElsewhere, and any
	// other error ended the sweep early, already recorded at ERROR.
	Sweep(ctx context.Context, dryRun bool) (storagesweep.Report, error)
}

// PruneStorage runs the storage sweep now, or judges one without deleting.
func (s *server) PruneStorage(
	ctx context.Context,
	req *connect.Request[agentreplv1.PruneStorageRequest],
) (*connect.Response[agentreplv1.PruneStorageResponse], error) {
	report, err := s.deps.Storage.Sweep(ctx, req.Msg.GetDryRun())
	resp := &agentreplv1.PruneStorageResponse{}
	switch {
	case errors.Is(err, storagesweep.ErrSweepRunning):
		resp.Result = pruneStorageError(&agentreplv1.PruneStorageError{Cause: &agentreplv1.PruneStorageError_AlreadyRunning{AlreadyRunning: &agentreplv1.PruneStorageAlreadyRunning{}}})
	case errors.Is(err, storagesweep.ErrHeldElsewhere):
		resp.Result = pruneStorageError(&agentreplv1.PruneStorageError{Cause: &agentreplv1.PruneStorageError_HeldElsewhere{HeldElsewhere: &agentreplv1.PruneStorageHeldElsewhere{}}})
	case err != nil:
		resp.Result = pruneStorageError(&agentreplv1.PruneStorageError{Cause: &agentreplv1.PruneStorageError_Failed{Failed: &agentreplv1.PruneStorageFailed{Detail: err.Error()}}})
	default:
		resp.Result = &agentreplv1.PruneStorageResponse_Success{Success: pruneStorageSuccess(report)}
	}
	return connect.NewResponse(resp), nil
}

// pruneStorageError is an error arm.
func pruneStorageError(e *agentreplv1.PruneStorageError) *agentreplv1.PruneStorageResponse_Error {
	return &agentreplv1.PruneStorageResponse_Error{Error: e}
}

// pruneStorageSuccess renders a sweep's report. The kept counts are ordered
// by the contract's reason order, so the answer reads the same however the
// map iterates.
func pruneStorageSuccess(r storagesweep.Report) *agentreplv1.PruneStorageSuccess {
	out := &agentreplv1.PruneStorageSuccess{DryRun: r.DryRun, Bytes: uint64(r.Bytes()), Failures: uint32(r.Failures)}
	for _, rm := range r.Removed {
		out.Removed = append(out.Removed, &agentreplv1.PruneStorageRemoval{Path: rm.Path, Reason: pruneStorageReason(rm.Reason), Bytes: uint64(rm.Bytes)})
	}
	for reason, n := range r.Kept {
		out.Kept = append(out.Kept, &agentreplv1.PruneStorageKept{Reason: pruneStorageKeepReason(reason), Count: uint32(n)})
	}
	slices.SortFunc(out.Kept, func(a, b *agentreplv1.PruneStorageKept) int { return cmp.Compare(a.Reason, b.Reason) })
	return out
}

// pruneStorageReason is a deletion reason's wire value.
func pruneStorageReason(r storagesweep.Reason) agentreplv1.PruneStorageReason {
	switch r {
	case storagesweep.ReasonNoWorkspace:
		return agentreplv1.PruneStorageReason_PRUNE_STORAGE_REASON_NO_WORKSPACE
	case storagesweep.ReasonLanded:
		return agentreplv1.PruneStorageReason_PRUNE_STORAGE_REASON_LANDED
	case storagesweep.ReasonClosedBackups:
		return agentreplv1.PruneStorageReason_PRUNE_STORAGE_REASON_CLOSED_BACKUPS
	case storagesweep.ReasonAged:
		return agentreplv1.PruneStorageReason_PRUNE_STORAGE_REASON_AGED
	}
	panic("server: a storage sweep deleted for a reason the contract does not draw: " + string(r))
}

// pruneStorageKeepReason is a keep reason's wire value.
func pruneStorageKeepReason(r storagesweep.KeepReason) agentreplv1.PruneStorageKeepReason {
	switch r {
	case storagesweep.KeepOpenWorkspace:
		return agentreplv1.PruneStorageKeepReason_PRUNE_STORAGE_KEEP_REASON_OPEN_WORKSPACE
	case storagesweep.KeepRecentlyClosed:
		return agentreplv1.PruneStorageKeepReason_PRUNE_STORAGE_KEEP_REASON_RECENTLY_CLOSED
	case storagesweep.KeepNotADirectory:
		return agentreplv1.PruneStorageKeepReason_PRUNE_STORAGE_KEEP_REASON_NOT_A_DIRECTORY
	case storagesweep.KeepNoBackups:
		return agentreplv1.PruneStorageKeepReason_PRUNE_STORAGE_KEEP_REASON_NO_BACKUPS
	case storagesweep.KeepYoung:
		return agentreplv1.PruneStorageKeepReason_PRUNE_STORAGE_KEEP_REASON_YOUNG
	case storagesweep.KeepKeptByDirectory:
		return agentreplv1.PruneStorageKeepReason_PRUNE_STORAGE_KEEP_REASON_KEPT_BY_DIRECTORY
	}
	panic("server: a storage sweep kept an entry for a reason the contract does not draw: " + string(r))
}
