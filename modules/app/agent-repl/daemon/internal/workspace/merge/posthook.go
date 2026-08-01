package merge

import (
	"context"
	"time"
)

// This file is merge.Coordinator's outbound port onto the work that happens
// AFTER a merge has fully landed.
//
// A merged workspace is not the end of the story. When the target worktree is
// itself a linked worktree, the PARENT that spawned the merged child has to be
// told its child landed, and a workspace created with a postprocessing prompt
// has that prompt run in the parent once the merge finishes. Both used to be
// Emacs's job (a headless `claude -p` fired from the editor's merge handler),
// which meant they simply did not happen when no editor was watching.
//
// The port is an interface for the same reason merge.ConflictResolver is: the
// implementation needs the session controller and the workspace-creation
// records, and the merge subsystem must not import either.

// PostMergeHook is invoked by merge.Coordinator EXACTLY ONCE per `merged`
// terminal outcome, and never for merge_failed, a conflict, or an abandoned
// merge.
//
// CONTRACT:
//
//   - It runs only AFTER the queue entry has been dropped and the lease
//     released, so a hook may freely prompt the sessions the merge held.
//   - It runs OFF the drain goroutine. A slow or hung hook must never stall the
//     repository's queue, so the coordinator does not wait for it and the next
//     merge on that repository starts immediately.
//   - It CANNOT un-merge the merge. An error is loud-logged and retained as a
//     merge.PostMergeFailure, never converted into a merge_failed transition:
//     the commits are on the target either way, and claiming otherwise would
//     make the pushed state lie about the tree.
//   - Its context is the COORDINATOR's, not any caller's. A merge outlives the
//     command that asked for it, and so does its aftermath; the hook is
//     cancelled only when the coordinator closes.
type PostMergeHook interface {
	AfterMerged(ctx context.Context, req Request) error
}

// PostMergeFailure is the retained record of one merge.PostMergeHook error.
//
// The loud log is the canonical record, but a log line is not queryable from
// inside the process, and the hook runs off the drain goroutine where no caller
// is left to receive its error. Retaining the failure is what keeps "the merge
// landed but its handoff did not" an observable daemon fact rather than a line
// somebody has to go grep for.
type PostMergeFailure struct {
	// Repo is the queue key the merge drained under.
	Repo string
	// Workspace is the merged workspace's key (merge.Request.Workspace).
	Workspace string
	// Name is the merged workspace's display name (merge.Request.Name).
	Name string
	// At is when the hook reported the failure.
	At time.Time
	// Err is the hook's error, unwrapped and unaltered.
	Err error
}

// maxRetainedPostMergeFailures bounds the retained failure record. Merges are
// human-paced, so the cap is only there to keep a pathologically broken hook
// from growing the record without limit across a long-lived daemon; an
// eviction is loud-logged, and every failure is in the canonical log regardless.
const maxRetainedPostMergeFailures = 64
