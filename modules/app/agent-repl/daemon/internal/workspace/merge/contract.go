package merge

import "context"

// This file is the FROZEN CONTRACT for the merge subsystem: the interfaces
// every implementation lands behind, declared ahead of the implementations so
// concurrent work (and the integration suite written alongside it) codes
// against fixed signatures rather than against each other's in-flight edits.
//
// Nothing here has behavior. Implementations live beside it.

// Position is where an enqueued merge sits on its repository's queue. It is
// what the daemon pushes to the frontends as merge_queue_position /
// merge_queue_depth on WorkspaceState.
type Position struct {
	// Index is 1-based: the merge currently cherry-picking is 1. Zero means
	// not enqueued, and is never returned by a successful Enqueue.
	Index int
	// Depth is the total number of entries on the repository's queue,
	// including the one at Index.
	Depth int
	// Repo is the queue key: the repository identity resolved from the target
	// worktree, shared by every sibling worktree of that repo.
	Repo string
}

// Coordinator is the per-repository merge singleton. It owns the queue, the
// shim lease, and conflict resolution, and it is the ONLY caller of the git
// cherry-pick layer (merge.Driver).
//
// Serialization is by single ownership, not by locking convention: one owner
// per repository drains its queue, so two cherry-picks against the same target
// worktree are unrepresentable rather than unlikely.
type Coordinator interface {
	// Enqueue admits a merge request onto its repository's queue and returns
	// the position it landed at. It emits merge_queued when the merge is not
	// admitted immediately.
	//
	// It returns as soon as the request is DURABLY enqueued, not when the
	// merge completes: a queued merge cannot report a terminal outcome inline,
	// so completion is observed through the pushed merge state.
	Enqueue(ctx context.Context, req Request) (Position, error)

	// Drain reconstructs the queues from their durable record and resumes
	// draining them. It runs at daemon boot, which is what lets a merge queue
	// survive the daemon bounce that a self-merge of the daemon itself causes.
	Drain(ctx context.Context) error
}

// Lease is the exclusivity claim merge.Coordinator holds over a workspace's
// shim while it drives that workspace's merge.
//
// While the lease is held:
//   - any in-flight USER turn is interrupted,
//   - further user prompts are refused (the frontends explain why rather than
//     silently dropping them),
//   - only the holder may submit prompts, and every conversation item the
//     session produces is stamped CONVERSATION_SOURCE_MERGE.
type Lease interface {
	// Acquire takes the lease for ws and returns the release func. Release is
	// called on EVERY terminal phase — merged, merge_failed, and an abandoned
	// merge_conflict alike — so a lease never outlives the merge that took it.
	//
	// A lease that survives a daemon bounce is reconstructed by
	// Coordinator.Drain rather than silently dropped.
	Acquire(ctx context.Context, ws string) (release func(), err error)

	// Held reports whether the lease on ws is currently held. It is what the
	// prompt path consults to refuse a user submission, and what resolves
	// merge_lease_held on the pushed WorkspaceState.
	Held(ws string) bool
}

// Queue is the durable per-repository request channel merge.Coordinator
// drains. The model is a channel — a producer writes a request, the
// repository's coordinator reads it — but the substrate is durable so a
// daemon bounce loses nothing.
type Queue interface {
	// Publish durably records req for repo and returns its position.
	Publish(ctx context.Context, repo string, req Request) (Position, error)
	// Subscribe returns the repo's request stream plus its cancel func.
	Subscribe(repo string) (<-chan Request, func())
	// Snapshot reports every repo's outstanding entries, for boot-time
	// reconstruction and for resolving queue depth to the frontends.
	Snapshot() map[string][]Request
}

// RepoKeyer resolves the queue key (a repository identity) from a worktree
// directory. Sibling worktrees of one repository MUST resolve to the same key,
// which is the property that gives the repository a single queue.
type RepoKeyer interface {
	RepoKey(ctx context.Context, worktreeDir string) (string, error)
}
