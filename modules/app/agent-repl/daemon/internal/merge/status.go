package merge

import (
	"context"
	"fmt"

	"agent-repld/internal/dlog"
	"agent-repld/internal/wsm"
)

// QueueStatus is one registered repository's queue as it stands.
type QueueStatus struct {
	// Repository is the registered repository the queue belongs to.
	Repository wsm.Repository
	// Queue is its queue, every started member's step filled in.
	Queue QueueSnapshot
}

// Status implements Orchestrator.
func (o *orchestrator) Status(ctx context.Context, scope *RepositoryScope) ([]QueueStatus, error) {
	const op = "daemon.merge.status"
	var repositories []wsm.Repository
	if scope != nil {
		repository, _, err := o.scopedRepository(ctx, op, "a merge queue status", *scope)
		if err != nil {
			return nil, err
		}
		repositories = []wsm.Repository{repository}
	} else {
		all, err := o.deps.DB.ListRepositories(ctx)
		if err != nil {
			o.deps.Log.Global().Error(op, "could not read the repository registry to answer the merge queues' status", dlog.WithCause(nil, err))
			return nil, fmt.Errorf("merge: reading the repository registry: %w", err)
		}
		repositories = all
	}
	out := make([]QueueStatus, 0, len(repositories))
	for _, repository := range repositories {
		key, err := o.repoKeyFor(ctx, repository.Dir)
		if err != nil {
			o.deps.Log.Global().Error(op, "could not resolve a repository's queue key", dlog.WithCause(dlog.Context{"repository": string(repository.ID), "dir": repository.Dir}, err))
			return nil, err
		}
		queue, err := o.readQueue(ctx, o.deps.DB, key)
		if err != nil {
			o.deps.Log.Global().Error(op, "could not read a repository's merge queue", dlog.WithCause(dlog.Context{"repository": string(repository.ID), "repo": string(key)}, err))
			return nil, fmt.Errorf("merge: reading the queue of %s: %w", repository.ID, err)
		}
		o.events.mu.Lock()
		for i := range queue.Members {
			if queue.Members[i].Started {
				queue.Members[i].Step = o.events.steps[queue.Members[i].Workspace]
			}
		}
		o.events.mu.Unlock()
		out = append(out, QueueStatus{Repository: repository, Queue: queue})
	}
	return out, nil
}
