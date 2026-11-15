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
	// OffQueue are its merges that left the queue without landing and whose
	// requests still stand, oldest departure first (offqueue.go).
	OffQueue []OffQueueMember
	// Mediation is the deploy mediation the queue waits on, nil while it owes
	// none (owedMediation).
	Mediation *OwedMediation
}

// Status implements Orchestrator.
func (o *orchestrator) Status(ctx context.Context, scope *RepositoryScope) ([]QueueStatus, error) {
	const op = "daemon.merge.status"
	var queues []registeredQueue
	if scope != nil {
		repository, key, err := o.scopedRepository(ctx, op, "a merge queue status", *scope)
		if _, refused := Refused(err); err != nil && !refused {
			o.deps.Log.Global().Error(op, "could not resolve the scoped repository's queue key", dlog.WithCause(dlog.Context{"repository": string(scope.ID), "dir": scope.Dir}, err))
		}
		if err != nil {
			return nil, err
		}
		queues = []registeredQueue{{repository: repository, key: key}}
	} else {
		// ONE UNREADABLE REGISTRATION NEVER FAILS THE REST: a repository
		// whose queue key cannot be resolved (its checkout deleted) is
		// passed over at WARN, as every other sweep of the registry does.
		all, err := o.registeredQueues(ctx, op)
		if err != nil {
			o.deps.Log.Global().Error(op, "could not read the repository registry to answer the merge queues' status", dlog.WithCause(nil, err))
			return nil, fmt.Errorf("merge: reading the repository registry: %w", err)
		}
		queues = all
	}
	// THE MERGES OFF THEIR QUEUES ARE JUDGED NOW: a status read is an
	// agent's, which may wait on git, and it answers what holds now.
	if o.judgeOffQueue(ctx) {
		o.publishOffQueue(ctx)
	}
	out := make([]QueueStatus, 0, len(queues))
	for _, registered := range queues {
		repository, key := registered.repository, registered.key
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
		off, err := o.offQueueOf(ctx, repository)
		if err != nil {
			o.deps.Log.Global().Error(op, "could not read a repository's merges off its queue", dlog.WithCause(dlog.Context{"repository": string(repository.ID)}, err))
			return nil, fmt.Errorf("merge: reading the merges off the queue of %s: %w", repository.ID, err)
		}
		mediation, err := o.owedMediation(ctx, key)
		if err != nil {
			o.deps.Log.Global().Error(op, "could not read the deploy mediation a repository's queue waits on", dlog.WithCause(dlog.Context{"repository": string(repository.ID)}, err))
			return nil, fmt.Errorf("merge: reading the mediation of %s: %w", repository.ID, err)
		}
		out = append(out, QueueStatus{Repository: repository, Queue: queue, OffQueue: off, Mediation: mediation})
	}
	return out, nil
}
