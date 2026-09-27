package prompthandler

import (
	"context"
	"sync"

	"claude-repld/internal/ids"
)

// A DUPLICATE IS ONLY EVER A SUBMISSION THE QUEUE ACCEPTED.
//
// The idempotency claim is durable (wsm.ClaimIdempotencyKey) and is stamped
// accepted only once the queue took the submission (wsm.AcceptIdempotencyKey).
// A claim that never reached that stamp -- the queue call hung, errored, was
// cancelled, or the process died -- is re-driven by the retry rather than
// refused. That rule is only exactly-once if the ORIGINAL submission is not
// still running in this process while its retry is re-driven, so the
// submissions of one key are serialized here: a retry arriving while its
// original is still in flight waits for the original to finish (or for its own
// context to end) and then claims, seeing whatever the original settled.

// inflightKey names one idempotency key in one workspace.
type inflightKey struct {
	ws  ids.WorkspaceID
	key string
}

// inflight serializes the submissions of each idempotency key in this process.
// An entry's channel is closed when its submission ends.
type inflight struct {
	mu   sync.Mutex
	held map[inflightKey]chan struct{}
}

func newInflight() *inflight {
	return &inflight{held: map[inflightKey]chan struct{}{}}
}

// acquire waits until no other submission of key is in flight in this process,
// then takes the key. It answers ctx's error when the caller gives up first,
// and waited reports whether it had to wait at all.
func (f *inflight) acquire(ctx context.Context, ws ids.WorkspaceID, key string) (release func(), waited bool, err error) {
	k := inflightKey{ws: ws, key: key}
	for {
		f.mu.Lock()
		busy, taken := f.held[k]
		if !taken {
			done := make(chan struct{})
			f.held[k] = done
			f.mu.Unlock()
			return func() {
				f.mu.Lock()
				delete(f.held, k)
				f.mu.Unlock()
				close(done)
			}, waited, nil
		}
		f.mu.Unlock()
		waited = true
		select {
		case <-busy:
		case <-ctx.Done():
			return nil, waited, ctx.Err()
		}
	}
}

// claim is one submission's hold on its turn and its idempotency key.
type claim struct {
	// turn is the turn the submission is driven under.
	turn ids.TurnID
	// key is the client's idempotency key; empty for a keyless submission,
	// which has nothing to stamp.
	key string
	// release lets the key's next submission in; nil when nothing was taken.
	release func()
}

// done releases the key, if one was taken.
func (c claim) done() {
	if c.release != nil {
		c.release()
	}
}
