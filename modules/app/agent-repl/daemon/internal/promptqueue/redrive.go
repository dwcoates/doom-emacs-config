package promptqueue

import (
	"context"

	"claude-repld/internal/dlog"
)

// redriveKey marks a context whose submission RE-DRIVES an earlier attempt.
//
// THE MARK LIVES HERE, in the lowest package that reads it, and nowhere else:
// prompthandler.WithRedrive sets exactly this key, so the handler and the
// queue read one mark rather than two that could disagree.
type redriveKey struct{}

// WithRedrive marks ctx's submission as a re-drive of an attempt its caller
// already made under the same idempotency key -- the held-prompt ingress's
// resubmission of a prompt a client could not hand to a live daemon.
func WithRedrive(ctx context.Context) context.Context {
	return context.WithValue(ctx, redriveKey{}, true)
}

// IsRedrive reports whether ctx carries WithRedrive's mark.
func IsRedrive(ctx context.Context) bool {
	marked, _ := ctx.Value(redriveKey{}).(bool)
	return marked
}

// refusalLevel answers the level a refusal about a STANDING CONDITION (a merge
// in flight, a cold gate, no session, a move sealed away) is recorded at here.
//
// A live caller's refusal is recorded at level, the arm's own. A RE-DRIVEN
// submission's is DEBUG: its re-driver retries it on a backoff for as long as
// the condition stands, and records the refusal itself, once per entry and
// refusal kind. At level here, one merge produced the same WARN on every retry
// of every held prompt waiting on it.
func refusalLevel(ctx context.Context, log dlog.Logger, level func(operation, message string, fields dlog.Context)) func(operation, message string, fields dlog.Context) {
	if IsRedrive(ctx) {
		return log.Debug
	}
	return level
}
