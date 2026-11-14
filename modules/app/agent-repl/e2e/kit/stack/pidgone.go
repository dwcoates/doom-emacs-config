package stack

import (
	"context"
	"fmt"

	"agent-repld/integration/grantedclock"
	"agent-repld/integration/harness"
)

// AwaitPIDsGone waits, on each one's kernel exit event, for every process in
// PIDS (processes the caller does not reap, so no Wait can block on them) to
// have exited, until CTX ends. A process that has exited and is not reaped
// yet counts as gone: it runs nothing and holds nothing. At CTX's end it
// answers the pids not yet seen to exit, with why CTX ended.
func AwaitPIDsGone(ctx context.Context, pids []int) error {
	for i, pid := range pids {
		if err := harness.WaitProcessExit(ctx, pid); err != nil {
			if ctx.Err() != nil {
				return fmt.Errorf("pids %v still running: %w", pids[i:], grantedclock.Explain(ctx))
			}
			return fmt.Errorf("await pid %d's exit: %w", pid, err)
		}
	}
	return nil
}
