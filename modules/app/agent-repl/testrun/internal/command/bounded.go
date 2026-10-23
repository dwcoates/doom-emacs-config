package command

import (
	"context"
	"errors"
	"fmt"
	"os/exec"
	"strings"
	"time"
)

// WaitDelay is how long a killed bounded command's output pipes are waited
// on, so a grandchild still holding them open cannot hold the caller past
// the bound.
const WaitDelay = time.Second

// Bounded is name with args, killed once ctx is done. Every command testrun
// bounds is started here and judged by Outcome: the RAM disk's disk-manager
// probes. (The reap reads the process table in process, proctable, never
// through a command: a ps blocked in the kernel outlives any bound.)
func Bounded(ctx context.Context, name string, args ...string) *exec.Cmd {
	cmd := exec.CommandContext(ctx, name, args...)
	cmd.WaitDelay = WaitDelay
	return cmd
}

// Line names a command in an error: its name and arguments.
func Line(name string, args []string) string {
	return strings.Join(append([]string{name}, args...), " ")
}

// Unanswered is a command its bound killed before it answered.
type Unanswered struct {
	// Line names the command.
	Line  string
	Bound time.Duration
	// Hint, when set, says what the silence suggests.
	Hint string
	// Cause is why the command was cut off: its context's error.
	Cause error
}

func (e *Unanswered) Error() string {
	msg := fmt.Sprintf("%s did not answer within %s", e.Line, e.Bound)
	if e.Hint != "" {
		msg += "; " + e.Hint
	}
	return msg + ": " + e.Cause.Error()
}

func (e *Unanswered) Unwrap() error { return e.Cause }

// Outcome is a finished bounded command's error: an *Unanswered once ctx's
// deadline has passed, whatever the command answered; else its own failure,
// naming line and carrying what it printed, if anything; else nil.
func Outcome(ctx context.Context, bound time.Duration, line, hint string, printed []byte, err error) error {
	if errors.Is(ctx.Err(), context.DeadlineExceeded) {
		return &Unanswered{Line: line, Bound: bound, Hint: hint, Cause: ctx.Err()}
	}
	if err == nil {
		return nil
	}
	if text := strings.TrimSpace(string(printed)); text != "" {
		return fmt.Errorf("%s: %w: %s", line, err, text)
	}
	return fmt.Errorf("%s: %w", line, err)
}
