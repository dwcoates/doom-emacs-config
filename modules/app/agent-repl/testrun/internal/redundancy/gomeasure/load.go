package gomeasure

import (
	"context"
	"fmt"

	"agentrepl/testrun/internal/redundancy/proc"
)

// The load gate is proc's (proc/gate.go), with the Go adapter's defaults.
const (
	DefaultMaxLoad   = proc.DefaultMaxLoad
	DefaultQuietWait = proc.DefaultQuietWait
)

// Clock is what the load gate waits by.
type Clock = proc.Clock

func (m *measurer) gate() proc.Gate {
	return proc.Gate{Run: m.run, Log: m.log, MaxLoad: m.cfg.MaxLoad, QuietWait: m.cfg.QuietWait, Clock: m.cfg.Clock}
}

// awaitQuiet waits for a quiet host before the named step.
func (m *measurer) awaitQuiet(ctx context.Context, before string) error {
	if err := m.gate().AwaitQuiet(ctx, before); err != nil {
		return fmt.Errorf("gomeasure: %w", err)
	}
	return nil
}
