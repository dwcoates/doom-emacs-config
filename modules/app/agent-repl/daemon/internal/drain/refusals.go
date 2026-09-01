package drain

import (
	"time"

	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
)

// refusalWindow is the rate-limited refusal record's state: one WARN per
// window, carrying the exact suppressed count for the window just closed and
// the running total since the daemon came up.
//
// RULED (2026-08-28): repeated refusals under the drain lease log rate-limited
// with exact counts, never a flood. The counts are exact because a suppressed
// refusal still increments them — suppression hides the RECORD, never the
// fact.
type refusalWindow struct {
	// total is every refusal since the controller came up.
	total int
	// suppressed is the refusals since the last emitted record.
	suppressed int
	// openedAt is when the current window began; zero before the first
	// refusal, which is what makes the first one emit immediately.
	openedAt time.Time
}

// NoteRefusal records one refused or held submission under the drain lease.
func (c *controller) NoteRefusal(ws ids.WorkspaceID) {
	now := c.deps.Clock.Now()

	c.mu.Lock()
	c.refusals.total++
	emit := c.refusals.openedAt.IsZero() || now.Sub(c.refusals.openedAt) >= c.deps.RefusalWindow
	if !emit {
		c.refusals.suppressed++
		suppressed, total := c.refusals.suppressed, c.refusals.total
		c.mu.Unlock()
		c.log.Debug(opRefusal, "suppressed a drain refusal record inside the rate-limit window",
			dlog.Context{"workspace": string(ws), "suppressed": suppressed, "total": total})
		return
	}
	suppressed, total := c.refusals.suppressed, c.refusals.total
	c.refusals.suppressed = 0
	c.refusals.openedAt = now
	c.mu.Unlock()

	c.log.Warn(opRefusal, "a submission was refused or held under the drain lease", dlog.Context{
		"workspace":  string(ws),
		"suppressed": suppressed,
		"total":      total,
		"window":     c.deps.RefusalWindow.String(),
	})
}
