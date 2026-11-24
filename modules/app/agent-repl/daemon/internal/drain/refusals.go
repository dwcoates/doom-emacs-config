package drain

import (
	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/ratewindow"
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
	// window admits one record per RefusalWindow, the first at once.
	window ratewindow.Window
}

// NoteRefusal records one refused or held submission under the drain lease.
func (c *controller) NoteRefusal(ws ids.WorkspaceID) {
	now := c.deps.Clock.Now()

	c.mu.Lock()
	c.refusals.total++
	c.refusals.window.Every = c.deps.RefusalWindow
	emit, suppressed := c.refusals.window.Admit(now)
	total := c.refusals.total
	c.mu.Unlock()
	if !emit {
		c.log.Debug(opRefusal, "suppressed a drain refusal record inside the rate-limit window",
			dlog.Context{"workspace": string(ws), "suppressed": suppressed, "total": total})
		return
	}

	c.log.Warn(opRefusal, "a submission was refused or held under the drain lease", dlog.Context{
		"workspace":  string(ws),
		"suppressed": suppressed,
		"total":      total,
		"window":     c.deps.RefusalWindow.String(),
	})
}
