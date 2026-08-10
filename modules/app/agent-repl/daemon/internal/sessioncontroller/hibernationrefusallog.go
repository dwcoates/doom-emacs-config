package sessioncontroller

import "sync"

// hibernationRefusalSummaryIntervalMs is how often a workspace that keeps being
// refused a hibernation gets a counted summary line, in milliseconds. It is the
// client_log refusal limiter's interval (internal/frontend/clientlogrefusal.go)
// expressed on this package's millisecond clock, and it is the same trade: a
// workspace stuck in the refused state costs one line per ten seconds rather
// than one per sweep tick, and an operator watching the log still sees the
// condition while it is happening.
const hibernationRefusalSummaryIntervalMs int64 = 10_000

// hibernationRefusalDecision is what the limiter says about ONE refusal.
//
// The refusal itself is never affected by any of this: the hibernation is still
// refused and nothing is still stopped. This decides only how loudly it is
// reported.
type hibernationRefusalDecision struct {
	// first is set for the first refusal seen for a (workspace, reason) pair —
	// the line that carries the whole evidence, because it is the one an
	// operator diagnoses from.
	first bool
	// summary is set when this refusal closes a summary window, in which case
	// suppressed and total are the counts to report.
	summary bool
	// suppressed is how many refusals for this workspace were reported by no
	// line of their own since the previous summary.
	suppressed int
	// total is every refusal this workspace has produced, first lines
	// included, so no count is lost between summaries.
	total int
}

// hibernationRefusalLimiter rate-limits the REPORTING of refused hibernation
// transitions, per workspace.
//
// WHY: a refusal that is a STANDING condition rather than an event is written
// once per sweep tick for as long as it stands. The cut-conversation gate is
// exactly that — a workspace whose conversation was compacted or cleared is
// refused the cache-expired cause from the moment its cache goes cold until the
// 6h idle cutoff finally reaps it, which is hours of one line per tick. The
// first refusal says everything the thousandth does; what the thousandth adds
// is a COUNT, and a count is what a summary carries.
//
// The zero value is usable, so nothing has to construct it.
type hibernationRefusalLimiter struct {
	mu sync.Mutex
	// seen holds the (workspace, reason) pairs already reported in full.
	seen map[string]struct{}
	// windows is the per-workspace summary state.
	windows map[string]*hibernationRefusalWindow
}

type hibernationRefusalWindow struct {
	suppressed    int
	total         int
	lastSummaryMs int64
}

// observe records one refusal at nowMs and reports how it should be logged.
func (l *hibernationRefusalLimiter) observe(workspace, reason string, nowMs, intervalMs int64) hibernationRefusalDecision {
	if intervalMs <= 0 {
		intervalMs = hibernationRefusalSummaryIntervalMs
	}
	l.mu.Lock()
	defer l.mu.Unlock()
	if l.seen == nil {
		l.seen = map[string]struct{}{}
		l.windows = map[string]*hibernationRefusalWindow{}
	}
	window := l.windows[workspace]
	if window == nil {
		window = &hibernationRefusalWindow{lastSummaryMs: nowMs}
		l.windows[workspace] = window
	}
	window.total++
	key := workspace + "\x00" + reason
	if _, ok := l.seen[key]; !ok {
		l.seen[key] = struct{}{}
		// A full line was emitted for this refusal, so it is not suppressed —
		// but it does open the workspace's window, so the run that follows is
		// summarized rather than reported tick by tick.
		window.lastSummaryMs = nowMs
		return hibernationRefusalDecision{first: true, total: window.total}
	}
	window.suppressed++
	if nowMs-window.lastSummaryMs < intervalMs {
		return hibernationRefusalDecision{total: window.total}
	}
	suppressed := window.suppressed
	window.suppressed = 0
	window.lastSummaryMs = nowMs
	return hibernationRefusalDecision{summary: true, suppressed: suppressed, total: window.total}
}

// noteHibernationRefusal records one refusal on the Manager's limiter, reading
// the clock the rest of this package reads.
func (m *Manager) noteHibernationRefusal(workspace, reason string) hibernationRefusalDecision {
	return m.hibernationRefusals.observe(workspace, reason, m.now(), hibernationRefusalSummaryIntervalMs)
}
