package frontend

import (
	"strings"
	"sync"
	"time"
)

// clientLogRefusalSummaryInterval is how often a workspace that keeps producing
// refusals gets a counted summary line. It is long enough that a webview stuck
// stamping a retired session id costs one line per ten seconds instead of one
// per forwarded record, and short enough that an operator watching the log sees
// the condition while it is still happening.
const clientLogRefusalSummaryInterval = 10 * time.Second

// clientLogRefusalDecision is what the limiter says about ONE refusal.
//
// The refusal itself is never affected by any of this: identity-mismatched
// telemetry is still refused and still never written. This decides only how
// loudly the refusal is reported.
type clientLogRefusalDecision struct {
	// first is set for the first refusal seen for a (workspace, reason) pair.
	// It is the line that carries the whole evidence — got/want identities,
	// request id — because it is the one an operator diagnoses from.
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

// clientLogRefusalLimiter rate-limits the REPORTING of client_log telemetry
// write refusals.
//
// WHY: a webview that outlives its session stamps every forwarded record with
// the retired session id, and each one is refused. Production saw 2,606 warn
// lines in three minutes from that single condition — one per record — which
// drowns the log the refusal exists to be read in. The first refusal of a kind
// says everything the thousandth does; what the thousandth adds is a COUNT, and
// a count is what a summary carries.
type clientLogRefusalLimiter struct {
	now      func() time.Time
	interval time.Duration

	mu sync.Mutex
	// seen holds the (workspace, reason) pairs already reported in full.
	seen map[string]struct{}
	// windows is the per-workspace summary state.
	windows map[string]*clientLogRefusalWindow
}

type clientLogRefusalWindow struct {
	suppressed  int
	total       int
	lastSummary time.Time
}

func newClientLogRefusalLimiter(now func() time.Time, interval time.Duration) *clientLogRefusalLimiter {
	if now == nil {
		now = time.Now
	}
	if interval <= 0 {
		interval = clientLogRefusalSummaryInterval
	}
	return &clientLogRefusalLimiter{
		now:      now,
		interval: interval,
		seen:     map[string]struct{}{},
		windows:  map[string]*clientLogRefusalWindow{},
	}
}

// observe records one refusal and reports how it should be logged.
func (l *clientLogRefusalLimiter) observe(workspace, reason string) clientLogRefusalDecision {
	l.mu.Lock()
	defer l.mu.Unlock()
	now := l.now()
	window := l.windows[workspace]
	if window == nil {
		window = &clientLogRefusalWindow{lastSummary: now}
		l.windows[workspace] = window
	}
	window.total++
	key := workspace + "\x00" + reason
	if _, ok := l.seen[key]; !ok {
		l.seen[key] = struct{}{}
		// A full line was emitted for this refusal, so it is not suppressed —
		// but it does open the workspace's window, so a run that follows it is
		// summarized rather than reported line by line.
		window.lastSummary = now
		return clientLogRefusalDecision{first: true, total: window.total}
	}
	window.suppressed++
	if now.Sub(window.lastSummary) < l.interval {
		return clientLogRefusalDecision{total: window.total}
	}
	suppressed := window.suppressed
	window.suppressed = 0
	window.lastSummary = now
	return clientLogRefusalDecision{summary: true, suppressed: suppressed, total: window.total}
}

// clientLogRefusalReason reduces a refusal message to the part that names WHAT
// was refused, dropping every `key=value` fact from it.
//
// The facts are exactly what varies per record — got/want identities, request
// id, workspace — so keying on the full message would make every record its own
// "first" refusal and rate-limit nothing. Nothing is lost by dropping them: the
// first line of each kind reports the message in full.
func clientLogRefusalReason(message string) string {
	fields := strings.Fields(message)
	kept := fields[:0]
	for _, field := range fields {
		if strings.Contains(field, "=") {
			continue
		}
		kept = append(kept, field)
	}
	return strings.Join(kept, " ")
}
