package footer

import (
	"fmt"
	"time"
)

// trimZero drops a trailing ".0" so "18.0k" draws as "18k".
func trimZero(s string) string {
	if len(s) > 2 && s[len(s)-2:] == ".0" {
		return s[:len(s)-2]
	}
	return s
}

// formatLatency renders a first-token latency. Sub-second latencies are the
// common case and are drawn in milliseconds; anything longer reads better in
// seconds.
func formatLatency(d time.Duration) string {
	if d < time.Second {
		return fmt.Sprintf("%dms", d.Milliseconds())
	}
	return trimZero(fmt.Sprintf("%.1f", d.Seconds())) + "s"
}

// truncate shortens a line the daemon draws into a fixed-width row. The
// contract's rule is "what arrives is what is drawn", so the truncation is the
// daemon's and never the client's.
func truncate(s string, max int) string {
	if max <= 1 || len([]rune(s)) <= max {
		return s
	}
	r := []rune(s)
	return string(r[:max-1]) + "…"
}

// epochMs is the one spelling of an instant on this wire.
func epochMs(t time.Time) int64 { return t.UnixMilli() }
