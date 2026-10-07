package ladder

import (
	conversationv1 "agentrepl/proto/conversation/v1"
)

// DegradedWindowOpen reports whether the shim's diagnostics carry an open
// degraded window, which is what makes a serving link read as degraded. It is
// the ONE reading the footer and the roster both take from a diagnostics
// push, so the two cannot disagree about whether the link is degraded.
func DegradedWindowOpen(d *conversationv1.SessionDiagnostics) bool {
	for _, w := range d.GetDegradedWindows() {
		if _, open := w.GetExtent().(*conversationv1.SessionDegradedWindow_Open); open {
			return true
		}
	}
	return false
}
