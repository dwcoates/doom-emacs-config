package topbar

import (
	"fmt"

	"claude-repld/internal/ids"
)

// THE CONTEXT CHIP'S WINDOW FILL (topbar.proto TopbarContextChip.window_fill;
// design record 2026-10-02 decision 3).

// assumedContextWindow is the window size the chip's fill is taken against
// when the vendor states none: 1,000,000 tokens, the ONE assumed figure the
// owner sanctioned (2026-10-01).
const assumedContextWindow int64 = 1_000_000

// The window's sources, as the edge record names them.
const (
	windowSourceVendor  = "vendor_max_tokens"
	windowSourceAssumed = "assumed_1m"
)

// contextWindow is the size the chip's figure is a fraction of, and where it
// came from: the vendor's usable window for the session's model (the same
// bound its own percentage is against, so the chip and the /context panel
// agree), else the sanctioned assumption.
func contextWindow(s *wsState) (int64, string) {
	if window := s.contextUsage.GetMaxTokens(); window > 0 {
		return window, windowSourceVendor
	}
	return assumedContextWindow, windowSourceAssumed
}

// windowFill is the chip's figure over the window, clamped to [0, 1].
func windowFill(s *wsState) float64 {
	window, _ := contextWindow(s)
	return WindowFill(contextTokens(s), window)
}

// WindowFill is TOKENS over WINDOW, clamped to [0, 1]: the ONE fill every
// surface that colors a token figure by how full the window is takes (the
// context chip, the cold gate's figure, the footer's cold-gate line). A
// window that is not positive is a caller's bug (contextWindow never answers
// one) and panics rather than dividing by it.
func WindowFill(tokens, window int64) float64 {
	if window <= 0 {
		panic(fmt.Sprintf("topbar.WindowFill: window %d is not positive", window))
	}
	fill := float64(tokens) / float64(window)
	return min(max(fill, 0), 1)
}

// ContextWindow answers the window the context chip measures WS's figure
// against: the vendor's stated window, else the sanctioned assumption. The
// cold gate measures its own figure against the same window, so the gate and
// the chip agree on how full one count is.
func (r *resolver) ContextWindow(ws ids.WorkspaceID) int64 {
	r.mu.Lock()
	defer r.mu.Unlock()
	window, _ := contextWindow(r.stateLocked(ws))
	return window
}
