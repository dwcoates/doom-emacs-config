package topbar

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
	fill := float64(contextTokens(s)) / float64(window)
	return min(max(fill, 0), 1)
}
