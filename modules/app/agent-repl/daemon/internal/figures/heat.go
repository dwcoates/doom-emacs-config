package figures

// tokenHeatStops are the fresh-input figures the token gradient's four colors
// sit at -- green, yellow, orange, red -- evenly spaced along the gradient, so
// stop i is position i/3 (frontend.v1 TokenHeat).
var tokenHeatStops = [...]uint64{0, 30_000, 50_000, 100_000}

// TokenHeat maps a fresh-input figure onto the token gradient,
// piecewise-linearly between the stops bracketing it, and holds every figure
// at or past the last stop at red (1). ONE RULE, daemon-wide: the footer's
// tokens cell and a response bubble's corner figure are colored by it alike.
func TokenHeat(fresh uint64) float64 {
	last := len(tokenHeatStops) - 1
	for i := 1; i <= last; i++ {
		if fresh >= tokenHeatStops[i] {
			continue
		}
		lo, hi := tokenHeatStops[i-1], tokenHeatStops[i]
		within := float64(fresh-lo) / float64(hi-lo)
		return (float64(i-1) + within) / float64(last)
	}
	return 1
}
