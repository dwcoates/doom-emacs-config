package workspace

import (
	"time"

	"agent-repld/internal/startslots"
)

// vendorSlotWaitReported is how long a start waits for a slot before the wait
// is recorded at INFO.
const vendorSlotWaitReported = 2 * time.Second

// startSlotsOf answers the pool the fleet's vendor starts take their slot
// from: the one shared with the bounce registry, or one of the fleet's own.
func startSlotsOf(shared *startslots.Pool) *startslots.Pool {
	if shared != nil {
		return shared
	}
	return startslots.New(startslots.DefaultSize())
}
