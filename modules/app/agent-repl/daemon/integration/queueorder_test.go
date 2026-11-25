//go:build integration

package integration

import (
	"testing"

	"agent-repld/integration/harness"
)

// TestDurableQueueOrderOfAnEmptyQueueIsEmpty covers the helper's own read: a
// daemon with nothing queued answers an empty order after its orderly exit.
func TestDurableQueueOrderOfAnEmptyQueueIsEmpty(t *testing.T) {
	t.Parallel()
	// Arrange
	repo := harness.NewRepo(t)
	repo.DeploysAsAgentRepl()
	d := harness.StartDaemon(t, harness.Opts{DurableState: true})

	// Act
	order := durableQueueOrder(t, d)

	// Assert
	if len(order) != 0 {
		t.Fatalf("durableQueueOrder = %v, want an empty queue", order)
	}
}
