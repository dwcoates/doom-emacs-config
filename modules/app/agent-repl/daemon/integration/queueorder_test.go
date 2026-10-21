//go:build integration

package integration

import (
	"os"
	"strings"
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

// TestTheDurableQueueIsReadThroughOneHelper holds every integration test that
// reads the merge queue's stored order to durableQueueOrder, so no test
// hand-rolls its own read of the table.
func TestTheDurableQueueIsReadThroughOneHelper(t *testing.T) {
	t.Parallel()
	// Arrange: the suite's TEST sources, which is where the helper and every
	// read it replaces live (the suite has no production source, so
	// sourcescan.Production, which skips tests, does not apply).
	entries, err := os.ReadDir(".")
	if err != nil {
		t.Fatalf("list the suite's sources: %v", err)
	}
	var files []string
	for _, entry := range entries {
		if strings.HasSuffix(entry.Name(), "_test.go") {
			files = append(files, entry.Name())
		}
	}

	// Act
	var sites []string
	for _, file := range files {
		code, err := os.ReadFile(file)
		if err != nil {
			t.Fatalf("read %s: %v", file, err)
		}
		for range strings.Count(string(code), "SELECT workspace_id FROM merge_queue ORDER BY seq") {
			sites = append(sites, file)
		}
	}

	// Assert: the helper's own query, and the literal in this test.
	if want := []string{"merge_test.go", "queueorder_test.go"}; strings.Join(sites, ",") != strings.Join(want, ",") {
		t.Fatalf("queue order reads in %v, want only the helper's in %v", sites, want)
	}
}
