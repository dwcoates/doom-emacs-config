package feed

import (
	"context"
	"strings"
	"testing"

	"agent-repld/internal/ids"
)

func TestPrimingReadsAWorkspacesFactsOnce(t *testing.T) {
	// Arrange.
	store := newFakePullReferences()
	h := newDurableHarness(t, durableStores{pullReferences: store})
	before := store.loads

	// Act: the other workspace is primed three times.
	for range 3 {
		h.resolver.prime(otherWorkspace)
	}

	// Assert.
	if got := store.loads - before; got != 1 {
		t.Fatalf("pull reference reads = %d, want one for the workspace's life", got)
	}
}

func TestAWorkspaceNeededUnprimedPanics(t *testing.T) {
	tests := []struct {
		name string
		use  func(r *resolver)
	}{
		{name: "its state", use: func(r *resolver) { r.state("never-primed") }},
		{name: "its logger", use: func(r *resolver) { r.logger("never-primed") }},
		{name: "its known pull requests", use: func(r *resolver) { r.knownPulls("never-primed") }},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			h := newDurableHarness(t, durableStores{pullReferences: newFakePullReferences()})
			h.resolver.mu.Lock()
			defer h.resolver.mu.Unlock()
			defer func() {
				// Assert.
				got, _ := recover().(string)
				if !strings.Contains(got, "never-primed") || !strings.Contains(got, "without being primed") {
					t.Fatalf("panic = %q, want the unprimed workspace named", got)
				}
			}()

			// Act.
			tt.use(h.resolver)
		})
	}
}

func TestAWorkspaceLoggerACancelledLookupLeftGlobalIsResolvedAgainByTheNextPrime(t *testing.T) {
	// Arrange: the registry's first read is cut by the daemon's context.
	h := newHarness(t)
	reads := 0
	h.resolver.deps.WorkspaceDir = func(ids.WorkspaceID) (string, error) {
		reads++
		if reads == 1 {
			return "", context.Canceled
		}
		return "/tmp/ws", nil
	}
	h.resolver.prime(otherWorkspace)

	// Act.
	h.resolver.prime(otherWorkspace)
	h.resolver.mu.Lock()
	h.resolver.logger(otherWorkspace)
	_, cached := h.resolver.loggers[otherWorkspace]
	h.resolver.mu.Unlock()

	// Assert.
	if reads != 2 || !cached {
		t.Fatalf("registry reads = %d, cached = %v; want the second prime to resolve and keep the sink", reads, cached)
	}
}
