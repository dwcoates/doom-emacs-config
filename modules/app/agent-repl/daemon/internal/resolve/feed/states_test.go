package feed

import "testing"

// The accumulators hold exactly what the wire cannot restate. Each resolves on
// first sight, because a frame of an upserted unit may be the FIRST one a late
// consumer receives.

func TestUnitResolvesOnFirstSightAndIsStableAfterwards(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.resolver.mu.Lock()
	defer h.resolver.mu.Unlock()
	s := h.resolver.state(testWorkspace)

	// Act.
	first := s.unit("unit-1")
	first.startedAtMs = 1_000
	second := s.unit("unit-1")

	// Assert.
	if second != first {
		t.Fatal("a second lookup minted a new unit rather than resolving the first")
	}
	if second.startedAtMs != 1_000 {
		t.Fatalf("startedAtMs = %d, want the carried fact", second.startedAtMs)
	}
}

func TestProseResolvesOnFirstSightAndIsStableAfterwards(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.resolver.mu.Lock()
	defer h.resolver.mu.Unlock()
	s := h.resolver.state(testWorkspace)

	// Act.
	first := s.prose("unit-1")
	first.markdown = "hello"
	second := s.prose("unit-1")

	// Assert.
	if second != first || second.markdown != "hello" {
		t.Fatal("the prose fold did not resolve to the same accumulation")
	}
}

func TestShellResolvesOnFirstSightAndIsStableAfterwards(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.resolver.mu.Lock()
	defer h.resolver.mu.Unlock()
	s := h.resolver.state(testWorkspace)

	// Act.
	first := s.shell("work-1")
	first.spool = "out"
	second := s.shell("work-1")

	// Assert.
	if second != first || second.spool != "out" {
		t.Fatal("the shell accumulation did not resolve to the same state")
	}
}

func TestSeparateUnitsNeverShareAccumulation(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.resolver.mu.Lock()
	defer h.resolver.mu.Unlock()
	s := h.resolver.state(testWorkspace)

	// Act.
	a := s.unit("unit-1")
	b := s.unit("unit-2")

	// Assert: two blocks arriving at once cannot merge.
	if a == b {
		t.Fatal("two units resolved to one accumulation")
	}
}

func TestADetachmentsSourceFollowsTheReplayednessOfItsPlane(t *testing.T) {
	for _, tc := range []struct {
		plane rowPlane
		want  detachmentSource
	}{
		{planeLive, detachmentLive},
		{planeHistory, detachmentReplayed},
		{planeInherited, detachmentReplayed},
	} {
		t.Run(tc.plane.String(), func(t *testing.T) {
			// Act / Assert.
			if got := detachmentSourceOf(tc.plane); got != tc.want {
				t.Fatalf("source of %s = %s, want %s", tc.plane, got, tc.want)
			}
		})
	}
}

func TestEveryDetachmentSourceHasAName(t *testing.T) {
	for _, tc := range []struct {
		source detachmentSource
		want   string
	}{
		{detachmentLive, "live"},
		{detachmentReplayed, "replayed"},
		{detachmentStanding, "standing"},
		{detachmentStanding + 1, "unknown"},
	} {
		t.Run(tc.want, func(t *testing.T) {
			// Act / Assert.
			if got := tc.source.String(); got != tc.want {
				t.Fatalf("String() = %q, want %q", got, tc.want)
			}
		})
	}
}
