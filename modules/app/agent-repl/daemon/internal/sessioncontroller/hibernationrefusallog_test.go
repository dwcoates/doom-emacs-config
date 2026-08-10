package sessioncontroller

import "testing"

// THE FIRST REFUSAL IS THE ONE AN OPERATOR DIAGNOSES FROM, so it is never
// suppressed.
func TestHibernationRefusalLimiterReportsTheFirstRefusalInFull(t *testing.T) {
	// Arrange.
	var limiter hibernationRefusalLimiter

	// Act.
	got := limiter.observe("ws", "conversation-cut/cache_expired", 1_000, 10_000)

	// Assert.
	if !got.first || got.total != 1 {
		t.Fatalf("first refusal = %+v, want {first:true total:1}", got)
	}
}

// A RUN INSIDE THE WINDOW IS SILENT, which is the whole point: a standing
// refusal costs one line, not one per sweep tick.
func TestHibernationRefusalLimiterSuppressesInsideTheWindow(t *testing.T) {
	// Arrange.
	var limiter hibernationRefusalLimiter
	limiter.observe("ws", "conversation-cut/cache_expired", 1_000, 10_000)

	// Act.
	got := limiter.observe("ws", "conversation-cut/cache_expired", 5_000, 10_000)

	// Assert.
	if got.first || got.summary {
		t.Fatalf("refusal inside the window = %+v, want no line of its own", got)
	}
}

// AND THE SUMMARY CARRIES EXACT COUNTS, because a count is the only thing the
// thousandth refusal adds to the first.
func TestHibernationRefusalLimiterSummarizesWithExactCounts(t *testing.T) {
	// Arrange — a first line, then three suppressed refusals.
	var limiter hibernationRefusalLimiter
	limiter.observe("ws", "conversation-cut/cache_expired", 1_000, 10_000)
	limiter.observe("ws", "conversation-cut/cache_expired", 2_000, 10_000)
	limiter.observe("ws", "conversation-cut/cache_expired", 3_000, 10_000)
	limiter.observe("ws", "conversation-cut/cache_expired", 4_000, 10_000)

	// Act — the window closes.
	got := limiter.observe("ws", "conversation-cut/cache_expired", 11_001, 10_000)

	// Assert.
	if !got.summary || got.suppressed != 4 || got.total != 5 {
		t.Fatalf("window-closing refusal = %+v, want {summary:true suppressed:4 total:5}", got)
	}
}

// A SECOND SUMMARY COUNTS ONLY WHAT FOLLOWED THE FIRST, so no refusal is
// double-counted and none is lost.
func TestHibernationRefusalLimiterResetsSuppressedPerWindow(t *testing.T) {
	// Arrange.
	var limiter hibernationRefusalLimiter
	limiter.observe("ws", "conversation-cut/cache_expired", 1_000, 10_000)
	limiter.observe("ws", "conversation-cut/cache_expired", 2_000, 10_000)
	limiter.observe("ws", "conversation-cut/cache_expired", 11_001, 10_000)
	limiter.observe("ws", "conversation-cut/cache_expired", 12_000, 10_000)

	// Act.
	got := limiter.observe("ws", "conversation-cut/cache_expired", 22_001, 10_000)

	// Assert.
	if !got.summary || got.suppressed != 2 || got.total != 5 {
		t.Fatalf("second summary = %+v, want {summary:true suppressed:2 total:5}", got)
	}
}

// WORKSPACES DO NOT SHARE A WINDOW, or a busy one would silence a quiet one's
// first and only refusal.
func TestHibernationRefusalLimiterKeepsWorkspacesApart(t *testing.T) {
	// Arrange.
	var limiter hibernationRefusalLimiter
	limiter.observe("ws-a", "conversation-cut/cache_expired", 1_000, 10_000)

	// Act.
	got := limiter.observe("ws-b", "conversation-cut/cache_expired", 1_001, 10_000)

	// Assert.
	if !got.first || got.total != 1 {
		t.Fatalf("first refusal for a second workspace = %+v, want its own full line", got)
	}
}
