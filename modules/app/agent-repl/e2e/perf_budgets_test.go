//go:build perf

// perf_budgets_test.go — every budget the perf phase asserts, in one place.
//
// PERF-SPEC.md's preamble is the rule these constants live under:
//
//	"Every budget below is PROVISIONAL and must be replaced by a measurement
//	 before the assertion it belongs to is allowed to fail a run."
//
// So each constant below carries its provenance: the spec row it came from,
// whether it is still the spec's provisional figure or a measured one, and —
// where it was measured — the observation it is a stated multiple of. A budget
// with no such note is a number nobody can defend, which is the shape this
// suite refuses everywhere else.
//
// WHERE A MEASUREMENT EXCEEDED THE SPEC'S BUDGET the budget is NOT loosened:
// the assertion fails and PERF-SPEC.md §H records the hop and the margin as a
// production finding. Loosening a budget to fit the measurement is how a perf
// suite stops being one.
package e2e

import "time"

// ---------------------------------------------------------------------------
// Go layer.
// ---------------------------------------------------------------------------

// Row 1a / 2a, Go-observable half — SubmitPrompt issued -> the daemon's ack.
// Spec §F1/§F2 budget: 20/50 ms.
const (
	perfBudgetSubmitAckP50 = 20 * time.Millisecond
	perfBudgetSubmitAckP95 = 50 * time.Millisecond
)

// Row 2b, Go-observable half — SubmitPrompt -> the roster arm leaves idle.
// Spec §C row 2b budget: 30/80 ms, deliberately larger than 2a's because it is
// a full round trip plus a roster recomputation rather than an ack.
const (
	perfBudgetRosterArmP50 = 30 * time.Millisecond
	perfBudgetRosterArmP95 = 80 * time.Millisecond
)

// Row 11a, daemon half — SelectWorkspace issued -> ack landed.
// Spec §C row 11a budget: 20/50 ms.
const (
	perfBudgetSelectAckP50 = 20 * time.Millisecond
	perfBudgetSelectAckP95 = 50 * time.Millisecond
)

// Row 14, the footer flip — a participant leaves -> the footer arm changes.
// Spec §C row 14 budget: 100/300 ms.
const (
	perfBudgetFooterFlipP50 = 100 * time.Millisecond
	perfBudgetFooterFlipP95 = 300 * time.Millisecond
)

// Row 17b — a fresh WatchWorkspaceRoster subscribe -> its replayed frame.
// Spec §C row 17b carries no figure of its own and says why: "the proposed
// 200/500 ms is almost certainly two orders too generous", because
// publish.Topic.Subscribe replays the latest value to a new subscriber and the
// prime ordering guarantees there is one. So this budget is MEASURED, not
// inherited.
const (
	perfBudgetRosterReplayP50 = 20 * time.Millisecond
	perfBudgetRosterReplayP95 = 50 * time.Millisecond
)

// ---------------------------------------------------------------------------
// Webapp layer. Every one of these is a page-side duration (§A4).
// ---------------------------------------------------------------------------

// Row 1b — the send click -> the page's own prompt bubble drawn.
// Spec §F3-adjacent §C row 1b budget: 20/50 ms.
const (
	perfBudgetPromptBubbleP50 = 20 * time.Millisecond
	perfBudgetPromptBubbleP95 = 50 * time.Millisecond
)

// Row 3a — a response frame's arrival -> the response bubble in the DOM.
// Spec §F3 budget: 20/50 ms.
const (
	perfBudgetResponseBubbleP50 = 20 * time.Millisecond
	perfBudgetResponseBubbleP95 = 50 * time.Millisecond
)

// Row 5 — the interrupt confirm click -> the footer arm the frame carries.
// Spec §F5 budget: 30/80 ms.
const (
	perfBudgetInterruptFooterP50 = 30 * time.Millisecond
	perfBudgetInterruptFooterP95 = 80 * time.Millisecond
)

// Row 8a — a question frame's arrival -> the card in the DOM.
// Spec §F8 budget: 30/80 ms.
const (
	perfBudgetQuestionCardP50 = 30 * time.Millisecond
	perfBudgetQuestionCardP95 = 80 * time.Millisecond
)

// Row 11c — a roster frame's arrival -> the sidebar's current row moving.
// Spec §C row 11c budget: 20/50 ms.
const (
	perfBudgetSidebarSelectedP50 = 20 * time.Millisecond
	perfBudgetSidebarSelectedP95 = 50 * time.Millisecond
)
