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
// suite stops being one. NO ROW HERE HIT THAT CASE — every measured p95 came
// in under its provisional budget, several by more than a factor of ten.
//
// THE RULE EVERY NUMBER BELOW WAS DERIVED BY, applied once and stated once:
//
//	budget = min(the spec's provisional budget, 3 x the measured value)
//
// The `min` is what makes it a one-way ratchet. Three times a measurement is
// this suite's standing multiple of an observed healthy maximum, and it
// TIGHTENS a budget with too much headroom; it may never widen one, because a
// budget widened to fit a measurement asserts nothing about the measurement.
//
// THE MEASUREMENT: three full serial perf runs (`-count=3`) on a 16-core host
// at load average 6.1-8.3, with the calibration guard passing on all three
// (loopback 225µs against a 250µs baseline, cpu probe 42.2ms against 42ms).
// Each row's figure below is the WORST of the three runs, so the ratchet is
// applied to the tail of the spread and not to a lucky run.
package e2e

import "time"

// ---------------------------------------------------------------------------
// Go layer.
// ---------------------------------------------------------------------------

// Row 1a / 2a, Go-observable half — SubmitPrompt issued -> the daemon's ack.
//
// Spec §F1/§F2 budget 20/50 ms. MEASURED p50 7.92/8.48/8.87 ms, p95
// 9.13/9.94/10.32 ms. The p50 keeps the spec's 20 ms (3 x 8.87 = 26.6 ms would
// have WIDENED it, and the ratchet forbids that); the p95 tightens to 3 x
// 10.32 = 31 ms, rounded to 32.
const (
	perfBudgetSubmitAckP50 = 20 * time.Millisecond
	perfBudgetSubmitAckP95 = 32 * time.Millisecond
)

// Row 2b, Go-observable half — SubmitPrompt -> the roster arm leaves idle.
//
// Spec §C row 2b budget 30/80 ms, deliberately larger than 2a's because it is
// a full round trip plus a roster recomputation rather than an ack. MEASURED
// p50 8.15/8.30/8.63 ms, p95 9.13/9.95/10.59 ms — which does NOT bear the
// spec's premise out: the push-carried arm arrives within a millisecond of the
// ack, so 2a and 2b cost the same today. Tightened to 3 x the worst of each:
// 26 ms and 32 ms.
const (
	perfBudgetRosterArmP50 = 26 * time.Millisecond
	perfBudgetRosterArmP95 = 32 * time.Millisecond
)

// Row 11a, daemon half — SelectWorkspace issued -> ack landed.
//
// Spec §C row 11a budget 20/50 ms. MEASURED p50 479/495/497 µs, p95
// 669/829/1170 µs — two orders under the provisional figure, because the verb
// is one map write and a republish. Tightened to 3 x the worst of each: 1.5 ms
// and 3.5 ms.
//
// THIS ROW HAS THE WIDEST RUN-TO-RUN SPREAD IN THE PHASE (its p95 moved 75%
// across three runs, against 6% for most others), which is why its baseline is
// the high-water mark of a -count=3 run rather than one repetition's.
const (
	perfBudgetSelectAckP50 = 1500 * time.Microsecond
	perfBudgetSelectAckP95 = 3500 * time.Microsecond
)

// Row 14, the footer flip — a participant leaves -> the footer says so.
//
// Spec §C row 14 budget 100/300 ms. MEASURED p50 182/191/183 µs, p95
// 224/232/251 µs — nearly three orders under it, and that is not a surprise
// once the restatement is read: the spec's 100/300 ms sized a CLIENT'S OWN
// DETECTION of a dead daemon, which is dominated by streams.ts's 250 ms
// reconnect backoff. What this row measures instead is the daemon's publish
// path for a connectivity change, which no backoff sits on. Tightened to 3 x
// the worst of each: 600 µs and 800 µs.
const (
	perfBudgetFooterFlipP50 = 600 * time.Microsecond
	perfBudgetFooterFlipP95 = 800 * time.Microsecond
)

// Row 17b — a fresh WatchWorkspaceRoster subscribe -> its replayed frame.
//
// §C row 17b carries no figure of its own and predicts the shape of the
// answer: "the proposed 200/500 ms is almost certainly two orders too
// generous", because publish.Topic.Subscribe replays the latest value to a new
// subscriber and the prime ordering guarantees there is one. THE PREDICTION
// HELD, and the margin is the one it named: MEASURED p50 247/236/229 µs, p95
// 663/691/611 µs against a proposed 200/500 ms — 800x and 700x. Set at 3 x the
// worst of each: 800 µs and 2.1 ms.
const (
	perfBudgetRosterReplayP50 = 800 * time.Microsecond
	perfBudgetRosterReplayP95 = 2100 * time.Microsecond
)

// ---------------------------------------------------------------------------
// Webapp layer. Every one of these is a page-side duration (§A4).
// ---------------------------------------------------------------------------

// Row 1b — the send click -> the page's own prompt bubble drawn.
//
// Spec §C row 1b budget 20/50 ms. MEASURED p50 4.31/4.23/4.53 ms, p95
// 6.49/6.06/6.44 ms — a real round trip (the bubble is not optimistic), inside
// jsdom, comfortably inside the provisional figure. Tightened to 3 x the worst
// of each: 14 ms and 20 ms.
const (
	perfBudgetPromptBubbleP50 = 14 * time.Millisecond
	perfBudgetPromptBubbleP95 = 20 * time.Millisecond
)

// Row 3a — a response frame's arrival -> the response bubble in the DOM.
//
// Spec §F3 budget 20/50 ms. MEASURED p50 959/991/921 µs, p95
// 1.66/1.48/1.67 ms. This is the purest hop in the phase — decode plus one
// synchronous DOM write, with no round trip in it — and it comes in at about a
// millisecond. Tightened to 3 x the worst of each: 3 ms and 5 ms.
//
// §C row 3b's warning about instrument granularity is worth carrying forward
// here: at a p50 near 1 ms this measurement is only a few quanta wide even on
// the captured real clock, so row 3b's tighter 5/15 ms budget should be sized
// against THIS distribution when it is built, not proposed independently.
const (
	perfBudgetResponseBubbleP50 = 3 * time.Millisecond
	perfBudgetResponseBubbleP95 = 5 * time.Millisecond
)

// Row 5 — the interrupt click -> the footer arm the frame carries.
//
// Spec §F5 budget 30/80 ms. MEASURED p50 5.50/5.42/5.44 ms, p95
// 6.07/6.37/6.18 ms. Tightened to 3 x the worst of each: 17 ms and 20 ms.
const (
	perfBudgetInterruptFooterP50 = 17 * time.Millisecond
	perfBudgetInterruptFooterP95 = 20 * time.Millisecond
)

// Row 8a — a question frame's arrival -> the card in the DOM.
//
// Spec §F8 budget 30/80 ms. MEASURED p50 3.63/3.39/3.46 ms, p95
// 4.50/4.74/4.05 ms — dearer than row 3a's bare response bubble by about 2.5
// ms, which is the card's controls being built. Tightened to 3 x the worst of
// each: 11 ms and 15 ms.
const (
	perfBudgetQuestionCardP50 = 11 * time.Millisecond
	perfBudgetQuestionCardP95 = 15 * time.Millisecond
)

// Row 11c — a roster frame's arrival -> the sidebar's current row moving.
//
// Spec §C row 11c budget 20/50 ms. MEASURED p50 1.26/1.26/1.28 ms, p95
// 1.53/1.41/1.53 ms, at TWO workspaces in the rail. THE OWNER'S QUESTION —
// "is a workspace switch reflected in the webapp sidebar more-or-less
// instantly?" — is answered yes for the half this layer can see: the marker
// moves about 1.3 ms after the roster frame lands, and that includes the whole
// rail being rebuilt (§C row 21: sidebar.ts replaceChildren's the body per
// push, O(N) by construction). The Emacs-originated half of the switch is
// phase 2. Tightened to 3 x the worst of each: 4 ms and 5 ms.
const (
	perfBudgetSidebarSelectedP50 = 4 * time.Millisecond
	perfBudgetSidebarSelectedP95 = 5 * time.Millisecond
)
