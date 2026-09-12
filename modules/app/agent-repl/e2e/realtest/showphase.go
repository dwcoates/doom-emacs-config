//go:build realtest

package realtest

import (
	"fmt"
	"time"
)

// THE SHOW PHASE, AND WHY IT PRESSES MORE THAN ONCE.
//
// Panels paint on FIRST SHOW, not on hidden launch (owner ruling 2026-09-11,
// docs/REALTEST-JUDGEMENT-CALLS.md row 40): `open -gj` leaves Emacs
// visible-but-unfocused, the settled webview invariant parks pre-creation in
// exactly that state rather than steal focus, and the parked queue drains on
// the first focus edge. So every realtest that asserts a painted panel has to
// PRODUCE that edge, and then wait.
//
// THE 2026-09-12 FINDING. Realtest 8 reported both open workspaces unpainted
// after a 2m wait, where realtest 1 paints in tens of milliseconds. The wait
// itself was never the difference: realtest 8 shows Emacs and calls the same
// `waitForShown` on the same `panel-painted` marker that realtest 1 does. The
// difference is HOW LONG EMACS HOLDS FOCUS, and that is a property of the
// product's drain, not of the assertion:
//
//   - `agent-repl--webview-precreate-drain` (lisp/webview-recovery.el) pops ONE
//     workspace per timer tick, `agent-repl-webview-precreate-stagger-seconds`
//     apart, and re-evaluates `agent-repl--webview-precreate-hold-p` before
//     every single one. The instant Emacs is visible-but-unfocused again, the
//     whole remaining queue re-parks and waits for another focus edge.
//   - The key driver holds Emacs active for about 0.3s per keypress and then
//     restores the previously frontmost application, by design — a realtest
//     must not leave the owner's desktop changed.
//   - Realtest 1's `proveKeyDriver` presses TWO chords and reads Emacs's own
//     `recent-keys` back between them, so it produces two focus edges seconds
//     apart. The old `wsActShowEmacs` pressed ONE harmless `<escape>` and
//     produced exactly one ~0.3s window, after which the harness sat still for
//     two minutes with Emacs unfocused — a state in which the product will
//     never paint the rest of the queue, however long the ceiling is.
//
// So the show phase is one shared helper (showEmacsAndWaitForPaint) used by
// every realtest that asserts a paint, and it re-issues the focus edge while it
// waits instead of producing one and hoping. How many edges it took is
// REPORTED: one edge is the healthy shape, and more than one is a product
// finding about the drain re-parking mid-queue, stated in the manifest rather
// than smoothed over by the retry that revealed it.

const (
	// showEdgeCeiling is how long the panels get to paint after ONE focus
	// edge, before another edge is issued.
	//
	// Sized as a small multiple of the observed healthy paint: realtest 1
	// reports the panel paint cost (focus edge to `watch-load: load-changed`)
	// in the hundreds of milliseconds. Twenty seconds is far above that and
	// still short enough that the whole show phase reports a stall in the time
	// the old single-edge wait spent on its first poll.
	showEdgeCeiling = 20 * time.Second

	// showMaxFocusEdges is how many focus edges the show phase produces before
	// it reports the panels as unpainted.
	//
	// Three rather than one because the drain re-parks per queue item, and
	// three rather than many because a paint that needs a fourth edge is a
	// product defect the owner must see, not something to press through.
	showMaxFocusEdges = 3
)

// showPhaseNote renders what the show phase had to do, in the words the
// manifest carries.
//
// It is pure so the same sentence reaches the test log and the manifest, and so
// the wording of a finding is testable without a running editor.
func showPhaseNote(edges int, painted bool, elapsed time.Duration, expected int) string {
	switch {
	case painted && edges <= 1:
		return fmt.Sprintf("every one of the %d open workspace(s) painted its panel %s after the single focus "+
			"edge this run produced", expected, elapsed.Round(time.Millisecond))
	case painted:
		return fmt.Sprintf("PRODUCT FINDING: the %d open workspace(s) painted only after %d focus edges "+
			"(%s in all). One edge is the healthy shape; the pre-creation drain re-checks its hold before every "+
			"queue item and re-parks the remainder the instant Emacs is visible-but-unfocused again, so a focus "+
			"window shorter than the whole queue's mount cost strands the rest until something focuses Emacs "+
			"again. The harness produced the extra edges so the run could continue; the re-parking is the product's",
			expected, edges, elapsed.Round(time.Millisecond))
	default:
		return fmt.Sprintf("PANELS DID NOT PAINT: after %d focus edge(s) over %s, not all %d open workspace(s) "+
			"had painted. Each edge REQUESTED activation, held the target for the keypress and handed focus "+
			"back; whether the editor actually took focus is a separate reading and, when it did not, the "+
			"NO REAL FOCUS EDGE note says so and this verdict is not the product's. If the edges were real, "+
			"the paint is not waiting on a longer ceiling", edges, elapsed.Round(time.Millisecond), expected)
	}
}

// showPhaseEdgeCeiling is how long to wait after the edge numbered `edge`
// (counting from 1), given the whole show phase may not exceed showCeiling.
//
// The last edge gets whatever is left rather than its own slice, so the phase's
// total bound is still showCeiling and a run does not report a stall earlier
// than the ceiling the plan sized.
func showPhaseEdgeCeiling(edge int, spent time.Duration) time.Duration {
	if edge >= showMaxFocusEdges {
		remaining := showCeiling - spent
		if remaining > showEdgeCeiling {
			return remaining
		}
		return showEdgeCeiling
	}
	return showEdgeCeiling
}
