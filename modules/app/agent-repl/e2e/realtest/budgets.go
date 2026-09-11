//go:build realtest

package realtest

import (
	"fmt"
	"time"
)

// THE PHASE BUDGETS, AND WHY THEY ARE NOT NUMBERS YET.
//
// docs/REALTEST-PLAN.md and this repo's standing rule on wait bounds
// (AGENTS.md, "Test wait/timeout bounds are measured, not guessed") both say
// the same thing: a bound is a small multiple of the slowest HEALTHY case it
// has to cover, sized from a run that was actually observed. A budget invented
// before the first run is not a bound, it is a guess that will either pass
// everything or fail on nothing to do with the product.
//
// So realtest 1 is a MEASUREMENT run first and a gate second, and the two are
// distinguished in the open:
//
//   - With AGENT_REPL_REALTEST_MEASURE=1 the test performs its one cold start,
//     reports every phase timing at the site, writes the manifest, and
//     enforces the LOG HARVEST — which is the actual remediation bar — while
//     saying in its output that the phase budgets are NOT enforced. Nothing
//     green here can be mistaken for a passed budget.
//
//   - Without it the test enforces the table below, and while any entry is
//     still `unmeasured` it FAILS immediately, naming this file and the phase.
//     A run cannot quietly skip a gate that has no number in it.
//
// A realtest now starts Emacs once per run (owner ruling, 2026-09-11), so one
// run no longer produces a set on its own the way three cold starts used to.
// The set a phase's healthy maximum is sized from is whatever MANIFEST.md
// files the realtest run directory history actually holds — each one a single
// cold start — accumulated across however many authorized runs have happened
// by the time the budget is filled in. Where only one such run exists yet, the
// budget is sized from that one run alone and its Basis says so, rather than
// waiting on a set size this file no longer assumes.

// unmeasured is the sentinel for a budget no run has sized yet.
const unmeasured time.Duration = -1

// Budget is one phase's bound, with the measurement behind it.
type Budget struct {
	Phase PhaseName
	Limit time.Duration
	// Basis is the observed healthy maximum this Limit is a multiple of, and
	// the run it came from. Empty while Limit is `unmeasured`.
	Basis string
}

// budgets is the table. Every phase the reader can measure has a row, so a new
// phase cannot be added without deciding what bounds it.
var budgets = []Budget{
	{Phase: PhaseDoomBoot, Limit: unmeasured},
	{Phase: PhaseModuleLoaded, Limit: unmeasured},
	{Phase: PhaseDaemonSpawned, Limit: unmeasured},
	{Phase: PhaseDaemonAnswered, Limit: unmeasured},
	{Phase: PhaseLinkUp, Limit: unmeasured},
	{Phase: PhaseRosterSubscribed, Limit: unmeasured},
	{Phase: PhaseFirstRoster, Limit: unmeasured},
	{Phase: PhaseTabDrawn, Limit: unmeasured},
	// PhaseWebviewArmed replaces PhasePanelPainted as the hidden-startup
	// gate (owner ruling 2026-09-11; phases.go says why). It is still
	// measured from spawn like every other row here.
	{Phase: PhaseWebviewArmed, Limit: unmeasured},
	// PhaseFocusEdge is spawn to the harness bringing Emacs forward for the
	// key self-test — a real observed edge, but ALSO where the harness's own
	// deliberate hidden-window wait ends. A budget on this row bounds how
	// long the show-phase wait itself took, which is worth watching for
	// harness health, but it is never a proxy for product latency: nothing
	// downstream (PhaseTotal, PhasePanelPainted) is computed by adding to it.
	{Phase: PhaseFocusEdge, Limit: unmeasured},
	// PhasePanelPainted is now measured from PhaseFocusEdge, not spawn (owner
	// ruling 2026-09-11): it is the panel's INTRINSIC paint cost, so a future
	// healthy-maximum measurement for this row is sized from "focus edge to
	// load" (observed around 0.4s), never from "spawn to load" — the latter
	// bakes in however long the harness felt like waiting before it showed
	// Emacs, which is harness overhead the owner never experiences and must
	// never be reported as latency.
	{Phase: PhasePanelPainted, Limit: unmeasured},
	// PhaseTotal is startup-usable: spawn to the latest of tab-drawn,
	// link-up, roster-subscribed, first-roster and webview-armed (phases.go's
	// usableEdges). It EXCLUDES PhasePanelPainted on purpose, so a future
	// healthy-maximum measurement for this row is sized from "spawn to
	// usable" (observed around 2.8s) — the number the owner actually waits
	// on — never from "spawn to shown-and-painted", which would fold the
	// harness's own arbitrary wait before it reveals Emacs into a bound that
	// is supposed to be about the product.
	{Phase: PhaseTotal, Limit: unmeasured},
}

// BudgetFor returns the budget for a phase.
func BudgetFor(phase PhaseName) (Budget, bool) {
	for _, b := range budgets {
		if b.Phase == phase {
			return b, true
		}
	}
	return Budget{}, false
}

// UnmeasuredBudgets names every phase still carrying the sentinel.
func UnmeasuredBudgets() []PhaseName {
	var out []PhaseName
	for _, b := range budgets {
		if b.Limit == unmeasured {
			out = append(out, b.Phase)
		}
	}
	return out
}

// CheckBudgets returns one message per phase over budget, naming the phase, the
// measurement and the bound — which is what the plan asks for: "a phase over
// budget fails naming the phase".
//
// A phase with no budget row is reported as such rather than passed silently.
func CheckBudgets(measurements []Measurement) []string {
	var over []string
	for _, m := range measurements {
		if m.Note != "" {
			continue
		}
		budget, ok := BudgetFor(m.Phase)
		if !ok {
			over = append(over, fmt.Sprintf(
				"phase %s (workspace %s) has no budget row in budgets.go, so nothing bounds it",
				m.Phase, m.Workspace))
			continue
		}
		if budget.Limit == unmeasured {
			continue
		}
		if m.Elapsed > budget.Limit {
			over = append(over, fmt.Sprintf(
				"phase %s (workspace %s) took %s, over its budget of %s (basis: %s)",
				m.Phase, m.Workspace, m.Elapsed.Round(time.Millisecond), budget.Limit, budget.Basis))
		}
	}
	return over
}
