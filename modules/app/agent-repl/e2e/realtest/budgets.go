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
	// doom-boot: healthy max 2.627s, realtest-20260911-154945 (n=25
	// cold-start observations across every MANIFEST.md under
	// ~/.claude-emacs/realtest/ as of 2026-09-12). 3x per AGENTS.md's
	// standing wait/timeout rule.
	{Phase: PhaseDoomBoot, Limit: 7881 * time.Millisecond,
		Basis: "healthy max 2.627s, realtest-20260911-154945 (n=25); 3x ~= 7.881s"},
	// module-loaded: healthy max 2.967s, realtest-20260911-154945 (n=21).
	//
	// THIS ROW WAS `unmeasured` UNTIL THE MARKER WAS CORRECTED, and it read
	// n=0 for a reason that had nothing to do with the phase: the reader
	// matched `elisp.daemon.ensure-command`, a record only the interactive
	// retry writes, while the startup path writes
	// `elisp.daemon.ensure-scheduled` (phases.go says why). The phase had
	// been happening, and being logged, in every run all along.
	//
	// The maximum above is measured, not derived. `ensure-scheduled` appears
	// in the Messages.txt of 22 of the 23 MANIFEST.md runs under
	// ~/.claude-emacs/realtest/; 21 of those are single-cold-start runs whose
	// manifest `spawned at` is the spawn that produced the record, and the
	// slowest of the 21 is realtest-20260911-154945 at 2.967s from spawn.
	// (realtest-20260911-121357 is excluded: it is a three-cold-start run
	// from the old regime, so its single Messages.txt belongs to a later
	// spawn than its first manifest entry and the difference is not a
	// latency.) The measurement is corroborated intra-process: across all 22,
	// `ensure-scheduled` lands 0.204s-0.340s after the module's first record,
	// and 2.627s (that run's doom-boot max) + 0.340s is the same 2.967s.
	// 3x per AGENTS.md's standing wait/timeout rule.
	{Phase: PhaseModuleLoaded, Limit: 8901 * time.Millisecond,
		Basis: "healthy max 2.967s, realtest-20260911-154945 (n=21); 3x ~= 8.901s"},
	// daemon-spawned: healthy max 3.201s, realtest-20260911-154945 (n=21).
	{Phase: PhaseDaemonSpawned, Limit: 9603 * time.Millisecond,
		Basis: "healthy max 3.201s, realtest-20260911-154945 (n=21); 3x ~= 9.603s"},
	// daemon-answered: healthy max 3.474s, realtest-20260911-154945 (n=24 —
	// includes the one run, realtest-20260911-200208, where a LATER cold
	// start's daemon never answered at all; that failure produced no
	// numeric daemon-answered observation to fold in, so it does not
	// inflate this max).
	{Phase: PhaseDaemonAnswered, Limit: 10422 * time.Millisecond,
		Basis: "healthy max 3.474s, realtest-20260911-154945 (n=24); 3x ~= 10.422s"},
	// link-up: healthy max 3.465s, realtest-20260911-154945 (n=21).
	{Phase: PhaseLinkUp, Limit: 10395 * time.Millisecond,
		Basis: "healthy max 3.465s, realtest-20260911-154945 (n=21); 3x ~= 10.395s"},
	// roster-subscribed: healthy max 3.474s, realtest-20260911-154945 (n=21).
	{Phase: PhaseRosterSubscribed, Limit: 10422 * time.Millisecond,
		Basis: "healthy max 3.474s, realtest-20260911-154945 (n=21); 3x ~= 10.422s"},
	// first-roster: DERIVED from tab-drawn, not measured, because this
	// phase's marker has genuinely never been written to a file: roster.el
	// emitted `elisp.roster.reconcile:` at DEBUG, below the durable sink's
	// default level, so the run history holds no observation of it to size a
	// bound from (phases.go says why, and roster.el now emits it at INFO).
	//
	// THE DERIVATION. The reconcile record is written by
	// `agent-repl-roster-reconcile` immediately after the tab walk, in the
	// same synchronous call, from the same roster push that produces every
	// `elisp.roster.tab-open` record. So first-roster lands after the LAST
	// tab-open of the first push and before anything else that push triggers:
	// it is tab-drawn's healthy maximum plus the walk's tail, which is the
	// teardown pass and one `setq` over an already-built list. That tail is
	// far below the millisecond the timestamps resolve to — in the
	// realtest-20260911-154945 log the whole walk, both tab-opens included,
	// spans 3ms — so tab-drawn's healthy maximum is taken as this phase's,
	// unrounded: 3.479s, realtest-20260911-154945 (n=40). 3x ~= 10.437s.
	//
	// A derived bound is a stand-in for a measured one. The first authorized
	// run after this change observes the phase directly; replace this row's
	// Basis with that measurement, and if the observation exceeds 3.479s the
	// derivation was wrong rather than the product being slow.
	{Phase: PhaseFirstRoster, Limit: 10437 * time.Millisecond,
		Basis: "derived from tab-drawn healthy max 3.479s, realtest-20260911-154945 (n=40); the reconcile record is written in the same call immediately after the tab walk; 3x ~= 10.437s"},
	// tab-drawn: healthy max 3.479s, realtest-20260911-154945, workspace
	// 2b81f45a724642ef (n=40 — two workspaces per cold start across 20
	// runs that had any open workspace to draw a tab for).
	{Phase: PhaseTabDrawn, Limit: 10437 * time.Millisecond,
		Basis: "healthy max 3.479s, realtest-20260911-154945 (n=40); 3x ~= 10.437s"},
	// PhaseWebviewArmed replaces PhasePanelPainted as the hidden-startup
	// gate (owner ruling 2026-09-11; phases.go says why). It is still
	// measured from spawn like every other row here.
	//
	// webview-armed: healthy max 2.757s, realtest-20260911-184347 (n=17).
	{Phase: PhaseWebviewArmed, Limit: 8271 * time.Millisecond,
		Basis: "healthy max 2.757s, realtest-20260911-184347 (n=17); 3x ~= 8.271s"},
	// PhaseFocusEdge is spawn to the harness bringing Emacs forward for the
	// key self-test — a real observed edge, but ALSO where the harness's own
	// deliberate hidden-window wait ends. A budget on this row bounds how
	// long the show-phase wait itself took, which is worth watching for
	// harness health, but it is never a proxy for product latency: nothing
	// downstream (PhaseTotal, PhasePanelPainted) is computed by adding to it.
	//
	// focus-edge: healthy max 6.168s, realtest-20260912-113650 (n=10 — every
	// run that recorded this phase; it is only emitted on runs whose acts
	// exercise the key self-test's hidden-window wait). The spread across
	// those 10 (4.119s-6.168s) is wide because it is a deliberate harness
	// wait, not a product edge, so 3x of the observed max is sized as a
	// hang-detector for the harness's own wait, not as a latency target.
	{Phase: PhaseFocusEdge, Limit: 18504 * time.Millisecond,
		Basis: "harness-wait healthy max 6.168s, realtest-20260912-113650 (n=10); 3x ~= 18.504s (bounds harness health, not product latency)"},
	// PhasePanelPainted is now measured from PhaseFocusEdge, not spawn (owner
	// ruling 2026-09-11): it is the panel's INTRINSIC paint cost, so a future
	// healthy-maximum measurement for this row is sized from "focus edge to
	// load" (observed around 0.4s), never from "spawn to load" — the latter
	// bakes in however long the harness felt like waiting before it showed
	// Emacs, which is harness overhead the owner never experiences and must
	// never be reported as latency.
	//
	// panel-painted: healthy max 0.409s, realtest-20260911-191045, workspace
	// "explanation-engine" (n=20 — every "panel paint cost (focus edge to
	// load)" observation across every run; the two older "panel-painted (on
	// first show)" observations from realtest-20260911-184347, 5.883s and
	// 6.051s, are the OLD spawn-based semantics this row explicitly must not
	// be sized from, so they are excluded).
	{Phase: PhasePanelPainted, Limit: 1227 * time.Millisecond,
		Basis: "healthy max 409ms (focus edge to load), realtest-20260911-191045 (n=20); 3x ~= 1.227s"},
	// PhaseTotal is startup-usable: spawn to the latest of tab-drawn,
	// link-up, roster-subscribed, first-roster and webview-armed (phases.go's
	// usableEdges). It EXCLUDES PhasePanelPainted on purpose, so a future
	// healthy-maximum measurement for this row is sized from "spawn to
	// usable" (observed around 2.8s) — the number the owner actually waits
	// on — never from "spawn to shown-and-painted", which would fold the
	// harness's own arbitrary wait before it reveals Emacs into a bound that
	// is supposed to be about the product.
	//
	// total: healthy max 3.479s, realtest-20260911-154945 (n=23 — every
	// "total" observation from before the 2026-09-11 rename plus every
	// "startup-usable (spawn to usable)" observation after it; both label
	// the identical computation, spawn to the latest usable edge, so they
	// are one set). The single "total (spawn to shown-and-painted)"
	// observation from realtest-20260911-184347 (6.051s) is the OLD
	// semantics that folded panel-painted in and is excluded for the same
	// reason as above.
	{Phase: PhaseTotal, Limit: 10437 * time.Millisecond,
		Basis: "healthy max 3.479s (spawn to usable), realtest-20260911-154945 (n=23); 3x ~= 10.437s"},
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
