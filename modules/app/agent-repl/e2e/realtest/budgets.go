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
	// doom-boot: healthy max 2.627s, realtest-20260911-154945 (n=28 —
	// every doom-boot observation across every MANIFEST.md under
	// ~/.claude-emacs/realtest/ as of 2026-09-12, including the two
	// locked-screen runs realtest-20260912-161209/164343: this is a
	// pre-focus, spawn-to-boot phase, so it is real on those runs even
	// though their focus-dependent rows are not). At n=28 the pack sits
	// 1.464s-2.627s (mean 1.784s) with a single outlier at the max, so a
	// 2x multiple — not the previous 3x — already sits comfortably above
	// every observation while more than halving the old margin. 2x ~=
	// 5.254s. This would now catch a doom-boot regression anywhere past
	// ~2.6x today's typical (~2.0s), where the old 7.881s bound tolerated
	// nearly 4x before firing.
	{Phase: PhaseDoomBoot, Limit: 5254 * time.Millisecond,
		Basis: "healthy max 2.627s, realtest-20260911-154945 (n=28); 2x ~= 5.254s"},
	// module-loaded: healthy max 1.814s, realtest-20260912-143213 (n=5).
	//
	// THIS ROW WAS `unmeasured` UNTIL THE MARKER WAS CORRECTED (the reader
	// used to match `elisp.daemon.ensure-command`, a record only the
	// interactive retry writes, while the startup path writes
	// `elisp.daemon.ensure-scheduled`; phases.go says why). The fix landed
	// the phase into the standard per-run phase table itself, so unlike the
	// previous pass this row is now sized straight from MANIFEST.md's own
	// "module-loaded" rows rather than from a separate Messages.txt grep:
	// five runs carry it (realtest-20260912-133741 onward), ranging
	// 1.677s-1.814s, mean 1.721s — a genuinely tight cluster. n=5 is still a
	// small sample for a phase this new, so the multiple keeps a little more
	// headroom than the well-sampled boot-chain rows below: 2.5x ~= 4.535s.
	// That is still more than 2x tighter than the previous 8.901s and would
	// catch a regression past ~2.5x today's ~1.7s typical.
	{Phase: PhaseModuleLoaded, Limit: 4535 * time.Millisecond,
		Basis: "healthy max 1.814s, realtest-20260912-143213 (n=5); 2.5x ~= 4.535s"},
	// daemon-spawned: healthy max 3.201s, realtest-20260911-154945 (n=26).
	// Same well-sampled boot-chain shape as doom-boot (mean 2.172s, max/mean
	// 1.47), so the same 2x applies. 2x ~= 6.402s, versus the old 9.603s.
	{Phase: PhaseDaemonSpawned, Limit: 6402 * time.Millisecond,
		Basis: "healthy max 3.201s, realtest-20260911-154945 (n=26); 2x ~= 6.402s"},
	// daemon-answered: healthy max 3.474s, realtest-20260911-154945 (n=27 —
	// includes the one run, realtest-20260911-200208, where this same cold
	// start's daemon never answered at all; that failure produced no numeric
	// daemon-answered observation to fold in, so it does not inflate this
	// max, and its own doom-boot reading is unaffected and counted above).
	// mean 2.426s, max/mean 1.43 — same shape as the rest of the chain. 2x
	// ~= 6.948s, versus the old 10.422s.
	{Phase: PhaseDaemonAnswered, Limit: 6948 * time.Millisecond,
		Basis: "healthy max 3.474s, realtest-20260911-154945 (n=27); 2x ~= 6.948s"},
	// link-up: healthy max 3.465s, realtest-20260911-154945 (n=26). 2x ~=
	// 6.93s, versus the old 10.395s.
	{Phase: PhaseLinkUp, Limit: 6930 * time.Millisecond,
		Basis: "healthy max 3.465s, realtest-20260911-154945 (n=26); 2x ~= 6.93s"},
	// roster-subscribed: healthy max 3.474s, realtest-20260911-154945
	// (n=26). 2x ~= 6.948s, versus the old 10.422s.
	{Phase: PhaseRosterSubscribed, Limit: 6948 * time.Millisecond,
		Basis: "healthy max 3.474s, realtest-20260911-154945 (n=26); 2x ~= 6.948s"},
	// first-roster: healthy max 2.105s, realtest-20260912-133741 (n=5).
	//
	// THIS ROW WAS ALSO `unmeasured`-in-spirit until now: it used to be
	// DERIVED from tab-drawn because roster.el emitted
	// `elisp.roster.reconcile:` at DEBUG, below the durable sink's default
	// level, so no run had ever recorded it directly (phases.go says why,
	// and roster.el now emits it at INFO). That fix is the "first authorized
	// run after this change observes the phase directly" the old comment
	// asked for: five runs now carry a genuine first-roster row
	// (realtest-20260912-133741 onward), 1.826s-2.105s, mean 1.941s — every
	// one of them at or below its run's own tab-drawn reading, exactly as
	// the derivation predicted. This Basis replaces the derivation with the
	// real measurement. Small n like module-loaded's, so the same 2.5x:
	// 2.5x ~= 5.263s (2105ms * 2.5 = 5262.5ms, rounded up to the nearest ms).
	{Phase: PhaseFirstRoster, Limit: 5263 * time.Millisecond,
		Basis: "healthy max 2.105s, realtest-20260912-133741 (n=5); 2.5x ~= 5.263s"},
	// tab-drawn: healthy max 3.479s, realtest-20260911-154945, workspace
	// 2b81f45a724642ef (n=51 — every per-workspace tab-drawn observation
	// across every MANIFEST.md as of 2026-09-12; this is the most-sampled
	// row in the table). Same boot-chain shape as the rows above (mean
	// 2.393s, max/mean 1.45). 2x ~= 6.958s, versus the old 10.437s.
	{Phase: PhaseTabDrawn, Limit: 6958 * time.Millisecond,
		Basis: "healthy max 3.479s, realtest-20260911-154945 (n=51); 2x ~= 6.958s"},
	// PhaseWebviewArmed replaces PhasePanelPainted as the hidden-startup
	// gate (owner ruling 2026-09-11; phases.go says why). It is still
	// measured from spawn like every other row here.
	//
	// webview-armed: healthy max 2.757s, realtest-20260911-184347 (n=22).
	// This is the tightest-spread row in the boot chain (mean 2.289s,
	// max/mean 1.20, versus ~1.45 for doom-boot/daemon-spawned/etc.), so it
	// carries a smaller multiple than its siblings: 1.75x ~= 4.825s
	// (2757ms * 1.75 = 4824.75ms, rounded up to the nearest ms), versus the
	// old 8.271s.
	{Phase: PhaseWebviewArmed, Limit: 4825 * time.Millisecond,
		Basis: "healthy max 2.757s, realtest-20260911-184347 (n=22); 1.75x ~= 4.825s"},
	// PhaseFocusEdge is spawn to the harness bringing Emacs forward for the
	// key self-test — a real observed edge, but ALSO where the harness's own
	// deliberate hidden-window wait ends. A budget on this row bounds how
	// long the show-phase wait itself took, which is worth watching for
	// harness health, but it is never a proxy for product latency: nothing
	// downstream (PhaseTotal, PhasePanelPainted) is computed by adding to it.
	//
	// focus-edge: healthy max 6.168s, realtest-20260912-113650 (n=13 — every
	// run that recorded this phase, up from n=10 at the last pass; it is
	// only emitted on runs whose acts exercise the key self-test's
	// hidden-window wait. None of the two locked-screen runs
	// (realtest-20260912-161209/164343) recorded this phase at all, so
	// there is nothing meaningless from them to exclude here). The spread
	// (4.119s-6.168s, mean 4.645s) is still wide, and unlike the product-
	// latency rows above it did NOT tighten with more observations: the
	// three new runs (4.146s, 4.16s, 4.424s) all landed inside the old
	// range. Because it is a deliberate harness wait, not a product edge,
	// it keeps more headroom than the well-sampled boot-chain rows —
	// 2.5x, not 2x — as a hang-detector for the harness's own wait, not a
	// latency target: 2.5x ~= 15.42s, versus the old 18.504s.
	{Phase: PhaseFocusEdge, Limit: 15420 * time.Millisecond,
		Basis: "harness-wait healthy max 6.168s, realtest-20260912-113650 (n=13); 2.5x ~= 15.42s (bounds harness health, not product latency)"},
	// PhasePanelPainted is now measured from PhaseFocusEdge, not spawn (owner
	// ruling 2026-09-11): it is the panel's INTRINSIC paint cost, so a future
	// healthy-maximum measurement for this row is sized from "focus edge to
	// load" (observed around 0.4s), never from "spawn to load" — the latter
	// bakes in however long the harness felt like waiting before it showed
	// Emacs, which is harness overhead the owner never experiences and must
	// never be reported as latency.
	//
	// panel-painted: healthy max 0.409s, realtest-20260911-191045, workspace
	// "explanation-engine" (n=24 — every "panel paint cost (focus edge to
	// load)" observation across every run as of 2026-09-12, up from n=20 at
	// the last pass. The two older "panel-painted (on first show)"
	// observations from realtest-20260911-184347, 5.883s and 6.051s, are the
	// OLD spawn-based semantics this row explicitly must not be sized from,
	// so they are excluded, as is that same run's "total (spawn to
	// shown-and-painted)" reading for the same reason. The two locked-screen
	// runs recorded no panel-paint-cost observation at all, so nothing from
	// them needed excluding either). The observations run 22ms-409ms
	// (mean 263ms) — a wide relative spread for a phase measured in tens of
	// milliseconds, where ordinary scheduling jitter is a bigger fraction of
	// the total than for the second-scale rows above, so it keeps the same
	// 2.5x headroom as the small-n rows rather than the boot chain's 2x:
	// 2.5x ~= 1.023s (409ms * 2.5 = 1022.5ms, rounded up to the nearest ms),
	// versus the old 1.227s.
	{Phase: PhasePanelPainted, Limit: 1023 * time.Millisecond,
		Basis: "healthy max 409ms (focus edge to load), realtest-20260911-191045 (n=24); 2.5x ~= 1.023s"},
	// PhaseTotal is startup-usable: spawn to the latest of tab-drawn,
	// link-up, roster-subscribed, first-roster and webview-armed (phases.go's
	// usableEdges). It EXCLUDES PhasePanelPainted on purpose, so a future
	// healthy-maximum measurement for this row is sized from "spawn to
	// usable" (observed around 2.8s) — the number the owner actually waits
	// on — never from "spawn to shown-and-painted", which would fold the
	// harness's own arbitrary wait before it reveals Emacs into a bound that
	// is supposed to be about the product.
	//
	// total: healthy max 3.479s, realtest-20260911-154945 (n=26 — every
	// "total" observation from before the 2026-09-11 rename plus every
	// "startup-usable (spawn to usable)" observation after it, up from
	// n=23 at the last pass; both label the identical computation, spawn to
	// the latest usable edge, so they are one set. This includes the two
	// locked-screen runs, since spawn-to-usable never depends on focus: both
	// (1.85s, 1.888s) land well inside the range). The single "total (spawn
	// to shown-and-painted)" observation from realtest-20260911-184347
	// (6.051s) is the OLD semantics that folded panel-painted in and is
	// excluded for the same reason as above. mean 2.420s, max/mean 1.44 —
	// same shape as the other boot-chain rows, so the same 2x: 2x ~= 6.958s,
	// versus the old 10.437s. Where the old bound would tolerate a total
	// startup slower than 10.437s (>5x today's typical ~2s) before firing,
	// this catches anything past ~3.5x today's typical.
	{Phase: PhaseTotal, Limit: 6958 * time.Millisecond,
		Basis: "healthy max 3.479s (spawn to usable), realtest-20260911-154945 (n=26); 2x ~= 6.958s"},
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
