//go:build realtest

package realtest

import (
	"context"
	"encoding/json"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"sort"
	"strconv"
	"strings"
	"testing"
	"time"
)

// REALTEST 1 — START THE EDITOR.
//
// docs/REALTEST-PLAN.md, "Startup and shape", item 1: "Start Emacs cold. Time
// to usable; which workspaces open and when; what the modeline and tab bar show
// while waiting."
//
// What this measures, from process spawn to usable, read off the module log's
// own timestamps rather than timed from here (phases.go says why):
//
//	Doom boot done, the module loaded, the daemon ensure commanded and answered
//	— adopted or spawned, reported as which — the link up, the first roster
//	push, then per workspace its tab drawn and the pre-creation queue armed
//	for it. THAT is hidden startup, and that is all of it — see below.
//
// TWO PHASES, NOT ONE (owner ruling, 2026-09-11). The realtest launches
// Emacs with `open -gj` to preserve the owner's focus, and on this machine
// that leaves the frame visible-but-unfocused rather than truly hidden. The
// settled webview invariant parks a workspace's pre-creation in exactly that
// state rather than steal focus, so no panel paints until the first focus
// edge — no matter how long hidden startup runs. So:
//
//   - HIDDEN: every open workspace's tab drawn, and the pre-creation queue
//     armed for the whole open set (elisp.webview-recovery.precreate-all /
//     precreate-parked queued=N reaching that count). No panel is required to
//     have painted here; requiring one asserted a bug that was never there.
//   - SHOW: the key self-test (below) is what first brings Emacs forward,
//     which is the focus edge the parked queue was waiting on. Once shown,
//     every open workspace's panel is asserted to paint within a generous
//     ceiling — proving panels DO paint, just on show, not on hidden launch.
//
// It asserts that EVERY workspace the state database holds is drawn (hidden
// phase) and painted (show phase), and lists them with times.
//
// It has no acts of its own before the show phase: it only observes a
// startup. The key driver is nonetheless proven at the END of the run, by
// pressing `s-}` and `M-2` once each and reading Emacs's own `(recent-keys)`
// back — because a driver first exercised by the realtest that needs it is a
// driver that fails on the day it matters. Proving it is also what produces
// the show phase's focus edge, so the show-phase assertions run right after.
//
// THE REMEDIATION BAR IS THE LOG HARVEST, not the timings: every WARN and ERROR
// written inside the run window across every log, with no allowlist, into the
// run's MANIFEST.md, and the test fails when the count is non-zero.

// runGateEnv is the environment gate. bin/realtest.sh sets it, and it is the
// only supported entry point: the preflight that script performs — the
// readiness gate, the human-in-Emacs refusal, the state backups — is not
// optional, and a `go test` that reached the owner's editor without it would
// have skipped all three.
const runGateEnv = "AGENT_REPL_REALTEST"

// measureEnv puts the run in measurement mode: phase budgets are reported and
// NOT enforced. budgets.go carries the whole reasoning; the short version is
// that a bound invented before the first observation is a guess, and the first
// three cold starts are what size it.
const measureEnv = "AGENT_REPL_REALTEST_MEASURE"

// socketEnv overrides the Emacs server socket, for a machine whose
// `temporary-file-directory` is not this one's.
const socketEnv = "AGENT_REPL_REALTEST_EMACS_SOCKET"

// outEnv overrides where the run directory (MANIFEST.md, the compiled key
// helper, probe answers) lands.
const outEnv = "AGENT_REPL_REALTEST_OUT"

// OBSERVATION CEILINGS, NOT BUDGETS.
//
// These bound how long the test WAITS before reporting that something did not
// happen. They are not performance assertions — budgets.go holds those, and a
// phase is judged against its own budget — so they are deliberately generous:
// a ceiling that fires turns a measurable slow startup into an unmeasurable
// timeout, which throws away the evidence the run exists to collect.
//
// They have no measured basis yet, and that is stated rather than hidden: the
// first authorized measurement is what sizes them, alongside the budgets, and
// until then a ceiling firing is itself a finding to report.
const (
	// serverCeiling is how long the Emacs server socket may take to answer
	// after the spawn. It covers a whole Doom boot on a machine that may be
	// busy with the owner's own work, since a realtest runs on a working
	// machine by definition.
	serverCeiling = 90 * time.Second
	// usableCeiling is how long the module log may take to show every open
	// workspace's tab drawn and the pre-creation queue armed for it (see
	// PhaseWebviewArmed). open-progress.el records live incidents between
	// 6.8s on a quiet machine and 16.9s on a congested one for ONE
	// workspace's bring-up, and this covers every workspace in the roster
	// plus the daemon bring-up in front of them.
	//
	// IT NO LONGER WAITS FOR A PAINTED PANEL. Panel-painted moved to the
	// show phase (showCeiling, below) on the owner's 2026-09-11 ruling:
	// `open -gj` leaves Emacs visible-but-unfocused, and the settled webview
	// invariant parks pre-creation in that state rather than steal focus, so
	// no panel paints until the first focus edge regardless of how long this
	// window runs.
	usableCeiling = 240 * time.Second
	// showCeiling is how long, AFTER Emacs is brought forward for the key
	// self-test, the module log may take to report every open workspace's
	// panel painted. This is the first focus edge the parked pre-creation
	// queue was waiting on, so it is only measured from here, never from
	// spawn — a hidden window of any length was never going to drain it.
	// It has no measured basis yet, same as usableCeiling: generous on
	// purpose, per the OBSERVATION CEILINGS note above.
	showCeiling = 120 * time.Second
	// exitCeiling bounds the key self-test's wait for Emacs's own
	// `(recent-keys)` to report a pressed chord; a stall there is a finding,
	// not something to wait out.
	exitCeiling = 60 * time.Second
	// pollInterval is how often a ceiling's predicate is re-read. Emacs is
	// answering an emacsclient round trip each time, so this is not free, and
	// it is set where a 90s ceiling costs well under a hundred probes.
	pollInterval = 1 * time.Second
)

func TestRealtestStartTheEditor(t *testing.T) {
	if os.Getenv(runGateEnv) != "1" {
		t.Skipf("realtest 1 drives the owner's real editor and runs only through bin/realtest.sh, which sets %s=1", runGateEnv)
	}
	ctx := context.Background()
	measureOnly := os.Getenv(measureEnv) == "1"

	if !measureOnly {
		if unmeasured := UnmeasuredBudgets(); len(unmeasured) > 0 {
			names := make([]string, 0, len(unmeasured))
			for _, phase := range unmeasured {
				names = append(names, string(phase))
			}
			t.Fatalf("these phases have no measured budget yet: %s.\n"+
				"Set them in e2e/realtest/budgets.go from an observed three-run measurement, "+
				"or run with %s=1 to take that measurement. A run must not report green on a gate with no number in it.",
				strings.Join(names, ", "), measureEnv)
		}
	}

	home, err := os.UserHomeDir()
	if err != nil {
		t.Fatalf("resolve the owner's home directory: %v", err)
	}
	runDir := os.Getenv(outEnv)
	if runDir == "" {
		runDir = filepath.Join(home, ".claude-emacs", "realtest",
			fmt.Sprintf("realtest-1-%s", time.Now().Format("20060102-150405")))
	}
	if err := os.MkdirAll(runDir, 0o755); err != nil {
		t.Fatalf("create the run directory %s: %v", runDir, err)
	}
	t.Logf("realtest 1 run directory: %s", runDir)

	socket := os.Getenv(socketEnv)
	if socket == "" {
		socket = filepath.Join(os.TempDir(), fmt.Sprintf("emacs%d", os.Getuid()), "server")
	}
	client := &Client{Socket: socket, Scratch: runDir}
	t.Logf("emacs server socket: %s", socket)
	t.Logf("emacsclient: %s", EmacsClientPath)

	stateDir := filepath.Join(home, ".claude-emacs")
	openWorkspaces, closedWorkspaces, err := ReadWorkspaces(ctx, StateDBPath(stateDir))
	if err != nil {
		t.Fatalf("read the workspaces the state database holds: %v", err)
	}
	t.Logf("the state database holds %d open workspace(s) and %d closed",
		len(openWorkspaces), len(closedWorkspaces))
	for _, ws := range openWorkspaces {
		t.Logf("  open workspace %s (%s) at %s", ws.ID, ws.Name, ws.Dir)
	}

	env := RealEnv(home, openWorkspaces)
	sources, err := EnumerateSources(env)
	if err != nil {
		t.Fatalf("enumerate the logs to harvest: %v", err)
	}
	t.Logf("harvesting %d log source(s); the module's own global sink is %s", len(sources), env.ModuleLog)

	// THE WINDOW OPENS HERE, before anything is launched, and the snapshot is
	// taken at the same moment: everything after this point is the run's, and
	// everything before it belongs to whoever wrote it.
	started := time.Now()
	snapshot := TakeSnapshot(sources)

	manifest := Manifest{
		Title:      "Realtest 1 — start the editor",
		Started:    started,
		Workspaces: openWorkspaces,
	}
	if measureOnly {
		manifest.Notes = append(manifest.Notes,
			"MEASUREMENT RUN: the phase budgets in e2e/realtest/budgets.go are reported and NOT enforced. "+
				"The log harvest below IS enforced.")
	}

	// ONE COLD START (owner ruling, 2026-09-11). One measurement is an
	// anecdote, but three cold starts existed only to hedge a focus problem
	// and to compare two launch methods; both are gone (see chooseMethod's
	// former home in launch.go and docs/REALTEST-JUDGEMENT-CALLS.md, realtest
	// 1, row 24), so nothing left in this test needs repetition.
	const run = 1
	launch := coldStart(ctx, t, client, run)

	phases := waitForUsable(ctx, t, run, sources, snapshot, launch.SpawnedAt, openWorkspaces)
	measurements := phases.Measure()

	manifest.Runs = append(manifest.Runs, ManifestRun{
		Index:        run,
		Method:       launch.Method,
		SpawnedAt:    launch.SpawnedAt,
		DaemonPath:   phases.DaemonPath,
		FrontBefore:  launch.FrontBefore,
		FrontAfter:   launch.FrontAfter,
		Disturbed:    launch.DisturbedOwner,
		Measurements: measurements,
	})

	// THE MEASUREMENTS ARE STATED AT THE SITE, which is what lets the
	// budgets be sized from this run's own output rather than from a file
	// somebody has to go find. This table is the HIDDEN phase: panel-painted
	// is not in it yet, and that is expected (see waitForUsable).
	t.Logf("cold start via %s: daemon %s", launch.Method, orUnknown(phases.DaemonPath))
	for _, m := range measurements {
		if m.Note != "" {
			t.Logf("  phase %-14s %-14s NOT OBSERVED: %s", m.Phase, m.Workspace, m.Note)
			continue
		}
		t.Logf("  phase %-14s %-14s %s from spawn", m.Phase, m.Workspace, m.Elapsed.Round(time.Millisecond))
	}

	assertEveryWorkspaceDrawn(t, run, openWorkspaces, phases)
	verifyVendorGuard(ctx, t, client, run)

	// THE KEY DRIVER, PROVEN. It also delivers the first focus edge: Emacs is
	// visible-but-unfocused up to here, and proveKeyDriver is what activates
	// it for the key self-test.
	driver := proveKeyDriver(ctx, t, client, runDir, &manifest)

	// THE SHOW PHASE. The parked pre-creation queue drains on a focus edge and
	// on nothing else, so a painted panel is assertable only from here.
	// Re-reading folds in everything waitForUsable already saw (nothing there
	// un-happens) plus whatever the show made happen, so the measurements and
	// budget check below replace the hidden-phase ones rather than duplicate
	// them. It is the one shared show helper, the same one realtests 2 through
	// 8 call.
	shown := showEmacsAndWaitForPaint(ctx, t, run, client, driver, sources, snapshot,
		launch.SpawnedAt, openWorkspaces, &manifest)
	shownMeasurements := shown.Measure()
	manifest.Runs[len(manifest.Runs)-1].Measurements = shownMeasurements

	t.Logf("cold start %d: after Emacs was shown for the key self-test:", run)
	for _, m := range shownMeasurements {
		if m.Note != "" {
			t.Logf("  phase %-14s %-14s NOT OBSERVED: %s", m.Phase, m.Workspace, m.Note)
			continue
		}
		t.Logf("  phase %-14s %-14s %s from spawn", m.Phase, m.Workspace, m.Elapsed.Round(time.Millisecond))
	}

	assertEveryWorkspacePainted(t, run, openWorkspaces, shown)
	manifest.BudgetBreaches = append(manifest.BudgetBreaches,
		prefixEach("cold start: ", CheckBudgets(shownMeasurements))...)

	verdict := "left focus alone"
	if launch.DisturbedOwner {
		verdict = fmt.Sprintf("MOVED FOCUS from %q to %q", launch.FrontBefore, launch.FrontAfter)
	}
	note := fmt.Sprintf("launch method `%s`: %s", launch.Method, verdict)
	manifest.Notes = append(manifest.Notes, note)
	t.Logf("%s", note)

	// THE HARVEST. The window closes here; the sources are re-enumerated first
	// so rotation siblings and workspace sinks the run itself created are read.
	manifest.Ended = time.Now()
	sources, err = EnumerateSources(env)
	if err != nil {
		t.Fatalf("re-enumerate the logs after the run: %v", err)
	}
	harvest, err := HarvestSources(sources, snapshot, Window{Start: started, End: manifest.Ended}, openWorkspaces)
	if err != nil {
		t.Fatalf("harvest the logs: %v", err)
	}
	manifest.Findings = harvest.Findings
	manifest.InfoCounts = harvest.InfoCounts

	messages, msgErr := client.Messages(ctx)
	if msgErr != nil {
		// A *Messages* buffer that cannot be read is itself a finding: it is
		// one of the sources the remediation bar covers, and a run that
		// silently skipped it would be claiming a clean harvest it did not
		// perform.
		manifest.Findings = append(manifest.Findings, Finding{
			Kind:      KindMalformed,
			Source:    "*Messages*",
			Path:      "(emacs buffer)",
			Workspace: GlobalWorkspace,
			Note:      fmt.Sprintf("Emacs's *Messages* buffer could not be read, so this source was not harvested: %v", msgErr),
		})
	} else {
		// The whole buffer IS the window: this run started the process.
		manifest.Findings = append(manifest.Findings, HarvestMessages(messages, 0, openWorkspaces)...)
		if err := os.WriteFile(filepath.Join(runDir, "Messages.txt"), []byte(messages), 0o644); err != nil {
			t.Fatalf("preserve the *Messages* buffer: %v", err)
		}
	}
	sortFindings(manifest.Findings)

	path, err := manifest.Write(runDir)
	if err != nil {
		t.Fatalf("write the run manifest: %v", err)
	}
	t.Logf("manifest: %s", path)

	if measureOnly {
		t.Logf("phase budgets: NOT ENFORCED (this is a measurement run; %s=1)", measureEnv)
		for _, breach := range manifest.BudgetBreaches {
			t.Logf("  would have breached: %s", breach)
		}
	} else if len(manifest.BudgetBreaches) > 0 {
		for _, breach := range manifest.BudgetBreaches {
			t.Errorf("%s", breach)
		}
	}

	if len(manifest.Findings) > 0 {
		t.Errorf("the log harvest found %d warning(s), error(s) or non-record(s) inside the run window; "+
			"every one is in %s, verbatim. There is no allowlist: nothing here is fixed by this test, "+
			"and the owner rules on each one (docs/REALTEST-PLAN.md).",
			len(manifest.Findings), path)
		for _, finding := range manifest.Findings {
			t.Logf("  [%s] %s %s (%s) %s | %s", finding.Kind, finding.Workspace, finding.Source,
				finding.Level, finding.Note, finding.Raw)
		}
	}
}

// waitForUsable polls the Emacs log sinks until the HIDDEN startup has
// produced everything realtest 1 measures while Emacs is not yet shown, or
// the observation ceiling expires.
//
// STARTUP-USABLE, HIDDEN, MEANS: every open workspace's tab drawn, and the
// pre-creation queue armed for the whole open set — NOT a painted panel.
// `open -gj` leaves Emacs visible-but-unfocused on this machine, and the
// settled webview invariant parks pre-creation there rather than steal
// focus, so no panel paints until the first focus edge (owner ruling
// 2026-09-11). Waiting on one here would wait the whole ceiling out on a
// startup that had already done everything hidden startup can do; the
// painted assertion moved to waitForShown, after the key self-test brings
// Emacs forward.
//
// It polls the LOG, not Emacs. Two reasons, and the second is the important
// one: asking Emacs whether it has drawn a tab costs an emacsclient round trip
// through the very command loop that is busy drawing it, and — worse — the
// answer would be Emacs's opinion of its own state at the moment it was asked,
// which carries no timestamp. The record does. So the poll is only ever used to
// decide WHEN TO STOP WAITING; every number reported comes from the log.
//
// It reads across the enumerated sources rather than one path because the
// per-workspace tab-open marker lives in each workspace's own `emacs.log`
// sink, not in the global module log (ReadPhases says why).
//
// It returns whatever it has when the ceiling expires rather than failing:
// a startup that did not finish is exactly the run whose partial phase table
// the owner needs to see, and the missing workspaces are asserted separately
// by assertEveryWorkspaceDrawn.
func waitForUsable(ctx context.Context, t *testing.T, run int, sources []Source, snap Snapshot, spawnedAt time.Time, expected []Workspace) Phases {
	t.Helper()
	var phases Phases
	waitUntil(ctx, t,
		fmt.Sprintf("cold start %d: every workspace's tab drawn and the pre-creation queue armed for it", run),
		usableCeiling,
		func() bool {
			read, err := ReadPhases(sources, snap, spawnedAt)
			if err != nil {
				// An Emacs sink that cannot be read at all is fatal here: the
				// phases are measured from these sinks, so a run that continued
				// past this would report an empty timeline as though it were a
				// fast one.
				t.Fatalf("cold start %d: read the startup phases: %v", run, err)
			}
			phases = read
			drawn := setOf(read.DrawnWorkspaces())
			if read.MaxArmed() < len(expected) {
				return false
			}
			for _, ws := range expected {
				if !matchedIn(drawn, ws.ID) {
					return false
				}
			}
			return true
		})
	return phases
}

// showEmacsAndWaitForPaint is THE show phase, shared by every realtest that
// asserts a painted panel.
//
// It brings Emacs forward, waits for every open workspace's panel to paint,
// re-issues the focus edge while it waits, restores the focus it found, and
// reports how many edges the paint needed. showphase.go carries the reasoning,
// including the 2026-09-12 finding that produced it: realtest 8 waited two
// minutes on panels that realtest 1 paints in tens of milliseconds, and the
// difference was never the wait — both call this same marker — but how long
// Emacs held focus, against a pre-creation drain that re-parks the rest of its
// queue the instant Emacs is visible-but-unfocused again.
//
// EVERY REALTEST THAT ASSERTS A PAINT CALLS THIS, realtest 1 included. The show
// phase used to be spelled twice — realtest 1, 2 and 3 produced their edge
// inside proveKeyDriver and realtests 5 through 8 through a separate
// wsActShowEmacs — and two spellings of one phase is how two runs start
// disagreeing about what a shown editor is.
//
// THE FOCUS CHECK IS A NET CHECK. It fails only when focus was left somewhere
// other than where it started, which is a failure to restore, not the momentary
// activation the driver deliberately performs.
func showEmacsAndWaitForPaint(ctx context.Context, t *testing.T, run int, client *Client, driver *KeyDriver,
	sources []Source, snap Snapshot, spawnedAt time.Time, expected []Workspace, manifest *Manifest) Phases {
	t.Helper()

	before, err := FrontmostApp(ctx)
	if err != nil {
		t.Fatalf("cold start %d: read which application is frontmost before Emacs is shown: %v", run, err)
	}

	started := time.Now()
	var phases Phases
	edges := 0
	realEdges := 0
	var lastFocus FocusReading
	painted := false
	for edge := 1; edge <= showMaxFocusEdges; edge++ {
		ceiling := showPhaseEdgeCeiling(edge, time.Since(started))
		if driver != nil {
			receipt, pressErr := driver.PressWithReceipt(ctx, wsActEscape)
			if pressErr != nil {
				note := fmt.Sprintf("SHOWING EMACS FAILED on focus edge %d: pressing %s (%s) answered %v. "+
					"The parked pre-creation queue drains on a focus edge and on nothing else, so no panel can "+
					"paint until one is produced", edge, wsActEscape.Emacs, wsActEscape.Why, pressErr)
				manifest.Notes = append(manifest.Notes, note)
				t.Errorf("%s", note)
			} else {
				edges++
				lastFocus = receipt.Focus
				if receipt.Focus.State == FocusFocused {
					realEdges++
				} else {
					// THE EDITOR SAID IT NEVER TOOK FOCUS, so the drain is
					// still held and this ceiling would be spent waiting for a
					// paint that cannot happen. The state is still READ once —
					// a zero ceiling is one poll, not none, so the phases this
					// helper returns are never left empty — and then the next
					// activation is requested immediately: a declined request
					// can be granted on the next one. If none of them is, the
					// note after the loop says why, rather than the run
					// reporting an unpainted panel two minutes later.
					ceiling = 0
				}
			}
		}
		phases = waitForPainted(ctx, t, run, sources, snap, spawnedAt, expected, ceiling)
		if everyWorkspacePainted(phases, expected) {
			painted = true
			break
		}
	}
	if driver != nil && edges > 0 && realEdges == 0 {
		lock, lockErr := driver.ScreenLock(ctx)
		note := noFocusEdgeNote(edges, lock, lockErr, lastFocus)
		manifest.Notes = append(manifest.Notes, note)
		t.Errorf("%s", note)
	}

	after, err := FrontmostApp(ctx)
	if err != nil {
		t.Fatalf("cold start %d: read which application is frontmost after Emacs was shown: %v", run, err)
	}
	focusNote, focusFinding := focusAfterPressesNote("the show phase", sweepHoldsFocus(), before, after)
	manifest.Notes = append(manifest.Notes, focusNote)
	if focusFinding {
		t.Errorf("%s", focusNote)
	}

	// The verdict on the paint itself belongs to assertEveryWorkspacePainted,
	// which every caller runs on the phases returned here; this note says what
	// the show phase had to DO to get them, which no assertion on the phases
	// can see.
	note := showPhaseNote(edges, painted, time.Since(started), len(expected))
	manifest.Notes = append(manifest.Notes, note)
	t.Logf("%s", note)

	if driver == nil {
		note := "NO FOCUS EDGE WAS PRODUCED: the key driver is unavailable, so Emacs was never brought " +
			"forward and the parked pre-creation queue could not drain. Any unpainted panel below is that, " +
			"not the product"
		manifest.Notes = append(manifest.Notes, note)
		t.Errorf("%s", note)
	}
	return phases
}

// waitForPainted polls the Emacs log sinks until every expected workspace's
// panel has painted, or `ceiling` expires.
//
// It reads the SAME sources and the SAME spawnedAt as waitForUsable — the
// elapsed times it reports are still measured from real log edges, which is
// what latency must always come from (docs/REALTEST-JUDGEMENT-CALLS.md, the
// 2026-09-11 ruling on harness overhead).
func waitForPainted(ctx context.Context, t *testing.T, run int, sources []Source, snap Snapshot,
	spawnedAt time.Time, expected []Workspace, ceiling time.Duration) Phases {
	t.Helper()
	var phases Phases
	waitUntil(ctx, t,
		fmt.Sprintf("cold start %d: every workspace's panel painted, now that Emacs is shown", run),
		ceiling,
		func() bool {
			read, err := ReadPhases(sources, snap, spawnedAt)
			if err != nil {
				t.Fatalf("cold start %d: read the show-phase startup phases: %v", run, err)
			}
			phases = read
			return everyWorkspacePainted(read, expected)
		})
	return phases
}

// everyWorkspacePainted reports whether the phases hold a painted panel for
// every expected workspace.
func everyWorkspacePainted(phases Phases, expected []Workspace) bool {
	painted := setOf(phases.PaintedWorkspaces())
	for _, ws := range expected {
		if !matchedInPainted(painted, ws) {
			return false
		}
	}
	return true
}

// coldStart launches the editor once and waits for it to become usable.
//
// There is exactly one launch method, `open -g -a Emacs` (owner ruling,
// 2026-09-11; see launch.go for why the second method and its rotation across
// runs were removed).
func coldStart(ctx context.Context, t *testing.T, client *Client, run int) Launch {
	t.Helper()

	if client.Alive(ctx) {
		t.Fatalf("cold start %d: an Emacs is already answering %s. A cold start must start the process itself, "+
			"and bin/realtest.sh is what quits a standing editor — after the backups, and only with "+
			"AGENT_REPL_REALTEST_TAKEOVER=1", run, client.Socket)
	}

	launch, err := LaunchOpenBackground(ctx)
	if err != nil {
		t.Fatalf("cold start %d: %v", run, err)
	}

	waitUntil(ctx, t, fmt.Sprintf("cold start %d: the Emacs server to answer", run), serverCeiling,
		func() bool { return client.Alive(ctx) })

	if err := launch.ObserveFocus(ctx); err != nil {
		t.Fatalf("cold start %d: read which application is frontmost after the launch: %v", run, err)
	}
	return launch
}

// assertEveryWorkspaceDrawn is the plan's HIDDEN-startup assertion: every
// workspace the state database holds got a drawn tab and was armed for
// pre-creation, and they are listed with times. Painted panels are NOT
// asserted here — see assertEveryWorkspacePainted — because the parked
// pre-creation queue does not drain until Emacs is shown (owner ruling
// 2026-09-11): a hidden-phase assertion that still required a painted panel
// would be asserting a bug that isn't one.
//
// Tab-drawn and armed are still logged as separate conditions where they
// fail, because they fail for different reasons — a tab is Emacs's own
// roster reconcile, "armed" is the pre-creation queue's own count — and one
// message naming both would name neither.
func assertEveryWorkspaceDrawn(t *testing.T, run int, expected []Workspace, phases Phases) {
	t.Helper()
	drawn := setOf(phases.DrawnWorkspaces())

	for _, ws := range expected {
		if !matchedIn(drawn, ws.ID) {
			t.Errorf("cold start %d: workspace %s (%s) is in the state database but no tab was drawn for it "+
				"(no `elisp.roster.tab-open` record naming it inside the run)", run, ws.ID, ws.Name)
		}
	}

	for id := range drawn {
		if !matchedInWorkspaces(expected, id) {
			t.Errorf("cold start %d: a tab was drawn for workspace %s, which the state database does not hold",
				run, id)
		}
	}

	if armed := phases.MaxArmed(); armed < len(expected) {
		t.Errorf("cold start %d: the pre-creation queue armed at most %d workspace(s) for %d open "+
			"workspace(s) (no `elisp.webview-recovery.precreate-all: queued=N` or `precreate-parked "+
			"queued=N` reached that count inside the run) — the panel that never paints on first show "+
			"is downstream of this, not of focus", run, armed, len(expected))
	}
}

// assertEveryWorkspacePainted is the plan's SHOW-phase assertion: once Emacs
// is focused, every open workspace's panel paints. Called after proveKeyDriver
// has brought Emacs forward, against phases read by waitForShown.
func assertEveryWorkspacePainted(t *testing.T, run int, expected []Workspace, phases Phases) {
	t.Helper()
	painted := setOf(phases.PaintedWorkspaces())
	for _, ws := range expected {
		if !matchedInPainted(painted, ws) {
			t.Errorf("cold start %d: workspace %s (%s) had no panel painted after Emacs was shown "+
				"(no `elisp.frontend.watch-load: load-changed` or `elisp.webview-recovery.precreate-created "+
				"ws=%s reason=focused` record naming it inside the run)", run, ws.ID, ws.Name, ws.Name)
		}
	}
}

// matchedIn tolerates the two spellings of a workspace id that the logs carry —
// a record's full id and a shorter prefix — by matching on either being a
// prefix of the other. An exact-match-only comparison read as a missing tab for
// every workspace on the owner's machine, which is a wrong answer about the
// product produced entirely by a wrong answer about the id.
func matchedIn(set map[string]bool, id string) bool {
	for candidate := range set {
		if candidate == id || strings.HasPrefix(candidate, id) || strings.HasPrefix(id, candidate) {
			return true
		}
	}
	return false
}

// matchedInPainted is matchedIn, but tried against BOTH a workspace's id and
// its registered name. Every other per-workspace marker this file reads is
// attributed by id (rec.WorkspaceID, resolved from the workspace regardless
// of which string a log call's subject happened to be), but
// `precreate-created` is written on the central sink with no `workspace_id`
// at all — its own `ws=` names the workspace by the NAME its hash is keyed
// by (phases.go's wsEqualsRe says why), not by the daemon id the state
// database's `ws.ID` holds. Trying the name as well as the id is what lets
// that one marker still count as a match.
func matchedInPainted(painted map[string]bool, ws Workspace) bool {
	return matchedIn(painted, ws.ID) || matchedIn(painted, ws.Name)
}

func matchedInWorkspaces(workspaces []Workspace, id string) bool {
	for _, ws := range workspaces {
		if ws.ID == id || strings.HasPrefix(ws.ID, id) || strings.HasPrefix(id, ws.ID) {
			return true
		}
	}
	return false
}

func setOf(values []string) map[string]bool {
	out := make(map[string]bool, len(values))
	for _, value := range values {
		out[value] = true
	}
	return out
}

// verifyVendorGuard proves the ONE substitution actually reached the processes.
//
// The source says it should: `open --env` states it on the Emacs process,
// lisp/daemon.el's `agent-repl-daemon--environment` passes
// `process-environment` through to the daemon, and the daemon's `spawnEnv`
// passes `os.Environ()` through to each shim. None of that proves the process
// standing here has it, and a realtest that believed the vendor was forbidden
// while the real SDK was one prompt away would be spending the owner's tokens
// to find out. The kernel's copy of the environment is the proof.
func verifyVendorGuard(ctx context.Context, t *testing.T, client *Client, run int) {
	t.Helper()

	emacsPID, err := client.ReadInt(ctx, `(emacs-pid)`)
	if err != nil {
		t.Fatalf("cold start %d: read the Emacs pid: %v", run, err)
	}
	assertGuarded(ctx, t, run, "emacs", emacsPID)

	// The daemon: Emacs's own process object when this launch spawned it, and
	// otherwise the answering daemon it adopted, found by its binary.
	raw, err := client.Read(ctx, `(and agent-repl--frontend-daemon-process
                                       (process-live-p agent-repl--frontend-daemon-process)
                                       (process-id agent-repl--frontend-daemon-process))`)
	if err != nil {
		t.Fatalf("cold start %d: read the daemon process Emacs holds: %v", run, err)
	}
	var daemonPID int
	if err := json.Unmarshal(raw, &daemonPID); err != nil || daemonPID == 0 {
		found, findErr := pgrepOne(ctx, "claude-repld")
		if findErr != nil {
			t.Errorf("cold start %d: Emacs holds no daemon process and no `claude-repld` could be found, "+
				"so the vendor guard could not be verified on the daemon: %v", run, findErr)
			return
		}
		t.Logf("cold start %d: the daemon was adopted, not spawned by this Emacs; verifying pid %d", run, found)
		daemonPID = found
	}
	assertGuarded(ctx, t, run, "daemon", daemonPID)
}

func assertGuarded(ctx context.Context, t *testing.T, run int, what string, pid int) {
	t.Helper()
	env, err := ProcessEnvironment(ctx, pid)
	if err != nil {
		t.Errorf("cold start %d: read the %s's (pid %d) environment to verify the vendor guard: %v",
			run, what, pid, err)
		return
	}
	if env[vendorGuardEnv] == "" {
		t.Errorf("cold start %d: the %s (pid %d) does NOT carry %s, so a real Claude call is possible "+
			"in this run. Every realtest forbids the vendor; see docs/REALTEST-PLAN.md",
			run, what, pid, vendorGuardEnv)
		return
	}
	t.Logf("cold start %d: the %s (pid %d) carries %s=%s", run, what, pid, vendorGuardEnv, env[vendorGuardEnv])
}

// pgrepOne finds one process by a pattern in its command line.
func pgrepOne(ctx context.Context, pattern string) (int, error) {
	callCtx, cancel := context.WithTimeout(ctx, 10*time.Second)
	defer cancel()
	out, err := exec.CommandContext(callCtx, "pgrep", "-f", pattern).Output()
	if err != nil {
		return 0, fmt.Errorf("pgrep -f %q: %w", pattern, err)
	}
	fields := strings.Fields(string(out))
	if len(fields) == 0 {
		return 0, fmt.Errorf("no process matches %q", pattern)
	}
	pid, err := strconv.Atoi(fields[0])
	if err != nil {
		return 0, fmt.Errorf("pgrep -f %q answered %q: %w", pattern, fields[0], err)
	}
	return pid, nil
}

// proveKeyDriver presses the two chords and reads Emacs's own account of them.
//
// It reads `(recent-keys)` rather than observing a workspace switch: the switch
// would also have happened if something had CALLED the command, and the whole
// point of a real key event is that Emacs's keymap is what resolved it.
//
// The key driver deliberately brings Emacs frontmost for the instant of each
// keypress and restores the prior frontmost app afterwards (keydriver.swift
// says why a no-activation post reaches no key window). So the focus check
// below is a NET check: it fails only if focus was left on something other than
// where it started, which is a failure to restore, not the momentary activation
// itself. This is the one phase that touches focus at all; startup never does.
func proveKeyDriver(ctx context.Context, t *testing.T, client *Client, runDir string, manifest *Manifest) *KeyDriver {
	t.Helper()

	pid, err := client.ReadInt(ctx, `(emacs-pid)`)
	if err != nil {
		t.Fatalf("read the Emacs pid for the key driver: %v", err)
	}
	driver := &KeyDriver{Pid: pid, Scratch: runDir, Client: client, KeepFocus: sweepHoldsFocus()}
	if err := driver.Build(ctx); err != nil {
		// SURFACED, NOT WORKED AROUND. The plan rules that if key delivery to
		// Emacs is impossible, the owner decides the alternative; there is no
		// elisp fallback here on purpose.
		note := fmt.Sprintf("KEY DRIVER UNAVAILABLE: %v", err)
		manifest.Notes = append(manifest.Notes, note)
		t.Errorf("%s", note)
		t.Logf("the second mechanism, System Events `key code`, is implemented (keys.go) but delivers to the " +
			"FRONTMOST application, so using it would require bringing Emacs forward and disturbing the owner. " +
			"It was NOT attempted. No elisp fallback was taken.")
		return nil
	}
	manifest.Notes = append(manifest.Notes,
		fmt.Sprintf("key driver: %s; accessibility trust held", driver.Method))

	before, err := FrontmostApp(ctx)
	if err != nil {
		t.Fatalf("read which application is frontmost before the key self-test: %v", err)
	}

	for _, chord := range []Chord{SwitchRight, SwitchToSecond} {
		if err := driver.Press(ctx, chord); err != nil {
			t.Errorf("press %s (%s): %v", chord.Emacs, chord.Why, err)
			continue
		}
		// The keymap resolves the chord in Emacs's command loop, which is a
		// different process; the read below is what waits for it, and it
		// waits on Emacs's own account of its input rather than on a clock.
		var keys, command string
		waitUntil(ctx, t, fmt.Sprintf("emacs to report %s in its own recent keys", chord.Emacs), exitCeiling,
			func() bool {
				keys, err = RecentKeys(ctx, client)
				if err != nil {
					return false
				}
				return strings.Contains(keys, chord.Emacs)
			})
		if !strings.Contains(keys, chord.Emacs) {
			t.Errorf("pressed %s but Emacs's own (recent-keys) does not contain it, so the event did not reach "+
				"its keymap. recent-keys ended with: %s", chord.Emacs, tail(keys, 120))
			continue
		}
		if command, err = LastCommand(ctx, client); err != nil {
			t.Errorf("read last-command after %s: %v", chord.Emacs, err)
			continue
		}
		note := fmt.Sprintf("real key event %s reached Emacs's keymap; last-command became `%s`", chord.Emacs, command)
		manifest.Notes = append(manifest.Notes, note)
		t.Logf("%s", note)
	}

	after, err := FrontmostApp(ctx)
	if err != nil {
		t.Fatalf("read which application is frontmost after the key self-test: %v", err)
	}
	note, finding := focusAfterPressesNote("the key self-test", driver.KeepFocus, before, after)
	manifest.Notes = append(manifest.Notes, note)
	if finding {
		t.Errorf("%s", note)
	}
	return driver
}

// waitUntil polls a predicate to a ceiling.
//
// It does NOT fail on its own: it returns when the ceiling expires and lets the
// caller say what the unmet condition means, because a ceiling firing is
// sometimes the finding and sometimes fatal, and only the caller knows which.
func waitUntil(ctx context.Context, t *testing.T, what string, ceiling time.Duration, predicate func() bool) {
	t.Helper()
	deadline := time.Now().Add(ceiling)
	for {
		if predicate() {
			return
		}
		if time.Now().After(deadline) {
			t.Logf("waited %s for %s and it did not happen", ceiling, what)
			return
		}
		select {
		case <-ctx.Done():
			return
		case <-time.After(pollInterval):
		}
	}
}

func prefixEach(prefix string, values []string) []string {
	out := make([]string, 0, len(values))
	for _, value := range values {
		out = append(out, prefix+value)
	}
	sort.Strings(out)
	return out
}
