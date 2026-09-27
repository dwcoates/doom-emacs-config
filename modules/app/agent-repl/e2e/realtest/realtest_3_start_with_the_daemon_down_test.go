//go:build realtest

package realtest

import (
	"context"
	"fmt"
	"os"
	"path/filepath"
	"regexp"
	"testing"
	"time"
)

// REALTEST 3 — START WITH THE DAEMON DOWN.
//
// docs/REALTEST-PLAN.md, "Startup and shape", item 3: "Start Emacs with the
// daemon down. It is built and spawned; time and feedback for that path."
//
// SAME MEASUREMENTS AS REALTESTS 1 AND 2, taken over the spawn-inclusive
// startup: every phase, tab, panel, key and harvest assertion is realtest 1's,
// called through realtest 1's own helpers. The phase table is therefore
// directly comparable with realtest 2's, and the difference between the two
// tables IS what bringing a daemon up costs the owner — which is the "time"
// half of the plan's item.
//
// The "feedback" half is the other addition, and it is what this test asserts
// that neither of the other two can. A cold bring-up is the longest wait the
// editor ever makes the owner sit through, and realtest 1's first run recorded
// exactly why that matters: "nothing in the mode line said anything while
// bring-up ran, so the 13.5 s to a link reads to the owner as a hang"
// (docs/REALTEST-PLAN.md, realtest 1 run 1). The startup feedback added since
// is asserted here, from the records the module's one log function writes for
// it (startup_shared_test.go documents both channels and how they are matched):
//
//	THE MODE-LINE SEGMENT lifecycle — `elisp.daemon.lifecycle state=starting`,
//	then `state=linking`, then `state=ready`. `ready` rather than `adopted`
//	because this Emacs spawned the daemon itself and its process is still live
//	(lisp/daemon.el, `agent-repl-daemon--spawned-here-p`).
//
//	THE MINIBUFFER ECHOES — "starting the daemon…", "linking to the daemon…",
//	"daemon ready", and "loading workspaces (n/m)…" as each workspace finishes
//	painting.
//
// And the adoption records are asserted ABSENT, for the same reason realtest 2
// asserts the spawn records absent: a test that only checked its own path
// would pass on a run that took both.
//
// THE PRECONDITION IS THE RUNNER'S TO ESTABLISH, AND THIS TEST WILL NOT
// ESTABLISH IT. No daemon may be running when this test starts, and if one is,
// the test refuses and names the pid. It does NOT kill it, in a sweep or
// alone: the process it would be killing is the owner's, and a test does not
// make that decision.
//
// bin/realtest.sh does, and only when it is told to. Stopping the daemon ends
// every live session it holds, which the editor takeover says nothing about,
// so it is a consent of its own: with AGENT_REPL_REALTEST_STOP_DAEMON=1 the
// runner quits the editor, SIGTERMs the daemon and waits for it to go before
// this test starts; without it the runner SKIPS realtest 3 and says why,
// rather than running it into the refusal below. Either way this refusal is
// what decides, and the manual route — stop the daemon deliberately (SPC o C-d
// from the editor, or kill the pid) and run realtest 3 on its own — is
// unchanged.
//
// "IT IS BUILT AND SPAWNED" — the spawn is asserted, the build is only
// REPORTED. bin/realtest.sh's readiness refusal already guarantees every
// deployed system is at this checkout's revision, so the module's staleness
// check finds nothing to do and writes `elisp.daemon.build-skipped-fresh`
// instead of building. Asserting a build would be asserting that the preflight
// failed. Which of the two happened is put in the manifest, because a run that
// DID build measured a different startup and the phase table must not be read
// as though it did not.

// daemonDownCeiling bounds how long the run waits, after quitting Emacs, to
// confirm no daemon came up behind it.
//
// It is a SETTLING bound like realtest 2's restartCeiling, and short for the
// same reason: nothing is being waited FOR here. It exists because the quit
// and a daemon's own exit are concurrent, so the answer to "is anything
// running" is only stable once the quit's dust has cleared.
const daemonDownCeiling = 15 * time.Second

// rt3BuildRan and rt3BuildSkipped are realtest 3's own markers, for the build
// the plan's item mentions. They are declared here rather than in the shared
// marker set because nothing else asserts or reports on them, and they are
// REPORTED rather than asserted: see the header on why a build is not expected
// under the readiness refusal.
var (
	rt3BuildRan = startupFeedbackMarker{
		ID:   "rt3:build ran",
		Re:   regexp.MustCompile(`^elisp\.daemon\.build script=`),
		What: "the daemon build running before the spawn",
	}
	rt3BuildSkipped = startupFeedbackMarker{
		ID:   "rt3:build skipped as fresh",
		Re:   regexp.MustCompile(`^elisp\.daemon\.build-skipped-fresh(\s|$)`),
		What: "the staleness check finding nothing to build",
	}
)

func TestRealtestStartWithTheDaemonDown(t *testing.T) {
	if os.Getenv(runGateEnv) != "1" {
		t.Skipf("realtest 3 drives the owner's real editor and runs only through bin/realtest.sh, which sets %s=1", runGateEnv)
	}
	ctx := context.Background()
	measureOnly := os.Getenv(measureEnv) == "1"
	requireMeasuredBudgets(t, measureOnly)

	home, err := os.UserHomeDir()
	if err != nil {
		t.Fatalf("resolve the owner's home directory: %v", err)
	}
	runDir := startupRunDir(t, home, "realtest-3")
	t.Logf("realtest 3 run directory: %s", runDir)

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

	// THE PRECONDITION, CHECKED BEFORE THE OWNER'S EDITOR IS TOUCHED. A run
	// that discovered a running daemon after quitting Emacs would have closed
	// the editor for a run it then refuses.
	requireNoDaemonRunning(ctx, t, "before the run")

	// THE WINDOW OPENS HERE, before the quit: everything after this point is
	// the run's.
	started := time.Now()
	snapshot := TakeSnapshot(sources)

	manifest := Manifest{
		Title:      "Realtest 3 — start with the daemon down",
		Started:    started,
		Workspaces: openWorkspaces,
	}
	if measureOnly {
		manifest.Notes = append(manifest.Notes,
			"MEASUREMENT RUN: the phase budgets in e2e/realtest/budgets.go are reported and NOT enforced. "+
				"The log harvest below IS enforced.")
	}
	manifest.Notes = append(manifest.Notes,
		"no daemon was running when this run started: the startup measured below INCLUDES bringing one up")

	quitStandingEmacs(ctx, t, client, "realtest 3's cold start")

	// CHECKED AGAIN AFTER THE QUIT. An Emacs still running with no daemon
	// retries its ensure, so one could have come up between the first check
	// and the quit; and a daemon Emacs did spawn in that gap would outlive it.
	// Confirming the precondition once, early, would have confirmed it about a
	// moment that has passed.
	requireNoDaemonRunning(ctx, t, "after the standing Emacs was quit")

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

	logMeasurements(t, fmt.Sprintf("cold start with the daemon down via %s: daemon %s",
		launch.Method, orUnknown(phases.DaemonPath)), measurements)

	assertEveryWorkspaceDrawn(t, run, openWorkspaces, phases)
	if phases.DaemonPath != "spawned" {
		t.Errorf("the daemon path this cold start took reads %q, not \"spawned\": no daemon was running on the way "+
			"in, so the frontend was supposed to start one of its own (docs/REALTEST-PLAN.md, startup item 3)",
			orUnknown(phases.DaemonPath))
	}
	assertDaemonNowServing(ctx, t)
	verifyVendorGuard(ctx, t, client, run)

	driver := proveKeyDriver(ctx, t, client, runDir, &manifest)

	shown := showEmacsAndWaitForPaint(ctx, t, run, client, driver, sources, snapshot,
		launch.SpawnedAt, openWorkspaces, &manifest)
	shownMeasurements := shown.Measure()
	manifest.Runs[len(manifest.Runs)-1].Measurements = shownMeasurements

	logMeasurements(t, "cold start with the daemon down: after Emacs was shown for the key self-test:", shownMeasurements)

	assertEveryWorkspacePainted(t, run, openWorkspaces, shown)
	manifest.BudgetBreaches = append(manifest.BudgetBreaches,
		prefixEach("cold start (daemon down): ", CheckBudgets(shownMeasurements))...)

	// THE FEEDBACK, READ AFTER THE SHOW PHASE, for the reason the shared
	// marker set states: "loading workspaces (n/m)…" is driven by the painted
	// count, and no panel paints until Emacs is brought forward.
	assertSpawnFeedback(t, sources, snapshot, launch.SpawnedAt, len(openWorkspaces), &manifest)

	verdict := "left focus alone"
	if launch.DisturbedOwner {
		verdict = fmt.Sprintf("MOVED FOCUS from %q to %q", launch.FrontBefore, launch.FrontAfter)
	}
	note := fmt.Sprintf("launch method `%s`: %s", launch.Method, verdict)
	manifest.Notes = append(manifest.Notes, note)
	t.Logf("%s", note)

	finishStartupRun(ctx, t, client, env, snapshot, openWorkspaces, runDir, &manifest, measureOnly)
}

// requireNoDaemonRunning refuses the run while any resident daemon is up.
//
// IT DOES NOT KILL ANYTHING. Stopping the owner's daemon is the owner's act or
// the lead's, performed deliberately, and bin/realtest.sh already sets the
// precedent for what a harness does when it meets a daemon it cannot work
// with: it declines and names the pid and the ways to stop it. A test that
// killed the daemon to make its own precondition true would also be a test
// that could kill a daemon on a run where the precondition was a mistake.
//
// `when` says WHICH of the two checks failed, because they mean different
// things: before the run means the lead has not stopped it yet, after the quit
// means something brought one up while the run was in flight.
func requireNoDaemonRunning(ctx context.Context, t *testing.T, when string) {
	t.Helper()
	deadline := time.Now().Add(daemonDownCeiling)
	for {
		pids, err := daemonPIDs(ctx)
		if err != nil {
			t.Fatalf("look for a running daemon %s: %v", when, err)
		}
		if len(pids) == 0 {
			t.Logf("no `%s` process is running %s: the cold start below has to bring one up", daemonPattern, when)
			return
		}
		if time.Now().After(deadline) {
			t.Fatalf("%d `%s` process(es) are still running %s (pids %v), and realtest 3 measures a startup with "+
				"the daemon DOWN. This test does not stop the owner's daemon. Either let bin/realtest.sh do it "+
				"with AGENT_REPL_REALTEST_STOP_DAEMON=1 (a consent of its own, because stopping the daemon ends "+
				"every live session it holds), or stop it deliberately (SPC o C-d from the editor, or kill %v) and "+
				"run realtest 3 on its own (docs/REALTEST-PLAN.md, startup item 3).",
				len(pids), daemonPattern, when, pids, pids)
		}
		select {
		case <-ctx.Done():
			return
		case <-time.After(pollInterval):
		}
	}
}

// assertDaemonNowServing checks that the cold start actually left a daemon
// running, and exactly one.
//
// The log says the frontend started one; this says the process exists. They
// can disagree — a daemon that started and exited writes `elisp.daemon.started`
// and leaves nothing — and when they do, the log is the claim and the process
// table is the fact.
func assertDaemonNowServing(ctx context.Context, t *testing.T) {
	t.Helper()
	pids, err := daemonPIDs(ctx)
	if err != nil {
		t.Errorf("look for the daemon this cold start spawned: %v", err)
		return
	}
	switch len(pids) {
	case 1:
		t.Logf("the cold start left one daemon serving, pid %d", pids[0])
	case 0:
		t.Errorf("no `%s` process is running after the cold start, so whatever the log says was started is not "+
			"there now: the daemon exited during the startup this run measured", daemonPattern)
	default:
		t.Errorf("%d `%s` processes are running after the cold start (pids %v), where one was to be spawned: "+
			"a startup that leaves rival daemons behind is a leak the next realtest will inherit",
			len(pids), daemonPattern, pids)
	}
}

// assertSpawnFeedback checks what the owner saw while the daemon came up, and
// what they must NOT have seen.
//
// PRESENT: the frontend's own `elisp.daemon.started`, the mode-line lifecycle
// through `starting` and `linking` to `ready`, and each matching minibuffer
// echo. ABSENT: the adoption records. The workspace-progress echo is asserted
// only when the state database holds a workspace to load, since with an empty
// roster there is no count to report.
//
// The build is REPORTED into the manifest and not asserted either way, for the
// reason the file header gives: under bin/realtest.sh's readiness refusal the
// tree is fresh, so the staleness check finds nothing to do — but a run that
// did build measured a different startup and the phase table must say so.
func assertSpawnFeedback(t *testing.T, sources []Source, snap Snapshot, spawnedAt time.Time,
	openWorkspaces int, manifest *Manifest) {
	t.Helper()
	want := []startupFeedbackMarker{
		feedbackDaemonStarted,
		feedbackLifecycleStarting,
		feedbackLifecycleLinking,
		feedbackLifecycleReady,
		feedbackEchoStarting,
		feedbackEchoLinking,
		feedbackEchoReady,
	}
	if openWorkspaces > 0 {
		want = append(want, feedbackEchoLoadingWorkspaces)
	}
	unwanted := []startupFeedbackMarker{
		feedbackDaemonAdopted,
		feedbackLifecycleAdopted,
		feedbackEchoAdopted,
	}
	reported := []startupFeedbackMarker{rt3BuildRan, rt3BuildSkipped}

	all := append(append(append([]startupFeedbackMarker{}, want...), unwanted...), reported...)
	seen, err := readStartupFeedback(sources, snap, spawnedAt, all)
	if err != nil {
		t.Fatalf("read the startup feedback records: %v", err)
	}
	t.Logf("the feedback the owner saw while the daemon came up:")
	assertFeedbackPresent(t, seen, spawnedAt, want)
	assertFeedbackAbsent(t, seen, unwanted)

	switch {
	case seen[rt3BuildRan.ID].Count > 0:
		note := fmt.Sprintf("A BUILD RAN during this startup (%s): the phase table above includes it, so it is not "+
			"comparable with a run whose tree was already fresh.", seen[rt3BuildRan.ID].Raw)
		manifest.Notes = append(manifest.Notes, note)
		t.Logf("%s", note)
	case seen[rt3BuildSkipped.ID].Count > 0:
		note := "no build ran: the staleness check found the tree fresh, which is what bin/realtest.sh's readiness " +
			"refusal guarantees. The phase table above is spawn cost with no build in it."
		manifest.Notes = append(manifest.Notes, note)
		t.Logf("%s", note)
	default:
		note := "neither a build nor a build-skipped record was written during this startup, so what the staleness " +
			"check decided is not on the record at all."
		manifest.Notes = append(manifest.Notes, note)
		t.Logf("%s", note)
	}
}
