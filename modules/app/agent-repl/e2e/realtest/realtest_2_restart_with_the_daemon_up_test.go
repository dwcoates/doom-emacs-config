//go:build realtest

package realtest

import (
	"context"
	"fmt"
	"os"
	"path/filepath"
	"testing"
	"time"
)

// REALTEST 2 — RESTART WITH THE DAEMON UP.
//
// docs/REALTEST-PLAN.md, "Startup and shape", item 2: "Quit and restart Emacs
// with the daemon still up. Same measurements; the daemon is adopted, not
// rebuilt."
//
// SAME MEASUREMENTS AS REALTEST 1, DIFFERENT PRECONDITION. Every phase, tab,
// panel, key and harvest assertion here is realtest 1's, called through
// realtest 1's own helpers rather than restated: doom-boot, daemon-answered,
// link-up, roster-subscribed, tab-drawn per workspace and webview-armed in the
// hidden window; focus-edge and panel-painted in the show window; the log
// harvest as the remediation bar. What this test adds is the precondition and
// the assertion that the precondition was honoured:
//
//	A DAEMON IS ALREADY SERVING when Emacs starts, and the new Emacs must ADOPT
//	it. Adoption is asserted four ways, because each catches a different way
//	the run could be wrong about it: `elisp.daemon.adopted` is present and
//	`elisp.daemon.started` is absent (the frontend's own account of which path
//	it took); the mode-line lifecycle settles on `adopted` and never on `ready`
//	(what the owner saw while waiting); Phases.DaemonPath reads "adopted" (what
//	the measurement report will say); and the daemon's PID after the restart is
//	the SAME PID as before it (what actually happened, independent of anything
//	the frontend claims).
//
// THE QUIT IS AN ACT OF THIS TEST, NOT ITS SETUP. The plan's item is "quit and
// restart", so the quit happens inside the run window and its records are
// harvested like everything else. It goes through the existing takeover path —
// Client.Kill behind the AGENT_REPL_REALTEST_TAKEOVER flag, exactly as
// bin/realtest.sh's second refusal does it (quitStandingEmacs says why the
// script's own check cannot cover this one). No new way to close the owner's
// editor is invented here.
//
// THE DAEMON SURVIVING THE QUIT IS ITSELF AN ASSERTION. The plan's premise is
// that the daemon is still up after Emacs goes away; the frontend spawns it as
// a `make-process` child (lisp/daemon.el,
// `agent-repl--frontend-spawn-daemon`), so whether it outlives the parent is a
// property of the product and not something this test may assume. If the
// daemon dies with Emacs, this test fails naming that — which is the finding,
// not a harness problem to work around by restarting it.
//
// NOTHING IS REBUILT. The plan says "adopted, not rebuilt", and
// bin/realtest.sh's readiness refusal already guarantees the tree is fresh, so
// no build echo is expected on either startup realtest and none is asserted.

// restartCeiling bounds how long the run waits for the daemon that was serving
// before the quit to still be serving after it.
//
// It is a SETTLING bound, not a bring-up one: the daemon is already running and
// nothing here is waiting for it to start, only for the quit's own dust to
// clear before the pid is read back. It is deliberately short for that reason —
// a daemon that needs longer than this to still exist is a daemon that is
// exiting, which is the finding.
const restartCeiling = 15 * time.Second

func TestRealtestRestartWithTheDaemonUp(t *testing.T) {
	if os.Getenv(runGateEnv) != "1" {
		t.Skipf("realtest 2 drives the owner's real editor and runs only through bin/realtest.sh, which sets %s=1", runGateEnv)
	}
	ctx := context.Background()
	measureOnly := os.Getenv(measureEnv) == "1"
	requireMeasuredBudgets(t, measureOnly)

	home, err := os.UserHomeDir()
	if err != nil {
		t.Fatalf("resolve the owner's home directory: %v", err)
	}
	runDir := startupRunDir(t, home, "realtest-2")
	t.Logf("realtest 2 run directory: %s", runDir)

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

	// THE PRECONDITION, READ BEFORE THE WINDOW OPENS. A daemon that is not
	// there cannot be adopted, and a run that discovered that after quitting
	// the owner's editor would have closed it to measure nothing.
	daemonBefore := requireOneDaemonServing(ctx, t,
		"realtest 2 measures an ADOPTION, so a daemon must already be serving when Emacs starts")

	// The daemon about to be adopted is verified guarded BEFORE it is adopted,
	// not after. bin/realtest.sh declines on an unguarded daemon for exactly
	// this reason, and this is the same check at the moment it matters: from
	// here on the new Emacs will be talking to this process, and every shim it
	// spawns inherits its environment.
	assertGuarded(ctx, t, 1, "daemon (to be adopted)", daemonBefore)

	// THE WINDOW OPENS HERE, before the quit: the quit is the test's first act
	// and everything it writes belongs to this run.
	started := time.Now()
	snapshot := TakeSnapshot(sources)

	manifest := Manifest{
		Title:      "Realtest 2 — restart with the daemon up",
		Started:    started,
		Workspaces: openWorkspaces,
	}
	if measureOnly {
		manifest.Notes = append(manifest.Notes,
			"MEASUREMENT RUN: the phase budgets in e2e/realtest/budgets.go are reported and NOT enforced. "+
				"The log harvest below IS enforced.")
	}
	manifest.Notes = append(manifest.Notes,
		fmt.Sprintf("the daemon serving before the restart was pid %d", daemonBefore))

	quit := quitStandingEmacs(ctx, t, client, "realtest 2's restart")
	manifest.Notes = append(manifest.Notes,
		map[bool]string{
			true:  "the standing Emacs was quit by this run; the restart below is the plan's second half",
			false: "no Emacs was answering, so there was nothing to quit; the restart below still measures an adoption",
		}[quit])

	assertDaemonSurvivedTheQuit(ctx, t, daemonBefore)

	// ONE START, as realtest 1 does it: the same coldStart helper, so the
	// launch method, the focus observation and the server wait cannot drift
	// between the two startup realtests.
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

	logMeasurements(t, fmt.Sprintf("restart via %s: daemon %s", launch.Method, orUnknown(phases.DaemonPath)), measurements)

	assertEveryWorkspaceDrawn(t, run, openWorkspaces, phases)
	if phases.DaemonPath != "adopted" {
		t.Errorf("the daemon path this restart took reads %q, not \"adopted\": a daemon was serving on the way in "+
			"(pid %d), so the frontend was supposed to attach to it rather than start one of its own "+
			"(docs/REALTEST-PLAN.md, startup item 2)", orUnknown(phases.DaemonPath), daemonBefore)
	}
	verifyVendorGuard(ctx, t, client, run)

	// THE KEY DRIVER, PROVEN, and the first focus edge with it: Emacs is
	// visible-but-unfocused up to here, and this is what activates it, which
	// is what drains the parked pre-creation queue.
	driver := proveKeyDriver(ctx, t, client, runDir, &manifest)

	shown := showEmacsAndWaitForPaint(ctx, t, run, client, driver, sources, snapshot,
		launch.SpawnedAt, openWorkspaces, &manifest)
	shownMeasurements := shown.Measure()
	manifest.Runs[len(manifest.Runs)-1].Measurements = shownMeasurements

	logMeasurements(t, "restart: after Emacs was shown for the key self-test:", shownMeasurements)

	assertEveryWorkspacePainted(t, run, openWorkspaces, shown)
	manifest.BudgetBreaches = append(manifest.BudgetBreaches,
		prefixEach("restart: ", CheckBudgets(shownMeasurements))...)

	// THE FEEDBACK, READ AFTER THE SHOW PHASE. The lifecycle records land
	// early, but "loading workspaces (n/m)…" is driven by the painted count,
	// which does not move until Emacs is brought forward — so reading before
	// the show would report a line as missing that had simply not happened
	// yet (startup_shared_test.go says the same at the marker).
	assertAdoptionFeedback(t, sources, snapshot, launch.SpawnedAt, len(openWorkspaces))

	// THE SAME PROCESS, not merely a daemon. Everything above is the
	// frontend's own account of what it did; this is the kernel's.
	assertSameDaemonAdopted(ctx, t, daemonBefore)

	verdict := "left focus alone"
	if launch.DisturbedOwner {
		verdict = fmt.Sprintf("MOVED FOCUS from %q to %q", launch.FrontBefore, launch.FrontAfter)
	}
	note := fmt.Sprintf("launch method `%s`: %s", launch.Method, verdict)
	manifest.Notes = append(manifest.Notes, note)
	t.Logf("%s", note)

	finishStartupRun(ctx, t, client, env, snapshot, openWorkspaces, runDir, &manifest, measureOnly)
}

// requireOneDaemonServing reads the resident daemon's pid, and refuses when
// there is not exactly one.
//
// EXACTLY ONE, and the two refusals are separate because they are opposite
// problems. None means the precondition does not hold and the run would be
// measuring a spawn while claiming to measure an adoption — realtest 3's
// subject, taken by accident. More than one means "the daemon" is ambiguous:
// the frontend attaches to whichever address file is current, so the pid this
// test would compare across the restart is a guess, and an adoption assertion
// resting on a guess is not one.
func requireOneDaemonServing(ctx context.Context, t *testing.T, why string) int {
	t.Helper()
	pids, err := daemonPIDs(ctx)
	if err != nil {
		t.Fatalf("look for the resident daemon: %v", err)
	}
	switch len(pids) {
	case 1:
		t.Logf("one daemon is serving, pid %d", pids[0])
		return pids[0]
	case 0:
		t.Fatalf("no `%s` process is running, and %s. Bring one up first — realtest 1 leaves one behind, "+
			"and so does starting Emacs by hand — then run this test.", daemonPattern, why)
	default:
		t.Fatalf("%d `%s` processes are running (pids %v), so which one Emacs will adopt is not determined. "+
			"Stop all but the one this run should measure, then try again.", len(pids), daemonPattern, pids)
	}
	return 0
}

// assertDaemonSurvivedTheQuit checks the plan's own premise: after Emacs goes
// away, the daemon is still up.
//
// It waits rather than reading once, because the quit and the daemon's own
// reaction to losing its frontend are concurrent, and a pid read the instant
// the socket stopped answering would sometimes catch a daemon that was about
// to exit and sometimes one that already had. The wait is short (restartCeiling
// says why) and it is a wait for the pid to STAY alive, so it is settled by
// time and not by the first sample.
func assertDaemonSurvivedTheQuit(ctx context.Context, t *testing.T, pid int) {
	t.Helper()
	deadline := time.Now().Add(restartCeiling)
	for time.Now().Before(deadline) {
		if !processAlive(pid) {
			t.Fatalf("the daemon (pid %d) exited when Emacs was quit, so there is nothing left to adopt and "+
				"realtest 2's precondition cannot hold. docs/REALTEST-PLAN.md, startup item 2, is that the daemon "+
				"is still up across a restart: a daemon that dies with its frontend is the finding here, not a "+
				"harness problem — the run stops rather than starting a fresh daemon and calling it an adoption", pid)
		}
		select {
		case <-ctx.Done():
			return
		case <-time.After(pollInterval):
		}
	}
	t.Logf("the daemon (pid %d) is still serving %s after Emacs was quit", pid, restartCeiling)
}

// assertSameDaemonAdopted checks that the daemon standing after the restart is
// the same PROCESS that was standing before it.
//
// This is the assertion the frontend's own records cannot make. `elisp.daemon.
// adopted` says the frontend believed it attached to something already
// answering; a daemon that died and was replaced between the two — by a
// launchd job, by a stale-address retry, by anything — could still produce that
// record. The pid is what distinguishes "adopted the running daemon" from
// "found a daemon running".
func assertSameDaemonAdopted(ctx context.Context, t *testing.T, before int) {
	t.Helper()
	pids, err := daemonPIDs(ctx)
	if err != nil {
		t.Errorf("look for the resident daemon after the restart: %v", err)
		return
	}
	for _, pid := range pids {
		if pid == before {
			if len(pids) > 1 {
				t.Errorf("the daemon serving before the restart (pid %d) is still running, but %d `%s` processes "+
					"are now up (pids %v): the restart left an extra daemon behind, which is a leak whether or not "+
					"the adoption itself was correct", before, len(pids), daemonPattern, pids)
			}
			t.Logf("the adopted daemon is the same process that was serving before the restart, pid %d", before)
			return
		}
	}
	t.Errorf("the daemon serving before the restart (pid %d) is no longer among the running `%s` processes (%v), "+
		"so this restart did not adopt the daemon it was supposed to adopt: it replaced it",
		before, daemonPattern, pids)
}

// assertAdoptionFeedback checks what the owner saw during the restart, and
// what they must NOT have seen.
//
// PRESENT: the mode-line lifecycle passing through `linking` and settling on
// `adopted`, the matching minibuffer echoes, and the frontend's own
// `elisp.daemon.adopted` record. ABSENT: everything the SPAWN path writes.
// Asserting only the presence would pass a run that adopted a daemon and then
// started a second one anyway, which is the exact defect the adoption
// assertions exist to catch.
//
// The workspace-progress echo is asserted only when the state database holds a
// workspace to load; with an empty roster there is no count to report and
// requiring the line would be requiring the module to invent one.
func assertAdoptionFeedback(t *testing.T, sources []Source, snap Snapshot, spawnedAt time.Time, openWorkspaces int) {
	t.Helper()
	want := []startupFeedbackMarker{
		feedbackDaemonAdopted,
		feedbackLifecycleLinking,
		feedbackLifecycleAdopted,
		feedbackEchoLinking,
		feedbackEchoAdopted,
	}
	if openWorkspaces > 0 {
		want = append(want, feedbackEchoLoadingWorkspaces)
	}
	unwanted := []startupFeedbackMarker{
		feedbackDaemonStarted,
		feedbackLifecycleStarting,
		feedbackLifecycleReady,
		feedbackEchoStarting,
		feedbackEchoReady,
	}

	seen, err := readStartupFeedback(sources, snap, spawnedAt, append(append([]startupFeedbackMarker{}, want...), unwanted...))
	if err != nil {
		t.Fatalf("read the startup feedback records: %v", err)
	}
	t.Logf("the feedback the owner saw during the restart:")
	assertFeedbackPresent(t, seen, spawnedAt, want)
	assertFeedbackAbsent(t, seen, unwanted)
}
