//go:build realtest

package realtest

import (
	"context"
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"
)

// REALTEST 6 — REGISTER A DIRECTORY, CLOSE IT, RE-OPEN IT.
//
// docs/REALTEST-PLAN.md, "Workspaces", item 6: "Register a directory (`SPC TAB
// C-n`); re-open a closed workspace (`SPC TAB O`)."
//
// RE-OPEN IS THE CAPITAL SINCE THE 2026-09-12 OWNER RULING. `SPC TAB o` is the
// one-shot create now, and a harness still pressing it raises "One-shot
// commission: " and would create a workspace where it meant to re-open one.
//
// What it asserts, in order:
//
//   - `SPC TAB C-n` REACHES ITS COMMAND. The chord is pressed as real key
//     events and the register command's own prompt, "Add project directory: ",
//     is read back out of the minibuffer; it is then aborted with a real `C-g`.
//   - REGISTERING PRODUCES A WORKSPACE AND A TAB. The daemon MINTS the
//     identity for the directory it is handed — that is what register is
//     (lisp/commands.el) — so the state database gains a workspace whose `dir`
//     is the scratch repository, Emacs's roster holds an open row for it, the
//     tab bar's own order holds its tab, and the log carries both
//     `elisp.commands.add-project-registered` and `elisp.roster.tab-open:`
//     naming it.
//   - CLOSING REMOVES THE TAB AND KEEPS THE WORKSPACE RE-OPENABLE. The tab
//     leaves the drawn order and the roster row goes to closed, and the state
//     database STILL HOLDS the record — a close that forgot the record would
//     have nothing to re-open, which is the whole distinction between close
//     and nuke.
//   - `SPC TAB O` REACHES ITS COMMAND, and re-opening RESTORES THE SAME
//     WORKSPACE. The identity asserted is the workspace id, byte for byte,
//     because the id is the identity and a path has many spellings: a re-open
//     that minted a second workspace at the same directory would look right in
//     the tab bar and be a different workspace underneath. The tab is drawn
//     again for that same id.
//
// A DEDICATED SCRATCH REPOSITORY (lead's standing decision, 2026-09-12). The
// directory this test registers is one it creates under the run directory
// (`git init`, one commit), never one of the owner's. Real git runs, because
// registering a directory makes the daemon read a real repository — that is
// inherent to driving the real product and is a different thing from the
// no-real-git rule governing the unit and integration suites.
//
// AND IT CLEANS UP AFTER ITSELF, through `t.Cleanup` so a failure halfway
// still tears down what it had made: the workspace is closed, then FORGOTTEN
// — through the command-file ingress, the only door onto the `Forget` verb
// today (daemon/internal/workspace/forget.go is not yet on the rpc surface) —
// and the scratch repository is deleted. A forget the daemon refuses is
// reported loudly rather than hidden, and does not fail this run: cleanup
// runs after the verdict (wsActCleanupRegistered).
//
// A DELIBERATE, STATED DEVIATION FROM "REAL KEYS" for the acts (authorized by
// the lead, 2026-09-12; the substrate's "The acts" commentary carries the full
// reasoning). Both chords are real. The minibuffer answers are supplied
// through the read-only probe transport, entering the same user-facing
// commands with `call-interactively`: the register command reads a DIRECTORY
// PATH, which typing character by character through the completion UI would
// test file-name completion rather than the product, and the re-open command's
// picker is a `completing-read` with `require-match` over the closed rows.
//
// THE REMEDIATION BAR IS THE LOG HARVEST, the same one realtest 1 carries:
// every WARN and ERROR written inside the run window across every log, with no
// allowlist, into the run's MANIFEST.md, and the test fails when the count is
// non-zero.
//
// PRECONDITION FOR THE LEAD: run this realtest in its own `bin/realtest.sh
// -run` invocation. It performs a cold start, and a cold start refuses to run
// against an Emacs that is already answering.
func TestRealtestRegisterAndReopen(t *testing.T) {
	if os.Getenv(runGateEnv) != "1" {
		t.Skipf("realtest 6 drives the owner's real editor and runs only through bin/realtest.sh, which sets %s=1", runGateEnv)
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
				"Set them in e2e/realtest/budgets.go from an observed measurement, "+
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
			fmt.Sprintf("realtest-6-%s", time.Now().Format("20060102-150405")))
	}
	if err := os.MkdirAll(runDir, 0o755); err != nil {
		t.Fatalf("create the run directory %s: %v", runDir, err)
	}
	t.Logf("realtest 6 run directory: %s", runDir)

	socket := os.Getenv(socketEnv)
	if socket == "" {
		socket = filepath.Join(os.TempDir(), fmt.Sprintf("emacs%d", os.Getuid()), "server")
	}
	client := &Client{Socket: socket, Scratch: runDir}
	t.Logf("emacs server socket: %s", socket)
	t.Logf("emacsclient: %s", EmacsClientPath)

	stateDir := filepath.Join(home, ".claude-emacs")
	dbPath := StateDBPath(stateDir)
	openBefore, closedBefore, err := ReadWorkspaces(ctx, dbPath)
	if err != nil {
		t.Fatalf("read the workspaces the state database holds: %v", err)
	}
	allBefore := append(append([]Workspace{}, openBefore...), closedBefore...)
	t.Logf("before the run the state database holds %d open workspace(s) and %d closed",
		len(openBefore), len(closedBefore))

	env := RealEnv(home, openBefore)
	sources, err := EnumerateSources(env)
	if err != nil {
		t.Fatalf("enumerate the logs to harvest: %v", err)
	}
	t.Logf("harvesting %d log source(s); the module's own global sink is %s", len(sources), env.ModuleLog)

	started := time.Now()
	snapshot := TakeSnapshot(sources)

	manifest := Manifest{
		Title:      "Realtest 6 — register a directory; re-open a closed workspace",
		Started:    started,
		Workspaces: openBefore,
	}
	if measureOnly {
		manifest.Notes = append(manifest.Notes,
			"MEASUREMENT RUN: the phase budgets in e2e/realtest/budgets.go are reported and NOT enforced. "+
				"The log harvest below IS enforced.")
	}

	// ---- The startup this realtest stands on --------------------------

	const run = 1
	launch := coldStart(ctx, t, client, run)
	phases := waitForUsable(ctx, t, run, sources, snapshot, launch.SpawnedAt, openBefore)
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
	t.Logf("cold start via %s: daemon %s", launch.Method, orUnknown(phases.DaemonPath))
	for _, m := range measurements {
		if m.Note != "" {
			t.Logf("  phase %-14s %-14s NOT OBSERVED: %s", m.Phase, m.Workspace, m.Note)
			continue
		}
		t.Logf("  phase %-14s %-14s %s from spawn", m.Phase, m.Workspace, m.Elapsed.Round(time.Millisecond))
	}
	assertEveryWorkspaceDrawn(t, run, openBefore, phases)
	verifyVendorGuard(ctx, t, client, run)

	driver := wsActKeyDriver(ctx, t, client, runDir, &manifest)
	shown := showEmacsAndWaitForPaint(ctx, t, run, client, driver, sources, snapshot,
		launch.SpawnedAt, openBefore, &manifest)
	shownMeasurements := shown.Measure()
	manifest.Runs[len(manifest.Runs)-1].Measurements = shownMeasurements
	assertEveryWorkspacePainted(t, run, openBefore, shown)
	manifest.BudgetBreaches = append(manifest.BudgetBreaches,
		prefixEach("cold start: ", CheckBudgets(shownMeasurements))...)

	// ---- The act: `SPC TAB C-n` ---------------------------------------

	scratch := wsActScratchRepo(t, runDir, "scratch-repo")
	t.Cleanup(func() { wsActRemoveScratchRepo(t, scratch) })

	wsActProveChord(ctx, t, client, driver,
		[]Chord{wsActLeader, wsActTab, wsActRegisterKey}, "Add project directory:", &manifest)

	registerStarted := time.Now()
	if err := wsActRegisterDirectory(ctx, client, scratch); err != nil {
		t.Fatalf("register the scratch repository %s through `SPC TAB C-n`'s command: %v", scratch, err)
	}

	afterRegister := wsActWaitForDB(ctx, t, "the registry to hold the registered directory", dbPath,
		func(all []Workspace) bool {
			_, ok := wsActWorkspaceByDir(all, scratch)
			return ok
		})
	registered, ok := wsActWorkspaceByDir(afterRegister, scratch)
	if !ok {
		fresh := wsActNewSince(allBefore, afterRegister)
		names := make([]string, 0, len(fresh))
		for _, ws := range fresh {
			names = append(names, fmt.Sprintf("%s (%s at %s)", ws.ID, ws.Name, ws.Dir))
		}
		t.Fatalf("`SPC TAB C-n` was entered with %s and the state database holds no workspace at that "+
			"directory: registering minted no identity. The records that did appear are: %s",
			scratch, strings.Join(names, "; "))
	}
	t.Logf("the register minted workspace %s (%s) for %s, %s after the command was entered",
		registered.ID, registered.Name, registered.Dir, time.Since(registerStarted).Round(time.Millisecond))

	rows, err := wsActRosterRows(ctx, client)
	if err != nil {
		t.Fatalf("read the roster after the register: %v", err)
	}
	row, hasRow := wsActRowByID(rows, registered.ID)
	tabName := row.Name
	if tabName == "" {
		tabName = registered.Name
	}
	t.Cleanup(func() { wsActCleanupRegistered(ctx, t, client, dbPath, registered, tabName) })

	if !hasRow {
		t.Errorf("the register minted workspace %s for %s and Emacs's roster holds no row for it, so the "+
			"editor was never told the workspace exists", registered.ID, scratch)
	} else if row.Closed {
		t.Errorf("the roster row for the registered workspace %s (%q) is CLOSED, so registering produced a "+
			"workspace with no tab", registered.ID, tabName)
	}

	allNow := append(append([]Workspace{}, allBefore...), registered)
	actSources, err := EnumerateSources(RealEnv(home, allNow))
	if err != nil {
		t.Fatalf("re-enumerate the logs now that the run has registered a workspace: %v", err)
	}

	minted, err := wsActScan(actSources, snapshot, registerStarted, wsActRegisteredRe)
	if err != nil {
		t.Fatalf("read the log for the register command's own record: %v", err)
	}
	if hit, found := wsActHitNaming(minted, registered.ID); found {
		t.Logf("the log records the register: %s", hit.Message)
	} else {
		refused, scanErr := wsActScan(actSources, snapshot, registerStarted, wsActRegisterRefusedRe)
		if scanErr != nil {
			t.Fatalf("read the log for the register command's refusal record: %v", scanErr)
		}
		if len(refused) > 0 {
			t.Errorf("the register command recorded a REFUSAL rather than a minted identity: %s", refused[0].Message)
		} else {
			t.Errorf("no `elisp.commands.add-project-registered` record names the registered workspace %s, "+
				"so nothing in the log says the daemon accepted the directory", registered.ID)
		}
	}

	tabs := wsActWaitForTabs(ctx, t, fmt.Sprintf("the tab bar to draw a tab for %q", tabName), client,
		func(tabs []string) bool { return wsActHasTab(tabs, tabName) })
	if !wsActHasTab(tabs, tabName) {
		t.Errorf("the register minted workspace %s (%q) and the tab bar's own order does not hold a tab for it: "+
			"the drawn order is %v", registered.ID, tabName, tabs)
	} else {
		t.Logf("the tab bar draws %q; the order is %v", tabName, tabs)
	}

	opened, err := wsActScan(actSources, snapshot, registerStarted, wsActTabOpenRe)
	if err != nil {
		t.Fatalf("read the log for the registered workspace's tab-open record: %v", err)
	}
	if hit, found := wsActHitNaming(opened, registered.ID); found {
		t.Logf("the log records the tab opening: %s", hit.Message)
	} else if hit, found := wsActHitNaming(opened, tabName); found {
		t.Logf("the log records the tab opening: %s", hit.Message)
	} else {
		t.Errorf("no `elisp.roster.tab-open:` record names the registered workspace %s (%q), so nothing in the "+
			"log says the tab was drawn", registered.ID, tabName)
	}

	// ---- The act: close it, and keep it re-openable --------------------

	closeStarted := time.Now()
	if err := wsActCloseWorkspace(ctx, client, tabName); err != nil {
		t.Fatalf("close the registered workspace %s (%q): %v", registered.ID, tabName, err)
	}

	afterClose := wsActWaitForDB(ctx, t, "the registry to mark the workspace closed", dbPath,
		func(all []Workspace) bool {
			_, _, closedNow, readErr := wsActWorkspacesNow(ctx, dbPath)
			if readErr != nil {
				return false
			}
			_, isClosed := wsActWorkspaceByID(closedNow, registered.ID)
			return isClosed
		})
	if _, still := wsActWorkspaceByID(afterClose, registered.ID); !still {
		t.Fatalf("closing the workspace %s (%q) removed its record from the state database entirely, so there "+
			"is nothing left to re-open. Close is a VIEW act; forgetting the record is what nuke does",
			registered.ID, tabName)
	}
	_, _, closedNow, err := wsActWorkspacesNow(ctx, dbPath)
	if err != nil {
		t.Fatalf("read the state database after the close: %v", err)
	}
	if _, isClosed := wsActWorkspaceByID(closedNow, registered.ID); !isClosed {
		t.Errorf("the close was sent and the state database still holds workspace %s (%q) as OPEN, so nothing "+
			"marked it closed", registered.ID, tabName)
	} else {
		t.Logf("the registry holds %s as closed and re-openable, %s after the close",
			registered.ID, time.Since(closeStarted).Round(time.Millisecond))
	}

	closedTabs := wsActWaitForTabs(ctx, t, fmt.Sprintf("the tab bar to drop the tab for %q", tabName), client,
		func(tabs []string) bool { return !wsActHasTab(tabs, tabName) })
	if wsActHasTab(closedTabs, tabName) {
		t.Errorf("the workspace %s (%q) was closed and the tab bar's own order still holds its tab: the drawn "+
			"order is %v", registered.ID, tabName, closedTabs)
	} else {
		t.Logf("the tab bar dropped %q; the order is now %v", tabName, closedTabs)
	}

	closedRows, err := wsActRosterRows(ctx, client)
	if err != nil {
		t.Fatalf("read the roster after the close: %v", err)
	}
	if row, present := wsActRowByID(closedRows, registered.ID); !present {
		t.Errorf("the workspace %s (%q) was closed and Emacs's roster holds no row for it at all, so the "+
			"re-open picker has nothing to offer", registered.ID, tabName)
	} else if !row.Closed {
		t.Errorf("the workspace %s (%q) was closed and its roster row still reads open", registered.ID, tabName)
	}

	// ---- The act: `SPC TAB O` ------------------------------------------

	wsActProveChord(ctx, t, client, driver,
		[]Chord{wsActLeader, wsActTab, wsActOpenKey}, "Open workspace:", &manifest)

	reopenStarted := time.Now()
	if err := wsActOpenWorkspace(ctx, client, tabName); err != nil {
		t.Fatalf("re-open the closed workspace %q through `SPC TAB O`'s command: %v", tabName, err)
	}

	afterReopen := wsActWaitForDB(ctx, t, "the registry to hold the workspace open again", dbPath,
		func(all []Workspace) bool {
			_, openNow, _, readErr := wsActWorkspacesNow(ctx, dbPath)
			if readErr != nil {
				return false
			}
			_, isOpen := wsActWorkspaceByID(openNow, registered.ID)
			return isOpen
		})
	reopened, still := wsActWorkspaceByID(afterReopen, registered.ID)
	if !still {
		t.Fatalf("re-opening %q did not restore workspace %s: the state database no longer holds that record "+
			"at all", tabName, registered.ID)
	}
	_, openNow, _, err := wsActWorkspacesNow(ctx, dbPath)
	if err != nil {
		t.Fatalf("read the state database after the re-open: %v", err)
	}
	if _, isOpen := wsActWorkspaceByID(openNow, registered.ID); !isOpen {
		t.Errorf("the re-open was sent and the state database still holds workspace %s (%q) as closed",
			registered.ID, tabName)
	} else {
		t.Logf("the registry holds %s open again, %s after the re-open",
			registered.ID, time.Since(reopenStarted).Round(time.Millisecond))
	}

	// THE IDENTITY IS THE ID, byte for byte. A re-open that minted a second
	// workspace at the same directory would draw a tab that looks right and be
	// a different workspace underneath, which is exactly what this comparison
	// exists to catch — so the directory is checked as well, and a second
	// record naming the same directory is a finding in its own right.
	if reopened.ID != registered.ID {
		t.Errorf("the re-opened workspace has id %s and the closed one had %s: re-opening did not restore the "+
			"same workspace", reopened.ID, registered.ID)
	}
	if !wsActSameDir(reopened.Dir, scratch) {
		t.Errorf("the re-opened workspace %s names directory %s, not the registered %s", reopened.ID, reopened.Dir, scratch)
	}
	duplicates := 0
	for _, ws := range afterReopen {
		if wsActSameDir(ws.Dir, scratch) {
			duplicates++
		}
	}
	if duplicates != 1 {
		t.Errorf("%d workspace records name the scratch repository %s after the re-open, not one: re-opening "+
			"minted a second identity for a directory that already had one", duplicates, scratch)
	}

	reopenedRows, err := wsActRosterRows(ctx, client)
	if err != nil {
		t.Fatalf("read the roster after the re-open: %v", err)
	}
	reopenedRow, present := wsActRowByID(reopenedRows, registered.ID)
	if !present {
		t.Errorf("the workspace %s was re-opened and Emacs's roster holds no row for it", registered.ID)
	} else if reopenedRow.Closed {
		t.Errorf("the workspace %s was re-opened and its roster row still reads closed", registered.ID)
	}
	reopenedName := reopenedRow.Name
	if reopenedName == "" {
		reopenedName = tabName
	}

	reopenedTabs := wsActWaitForTabs(ctx, t, fmt.Sprintf("the tab bar to draw a tab for %q again", reopenedName),
		client, func(tabs []string) bool { return wsActHasTab(tabs, reopenedName) })
	if !wsActHasTab(reopenedTabs, reopenedName) {
		t.Errorf("the workspace %s (%q) was re-opened and the tab bar's own order does not hold a tab for it: "+
			"the drawn order is %v", registered.ID, reopenedName, reopenedTabs)
	} else {
		t.Logf("the tab bar draws %q again; the order is %v", reopenedName, reopenedTabs)
	}

	reopenedSources, err := EnumerateSources(RealEnv(home, afterReopen))
	if err != nil {
		t.Fatalf("re-enumerate the logs after the re-open: %v", err)
	}
	reopenedMarkers, err := wsActScan(reopenedSources, snapshot, reopenStarted, wsActTabOpenRe)
	if err != nil {
		t.Fatalf("read the log for the re-opened workspace's tab-open record: %v", err)
	}
	if hit, found := wsActHitNaming(reopenedMarkers, registered.ID); found {
		t.Logf("the log records the tab re-opening: %s", hit.Message)
	} else if hit, found := wsActHitNaming(reopenedMarkers, reopenedName); found {
		t.Logf("the log records the tab re-opening: %s", hit.Message)
	} else {
		t.Errorf("no `elisp.roster.tab-open:` record names workspace %s (%q) after the re-open, so nothing in "+
			"the log says the tab was drawn again", registered.ID, reopenedName)
	}

	// ---- The harvest ---------------------------------------------------

	manifest.Ended = time.Now()
	finalSources, err := EnumerateSources(RealEnv(home, afterReopen))
	if err != nil {
		t.Fatalf("re-enumerate the logs after the run: %v", err)
	}
	harvest, err := HarvestSources(finalSources, snapshot, Window{Start: started, End: manifest.Ended}, afterReopen)
	if err != nil {
		t.Fatalf("harvest the logs: %v", err)
	}
	manifest.Workspaces = afterReopen
	manifest.Findings = harvest.Findings
	manifest.InfoCounts = harvest.InfoCounts

	messages, msgErr := client.Messages(ctx)
	if msgErr != nil {
		manifest.Findings = append(manifest.Findings, Finding{
			Kind:      KindMalformed,
			Source:    "*Messages*",
			Path:      "(emacs buffer)",
			Workspace: GlobalWorkspace,
			Note:      fmt.Sprintf("Emacs's *Messages* buffer could not be read, so this source was not harvested: %v", msgErr),
		})
	} else {
		manifest.Findings = append(manifest.Findings, HarvestMessages(messages, 0, afterReopen)...)
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
