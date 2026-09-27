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

// REALTEST 5 — CREATE A WORKSPACE, WORK IN IT, DELETE IT.
//
// docs/REALTEST-PLAN.md, "Workspaces", item 5: "Create a workspace (`SPC TAB
// n`), work in it, delete it. The tab appears, the workspace is usable, the
// tab goes away."
//
// What it asserts, in order:
//
//   - `SPC TAB n` REACHES ITS COMMAND. The chord is pressed as real key events
//     and the create command's own first prompt, "Initial prompt: ", is read
//     back out of the minibuffer, then aborted with a real `C-g`. Since the
//     2026-09-12 creation ruling that prompt is shared with the other dynamic
//     modes (`SPC TAB c`, `SPC TAB f`), so it is not evidence on its own that
//     THIS chord ran; Emacs's own `(recent-keys)` is asserted to carry the
//     sequence as well, and the two together identify the command.
//   - THE CREATE PRODUCES A ROSTER ROW AND A DRAWN TAB. The state database
//     gains exactly one workspace, Emacs's roster holds a row for it, and the
//     tab bar's own order (`agent-repl-roster--tab-order`) holds its tab, with
//     `elisp.roster.tab-open:` naming it in the log.
//   - ITS PANEL BECOMES USABLE. Per the settled paint-on-show model realtest 1
//     established (owner ruling, 2026-09-11), Emacs is brought forward BEFORE
//     the acts, so the focus edge the parked pre-creation queue waits on has
//     already happened and the new workspace's panel is required to paint —
//     `elisp.frontend.watch-load: load-changed`, or the parked drain's
//     `precreate-created ws=NAME reason=focused`.
//   - THE DELETE REMOVES THE TAB, THE ROW AND EVERY TRACE. The tab leaves the
//     drawn order, the roster row goes, the state database forgets the record,
//     and the worktree is gone from disk. "No orphan" is asserted against the
//     state database rather than against the roster, because the roster is a
//     view Emacs was pushed and a stale view claiming a record is gone is
//     precisely the defect the assertion exists to catch.
//
// "WORK IN IT" IS THE USABLE PANEL, not a conversation. The vendor is
// forbidden for the whole run and a prompt belongs to realtest 9; what this
// realtest is answerable for is that the workspace the owner just made is one
// they could work in — a drawn tab, a roster row, a painted panel.
//
// A DEDICATED SCRATCH REPOSITORY, AND REAL GIT (lead's standing decision,
// 2026-09-12). Creating a workspace makes the editor ask the daemon for a git
// worktree, so real git genuinely runs: that is inherent to driving the real
// product and is a different thing from the no-real-git rule that governs the
// unit and integration suites. What is fixed is WHERE. This test creates its
// own repository under the run directory (`git init`, one commit) and acts
// only against that; the owner's repositories are never touched. Everything it
// creates it removes, through `t.Cleanup` so a failure halfway still tears
// down what it had made — the registered scratch directory is closed, then
// FORGOTTEN through the command-file ingress, and a forget the daemon refuses
// is reported in the run's own output rather than left to be found
// (wsActCleanupRegistered says exactly what happened).
//
// THE REGISTER IS BOOTSTRAP HERE, NOT THE SUBJECT. Since the 2026-09-12
// creation ruling `SPC TAB n` takes its repository from the roster section the
// CURRENT workspace's row sits in — it asks for nothing but the prompt — so the
// scratch repository is registered first and the editor lands on the workspace
// that register minted (`agent-repl-verbs-select-minted`). That standing place
// IS the repository the create will use, and it is asserted before the act
// rather than assumed (wsActRequireDynamicRepository): a create against the
// wrong current workspace would write into a repository this run is not
// allowed to touch. Registering is realtest 6's subject and is asserted there;
// here it is only the ground the create stands on.
//
// A DELIBERATE, STATED DEVIATION FROM "REAL KEYS" for the act itself
// (authorized by the lead, 2026-09-12; the substrate's "The acts" commentary
// carries the full reasoning). The chord is real. The ONE minibuffer answer the
// create command asks for — its initial prompt, answered blank — is supplied
// through the read-only probe transport, entering the same user-facing command
// with `call-interactively`.
//
// THE REMEDIATION BAR IS THE LOG HARVEST, the same one realtest 1 carries:
// every WARN and ERROR written inside the run window across every log, with no
// allowlist, into the run's MANIFEST.md, and the test fails when the count is
// non-zero.
//
// PRECONDITION FOR THE LEAD: run this realtest in its own `bin/realtest.sh
// -run` invocation. It performs a cold start, and a cold start refuses to run
// against an Emacs that is already answering — so two act realtests in one
// `go test` process would have the second one refuse against the editor the
// first one left standing.
func TestRealtestCreateWorkDeleteAWorkspace(t *testing.T) {
	if os.Getenv(runGateEnv) != "1" {
		t.Skipf("realtest 5 drives the owner's real editor and runs only through bin/realtest.sh, which sets %s=1", runGateEnv)
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
			fmt.Sprintf("realtest-5-%s", time.Now().Format("20060102-150405")))
	}
	if err := os.MkdirAll(runDir, 0o755); err != nil {
		t.Fatalf("create the run directory %s: %v", runDir, err)
	}
	t.Logf("realtest 5 run directory: %s", runDir)

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

	// THE WINDOW OPENS HERE, before anything is launched, and the snapshot is
	// taken at the same moment: everything after this point is the run's.
	started := time.Now()
	snapshot := TakeSnapshot(sources)

	manifest := Manifest{
		Title:      "Realtest 5 — create a workspace, work in it, delete it",
		Started:    started,
		Workspaces: openBefore,
	}
	if measureOnly {
		manifest.Notes = append(manifest.Notes,
			"MEASUREMENT RUN: the phase budgets in e2e/realtest/budgets.go are reported and NOT enforced. "+
				"The log harvest below IS enforced.")
	}

	// ---- The startup this realtest stands on --------------------------
	//
	// It is a precondition, not the subject: realtest 1 measures a cold
	// start, and the phases are read and reported here so a slow or broken
	// bring-up cannot be mistaken for a slow or broken create.
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

	// ---- Emacs is shown, once, before any act -------------------------
	//
	// The parked pre-creation queue drains on the first focus edge and not
	// before, so a workspace created while Emacs had never been shown could
	// not paint a panel and the assertion that it does would be asserting a
	// bug that isn't one. Showing it here is what makes "the panel becomes
	// usable" an honest assertion about the create.
	driver := wsActKeyDriver(ctx, t, client, runDir, &manifest)
	shown := showEmacsAndWaitForPaint(ctx, t, run, client, driver, sources, snapshot,
		launch.SpawnedAt, openBefore, &manifest)
	shownMeasurements := shown.Measure()
	manifest.Runs[len(manifest.Runs)-1].Measurements = shownMeasurements
	assertEveryWorkspacePainted(t, run, openBefore, shown)
	manifest.BudgetBreaches = append(manifest.BudgetBreaches,
		prefixEach("cold start: ", CheckBudgets(shownMeasurements))...)

	// ---- The scratch repository, and the bootstrap register -----------

	scratch := wsActScratchRepo(t, runDir, "scratch-repo")
	t.Cleanup(func() { wsActRemoveScratchRepo(t, scratch) })

	if err := wsActRegisterDirectory(ctx, client, scratch); err != nil {
		t.Fatalf("register the scratch repository %s so the create command has a repository to pick: %v", scratch, err)
	}
	afterRegister := wsActWaitForDB(ctx, t, "the registry to hold the scratch repository", dbPath,
		func(all []Workspace) bool {
			_, ok := wsActWorkspaceByDir(all, scratch)
			return ok
		})
	registered, ok := wsActWorkspaceByDir(afterRegister, scratch)
	if !ok {
		t.Fatalf("the scratch repository %s was registered but the state database never gained a workspace for it, "+
			"so the editor has nowhere to stand and `SPC TAB n` would derive no repository", scratch)
	}
	t.Logf("bootstrap: the scratch repository registered as workspace %s (%s)", registered.ID, registered.Name)

	registeredRows, err := wsActRosterRows(ctx, client)
	if err != nil {
		t.Fatalf("read the roster after registering the scratch repository: %v", err)
	}
	registeredRow, _ := wsActRowByID(registeredRows, registered.ID)
	registeredName := registeredRow.Name
	if registeredName == "" {
		registeredName = registered.Name
	}
	t.Cleanup(func() { wsActCleanupRegistered(ctx, t, client, dbPath, registered, registeredName) })

	// The dynamic create derives its repository from where the editor is
	// STANDING, so that is what has to be true before the act — not that the
	// scratch repository appears in some picker.
	wsActRequireDynamicRepository(ctx, t, client, scratch)

	// ---- The act: `SPC TAB n` -----------------------------------------

	rt5ProveCreateChord(ctx, t, client, driver, &manifest)

	actStarted := time.Now()
	if err := wsActCreateWorkspace(ctx, client); err != nil {
		t.Fatalf("create a workspace in the scratch repository through `SPC TAB n`'s command: %v", err)
	}

	afterCreate := wsActWaitForDB(ctx, t, "the registry to hold the created workspace", dbPath,
		func(all []Workspace) bool { return len(wsActNewSince(afterRegister, all)) > 0 })
	fresh := wsActNewSince(afterRegister, afterCreate)
	if len(fresh) == 0 {
		t.Fatalf("`SPC TAB n` was entered against the scratch repository %s and the state database gained no "+
			"workspace at all: the create produced no roster row", scratch)
	}
	if len(fresh) > 1 {
		names := make([]string, 0, len(fresh))
		for _, ws := range fresh {
			names = append(names, fmt.Sprintf("%s (%s at %s)", ws.ID, ws.Name, ws.Dir))
		}
		t.Errorf("one create produced %d new workspace records, not one: %s", len(fresh), strings.Join(names, "; "))
	}
	created := fresh[0]
	t.Logf("the create produced workspace %s (%s) at %s, %s after the command was entered",
		created.ID, created.Name, created.Dir, time.Since(actStarted).Round(time.Millisecond))
	// THE NAME IS THE DAEMON'S SINCE THE 2026-09-12 RULING, and with a blank
	// prompt there is nothing to name the workspace after, so the daemon names
	// it after the workspace's own minted id and issues no headless naming call
	// at all (daemon/internal/workspace/create.go, `branchFor`). Nothing here
	// predicts a name: every assertion below is keyed on the id, and the tab
	// name is read back off the roster row for that id.

	// The nuke is the test's own last act, but it is registered as a cleanup
	// the moment the workspace exists: a failure between here and there must
	// still leave the owner's registry without this row.
	createdRows, err := wsActRosterRows(ctx, client)
	if err != nil {
		t.Fatalf("read the roster after the create: %v", err)
	}
	createdRow, hasRow := wsActRowByID(createdRows, created.ID)
	tabName := createdRow.Name
	if tabName == "" {
		tabName = created.Name
	}
	t.Cleanup(func() { wsActCleanupCreated(ctx, t, client, dbPath, created, tabName) })

	if !hasRow {
		t.Errorf("the create produced workspace %s (%s) in the state database but Emacs's roster holds no row "+
			"for it, so the editor was never told the workspace exists", created.ID, created.Name)
	} else if createdRow.Closed {
		t.Errorf("the roster row for the created workspace %s (%s) is CLOSED, so the create produced a workspace "+
			"with no tab", created.ID, tabName)
	}

	// ---- The tab appears ----------------------------------------------

	allNow := append(append([]Workspace{}, allBefore...), created, registered)
	actSources, err := EnumerateSources(RealEnv(home, allNow))
	if err != nil {
		t.Fatalf("re-enumerate the logs now that the run has created workspaces: %v", err)
	}

	tabs := wsActWaitForTabs(ctx, t, fmt.Sprintf("the tab bar to draw a tab for %q", tabName), client,
		func(tabs []string) bool { return wsActHasTab(tabs, tabName) })
	if !wsActHasTab(tabs, tabName) {
		t.Errorf("the create produced workspace %s (%s) and the tab bar's own order does not hold a tab for it: "+
			"the drawn order is %v", created.ID, tabName, tabs)
	} else {
		t.Logf("the tab bar draws %q; the order is %v", tabName, tabs)
	}

	opened, err := wsActScan(actSources, snapshot, actStarted, wsActTabOpenRe)
	if err != nil {
		t.Fatalf("read the log for the created workspace's tab-open record: %v", err)
	}
	if hit, found := wsActHitNaming(opened, created.ID); found {
		t.Logf("the log records the tab opening: %s", hit.Message)
	} else if hit, found := wsActHitNaming(opened, tabName); found {
		t.Logf("the log records the tab opening: %s", hit.Message)
	} else {
		t.Errorf("no `elisp.roster.tab-open:` record names the created workspace %s (%s) after the create, "+
			"so nothing in the log says the tab was drawn", created.ID, tabName)
	}

	// ---- The panel becomes usable -------------------------------------

	var painted Phases
	waitUntil(ctx, t, fmt.Sprintf("the created workspace %q to paint its panel", tabName), showCeiling,
		func() bool {
			read, readErr := ReadPhases(actSources, snapshot, launch.SpawnedAt)
			if readErr != nil {
				t.Fatalf("read the phases for the created workspace's panel: %v", readErr)
			}
			painted = read
			return matchedInPainted(setOf(read.PaintedWorkspaces()), Workspace{ID: created.ID, Name: tabName})
		})
	if !matchedInPainted(setOf(painted.PaintedWorkspaces()), Workspace{ID: created.ID, Name: tabName}) {
		t.Errorf("the created workspace %s (%s) never painted a panel, though Emacs had already been shown: "+
			"no `elisp.frontend.watch-load: load-changed` and no `elisp.webview-recovery.precreate-created "+
			"ws=%s reason=focused` record names it. The workspace was created but it is not one the owner "+
			"could work in", created.ID, tabName, tabName)
	} else {
		t.Logf("the created workspace %q painted its panel", tabName)
	}

	// The created workspace's own log links die with its worktree, so the
	// targets behind them are captured while it still stands. Without this
	// the harvest would silently skip every record this workspace wrote and
	// still report a clean run.
	preserved := wsActPreserveSinks(created)
	t.Logf("preserved %d of the created workspace's log targets for the harvest", len(preserved))

	// ---- The act: delete ----------------------------------------------
	//
	// NUKE, not close and not kill. The plan says the tab goes away and the
	// lead's assertion says no orphan is left, and nuke is the one verb that
	// deletes the worktree, deletes the branch AND forgets the registry
	// record. Close and kill both leave the row standing, closed.

	deleteStarted := time.Now()
	if err := wsActNukeWorkspace(ctx, client, tabName); err != nil {
		t.Fatalf("delete the created workspace %s (%s): %v", created.ID, tabName, err)
	}

	afterDelete := wsActWaitForDB(ctx, t, "the registry to forget the deleted workspace", dbPath,
		func(all []Workspace) bool {
			_, still := wsActWorkspaceByID(all, created.ID)
			return !still
		})
	if leftover, still := wsActWorkspaceByID(afterDelete, created.ID); still {
		t.Errorf("the delete was sent and the state database still holds the record for %s (%s) at %s: "+
			"the workspace is an ORPHAN", leftover.ID, leftover.Name, leftover.Dir)
	} else {
		t.Logf("the registry forgot the workspace %s after %s", created.ID,
			time.Since(deleteStarted).Round(time.Millisecond))
	}

	afterTabs := wsActWaitForTabs(ctx, t, fmt.Sprintf("the tab bar to drop the tab for %q", tabName), client,
		func(tabs []string) bool { return !wsActHasTab(tabs, tabName) })
	if wsActHasTab(afterTabs, tabName) {
		t.Errorf("the workspace %s (%s) was deleted and the tab bar's own order still holds its tab: "+
			"the drawn order is %v", created.ID, tabName, afterTabs)
	} else {
		t.Logf("the tab bar dropped %q; the order is now %v", tabName, afterTabs)
	}

	finalRows, err := wsActRosterRows(ctx, client)
	if err != nil {
		t.Fatalf("read the roster after the delete: %v", err)
	}
	if row, still := wsActRowByID(finalRows, created.ID); still {
		t.Errorf("the workspace %s was deleted and Emacs's roster still holds a row for it (%q, closed=%v): "+
			"the editor was never told the record is gone", created.ID, row.Name, row.Closed)
	}

	if _, err := os.Stat(created.Dir); err == nil {
		t.Errorf("the workspace %s was deleted and its worktree is still on disk at %s: the delete left an "+
			"orphaned working tree", created.ID, created.Dir)
	} else if !os.IsNotExist(err) {
		t.Errorf("check whether the deleted workspace's worktree %s is gone: %v", created.Dir, err)
	} else {
		t.Logf("the deleted workspace's worktree %s is gone from disk", created.Dir)
	}

	// The teardown record is reported rather than required. It is written on
	// the workspace's own sink AFTER the daemon has already deleted the
	// worktree that sink's link lives in, so whether it survives at all is a
	// property of the logging layout and not of the delete; the tab order,
	// the roster and the registry above are what the delete is judged on.
	tornDown, err := wsActScan(wsActMergeSources(actSources, preserved), snapshot, deleteStarted, wsActTabTeardownRe)
	if err != nil {
		t.Fatalf("read the log for the deleted workspace's tab-teardown record: %v", err)
	}
	if hit, found := wsActHitNaming(tornDown, tabName); found {
		t.Logf("the log records the tab teardown: %s", hit.Message)
	} else {
		t.Logf("no `elisp.roster.tab-teardown:` record naming %q was readable after the delete; the record is "+
			"written on the workspace's own sink, whose link the nuke had already destroyed", tabName)
	}

	// ---- The harvest ---------------------------------------------------

	manifest.Ended = time.Now()
	finalSources, err := EnumerateSources(RealEnv(home, allNow))
	if err != nil {
		t.Fatalf("re-enumerate the logs after the run: %v", err)
	}
	finalSources = wsActMergeSources(finalSources, preserved)
	harvest, err := HarvestSources(finalSources, snapshot, Window{Start: started, End: manifest.Ended}, allNow)
	if err != nil {
		t.Fatalf("harvest the logs: %v", err)
	}
	manifest.Workspaces = allNow
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
		// The whole buffer IS the window: this run started the process.
		manifest.Findings = append(manifest.Findings, HarvestMessages(messages, 0, allNow)...)
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

// ---- The act's chord ---------------------------------------------------

// rt5ProveCreateChord presses `SPC TAB n` as real key events and proves it
// reached `agent-repl-create-workspace`.
//
// TWO FACTS, BECAUSE THE PROMPT IS NO LONGER UNIQUE. Before the 2026-09-12
// creation ruling the create was the only command that opened with
// "Repository: " and the prompt alone identified it. The dynamic modes now
// share "Initial prompt: " — `SPC TAB c` and `SPC TAB f` ask the same first
// question — so the prompt says only that one of them ran, and Emacs's own
// `(recent-keys)` is asserted to carry the sequence as well. It is the only
// thing that says which key completed the chord.
//
// Like the substrate's proof, a failure here does not stop the run: the chord
// and the verb are separate claims, and a run that stopped would say nothing
// about whether creating a workspace works at all.
func rt5ProveCreateChord(ctx context.Context, t *testing.T, client *Client, driver *KeyDriver, manifest *Manifest) {
	t.Helper()
	sequence := []Chord{wsActLeader, wsActTab, wsActNewWorkspaceKey}
	if !wsActProveChord(ctx, t, client, driver, sequence, "Initial prompt:", manifest) {
		return
	}
	keys, err := RecentKeys(ctx, client)
	if err != nil {
		t.Errorf("read Emacs's own (recent-keys) to tell `SPC TAB n` from the other dynamic modes: %v", err)
		return
	}
	spelled, recorded := wsActSpell(sequence), SpellRecorded(sequence)
	if !strings.Contains(keys, recorded) {
		note := fmt.Sprintf("`%s` put a dynamic mode's prompt up, but Emacs's own (recent-keys) does not "+
			"contain %q, so the prompt cannot be credited to this chord: `SPC TAB c` and `SPC TAB f` ask the "+
			"same first question. recent-keys ends with: %s", spelled, recorded, tail(keys, 120))
		manifest.Notes = append(manifest.Notes, note)
		t.Errorf("%s", note)
		return
	}
	note := fmt.Sprintf("`%s` is confirmed as the CREATE command and not one of the other dynamic modes: "+
		"Emacs's own (recent-keys) contains %q and the prompt it raised was the dynamic create's",
		spelled, recorded)
	manifest.Notes = append(manifest.Notes, note)
	t.Logf("%s", note)
}
