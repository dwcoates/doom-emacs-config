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

// REALTEST 8: REORDER BY PRIORITY; CLOSE, REOPEN, KILL A WORKSPACE.
//
// docs/REALTEST-PLAN.md, "Workspaces", item 8.
//
// What it asserts, in order:
//
//   - PRIORITY REORDERS THE DRAWN TAB BAR. Two workspaces of this run's own are
//     given priorities that contradict the order they are currently drawn in,
//     and the DRAWN ORDER is asserted to have swapped, read out of
//     `agent-repl-roster--tab-order`, the one list the bar is drawn from. The
//     stored `workspaces.priority` is checked first and separately, because
//     "the verb did not land" and "it landed and the bar did not follow" send a
//     reader to two different places.
//   - CLOSE REMOVES THE TAB AND KEEPS THE WORKSPACE REOPENABLE. `SPC j d` as
//     real key events, then `workspaces.closed` at 1, the name gone from the
//     drawn order, and the roster row still present and marked closed, which is
//     what the reopen picker offers back.
//   - REOPEN RESTORES THE SAME WORKSPACE. `SPC TAB O`'s chord is proven, and
//     the identity asserted is the workspace id byte for byte: `OpenWorkspace`
//     takes the ref off the closed roster row and the daemon mints nothing on
//     that path, so a different id would mean a second workspace standing where
//     the first one was, with the first one's conversation stranded behind it.
//   - KILL ENDS THE SESSION AND LEAVES NO ORPHAN. `SPC j x` as real key events,
//     then the session terminal recorded as `killed`, the row at closed=1 and
//     still present, the tab gone, the workspace still offered by the reopen
//     picker, and the three orphan questions below all answered no.
//
// WHY THE ORDER ASSERTION IS A SWAP AND NOT AN ABSOLUTE POSITION
//
// The daemon's resolver orders the roster
// (daemon/internal/resolve/sidebar/order.go, `lessWorkspace`): priority rank
// ascending (P0.5, P1, P2, P3, and everything unprioritized at a rank of its
// own, last), then most-recently-selected first, then name, then id. The
// owner's own workspaces are in that same order and this run must not touch
// them, so an absolute position would be an assertion about the owner's
// registry rather than about priority. The run therefore records where ITS TWO
// workspaces sit relative to each other, gives the LATER one the higher
// priority and the EARLIER one the lower, and requires exactly that pair to
// change places. That is the smallest statement that is entirely about the
// mechanism.
//
// BOTH are set rather than one, so the resolver has to move both: a single
// change could be satisfied by a tie-break that happened to agree, and a swap
// of two stated ranks cannot.
//
// WHY THE KILL ASSERTION READS LIVENESS AND NOT THE SOCKET FILE
//
// A kill stops the process; nothing unlinks the socket node, which is cleared
// on the NEXT spawn or at daemon boot (`shimsocket.ClearStale`). So a
// `<id>.sock` still sitting on disk after a kill is correct behavior, and
// asserting on its absence would fail a healthy system. Three independent
// questions are asked instead, because a leak can pass any two of them:
//
//   - Is the pid the daemon recorded still running? That is the process the
//     daemon believed it owned.
//   - Is any process listening on a socket belonging to this workspace? That
//     catches a shim the daemon LOST TRACK of, an adopted survivor or a
//     relaunched generation on `<id>.nN.sock`, which is exactly the orphan that
//     would hold the workspace lock and refuse the next session.
//   - Does anything ANSWER on those nodes? Only a connect separates a stale
//     node from a live listener.
//
// THE DEVIATIONS FROM "REAL KEYS", each with its reason. The substrate's "The
// acts" commentary carries the general case; what is specific here:
//
//   - CLOSE AND KILL NEED NO DEVIATION AT ALL. Neither command asks anything:
//     both act on the workspace the editor is standing on and issue their verb
//     immediately. So `SPC j d` and `SPC j x` are pressed as real key events
//     and the act IS the keypress, with nothing stubbed. That is also why they
//     cannot be proven the way the substrate proves a chord, which waits for a
//     command's first minibuffer prompt: there is no prompt, so rt8PressAct
//     proves them through Emacs's own `(recent-keys)` and through the effect
//     itself, and asserts no minibuffer was left standing.
//   - STANDING ON A WORKSPACE before acting on it. Close, kill and the priority
//     command all act on the CURRENT workspace, so each act must be preceded by
//     a selection, and selection is realtest 4's own subject (`s-{`, `s-}`,
//     `M-<n>`). Driving it by numeral here would make this test's outcome
//     depend on which slot of the whole bar the run's workspace landed in,
//     which is a fact about the owner's registry. The switch enters
//     `agent-repl-switch-to-project`, the module's own command, through its own
//     project-root argument, and the run then asserts the editor really is
//     standing where it asked.
//   - THE PRIORITY PICKER. `SPC j m p`'s chord is real and proven by its own
//     "Priority: " prompt; the choice is then made through the same command
//     with only its `completing-read` answered, for the reason the substrate
//     gives about `require-match` pickers.
//   - THE REOPEN PICKER, identically, and through the substrate's own
//     `wsActOpenWorkspace`.
//
// A DEDICATED SCRATCH REPOSITORY (lead's standing decision, 2026-09-12), the
// same one realtests 5 and 6 use and through the same substrate. This run needs
// TWO workspaces in one repository to have an order to reorder, so it registers
// the scratch repository and then CREATES a second workspace inside it with
// `SPC TAB n`'s command, which is the ordinary way a second workspace comes to
// exist. Everything it creates it destroys through `t.Cleanup`, so a failure
// halfway still tears down; what the product has no verb to remove is stated
// rather than hidden, in wsActCleanupRegistered.
//
// A NOTE FOR THE LEAD ON THE VENDOR GUARD. Nothing here submits a prompt, so
// nothing here should reach `createRealQuery`, where the guard throws. REOPEN
// does bring a session up, and that bring-up spawns a shim -- which is safe
// under the guard rather than refused by it, because a guarded daemon spawns
// every shim with `--fake` (rt78_shared.go carries the account). No second
// variable is needed on the launch.
//
// ALSO A PRECONDITION FOR THE LEAD: run this realtest in its own
// `bin/realtest.sh -run` invocation. It performs a cold start, and a cold start
// refuses to run against an Emacs that is already answering.
//
// THE REMEDIATION BAR IS THE LOG HARVEST, the same one realtest 1 carries:
// every WARN and ERROR written inside the run window across every log, with no
// allowlist, into the run's MANIFEST.md, and the test fails when the count is
// non-zero.

// The chords realtest 8 presses beyond the substrate's own. Defined here
// rather than in keys.go because parallel authors are working in that file.
var (
	// rt8ClaudePrefix is the `j` of `SPC j`, the leader map's claude prefix.
	rt8ClaudePrefix = Chord{
		Emacs: "j", Keycode: 38,
		Why: "the claude prefix of the leader map; on its own it only opens that prefix",
	}
	// rt8ModifyPrefix is the `m` of `SPC j m`, the modify-workspace prefix.
	rt8ModifyPrefix = Chord{
		Emacs: "m", Keycode: 46,
		Why: "the modify-workspace prefix; on its own it only opens that prefix",
	}
	// rt8PriorityKey is the `p` of `SPC j m p`, bound to
	// `agent-repl-set-priority`.
	rt8PriorityKey = Chord{
		Emacs: "p", Keycode: 35,
		Why: "completes `SPC j m p`, which opens the priority picker",
	}
	// rt8CloseKey is the `d` of `SPC j d`, bound to
	// `agent-repl-close-workspace`.
	rt8CloseKey = Chord{
		Emacs: "d", Keycode: 2,
		Why: "completes `SPC j d`, which closes the current workspace; a VIEW act that leaves the session alone",
	}
	// rt8KillKey is the `x` of `SPC j x`, bound to
	// `agent-repl-kill-workspace`. Its neighbour `SPC j X` is the nuke, which
	// destroys data; the two differ by a shift, which is why this test also
	// asserts that the killed workspace survives as a closed row.
	rt8KillKey = Chord{
		Emacs: "x", Keycode: 7,
		Why: "completes `SPC j x`, which kills the current workspace's session by force",
	}
)

// rt8PriorityHigh and rt8PriorityLow are the two labels the picker offers that
// this test uses, and the ranks behind them.
//
// P1 and P3 rather than P0.5 and P3 for a plain reason: what matters is that
// two stated ranks disagree with the drawn order, and 1 against 3 separates
// them exactly as 0 against 3 would while keeping both labels short enough to
// read in a failure message.
const (
	rt8PriorityHigh = "P1"
	rt8PriorityLow  = "P3"
)

func TestRealtestPriorityCloseReopenKill(t *testing.T) {
	if os.Getenv(runGateEnv) != "1" {
		t.Skipf("realtest 8 drives the owner's real editor and runs only through bin/realtest.sh, which sets %s=1", runGateEnv)
	}
	ctx := context.Background()
	measureOnly := os.Getenv(measureEnv) == "1"
	requireMeasuredBudgets(t, measureOnly)

	home, err := os.UserHomeDir()
	if err != nil {
		t.Fatalf("resolve the owner's home directory: %v", err)
	}
	runDir := startupRunDir(t, home, "realtest-8")
	t.Logf("realtest 8 run directory: %s", runDir)

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
		Title:      "Realtest 8: priority, close, reopen, kill",
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

	manifest.Runs = append(manifest.Runs, ManifestRun{
		Index:        run,
		Method:       launch.Method,
		SpawnedAt:    launch.SpawnedAt,
		DaemonPath:   phases.DaemonPath,
		FrontBefore:  launch.FrontBefore,
		FrontAfter:   launch.FrontAfter,
		Disturbed:    launch.DisturbedOwner,
		Measurements: phases.Measure(),
	})
	t.Logf("cold start via %s: daemon %s", launch.Method, orUnknown(phases.DaemonPath))
	logMeasurements(t, "hidden startup", phases.Measure())
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

	// ---- Two workspaces of this run's own, in one repository ----------

	scratch := wsActScratchRepo(t, runDir, "scratch-repo")
	t.Cleanup(func() { wsActRemoveScratchRepo(t, scratch) })

	if err := wsActRegisterDirectory(ctx, client, scratch); err != nil {
		t.Fatalf("register the scratch repository %s through `SPC TAB C-n`'s command: %v", scratch, err)
	}
	afterRegister := wsActWaitForDB(ctx, t, "the registry to hold the registered directory", dbPath,
		func(all []Workspace) bool {
			_, ok := wsActWorkspaceByDir(all, scratch)
			return ok
		})
	alpha, ok := wsActWorkspaceByDir(afterRegister, scratch)
	if !ok {
		t.Fatalf("registering %s minted no workspace, so this run has nothing to order. The records that "+
			"appeared are: %s", scratch, rt8Describe(wsActNewSince(allBefore, afterRegister)))
	}
	alphaName := rt8TabName(ctx, t, client, alpha)
	t.Cleanup(func() { wsActCleanupRegistered(ctx, t, client, dbPath, alpha, alphaName) })
	wsActWaitForTabs(ctx, t, fmt.Sprintf("the tab bar to draw %q", alphaName), client,
		func(tabs []string) bool { return wsActHasTab(tabs, alphaName) })
	t.Logf("the registered workspace is %s (%q) at %s", alpha.ID, alphaName, alpha.Dir)

	// The second workspace is made with the DYNAMIC create, which takes its
	// repository from the workspace the editor is standing on — the one
	// register just minted and selected — and lets the daemon name it. That
	// standing place is asserted rather than assumed: a create against the
	// wrong current workspace would write into a repository this run is not
	// allowed to touch.
	wsActRequireDynamicRepository(ctx, t, client, scratch)
	if err := wsActCreateWorkspace(ctx, client); err != nil {
		t.Fatalf("create the second workspace through `SPC TAB n`'s command: %v", err)
	}
	afterCreate := wsActWaitForDB(ctx, t, "the registry to hold the created workspace", dbPath,
		func(all []Workspace) bool { return len(wsActNewSince(afterRegister, all)) > 0 })
	fresh := wsActNewSince(afterRegister, afterCreate)
	if len(fresh) != 1 {
		t.Fatalf("creating the second workspace produced %d new record(s), not one: %s. This run needs "+
			"exactly two workspaces of its own to have an order to reorder",
			len(fresh), rt8Describe(fresh))
	}
	beta := fresh[0]
	betaName := rt8TabName(ctx, t, client, beta)
	t.Cleanup(func() { wsActCleanupCreated(ctx, t, client, dbPath, beta, betaName) })
	wsActWaitForTabs(ctx, t, fmt.Sprintf("the tab bar to draw %q", betaName), client,
		func(tabs []string) bool { return wsActHasTab(tabs, betaName) })
	t.Logf("the created workspace is %s (%q) at %s", beta.ID, betaName, beta.Dir)

	// The created workspace's log sinks are captured while its worktree still
	// stands: the cleanup nukes it, and a nuke deletes the LINK, not the bytes
	// (wsActPreserveSinks carries the whole reasoning).
	preserved := wsActPreserveSinks(beta)

	// ---- The act: reorder by priority ---------------------------------

	_, later := rt8AssertPriorityReordersTheBar(ctx, t, client, dbPath, driver, &manifest,
		rt8Named{ID: alpha.ID, Name: alphaName, Dir: alpha.Dir},
		rt8Named{ID: beta.ID, Name: betaName, Dir: beta.Dir})

	// The workspace that is closed, reopened and killed is the one the reorder
	// left FIRST, so the three lifecycle acts do not also disturb an assertion
	// that has already been made about the other one.
	victim := later

	// ---- The acts: close, reopen, kill --------------------------------

	rt8AssertCloseKeepsItReopenable(ctx, t, client, dbPath, driver, &manifest, victim)
	rt8AssertReopenRestoresTheSameWorkspace(ctx, t, client, dbPath, driver, &manifest, victim)
	rt8AssertKillLeavesNoOrphan(ctx, t, client, dbPath, stateDir, driver, &manifest, victim)

	// ---- The harvest ---------------------------------------------------

	manifest.Ended = time.Now()
	finalAll, _, _, err := wsActWorkspacesNow(ctx, dbPath)
	if err != nil {
		t.Fatalf("read the state database before the harvest: %v", err)
	}
	finalSources := wsActMergeSources(rt8Sources(t, home, finalAll), preserved)
	harvest, err := HarvestSources(finalSources, snapshot, Window{Start: started, End: manifest.Ended}, finalAll)
	if err != nil {
		t.Fatalf("harvest the logs: %v", err)
	}
	manifest.Workspaces = finalAll
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
		manifest.Findings = append(manifest.Findings, HarvestMessages(messages, 0, finalAll)...)
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

// rt8Named is one workspace under the three names the three layers of this test
// speak: the id wsm.db uses, the name the tab bar draws, and the directory the
// project switch takes. Correlating them once, here, is how a "missing tab"
// stops being a wrong answer about the product produced by a wrong answer about
// an identity.
type rt8Named struct {
	ID   string
	Name string
	Dir  string
}

// ---- Priority ------------------------------------------------------------

// rt8AssertPriorityReordersTheBar is the plan's first act: "Reorder by
// priority."
//
// It returns the two workspaces in their POST-REORDER order, so the caller can
// pick which one the lifecycle acts operate on without re-reading the bar.
func rt8AssertPriorityReordersTheBar(ctx context.Context, t *testing.T, client *Client, dbPath string,
	driver *KeyDriver, manifest *Manifest, alpha, beta rt8Named) (rt8Named, rt8Named) {
	t.Helper()

	tabs, err := wsActTabOrder(ctx, client)
	if err != nil {
		t.Fatalf("read the drawn tab order before the reorder: %v", err)
	}
	alphaAt, betaAt := rt8IndexOf(tabs, alpha.Name), rt8IndexOf(tabs, beta.Name)
	if alphaAt < 0 || betaAt < 0 {
		t.Fatalf("the drawn tab bar does not hold both of this run's workspaces (%q at %d, %q at %d), so "+
			"there is no order between them to reorder. The bar is %v",
			alpha.Name, alphaAt, beta.Name, betaAt, tabs)
	}
	earlier, later := alpha, beta
	if betaAt < alphaAt {
		earlier, later = beta, alpha
	}
	t.Logf("before the reorder the bar draws %q before %q; the order is %v", earlier.Name, later.Name, tabs)

	// The chord is proven ONCE, on the first of the two workspaces: it is the
	// same binding both times, and pressing it twice would prove nothing the
	// first proof did not.
	rt8ProvePriorityChord(ctx, t, client, driver, manifest, later)

	rt8SetPriority(ctx, t, client, later, rt8PriorityHigh)
	rt8SetPriority(ctx, t, client, earlier, rt8PriorityLow)

	// THE STORED PRIORITY IS CHECKED FIRST, and separately: "the verb did not
	// land" and "it landed and the bar did not follow" send a reader to two
	// different places, and one message naming both would name neither.
	var facts map[string]rt78Facts
	waitUntil(ctx, t, "the state database to record both priorities", wsActActCeiling, func() bool {
		read, readErr := rt78ReadFacts(ctx, dbPath)
		if readErr != nil {
			return false
		}
		facts = read
		return facts[later.ID].Priority != "" && facts[earlier.ID].Priority != ""
	})
	if facts == nil || facts[later.ID].Priority == "" || facts[earlier.ID].Priority == "" {
		t.Fatalf("`SPC j m p`'s command was entered for both workspaces and the state database records "+
			"priority %q for %q and %q for %q. An empty value is UNSET, so at least one set did not land, "+
			"and the refusal is in this run's harvest",
			facts[later.ID].Priority, later.Name, facts[earlier.ID].Priority, earlier.Name)
	}
	t.Logf("the state database records priority %q for %q and %q for %q",
		facts[later.ID].Priority, later.Name, facts[earlier.ID].Priority, earlier.Name)

	after := wsActWaitForTabs(ctx, t, "the drawn tab bar to follow the new priorities", client,
		func(tabs []string) bool {
			l, e := rt8IndexOf(tabs, later.Name), rt8IndexOf(tabs, earlier.Name)
			return l >= 0 && e >= 0 && l < e
		})
	laterAt, earlierAt := rt8IndexOf(after, later.Name), rt8IndexOf(after, earlier.Name)
	if laterAt < 0 || earlierAt < 0 {
		t.Fatalf("after the reorder the drawn tab bar no longer holds both of this run's workspaces (%q at "+
			"%d, %q at %d); the bar is %v", later.Name, laterAt, earlier.Name, earlierAt, after)
	}
	if laterAt >= earlierAt {
		t.Errorf("%q was given %s and %q was given %s, and the drawn tab bar still puts %q at slot %d and "+
			"%q at slot %d. The resolver orders the roster by priority rank ascending and the tab bar "+
			"follows it strictly, so the bar is where a priority change has to show. The bar is %v",
			later.Name, rt8PriorityHigh, earlier.Name, rt8PriorityLow,
			later.Name, laterAt, earlier.Name, earlierAt, after)
	} else {
		note := fmt.Sprintf("priority reordered the drawn bar: %q (%s) now precedes %q (%s), where it "+
			"followed it before", later.Name, rt8PriorityHigh, earlier.Name, rt8PriorityLow)
		manifest.Notes = append(manifest.Notes, note)
		t.Logf("%s; the order is %v", note, after)
	}

	return earlier, later
}

// rt8ProvePriorityChord presses `SPC j m p` as real key events and proves it
// reached `agent-repl-set-priority` through the command's own "Priority: "
// prompt, which no other command in the leader map raises.
//
// The editor is stood on the workspace first, because the command acts on the
// current one and a chord that opened the picker over somebody else's workspace
// would be proving the binding while pointing at the wrong subject.
func rt8ProvePriorityChord(ctx context.Context, t *testing.T, client *Client, driver *KeyDriver,
	manifest *Manifest, ws rt8Named) {
	t.Helper()
	rt8StandOn(ctx, t, client, ws)
	wsActProveChord(ctx, t, client, driver,
		[]Chord{wsActLeader, rt8ClaudePrefix, rt8ModifyPrefix, rt8PriorityKey}, "Priority:", manifest)
}

// rt8SetPriority gives one workspace a priority through `SPC j m p`'s command.
//
// The workspace is named by the command's own optional argument, which exists
// so a caller with a workspace in hand does not go through a picker, and only
// the priority `completing-read` is answered. The label is one the picker
// actually offers (`agent-repl-verbs-priority-levels`), because the read is a
// `require-match` and an invented candidate would be refused.
func rt8SetPriority(ctx context.Context, t *testing.T, client *Client, ws rt8Named, label string) {
	t.Helper()
	form := fmt.Sprintf(`(progn
  (require 'cl-lib)
  (cl-letf (((symbol-function 'completing-read)
             (lambda (prompt &rest _)
               (if (string-prefix-p "Priority:" prompt)
                   %q
                 (error "realtest: unexpected completing-read prompt %%S" prompt)))))
    (agent-repl-set-priority %q))
  t)`, label, ws.Name)
	if _, err := client.Read(ctx, form); err != nil {
		t.Fatalf("give the workspace %q priority %s through `SPC j m p`'s command: %v", ws.Name, label, err)
	}
	t.Logf("gave %q priority %s", ws.Name, label)
}

// ---- Close, reopen, kill --------------------------------------------------

// rt8AssertCloseKeepsItReopenable is the plan's second act: "close".
//
// Close is a VIEW act by contract: the session is untouched, the worktree is
// untouched, and only the editor state is torn down. So the three things
// asserted are the three that make that true, and the third is the one that
// turns "the tab went away" into "the workspace can be brought back", which are
// different claims.
func rt8AssertCloseKeepsItReopenable(ctx context.Context, t *testing.T, client *Client, dbPath string,
	driver *KeyDriver, manifest *Manifest, ws rt8Named) {
	t.Helper()
	rt8StandOn(ctx, t, client, ws)
	rt8PressAct(ctx, t, client, driver, manifest,
		[]Chord{wsActLeader, rt8ClaudePrefix, rt8CloseKey})

	afterClose := wsActWaitForDB(ctx, t, "the registry to mark the workspace closed", dbPath,
		func(all []Workspace) bool {
			facts, err := rt78ReadFacts(ctx, dbPath)
			return err == nil && facts[ws.ID].Closed
		})
	if _, still := wsActWorkspaceByID(afterClose, ws.ID); !still {
		t.Fatalf("closing %q removed its record from the state database entirely, so there is nothing left "+
			"to re-open. Close is a VIEW act; forgetting the record is what nuke does", ws.Name)
	}
	facts, err := rt78ReadFacts(ctx, dbPath)
	if err != nil {
		t.Fatalf("read the state database after the close: %v", err)
	}
	if !facts[ws.ID].Closed {
		t.Fatalf("`SPC j d` was pressed on %q and the state database still records closed=0 for %s. A close "+
			"that is not QUIET is refused rather than performed, with no turn in flight, no live work, no "+
			"held prompts and no queued merge, and the refusal is in this run's harvest", ws.Name, ws.ID)
	}

	tabs := wsActWaitForTabs(ctx, t, fmt.Sprintf("the tab bar to drop the tab for %q", ws.Name), client,
		func(tabs []string) bool { return !wsActHasTab(tabs, ws.Name) })
	if wsActHasTab(tabs, ws.Name) {
		t.Errorf("closed %q and the tab bar's own order still holds its tab: the order is %v", ws.Name, tabs)
	}

	rows, err := wsActRosterRows(ctx, client)
	if err != nil {
		t.Fatalf("read the roster after the close: %v", err)
	}
	row, present := wsActRowByID(rows, ws.ID)
	switch {
	case !present:
		t.Errorf("closed %q and Emacs's roster holds no row for it at all, so the re-open picker has nothing "+
			"to offer. A workspace that cannot be offered back has not been closed, it has been lost", ws.Name)
	case !row.Closed:
		t.Errorf("closed %q and its roster row still reads open", ws.Name)
	default:
		manifest.Notes = append(manifest.Notes,
			fmt.Sprintf("`SPC j d` closed %q: the tab is gone and the roster still offers it back", ws.Name))
		t.Logf("`SPC j d` closed %q: the tab is gone and the roster row is closed and re-openable", ws.Name)
	}
}

// rt8AssertReopenRestoresTheSameWorkspace is the plan's third act: "reopen".
//
// THE ID IS THE ASSERTION. `OpenWorkspace` takes the ref straight off the
// closed roster row and the daemon mints nothing on that path, and registration
// is idempotent by normalized directory with `workspaces.dir` unique, so a
// different id here would mean a second workspace standing where the first one
// was, with the first one's conversation stranded behind it. A second record
// naming the same directory is therefore a finding in its own right.
func rt8AssertReopenRestoresTheSameWorkspace(ctx context.Context, t *testing.T, client *Client, dbPath string,
	driver *KeyDriver, manifest *Manifest, ws rt8Named) {
	t.Helper()

	wsActProveChord(ctx, t, client, driver,
		[]Chord{wsActLeader, wsActTab, wsActOpenKey}, "Open workspace:", manifest)

	if err := wsActOpenWorkspace(ctx, client, ws.Name); err != nil {
		t.Fatalf("re-open the closed workspace %q through `SPC TAB O`'s command: %v", ws.Name, err)
	}

	afterReopen := wsActWaitForDB(ctx, t, "the registry to hold the workspace open again", dbPath,
		func(all []Workspace) bool {
			facts, err := rt78ReadFacts(ctx, dbPath)
			if err != nil {
				return false
			}
			row, has := facts[ws.ID]
			return has && !row.Closed
		})
	reopened, still := wsActWorkspaceByID(afterReopen, ws.ID)
	if !still {
		t.Fatalf("re-opening %q did not restore workspace %s: the state database no longer holds that record "+
			"at all", ws.Name, ws.ID)
	}
	facts, err := rt78ReadFacts(ctx, dbPath)
	if err != nil {
		t.Fatalf("read the state database after the re-open: %v", err)
	}
	if facts[ws.ID].Closed {
		t.Errorf("the re-open was sent and the state database still holds workspace %s (%q) as closed",
			ws.ID, ws.Name)
	}
	if reopened.ID != ws.ID {
		t.Errorf("the re-opened workspace has id %s and the closed one had %s: re-opening did not restore "+
			"the same workspace", reopened.ID, ws.ID)
	}
	if !wsActSameDir(reopened.Dir, ws.Dir) {
		t.Errorf("the re-opened workspace %s names directory %s, not the %s it was closed from",
			reopened.ID, reopened.Dir, ws.Dir)
	}
	duplicates := 0
	for _, other := range afterReopen {
		if wsActSameDir(other.Dir, ws.Dir) {
			duplicates++
		}
	}
	if duplicates != 1 {
		t.Errorf("%d workspace records name %s after the re-open, not one: re-opening minted a second "+
			"identity for a directory that already had one", duplicates, ws.Dir)
	}

	tabs := wsActWaitForTabs(ctx, t, fmt.Sprintf("the tab bar to draw a tab for %q again", ws.Name), client,
		func(tabs []string) bool { return wsActHasTab(tabs, ws.Name) })
	if !wsActHasTab(tabs, ws.Name) {
		t.Errorf("the workspace %s (%q) was re-opened and the tab bar's own order does not hold a tab for "+
			"it: the order is %v", ws.ID, ws.Name, tabs)
	} else {
		manifest.Notes = append(manifest.Notes,
			fmt.Sprintf("`SPC TAB O` restored %q under the same id %s, with its tab back", ws.Name, ws.ID))
		t.Logf("the tab bar draws %q again; the order is %v", ws.Name, tabs)
	}
}

// rt8AssertKillLeavesNoOrphan is the plan's fourth act: "kill".
//
// Kill is the big red button: forced session death that never blocks, destroys
// no data, and leaves the worktree, the branch and every durable record
// standing. So the assertions are about what ENDED, what SURVIVED, and the
// thing a forced teardown is most likely to get wrong.
func rt8AssertKillLeavesNoOrphan(ctx context.Context, t *testing.T, client *Client, dbPath, stateDir string,
	driver *KeyDriver, manifest *Manifest, ws rt8Named) {
	t.Helper()

	beforeFacts, err := rt78ReadFacts(ctx, dbPath)
	if err != nil {
		t.Fatalf("read the state database before the kill: %v", err)
	}
	before := beforeFacts[ws.ID]

	rt8StandOn(ctx, t, client, ws)
	rt8PressAct(ctx, t, client, driver, manifest,
		[]Chord{wsActLeader, rt8ClaudePrefix, rt8KillKey})

	// The daemon records the terminal and the closed flag in the same kill, and
	// the terminal is the one that says a KILL happened: the closed flag alone
	// is what a plain close leaves behind too.
	var session rt78Session
	afterKill := wsActWaitForDB(ctx, t, "the daemon to record the session as killed", dbPath,
		func(all []Workspace) bool {
			read, readErr := rt78ReadSession(ctx, dbPath, ws.ID)
			if readErr != nil {
				return false
			}
			session = read
			facts, factsErr := rt78ReadFacts(ctx, dbPath)
			return factsErr == nil && session.TerminalKind == "killed" && facts[ws.ID].Closed
		})

	facts, err := rt78ReadFacts(ctx, dbPath)
	if err != nil {
		t.Fatalf("read the state database after the kill: %v", err)
	}
	if session.TerminalKind != "killed" {
		t.Errorf("`SPC j x` was pressed on %q and the session's terminal kind is %q, not \"killed\". Kill "+
			"records the terminal before it records the close, and the terminal is what distinguishes a kill "+
			"from a plain close", ws.Name, session.TerminalKind)
	}
	if !facts[ws.ID].Closed {
		t.Errorf("killed %q (%s) and the state database still records closed=0. A killed workspace's row "+
			"carries closed=1, because Emacs derives its tab set from that flag and a killed workspace has "+
			"no editor state left", ws.Name, ws.ID)
	}
	if _, still := wsActWorkspaceByID(afterKill, ws.ID); !still {
		t.Errorf("killing %q removed its record from the registry. Kill destroys NO data, the worktree, the "+
			"branch and every durable record survive, and only Nuke takes a workspace out of the roster. "+
			"Its neighbour in the bindings, `SPC j X`, IS the nuke", ws.Name)
	}
	if facts[ws.ID].Branch != before.Branch {
		t.Errorf("killing %q changed its branch from %q to %q. A kill ends a session and touches neither the "+
			"branch nor the worktree", ws.Name, before.Branch, facts[ws.ID].Branch)
	}

	tabs := wsActWaitForTabs(ctx, t, fmt.Sprintf("the tab bar to drop the killed %q", ws.Name), client,
		func(tabs []string) bool { return !wsActHasTab(tabs, ws.Name) })
	if wsActHasTab(tabs, ws.Name) {
		t.Errorf("killed %q and the tab bar's own order still holds its tab: the order is %v", ws.Name, tabs)
	}

	// A KILLED WORKSPACE IS STILL REOPENABLE, by contract: the terminal a kill
	// writes is rehydratable, so the conversation can be resumed later. It is
	// checked because kill and nuke are one shift apart in the bindings, and a
	// kill that behaved like a nuke would pass every other assertion above.
	rows, err := wsActRosterRows(ctx, client)
	if err != nil {
		t.Errorf("read the roster after the kill: %v", err)
	} else if row, present := wsActRowByID(rows, ws.ID); !present {
		t.Errorf("killed %q and Emacs's roster holds no row for it, so it cannot be re-opened. Kill destroys "+
			"no data and only Nuke takes a workspace out of the roster", ws.Name)
	} else if !row.Closed {
		t.Errorf("killed %q and its roster row still reads open", ws.Name)
	}

	rt8AssertNoShimSurvives(ctx, t, stateDir, manifest, ws, session)
}

// rt8AssertNoShimSurvives is the no-orphan half of the kill assertion: the
// three independent questions this file's header lists.
func rt8AssertNoShimSurvives(ctx context.Context, t *testing.T, stateDir string, manifest *Manifest,
	ws rt8Named, session rt78Session) {
	t.Helper()

	if session.ShimPID != "" && rt78ProcessAlive(ctx, session.ShimPID) {
		t.Errorf("killed %q (%s) and the shim pid the daemon recorded for it, %s, is still running. A forced "+
			"kill signals the process group and waits for the reap; a survivor holds the workspace lock and "+
			"the next session on this workspace is refused", ws.Name, ws.ID, session.ShimPID)
	}

	shims, err := rt78ShimsFor(ctx, stateDir, ws.ID)
	if err != nil {
		t.Errorf("enumerate the shim processes listening for %q after the kill: %v", ws.Name, err)
	} else if len(shims) > 0 {
		t.Errorf("killed %q (%s) and %d shim process(es) are still listening on a socket belonging to it:\n"+
			"  %s\nThe match is on `<state dir>/sock/%s.`, so it covers the relaunched generations "+
			"(`<id>.nN.sock`) an exact-path check would miss",
			ws.Name, ws.ID, len(shims), strings.Join(shims, "\n  "), ws.ID)
	}

	sockets, err := rt78WorkspaceSockets(stateDir, ws.ID)
	if err != nil {
		t.Errorf("enumerate the socket nodes belonging to %q after the kill: %v", ws.Name, err)
		return
	}
	live := 0
	for _, socket := range sockets {
		if rt78SocketAnswers(socket) {
			live++
			t.Errorf("killed %q (%s) and a connect to %s is still ANSWERED, so something is listening there. "+
				"A stale socket node left on disk is expected, since nothing unlinks it until the next spawn "+
				"or the next boot, but a node that ANSWERS is a live shim", ws.Name, ws.ID, socket)
		}
	}
	if live == 0 {
		note := fmt.Sprintf("`SPC j x` killed %q with no orphan: the recorded shim pid is gone, no process "+
			"listens for it, and none of its %d socket node(s) answers", ws.Name, len(sockets))
		manifest.Notes = append(manifest.Notes, note)
		t.Logf("%s", note)
	}
}

// ---- Small acts and readings ---------------------------------------------

// rt8PressAct presses a leader sequence whose command asks NOTHING, so the
// keypress IS the act.
//
// The substrate's wsActProveChord cannot be used here: it proves a chord by
// waiting for its command's first minibuffer prompt, and `SPC j d` and
// `SPC j x` have none. Two things are asserted instead. Emacs's own
// `(recent-keys)` must contain the sequence, which is its account of its INPUT
// and the only thing separating "the chord arrived" from "something called the
// command". And no minibuffer may be standing afterwards, because one that is
// means the sequence resolved to something else that asked a question, and
// leaving it up would let it swallow the next act's keys.
//
// A failure here is FATAL, unlike the substrate's chord proof, and for the
// opposite reason: there the chord and the act were separate claims and the act
// still ran through the command. Here the chord IS the act, so a sequence that
// did not arrive means nothing was done and every assertion after it would be
// asserting the wrong thing.
func rt8PressAct(ctx context.Context, t *testing.T, client *Client, driver *KeyDriver,
	manifest *Manifest, sequence []Chord) {
	t.Helper()
	spelled, recorded := wsActSpell(sequence), SpellRecorded(sequence)
	if driver == nil {
		t.Fatalf("`%s` could not be pressed because the key driver is unavailable, and this act IS the "+
			"keypress: the command it runs asks nothing, so there is nothing to enter instead. "+
			"docs/REALTEST-PLAN.md rules that there is no elisp fallback for a key", spelled)
	}
	for _, chord := range append([]Chord{wsActEscape}, sequence...) {
		if err := driver.Press(ctx, chord); err != nil {
			t.Fatalf("pressing %s of `%s` (%s) failed, and this act IS the keypress: %v",
				chord.Emacs, spelled, chord.Why, err)
		}
	}

	var keys string
	waitUntil(ctx, t, fmt.Sprintf("emacs to report `%s` in its own recent keys", spelled), wsActChordCeiling,
		func() bool {
			read, err := RecentKeys(ctx, client)
			if err != nil {
				return false
			}
			keys = read
			return strings.Contains(keys, recorded)
		})
	if !strings.Contains(keys, recorded) {
		where, _ := client.ReadString(ctx, `(format "buffer=%s evil-state=%s major-mode=%s"
        (buffer-name) (or (bound-and-true-p evil-state) "none") major-mode)`)
		reading, foreign := readChordRing(keys, wsActEscape.recorded(), recorded)
		t.Fatalf("pressed `%s` and Emacs's own (recent-keys) does not contain %q, so the sequence never "+
			"reached its keymap and the act did not happen. recent-keys ends with: %s. It was pressed at %s. %s",
			spelled, recorded, tail(keys, 120), where,
			wsActRingNote(reading, foreign, spelled, wsActEscape.Emacs))
	}

	prompt, err := wsActMinibufferPrompt(ctx, client)
	if err != nil {
		t.Errorf("read whether `%s` left a minibuffer standing: %v", spelled, err)
	} else if prompt != "" {
		t.Errorf("`%s` runs a command that asks nothing, and it left the minibuffer prompting %q. Either the "+
			"sequence resolved to a different command or the binding grew a question, and either way the "+
			"prompt would swallow the next act's keys", spelled, prompt)
	}

	note := fmt.Sprintf("real key events `%s` reached Emacs's keymap and the command behind them asks "+
		"nothing, so the keypress is the whole act", spelled)
	manifest.Notes = append(manifest.Notes, note)
	t.Logf("%s", note)
}

// rt8StandOn selects a workspace, and it is a DEVIATION stated at its own site.
//
// Every act in this realtest runs against the CURRENT workspace, so each has to
// be preceded by a selection, and selection by chord (`s-{`, `s-}`, `M-<n>`) is
// realtest 4's subject rather than this one's. Worse, a numeral names a SLOT OF
// THE WHOLE DRAWN BAR, so driving the selection that way would make this test's
// outcome depend on where in the owner's registry the run's workspaces landed.
// The switch enters `agent-repl-switch-to-project`, the module's own command,
// through its own project-root argument, and the run asserts afterwards that
// the editor really is standing where it asked.
func rt8StandOn(ctx context.Context, t *testing.T, client *Client, ws rt8Named) {
	t.Helper()
	if _, err := client.Read(ctx, fmt.Sprintf(
		`(progn (agent-repl-switch-to-project %q) t)`, ws.Dir)); err != nil {
		t.Fatalf("stand on the workspace %q at %s: %v", ws.Name, ws.Dir, err)
	}

	var current string
	waitUntil(ctx, t, "the editor to be standing on "+ws.Name, wsActActCeiling, func() bool {
		read, err := client.ReadString(ctx, `(or (agent-repl--ws-current-name) "")`)
		if err != nil {
			return false
		}
		current = read
		return current == ws.Name
	})
	if current != ws.Name {
		t.Fatalf("asked the editor to stand on %q and it is standing on %q, so the next act would be "+
			"performed on the wrong workspace", ws.Name, current)
	}
}

// rt8TabName is the name the tab bar draws a workspace under.
//
// The roster's own row name is preferred over the state database's, because the
// bar is drawn from the roster and the roster disambiguates names that collide
// across repositories.
func rt8TabName(ctx context.Context, t *testing.T, client *Client, ws Workspace) string {
	t.Helper()
	rows, err := wsActRosterRows(ctx, client)
	if err != nil {
		t.Fatalf("read the roster to name workspace %s: %v", ws.ID, err)
	}
	if row, ok := wsActRowByID(rows, ws.ID); ok && row.Name != "" {
		return row.Name
	}
	return ws.Name
}

// rt8Sources enumerates the log sources for a workspace set.
func rt8Sources(t *testing.T, home string, workspaces []Workspace) []Source {
	t.Helper()
	sources, err := EnumerateSources(RealEnv(home, workspaces))
	if err != nil {
		t.Fatalf("enumerate the logs: %v", err)
	}
	return sources
}

// rt8IndexOf is a name's slot in the drawn tab order, or -1.
func rt8IndexOf(tabs []string, name string) int {
	for i, tab := range tabs {
		if tab == name {
			return i
		}
	}
	return -1
}

// rt8Describe renders a workspace list for a failure message.
func rt8Describe(list []Workspace) string {
	if len(list) == 0 {
		return "(none)"
	}
	parts := make([]string, 0, len(list))
	for _, ws := range list {
		parts = append(parts, fmt.Sprintf("%s (%s at %s)", ws.ID, ws.Name, ws.Dir))
	}
	return strings.Join(parts, "; ")
}
