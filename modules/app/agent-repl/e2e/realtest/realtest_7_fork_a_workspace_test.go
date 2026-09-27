//go:build realtest

package realtest

import (
	"context"
	"fmt"
	"os"
	"path/filepath"
	"strconv"
	"strings"
	"testing"
	"time"
)

// REALTEST 7: FORK A WORKSPACE WITH ITS CONVERSATION.
//
// docs/REALTEST-PLAN.md, "Workspaces", item 7: "Fork a workspace with its
// conversation (`SPC TAB f`). The fork carries the conversation."
//
// What it asserts, in order:
//
//   - `SPC TAB f` REACHES ITS COMMAND. The chord is pressed as real key events
//     and the fork command's own first prompt, "Initial prompt: ", is read back
//     out of the minibuffer, then aborted with a real `C-g`. Since the
//     2026-09-12 creation ruling the fork is a DYNAMIC mode and asks for the
//     prompt alone, which `SPC TAB n` and `SPC TAB c` also do, so that prompt
//     is not evidence on its own here the way it is in realtest 6: Emacs's own
//     `(recent-keys)` is asserted to contain the sequence as well, and the two
//     together say which command the keymap resolved.
//   - THE FORK IS A NEW WORKSPACE: its own daemon-minted id, its own row in
//     `workspaces`, its own worktree, its own branch, its own tab.
//   - THE FORK CARRIES THE PARENT'S CONVERSATION, asserted on the daemon's own
//     durable expression of that fact and not on a guess. See below.
//   - THE PARENT IS UNCHANGED: same directory, same branch, same closed flag,
//     same conversation, same session, and it gained no inherited rows of its
//     own.
//
// WHERE A FORK'S INHERITED CONVERSATION ACTUALLY LIVES
//
// The daemon ports a fork's conversation in two halves, and only one of them is
// readable from outside:
//
//   - The VENDOR TRANSCRIPT half. `PortTranscript`
//     (daemon/internal/account/transcript.go) copies the parent's
//     `<vendor id>.jsonl` into the child's config root under a FRESH vendor
//     session id, re-minting every identity in it through one `remint.Mapper`.
//     That is why the child's `sessions.vendor_session_id` is its own and never
//     the parent's, which this test checks as far as it can see it.
//   - The DAEMON'S OWN PROMPT ROWS. The vendor transcript records what the
//     AGENT said; what the PERSON asked is a row the daemon draws, so porting
//     only the transcript left a forked feed showing an answer to a question it
//     did not show. `forkconversation.go` therefore reads the parent's whole
//     `ConversationPrompts` (its own inherited rows PLUS its `turns`, so a fork
//     of a fork keeps the grandparent's questions), re-mints every turn id
//     under the same mapping the transcript was ported with, and writes the
//     result to `ported_prompts` in one all-or-nothing transaction.
//
// `ported_prompts` is what this test asserts on, read read-only out of
// `~/.claude-emacs/wsm.db`. It is the right evidence for three reasons: a
// workspace has rows there if and only if it was forked, because the fork path
// is the only writer; the rows carry the parent's prompt TEXT verbatim, so
// "carries the conversation" is a comparison and not an inference; and their
// turn ids are the child's own re-minted ones, so the test can also show the
// port was a port and not a shared reference.
//
// What this test deliberately does NOT rest the verdict on:
//
//   - `workspaces.parent_id`. It is checked, but it cannot decide anything: the
//     table has no fork column, and a plain child sets `parent_id` identically
//     because the daemon's CreateSpec makes ForkFrom imply Parent. Parentage is
//     necessary and not sufficient.
//   - EMACS STATE. Emacs holds no feed and no conversation history; the feed
//     lives in the webapp behind `WatchFeed` and Emacs subscribes only to the
//     roster. The `:fork-session-id` key in lisp/workspace.el is vestigial,
//     since nothing in production writes it, so asserting on it would assert
//     nothing.
//
// A FORKED WORKSPACE IS NOT A VENDOR `fork` SUBAGENT
//
// Two different things share the word, and a recent fix concerns the other one.
// The vendor's own `fork` SUBAGENT type copies its caller's messages verbatim
// into the head of its sidechain transcript, keeping the original message ids
// and the producing agent's attribution; the sidecar now recognizes those
// records (`attributionAgent != AgentType`) and books them as QUOTED CONTEXT
// (residue: `kind = 'vendor_specific'`, no `book_agent_id`) rather than
// re-booking rows that already belong to another agent's book.
//
// A forked WORKSPACE is the opposite in every respect that matters here: the
// DAEMON does the copying, every identity is re-minted rather than preserved,
// the copied records are first-class rows in the child's own book, the fact is
// recorded durably in `ported_prompts`, and the parent's whole conversation IS
// drawn on the fork's feed. The two paths never meet: the sidecar's guard
// requires a non-empty agent type, which only a subagent transcript with a
// companion meta file has, and a workspace fork's ported artifact is a session
// transcript with neither. Nothing here asserts on quoted-context residue, and
// a finding about it in this run's harvest would be about the other mechanism.
//
// A DEDICATED SCRATCH REPOSITORY (lead's standing decision, 2026-09-12), the
// same one realtests 5 and 6 use and through the same substrate: the directory
// this test registers and forks inside is one it creates under the run
// directory, never one of the owner's, and everything it creates it destroys
// through `t.Cleanup`, so a failure halfway still tears down. Real git runs,
// because forking a workspace makes the daemon cut a real branch and
// materialize a real worktree; that is inherent to driving the real product and
// is a different thing from the no-real-git rule governing the unit and
// integration suites. What the product has no verb to remove is stated rather
// than hidden, in wsActCleanupRegistered.
//
// THE DEVIATIONS FROM "REAL KEYS", each with its reason. The substrate's "The
// acts" commentary carries the general case; two are specific to this test:
//
//   - The fork act itself. The chord is real and proven; the minibuffer answers
//     are supplied through the probe transport, entering the SAME user-facing
//     command with `call-interactively`, exactly as realtest 5 enters the
//     create command. `agent-repl-fork-workspace` asks a `require-match`
//     `completing-read` for the repository and then a `read-string` for the
//     initial prompt, and typing into the first through the completion UI would
//     test vertico's candidate ordering rather than the product.
//   - SEEDING THE PARENT'S CONVERSATION, which is weaker than the substrate's
//     rule and is called out for that reason. The daemon refuses a fork whose
//     parent has no conversation, so the parent must be given a prompt before
//     the act under test can do anything at all. The plan names no keybinding
//     for that here, since sending a prompt is realtest 9's whole subject, and
//     the send commands read the composer rather than take an argument, so the
//     prompt goes in through `agent-repl--send`, the one body every production
//     send site shares. That is a rung below a command, and it is a
//     PRECONDITION rather than the act under test: what realtest 7 asserts is
//     what the FORK did with a conversation that already existed.
//
// THE VENDOR GUARD ALONE IS ENOUGH TO SEED THE PARENT. Seeding means submitting
// a prompt, which used to demand AGENT_REPL_FAKE_SHIMS=1 on the launch beside
// the guard, because a guarded daemon refused the shim spawn and a real-vendor
// shim throws at `createRealQuery`. A guarded daemon now spawns every shim with
// `--fake` instead, so the one variable the launcher states covers it;
// rt78_shared.go carries the whole account.
//
// ALSO A PRECONDITION FOR THE LEAD: run this realtest in its own
// `bin/realtest.sh -run` invocation. It performs a cold start, and a cold start
// refuses to run against an Emacs that is already answering.
//
// THE REMEDIATION BAR IS THE LOG HARVEST, the same one realtest 1 carries:
// every WARN and ERROR written inside the run window across every log, with no
// allowlist, into the run's MANIFEST.md, and the test fails when the count is
// non-zero.

// rt7ForkKey is the `f` of `SPC TAB f`, bound to `agent-repl-fork-workspace`
// (lisp/keybindings.el). Defined here rather than in keys.go because parallel
// authors are working in that file.
var rt7ForkKey = Chord{
	Emacs:     "f",
	Keycode:   3,
	Modifiers: nil,
	Why:       "completes `SPC TAB f`, which opens the fork command's initial-prompt read",
}

// rt7SeedPrompt is what the parent is asked, and it is deliberately
// distinctive: the fork assertion is a comparison of prompt TEXT between two
// workspaces, and a sentence that could plausibly appear in the owner's own
// transcripts would make a false match possible.
const rt7SeedPrompt = "realtest 7 seed prompt: this sentence exists only to be inherited by a fork"

func TestRealtestForkAWorkspace(t *testing.T) {
	if os.Getenv(runGateEnv) != "1" {
		t.Skipf("realtest 7 drives the owner's real editor and runs only through bin/realtest.sh, which sets %s=1", runGateEnv)
	}
	ctx := context.Background()
	measureOnly := os.Getenv(measureEnv) == "1"
	requireMeasuredBudgets(t, measureOnly)

	home, err := os.UserHomeDir()
	if err != nil {
		t.Fatalf("resolve the owner's home directory: %v", err)
	}
	runDir := startupRunDir(t, home, "realtest-7")
	t.Logf("realtest 7 run directory: %s", runDir)

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
		Title:      "Realtest 7: fork a workspace with its conversation",
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

	// ---- The parent: a registered scratch repository with a conversation

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
	parent, ok := wsActWorkspaceByDir(afterRegister, scratch)
	if !ok {
		t.Fatalf("registering %s minted no workspace, so there is nothing to fork. The records that appeared "+
			"are: %s", scratch, rt7Describe(wsActNewSince(allBefore, afterRegister)))
	}
	parentName := rt7TabName(ctx, t, client, parent)
	t.Cleanup(func() { wsActCleanupRegistered(ctx, t, client, dbPath, parent, parentName) })
	t.Logf("the parent is workspace %s (%q) at %s", parent.ID, parentName, parent.Dir)

	wsActWaitForTabs(ctx, t, fmt.Sprintf("the tab bar to draw the parent %q", parentName), client,
		func(tabs []string) bool { return wsActHasTab(tabs, parentName) })

	rt7SeedConversation(ctx, t, client, dbPath, parent, parentName)

	before := rt7Snapshot(ctx, t, dbPath, parent.ID, "the parent, before the fork")
	if len(before.Conversation) == 0 {
		t.Fatalf("the parent %s (%q) has no conversation after one was seeded, so the fork would be refused "+
			"outright (`fork_parent_has_no_conversation`) and there would be nothing for it to carry",
			parent.ID, parentName)
	}
	t.Logf("the parent's conversation is %d prompt(s) before the fork", len(before.Conversation))

	// ---- The act: `SPC TAB f` -----------------------------------------

	rt7ProveForkChord(ctx, t, client, driver, &manifest)

	// The fork is a dynamic mode: it takes its repository from the workspace
	// the editor is standing on, which rt7SeedConversation has already asserted
	// is the parent. This states the same fact about the REPOSITORY, through
	// the product's own derivation, so a fork that landed somewhere this run
	// may not write is refused before it happens rather than found afterwards.
	wsActRequireDynamicRepository(ctx, t, client, scratch)
	forkStarted := time.Now()
	if err := rt7ForkWorkspace(ctx, client); err != nil {
		t.Fatalf("fork the parent through `SPC TAB f`'s command: %v", err)
	}

	afterFork := wsActWaitForDB(ctx, t, "the registry to hold a workspace forked from the parent", dbPath,
		func(all []Workspace) bool {
			_, found := rt7ChildOf(ctx, dbPath, all, parent.ID)
			return found
		})
	fork, found := rt7ChildOf(ctx, dbPath, afterFork, parent.ID)
	if !found {
		t.Fatalf("`SPC TAB f` was entered against the parent %s and the state database holds no workspace "+
			"whose `parent_id` is that parent. Either the fork was refused, in which case the refusal is in "+
			"this run's harvest, or it created nothing. The records that appeared are: %s",
			parent.ID, rt7Describe(wsActNewSince(allBefore, afterFork)))
	}
	forkName := rt7TabName(ctx, t, client, fork)
	t.Cleanup(func() { wsActCleanupCreated(ctx, t, client, dbPath, fork, forkName) })
	t.Logf("`SPC TAB f` produced workspace %s (%q) at %s, %s after the command was entered",
		fork.ID, forkName, fork.Dir, time.Since(forkStarted).Round(time.Millisecond))

	// The fork's own log sinks are captured while its worktree still stands: a
	// nuke deletes the LINK, not the bytes, and the harvester reads through the
	// link (wsActPreserveSinks carries the whole reasoning).
	preserved := wsActPreserveSinks(fork)

	// ---- The assertions -----------------------------------------------

	rt7AssertForkIsANewWorkspace(ctx, t, client, dbPath, parent, parentName, fork, forkName)
	rt7AssertForkCarriesTheConversation(ctx, t, dbPath, parent, fork, before)
	rt7AssertParentIsUnchanged(ctx, t, dbPath, parent, parentName, before)

	forkOpened, err := wsActScan(
		wsActMergeSources(rt7Sources(t, home, afterFork), preserved), snapshot, forkStarted, wsActTabOpenRe)
	if err != nil {
		t.Fatalf("read the log for the fork's tab-open record: %v", err)
	}
	if hit, has := wsActHitNaming(forkOpened, fork.ID); has {
		t.Logf("the log records the fork's tab opening: %s", hit.Message)
	} else if hit, has := wsActHitNaming(forkOpened, forkName); has {
		t.Logf("the log records the fork's tab opening: %s", hit.Message)
	} else {
		t.Errorf("no `elisp.roster.tab-open:` record names the fork %s (%q), so nothing in the log says its "+
			"tab was drawn", fork.ID, forkName)
	}

	// ---- The harvest ---------------------------------------------------

	manifest.Ended = time.Now()
	finalAll, _, _, err := wsActWorkspacesNow(ctx, dbPath)
	if err != nil {
		t.Fatalf("read the state database before the harvest: %v", err)
	}
	finalSources := wsActMergeSources(rt7Sources(t, home, finalAll), preserved)
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

// ---- The act -----------------------------------------------------------

// rt7ProveForkChord presses `SPC TAB f` as real key events and proves it
// reached `agent-repl-fork-workspace`.
//
// The substrate's proof is the command's own first prompt, which is enough
// where no other command asks it first. Here it is not: every dynamic mode
// opens with "Initial prompt: " since the 2026-09-12 ruling, so the prompt
// alone says only that ONE of them ran. Emacs's own `(recent-keys)` is
// therefore asserted as well, because it is the only thing that separates "the
// `f` arrived" from "the `n` did", and the two facts together identify the
// command.
//
// Like the substrate's proof, a failure here does not stop the run: the chord
// and the verb are separate claims, and a run that stopped would say nothing
// about whether forking works at all.
func rt7ProveForkChord(ctx context.Context, t *testing.T, client *Client, driver *KeyDriver, manifest *Manifest) {
	t.Helper()
	sequence := []Chord{wsActLeader, wsActTab, rt7ForkKey}
	if !wsActProveChord(ctx, t, client, driver, sequence, "Initial prompt:", manifest) {
		return
	}
	keys, err := RecentKeys(ctx, client)
	if err != nil {
		t.Errorf("read Emacs's own (recent-keys) to tell `SPC TAB f` from `SPC TAB n`: %v", err)
		return
	}
	spelled, recorded := wsActSpell(sequence), SpellRecorded(sequence)
	if !strings.Contains(keys, recorded) {
		note := fmt.Sprintf("`%s` put the fork command's prompt up, but Emacs's own (recent-keys) does not "+
			"contain %q, so the prompt cannot be credited to this chord: `SPC TAB n` and `SPC TAB c` ask the same first "+
			"question. recent-keys ends with: %s", spelled, recorded, tail(keys, 120))
		manifest.Notes = append(manifest.Notes, note)
		t.Errorf("%s", note)
		return
	}
	note := fmt.Sprintf("`%s` is confirmed as the FORK command and not the create command: Emacs's own "+
		"(recent-keys) contains %q and the prompt it raised was the fork command's", spelled, recorded)
	manifest.Notes = append(manifest.Notes, note)
	t.Logf("%s", note)
}

// rt7ForkWorkspace forks the current workspace through `SPC TAB f`'s command.
//
// `agent-repl-fork-workspace` reads ONE thing and nothing else since the
// 2026-09-12 creation ruling: a `read-string` for the initial prompt, answered
// BLANK. The repository is no longer asked for — the fork takes the one the
// current workspace sits in — and the daemon mints the name.
//
// The blank prompt is a choice about what this realtest is for, the same one
// realtest 5 makes for the create: the fork's own first turn is not the
// subject, the conversation it INHERITED is, and a prompt here would put a
// second conversation under test. The command reads a blank as ABSENCE and
// omits it from the request entirely, which is exactly what a user leaving it
// blank produces.
//
// The parent is not a parameter. The command reads it from the workspace the
// editor is standing on, and registering a directory lands on the workspace it
// minted (`agent-repl-verbs-select-minted`), so the parent is the scratch
// repository's workspace. That is asserted before the act rather than assumed
// (rt7SeedConversation): a fork of the wrong parent would be a wrong answer
// about the product produced entirely by a wrong assumption about where the
// editor was standing.
func rt7ForkWorkspace(ctx context.Context, client *Client) error {
	form := `(progn
  (require 'cl-lib)
  (cl-letf (((symbol-function 'read-string)
             (lambda (prompt &rest _)
               (if (string-prefix-p "Initial prompt:" prompt)
                   ""
                 (error "realtest: unexpected read-string prompt %S" prompt))))
            ((symbol-function 'completing-read)
             (lambda (prompt &rest _)
               (error "realtest: the fork asked a completing-read it should not ask: %S" prompt))))
    (call-interactively #'agent-repl-fork-workspace))
  t)`
	_, err := client.Read(ctx, form)
	return err
}

// rt7SeedConversation gives the parent the one prompt the fork will inherit,
// and waits for the DAEMON'S OWN RECORD of it.
//
// It waits on a row in `turns` carrying the text rather than on the send
// returning. The send is asynchronous and its acknowledgement says the request
// was accepted, not that a conversation exists; the fork's refusal arm is
// decided by what the daemon has STORED, so that is what has to be true before
// the fork is attempted.
//
// The editor is asserted to be standing on the parent first. Two things depend
// on it: the prompt is addressed by workspace name, and `SPC TAB f` reads its
// parent from the current workspace.
func rt7SeedConversation(ctx context.Context, t *testing.T, client *Client, dbPath string,
	parent Workspace, parentName string) {
	t.Helper()

	current, err := client.ReadString(ctx, `(or (agent-repl--ws-current-name) "")`)
	if err != nil {
		t.Fatalf("read which workspace the editor is standing on before the parent is seeded: %v", err)
	}
	if current != parentName {
		t.Fatalf("the editor is standing on %q, not on the registered workspace %q, so `SPC TAB f` would "+
			"fork the wrong parent. Registering a directory is supposed to land on the workspace it minted "+
			"(agent-repl-verbs-select-minted)", current, parentName)
	}

	if _, err := client.Read(ctx, fmt.Sprintf(
		`(progn (agent-repl--send :user-sent %q %q) t)`, rt7SeedPrompt, parentName)); err != nil {
		t.Fatalf("send the parent %q the prompt that gives it a conversation to fork: %v", parentName, err)
	}

	var turns []rt78Prompt
	waitUntil(ctx, t, "the daemon to record the parent's prompt as a turn", wsActActCeiling, func() bool {
		read, readErr := rt78Turns(ctx, dbPath, parent.ID)
		if readErr != nil {
			return false
		}
		turns = read
		for _, turn := range turns {
			if strings.Contains(turn.Text, rt7SeedPrompt) {
				return true
			}
		}
		return false
	})
	for _, turn := range turns {
		if strings.Contains(turn.Text, rt7SeedPrompt) {
			t.Logf("the parent's conversation is seeded: turn %s carries the prompt", turn.Turn)
			return
		}
	}
	t.Fatalf("sent the parent %q a prompt and the daemon recorded no turn carrying it; `turns` holds %d "+
		"row(s). Without a conversation the fork is refused outright "+
		"(fork_parent_has_no_conversation)", parentName, len(turns))
}

// ---- The assertions -----------------------------------------------------

// rt7State is everything one workspace's own tables say about it at one moment.
type rt7State struct {
	Facts rt78Facts
	Row   Workspace
	// Conversation is what a fork of this workspace would inherit: its
	// inherited rows followed by its own turns.
	Conversation []rt78Prompt
	// Ported is only the inherited half, so a test can say whether this
	// workspace was itself forked.
	Ported  []rt78Prompt
	Turns   []rt78Prompt
	Session rt78Session
}

// rt7Snapshot reads one workspace's whole state out of wsm.db.
func rt7Snapshot(ctx context.Context, t *testing.T, dbPath, id, what string) rt7State {
	t.Helper()
	facts, err := rt78ReadFacts(ctx, dbPath)
	if err != nil {
		t.Fatalf("read %s: %v", what, err)
	}
	row, ok := facts[id]
	if !ok {
		t.Fatalf("read %s: the state database holds no `workspaces` row for %s", what, id)
	}
	all, _, _, err := wsActWorkspacesNow(ctx, dbPath)
	if err != nil {
		t.Fatalf("read %s: %v", what, err)
	}
	record, _ := wsActWorkspaceByID(all, id)
	ported, err := rt78PortedPrompts(ctx, dbPath, id)
	if err != nil {
		t.Fatalf("read %s: %v", what, err)
	}
	turns, err := rt78Turns(ctx, dbPath, id)
	if err != nil {
		t.Fatalf("read %s: %v", what, err)
	}
	conversation, err := rt78Conversation(ctx, dbPath, id)
	if err != nil {
		t.Fatalf("read %s: %v", what, err)
	}
	session, err := rt78ReadSession(ctx, dbPath, id)
	if err != nil {
		t.Fatalf("read %s: %v", what, err)
	}
	return rt7State{
		Facts: row, Row: record, Conversation: conversation,
		Ported: ported, Turns: turns, Session: session,
	}
}

// rt7AssertForkIsANewWorkspace is the plan's first half: a fork is a workspace
// in its own right, with its own identity, worktree, branch and tab.
func rt7AssertForkIsANewWorkspace(ctx context.Context, t *testing.T, client *Client, dbPath string,
	parent Workspace, parentName string, fork Workspace, forkName string) {
	t.Helper()

	if fork.ID == parent.ID {
		t.Errorf("the fork and its parent are the same workspace id %s, so nothing was created", fork.ID)
	}
	if fork.Dir == "" || wsActSameDir(fork.Dir, parent.Dir) {
		t.Errorf("the fork %s sits at %q, which is the parent's own directory: a fork gets its own worktree",
			fork.ID, fork.Dir)
	}

	facts, err := rt78ReadFacts(ctx, dbPath)
	if err != nil {
		t.Fatalf("read the fork's row: %v", err)
	}
	if facts[fork.ID].ParentID != parent.ID {
		t.Errorf("the fork %s records `parent_id` %q, not the parent %s it was forked from",
			fork.ID, facts[fork.ID].ParentID, parent.ID)
	}
	if facts[fork.ID].Closed {
		t.Errorf("the fork %s (%q) was created closed, so it has no tab and the user cannot work in it",
			fork.ID, forkName)
	}
	if facts[fork.ID].Branch == "" || facts[fork.ID].Branch == facts[parent.ID].Branch {
		t.Errorf("the fork %s is on branch %q and the parent is on %q: a fork gets its own branch",
			fork.ID, facts[fork.ID].Branch, facts[parent.ID].Branch)
	}

	tabs := wsActWaitForTabs(ctx, t, fmt.Sprintf("the tab bar to draw the fork %q", forkName), client,
		func(tabs []string) bool { return wsActHasTab(tabs, forkName) })
	if !wsActHasTab(tabs, forkName) {
		t.Errorf("the fork %s (%q) has no tab: the drawn order is %v", fork.ID, forkName, tabs)
	}
	if !wsActHasTab(tabs, parentName) {
		t.Errorf("the parent %q lost its tab when it was forked; the drawn order is %v", parentName, tabs)
	}
	t.Logf("the fork has its own tab; the drawn order is %v", tabs)
}

// rt7AssertForkCarriesTheConversation is the plan's second half, and the one
// the item exists for: "The fork carries the conversation."
//
// The comparison is against the parent's conversation AS IT WAS BEFORE THE
// FORK, not as it is now. Reading the parent again afterwards would compare the
// fork to a moving target, since the parent is a live workspace with a session
// up, and a race there would read as an inheritance failure.
func rt7AssertForkCarriesTheConversation(ctx context.Context, t *testing.T, dbPath string,
	parent Workspace, fork Workspace, before rt7State) {
	t.Helper()

	// The port is one transaction on the create path, but the create is
	// answered asynchronously, so the rows are waited for rather than read the
	// instant the tab appears.
	var ported []rt78Prompt
	waitUntil(ctx, t, "the daemon to write the fork's inherited conversation", wsActActCeiling, func() bool {
		read, err := rt78PortedPrompts(ctx, dbPath, fork.ID)
		if err != nil {
			return false
		}
		ported = read
		return len(ported) >= len(before.Conversation)
	})

	if len(ported) == 0 {
		t.Fatalf("the fork %s has NO rows in `ported_prompts`, so it carries none of its parent's "+
			"conversation. That table is written from exactly one place, the daemon's fork path "+
			"(daemon/internal/workspace/forkconversation.go), in one all-or-nothing transaction, so an "+
			"empty table means the port did not happen at all rather than that it happened partially",
			fork.ID)
	}

	wantTexts := rt78PromptTexts(before.Conversation)
	gotTexts := rt78PromptTexts(ported)
	if len(gotTexts) != len(wantTexts) {
		t.Errorf("the fork %s inherited %d prompt(s) and the parent's conversation was %d at the moment it "+
			"was forked. A fork ports the WHOLE conversation, and a partial port is the defect the "+
			"single-transaction write exists to prevent.\n  parent: %q\n  fork:   %q",
			fork.ID, len(gotTexts), len(wantTexts), wantTexts, gotTexts)
	}
	for i := range wantTexts {
		if i >= len(gotTexts) {
			break
		}
		if gotTexts[i] != wantTexts[i] {
			t.Errorf("the fork's inherited prompt %d reads %q, and the parent's prompt %d was %q",
				i, gotTexts[i], i, wantTexts[i])
		}
	}

	// THE SEED PROMPT IS NAMED EXPLICITLY, beside the whole-conversation
	// comparison, because the two fail differently: a length or ordering
	// mismatch says the port went wrong, and this says the port did not carry
	// the one sentence this run put into the parent on purpose.
	if !rt7AnyContains(gotTexts, rt7SeedPrompt) {
		t.Errorf("the fork %s inherited %d prompt(s), none containing the prompt this run sent the parent "+
			"(%q). Whatever it carried, it is not this parent's conversation",
			fork.ID, len(gotTexts), rt7SeedPrompt)
	}

	// The ordinals are the ordering key both the port and the replay read, and
	// the port re-numbers them from zero so the child's copy is a whole
	// conversation in its own right. A gap or a repeat is a feed that draws the
	// parent's questions in the wrong order or twice.
	for i, row := range ported {
		want := strconv.Itoa(i)
		if row.Ordinal != want {
			t.Errorf("the fork's inherited row %d carries ordinal %q, not %q: the port re-numbers a child's "+
				"conversation contiguously from zero", i, row.Ordinal, want)
		}
	}

	// A PORT, NOT A SHARED REFERENCE. Every ported turn id is re-minted under
	// the same mapping the transcript was ported with, so the child's rows name
	// the child's own turns. A ported row still carrying one of the parent's
	// live turn ids would mean the two workspaces share a turn identity, which
	// is what `RemintPortedPrompts` exists to prevent.
	parentTurns := make(map[string]bool, len(before.Turns))
	for _, turn := range before.Turns {
		parentTurns[turn.Turn] = true
	}
	for _, row := range ported {
		if parentTurns[row.Turn] {
			t.Errorf("the fork's inherited row at ordinal %s carries turn id %s, which is one of the "+
				"PARENT'S own live turn ids. A fork re-mints every turn id it inherits; sharing one means "+
				"the two workspaces name the same turn", row.Ordinal, row.Turn)
		}
	}

	// The transcript half, as far as it is visible from here: the child resumes
	// under a vendor session id of its own. The parent's id appearing on the
	// child would mean two workspaces claiming one vendor session, which is
	// single-occupancy by contract.
	forkSession, err := rt78ReadSession(ctx, dbPath, fork.ID)
	if err != nil {
		t.Fatalf("read the fork's session row: %v", err)
	}
	switch {
	case !forkSession.Exists:
		t.Errorf("the fork %s has no `sessions` row, so nothing says which vendor session carries its "+
			"ported transcript", fork.ID)
	case forkSession.VendorID == "":
		t.Errorf("the fork %s has a session row with no vendor session id, so its ported transcript has no "+
			"identity to resume under", fork.ID)
	case before.Session.Exists && forkSession.VendorID == before.Session.VendorID:
		t.Errorf("the fork %s and its parent %s share the vendor session id %s. A fork mints a FRESH one "+
			"and re-mints the transcript into it; a vendor session id is single-occupancy",
			fork.ID, parent.ID, forkSession.VendorID)
	default:
		t.Logf("the fork resumes under its own vendor session id, not the parent's")
	}

	t.Logf("the fork carries the parent's whole conversation: %d inherited prompt(s), contiguous from "+
		"ordinal 0, under the fork's own re-minted turn ids", len(ported))
}

// rt7AssertParentIsUnchanged is the plan's third assertion: forking is not a
// move.
//
// It compares the parent against the snapshot taken before the fork field by
// field, because "unchanged" phrased as one comparison would name nothing when
// it failed, and these fields fail for entirely different reasons. The parent
// GAINING rows in `ported_prompts` gets its own check: those rows are what a
// workspace INHERITED, and a parent that suddenly has them has been treated as
// somebody's child by the very operation that made it a parent.
func rt7AssertParentIsUnchanged(ctx context.Context, t *testing.T, dbPath string,
	parent Workspace, parentName string, before rt7State) {
	t.Helper()

	after := rt7Snapshot(ctx, t, dbPath, parent.ID, "the parent, after the fork")

	if !wsActSameDir(after.Row.Dir, before.Row.Dir) {
		t.Errorf("the parent %s moved from %q to %q when it was forked", parent.ID, before.Row.Dir, after.Row.Dir)
	}
	if after.Facts.Branch != before.Facts.Branch {
		t.Errorf("the parent %s changed branch from %q to %q when it was forked",
			parent.ID, before.Facts.Branch, after.Facts.Branch)
	}
	if after.Facts.ParentID != before.Facts.ParentID {
		t.Errorf("the parent %s changed its own `parent_id` from %q to %q when it was forked: a fork gives "+
			"the CHILD a parent, never the parent one", parent.ID, before.Facts.ParentID, after.Facts.ParentID)
	}
	if after.Facts.Closed != before.Facts.Closed {
		t.Errorf("the parent %s went from closed=%v to closed=%v when it was forked",
			parent.ID, before.Facts.Closed, after.Facts.Closed)
	}
	if len(after.Ported) != len(before.Ported) {
		t.Errorf("the parent %s went from %d to %d row(s) in `ported_prompts`. Those rows are what a "+
			"workspace INHERITED, and forking gives them to the child, never to the parent",
			parent.ID, len(before.Ported), len(after.Ported))
	}

	beforeTexts := rt78PromptTexts(before.Conversation)
	afterTexts := rt78PromptTexts(after.Conversation)
	if len(afterTexts) != len(beforeTexts) {
		t.Errorf("the parent's conversation went from %d to %d prompt(s) across the fork.\n  before: %q\n  after:  %q",
			len(beforeTexts), len(afterTexts), beforeTexts, afterTexts)
	} else {
		for i := range beforeTexts {
			if beforeTexts[i] != afterTexts[i] {
				t.Errorf("the parent's prompt %d changed from %q to %q across the fork",
					i, beforeTexts[i], afterTexts[i])
			}
		}
	}

	if before.Session.Exists && after.Session.Exists && after.Session.VendorID != before.Session.VendorID {
		t.Errorf("the parent %s changed vendor session id from %s to %s across the fork: the fork ports a "+
			"COPY of the transcript and leaves the parent's own alone",
			parent.ID, before.Session.VendorID, after.Session.VendorID)
	}
	if after.Session.TerminalKind != before.Session.TerminalKind {
		t.Errorf("the parent %s's session terminal changed from %q to %q across the fork: forking a "+
			"workspace does not end its session",
			parent.ID, before.Session.TerminalKind, after.Session.TerminalKind)
	}

	t.Logf("the parent %q is unchanged across the fork: same worktree, same branch, same %d-prompt "+
		"conversation, same session", parentName, len(afterTexts))
}

// ---- Small readings -----------------------------------------------------

// rt7ChildOf finds the workspace whose `parent_id` names `parentID`.
//
// The fork is found BY ITS PARENTAGE rather than by name or by "a row that
// appeared": the daemon mints the name, so the run cannot predict it, and
// looking for any new row would race whatever else the owner's daemon happens
// to create while the run is going.
func rt7ChildOf(ctx context.Context, dbPath string, all []Workspace, parentID string) (Workspace, bool) {
	facts, err := rt78ReadFacts(ctx, dbPath)
	if err != nil {
		return Workspace{}, false
	}
	for _, ws := range all {
		if ws.ID != parentID && facts[ws.ID].ParentID == parentID {
			return ws, true
		}
	}
	return Workspace{}, false
}

// rt7TabName is the name the tab bar draws a workspace under.
//
// The roster's own row name is preferred over the state database's, because the
// bar is drawn from the roster and the roster disambiguates names that collide
// across repositories. The database name is the fallback for a workspace whose
// row has not been pushed yet.
func rt7TabName(ctx context.Context, t *testing.T, client *Client, ws Workspace) string {
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

// rt7Sources enumerates the log sources for a workspace set.
func rt7Sources(t *testing.T, home string, workspaces []Workspace) []Source {
	t.Helper()
	sources, err := EnumerateSources(RealEnv(home, workspaces))
	if err != nil {
		t.Fatalf("enumerate the logs: %v", err)
	}
	return sources
}

// rt7Describe renders a workspace list for a failure message.
func rt7Describe(list []Workspace) string {
	if len(list) == 0 {
		return "(none)"
	}
	parts := make([]string, 0, len(list))
	for _, ws := range list {
		parts = append(parts, fmt.Sprintf("%s (%s at %s)", ws.ID, ws.Name, ws.Dir))
	}
	return strings.Join(parts, "; ")
}

// rt7AnyContains reports whether any of `values` contains `want`.
//
// A containment check rather than equality: the module may decorate a prompt on
// its way to the daemon (a metaprompt directive, a prefix), and the assertion
// is that the parent's sentence travelled, not that nothing was added around
// it.
func rt7AnyContains(values []string, want string) bool {
	for _, value := range values {
		if strings.Contains(value, want) {
			return true
		}
	}
	return false
}
