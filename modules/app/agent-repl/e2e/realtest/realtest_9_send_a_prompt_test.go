//go:build realtest

package realtest

import (
	"bufio"
	"context"
	"encoding/json"
	"fmt"
	"os"
	"path/filepath"
	"regexp"
	"strings"
	"sync"
	"testing"
	"time"
)

// REALTEST 9 — SEND A PROMPT, WATCH IT THINK, READ THE ANSWER.
//
// docs/REALTEST-PLAN.md, "Conversation", item 9: "Send a prompt, watch it
// think, read the answer. Tab arm, footer and feed through the whole turn."
//
// It is the first realtest whose SUBJECT is a turn. Realtest 7 already gives a
// workspace a conversation, but it does so through `agent-repl--send` as a
// stated PRECONDITION and asserts nothing about the turn; here the send is the
// act, it is entered by real keys, and the whole turn is watched from the
// press to the conclusion.
//
// # THE ACTS, and which of them are real keys
//
//  1. A dedicated scratch repository is created under the run directory and
//     REGISTERED, exactly as realtests 5 through 8 do it and through the same
//     substrate. Registering is realtest 6's subject; here it is only the
//     ground the send stands on, and the editor lands on the workspace it
//     minted (`agent-repl-verbs-select-minted`), which is the workspace every
//     act below addresses.
//
//  2. `SPC o v` — `agent-repl-focus-input` (lisp/keybindings.el) — is PRESSED
//     as real key events, and the composer becoming the selected buffer is the
//     proof it arrived, read back through the read-only probe. It asks no
//     minibuffer question, so the substrate's prompt-shaped proof
//     (`wsActProveChord`) does not apply and Emacs's own `(recent-keys)` is
//     asserted alongside the buffer, the way realtest 8 proves `SPC j d` and
//     `SPC j x`.
//
//  3. THE PROMPT IS TYPED, CHARACTER BY CHARACTER, AS REAL KEY EVENTS. This is
//     the one realtest where nothing about the send may be supplied through the
//     probe transport: `agent-repl-send` reads the COMPOSER rather than taking
//     an argument, so a prompt inserted through a probe would be testing the
//     send with the composer's own path removed — and the composer is half of
//     what item 9 names. `i` enters evil insert state, the prompt's characters
//     are posted one keycode at a time (rt9PromptKeys), `<escape>` returns to
//     normal state, and the composer's contents are then READ BACK. The read-
//     back text, not the intended text, is what the answer is compared against:
//     a dropped character is reported as a harness key-delivery finding in its
//     own right, and the turn is still judged against the prompt the editor
//     actually holds rather than the one the run meant to type.
//
//  4. `RET` in the composer — `agent-repl-send` (lisp/input.el's
//     `agent-repl-input-mode-map`, `:ni "RET"`) — is PRESSED as a real key
//     event. That press is the act under test. `S-RET` is `newline`, `C-RET`
//     is the postfix send and `[remap +default/newline-above]` is the prefix
//     send; the plain RET is the ordinary one the owner presses, and the one
//     the plan names.
//
// # WHAT THE FAKE VENDOR ANSWERS, AND WHY THAT SCENARIO
//
// The vendor is forbidden for the whole run (`AGENT_REPL_FORBID_VENDOR_CALLS=1`
// on the Emacs process), and a daemon under the guard spawns every shim with
// `--fake` (`daemon/internal/shimclient/supervisor.go`, `fakeMode`), so the
// turn is answered from the offline scripted SDK. NOTHING EXTRA IS STATED ON
// THE LAUNCH, for the reason docs/REALTEST-PLAN.md gives under "Running
// realtest 7": `AGENT_REPL_FAKE_SHIMS=1` is a separate daemon test hook and is
// not needed here.
//
// The prompt names no `!scenario`, so it falls through to the DEFAULT PROSE
// SCENARIO (agent-shim/claude/shim/src/fake/scenarios/prose.ts, `PROSE`), which
// is chosen rather than added to because it is exactly the shape item 9 asks
// for and it is the shape an ordinary offline session already has:
//
//   - it THINKS, on both arms — one withheld thinking block and one visible
//     thinking block — before it says anything, which is what puts a turn into
//     the thinking arm at all;
//   - it then emits two text blocks, the first of them the fixed sentence
//     rt9AnswerOpening, so a known text reaches the feed whose content does not
//     depend on the prompt, the model or the permission mode;
//   - it concludes with `echo: <prompt> [mode=…] [model=…]`, which carries the
//     prompt VERBATIM, so the answer can be tied to this run's own send and not
//     to any other turn in the owner's logs;
//   - and it answers in microseconds, so nothing about this realtest waits on
//     the vendor.
//
// A NEW SCENARIO WAS DELIBERATELY NOT ADDED. `!hold` never concludes (it is
// realtest 10's subject) and everything else in the registry is a shape some
// other realtest is for; a scenario minted for this run would make the run's
// evidence about a code path only this run takes.
//
// # THE EDGES THIS RUN ASSERTS, AND THE ONES THE PRODUCT DOES NOT WRITE
//
// Every latency here is the delta between two REAL log edges (the spec's
// standing rule, "Latency is always an intrinsic log-edge delta"). The edges
// that exist are:
//
//	submit      `elisp.input.send ws=… origin=:user-sent` — INFO, written by
//	            `agent-repl--send`, the one body every production send site
//	            shares (lisp/input.el).
//	recorded    the `turns` row in `~/.claude-emacs/wsm.db` carrying the prompt
//	            text — the DAEMON'S OWN durable statement that the prompt
//	            reached it, read read-only through the snapshot path state.go
//	            owns.
//	delivered   `daemon.promptqueue.deliver` "delivered the prompt to the shim"
//	            — INFO (daemon/internal/promptqueue/deliver.go).
//	turn opened `shim.engine.turn` "opened a turn, delivered its prompt, and
//	            painted the opening page" — INFO
//	            (agent-shim/claude/shim/src/engine/turn.ts).
//	concluded   the same `turns` row's `closed_at` — the daemon's own durable
//	            turn-end edge.
//	in the feed `feed.final-answer-marked` — the webapp putting the final-answer
//	            treatment on the answering row, which is the feed's own
//	            statement that the answer is drawn in it
//	            (webapp/src/feed/rows/turn-ended.ts).
//	answer drawn
//	            `feed.draw-response` — INFO on a response row's first draw and
//	            on its settled one, carrying `characters` and `blocks`
//	            (webapp/src/feed/cards/response.ts). It is the edge the turn's
//	            last phase is measured to.
//
// FOUR EDGES ITEM 9 NAMES DO NOT EXIST, and each is recorded in
// docs/REALTEST-JUDGEMENT-CALLS.md as a LOGGING DEFECT for the lead to
// dispatch. This realtest asserts what exists and says, in its own manifest,
// what it could not assert:
//
//   - `daemon.promptqueue.submit` writes only a DEBUG record on the ordinary
//     path ("no lease stands; the submission takes the ordinary path",
//     daemon/internal/promptqueue/submit.go), and the deployed daemon runs at
//     the contract's INFO default, so no submit record is persisted. The run
//     SCANS for it and reports whether it was there; the prompt reaching the
//     daemon is asserted on the `turns` row and on `deliver`, both of which are
//     durable.
//   - THE FOOTER STATUS IS NOT LOGGED AND NOT READABLE. The progress footer is
//     drawn in the webview from the daemon's footer resolution, whose only
//     account of itself is `daemon.footer.status_decision` at DEBUG. There is
//     no probe for it — elisp cannot read the webview — and no INFO record
//     states the phase word. The run asserts the same status vocabulary where
//     it IS readable, on the tab arm.
//   - NOTHING RECORDS THE TAB ARM'S TRANSITIONS. `elisp.status.tab-state` is
//     log-verbose, so a run cannot read the arm's history out of the log and
//     has to sample it (rt9ArmWatch below). The SETTLED arm is asserted,
//     because it is deterministic; the working arms are REPORTED, because a
//     sampler that missed a transient the log never wrote would be a flake and
//     not a finding.
//   - NO LOG ANYWHERE CARRIES THE ANSWER'S TEXT. The feed's draw records carry
//     character counts and row ids, never prose, and the daemon's durable
//     tables hold the prompt and not the response. So "the answer text is in
//     the feed" is asserted in two halves that meet: the feed MARKED an
//     answering row for this turn (`feed.final-answer-marked`), and the feed
//     DREW a response bubble holding at least the scenario's known opening
//     sentence (`feed.draw-response`, `characters` and `blocks`).
//
//     THIS HALF USED TO NAME A RECORD THE PRODUCT DOES NOT WRITE FOR AN ANSWER.
//     `feed.draw-text-block` is the prompt block vocabulary's, and the response
//     renderer recorded nothing at all, so rt-run36..39 waited out the ceiling
//     for a 21-character block that no path emits. The record is the webapp's
//     now (`feed.draw-response`, webapp/src/feed/cards/response.ts) and the
//     assumption is gone.
//
// # A DEDICATED SCRATCH REPOSITORY, AND REAL GIT
//
// The lead's standing decision of 2026-09-12, unchanged: registering a
// directory makes the daemon run real git, which is inherent to driving the
// real product and is a different thing from the no-real-git rule governing
// the unit and integration suites. What is fixed is WHERE — a repository this
// test creates under the run directory and nothing else — and everything it
// creates it removes through `t.Cleanup`, so a failure halfway still tears
// down. The one residue the product has no verb for (a register's repository
// record) is stated rather than hidden, in wsActCleanupRegistered, and the
// leftovers guard registered from `wsActScratchRepo` fails the run for any
// registry row that outlives it.
//
// # PRECONDITION FOR THE LEAD
//
// Run this realtest in its own `bin/realtest.sh -run` invocation. It performs a
// cold start, and a cold start refuses to run against an Emacs that is already
// answering.
//
// THE REMEDIATION BAR IS THE LOG HARVEST, the same one every realtest carries:
// every WARN and ERROR written inside the run window across every log, with no
// allowlist, into the run's MANIFEST.md, and the test fails when the count is
// non-zero.

// ---- What is typed, and what comes back --------------------------------

// rt9Prompt is the sentence this run types into the composer.
//
// LOWERCASE LETTERS AND SPACES ONLY, because every character is posted as a
// real key event and a shifted character would need a modifier the table below
// deliberately does not carry: a prompt that needed one would be testing the
// modifier and not the send.
//
// It is distinctive on purpose, for the reason realtest 7's seed prompt is: the
// answer assertion is a text comparison against records in the OWNER'S logs,
// and a sentence that could plausibly appear in their own transcripts would
// make a false match possible.
const rt9Prompt = "realtest nine sends this prompt"

// rt9AnswerOpening is the fake vendor's fixed opening text block
// (agent-shim/claude/shim/src/fake/scenarios/prose.ts, `PROSE`).
//
// It is the ONE piece of the answer whose text does not depend on the prompt,
// the model or the permission mode, which is what makes it assertable: the
// conclusion is `echo: <prompt> [mode=…] [model=…]` and both bracketed values
// are whatever the session happens to be running under.
const rt9AnswerOpening = "Here is what I found."

// ---- The chords --------------------------------------------------------
//
// Defined here rather than in keys.go for the reason the substrate's are:
// parallel authors are working in that file. The two spellings a Chord carries
// are documented there.

var (
	// rt9OKey is the `o` of `SPC o v`, the leader's session-control prefix.
	rt9OKey = Chord{
		Emacs:     "o",
		Keycode:   31,
		Modifiers: nil,
		Why:       "the leader's session-control prefix; on its own it only opens that prefix",
	}
	// rt9FocusInputKey is the `v` of `SPC o v`, bound to
	// `agent-repl-focus-input` (lisp/keybindings.el).
	rt9FocusInputKey = Chord{
		Emacs:     "v",
		Keycode:   9,
		Modifiers: nil,
		Why:       "completes `SPC o v`, which selects the workspace's composer window",
	}
	// rt9InsertKey is `i`, evil's insert-state entry.
	//
	// The composer does NOT enter insert state on its own when it is focused —
	// only `agent-repl-discard-input` does that — so the run presses the key
	// the owner would press before typing.
	rt9InsertKey = Chord{
		Emacs:     "i",
		Keycode:   34,
		Modifiers: nil,
		Why:       "enters evil insert state in the composer so the characters that follow are typed, not commands",
	}
	// rt9SendKey is `RET` in the composer, bound to `agent-repl-send`
	// (lisp/input.el, `agent-repl-input-mode-map`, `:ni "RET"`).
	//
	// EMACS RECORDS IT AS `<return>`, NOT `RET`, for exactly the reason
	// wsActTab records as `<tab>`: the physical return key on this GUI (NS)
	// build arrives as the function key symbol `return`, which
	// `key-description` renders `<return>`, while `(kbd "RET")` is the ASCII
	// character 13 that renders "RET". A `(recent-keys)` assertion written
	// against the typed spelling would report the send as uncreditable while
	// the ring plainly held it.
	//
	// IT IS NOT REPEATABLE. It submits, and a second delivery would send the
	// composer's contents a second time — or, once the acceptance has cleared
	// the composer, send nothing and log `elisp.input.send-empty`. A run that
	// reported one turn when two happened would be lying in the worse
	// direction.
	rt9SendKey = Chord{
		Emacs:     "RET",
		Recorded:  "<return>",
		Keycode:   36,
		Modifiers: nil,
		Why:       "the composer's send key; it submits the composer's contents as one turn",
		RepeatWhy: "it submits the composer, so a second delivery would send a second turn or an empty one",
	}
)

// rt9PromptKeys is the keycode of every character rt9Prompt is made of, on a US
// layout.
//
// A table rather than a computed mapping: the set is tiny, a wrong entry is a
// silently mistyped prompt, and a reader can check a table against
// `<HIToolbox/Events.h>` in one pass. Anything not in it is a programming
// error in rt9Prompt and stops the run at the site (rt9TypePrompt).
var rt9PromptKeys = map[rune]int{
	'a': 0, 'e': 14, 'd': 2, 'h': 4, 'i': 34, 'l': 37, 'm': 46, 'n': 45,
	'o': 31, 'p': 35, 'r': 15, 's': 1, 't': 17, ' ': 49,
}

// ---- The markers -------------------------------------------------------
//
// Each is matched against `"<operation> <message>"`, because the two halves of
// the record carry the marker in different systems: an elisp record spells its
// operation INSIDE the message (`elisp.input.send ws=…`) while the daemon, the
// shim and the webapp carry it in the `operation` field with prose in the
// message. One reader for both is what keeps a marker from being written twice
// with two different anchors.

var (
	// rt9SendRe is `elisp.input.send ws=NAME origin=ORIGIN …`, input.el's own
	// account of a submission leaving the editor. It is the SUBMIT EDGE.
	rt9SendRe = regexp.MustCompile(`(^|\s)elisp\.input\.send ws=(\S+) origin=(\S+)`)
	// rt9SubmitRe is `daemon.promptqueue.submit`, the queue's own door. It is
	// DEBUG on the ordinary path and therefore absent from a default-level
	// daemon's log; the run scans for it and reports, and asserts elsewhere.
	rt9SubmitRe = regexp.MustCompile(`(^|\s)daemon\.promptqueue\.submit(\s|$)`)
	// rt9DeliverRe is `daemon.promptqueue.deliver`, INFO on the delivery of a
	// prompt to the shim.
	rt9DeliverRe = regexp.MustCompile(`(^|\s)daemon\.promptqueue\.deliver\s+delivered the prompt`)
	// rt9TurnOpenedRe is the shim's INFO record for a turn it accepted.
	rt9TurnOpenedRe = regexp.MustCompile(`(^|\s)shim\.engine\.turn\s+opened a turn`)
	// rt9FeedUserPromptRe is the webapp drawing the user's own bubble.
	rt9FeedUserPromptRe = regexp.MustCompile(`(^|\s)feed\.draw-user-prompt(\s|$)`)
	// rt9FeedResponseRe is the webapp drawing an agent response bubble. Its
	// `characters` context field carries the prose the row drew and `blocks`
	// how many prose blocks it drew, and it is INFO on a row's first draw and
	// on its settled one (webapp/src/feed/cards/response.ts).
	//
	// IT IS NOT `feed.draw-text-block`. That record belongs to the PROMPT block
	// vocabulary (webapp/src/feed/rows/blocks.ts) and no path draws an
	// assistant response through it, which is why sweeps rt-run36..39 waited
	// out the ceiling for a 21-character block that no code emits while the
	// answer was plainly on the page.
	rt9FeedResponseRe = regexp.MustCompile(`(^|\s)feed\.draw-response(\s|$)`)
	// rt9FeedFinalAnswerRe is the webapp marking the answering row with the
	// final-answer treatment: the feed's own statement that the answer is in
	// it.
	rt9FeedFinalAnswerRe = regexp.MustCompile(`(^|\s)feed\.final-answer-marked(\s|$)`)
	// rt9FeedAnyRe is any feed draw record at all. It answers a different
	// question from the three above: whether the webapp's feed records reached
	// the log in this window AT ALL, which separates "the feed drew nothing"
	// from "these records are not persisted at this level".
	rt9FeedAnyRe = regexp.MustCompile(`(^|\s)feed\.[a-z-]`)
)

// ---- The ceilings ------------------------------------------------------
//
// THESE ARE OBSERVATION CEILINGS, NOT BUDGETS, exactly as realtest 1's four
// are (e2e/REALTEST-SPEC.md, "Budgets, and why they ship unmeasured"). They
// bound how long the run waits before REPORTING that something did not happen,
// and they are generous on purpose: a ceiling that fires turns a measurable
// slow turn into an unmeasurable timeout, which throws away the evidence the
// run exists to collect.
//
// NO PHASE BUDGET IS ADDED TO budgets.go BY THIS REALTEST, and that is the
// repo's standing rule rather than an omission: a bound invented before the
// first observation is a guess. The turn's phases are MEASURED and reported at
// the site and in the manifest, and the manifest says in as many words that
// they are to be turned into budgets — a small multiple of the observed healthy
// maximum, with the runs behind it named — once the realtest run directory
// history holds enough of them.

const (
	// rt9SubmitCeiling bounds the submit edge reaching the log after the send
	// key is pressed. It covers one Emacs command, one daemon round trip and
	// the log write behind them.
	rt9SubmitCeiling = 60 * time.Second
	// rt9TurnCeiling bounds the whole turn, from the press to the daemon
	// stamping the turn closed. It is the substrate's own act ceiling, for the
	// same reason: a turn is a daemon round trip plus a shim spawn plus the
	// scripted vendor, and none of those has been measured on this machine
	// through this path.
	rt9TurnCeiling = wsActActCeiling
	// rt9FeedCeiling bounds the webapp's feed records for a turn that has
	// already concluded. The feed is downstream of the conclusion, so this is
	// the drawing and the ClientLog hop and nothing else.
	rt9FeedCeiling = 60 * time.Second
	// rt9ArmSampleInterval is how often the tab arm is read while the turn
	// runs. Tighter than the poll interval the substrate's waits use, because
	// this is sampling a transition rather than waiting for a settled fact.
	rt9ArmSampleInterval = 150 * time.Millisecond
)

// rt9SettledArms are the arms a workspace with no turn in flight may wear.
//
// `:idle`, `:ready` and `:done` are the three the roster resolves for a
// workspace whose session is up and doing nothing (lisp/status.el's icon table
// and the color table beside it). Anything else standing after the turn
// concluded is the finding.
var rt9SettledArms = map[string]bool{":idle": true, ":ready": true, ":done": true}

// rt9WorkingArms are the arms that say a turn is in flight. `:submitting` is
// the prompt on its way to the agent and `:thinking` is the agent on it; the
// footer's own `thinking · submitting` sub-status is the same split
// (daemon/internal/resolve/footer/api.go).
var rt9WorkingArms = map[string]bool{":submitting": true, ":thinking": true}

func TestRealtestSendAPrompt(t *testing.T) {
	if os.Getenv(runGateEnv) != "1" {
		t.Skipf("realtest 9 drives the owner's real editor and runs only through bin/realtest.sh, which sets %s=1", runGateEnv)
	}
	ctx := context.Background()
	measureOnly := os.Getenv(measureEnv) == "1"
	requireMeasuredBudgets(t, measureOnly)

	home, err := os.UserHomeDir()
	if err != nil {
		t.Fatalf("resolve the owner's home directory: %v", err)
	}
	runDir := startupRunDir(t, home, "realtest-9")
	t.Logf("realtest 9 run directory: %s", runDir)

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
		Title:      "Realtest 9: send a prompt, watch it think, read the answer",
		Started:    started,
		Workspaces: openBefore,
	}
	manifest.Notes = append(manifest.Notes,
		"THE TURN'S PHASES ARE MEASURED AND NOT BOUNDED. This realtest adds no row to "+
			"e2e/realtest/budgets.go: a bound invented before the first observation is a guess "+
			"(AGENTS.md, \"Test wait/timeout bounds are measured, not guessed\"). Each phase below is a "+
			"delta between two real log edges, and once the run directory history holds several of them "+
			"each becomes a budget sized as a small multiple of the observed healthy maximum, with the "+
			"runs behind it named.")
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

	// ---- The ground: a registered scratch repository -------------------

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
	ws, ok := wsActWorkspaceByDir(afterRegister, scratch)
	if !ok {
		t.Fatalf("registering %s minted no workspace, so there is nothing to send a prompt to. The records "+
			"that appeared are: %s", scratch, rt9Describe(wsActNewSince(allBefore, afterRegister)))
	}
	wsName := rt9TabName(ctx, t, client, ws)
	t.Cleanup(func() { wsActCleanupRegistered(ctx, t, client, dbPath, ws, wsName) })
	t.Logf("the prompt's workspace is %s (%q) at %s", ws.ID, wsName, ws.Dir)

	wsActWaitForTabs(ctx, t, fmt.Sprintf("the tab bar to draw %q", wsName), client,
		func(tabs []string) bool { return wsActHasTab(tabs, wsName) })

	// The sinks are captured while the worktree still stands, for the reason
	// wsActPreserveSinks gives: a nuke deletes the LINK and not the bytes, and
	// the harvester reads through the link.
	preserved := wsActPreserveSinks(ws)

	// EVERY EDGE BELOW IS SCANNED AGAINST THE CURRENT WORKSPACE SET, and that
	// is not a refinement — it is the difference between reading this run's
	// evidence and reading none of it.
	//
	// `sources` above was enumerated BEFORE the registration, from `openBefore`,
	// so it holds five canonical links for every workspace that already existed
	// and NONE for the one this run just minted. Every edge this realtest is
	// about — the shim's `shim.engine.turn`, the daemon's `deliver`, the
	// webapp's feed draws — is written into the NEW workspace's own sinks, so a
	// wait over the pre-registration set is waiting on files it never opens.
	// Sweep rt-run35 is exactly that: the run reported "waited 3m0s for the shim
	// to open a turn" and "THE FEED WROTE NOTHING" while the minted workspace's
	// shim log plainly held the turn record, timestamped inside the window.
	//
	// So the set is re-enumerated once here, against the workspace set the state
	// database holds now, the same way the final harvest does it. The SNAPSHOT
	// is deliberately not retaken: it is keyed by inode, so a source it already
	// knows is still read from where this run left it, and a source that did not
	// exist when it was taken is read whole from zero (harvest.go,
	// `resolveReads`) — which is precisely what a sink the run itself created
	// needs.
	turnSources := rt9Sources(t, home, afterRegister)
	t.Logf("scanning %d log source(s) for the turn's edges, re-enumerated against the %d workspace(s) the "+
		"state database holds now (the pre-registration set had %d source(s) and none of %s's)",
		len(turnSources), len(afterRegister), len(sources), ws.ID)

	// EVERY ACT BELOW ADDRESSES THE WORKSPACE THE EDITOR IS STANDING ON, and
	// nothing takes it as a parameter: `SPC o v` focuses the CURRENT
	// workspace's composer and `agent-repl-send` submits to the CURRENT
	// workspace. Registering lands on the workspace it minted
	// (`agent-repl-verbs-select-minted`), and that is asserted here rather than
	// assumed — a prompt sent into the wrong workspace would be a wrong answer
	// about the product produced entirely by a wrong assumption about where the
	// editor was standing.
	current, err := client.ReadString(ctx, `(or (agent-repl--ws-current-name) "")`)
	if err != nil {
		t.Fatalf("read which workspace the editor is standing on before the prompt is typed: %v", err)
	}
	if current != wsName {
		t.Fatalf("the editor is standing on %q, not on the registered workspace %q, so `SPC o v` would focus "+
			"another workspace's composer and the prompt would be sent into it. Registering a directory is "+
			"supposed to land on the workspace it minted (agent-repl-verbs-select-minted)", current, wsName)
	}

	// ---- Act one: `SPC o v` --------------------------------------------

	rt9FocusTheComposer(ctx, t, client, driver, wsName, &manifest)

	// ---- Act two: type the prompt --------------------------------------

	rt9TypePrompt(ctx, t, client, driver, &manifest)
	typed := rt9ReadComposer(ctx, t, client, wsName)
	if typed != rt9Prompt {
		note := fmt.Sprintf("TYPED PROMPT DOES NOT MATCH: the composer holds %q and the run typed %q as %d "+
			"real key events. The turn below is judged against what the composer actually holds, not against "+
			"what was meant, so the run still says something about the product; this is a HARNESS key-delivery "+
			"finding", typed, rt9Prompt, len([]rune(rt9Prompt)))
		manifest.Notes = append(manifest.Notes, note)
		t.Errorf("%s", note)
	} else {
		t.Logf("the composer holds the typed prompt verbatim: %q", typed)
	}
	if strings.TrimSpace(typed) == "" {
		t.Fatalf("the composer is empty after the prompt was typed, so `RET` would submit nothing " +
			"(`elisp.input.send-empty`) and every assertion below would be about a turn that never happened")
	}

	// ---- Act three: the send key, and the whole turn -------------------

	// The arm is sampled from BEFORE the press, through its own emacsclient
	// with its own answer-file ring: `Client.seq` is not concurrency-safe and
	// two probes sharing a ring slot would read each other's answers
	// (emacsclient.go, `probeResult.Seq`).
	//
	// SAMPLING ACROSS THE PRESS IS SAFE FOR THIS CHORD AND WOULD NOT BE FOR
	// EVERY CHORD. Probe traffic makes Emacs busy, and a `quit_char` posted
	// into a busy Emacs is handed to `handle_interrupt` rather than queued
	// (keys.go, `Chord.Interrupting`) — which is what the C-g investigation
	// cost. `RET` is an ordinary key: `kbd_buffer_store_buffered_event` queues
	// it and the command loop reads it when it next looks, so a probe in flight
	// delays the press and cannot swallow it.
	watch := rt9StartArmWatch(ctx, t, socket, runDir, wsName)
	defer watch.Stop()

	pressedAt := time.Now()
	rt9PressSend(ctx, t, client, driver, &manifest)

	// SUBMIT: the editor's own account of the submission leaving it.
	submitAt, submitOK := rt9AwaitEdge(ctx, t, "the editor to record the submission (`elisp.input.send`)",
		rt9SubmitCeiling, turnSources, snapshot, preserved, pressedAt, rt9SendRe,
		func(hit rt9Hit) bool { return strings.Contains(hit.Text, "ws="+wsName) })
	if !submitOK {
		t.Errorf("`RET` was pressed in %q's composer and no `elisp.input.send` record names that workspace "+
			"inside the run window. Either the send key did not reach `agent-repl-send`, or the send refused "+
			"before it logged; both are in this run's harvest", wsName)
		// The submit edge is the origin every phase below is measured from, so
		// the press time stands in for it and every measurement says so.
		submitAt = pressedAt
	}

	// RECORDED: the daemon's own durable statement that the prompt reached it.
	var turn rt9Turn
	waitUntil(ctx, t, "the daemon to record the prompt as a turn", rt9TurnCeiling, func() bool {
		rows, readErr := rt9Turns(ctx, dbPath, ws.ID)
		if readErr != nil {
			return false
		}
		for _, row := range rows {
			if strings.Contains(row.Text, typed) {
				turn = row
				return true
			}
		}
		return false
	})
	if turn.ID == "" {
		rows, _ := rt9Turns(ctx, dbPath, ws.ID)
		t.Fatalf("the prompt %q was sent to %q and the daemon recorded no `turns` row carrying it; the table "+
			"holds %d row(s) for that workspace. Nothing below can be asserted about a turn that does not "+
			"exist, so the run stops here rather than reporting a dozen consequences of one cause",
			typed, wsName, len(rows))
	}
	t.Logf("the daemon recorded the prompt as turn %s", turn.ID)

	// DELIVERED: the daemon handing the prompt to the shim.
	deliverAt, deliverOK := rt9AwaitEdge(ctx, t, "the daemon to deliver the prompt to the shim",
		rt9TurnCeiling, turnSources, snapshot, preserved, pressedAt, rt9DeliverRe,
		func(hit rt9Hit) bool { return hit.NamesWorkspace(ws) })
	if !deliverOK {
		t.Errorf("no `daemon.promptqueue.deliver` record inside the run window names workspace %s (%q), so "+
			"nothing says the prompt this run submitted reached the shim", ws.ID, wsName)
	}

	// THE SUBMIT DOOR'S OWN RECORD, reported rather than asserted: it is DEBUG
	// on the ordinary path and the deployed daemon runs at INFO.
	rt9ReportSubmitDoor(t, turnSources, snapshot, preserved, pressedAt, ws, wsName, &manifest)

	// TURN OPENED: the shim accepting the turn.
	openedAt, openedOK := rt9AwaitEdge(ctx, t, "the shim to open a turn for the prompt",
		rt9TurnCeiling, turnSources, snapshot, preserved, pressedAt, rt9TurnOpenedRe,
		func(hit rt9Hit) bool { return hit.NamesWorkspace(ws) })
	if !openedOK {
		t.Errorf("no `shim.engine.turn` record inside the run window says the shim opened a turn for "+
			"workspace %s (%q). The daemon delivering a prompt the shim never opened is where a turn goes "+
			"missing without anything failing", ws.ID, wsName)
	}

	// CONCLUDED: the daemon stamping the turn closed.
	var concluded rt9Turn
	waitUntil(ctx, t, "the daemon to stamp the turn closed", rt9TurnCeiling, func() bool {
		rows, readErr := rt9Turns(ctx, dbPath, ws.ID)
		if readErr != nil {
			return false
		}
		for _, row := range rows {
			if row.ID == turn.ID && row.ClosedAt != "" {
				concluded = row
				return true
			}
		}
		return false
	})
	concludedAt := time.Now()
	if concluded.ID == "" {
		t.Errorf("turn %s never closed inside %s: the daemon's `turns` row still carries no `closed_at`, so "+
			"as far as the durable record goes the turn is still running. The fake vendor answers in "+
			"microseconds, so a turn that does not conclude is a fact about the daemon, the shim or the link "+
			"between them and never about the model", turn.ID, rt9TurnCeiling)
	} else {
		t.Logf("turn %s concluded (close kind %s)", concluded.ID, orUnknown(concluded.CloseKind))
	}

	// ---- The feed ------------------------------------------------------

	answerDrawnAt, answerDrawnOK := rt9AssertTheAnswerIsInTheFeed(
		ctx, t, turnSources, snapshot, preserved, pressedAt, ws, wsName, &manifest)

	// ---- The arm, through the whole turn -------------------------------

	watch.Stop()
	rt9AssertTheArm(ctx, t, client, watch, wsName, &manifest)

	// ---- The phases ----------------------------------------------------

	rt9ReportPhases(t, &manifest, rt9Timings{
		Pressed:     pressedAt,
		Submitted:   submitAt,
		SubmittedOK: submitOK,
		Delivered:   deliverAt,
		DeliveredOK: deliverOK,
		Opened:      openedAt,
		OpenedOK:    openedOK,
		Concluded:   concludedAt,
		ConcludedOK: concluded.ID != "",
		AnswerDrawn: answerDrawnAt,
		AnswerOK:    answerDrawnOK,
	})

	// ---- The harvest ---------------------------------------------------

	manifest.Ended = time.Now()
	finalAll, _, _, err := wsActWorkspacesNow(ctx, dbPath)
	if err != nil {
		t.Fatalf("read the state database before the harvest: %v", err)
	}
	finalSources := wsActMergeSources(rt9Sources(t, home, finalAll), preserved)
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

// ---- The acts ----------------------------------------------------------

// rt9FocusTheComposer leaves the editor standing in the workspace's composer,
// pressing `SPC o v` as real key events and proving each press reached
// `agent-repl-focus-input`.
//
// `SPC o v` IS A TOGGLE, NOT A SELECT, and this act is written for the command
// the product actually ships. `agent-repl-focus-input` (lisp/panels.el) jumps
// BACK to the workspace's webview when the editor is already standing in the
// composer, and selects the composer from anywhere else. A minted workspace
// auto-selects its composer on arrival (`maybe-autoselect-input`,
// branch=select-input-win), so the common case here is that the first press
// finds the composer already selected and moves the editor OFF it. An act that
// asserted "composer" after one press was asserting the opposite of the
// designed behavior, and read a working toggle as a chord that never arrived.
//
// So the act reads where the editor stands BEFORE pressing, presses once,
// asserts the landing the toggle semantic predicts — the webview when it stood
// on the composer, the composer otherwise — records which case it was in the
// manifest, and presses a second time when the first landed on the webview.
//
// THE PROOF IS NOT A MINIBUFFER PROMPT, because this command asks nothing. It
// is the pair realtest 8 uses for `SPC j d` and `SPC j x`: Emacs's own
// `(recent-keys)` carrying the sequence, and the EFFECT the command exists for
// — the predicted window being the selected one. Either alone would be weaker
// than the pair: the ring says the keys arrived and not what they resolved to,
// and the selected window says something moved the editor and not that this
// chord did. Both presses are proven that same way.
//
// A FAILURE HERE IS FATAL, unlike the substrate's chord proof. The chord and
// the verb are separate claims for a command that can also be entered through
// the probe transport; here the composer being selected WHEN THIS ACT RETURNS
// is a PRECONDITION for everything after it, since the characters typed next go
// wherever the point is. A run that typed its prompt into the owner's source
// file and pressed return there would be worse than a run that stopped.
func rt9FocusTheComposer(ctx context.Context, t *testing.T, client *Client, driver *KeyDriver,
	wsName string, manifest *Manifest) {
	t.Helper()

	sequence := []Chord{wsActLeader, rt9OKey, rt9FocusInputKey}
	spelled := wsActSpell(sequence)

	if driver == nil {
		t.Fatalf("CHORD NOT PRESSED: the key driver is unavailable, so `%s` was never sent. Realtest 9's whole "+
			"subject is a prompt entered by real keys, and there is no probe-transport substitute for it that "+
			"would still be testing the composer", spelled)
	}

	// The reading BEFORE the press is what makes the assertion after it
	// meaningful: the toggle's landing is a function of where the editor
	// already stood, so a run that did not look cannot say whether the chord
	// worked.
	before, err := rt9ComposerSelected(ctx, client, wsName)
	if err != nil {
		t.Fatalf("read where the editor is standing before `%s` is pressed: %v. `%s` toggles between %q's "+
			"composer and its webview, so which landing the press predicts cannot be known without this "+
			"reading, and asserting either one blind would be a coin flip", spelled, err, spelled, wsName)
	}

	want := "composer"
	if before == "composer" {
		want = "webview"
	}
	note := fmt.Sprintf("before `%s` the editor is standing in %q of %q, so the toggle predicts %q for the "+
		"first press", spelled, before, wsName, want)
	manifest.Notes = append(manifest.Notes, note)
	t.Logf("%s", note)

	landed := rt9PressFocusInput(ctx, t, client, driver, sequence, wsName, before, want, "first", manifest)

	if landed == "composer" {
		return
	}

	// The first press jumped back to the webview, which is what the toggle
	// does from the composer. A second press is what leaves the editor where
	// the prompt has to be typed, and it is proven exactly as the first was.
	rt9PressFocusInput(ctx, t, client, driver, sequence, wsName, landed, "composer", "second", manifest)
}

// rt9PressFocusInput presses `SPC o v` once and proves the landing WANT.
//
// It returns where the editor ended up, which is WANT: a landing anywhere else
// is fatal, because the only reason this act exists is to know which buffer the
// next typed character goes into.
func rt9PressFocusInput(ctx context.Context, t *testing.T, client *Client, driver *KeyDriver,
	sequence []Chord, wsName, from, want, ordinal string, manifest *Manifest) string {
	t.Helper()
	defer wsActReportKeyDelivery(t, driver, manifest)

	spelled := wsActSpell(sequence)
	wsActClearPendingInput(ctx, t, client, driver,
		fmt.Sprintf("the %s press of `%s`", ordinal, spelled), manifest)
	for _, chord := range sequence {
		if err := driver.Press(ctx, chord); err != nil {
			t.Fatalf("CHORD NOT DELIVERED: pressing %s of the %s `%s` (%s) failed: %v",
				chord.Emacs, ordinal, spelled, chord.Why, err)
		}
	}

	var where string
	waitUntil(ctx, t, fmt.Sprintf("the %s `%s` to move the editor from %q to %q of %q",
		ordinal, spelled, from, want, wsName), wsActChordCeiling,
		func() bool {
			read, err := rt9ComposerSelected(ctx, client, wsName)
			if err != nil {
				return false
			}
			where = read
			return read == want
		})

	keys, keysErr := RecentKeys(ctx, client)
	if keysErr != nil {
		t.Errorf("read Emacs's own (recent-keys) after the %s `%s`: %v", ordinal, spelled, keysErr)
	}
	recorded := SpellRecorded(sequence)
	if !strings.Contains(keys, recorded) {
		note := fmt.Sprintf("`%s` is not in Emacs's own (recent-keys) as %q, so whatever moved the editor "+
			"on the %s press cannot be credited to this chord. recent-keys ends with: %s",
			spelled, recorded, ordinal, tail(keys, 120))
		manifest.Notes = append(manifest.Notes, note)
		t.Errorf("%s", note)
	}
	if where != want {
		t.Fatalf("CHORD DID NOT REACH ITS COMMAND: the %s `%s` was pressed with the editor standing in %q of "+
			"%q, so `agent-repl-focus-input` should have left it in %q, and it is standing in %q instead. "+
			"Emacs's own (recent-keys) ends with: %s. Everything after this types characters wherever the "+
			"point is, so the run stops rather than typing a prompt into whatever buffer this is",
			ordinal, spelled, from, wsName, want, where, tail(keys, 120))
	}

	note := fmt.Sprintf("the %s real key events `%s` reached `agent-repl-focus-input`: the editor moved from "+
		"%q to %q of %q and (recent-keys) ends with %s", ordinal, spelled, from, want, wsName, tail(keys, 60))
	manifest.Notes = append(manifest.Notes, note)
	t.Logf("%s", note)
	return where
}

// rt9TypePrompt enters insert state and types rt9Prompt one real key at a time.
//
// ONE KEY PER CHARACTER, and no probe writes the text. `agent-repl-send` reads
// the COMPOSER rather than taking an argument, so a prompt inserted through the
// probe transport would remove the composer from the very act item 9 names.
// This is the one place in the realtest layer where text is typed rather than
// supplied, and it is typed because there is nothing else here that would be
// testing the product.
//
// `<escape>` returns evil to normal state before the send. Both states carry
// the binding (`map! :ni "RET" #'agent-repl-send`), and normal state is chosen
// because it is unambiguous: nothing in insert state can be reinterpreting the
// return key when the run presses it.
func rt9TypePrompt(ctx context.Context, t *testing.T, client *Client, driver *KeyDriver, manifest *Manifest) {
	t.Helper()
	defer wsActReportKeyDelivery(t, driver, manifest)

	if err := driver.Press(ctx, rt9InsertKey); err != nil {
		t.Fatalf("press `i` to enter insert state in the composer: %v", err)
	}

	for index, char := range rt9Prompt {
		code, known := rt9PromptKeys[char]
		if !known {
			t.Fatalf("rt9Prompt character %q at index %d has no keycode in rt9PromptKeys, so it cannot be "+
				"typed as a real key event. Add it to the table or take it out of the prompt; a prompt that "+
				"cannot be typed is a prompt this realtest cannot send", char, index)
		}
		chord := Chord{
			Emacs:   string(char),
			Keycode: code,
			Why:     fmt.Sprintf("types character %d of the prompt into the composer", index+1),
			// A dropped character is caught by the read-back below and is
			// reported rather than retried: re-posting it would land it at the
			// point, which after the following characters have arrived is the
			// wrong place, and a prompt reassembled out of order would be a
			// worse reading than a short one.
			RepeatWhy: "it inserts a character at the point, so a second delivery would insert it twice",
		}
		if err := driver.Press(ctx, chord); err != nil {
			t.Fatalf("type character %d (%q) of the prompt as a real key event: %v", index+1, string(char), err)
		}
	}

	if err := driver.Press(ctx, wsActEscape); err != nil {
		t.Fatalf("press `<escape>` to leave insert state before the send: %v", err)
	}
	t.Logf("typed %d characters of the prompt into the composer as real key events", len([]rune(rt9Prompt)))
}

// rt9PressSend presses `RET` in the composer: the act realtest 9 exists for.
//
// It is pressed with no effect to poll for, because the effect is a whole turn
// and the run watches that itself. The press's own receipt — what the helper
// posted and what Emacs's input marks said about it — travels into the manifest
// through wsActReportKeyDelivery like every other press here.
func rt9PressSend(ctx context.Context, t *testing.T, client *Client, driver *KeyDriver, manifest *Manifest) {
	t.Helper()
	defer wsActReportKeyDelivery(t, driver, manifest)

	if err := driver.Press(ctx, rt9SendKey); err != nil {
		t.Fatalf("press `RET` to send the composer's contents: %v. That press is realtest 9's act, and "+
			"nothing after it would be about a turn", err)
	}
	keys, err := RecentKeys(ctx, client)
	if err != nil {
		t.Errorf("read Emacs's own (recent-keys) after the send key: %v", err)
		return
	}
	if !strings.Contains(keys, rt9SendKey.recorded()) {
		note := fmt.Sprintf("THE SEND KEY LEFT NO MARK: Emacs's own (recent-keys) does not contain %q after "+
			"`RET` was posted into the composer. recent-keys ends with: %s. Whether a turn happened is "+
			"decided below by the daemon's own records, and this says the key cannot be credited for it",
			rt9SendKey.recorded(), tail(keys, 120))
		manifest.Notes = append(manifest.Notes, note)
		t.Errorf("%s", note)
		return
	}
	note := fmt.Sprintf("real key event `RET` reached the composer: (recent-keys) ends with %s", tail(keys, 60))
	manifest.Notes = append(manifest.Notes, note)
	t.Logf("%s", note)
}

// ---- Reading the editor -------------------------------------------------

// rt9ComposerSelected answers where the editor is standing, in the vocabulary
// the caller needs: "composer" when the selected window shows this workspace's
// own input buffer, "webview" when it shows the workspace's frontend buffer,
// and otherwise that window's buffer name.
//
// THE WEBVIEW ARM IS NOT DECORATION. `SPC o v` toggles between those two
// buffers, so "the editor is on the webview" is a LANDING this act asserts and
// not a miss; without a name for it the probe would report the webview's buffer
// name and the reader would have to know it to tell a working toggle from a
// chord that went nowhere.
//
// It compares against `:input-buffer` and `:frontend-buffer` on the workspace's
// own plist rather than against buffer NAME patterns, because the names are a
// presentation detail and the plist entries are the product's own answer to
// which buffers those are.
//
// IT READS THE SELECTED WINDOW'S BUFFER, NEVER `(current-buffer)`. Every probe
// form is evaluated inside the transport's own `with-temp-file`
// (emacsclient.go, `probeWrapper`), so `(current-buffer)` inside a probe is the
// transport's temp buffer — named " *temp file*" — and never the buffer the
// owner is standing in. The old form asked that question and answered it about
// itself: it reported " *temp file*" for a correctly focused editor, which read
// as the chord having missed. Realtest 4's own selection probe reads
// `(window-buffer (selected-window))` for this reason and this one matches it.
func rt9ComposerSelected(ctx context.Context, client *Client, wsName string) (string, error) {
	form := fmt.Sprintf(`(let* ((ws %q)
       (sel (window-buffer (selected-window)))
       (buf (and (fboundp 'agent-repl--ws-get) (agent-repl--ws-get ws :input-buffer)))
       (webview (and (fboundp 'agent-repl--ws-get) (agent-repl--ws-get ws :frontend-buffer))))
  (cond
   ((and buf (buffer-live-p buf) (eq sel buf)) "composer")
   ((and webview (buffer-live-p webview) (eq sel webview)) "webview")
   (t (buffer-name sel))))`, wsName)
	return client.ReadString(ctx, form)
}

// rt9ReadComposer reads the composer's contents through the product's own
// reader.
//
// `agent-repl--read-input-buffer` is the ONE body every send site and the
// history push alike read the composer through, and it strips the drawn
// attachment markers that are not composed text. Reading the buffer directly
// would read something no send site ever sees.
func rt9ReadComposer(ctx context.Context, t *testing.T, client *Client, wsName string) string {
	t.Helper()
	form := fmt.Sprintf(`(or (and (fboundp 'agent-repl--read-input-buffer)
             (agent-repl--read-input-buffer %q))
        "")`, wsName)
	text, err := client.ReadString(ctx, form)
	if err != nil {
		t.Fatalf("read %q's composer back after typing the prompt into it: %v", wsName, err)
	}
	return strings.TrimSpace(text)
}

// ---- The tab arm, sampled ----------------------------------------------

// rt9ArmSample is one reading of the tab's three axes.
type rt9ArmSample struct {
	At time.Time
	// Arm is the roster status arm keyword, e.g. ":thinking", or "nil" when
	// the roster has not spoken about the workspace.
	Arm string
	// Color is the hue the tab bar paints that arm with
	// (`agent-repl-status-tab-color`), so the manifest carries the vocabulary
	// AGENTS.md's colour split is written in.
	Color string
	// Extent is "full" or "partial", the tab-bar vocabulary's own words for
	// whether the status colour reaches the name region.
	Extent string
}

// rt9ArmWatch samples one workspace's tab arm while a turn runs.
//
// IT EXISTS BECAUSE THE ARM'S HISTORY IS NOT WRITTEN ANYWHERE.
// `elisp.status.tab-state` is log-verbose, so a run cannot read the transitions
// out of the log the way it reads every other edge here, and sampling is the
// only reading available. That is recorded as a logging defect in
// docs/REALTEST-JUDGEMENT-CALLS.md; what follows from it is how the samples are
// USED: the settled arm is asserted, because it is a state and not a transient,
// and the working arms are reported, because a sampler that missed a transition
// shorter than its interval would be a flake rather than a finding.
type rt9ArmWatch struct {
	mu      sync.Mutex
	samples []rt9ArmSample
	stop    chan struct{}
	done    chan struct{}
	once    sync.Once
	// Err is the last probe error, kept so a watch that read nothing can say
	// why rather than reading as a workspace that never changed arm.
	err error
}

// rt9StartArmWatch begins sampling and returns the watch.
//
// ITS OWN CLIENT, ITS OWN SCRATCH DIRECTORY. `Client.seq` is incremented
// without a lock and the answer files cycle through a fixed ring, so two
// goroutines sharing one client would race for the sequence and read each
// other's answers out of the same slot (emacsclient.go, `probeResult.Seq`).
func rt9StartArmWatch(ctx context.Context, t *testing.T, socket, runDir, wsName string) *rt9ArmWatch {
	t.Helper()
	scratch := filepath.Join(runDir, "arm-watch")
	if err := os.MkdirAll(scratch, 0o755); err != nil {
		t.Fatalf("make the arm watcher's own probe scratch directory %s: %v", scratch, err)
	}
	client := &Client{Socket: socket, Scratch: scratch}
	form := fmt.Sprintf(`(let* ((ws %q)
       (arm (ignore-errors (and (fboundp 'agent-repl-status-tab-state)
                                (agent-repl-status-tab-state ws)))))
  (format "%%s\037%%s\037%%s"
          (if arm (symbol-name arm) "nil")
          (or (ignore-errors (and (fboundp 'agent-repl-status-tab-color)
                                  (agent-repl-status-tab-color arm)))
              "unreadable")
          (if (ignore-errors (and (fboundp 'agent-repl--ws-display-state)
                                  (agent-repl--ws-display-state ws)))
              "full" "partial")))`, wsName)

	watch := &rt9ArmWatch{stop: make(chan struct{}), done: make(chan struct{})}
	go func() {
		defer close(watch.done)
		for {
			raw, err := client.ReadString(ctx, form)
			at := time.Now()
			watch.mu.Lock()
			if err != nil {
				watch.err = err
			} else {
				fields := strings.Split(raw, wsActFieldSep)
				if len(fields) == 3 {
					sample := rt9ArmSample{At: at, Arm: fields[0], Color: fields[1], Extent: fields[2]}
					last := len(watch.samples) - 1
					// ONLY TRANSITIONS ARE KEPT. A turn takes seconds and the
					// interval is a seventh of a second, so storing every
					// reading would put hundreds of identical rows in the
					// manifest and bury the handful that say something.
					if last < 0 || watch.samples[last].Arm != sample.Arm ||
						watch.samples[last].Color != sample.Color ||
						watch.samples[last].Extent != sample.Extent {
						watch.samples = append(watch.samples, sample)
					}
				}
			}
			watch.mu.Unlock()

			select {
			case <-watch.stop:
				return
			case <-ctx.Done():
				return
			case <-time.After(rt9ArmSampleInterval):
			}
		}
	}()
	return watch
}

// Stop ends the sampling and waits for the goroutine to finish.
//
// Idempotent, because it is both deferred and called at the site where the run
// is done with it: the deferred call is what covers a `t.Fatalf` between the
// two, and a second close of the channel would panic.
func (w *rt9ArmWatch) Stop() {
	w.once.Do(func() {
		close(w.stop)
		<-w.done
	})
}

// Samples returns the transitions observed, oldest first.
func (w *rt9ArmWatch) Samples() []rt9ArmSample {
	w.mu.Lock()
	defer w.mu.Unlock()
	return append([]rt9ArmSample{}, w.samples...)
}

// Err returns the last probe error, if any.
func (w *rt9ArmWatch) Err() error {
	w.mu.Lock()
	defer w.mu.Unlock()
	return w.err
}

// rt9AssertTheArm is item 9's "tab arm ... through the whole turn".
//
// The SETTLED arm is asserted and the WORKING arms are reported, and the
// difference is deliberate — see rt9ArmWatch. The settled arm is read fresh
// rather than taken from the last sample: the sampler is stopped before this
// runs, and a state read after the turn concluded is a state, not a race.
func rt9AssertTheArm(ctx context.Context, t *testing.T, client *Client, watch *rt9ArmWatch,
	wsName string, manifest *Manifest) {
	t.Helper()

	samples := watch.Samples()
	if err := watch.Err(); err != nil {
		note := fmt.Sprintf("THE TAB ARM COULD NOT BE SAMPLED CLEANLY: the last probe of %q's arm failed: %v. "+
			"%d transition(s) were read before that", wsName, err, len(samples))
		manifest.Notes = append(manifest.Notes, note)
		t.Errorf("%s", note)
	}

	rendered := make([]string, 0, len(samples))
	sawWorking := false
	for _, sample := range samples {
		rendered = append(rendered, fmt.Sprintf("%s %s (%s, %s)",
			sample.At.Format("15:04:05.000"), sample.Arm, sample.Color, sample.Extent))
		if rt9WorkingArms[sample.Arm] {
			sawWorking = true
		}
	}
	manifest.Notes = append(manifest.Notes, fmt.Sprintf(
		"THE TAB ARM THROUGH THE TURN, sampled every %s (nothing logs the arm's transitions; "+
			"docs/REALTEST-JUDGEMENT-CALLS.md records that as a logging defect): %s",
		rt9ArmSampleInterval, strings.Join(rendered, " -> ")))
	for _, line := range rendered {
		t.Logf("  tab arm: %s", line)
	}

	if sawWorking {
		t.Logf("the tab arm was observed in a working state (`:submitting` or `:thinking`) during the turn")
	} else {
		// NOT A FAILURE, and the note says why in the manifest so a reader is
		// not left to guess whether the harness or the product came up short.
		note := fmt.Sprintf("THE WORKING ARM WAS NOT OBSERVED. %q's tab was never sampled at `:submitting` or "+
			"`:thinking` while the turn ran. The fake vendor answers in microseconds and the sampler reads "+
			"every %s, so a turn shorter than one interval leaves nothing to see; nothing in any log records "+
			"the arm, so the run cannot tell that case from an arm that never changed. This is REPORTED and "+
			"not failed for exactly that reason, and the logging defect behind it is in "+
			"docs/REALTEST-JUDGEMENT-CALLS.md", wsName, rt9ArmSampleInterval)
		manifest.Notes = append(manifest.Notes, note)
		t.Logf("%s", note)
	}

	settled, err := client.ReadString(ctx, fmt.Sprintf(`(let ((arm (ignore-errors
        (and (fboundp 'agent-repl-status-tab-state) (agent-repl-status-tab-state %q)))))
  (if arm (symbol-name arm) "nil"))`, wsName))
	if err != nil {
		t.Errorf("read %q's tab arm after the turn concluded: %v", wsName, err)
		return
	}
	if !rt9SettledArms[settled] {
		t.Errorf("the turn concluded and %q's tab arm is %s, which is not one of the settled arms "+
			"(:idle, :ready, :done). A workspace whose turn is over and whose tab still says it is working is "+
			"the state the owner reads as a hung session", wsName, settled)
		return
	}
	t.Logf("the turn concluded and %q's tab arm settled at %s", wsName, settled)
}

// ---- The feed ----------------------------------------------------------

// rt9AssertTheAnswerIsInTheFeed is item 9's "the feed shows the user bubble
// then the assistant answer with the known text".
//
// IT IS ASSERTED IN TWO HALVES THAT MEET, because no log anywhere carries the
// answer's prose (see the file's header, and the logging defect in
// docs/REALTEST-JUDGEMENT-CALLS.md):
//
//   - the feed MARKED an answering row for this turn
//     (`feed.final-answer-marked`), which is the feed's own statement that the
//     answer is drawn in it and not merely that a turn ended;
//   - the feed DREW a response bubble holding at least the scenario's known
//     opening sentence (`feed.draw-response`, `characters` >= its length and
//     `blocks` >= 1), which ties a drawn bubble to a known text without the
//     log having to carry the text.
//
// THE SECOND HALF USED TO NAME `feed.draw-text-block` AND A LENGTH OF EXACTLY
// 21, AND THAT WAS FALSE. `feed.draw-text-block` is the PROMPT block
// vocabulary's record (webapp/src/feed/rows/blocks.ts); a response is drawn by
// the response renderer, which wrote nothing at all until `feed.draw-response`
// was added. So sweeps rt-run36..39 waited out the ceiling for a record no code
// path emits, while `feed.final-answer-marked` in the same runs reported
// `styled_bubble: true` — the mark finding and styling the answering
// `.bubble.assistant`, which is the answer, drawn.
//
// THE COMPARISON IS `>=`, NOT `==`, because the daemon folds a response's
// fragments into ONE bubble row and re-pushes the whole row as it grows
// (daemon/internal/resolve/feed/response.go, "THE PROSE FOLD"): the settled
// bubble holds the opening sentence, the thinking prose after it and the
// echoed conclusion, so an equality would only ever match a bubble caught
// mid-arrival. `blocks` is stated by the same record and asserted at one or
// more, so a fold that ever grew a second block is visible rather than assumed.
//
// THE WHOLE ASSERTION IS GATED ON THE FEED HAVING WRITTEN ANYTHING AT ALL. If
// no `feed.*` record reached this workspace's `webapp.log` inside the window,
// that ONE fact is reported instead of three consequences of it: either the
// panel drew nothing, or the webapp's records are not persisted at the level
// this deployment runs, and a reader needs to know which question to ask before
// they read three failures that all have the same cause.
//
// THE TWO HALVES ARE WAITED FOR SEPARATELY, because they arrive by different
// paths. The final-answer mark is drawn from the turn-end path the instant the
// turn concludes; the answer's prose comes the long way round — vendor
// transcript, sidecar, store, daemon, feed — and in rt-run36 landed a minute
// after it. Scanning once the mark was up read the feed at the earliest moment
// the answer could not yet be in it, so the draw is WAITED for on its own
// ceiling.
//
// NO TIMESTAMP FLOOR IS PUT UNDER THAT WAIT, deliberately. The wait is
// SEQUENCED after the mark, but a settled response drawn BEFORE the turn's
// terminal row is the healthy ordering and not a miss, and a floor at the mark
// would make the run wait out its ceiling for a draw that had already happened.
// The earliest qualifying draw is the edge, and the phase measured from it
// reports a negative delta when the answer beat the conclusion's reading —
// which is itself the answer to the question the phase asks.
//
// It returns the instant the answer's own bubble was drawn, so the turn's last
// phase can be measured from the conclusion to it.
func rt9AssertTheAnswerIsInTheFeed(ctx context.Context, t *testing.T, sources []Source, snap Snapshot,
	preserved []Source, since time.Time, ws Workspace, wsName string, manifest *Manifest) (time.Time, bool) {
	t.Helper()

	// The feed is downstream of the conclusion, so its records are waited for
	// rather than read the instant the turn closed.
	waitUntil(ctx, t, "the webapp to mark the turn's answering row in the feed", rt9FeedCeiling, func() bool {
		hits, err := rt9Scan(sources, snap, preserved, since, rt9FeedFinalAnswerRe)
		if err != nil {
			return false
		}
		return rt9AnyNaming(hits, ws)
	})

	any, err := rt9Scan(sources, snap, preserved, since, rt9FeedAnyRe)
	if err != nil {
		t.Errorf("read the webapp's feed records for workspace %s (%q): %v", ws.ID, wsName, err)
		return time.Time{}, false
	}
	if !rt9AnyNaming(any, ws) {
		note := fmt.Sprintf("THE FEED WROTE NOTHING. No `feed.*` record inside the run window is attributed to "+
			"workspace %s (%q), so this run cannot say what the feed drew. Two things produce that and they "+
			"are answered in different places: the panel drew nothing (a product finding, and the harvest "+
			"below carries whatever went wrong), or the webapp's feed records are not persisted at the level "+
			"this deployment runs (a deployment fact). The turn's own conclusion is asserted above and does "+
			"not depend on this", ws.ID, wsName)
		manifest.Notes = append(manifest.Notes, note)
		t.Errorf("%s", note)
		return time.Time{}, false
	}

	userPrompt, err := rt9Scan(sources, snap, preserved, since, rt9FeedUserPromptRe)
	if err != nil {
		t.Errorf("read the feed's user-prompt records for workspace %s (%q): %v", ws.ID, wsName, err)
	} else if !rt9AnyNaming(userPrompt, ws) {
		t.Errorf("the feed drew no user prompt row for workspace %s (%q) inside the run window: the bubble "+
			"carrying what the owner asked is the first thing item 9 names", ws.ID, wsName)
	} else {
		t.Logf("the feed drew the user's own prompt bubble")
	}

	marked, err := rt9Scan(sources, snap, preserved, since, rt9FeedFinalAnswerRe)
	if err != nil {
		t.Errorf("read the feed's final-answer records for workspace %s (%q): %v", ws.ID, wsName, err)
	} else if !rt9AnyNaming(marked, ws) {
		t.Errorf("the feed marked no answering row for workspace %s (%q) inside the run window "+
			"(`feed.final-answer-marked`), so nothing says the turn's answer is drawn in the feed. A "+
			"`feed.final-answer-row-absent` warning in this run's harvest would say the feed was told about "+
			"an answer it had not drawn, which is a different defect", ws.ID, wsName)
	} else {
		t.Logf("the feed marked the turn's answering row with the final-answer treatment")
	}

	// THE ANSWER'S BUBBLE IS WAITED FOR, NOT SCANNED FOR ONCE. It comes by a
	// different path from the mark above, so the mark standing says nothing
	// about the prose being drawn yet.
	want := len([]rune(rt9AnswerOpening))
	drawnAt, drawn := rt9AwaitEdge(ctx, t,
		fmt.Sprintf("the webapp to draw a response bubble of at least %d characters in the feed", want),
		rt9FeedCeiling, sources, snap, preserved, since, rt9FeedResponseRe,
		func(hit rt9Hit) bool { return rt9AnswerDrawn(hit, ws, want) })
	if !drawn {
		// The diagnostic names every response draw the feed DID make, which is
		// what tells a reader whether a shorter answer arrived or none did.
		draws, scanErr := rt9Scan(sources, snap, preserved, since, rt9FeedResponseRe)
		if scanErr != nil {
			t.Errorf("read the feed's response-draw records for workspace %s (%q): %v", ws.ID, wsName, scanErr)
			return time.Time{}, false
		}
		t.Errorf("the feed drew no response bubble of at least %d characters for workspace %s (%q) within %s "+
			"of the turn concluding, %d being the length of the fake vendor's known opening sentence %q. The "+
			"response draws it did make were %s. No log carries a feed row's prose, so the character count is "+
			"the only thing that ties a drawn bubble to a known text; a count that never reaches it says the "+
			"answer that reached the feed is not the one the scenario produced", want, ws.ID, wsName,
			rt9FeedCeiling, want, rt9AnswerOpening, rt9DescribeResponseDraws(draws, ws))
		return time.Time{}, false
	}
	t.Logf("the feed drew a response bubble of at least %d characters, the length of the scenario's known "+
		"opening sentence", want)
	return drawnAt, true
}

// rt9AnswerDrawn reports whether ONE `feed.draw-response` record is this
// workspace drawing an answer that could hold the scenario's opening sentence.
//
// It is a named function rather than a closure so it can be driven over a
// fixture record by TestRt9AnswerDrawnAcceptsOnlyAWorkspacesLongEnoughDraw,
// which needs no editor: the acceptance rule is the whole of what changed when
// the marker moved off `feed.draw-text-block`, and a rule only a live sweep can
// exercise is a rule nothing checks between sweeps.
//
// A record missing either field is REFUSED rather than assumed: `characters`
// and `blocks` are both stated by the producer, and a draw whose fields cannot
// be read is a record this reader does not understand.
func rt9AnswerDrawn(hit rt9Hit, ws Workspace, want int) bool {
	if !hit.Names(ws) {
		return false
	}
	characters, ok := hit.ContextInt("characters")
	if !ok || characters < want {
		return false
	}
	blocks, ok := hit.ContextInt("blocks")
	return ok && blocks >= 1
}

// rt9DescribeResponseDraws renders this workspace's response draws for a
// failure message.
func rt9DescribeResponseDraws(hits []rt9Hit, ws Workspace) string {
	parts := make([]string, 0, len(hits))
	for _, hit := range hits {
		if !hit.Names(ws) {
			continue
		}
		characters, hasCharacters := hit.ContextInt("characters")
		blocks, hasBlocks := hit.ContextInt("blocks")
		if !hasCharacters || !hasBlocks {
			parts = append(parts, fmt.Sprintf("a draw at %s whose `characters`/`blocks` could not be read",
				hit.At.Format(time.RFC3339Nano)))
			continue
		}
		parts = append(parts, fmt.Sprintf("%d characters in %d block(s) at %s", characters, blocks,
			hit.At.Format(time.RFC3339Nano)))
	}
	if len(parts) == 0 {
		return "(none)"
	}
	return strings.Join(parts, "; ")
}

// rt9ReportSubmitDoor scans for the queue's own submit record and reports what
// it found.
//
// IT IS A REPORT AND NOT AN ASSERTION, and the file's header says why: on the
// ordinary path `daemon.promptqueue.submit` is written at DEBUG and the
// deployed daemon runs at the logging contract's INFO default, so its absence
// says nothing about whether the prompt reached the queue. That the door has no
// INFO record at all is recorded in docs/REALTEST-JUDGEMENT-CALLS.md as a
// logging defect; once it has one, this becomes an assertion.
func rt9ReportSubmitDoor(t *testing.T, sources []Source, snap Snapshot, preserved []Source,
	since time.Time, ws Workspace, wsName string, manifest *Manifest) {
	t.Helper()
	hits, err := rt9Scan(sources, snap, preserved, since, rt9SubmitRe)
	if err != nil {
		t.Errorf("scan for the queue's own submit record for workspace %s (%q): %v", ws.ID, wsName, err)
		return
	}
	if rt9AnyNaming(hits, ws) {
		note := fmt.Sprintf("the queue's own door recorded the submission: a `daemon.promptqueue.submit` "+
			"record inside the run window names workspace %s (%q)", ws.ID, wsName)
		manifest.Notes = append(manifest.Notes, note)
		t.Logf("%s", note)
		return
	}
	note := fmt.Sprintf("NO `daemon.promptqueue.submit` RECORD, AND THAT IS EXPECTED TODAY: on the ordinary " +
		"path the queue writes only a DEBUG record at its own door " +
		"(daemon/internal/promptqueue/submit.go, \"no lease stands; the submission takes the ordinary " +
		"path\") and the deployed daemon runs at the contract's INFO default. The prompt reaching the daemon " +
		"is asserted on the daemon's own durable `turns` row and on `daemon.promptqueue.deliver` instead. " +
		"The missing INFO record at the submit door is a LOGGING DEFECT recorded in " +
		"docs/REALTEST-JUDGEMENT-CALLS.md")
	manifest.Notes = append(manifest.Notes, note)
	t.Logf("%s", note)
}

// ---- The phases ---------------------------------------------------------

// rt9Timings is the turn's real log edges, in the order they happen.
type rt9Timings struct {
	Pressed     time.Time
	Submitted   time.Time
	SubmittedOK bool
	Delivered   time.Time
	DeliveredOK bool
	Opened      time.Time
	OpenedOK    bool
	Concluded   time.Time
	ConcludedOK bool
	AnswerDrawn time.Time
	AnswerOK    bool
}

// rt9ReportPhases measures the turn and writes the measurements into the
// manifest.
//
// EVERY PHASE IS AN EDGE-TO-EDGE DELTA and never a harness interval. The one
// exception is stated where it is used: `press -> submit` bounds the editor's
// own path from the key event to the record, which is the only phase whose
// start is something the harness did rather than something the product logged,
// and it is reported as such.
//
// The conclusion's edge is the moment the daemon's `closed_at` was READ, not
// the stamp inside it: the stamp is the daemon's clock in its own units and
// the rest of these are wall-clock instants from log lines, and mixing the two
// would report a delta between two different clocks. The reading is therefore
// an upper bound and says so.
func rt9ReportPhases(t *testing.T, manifest *Manifest, timings rt9Timings) {
	t.Helper()
	type phase struct {
		name  string
		from  time.Time
		to    time.Time
		ok    bool
		about string
	}
	phases := []phase{
		{"send key -> submit record", timings.Pressed, timings.Submitted, timings.SubmittedOK,
			"the editor's own path from the key event to `elisp.input.send`; its start is the harness's " +
				"post and not a product edge, so it is the one phase here that is not edge-to-edge"},
		{"submit -> delivered to the shim", timings.Submitted, timings.Delivered,
			timings.SubmittedOK && timings.DeliveredOK,
			"`elisp.input.send` to `daemon.promptqueue.deliver`"},
		{"submit -> turn opened", timings.Submitted, timings.Opened,
			timings.SubmittedOK && timings.OpenedOK,
			"`elisp.input.send` to the shim's `shim.engine.turn` \"opened a turn\""},
		{"submit -> turn concluded", timings.Submitted, timings.Concluded,
			timings.SubmittedOK && timings.ConcludedOK,
			"`elisp.input.send` to the daemon's `closed_at` being readable; an UPPER BOUND, since the " +
				"end is when the run read the stamp and not the stamp itself"},
		{"turn concluded -> answer text drawn", timings.Concluded, timings.AnswerDrawn,
			timings.ConcludedOK && timings.AnswerOK,
			"the daemon's `closed_at` being readable to the `feed.draw-response` record for the " +
				"answer's own bubble; an OBSERVATION CEILING and not a budget, like every " +
				"phase here. Its start is the run's READ of the stamp rather than the stamp, so a block " +
				"already drawn when the run got round to reading reports as a negative delta — which is " +
				"itself the answer to the question the phase exists to ask"},
	}
	lines := make([]string, 0, len(phases))
	for _, p := range phases {
		if !p.ok {
			line := fmt.Sprintf("%s: NOT MEASURED (%s)", p.name, p.about)
			lines = append(lines, line)
			t.Logf("  turn phase %s", line)
			continue
		}
		line := fmt.Sprintf("%s: %s (%s)", p.name, p.to.Sub(p.from).Round(time.Millisecond), p.about)
		lines = append(lines, line)
		t.Logf("  turn phase %s", line)
	}
	manifest.Notes = append(manifest.Notes,
		"THE TURN'S PHASES, from this run's own log edges:\n  "+strings.Join(lines, "\n  "))
}

// ---- Reading the daemon's own turn record ------------------------------

// rt9Turn is one row of the daemon's `turns` table, with the two columns that
// say a turn is over.
//
// It is not rt78Prompt: that shape reads `ported_prompts` and `turns` through
// the same four fields because a fork assertion compares what was SAID, and it
// carries neither `closed_at` nor `close_kind`. Realtest 9's subject is a turn
// running to its end, so the end is what it has to read.
//
// `closed_at` IS THE TURN-END EDGE, and it is the daemon's own durable one:
// `promptqueue`'s `turn_ended` stamps it (daemon/internal/promptqueue/
// lifecycle.go) and nothing else writes it, so a row carrying it is the daemon
// saying the turn is over rather than a reader inferring it from a quiet log.
type rt9Turn struct {
	ID        string
	Text      string
	StartedAt string
	ClosedAt  string
	CloseKind string
}

// rt9Turns reads one workspace's turns, oldest first.
//
// Through `rt78Query`, which is `queryStateDB`, so this read takes the same
// snapshot-and-verify path every other realtest read of the owner's registry
// takes. A second spelling of "open the owner's database" is exactly the drift
// that rule exists to prevent.
func rt9Turns(ctx context.Context, dbPath, id string) ([]rt9Turn, error) {
	rows, err := rt78Query(ctx, dbPath, fmt.Sprintf(
		`SELECT id, text, started_at, IFNULL(closed_at, ''), IFNULL(close_kind, '') FROM turns
		 WHERE workspace_id = %s ORDER BY started_at, id;`, rt78Quote(id)))
	if err != nil {
		return nil, err
	}
	out := make([]rt9Turn, 0, len(rows))
	for i, fields := range rows {
		if len(fields) != 5 {
			return nil, fmt.Errorf("row %d of `turns` in %s has %d fields, not 5: %q",
				i+1, dbPath, len(fields), strings.Join(fields, "|"))
		}
		out = append(out, rt9Turn{
			ID: fields[0], Text: fields[1], StartedAt: fields[2],
			ClosedAt: fields[3], CloseKind: fields[4],
		})
	}
	return out, nil
}

// ---- Reading the logs ---------------------------------------------------

// rt9Hit is one record a marker matched.
type rt9Hit struct {
	At time.Time
	// Text is `"<operation> <message>"`, which is what the markers are matched
	// against; the two halves carry the marker in different systems.
	Text         string
	WorkspaceID  string
	WorkspaceDir string
	// Source is the sink the record came out of, so a hit can be attributed to
	// a workspace even when the record itself names none.
	Source Source
	// Context is the record's own context object, unparsed.
	Context json.RawMessage
}

// Names reports whether this hit belongs to a workspace.
//
// THREE ANSWERS IN PRECEDENCE ORDER, which is the harvester's own attribution
// rule (harvest.go): the record's `workspace_id`, then its `workspace_dir`,
// then the workspace its SINK belongs to. A per-workspace sink is a strong
// answer on its own — a record in `<worktree>/.claude/emacs/webapp.log` is that
// workspace's by construction — and it is what makes a webapp record, which
// carries no workspace field of its own, attributable at all.
func (h rt9Hit) Names(ws Workspace) bool {
	if ws.ID != "" && h.WorkspaceID == ws.ID {
		return true
	}
	if ws.Dir != "" && h.WorkspaceDir != "" && wsActSameDir(h.WorkspaceDir, ws.Dir) {
		return true
	}
	return ws.ID != "" && h.Source.Workspace == ws.ID
}

// NamesWorkspace is Names, spelled for a caller passing a predicate.
func (h rt9Hit) NamesWorkspace(ws Workspace) bool { return h.Names(ws) }

// ContextInt reads one integer field out of the record's context.
func (h rt9Hit) ContextInt(field string) (int, bool) {
	if len(h.Context) == 0 {
		return 0, false
	}
	var fields map[string]json.RawMessage
	if err := json.Unmarshal(h.Context, &fields); err != nil {
		return 0, false
	}
	raw, ok := fields[field]
	if !ok {
		return 0, false
	}
	var value int
	if err := json.Unmarshal(raw, &value); err != nil {
		return 0, false
	}
	return value, true
}

// rt9AnyNaming reports whether any hit belongs to a workspace.
func rt9AnyNaming(hits []rt9Hit, ws Workspace) bool {
	for _, hit := range hits {
		if hit.Names(ws) {
			return true
		}
	}
	return false
}

// rt9Scan reads EVERY source for one marker, from `since` onward.
//
// It is not wsActScan: that reader is deliberately limited to the two Emacs
// sinks, because the startup markers land nowhere else and the other sinks
// reach gigabytes. Realtest 9's edges are spread across four systems — the
// editor's sink, the daemon's, the shim's and the webapp's — so this one reads
// them all, and it reads the per-workspace sinks rather than the global service
// logs wherever both would carry the record.
//
// IT DOES NOT ENUMERATE ANYTHING. `base` is whatever the caller hands it, and
// the caller owes it a set enumerated against the CURRENT workspace set — which
// is why the turn's edges are scanned over `turnSources`, re-enumerated after
// the registration, and not over the set built before the run had a workspace
// to send to. A comment here once claimed this function re-enumerated per call;
// it never did, and while that claim stood every edge in the turn was being
// waited for over files the minted workspace does not write to.
//
// The preserved sinks are merged in on top, and the offsets come from the
// run-start snapshot by inode: a source the snapshot knew resumes where the run
// left it, and one that appeared afterwards is read whole from zero
// (harvest.go, `resolveReads`).
func rt9Scan(base []Source, snap Snapshot, preserved []Source, since time.Time,
	re *regexp.Regexp) ([]rt9Hit, error) {
	var hits []rt9Hit
	for _, src := range wsActMergeSources(base, preserved) {
		if src.Kind != KindJSONL {
			continue
		}
		reads, _, err := resolveReads(src, snap)
		if err != nil {
			return nil, err
		}
		for _, r := range reads {
			found, err := rt9ScanFile(src, r.path, r.offset, since, re)
			if err != nil {
				return nil, err
			}
			hits = append(hits, found...)
		}
	}
	return hits, nil
}

// rt9ScanFile reads ONE file from `offset` to end for a marker.
//
// A file that does not exist is not an error, for the reason readPhaseRecords
// gives: a workspace whose sink holds no records yet has no file behind its
// link.
func rt9ScanFile(src Source, path string, offset int64, since time.Time, re *regexp.Regexp) ([]rt9Hit, error) {
	file, err := os.Open(path)
	if err != nil {
		if os.IsNotExist(err) {
			return nil, nil
		}
		return nil, fmt.Errorf("open %s to read a turn marker: %w", path, err)
	}
	defer file.Close()
	if offset > 0 {
		if _, err := file.Seek(offset, 0); err != nil {
			return nil, fmt.Errorf("seek %s to the snapshot offset %d: %w", path, offset, err)
		}
	}

	var hits []rt9Hit
	scanner := bufio.NewScanner(file)
	scanner.Buffer(make([]byte, 0, 1<<20), 1<<24)
	for scanner.Scan() {
		text := scanner.Text()
		if strings.TrimSpace(text) == "" {
			continue
		}
		var rec record
		if err := json.Unmarshal([]byte(text), &rec); err != nil {
			// A line this reader cannot parse carries no marker. The harvester
			// reports it as a malformed record in its own right, which is
			// where that finding belongs.
			continue
		}
		at, parseErr := time.Parse(time.RFC3339Nano, rec.Timestamp)
		if parseErr != nil || at.Before(since) {
			continue
		}
		combined := strings.TrimSpace(rec.Operation + " " + rec.Message)
		if !re.MatchString(combined) {
			continue
		}
		hits = append(hits, rt9Hit{
			At: at, Text: combined, WorkspaceID: rec.WorkspaceID,
			WorkspaceDir: rec.WorkspaceDir, Source: src, Context: rec.Context,
		})
	}
	if err := scanner.Err(); err != nil {
		return nil, fmt.Errorf("read %s: %w", path, err)
	}
	return hits, nil
}

// rt9AwaitEdge polls the logs until a marker a predicate accepts appears, and
// returns when it did.
//
// It returns the EARLIEST matching record's timestamp rather than the moment
// the poll noticed it, because that is the edge; the poll's own latency is
// harness overhead and must never reach a reported latency (the spec's
// standing rule).
func rt9AwaitEdge(ctx context.Context, t *testing.T, what string, ceiling time.Duration,
	base []Source, snap Snapshot, preserved []Source, since time.Time,
	re *regexp.Regexp, accept func(rt9Hit) bool) (time.Time, bool) {
	t.Helper()
	var at time.Time
	var found bool
	waitUntil(ctx, t, what, ceiling, func() bool {
		hits, err := rt9Scan(base, snap, preserved, since, re)
		if err != nil {
			return false
		}
		for _, hit := range hits {
			if !accept(hit) {
				continue
			}
			if !found || hit.At.Before(at) {
				at, found = hit.At, true
			}
		}
		return found
	})
	return at, found
}

// ---- Small readings -----------------------------------------------------

// rt9TabName is the name the tab bar draws a workspace under.
//
// The roster's own row name is preferred over the state database's, for the
// reason realtest 7 gives: the bar is drawn from the roster and the roster
// disambiguates names that collide across repositories.
func rt9TabName(ctx context.Context, t *testing.T, client *Client, ws Workspace) string {
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

// rt9Sources enumerates the log sources for a workspace set.
func rt9Sources(t *testing.T, home string, workspaces []Workspace) []Source {
	t.Helper()
	sources, err := EnumerateSources(RealEnv(home, workspaces))
	if err != nil {
		t.Fatalf("enumerate the logs: %v", err)
	}
	return sources
}

// rt9Describe renders a workspace list for a failure message.
func rt9Describe(list []Workspace) string {
	if len(list) == 0 {
		return "(none)"
	}
	parts := make([]string, 0, len(list))
	for _, ws := range list {
		parts = append(parts, fmt.Sprintf("%s (%s at %s)", ws.ID, ws.Name, ws.Dir))
	}
	return strings.Join(parts, "; ")
}

// ---- The one thing about this reader that is unit-testable --------------

// TestRt9ScanReadsASourceThatAppearedAfterTheSnapshotFromZero pins the property
// the pre-registration source set silently broke: a sink that did not exist
// when the snapshot was taken is read WHOLE, from byte zero, and its records
// are found.
//
// It drives no editor and spawns nothing; it is the same shape as the
// wsActForget unit tests further up this package.
//
// The scenario is exactly the one sweep rt-run35 hit: at snapshot time the run
// knows one workspace's shim sink; the workspace it is about to register does
// not exist yet, and its sink appears mid-run carrying the record every edge
// wait is looking for. If the offset for the new source came from anywhere but
// zero — a path-keyed default, or the other source's size — the record would be
// seeked past and the run would report a turn that plainly happened as one that
// never did.
func TestRt9ScanReadsASourceThatAppearedAfterTheSnapshotFromZero(t *testing.T) {
	root := t.TempDir()
	since := time.Now().Add(-time.Minute)
	stamp := time.Now().Format(time.RFC3339Nano)

	rt9WriteRecord := func(path, workspaceID, operation, message string) {
		t.Helper()
		line, err := json.Marshal(record{
			Timestamp: stamp, Level: "INFO", Operation: operation,
			Message: message, WorkspaceID: workspaceID,
		})
		if err != nil {
			t.Fatalf("render a log record: %v", err)
		}
		if err := os.MkdirAll(filepath.Dir(path), 0o755); err != nil {
			t.Fatalf("create the directory holding %s: %v", path, err)
		}
		file, err := os.OpenFile(path, os.O_CREATE|os.O_WRONLY|os.O_APPEND, 0o644)
		if err != nil {
			t.Fatalf("open %s: %v", path, err)
		}
		defer file.Close()
		if _, err := file.Write(append(line, '\n')); err != nil {
			t.Fatalf("append a record to %s: %v", path, err)
		}
	}

	// Arrange: one sink standing before the run, long enough that its size
	// would seek past the whole of the sink that appears later.
	standing := filepath.Join(root, "standing", "shim.log")
	for i := 0; i < 8; i++ {
		rt9WriteRecord(standing, "standing-ws", "shim.engine.turn",
			"opened a turn, delivered its prompt, and painted the opening page")
	}
	standingSource := Source{Name: "workspace.shim.log", Path: standing, Kind: KindJSONL, Workspace: "standing-ws"}
	snap := TakeSnapshot([]Source{standingSource})

	// Act: the run registers a workspace, its sink appears, and the shim writes
	// the turn record into it. The scan is handed the CURRENT source set.
	minted := filepath.Join(root, "minted", "shim.log")
	rt9WriteRecord(minted, "minted-ws", "shim.engine.turn",
		"opened a turn, delivered its prompt, and painted the opening page")
	mintedSource := Source{Name: "workspace.shim.log", Path: minted, Kind: KindJSONL, Workspace: "minted-ws"}

	hits, err := rt9Scan([]Source{standingSource, mintedSource}, snap, nil, since, rt9TurnOpenedRe)
	if err != nil {
		t.Fatalf("scan the sources for the turn marker: %v", err)
	}

	// Assert: the minted workspace's record is among the hits, and the standing
	// sink's pre-snapshot records are not — the offsets still apply.
	if !rt9AnyNaming(hits, Workspace{ID: "minted-ws"}) {
		t.Errorf("rt9Scan found no `shim.engine.turn` record for the workspace whose sink appeared after the "+
			"snapshot; a source the snapshot never saw must be read whole from zero. It found %d hit(s): %s",
			len(hits), rt9DescribeHits(hits))
	}
	if rt9AnyNaming(hits, Workspace{ID: "standing-ws"}) {
		t.Errorf("rt9Scan re-read the standing sink's pre-snapshot records, so the snapshot offsets are not "+
			"being applied and every earlier run's records would be reported as this run's. It found %d "+
			"hit(s): %s", len(hits), rt9DescribeHits(hits))
	}
}

// TestRt9AnswerDrawnAcceptsOnlyAWorkspacesLongEnoughDraw pins the acceptance
// rule the answer assertion now rests on, over a real `feed.draw-response`
// record read the way a sweep reads one.
//
// It drives no editor and spawns nothing: the record is written into a temp
// sink, scanned with the marker the run uses, and handed to the matcher. The
// rule is the whole of what changed when the marker moved off
// `feed.draw-text-block`, and a rule only a live sweep can exercise is a rule
// nothing checks between sweeps — which is how the 21-character assumption
// survived four of them.
func TestRt9AnswerDrawnAcceptsOnlyAWorkspacesLongEnoughDraw(t *testing.T) {
	want := len([]rune(rt9AnswerOpening))
	ws := Workspace{ID: "answering-ws"}

	cases := []struct {
		name        string
		workspaceID string
		context     map[string]any
		accepted    bool
	}{
		{
			name:        "the settled answer's own bubble",
			workspaceID: ws.ID,
			context:     map[string]any{"characters": want + 200, "blocks": 1, "settled": true},
			accepted:    true,
		},
		{
			name:        "a bubble caught exactly at the opening sentence",
			workspaceID: ws.ID,
			context:     map[string]any{"characters": want, "blocks": 1, "settled": false},
			accepted:    true,
		},
		{
			name:        "a bubble that has not yet grown to the opening sentence",
			workspaceID: ws.ID,
			context:     map[string]any{"characters": want - 1, "blocks": 1, "settled": false},
			accepted:    false,
		},
		{
			name:        "a draw stating no block, which is not a drawn bubble",
			workspaceID: ws.ID,
			context:     map[string]any{"characters": want + 200, "blocks": 0, "settled": true},
			accepted:    false,
		},
		{
			name:        "a draw whose `characters` the record does not carry",
			workspaceID: ws.ID,
			context:     map[string]any{"blocks": 1, "settled": true},
			accepted:    false,
		},
		{
			name:        "another workspace's answer, drawn in the same window",
			workspaceID: "someone-elses-ws",
			context:     map[string]any{"characters": want + 200, "blocks": 1, "settled": true},
			accepted:    false,
		},
	}

	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange: one `feed.draw-response` record in a sink of its own.
			root := t.TempDir()
			since := time.Now().Add(-time.Minute)
			path := filepath.Join(root, "webapp.log")
			context, err := json.Marshal(tc.context)
			if err != nil {
				t.Fatalf("render the record's context: %v", err)
			}
			line, err := json.Marshal(record{
				Timestamp: time.Now().Format(time.RFC3339Nano), Level: "INFO",
				Operation: "feed.draw-response", Message: "drew a response bubble settled",
				WorkspaceID: tc.workspaceID, Context: context,
			})
			if err != nil {
				t.Fatalf("render a log record: %v", err)
			}
			if err := os.WriteFile(path, append(line, '\n'), 0o644); err != nil {
				t.Fatalf("write %s: %v", path, err)
			}
			source := Source{Name: "workspace.webapp.log", Path: path, Kind: KindJSONL, Workspace: tc.workspaceID}

			// Act: read it the way the run's wait reads it.
			hits, err := rt9Scan([]Source{source}, TakeSnapshot(nil), nil, since, rt9FeedResponseRe)
			if err != nil {
				t.Fatalf("scan the sink for the response draw: %v", err)
			}
			if len(hits) != 1 {
				t.Fatalf("the marker matched %d record(s) in a sink holding one `feed.draw-response`: %s",
					len(hits), rt9DescribeHits(hits))
			}
			accepted := rt9AnswerDrawn(hits[0], ws, want)

			// Assert.
			if accepted != tc.accepted {
				t.Errorf("rt9AnswerDrawn accepted=%t for %s, want %t. The record was %s", accepted,
					rt9DescribeResponseDraws(hits, Workspace{ID: tc.workspaceID}), tc.accepted, line)
			}
		})
	}
}

// rt9DescribeHits renders a hit list for a failure message.
func rt9DescribeHits(hits []rt9Hit) string {
	if len(hits) == 0 {
		return "(none)"
	}
	parts := make([]string, 0, len(hits))
	for _, hit := range hits {
		parts = append(parts, fmt.Sprintf("%s in %s", hit.WorkspaceID, hit.Source.Path))
	}
	return strings.Join(parts, "; ")
}
