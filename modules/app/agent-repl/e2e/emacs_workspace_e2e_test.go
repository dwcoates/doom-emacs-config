// The Emacs client layer, AREA B — workspace lifecycle (EMACS-LAYER-SPEC.md
// scenarios 7-13).
//
// Every test here drives the REAL module through a REAL Doom in a REAL tty
// frame, and reads Emacs's own state back AS DATA — `agent-repl--workspaces',
// `agent-repl-host--by-name', `agent-repl-roster--tab-order',
// `agent-repl--ws-tabline-names',
// `agent-repl-held-ingress-waiting' — never the drawn sidebar, never a rendered tab
// string. The workspace verbs are the module's OWN leader bindings, which
// EMACS-LAYER-SPEC.md's "Which scenarios should assert a binding" section
// calls "the strongest case in the list", so each verb is reached the way a
// user reaches it:
//
//   - `SPC TAB p' (register) and `SPC TAB C-n' (create) PROMPT for their
//     arguments, so they are asserted with `LeaderBinding' — a lookup — and
//     then invoked as commands with their argument collection satisfied the
//     standard ERT way, by binding the readers for the duration of the one
//     call. A press would block the command loop on a prompt nobody can
//     answer, which this layer reports (correctly) as a WEDGE.
//   - `SPC j d' (close) and `SPC j x' (kill) take the CURRENT workspace, so
//     they are PRESSED with `Leader': real keymap lookup, real command.
//
// Git is the scripted fake git the world installs ahead of Emacs on PATH; no
// real git process runs anywhere. The vendor is the fake SDK, and it is the
// only writer of vendor files: nothing here writes a store row or a JSONL
// line.
//
// THE DAEMON IS NOT STARTED BY GO. Emacs starts it through the module's own
// launcher (`EnsureDaemon'), which is this layer's reason for existing; the
// Go-side cross-checks dial the address that launcher published.
package e2e

import (
	"context"
	"encoding/json"
	"fmt"
	"path/filepath"
	"slices"
	"testing"
	"time"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
	"agentrepl/protohelpers/rosterwalk"

	"connectrpc.com/connect"

	"agent-repld/integration/grantedclock"
	"agent-repld/integration/harness"
)

// ---------------------------------------------------------------------------
// BOUNDS
// ---------------------------------------------------------------------------
//
// PROVISIONAL, for the reason EMACS-LAYER-SPEC.md's "The bounds are still
// unmeasured" table gives: the image's Emacs 28.2 cannot boot interactive
// Doom, so no healthy run of this layer exists to measure against. Each is a
// NAMED constant with its reason attached, per SPEC.md §B "Waits", and each
// is to be tightened to a small multiple of the observed healthy maximum the
// moment an Emacs 29.1+ image produces one.

// emacsVerbBound bounds ONE workspace verb's full round trip: the unary call
// out of Emacs, the daemon's answer, the roster push back, and Emacs's own
// reconciliation of it into tabs. It is the suite's default because a verb is
// the same shape of work as every other awaited daemon answer here.
const emacsVerbBound = DefaultTimeout

// emacsTabOpenBound bounds a workspace's FIRST open being answered: the
// daemon answers a tab open only once the workspace's shim has spawned and its
// webapp page has drawn (open-tabs.el), so the wait spans a node-shim spawn and
// a WebKit page boot, not one verb's round trip. Measured 2026-10-17: alone
// 0.47-0.9s; inside the merge gate, which runs e2e-emacs beside the ERT suite,
// 0.77-2.77s over eleven awaits, and one at 5.25s whose page sat behind a
// logged 1038ms web-process main-thread stall. It is just over twice the
// gate's ordinary worst.
const emacsTabOpenBound = 6 * time.Second

// emacsTurnBound bounds a fake-SDK turn reaching a given roster arm. The fake
// vendor answers without network or model latency, so this is the daemon and
// shim plumbing plus one roster push, not a model call.
const emacsTurnBound = DefaultTimeout

// emacsWedgeProbeBound bounds the single post-teardown liveness probe
// scenario 13 makes. It is deliberately the SAME bound the heartbeat runs
// under: the claim is "Emacs still answers the command loop", and a slower
// answer than the heartbeat tolerates is already a missed heartbeat.
const emacsWedgeProbeBound = HeartbeatBound

// holdScenario parks a turn IN FLIGHT until an interrupt lands
// (`agent-shim/claude/shim/src/fake/scenarios/lifecycle.ts', scenario
// "hold"). It is how a scenario that must act "with work in flight" gets work
// in flight without racing a turn that would otherwise finish first.
const holdScenario = "!hold"

// ---------------------------------------------------------------------------
// THE AREA FIXTURE
// ---------------------------------------------------------------------------

// emacsWorkspaceFixture is one Emacs world with the daemon launched by Emacs
// and ONE workspace registered against a scripted fake-git worktree.
//
// Registration is the fixture rather than a step because six of this area's
// seven scenarios act ON a workspace; scenario 8's subject IS the
// registration, and it asserts the facts this fixture only establishes.
type emacsWorkspaceFixture struct {
	World *EmacsWorld
	Emacs *Emacs
	Repo  *harness.Repo

	// Name is the workspace name EMACS knows it by, read out of
	// `agent-repl--workspaces'.
	Name string
	// RefID is the DAEMON-MINTED WorkspaceRef id. Emacs never constructs
	// one; this is read back out of `agent-repl-host--by-name'.
	RefID string
}

func newEmacsWorkspaceFixture(t *testing.T, box sandbox) *emacsWorkspaceFixture {
	t.Helper()

	w := NewEmacsWorld(t, box)
	e := w.Emacs
	e.EnsureDaemon()

	repo := harness.NewRepoAt(t, filepath.Join(box.Scratch(), "repo"))

	// `SPC TAB p' prompts for the directory (`read-directory-name'), so
	// the binding is ASSERTED and the command is invoked with its argument.
	if want, got := "agent-repl-add-project-workspace", e.LeaderBinding("TAB p"); got != want {
		t.Fatalf("SPC TAB p resolves to %q, want %q", got, want)
	}
	e.Eval(`(agent-repl-add-project-workspace ` + elispString(repo.Dir) + `)`)

	raw := e.AwaitEvalFor(emacsVerbBound, "the workspace to appear in Emacs's registry",
		workspaceRegistryNamesForm,
		func(raw json.RawMessage) bool { return len(decodeStrings(raw)) == 1 })
	name := decodeStrings(raw)[0]

	f := &emacsWorkspaceFixture{World: w, Emacs: e, Repo: repo, Name: name}
	f.RefID = f.awaitRefID()
	awaitTabsOpen(e, []string{name})
	return f
}

// emacsTabOrderForm reads `agent-repl-roster--tab-order' as names: the tabs
// the daemon has answered OPEN (open-tabs.el), in roster walk order.
const emacsTabOrderForm = `(mapcar (lambda (n) (format "%s" n)) agent-repl-roster--tab-order)`

// awaitTabsOpen waits until every one of NAMES is in the tab order. A tab is
// held out of the order until the daemon answers its `OpenTabs' open
// (open-tabs.el), so a scenario that reads the order before that reads a
// workspace that is registered but not yet open.
func awaitTabsOpen(e *Emacs, names []string) []string {
	e.t.Helper()
	return decodeStrings(e.AwaitEvalFor(emacsTabOpenBound, fmt.Sprintf("the tabs %q to be answered open", names),
		emacsTabOrderForm,
		func(raw json.RawMessage) bool {
			order := decodeStrings(raw)
			for _, name := range names {
				if !containsString(order, name) {
					return false
				}
			}
			return true
		}))
}

// awaitTabOrder waits until the tab order is exactly WANT, every tab answered
// open and in that order. Having every name present is not enough after a
// reorder: the tabs were open in their old order before the reordering
// roster landed.
func awaitTabOrder(e *Emacs, want []string) []string {
	e.t.Helper()
	return decodeStrings(e.AwaitEvalFor(emacsTabOpenBound, fmt.Sprintf("the tab order to be %q", want),
		emacsTabOrderForm,
		func(raw json.RawMessage) bool {
			return slices.Equal(decodeStrings(raw), want)
		}))
}

// awaitRefID waits for the daemon-minted ref to land in Emacs's host table
// and answers its id.
func (f *emacsWorkspaceFixture) awaitRefID() string {
	f.Emacs.t.Helper()
	f.Emacs.AwaitEvalFor(emacsVerbBound, "the daemon-minted ref for the workspace",
		`(plist-get (agent-repl-host-ref `+elispString(f.Name)+`) :id)`,
		func(raw json.RawMessage) bool { return !isJSONNull(raw) })
	return f.Emacs.EvalString(`(plist-get (agent-repl-host-ref ` + elispString(f.Name) + `) :id)`)
}

// emacsWSTablineNamesForm reads the names the tab bar DRAWS, in roster order.
//
// NOT `tab-bar-tabs`: `status.el` paints the bar from `tab-bar-format`, so
// Emacs's built-in tabs are window configurations named after whatever buffer
// they hold (a `*magit: ...*` status buffer once a project switch has run, or
// `*agent-panel-input-repo*` once the panel is open) and carry no workspace
// name at all. `agent-repl--ws-tabline-names` is the enumeration the renderer
// itself walks — the drawn names, in roster order — so it is what "the tab
// bar's names" means in this module.
const emacsWSTablineNamesForm = `(agent-repl--ws-tabline-names)`

// tabNames reads the DRAWN tab names as DATA, never the rendered tab-bar
// string.
func (f *emacsWorkspaceFixture) tabNames() []string {
	f.Emacs.t.Helper()
	return f.Emacs.EvalStrings(emacsWSTablineNamesForm)
}

// rosterTabOrder reads `agent-repl-roster--tab-order', the roster walk order
// the tab bar follows strictly.
func (f *emacsWorkspaceFixture) rosterTabOrder() []string {
	f.Emacs.t.Helper()
	return f.Emacs.EvalStrings(emacsTabOrderForm)
}

// openPanel opens the agent-repl panel through the ordinary command and waits
// for its buffers to be displayed.
func (f *emacsWorkspaceFixture) openPanel() {
	f.Emacs.t.Helper()
	f.Emacs.Eval(`(agent-repl-frontend-open-panel)`)
	f.Emacs.AwaitEvalFor(emacsVerbBound, "the agent-repl panel windows to appear",
		`(mapcar (lambda (w) (buffer-name (window-buffer w))) (window-list))`,
		func(raw json.RawMessage) bool {
			for _, b := range decodeStrings(raw) {
				if hasAnyPrefix(b, "*agent-frontend-", "*agent-panel") {
					return true
				}
			}
			return false
		})
}

func hasAnyPrefix(s string, prefixes ...string) bool {
	for _, p := range prefixes {
		if len(s) >= len(p) && s[:len(p)] == p {
			return true
		}
	}
	return false
}

// submitFromComposer types TEXT into the composer and PRESSES RET, which is
// how a user submits. It is `emGHISubmit', so it first waits for the
// workspace's session to be up: opening the panel does not wait for the
// session, and the daemon refuses a submit to a workspace with none
// (`no_session'), which left a parked turn never in flight.
func (f *emacsWorkspaceFixture) submitFromComposer(text string) {
	f.Emacs.t.Helper()
	emGHISubmit(f.Emacs.t, f.Emacs, f.Name, text)
}

// awaitStatusArm waits for the workspace's roster arm to be one of ARMS.
// `agent-repl-roster--status-by-id' is the module's own arm table — the ONE
// source for tab coloring — so this reads the decision, not the paint.
func (f *emacsWorkspaceFixture) awaitStatusArm(what string, arms ...string) {
	f.Emacs.t.Helper()
	form := `(let ((arm (gethash ` + elispString(f.RefID) + ` agent-repl-roster--status-by-id)))
                (and arm (format "%s" arm)))`
	f.Emacs.AwaitEvalFor(emacsTurnBound, what, form, func(raw json.RawMessage) bool {
		var got string
		if isJSONNull(raw) || json.Unmarshal(raw, &got) != nil {
			return false
		}
		for _, arm := range arms {
			if got == arm {
				return true
			}
		}
		return false
	})
}

// parkAHeldTurn submits the fake SDK's `!hold' scenario and waits until the
// turn is actually in flight, so a scenario that must act "with work in
// flight" is synchronized on the work rather than racing it.
func (f *emacsWorkspaceFixture) parkAHeldTurn() {
	f.Emacs.t.Helper()
	f.openPanel()
	f.submitFromComposer(holdScenario)
	f.awaitStatusArm("the held turn to be in flight", ":submitting", ":thinking")
}

// releaseHeldWork kills the workspace's session so a parked `!hold' turn does
// not outlive the test. NOT an assertion — teardown hygiene, so a held turn
// cannot make the world's own shutdown wait on an interrupt that is never
// coming.
func (f *emacsWorkspaceFixture) releaseHeldWork() {
	f.Emacs.t.Helper()
	f.Emacs.Eval(`(ignore-errors ` + killWorkspaceForm(f.Name) + `)`)
}

// ---------------------------------------------------------------------------
// THE DAEMON-SIDE CROSS-CHECK
// ---------------------------------------------------------------------------

// awaitDaemonRoster dials the daemon AT THE ADDRESS EMACS'S OWN LAUNCHER
// PUBLISHED and waits for its roster to satisfy PRED.
//
// This exists so a green Emacs assertion cannot mean "Emacs dropped something
// the daemon also dropped": close is a VIEW act by contract, and the only way
// to say the daemon still holds the workspace is to ask the daemon.
func awaitDaemonRoster(t *testing.T, addr string, bound time.Duration, what string,
	pred func(*frontendv1.WorkspaceRoster) bool) *frontendv1.WorkspaceRoster {
	t.Helper()

	ctx, cancel := grantedclock.WithBound(context.Background(), bound)
	defer cancel()

	client := harness.DialAt(t, addr)
	stream, err := client.WatchWorkspaceRoster(ctx,
		connect.NewRequest(&agentreplv1.WatchWorkspaceRosterRequest{}))
	if err != nil {
		t.Fatalf("await %s: WatchWorkspaceRoster on the daemon Emacs launched: %v", what, err)
	}
	defer stream.Close()

	var last *frontendv1.WorkspaceRoster
	for stream.Receive() {
		last = stream.Msg().GetRoster()
		if pred(last) {
			return last
		}
	}
	t.Fatalf("await %s: never satisfied within %s; last roster was %v (stream error: %v)",
		what, bound, last, stream.Err())
	return nil
}

// rosterHasRefID answers whether the daemon's roster carries a row for ID,
// walking every grouping and every nested family. Rows are matched on the
// WorkspaceRef id — the join key — never on the display name, which is not
// unique across repositories.
func rosterHasRefID(r *frontendv1.WorkspaceRoster, id string) bool {
	found := false
	walkRosterRows(r, func(row *frontendv1.RosterRow) {
		if row.GetWorkspace().GetWorkspace().GetId() == id {
			found = true
		}
	})
	return found
}

func walkRosterRows(r *frontendv1.WorkspaceRoster, visit func(*frontendv1.RosterRow)) {
	for _, row := range rosterwalk.AllRows(r) {
		visit(row)
	}
}

// ---------------------------------------------------------------------------
// #7 — CreateWorkspaceAppearsOnTheRoster
// ---------------------------------------------------------------------------

func containsString(xs []string, want string) bool {
	for _, x := range xs {
		if x == want {
			return true
		}
	}
	return false
}

// ---------------------------------------------------------------------------
// #8 — RegisterAnExistingDirectory
// ---------------------------------------------------------------------------

// ---------------------------------------------------------------------------
// #9 — SelectOnWorkspaceSwitch
// ---------------------------------------------------------------------------

// ---------------------------------------------------------------------------
// #10 — CloseWorkspaceIsAViewAct
// ---------------------------------------------------------------------------

// ---------------------------------------------------------------------------
// #11 — CloseWithAHeldPromptDoesNotTearTheTabDown
// ---------------------------------------------------------------------------

// ---------------------------------------------------------------------------
// #12 — KillWorkspaceNeverBlocks
// ---------------------------------------------------------------------------

// ---------------------------------------------------------------------------
// #13 — CloseThenKillDoesNotWedgeEmacs
// ---------------------------------------------------------------------------

// ---------------------------------------------------------------------------
// #14 — NavigationFollowsTheDrawnTabBar
// ---------------------------------------------------------------------------

// standOn switches to WS through the module's own persp boundary and waits
// until Emacs agrees that is where it stands. It is the ARRANGE step for a
// navigation gesture, never the act under test: the acts below are keypresses.
func (f *emacsWorkspaceFixture) standOn(ws string) {
	f.Emacs.t.Helper()
	f.Emacs.Eval(`(progn (agent-repl--ws-switch ` + elispString(ws) + `) t)`)
	f.awaitCurrent(ws, "the arranged workspace to be current")
}

// awaitCurrent waits for Emacs's own current-workspace name to be WANT, and
// reports what it was instead when it never is.
func (f *emacsWorkspaceFixture) awaitCurrent(want, what string) {
	f.Emacs.t.Helper()
	f.Emacs.AwaitEvalFor(emacsVerbBound, what,
		`(format "%s" (agent-repl--ws-current-name))`,
		func(raw json.RawMessage) bool {
			var got string
			return json.Unmarshal(raw, &got) == nil && got == want
		})
}

// pressAndLand presses KEYS and asserts Emacs lands on WANT, naming the drawn
// bar in the failure so a wrong-direction cycle reads as one.
func (f *emacsWorkspaceFixture) pressAndLand(keys, want string, order []string) {
	f.Emacs.t.Helper()
	f.Emacs.Keys(keys)
	f.awaitCurrent(want, "`"+keys+"' to land on "+want+" of the drawn bar "+
		fmt.Sprint(order))
}
