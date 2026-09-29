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
//   - `SPC TAB C-n' (register) and `SPC TAB n' (create) PROMPT for their
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
	"testing"
	"time"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"connectrpc.com/connect"

	"claude-repld/integration/harness"
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

	// `SPC TAB C-n' prompts for the directory (`read-directory-name'), so
	// the binding is ASSERTED and the command is invoked with its argument.
	if want, got := "agent-repl-add-project-workspace", e.LeaderBinding("TAB C-n"); got != want {
		t.Fatalf("SPC TAB C-n resolves to %q, want %q", got, want)
	}
	e.Eval(`(agent-repl-add-project-workspace ` + elispString(repo.Dir) + `)`)

	raw := e.AwaitEvalFor(emacsVerbBound, "the workspace to appear in Emacs's registry",
		`(let (names) (maphash (lambda (k v) (when (plist-get v :project-dir) (push k names))) agent-repl--workspaces) names)`,
		func(raw json.RawMessage) bool { return len(decodeStrings(raw)) == 1 })
	name := decodeStrings(raw)[0]

	f := &emacsWorkspaceFixture{World: w, Emacs: e, Repo: repo, Name: name}
	f.RefID = f.awaitRefID()
	return f
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
	return f.Emacs.EvalStrings(`(mapcar (lambda (n) (format "%s" n)) agent-repl-roster--tab-order)`)
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

// inputBuffer answers the workspace's composer buffer name.
func (f *emacsWorkspaceFixture) inputBuffer() string {
	f.Emacs.t.Helper()
	return f.Emacs.EvalString(`(buffer-name (agent-repl--input-buffer ` + elispString(f.Name) + `))`)
}

// submitFromComposer types TEXT into the composer and PRESSES RET, which is
// how a user submits: `input.el' binds RET on `agent-repl-input-mode-map'
// through `map!', so the press asserts the binding and the command together.
func (f *emacsWorkspaceFixture) submitFromComposer(text string) {
	f.Emacs.t.Helper()
	buffer := f.inputBuffer()
	f.Emacs.Eval(`(with-current-buffer (agent-repl--input-buffer ` + elispString(f.Name) + `)
                    (erase-buffer)
                    (insert ` + elispString(text) + `)
                    t)`)
	if want, got := "agent-repl-send", f.Emacs.BindingForIn(buffer, "RET"); got != want {
		f.Emacs.t.Fatalf("composer RET resolves to %q, want %q", got, want)
	}
	f.Emacs.KeysIn(buffer, "RET")
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

	ctx, cancel := context.WithTimeout(context.Background(), bound)
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
	var walk func(rows []*frontendv1.RosterRow)
	walk = func(rows []*frontendv1.RosterRow) {
		for _, row := range rows {
			visit(row)
			walk(row.GetChildren())
		}
	}
	for _, s := range r.GetRepository().GetSections() {
		walk(s.GetRows().GetRows())
	}
	for _, s := range r.GetTask().GetSections() {
		walk(s.GetRows().GetRows())
	}
	walk(r.GetRecentlyMerged().GetRows().GetRows())
}

// ---------------------------------------------------------------------------
// #7 — CreateWorkspaceAppearsOnTheRoster
// ---------------------------------------------------------------------------

// TestEmacsCreateWorkspaceAppearsOnTheRoster is scenario 7: the DAEMON minted
// the identity and Emacs only reacted — a new row in
// `agent-repl-roster--rows-by-id', and the name in BOTH
// `agent-repl-roster--tab-order' and `agent-repl--ws-tabline-names'.
//
// `SPC TAB n' prompts three times (repository, initial prompt, name, base
// ref), so the binding is asserted as a LOOKUP and the command is then run
// with its readers bound for the duration of the one call — the standard ERT
// way, which keeps the command's own argument-collection code path.
func TestEmacsCreateWorkspaceAppearsOnTheRoster(t *testing.T) {
	t.Parallel()
	box := requireSandbox(t)
	f := newEmacsWorkspaceFixture(t, box)
	e := f.Emacs

	if want, got := "agent-repl-create-workspace", e.LeaderBinding("TAB n"); got != want {
		t.Fatalf("SPC TAB n resolves to %q, want %q", got, want)
	}

	before := len(f.rosterTabOrder())

	// The readers answer out of what the COMMAND offers them: the repository
	// picker takes the first candidate the command composed from the roster's
	// own sections, and every free-text reader answers blank except the
	// initial prompt — blank name and blank base ref are the documented
	// "daemon mints one" and "default branch" cases.
	e.Eval(`(cl-letf (((symbol-function 'completing-read)
                        (lambda (_prompt candidates &rest _)
                          (car (append candidates nil))))
                       ((symbol-function 'read-string)
                        (lambda (prompt &rest _)
                          (if (string-prefix-p "Initial prompt" prompt) "hello" ""))))
               (call-interactively #'agent-repl-create-workspace)
               t)`)

	e.AwaitEvalFor(emacsVerbBound, "the created workspace's row to reach the roster table",
		`(hash-table-count agent-repl-roster--rows-by-id)`,
		func(raw json.RawMessage) bool {
			var n int
			return !isJSONNull(raw) && json.Unmarshal(raw, &n) == nil && n > before
		})

	order := f.rosterTabOrder()
	if len(order) <= before {
		t.Fatalf("agent-repl-roster--tab-order = %v, want more than the %d it started with", order, before)
	}

	// The tab bar follows the roster's order strictly, so every name the
	// roster walk produced must have a tab.
	tabs := f.tabNames()
	for _, name := range order {
		if !containsString(tabs, name) {
			t.Fatalf("agent-repl--ws-tabline-names = %v, want a tab for the roster-ordered workspace %q", tabs, name)
		}
	}
}

// TestEmacsANewProjectDoesNotRecycleTheWorkspaceBeingLeft is the other half of
// scenario 7: a created workspace keeps its tab when the NEXT project is
// registered.
//
// THE DEFECT THIS PINS. Doom's `+workspaces-switch-to-project-h` recycles the
// workspace being LEFT -- `+workspace-rename` onto the entered project's name
// -- whenever `+workspaces-on-switch-project-behavior` is its `non-empty`
// default and `+workspace-buffer-list` is empty. An agent-repl workspace
// showing its agent IS empty by that test, because the panel is a webview
// buffer and deliberately not a `doom-real-buffer-list` member. So the
// abandoned workspace's persp left `persp-names-cache` under its old name
// while `agent-repl--workspaces` and the daemon's roster both kept carrying
// it -- and since `agent-repl--ws-tabline-names` INTERSECTS those two, its tab
// silently vanished from a bar the roster still said it belonged on.
//
// Every readback here is one the scenarios above already make; what is new is
// only that they are made about the FIRST workspace AFTER a second arrived.
func TestEmacsANewProjectDoesNotRecycleTheWorkspaceBeingLeft(t *testing.T) {
	t.Parallel()
	box := requireSandbox(t)
	f := newEmacsWorkspaceFixture(t, box)
	e := f.Emacs

	// Assert first, in the LIVE Doom, that the policy is actually installed.
	// `agent-repl--ws-install-persp-policy` runs from a `with-eval-after-load`
	// on persp-mode, so a unit test can prove what the function writes but not
	// that anything ever called it; this is the half only a booted Doom can
	// say. And it is the value Doom's own hook branches on, read back by its
	// own name rather than inferred from what the tab bar happened to draw.
	if got := e.EvalString(`(format "%s" +workspaces-on-switch-project-behavior)`); got != "t" {
		t.Fatalf("+workspaces-on-switch-project-behavior is %q in the running Doom, want t: at any other "+
			"value `+workspaces-switch-to-project-h` may RECYCLE the workspace being left, renaming its "+
			"perspective out from under a registry and a roster that both still carry it", got)
	}

	// Arrange: a created workspace, which the create SELECTS -- so it is the
	// one a second registration would leave.
	before := len(f.rosterTabOrder())
	e.Eval(`(cl-letf (((symbol-function 'completing-read)
                        (lambda (_prompt candidates &rest _)
                          (car (append candidates nil))))
                       ((symbol-function 'read-string)
                        (lambda (prompt &rest _)
                          (if (string-prefix-p "Initial prompt" prompt) "hello" ""))))
               (call-interactively #'agent-repl-create-workspace)
               t)`)
	e.AwaitEvalFor(emacsVerbBound, "the created workspace's tab to be drawn",
		emacsWSTablineNamesForm,
		func(raw json.RawMessage) bool { return len(decodeStrings(raw)) > before })
	created := ""
	for _, name := range f.tabNames() {
		if name != f.Name {
			created = name
		}
	}
	if created == "" {
		t.Fatalf("agent-repl--ws-tabline-names = %v, want a tab the fixture's own %q is not",
			f.tabNames(), f.Name)
	}

	// Act: a SECOND repository registered, which switches projects out of the
	// created workspace.
	second := harness.NewRepoAt(t, filepath.Join(box.Scratch(), "repo-second"))
	secondName := addProjectWorkspace(t, e, second.Dir)

	// Assert: the created workspace still has its perspective, and therefore
	// still has its tab. The perspective cache is read TOO, because it is the
	// half the intersection loses and a tab count alone would not say which
	// half moved.
	e.AwaitEvalFor(emacsVerbBound, "the second workspace's own tab to be drawn",
		emacsWSTablineNamesForm,
		func(raw json.RawMessage) bool { return containsString(decodeStrings(raw), secondName) })
	if tabs := f.tabNames(); !containsString(tabs, created) {
		persps := e.EvalStrings(`(if (boundp 'persp-names-cache) persp-names-cache nil)`)
		t.Fatalf("agent-repl--ws-tabline-names = %v after registering %s; the created workspace %q lost "+
			"its tab, and the perspective cache is %v -- Doom recycled the workspace being left instead "+
			"of making the new project its own",
			tabs, second.Dir, created, persps)
	}
	if persps := e.EvalStrings(`(if (boundp 'persp-names-cache) persp-names-cache nil)`); !containsString(persps, created) {
		t.Fatalf("persp-names-cache = %v, want the created workspace %q still in it: its perspective was "+
			"renamed out from under the registry, which still carries the name", persps, created)
	}
}

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

// TestEmacsRegisterAnExistingDirectory is scenario 8: `SPC TAB C-n' hands the
// daemon a DIRECTORY and the daemon MINTS the identity.
//
// The claim that matters is the second half: `agent-repl-host--by-name' holds
// a ref whose `:id' EMACS NEVER CONSTRUCTED. Register is one of Emacs's only
// two workspace verbs, and everything else about the workspace the daemon
// derives and pushes.
func TestEmacsRegisterAnExistingDirectory(t *testing.T) {
	t.Parallel()
	box := requireSandbox(t)
	f := newEmacsWorkspaceFixture(t, box)

	if !f.Emacs.EvalBool(`(agent-repl--ws-known-p ` + elispString(f.Name) + `)`) {
		t.Fatalf("agent-repl--workspaces has no entry for %q after registering %s", f.Name, f.Repo.Dir)
	}
	if f.RefID == "" {
		t.Fatalf("agent-repl-host--by-name holds no daemon-minted ref id for %q", f.Name)
	}

	// The daemon minted it, so the daemon must know it. Asking the daemon is
	// the only way to say "Emacs did not invent this id".
	awaitDaemonRoster(t, f.Emacs.DaemonAddr(), emacsVerbBound,
		"the daemon's own roster to carry the ref it minted",
		func(r *frontendv1.WorkspaceRoster) bool { return rosterHasRefID(r, f.RefID) })
}

// ---------------------------------------------------------------------------
// #9 — SelectOnWorkspaceSwitch
// ---------------------------------------------------------------------------

// TestEmacsSelectOnWorkspaceSwitch is scenario 9: switching is one of Emacs's
// only two inputs to the roster, and the proof it happened is
// `agent-repl-host-last-selected-id' — which host.el records ONLY ON THE ACK,
// so its value is what the DAEMON stamped as current, never what Emacs hoped.
func TestEmacsSelectOnWorkspaceSwitch(t *testing.T) {
	t.Parallel()
	box := requireSandbox(t)
	f := newEmacsWorkspaceFixture(t, box)
	e := f.Emacs

	// A SWITCH IS A CHANGE OF PERSPECTIVE. Select rides persp-mode's
	// activation hook, so re-switching to the workspace registration already
	// made current activates nothing and owes no Select. The fixture leaves
	// exactly one workspace standing, so a SECOND one is what makes the switch
	// under test a real switch rather than a no-op.
	other := harness.NewRepoAt(t, filepath.Join(box.Scratch(), "other-repo"))
	e.Eval(`(agent-repl-add-project-workspace ` + elispString(other.Dir) + `)`)
	e.AwaitEvalFor(emacsVerbBound, "Emacs to stand on the second workspace",
		`(format "%s" (agent-repl--ws-current-name))`,
		func(raw json.RawMessage) bool {
			var got string
			return !isJSONNull(raw) && json.Unmarshal(raw, &got) == nil && got != f.Name
		})

	// Clear it only now: both registrations switched, and a stale value would
	// make this vacuous.
	e.Eval(`(setq agent-repl-host-last-selected-id nil)`)
	e.Eval(`(agent-repl-switch-to-project ` + elispString(f.Repo.Dir) + `)`)

	// The switch must actually land, or the Select assertion below would be
	// waiting on an act that was never performed.
	e.AwaitEvalFor(emacsVerbBound, "the first workspace to become current again",
		`(format "%s" (agent-repl--ws-current-name))`,
		func(raw json.RawMessage) bool {
			var got string
			return !isJSONNull(raw) && json.Unmarshal(raw, &got) == nil && got == f.Name
		})

	e.AwaitEvalFor(emacsVerbBound, "the daemon to ack Emacs's select",
		`(and agent-repl-host-last-selected-id (format "%s" agent-repl-host-last-selected-id))`,
		func(raw json.RawMessage) bool {
			var got string
			return !isJSONNull(raw) && json.Unmarshal(raw, &got) == nil && got == f.RefID
		})
}

// TestEmacsRegisteringAWorkspaceMakesItTheDaemonsCurrentRow is scenario 9's
// other half, and it is the half nothing covered.
//
// Scenario 9 above drives a SWITCH to a workspace that was already
// registered, so its perspective activation finds a daemon-minted ref and the
// Select goes out. A REGISTRATION is the opposite order: Doom switches to the
// new perspective and the daemon answers with the ref afterwards, so that
// activation found no ref and skipped. Nothing came back for it, and the
// consequence was end to end: the daemon never stamped `current`, so every
// `RosterRow.current` it resolved was false and NO sidebar row in any webview
// was ever drawn as the selected one.
//
// THE ASSERTION IS ON THE ROW'S OWN FLAG, not on Emacs's `last-selected-id`.
// The id says Emacs got an ack; the row's flag is the field the webapp turns
// into `data-current` (`webapp/src/sidebar/row.ts`), so it is the one that
// says the selection actually reaches what draws it.
//
// COST: one fixture world and one roster dial, which is what every scenario
// in this file already pays; it adds no wait the file does not already make.
func TestEmacsRegisteringAWorkspaceMakesItTheDaemonsCurrentRow(t *testing.T) {
	t.Parallel()
	box := requireSandbox(t)
	f := newEmacsWorkspaceFixture(t, box)

	// NOTHING IS SWITCHED TO. The fixture registers and stops, which is
	// exactly the state the defect lived in: the workspace Emacs is standing
	// in, freshly registered, and never switched to since.
	if got := f.Emacs.EvalString(`(format "%s" (agent-repl--ws-current-name))`); got != f.Name {
		t.Fatalf("the current workspace is %q, want the freshly registered %q", got, f.Name)
	}

	awaitDaemonRoster(t, f.Emacs.DaemonAddr(), emacsVerbBound,
		"the daemon's roster to mark the registered workspace's row as the current one",
		func(r *frontendv1.WorkspaceRoster) bool { return rosterCurrentRowRefID(r) == f.RefID })
}

// rosterCurrentRowRefID answers the ref id of the row the daemon marked as
// the selected one, or the empty string when no row carries the flag.
func rosterCurrentRowRefID(r *frontendv1.WorkspaceRoster) string {
	id := ""
	walkRosterRows(r, func(row *frontendv1.RosterRow) {
		if row.GetCurrent().GetCurrent() {
			id = row.GetWorkspace().GetWorkspace().GetId()
		}
	})
	return id
}

// ---------------------------------------------------------------------------
// #10 — CloseWorkspaceIsAViewAct
// ---------------------------------------------------------------------------

// TestEmacsCloseWorkspaceIsAViewAct is scenario 10. Close takes the CURRENT
// workspace, so it is PRESSED as `SPC j d' rather than called.
//
// Two halves, and both are needed: Emacs drops the tab and the host entry,
// AND the daemon still holds the workspace. "A view act" is exactly the
// difference between the two, and only the second half can say it.
func TestEmacsCloseWorkspaceIsAViewAct(t *testing.T) {
	t.Parallel()
	box := requireSandbox(t)
	f := newEmacsWorkspaceFixture(t, box)
	e := f.Emacs

	if want, got := "agent-repl-close-workspace", e.LeaderBinding("j d"); got != want {
		t.Fatalf("SPC j d resolves to %q, want %q", got, want)
	}
	if got := e.EvalString(`(format "%s" (agent-repl--ws-current-name))`); got != f.Name {
		t.Fatalf("the current workspace is %q, want %q: `SPC j d' acts on the current one", got, f.Name)
	}

	e.Leader("j d")

	e.AwaitEvalFor(emacsVerbBound, "the closed workspace's tab to go away",
		emacsWSTablineNamesForm,
		func(raw json.RawMessage) bool { return !containsString(decodeStrings(raw), f.Name) })

	// The DAEMON-side session is untouched: its roster still carries the row.
	awaitDaemonRoster(t, e.DaemonAddr(), emacsVerbBound,
		"the daemon to still hold the closed workspace",
		func(r *frontendv1.WorkspaceRoster) bool { return rosterHasRefID(r, f.RefID) })
}

// ---------------------------------------------------------------------------
// #11 — CloseWithAHeldPromptDoesNotTearTheTabDown
// ---------------------------------------------------------------------------

// TestEmacsCloseWithAHeldPromptDoesNotTearTheTabDown is scenario 11.
//
// What Emacs OWES here is precisely to NOT ACT. Per elisp.md the refusal
// manifests in the WEBAPP FOOTER, not in an Emacs dialog, so the assertion is
// negative: the tab is still there.
//
// The held prompt is the DAEMON's (owner ruling 2026-09-28: Emacs holds no
// prompt in memory). `SPC j RET` submits the deferral AT ONCE with
// `:delivery :deferred` and the daemon holds it in its tray. So the half of
// this scenario that says undelivered intent is never silently discarded is
// read at the edges Emacs can see: the deferral reached the RPC boundary
// deferred, the daemon answered it `:success` (took it) before the close was
// asked for, and nothing waits in the on-disk ingress — the one place Emacs
// itself would hold a prompt the daemon did not take. The daemon's tray is
// not read: this fixture exposes no daemon client.
//
// The turn is parked with the fake SDK's `!hold' scenario so the close is
// genuinely refused rather than merely slow: the deferral is held against a
// turn that is actually in flight.
func TestEmacsCloseWithAHeldPromptDoesNotTearTheTabDown(t *testing.T) {
	t.Parallel()
	box := requireSandbox(t)
	f := newEmacsWorkspaceFixture(t, box)
	e := f.Emacs
	t.Cleanup(f.releaseHeldWork)

	// ARRANGE: a parked turn, then the deferral through its own ordinary
	// command. The observers are armed after the parked turn's send, so they
	// see only the deferral.
	f.parkAHeldTurn()
	armSubmissionObserver(t, e)
	armSubmissionAnswerObserver(t, e)

	const deferred = "the deferred prompt"
	e.Eval(`(with-current-buffer (agent-repl--input-buffer ` + elispString(f.Name) + `)
               (erase-buffer)
               (insert ` + elispString(deferred) + `)
               t)`)
	e.Eval(`(agent-repl-queue-deferred-prompt)`)

	// Check it landed with the daemon BEFORE asking for the close: a
	// deferral the daemon never took would make the close's refusal say
	// nothing about held work.
	sent := awaitSubmissions(t, e, 1, "the deferral to reach the RPC boundary")[0]
	if sent.Text != deferred || sent.Delivery != ":deferred" {
		t.Fatalf("the deferral was submitted as text %q delivery %q, want %q delivered :deferred",
			sent.Text, sent.Delivery, deferred)
	}
	if arm := awaitSubmissionAnswer(t, e, deferred, "the daemon to answer the deferral"); arm != ":success" {
		t.Fatalf("the daemon answered the deferral %s, want :success: the daemon holds it", arm)
	}

	// ACT
	e.Leader("j d")

	// ASSERT: the tab SURVIVES. Waiting for the close's own round trip to
	// land first is what makes this a real assertion rather than a race with
	// it: the refusal is echoed to the echo area, so the message is the edge.
	e.AwaitEvalFor(emacsVerbBound, "the blocked close to be answered",
		`(with-current-buffer "*Messages*"
                   (and (string-match-p "close blocked" (buffer-string)) t))`,
		func(raw json.RawMessage) bool { return !isJSONNull(raw) })

	if tabs := f.tabNames(); !containsString(tabs, f.Name) {
		t.Fatalf("agent-repl--ws-tabline-names = %v, want %q still present: a blocked close draws no dialog and leaves the tab in place", tabs, f.Name)
	}
	// ASSERT: nothing waits on disk. The daemon took the deferral, so the
	// ingress — Emacs's only holding place — is empty.
	if n := e.EvalInt(`(agent-repl-held-ingress-waiting ` + elispString(f.Name) + `)`); n != 0 {
		t.Fatalf("%d of %q's prompts wait in the held-prompt ingress, want none: the daemon took the deferral", n, f.Name)
	}
}

// ---------------------------------------------------------------------------
// #12 — KillWorkspaceNeverBlocks
// ---------------------------------------------------------------------------

// TestEmacsKillWorkspaceNeverBlocks is scenario 12: kill is FORCED, and takes
// no refusal path even with work in flight. The `!hold' turn is parked first
// precisely so "even with work in flight" is a fact of the test rather than a
// hope about its timing.
func TestEmacsKillWorkspaceNeverBlocks(t *testing.T) {
	t.Parallel()
	box := requireSandbox(t)
	f := newEmacsWorkspaceFixture(t, box)
	e := f.Emacs

	if want, got := "agent-repl-kill-workspace", e.LeaderBinding("j x"); got != want {
		t.Fatalf("SPC j x resolves to %q, want %q", got, want)
	}

	f.parkAHeldTurn()

	// The kill asks first (owner ruling: `SPC j x' confirms with y/n), and the
	// user answers yes.
	e.LeaderAnsweringYes(killConfirmPrompt, "j x")

	e.AwaitEvalFor(emacsVerbBound, "the killed workspace's tab to go away",
		emacsWSTablineNamesForm,
		func(raw json.RawMessage) bool { return !containsString(decodeStrings(raw), f.Name) })

	// NO refusal path was taken. The blocked-close message is the one this
	// verb must never produce, and reading it is the sanctioned rendered
	// exception: the refusal IS echo-area text by contract.
	if e.EvalBool(`(with-current-buffer "*Messages*"
                          (and (string-match-p "kill blocked" (buffer-string)) t))`) {
		t.Fatal("*Messages* carries a kill refusal: kill is forced and never blocks")
	}
}

// ---------------------------------------------------------------------------
// #13 — CloseThenKillDoesNotWedgeEmacs
// ---------------------------------------------------------------------------

// TestEmacsCloseThenKillDoesNotWedgeEmacs is scenario 13, and THE HEARTBEAT
// IS THE ASSERTION.
//
// This pins the sentinel/kill-buffer recursion named in EMACS-LAYER-SPEC.md's
// opening: closing a workspace re-entered a process sentinel that killed a
// buffer that ran the sentinel again. There is no frame for it — it manifests
// only as an Emacs that stops answering — so the two verbs are run back to
// back on a workspace with a LIVE PANEL, which is the configuration the
// recursion needed, and the test then asks Emacs a question it must answer.
//
// The heartbeat goroutine fails this test immediately on a missed probe, with
// the last form sent plus *Messages* and a backtrace in the artifacts
// directory; the explicit probe below is the same claim asked once more after
// the verbs have settled, so the test cannot pass by finishing before the
// heartbeat noticed.
//
// THE KILL NAMES ITS WORKSPACE INSTEAD OF BEING PICKED FOR. `SPC j x' with no
// argument resolves its target through `agent-repl-verbs--target-ws', which
// reads the LIVE registry — and the close that just ran tombstones this name
// the moment the daemon's roster push lands with the row `closed'. Which side
// of that push the kill falls on is the daemon's schedule, not this
// scenario's subject: measured on 2026-09-04 the picker was still holding the
// name when the layer ran one scenario at a time, and had already lost it
// when five ran at once, so the keystroke form made the verb refuse with "No
// agent-repl workspaces registered" instead of running. Naming the target
// makes BOTH verbs run on every schedule, which is what "both commands back
// to back" (EMACS-LAYER-SPEC.md scenario 13) asks for; that `SPC j x' is this
// command is scenario 12's assertion, made again here so the keystroke path
// is not lost.
func TestEmacsCloseThenKillDoesNotWedgeEmacs(t *testing.T) {
	t.Parallel()
	box := requireSandbox(t)
	f := newEmacsWorkspaceFixture(t, box)
	e := f.Emacs

	if want, got := "agent-repl-kill-workspace", e.LeaderBinding("j x"); got != want {
		t.Fatalf("SPC j x resolves to %q, want %q", got, want)
	}

	f.openPanel()

	e.Leader("j d")
	e.Eval(killWorkspaceForm(f.Name))

	e.AwaitEvalFor(emacsWedgeProbeBound, "emacs to still answer its command loop after close-then-kill",
		`(emacs-pid)`,
		func(raw json.RawMessage) bool { return !isJSONNull(raw) })

	// The SAME Emacs answered — not a replacement that came up behind a
	// process that died and restarted.
	if pid := e.EvalInt(`(emacs-pid)`); pid != e.Doom.PID {
		t.Fatalf("emacs-pid = %d, want the pid the readiness stamp carried (%d)", pid, e.Doom.PID)
	}
}

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

// TestEmacsWorkspaceNavigationFollowsTheDrawnTabBar is the navigation
// scenario, and the DRAWN ORDER IS THE WHOLE ASSERTION.
//
// THE DEFECT THIS PINS. `s-{' / `s-}' counted along
// `agent-repl--live-ws-names' — the registry hash's KEY ORDER — while the bar
// is drawn from `agent-repl-roster-tab-order'. The two are different lists:
// measured on the owner's live Emacs the registry read (main, chess960,
// explanation-engine, first-kept, none) against a bar drawing (first-kept,
// explanation-engine, chess960). So left went right, and off one end the
// switch reached persp-mode's own `none' (an error) or Doom's `main' (the
// splash screen, no tab highlighted). `M-<n>' fell through to Doom's
// `+workspace/switch-to-(n-1)', which indexes persp-mode's perspective list
// with `main' at slot 0, so `M-1' landed on the splash screen and `M-2' on
// the first tab.
//
// Only a booted Doom can say this: both halves are about which keymap wins
// and which list the winner walks, and a unit test can prove what our own
// command does but not that Doom's binding no longer answers the chord.
func TestEmacsWorkspaceNavigationFollowsTheDrawnTabBar(t *testing.T) {
	t.Parallel()
	box := requireSandbox(t)
	f := newEmacsWorkspaceFixture(t, box)
	e := f.Emacs

	// Arrange: three workspaces, so a direction is observable (with two, left
	// and right land on the same tab and a reversed cycle still passes).
	second := harness.NewRepoAt(t, filepath.Join(box.Scratch(), "repo-second"))
	secondName := addProjectWorkspace(t, e, second.Dir)
	third := harness.NewRepoAt(t, filepath.Join(box.Scratch(), "repo-third"))
	thirdName := addProjectWorkspace(t, e, third.Dir)

	e.AwaitEvalFor(emacsVerbBound, "all three workspaces to be drawn on the tab bar",
		emacsWSTablineNamesForm,
		func(raw json.RawMessage) bool {
			tabs := decodeStrings(raw)
			return containsString(tabs, f.Name) &&
				containsString(tabs, secondName) &&
				containsString(tabs, thirdName)
		})

	// The order under test is the one the BAR is drawn from, read as data.
	order := f.rosterTabOrder()
	if len(order) != 3 {
		t.Fatalf("agent-repl-roster--tab-order = %v, want exactly the three registered workspaces "+
			"(%q, %q, %q)", order, f.Name, secondName, thirdName)
	}
	for _, name := range order {
		if !containsString(f.tabNames(), name) {
			t.Fatalf("agent-repl--ws-tabline-names = %v, want a tab for the roster-ordered workspace %q",
				f.tabNames(), name)
		}
	}

	// The chords are the module's, not Doom's. `M-1' is the load-bearing one:
	// Doom binds it to `+workspace/switch-to-0' in `global-map', and our
	// minor-mode map has to win that lookup.
	for keys, want := range map[string]string{
		"s-}": "agent-repl-switch-right",
		"s-{": "agent-repl-switch-left",
		"M-1": "agent-repl-switch-to-workspace-1",
		"M-2": "agent-repl-switch-to-workspace-2",
	} {
		if got := e.BindingFor(keys); got != want {
			t.Fatalf("%s resolves to %q in the running Doom, want %q: Doom's own numeral "+
				"indexes persp-mode's perspective list, whose slot 0 is its `main'", keys, got, want)
		}
	}

	// Act + assert: `s-}' walks the bar left to right, wrapping.
	f.standOn(order[0])
	f.pressAndLand("s-}", order[1], order)
	f.pressAndLand("s-}", order[2], order)
	f.pressAndLand("s-}", order[0], order)

	// And `s-{' walks it right to left, wrapping.
	f.standOn(order[2])
	f.pressAndLand("s-{", order[1], order)
	f.pressAndLand("s-{", order[0], order)
	f.pressAndLand("s-{", order[2], order)

	// `M-2' is the SECOND DRAWN TAB, which is what Doom's own `M-2' was not.
	f.standOn(order[0])
	f.pressAndLand("M-2", order[1], order)

	// `M-1' is the FIRST DRAWN TAB and never Doom's `main': that pseudo
	// perspective is where the old binding landed, and it is read back BY ITS
	// OWN NAME rather than inferred from the tab that ended up highlighted.
	f.pressAndLand("M-1", order[0], order)
	main := e.EvalString(`(format "%s" (agent-repl--ws-main-name))`)
	if current := e.EvalString(`(format "%s" (agent-repl--ws-current-name))`); current == main {
		t.Fatalf("`M-1' landed on %q, Doom's own startup perspective: the numerals index the drawn "+
			"tab bar %v, which a pseudo perspective can never appear in", current, order)
	}
}
