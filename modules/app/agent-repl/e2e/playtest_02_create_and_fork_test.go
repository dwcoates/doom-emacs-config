//go:build playtest

package e2e

import (
	"encoding/json"
	"fmt"
	"strings"
	"testing"

	frontendv1 "agentrepl/proto/frontend/v1"
)

// OWNER 2 of PLAYTEST-PLAN.md's partition: A4-A6 -- adding a project from a
// directory, creating a workspace and a child of the current one, and
// forking a workspace with its conversation.
//
// THE THREE PLAYBOOKS ARE THREE WORLDS. Each takes its own Emacs on its own
// Xvfb with its own daemon, because a playbook shares nothing with another
// (PLAYTEST-PLAN.md, "Parallelism").
//
// WHAT THE PLAN CALLS "NESTING IN TAB NAMES" IS ORDER, NOT SPELLING. The
// module's own rule is `agent-repl-roster--tab-name`: a tab is named after
// the row's display name and carries NO parent marker at all -- the only
// decoration it can gain is a `·<repo label>` suffix, and that is a
// COLLISION disambiguator between repositories, never a parentage one.
// Nesting is expressed by the roster walk instead: `agent-repl-roster-walk`
// walks a section DEPTH-FIRST, so a child's tab stands IMMEDIATELY AFTER its
// parent's. A.5 therefore asserts the ORDER and says so in its manifest,
// rather than asserting a prefix the product does not write. Whether the
// product SHOULD write one is an open question for the lead; nothing here
// adds one.

// The slugs the daemon derives from these prompts are what the created
// workspaces are named after, and each is distinct so a new tab can be told
// from every other by its name alone.
//
// EACH IS EXACTLY THREE WORDS, which is `workspace.SlugWordLimit`: the
// naming rule keeps at most three, so a longer prompt would be truncated and
// the expectation below would be a guess rather than the rule.
const (
	playtestCreatePrompt = "sketch the outline"
	playtestChildPrompt  = "carve the alcove"
	playtestForkPrompt   = "trace the lantern"
)

// The slug each of those prompts becomes. The daemon may still PREFIX the
// name (`AGENT_WORKSPACE_PREFIX`, the "DWC/" convention), so a playbook
// asserts CONTAINMENT and reads the actual name off the new tab -- it never
// predicts the whole name.
const (
	playtestCreateSlug = "sketch-the-outline"
	playtestChildSlug  = "carve-the-alcove"
	playtestForkSlug   = "trace-the-lantern"
)

// playtestParentPrompt is the plain-prose turn A.6's PARENT runs before it is
// forked, and it is what the fork's own feed must then carry. It carries no
// `!` prefix, so the fake SDK answers it with its default prose scenario.
const playtestParentPrompt = "recount the harbor lantern story for the playtest"

// playtestParentEcho is the ANSWER the fake SDK gives that prompt: its prose
// scenario's conclusion is `echo: <prompt> [mode=...] [model=...]`
// (agent-shim/claude/shim/src/fake/scenarios/prose.ts), so this prefix
// identifies the PARENT's own settled response wherever it is drawn -- and it
// is drawn on the FORK's feed, which is what the fork's readback measures.
const playtestParentEcho = "echo: " + playtestParentPrompt

// ---------------------------------------------------------------------------
// SHARED READBACKS
// ---------------------------------------------------------------------------

// playtestRosterTabOrder reads `agent-repl-roster--tab-order', the roster
// walk order the tab bar follows strictly. It is the same readback
// `emacsWorkspaceFixture.rosterTabOrder` makes; a playbook has no fixture, so
// it is spelled once here.
func playtestRosterTabOrder(s *playtestScenario) []string {
	s.E.t.Helper()
	return s.E.EvalStrings(`(mapcar (lambda (n) (format "%s" n)) agent-repl-roster--tab-order)`)
}

// playtestRefID waits for the daemon-minted ref to land in Emacs's host table
// for WS and answers its id -- the same two-step `awaitRefID` makes: wait for
// non-nil, then read.
//
// The id is what a roster row is matched on. Names collide across
// repositories and ids do not, so every daemon-side cross-check below joins
// on this.
func playtestRefID(t *testing.T, s *playtestScenario, ws string) string {
	t.Helper()
	form := `(plist-get (agent-repl-host-ref ` + elispString(ws) + `) :id)`
	s.E.AwaitEvalFor(emacsVerbBound, "the daemon-minted ref id for "+ws, form,
		func(raw json.RawMessage) bool { return !isJSONNull(raw) })
	return s.E.EvalString(form)
}

// playtestRefDir answers WS's worktree directory off the daemon-minted ref.
//
// `WorkspaceRef.dir` is the ref's second field and is documented as "for
// display and for opening files" -- which is exactly what
// `agent-repl-switch-to-project` takes, since that command's own docstring
// says its argument is a PROJECT ROOT PATH and not a workspace name.
func playtestRefDir(t *testing.T, s *playtestScenario, ws string) string {
	t.Helper()
	dir := s.E.EvalString(`(plist-get (agent-repl-host-ref ` + elispString(ws) + `) :dir)`)
	if dir == "" {
		t.Fatalf("the daemon-minted ref for %q carries no :dir, so there is no path to switch to", ws)
	}
	return dir
}

// playtestCurrentName answers the workspace Emacs is standing on.
func playtestCurrentName(s *playtestScenario) string {
	s.E.t.Helper()
	return s.E.EvalString(`(format "%s" (agent-repl--ws-current-name))`)
}

// playtestAwaitCurrent waits until WS is the selected workspace.
func playtestAwaitCurrent(t *testing.T, s *playtestScenario, ws string) {
	t.Helper()
	s.E.AwaitEvalFor(emacsVerbBound, "the selected workspace to be "+ws,
		`(format "%s" (agent-repl--ws-current-name))`,
		func(raw json.RawMessage) bool { return decodeString(raw) == ws })
}

// playtestStandOn makes WS the current workspace, switching only when it is
// not already.
//
// IT ANSWERS WHETHER A SWITCH WAS NEEDED, and that answer is the playbook's
// evidence about whether creating a workspace SELECTS it in Emacs. Per
// `agent-repl-verb-create`'s own docstring nothing happens on success and the
// tab arrives through the roster push, so this reads the product rather than
// assuming either way.
func playtestStandOn(t *testing.T, s *playtestScenario, ws string) (switched bool) {
	t.Helper()
	if playtestCurrentName(s) == ws {
		return false
	}
	s.E.Eval(`(agent-repl-switch-to-project ` + elispString(playtestRefDir(t, s, ws)) + `)`)
	playtestAwaitCurrent(t, s, ws)
	return true
}

// playtestNewTabName answers the single name in AFTER that is not in BEFORE.
//
// A created workspace's name is the DAEMON'S, derived from the prompt by the
// naming rule and possibly prefixed, so a playbook READS it rather than
// predicting it. More than one new name means something else was created
// alongside, which would make every assertion below ambiguous.
func playtestNewTabName(t *testing.T, before, after []string) string {
	t.Helper()
	var fresh []string
	for _, name := range after {
		if !containsString(before, name) {
			fresh = append(fresh, name)
		}
	}
	if len(fresh) != 1 {
		t.Fatalf("the tab bar went from %v to %v, want exactly one new tab; new names were %v",
			before, after, fresh)
	}
	return fresh[0]
}

// playtestAwaitNewTab waits until the tab bar carries exactly one name it did
// not carry BEFORE, and answers it -- ON A SETTLED FRAME.
//
// Both `agent-repl-create-workspace` and `agent-repl-fork-workspace` pass
// `:select t`, so the minted workspace LANDS on its own panel, and the tab's
// arrival is the first step of that landing rather than its last (see
// landing_test.go). Every create and fork in every playbook reads its new
// name through here, so waiting the landing out here is what keeps the step
// after it from racing a panel show.
func playtestAwaitNewTab(t *testing.T, s *playtestScenario, before []string, what string) string {
	t.Helper()
	s.E.AwaitEvalFor(emacsVerbBound, what, emacsWSTablineNamesForm,
		func(raw json.RawMessage) bool { return len(decodeStrings(raw)) == len(before)+1 })
	name := playtestNewTabName(t, before, s.tabNames())
	awaitMintedLanding(s.E, name)
	return name
}

// playtestCreateReaders runs BODY with the two prompting readers
// `agent-repl-create-workspace` and `agent-repl-fork-workspace` use bound for
// the duration of the one call -- the standard ERT way, which keeps the
// command's OWN argument-collection code path rather than bypassing it.
//
// The repository picker takes the FIRST candidate the command composed from
// the roster's own sections. Every free-text reader answers blank except the
// initial prompt: blank name and blank base ref are the documented "the
// daemon mints one" and "the repository's default branch" cases, and blank is
// ABSENCE by `agent-repl-verbs--optional-string`'s own rule.
func playtestCreateReaders(prompt, body string) string {
	return `(cl-letf (((symbol-function 'completing-read)
                        (lambda (_prompt candidates &rest _)
                          (car (append candidates nil))))
                       ((symbol-function 'read-string)
                        (lambda (prompt &rest _)
                          (if (string-prefix-p "Initial prompt" prompt) ` + elispString(prompt) + ` ""))))
               ` + body + `
               t)`
}

// ---------------------------------------------------------------------------
// THE DAEMON-SIDE ORDER AND NESTING CROSS-CHECKS
// ---------------------------------------------------------------------------

// playtestOpenRowIDs answers the daemon roster's OPEN rows' ids in the order
// `agent-repl-roster-walk` walks them: each repository section's rows
// depth-first, then the recently-merged section's.
//
// It is the daemon's own order, read off the daemon's own roster, so
// comparing the tab bar against it says the tab bar FOLLOWS the roster rather
// than that two Emacs-side variables agree with each other.
//
// Closed rows are dropped because `agent-repl-roster-desired-tabs` drops
// them: "only rows with `closed = false' get a tab" is the whole membership
// rule, so including them here would compare two different sets.
func playtestOpenRowIDs(r *frontendv1.WorkspaceRoster) []string {
	var ids []string
	var walk func(rows []*frontendv1.RosterRow)
	walk = func(rows []*frontendv1.RosterRow) {
		for _, row := range rows {
			if !row.GetClosed().GetClosed() {
				ids = append(ids, row.GetWorkspace().GetWorkspace().GetId())
			}
			walk(row.GetChildren())
		}
	}
	for _, section := range r.GetRepository().GetSections() {
		walk(section.GetRows().GetRows())
	}
	walk(r.GetRecentlyMerged().GetRows().GetRows())
	return ids
}

// playtestRowChildIDs answers the ids of PARENT's own children in the daemon
// roster, or false when no row carries that id at all.
func playtestRowChildIDs(r *frontendv1.WorkspaceRoster, parentID string) ([]string, bool) {
	var children []string
	found := false
	walkRosterRows(r, func(row *frontendv1.RosterRow) {
		if row.GetWorkspace().GetWorkspace().GetId() != parentID {
			return
		}
		found = true
		children = nil
		for _, child := range row.GetChildren() {
			children = append(children, child.GetWorkspace().GetWorkspace().GetId())
		}
	})
	return children, found
}

// playtestTabIDs maps the drawn tab names to the daemon-minted ref ids behind
// them, in tab order, so the tab bar can be compared to the daemon's roster
// on the JOIN KEY rather than on display names -- which are not unique across
// repositories.
func playtestTabIDs(t *testing.T, s *playtestScenario, names []string) []string {
	t.Helper()
	ids := make([]string, 0, len(names))
	for _, name := range names {
		ids = append(ids, playtestRefID(t, s, name))
	}
	return ids
}

// playtestAwaitSelectedPanel waits until the SELECTED window shows one of
// WS's OWN panel buffers -- its webview or its composer -- and answers the
// buffer's name and major mode.
//
// WHAT THIS GUARDS. A create switches to the minted worktree through
// projectile, and Doom then asks `+workspaces-switch-project-function` what
// the new perspective should show. The module answers that question in
// `agent-repl--ws-switch-project-display`: a workspace ALREADY OWNS ITS
// DISPLAY, because the persp activation drains put its panel in the main
// area. The panel's webview is deliberately not a `doom-real-buffer-list`
// member, so an answer that did not recognize the workspace fell through to
// the empty-project magit fallback and left magit status sitting where the
// agent belongs -- which is the defect this readback fails on, by name.
func playtestAwaitSelectedPanel(t *testing.T, s *playtestScenario, ws string) (string, string) {
	t.Helper()
	e := s.E
	webview := e.EvalString(`(agent-repl--frontend-webview-buffer-name ` + elispString(ws) + `)`)
	input := awaitInputBuffer(t, e, ws)
	e.AwaitEvalFor(panelSettleBound,
		fmt.Sprintf("the selected window to show %q's own panel", ws),
		playtestSelectedBufferForm,
		func(raw json.RawMessage) bool {
			got := decodeString(raw)
			return got == webview || got == input
		})
	name := e.EvalString(playtestSelectedBufferForm)
	mode := e.EvalString(`(format "%s" (buffer-local-value 'major-mode (window-buffer (selected-window))))`)
	return name, mode
}

// ---------------------------------------------------------------------------
// A.4 -- ADD PROJECT FROM DIRECTORY
// ---------------------------------------------------------------------------

// TestPlaytestAddProjectFromDirectory is plan A.4: a second repository
// registered through `SPC TAB C-n`, a second tab, and the tab bar in the
// roster's own order.
//
// The visual subject is the TAB BAR -- two named tabs with the second one
// highlighted -- which is section A's subject and a surface no Connect-dialing
// test can see at all.
func TestPlaytestAddProjectFromDirectory(t *testing.T) {
	t.Parallel()
	s := newPlaytestScenario(t, "02-add-project",
		"Plan A.4. Two repositories registered from directories through `SPC TAB C-n`, and a tab "+
			"bar carrying both in the daemon roster's own depth-first order.")
	p := s.Book

	first := s.repoAt(t, "repo-first")
	firstName := s.register(t, first.Dir)
	s.openPanel(t)
	p.note("the first repository registered with `SPC TAB C-n` and its panel opened",
		fmt.Sprintf("the composer buffer for %q exists and the webapp drew its footer against this daemon", firstName))

	second := s.repoAt(t, "repo-second")
	secondName := s.register(t, second.Dir)

	// REGISTERING SELECTS, which is one of Emacs's only two inputs to the
	// roster, so the assertion is on the module's own current-workspace
	// accessor -- and it is made BEFORE the picture, because the picture's
	// sentence says which tab is highlighted.
	playtestAwaitCurrent(t, s, secondName)

	// AND WHAT IT SELECTED COMES UP ON ITS OWN PANEL, NOT ON MAGIT.
	// Registering a workspace is the same "you just made a workspace"
	// behavior as creating one (A.5 asserts the create half with this same
	// readback), so the main area must carry the new workspace's agent. A
	// magit status buffer here is the register having switched to a
	// directory the daemon had not accepted yet: no workspace at the path,
	// no landing panels armed, and Doom's empty-project fallback filling the
	// frame instead.
	selected, mode := playtestAwaitSelectedPanel(t, s, secondName)
	if strings.HasPrefix(mode, "magit-") {
		t.Fatalf("registering %q left the selected window on %q in %s: a directory the daemon just "+
			"accepted must come up on its OWN PANEL, not on magit status", secondName, selected, mode)
	}
	p.note("`SPC TAB C-n` answered and Emacs was asked what it is standing on",
		fmt.Sprintf("`agent-repl--ws-current-name` is %q and the selected window shows that workspace's "+
			"own panel %q: the register SELECTED the workspace it minted and landed on its agent",
			secondName, selected))

	names := s.tabNames()
	if len(names) != 2 || names[0] != firstName || names[1] != secondName {
		t.Fatalf("the tab bar draws %v, want exactly [%s %s]", names, firstName, secondName)
	}

	// THE TAB BAR FOLLOWS THE ROSTER, and both halves of that are checked.
	// First against Emacs's own walk order, which is what the renderer reads.
	if order := playtestRosterTabOrder(s); len(order) != len(names) {
		t.Fatalf("agent-repl-roster--tab-order = %v and the drawn names are %v: they must be the same walk", order, names)
	} else {
		for i, name := range names {
			if order[i] != name {
				t.Fatalf("the tab bar draws %v but the roster walk is %v: the bar does not follow the roster", names, order)
			}
		}
	}

	// And then against the DAEMON'S own roster, joined on the ref id, which
	// is what says the walk order is the daemon's rather than a second
	// Emacs-side variable agreeing with the first.
	wantIDs := playtestTabIDs(t, s, names)
	roster := awaitDaemonRoster(t, s.E.DaemonAddr(), emacsVerbBound,
		"the daemon's roster to carry both registered workspaces",
		func(r *frontendv1.WorkspaceRoster) bool {
			for _, id := range wantIDs {
				if !rosterHasRefID(r, id) {
					return false
				}
			}
			return true
		})
	gotIDs := playtestOpenRowIDs(roster)
	if strings.Join(gotIDs, ",") != strings.Join(wantIDs, ",") {
		t.Fatalf("the daemon's depth-first open rows are %v and the tab bar's are %v: the tab order is not the roster walk",
			gotIDs, wantIDs)
	}
	p.note("the daemon asked for its own roster at the address Emacs's launcher published",
		fmt.Sprintf("both refs are on the daemon's roster and its depth-first walk of open rows is exactly the tab bar's order (%v)", gotIDs))

	// A WORKSPACE NOTHING HAS BEEN WIRED TO IS `:none`. Registering mints an
	// identity; it does not bring a session up, which the first submit does.
	s.awaitArm(t, secondName, "the second workspace's tab arm to be published", playtestUnwiredArm)
	s.captureArm(t, "two-tabs", secondName,
		"a second repository registered from its directory through `agent-repl-add-project-workspace` (`SPC TAB C-n`)",
		playtestUnwiredArm,
		fmt.Sprintf("The tab bar must carry EXACTLY TWO workspace tabs, %q first and %q second, in that "+
			"order, and the SECOND one must be the HIGHLIGHTED one -- registering selects it, and "+
			"`agent-repl--ws-current-name` reads %q.", firstName, secondName, secondName))
}

// ---------------------------------------------------------------------------
// A.5 -- NEW WORKSPACE AND A CHILD OF THE CURRENT ONE
// ---------------------------------------------------------------------------

// TestPlaytestNewWorkspaceAndChild is plan A.5: `SPC TAB n` creates a
// workspace the daemon names from the prompt, and `C-u SPC TAB n` creates a
// CHILD of the one Emacs is standing on.
//
// WHAT "NESTING" LOOKS LIKE HERE. The plan's line says "nesting in tab
// names", but `agent-repl-roster--tab-name` writes no parent marker of any
// kind -- see this file's header. The nesting the product actually expresses
// is ORDER: `agent-repl-roster-walk` is depth-first, so the child's tab
// stands immediately after its parent's. That is what is asserted and what
// the manifest sentence says.
func TestPlaytestNewWorkspaceAndChild(t *testing.T) {
	t.Parallel()
	s := newPlaytestScenario(t, "02-new-and-child",
		"Plan A.5. `SPC TAB n` creates a workspace the daemon names from the initial prompt AND "+
			"stands on it, and `C-u SPC TAB n` creates a CHILD of the current one, whose row nests "+
			"under its parent's and whose tab stands immediately after it.")
	p, e := s.Book, s.E

	repository := s.repoAt(t, "repo")
	rootName := s.register(t, repository.Dir)
	s.openPanel(t)
	p.note("one repository registered with `SPC TAB C-n` and its panel opened",
		fmt.Sprintf("the composer buffer for %q exists and the webapp drew its footer against this daemon", rootName))

	// `SPC TAB n` PROMPTS, so the binding is a LOOKUP and the command is then
	// invoked with its readers bound. A press would block the command loop on
	// a prompt nobody can answer, which this layer reports (correctly) as a
	// wedge.
	if want, got := "agent-repl-create-workspace", e.LeaderBinding("TAB n"); got != want {
		t.Fatalf("SPC TAB n resolves to %q, want %q", got, want)
	}

	// ---- the plain create ------------------------------------------------
	beforeCreate := s.tabNames()
	e.Eval(playtestCreateReaders(playtestCreatePrompt, `(call-interactively #'agent-repl-create-workspace)`))
	createdName := playtestAwaitNewTab(t, s, beforeCreate, "the created workspace's tab to arrive on the roster push")
	if !strings.Contains(createdName, playtestCreateSlug) {
		t.Fatalf("the created workspace is named %q, want a name carrying the slug %q the naming rule derives from %q",
			createdName, playtestCreateSlug, playtestCreatePrompt)
	}
	createdID := playtestRefID(t, s, createdName)
	awaitDaemonRoster(t, e.DaemonAddr(), emacsVerbBound,
		"the daemon's roster to carry the created workspace",
		func(r *frontendv1.WorkspaceRoster) bool { return rosterHasRefID(r, createdID) })

	// THE CREATE CARRIED AN INITIAL PROMPT, so a turn is running on the fake
	// SDK. The arm is waited to a SETTLED one and then RE-READ, because the
	// manifest sentence must state the arm the product is on at the instant
	// of the picture rather than a guess at it -- which is what `armPaint`
	// exists for.
	s.awaitArm(t, createdName, "the created workspace's initial turn to settle", emGHISettledArms...)
	createdArm, _ := s.armPaint(t, createdName)
	s.captureArm(t, "created-tab", createdName,
		fmt.Sprintf("`agent-repl-create-workspace` (`SPC TAB n`) with the initial prompt %q", playtestCreatePrompt),
		createdArm,
		fmt.Sprintf("The tab bar must carry the new tab %q BESIDE the registered %q -- the daemon named it "+
			"from the initial prompt, so the name carries %q.", createdName, rootName, playtestCreateSlug))

	// ---- standing on the parent ------------------------------------------
	//
	// CREATING A WORKSPACE SELECTS IT, and that is now ASSERTED rather than
	// read off the product and narrated. `agent-repl-create-workspace` passes
	// `:select t` to `agent-repl-verb-create`, which stands on the dir
	// `CreateWorkspaceSuccess` minted through `agent-repl-switch-to-project`
	// -- the same step `agent-repl-add-project-workspace` takes after
	// registering a directory.
	//
	// THE WAIT IS NOT A COURTESY. The switch is deferred onto a timer inside
	// `agent-repl-switch-to-project` so the perspective change completes and
	// Emacs redraws before any blocking I/O, so the selection lands strictly
	// after the create's answer rather than inside it.
	playtestAwaitCurrent(t, s, createdName)
	// AND NO SWITCH WAS NEEDED TO GET THERE: standing on it is a no-op, which
	// is what distinguishes "the create selected it" from "this playbook
	// selected it".
	if switched := playtestStandOn(t, s, createdName); switched {
		t.Fatalf("creating %q left Emacs standing somewhere else: `SPC TAB n` must SELECT the workspace it "+
			"just made, the way registering a directory does", createdName)
	}
	// AND WHAT IT IS STANDING ON IS THE AGENT, NOT MAGIT. Selecting the
	// created workspace must land the main area on that workspace's own
	// panel; a magit status buffer there is the switch-project fallback
	// having overwritten the panel.
	selected, mode := playtestAwaitSelectedPanel(t, s, createdName)
	if strings.HasPrefix(mode, "magit-") {
		t.Fatalf("creating %q left the selected window on %q in %s: the workspace it just made must come up "+
			"on its OWN PANEL, not on magit status", createdName, selected, mode)
	}
	p.note("`SPC TAB n` answered and Emacs was asked what it is standing on",
		fmt.Sprintf("`agent-repl--ws-current-name` is %q with no switch performed and the selected window "+
			"shows that workspace's own panel %q: the create SELECTED the workspace it made and landed on "+
			"its agent, so the child create below reads the right parent", createdName, selected))

	// ---- the child create ------------------------------------------------
	beforeChild := s.tabNames()
	e.Eval(playtestCreateReaders(playtestChildPrompt,
		`(let ((current-prefix-arg '(4))) (call-interactively #'agent-repl-create-workspace))`))
	childName := playtestAwaitNewTab(t, s, beforeChild, "the child workspace's tab to arrive on the roster push")
	if !strings.Contains(childName, playtestChildSlug) {
		t.Fatalf("the child workspace is named %q, want a name carrying the slug %q the naming rule derives from %q",
			childName, playtestChildSlug, playtestChildPrompt)
	}
	childID := playtestRefID(t, s, childName)

	// THE NESTING IS THE DAEMON'S OWN, so the daemon is what is asked: the
	// parent's row must carry the child's id among its children.
	awaitDaemonRoster(t, e.DaemonAddr(), emacsVerbBound,
		"the daemon's roster to nest the child under its parent",
		func(r *frontendv1.WorkspaceRoster) bool {
			children, found := playtestRowChildIDs(r, createdID)
			return found && containsString(children, childID)
		})
	p.note("`C-u SPC TAB n` run with the current workspace being the one just created",
		fmt.Sprintf("the daemon's roster row for %q carries %q among its children: the child is nested under its parent",
			createdName, childName))

	// AND THE ORDER IS THE NESTING'S VISIBLE FORM. The walk is depth-first, so
	// the child's tab stands IMMEDIATELY after its parent's.
	tabs := s.tabNames()
	parentAt, childAt := -1, -1
	for i, name := range tabs {
		switch name {
		case createdName:
			parentAt = i
		case childName:
			childAt = i
		}
	}
	if parentAt < 0 || childAt < 0 {
		t.Fatalf("the tab bar draws %v, want both %q and %q on it", tabs, createdName, childName)
	}
	if childAt != parentAt+1 {
		t.Fatalf("the tab bar draws %v: %q is at %d and its parent %q at %d, want the child IMMEDIATELY after its parent -- the roster walk is depth-first",
			tabs, childName, childAt, createdName, parentAt)
	}

	// The child's create carried an initial prompt too, so the same settle
	// and re-read applies.
	s.awaitArm(t, childName, "the child workspace's initial turn to settle", emGHISettledArms...)
	childArm, _ := s.armPaint(t, childName)
	s.captureArm(t, "child-tab", childName,
		fmt.Sprintf("`C-u SPC TAB n` (`agent-repl-create-workspace` with a prefix argument) with the initial prompt %q", playtestChildPrompt),
		childArm,
		fmt.Sprintf("The tab bar must carry %q IMMEDIATELY AFTER %q -- that adjacency is how this product "+
			"expresses the child's nesting. THE NAME CARRIES NO NESTING MARKER: "+
			"`agent-repl-roster--tab-name` writes the row's display name and nothing else, so %q must "+
			"NOT be drawn with a parent prefix, an indent, or any other parentage decoration. The full "+
			"drawn order is %v.", childName, createdName, childName, tabs))
}

// ---------------------------------------------------------------------------
// A.6 -- FORK A WORKSPACE AND ITS CONVERSATION
// ---------------------------------------------------------------------------

// TestPlaytestForkWorkspaceAndConversation is plan A.6: a workspace with a
// conversation in it is forked through `SPC TAB f`, and the fork's own feed
// carries the parent's history.
//
// THE PARENT MUST HAVE SOMETHING TO INHERIT, so a plain-prose turn is run and
// SETTLED first. A fork taken off an empty workspace would satisfy every
// structural assertion here and prove nothing about the transcript.
func TestPlaytestForkWorkspaceAndConversation(t *testing.T) {
	t.Parallel()
	s := newPlaytestScenario(t, "02-fork",
		"Plan A.6. A workspace with a settled conversation is forked through `SPC TAB f`; the fork's "+
			"row nests under its parent's, its tab stands immediately after its parent's, and its own "+
			"feed comes up live. The plan also asked for the PARENT's history in that feed, and the "+
			"product does not put it there -- see the step that reads the fork's feed.")
	p, e := s.Book, s.E

	repository := s.repoAt(t, "repo")
	parentName := s.register(t, repository.Dir)
	s.openPanel(t)

	// ---- the parent's conversation ---------------------------------------
	s.submit(t, playtestParentPrompt)
	s.awaitInPage(t, "the parent's own prompt bubble to arrive on the standing tail",
		`document.querySelector('[data-feed-row][data-row-kind="userPrompt"]')`)
	s.awaitInPage(t, "the parent's response bubble to settle on the standing tail",
		`document.querySelector('[data-feed-row][data-row-kind="activity"][data-unit="response"][data-state="success"]')`)
	s.awaitArm(t, parentName, "the parent's turn to settle", emGHISettledArms...)
	p.note(fmt.Sprintf("a plain-prose prompt submitted into %q with composer RET", parentName),
		"the parent's feed carries its own prompt bubble and a SETTLED response bubble, so there is a "+
			"transcript for the fork to inherit")

	// ---- the fork --------------------------------------------------------
	//
	// `SPC TAB f` prompts for the repository and the initial prompt, so it is
	// asserted as a LOOKUP and then invoked with its readers bound. The parent
	// is the CURRENT workspace by construction -- the command reads
	// `agent-repl--ws-current-name` itself -- so the selection is asserted
	// before the call rather than hoped for.
	if want, got := "agent-repl-fork-workspace", e.LeaderBinding("TAB f"); got != want {
		t.Fatalf("SPC TAB f resolves to %q, want %q", got, want)
	}
	playtestAwaitCurrent(t, s, parentName)
	parentID := playtestRefID(t, s, parentName)

	beforeFork := s.tabNames()
	e.Eval(playtestCreateReaders(playtestForkPrompt, `(call-interactively #'agent-repl-fork-workspace)`))
	forkName := playtestAwaitNewTab(t, s, beforeFork, "the forked workspace's tab to arrive on the roster push")
	if !strings.Contains(forkName, playtestForkSlug) {
		t.Fatalf("the forked workspace is named %q, want a name carrying the slug %q the naming rule derives from %q",
			forkName, playtestForkSlug, playtestForkPrompt)
	}
	forkID := playtestRefID(t, s, forkName)

	// A FORK IS A CHILD BY CONSTRUCTION -- `agent-repl-verb-create` refuses a
	// fork without a parent before it issues any rpc -- so the daemon's roster
	// must nest it under the parent it was taken from.
	awaitDaemonRoster(t, e.DaemonAddr(), emacsVerbBound,
		"the daemon's roster to nest the fork under the workspace it was forked from",
		func(r *frontendv1.WorkspaceRoster) bool {
			children, found := playtestRowChildIDs(r, parentID)
			return found && containsString(children, forkID)
		})
	p.note(fmt.Sprintf("`SPC TAB f` run on %q with the initial prompt %q", parentName, playtestForkPrompt),
		fmt.Sprintf("the daemon's roster row for %q carries %q among its children", parentName, forkName))

	// ---- the fork's own feed ---------------------------------------------
	//
	// The playbook re-points at the fork: `openPanel` re-derives the composer
	// buffer and waits for the fork's OWN page to mount, which is what makes
	// every `awaitInPage` below a question about the fork's feed rather than
	// about the parent's.
	playtestStandOn(t, s, forkName)
	s.Name = forkName
	s.openPanel(t)

	// THE FORK IS LIVE AND ANSWERING IN ITS OWN RIGHT: its initial prompt
	// bubble and a SETTLED response are on its own standing tail. That is the
	// claim this playbook can make about the fork's feed, and it is asserted
	// before the picture is taken.
	s.awaitInPage(t, "the fork's own initial prompt bubble to be on its feed",
		`Array.prototype.some.call(
                   document.querySelectorAll('[data-feed-row][data-row-kind="userPrompt"]'),
                   function (row) { return row.textContent.indexOf(`+jsString(playtestForkPrompt)+`) !== -1; })`)
	s.awaitInPage(t, "the fork's own response bubble to settle on its feed",
		`document.querySelector('[data-feed-row][data-row-kind="activity"][data-unit="response"][data-state="success"]')`)
	s.awaitArm(t, forkName, "the fork's own initial turn to settle", emGHISettledArms...)

	// THE PARENT'S WHOLE TURN IS THERE, ABOVE THE FORK'S OWN, and both facts
	// are measured rather than narrated.
	//
	// PLAYTEST-PLAN.md A.6 asks for "parent history in feed". MEASURED here:
	// the fork's feed carries the parent's QUESTION and the parent's settled
	// ANSWER ("echo: recount the harbor lantern story ...", the fake SDK's own
	// conclusion for the parent's prompt), in that order, and both stand above
	// the fork's own first prompt bubble.
	//
	// A FORK PORTS THE WHOLE CONVERSATION UNDER THE CHILD'S IDENTITIES. The
	// vendor JSONL transcript is ported into the child's config root under a
	// fresh vendor session id and its assistant lines reach the feed through
	// the store; the prompt bubbles come from the daemon's own per-workspace
	// conversation rows, and those are ported too — re-minted under the same
	// mapping, so the child's questions and its ported answers name one another
	// exactly as the parent's did. The feed's ordering key is the plane a row
	// was drawn in, so the ported conversation stands above the rows the fork
	// draws for itself however the two arrive.
	//
	// THE ORDER IS ASSERTED, not merely the presences: earlier runs of this
	// playbook (out-run7 against out-run8) recorded the parent's rows and the
	// fork's own changing places between runs, and a presence-only assertion
	// would go on passing on the day that came back.
	forkRows, parentPromptAt, parentAnswerAt, forkPromptAt := s.forkFeedFacts(t)
	if parentPromptAt < 0 {
		t.Fatalf("the fork's feed draws %d rows and none of them is the parent's prompt %q: a fork ports the "+
			"WHOLE conversation, so the parent's question belongs on the child's feed", forkRows, playtestParentPrompt)
	}
	if parentAnswerAt < 0 {
		t.Fatalf("the fork's feed draws %d rows and none of them is the parent's answer %q: the ported vendor "+
			"transcript reaches the child's feed through the store", forkRows, playtestParentEcho)
	}
	if forkPromptAt < 0 {
		t.Fatalf("the fork's feed draws %d rows and none of them is the fork's own prompt %q", forkRows, playtestForkPrompt)
	}
	if !(parentPromptAt < parentAnswerAt && parentAnswerAt < forkPromptAt) {
		t.Fatalf("the fork's feed draws the parent's prompt at %d, the parent's answer at %d and the fork's own "+
			"prompt at %d over %d rows: want the parent's whole turn, in order, ABOVE the fork's own",
			parentPromptAt, parentAnswerAt, forkPromptAt, forkRows)
	}
	p.note("the fork's own feed read for the PARENT's turn",
		fmt.Sprintf("the fork's feed carries %d rows: the parent's QUESTION %q is at row %d, the parent's settled "+
			"ANSWER %q at row %d, and the fork's OWN prompt %q at row %d -- the fork ported the whole "+
			"conversation under its own identities, so the parent's turn reads above the fork's",
			forkRows, playtestParentPrompt, parentPromptAt, playtestParentEcho, parentAnswerAt,
			playtestForkPrompt, forkPromptAt))

	// THE FORK'S TAB FOLLOWS ITS PARENT'S, which is the same depth-first
	// nesting A.5 asserts. It is a FUNCTIONAL assertion with a note rather
	// than part of the picture's sentence below: the picture's subject is the
	// FEED, and the panel fills the frame at the size every capture is taken
	// at, so a sentence about the tab bar would send a reviewer looking for a
	// surface this particular frame does not carry.
	tabs := s.tabNames()
	parentAt, forkAt := -1, -1
	for i, name := range tabs {
		switch name {
		case parentName:
			parentAt = i
		case forkName:
			forkAt = i
		}
	}
	if parentAt < 0 || forkAt < 0 {
		t.Fatalf("the tab bar draws %v, want both %q and %q on it", tabs, parentName, forkName)
	}
	if forkAt != parentAt+1 {
		t.Fatalf("the tab bar draws %v: %q is at %d and its parent %q at %d, want the fork IMMEDIATELY after its parent -- the roster walk is depth-first",
			tabs, forkName, forkAt, parentName, parentAt)
	}
	p.note("the tab bar read back after the fork arrived",
		fmt.Sprintf("`agent-repl--ws-tabline-names` is %v: %q stands immediately after its parent %q, and its "+
			"name carries NO nesting marker -- nesting is order here, not spelling", tabs, forkName, parentName))

	p.capture("forked-feed", "the fork made current and its panel opened",
		fmt.Sprintf("the fork's OWN prompt bubble (%q) and a SETTLED response bubble are on its standing tail, "+
			"and its roster arm has settled", playtestForkPrompt),
		fmt.Sprintf("The panel shows the FORK's conversation, and it opens with the PARENT's: a prompt bubble "+
			"reading %q with the parent's own settled answer (%q) beneath it, and only then the fork's own "+
			"prompt bubble (%q) with its own prose response (%q). The sidebar draws %q nested under its "+
			"parent %q. The plan's A.6 asked for the parent's history in the feed and this is it, WHOLE and "+
			"IN ORDER: a fork ports the parent's vendor transcript AND the daemon's own prompt rows, both "+
			"re-minted under one mapping, so the question and the answer that belong together arrive "+
			"together and stand above everything the fork has of its own.",
			playtestParentPrompt, playtestParentEcho, playtestForkPrompt, "echo: "+playtestForkPrompt,
			forkName, parentName))
}

// forkFeedFacts answers how many rows the current workspace's feed is drawing
// and WHERE on it three sentences sit: the PARENT's own prompt bubble, the
// PARENT's own settled answer, and the FORK's own prompt bubble. A sentence on
// no row answers -1.
//
// THE FOUR FACTS TRAVEL TOGETHER because they are read in ONE probe: asking
// twice would let the page change between the count the note states and the
// order the assertions make, and a note that disagreed with its own assertion
// is exactly the kind of evidence a reviewer cannot use.
//
// POSITIONS RATHER THAN PRESENCES. A fork ports the WHOLE conversation under
// the child's identities, so the parent's question and the parent's answer are
// both on the fork's feed and both stand ABOVE the fork's own first question.
// Presence alone would go on passing on the day that order came apart, and
// this playbook's own earlier runs are what recorded it coming apart.
func (s *playtestScenario) forkFeedFacts(t *testing.T) (rows, parentPromptAt, parentAnswerAt, forkPromptAt int) {
	t.Helper()
	s.E.Eval(`(setq agent-repl-playtest--js nil)`)
	script := `(function () {
                     var all = document.querySelectorAll('[data-feed-row]');
                     function at(selector, needle) {
                       var rows = document.querySelectorAll(selector);
                       for (var i = 0; i < all.length; i++) {
                         for (var j = 0; j < rows.length; j++) {
                           if (all[i] === rows[j] && all[i].textContent.indexOf(needle) !== -1) return i;
                         }
                       }
                       return -1;
                     }
                     var prompt = at('[data-feed-row][data-row-kind="userPrompt"]', ` + jsString(playtestParentPrompt) + `);
                     var answer = at('[data-feed-row]', ` + jsString(playtestParentEcho) + `);
                     var own = at('[data-feed-row][data-row-kind="userPrompt"]', ` + jsString(playtestForkPrompt) + `);
                     return all.length + ":" + prompt + ":" + answer + ":" + own;
                   })()`
	raw := s.E.AwaitEvalFor(playtestPageBound, "the fork's feed row count and where the parent's turn sits on it",
		`(agent-repl-playtest--probe `+elispString(s.Name)+` `+elispString(script)+`)`,
		func(raw json.RawMessage) bool { return strings.Count(decodeString(raw), ":") == 3 })
	answer := decodeString(raw)
	if _, err := fmt.Sscanf(answer, "%d:%d:%d:%d", &rows, &parentPromptAt, &parentAnswerAt, &forkPromptAt); err != nil {
		t.Fatalf("the page answered %q for its feed facts, want \"<count>:<parent-prompt>:<parent-answer>:<fork-prompt>\": %v", answer, err)
	}
	return rows, parentPromptAt, parentAnswerAt, forkPromptAt
}
