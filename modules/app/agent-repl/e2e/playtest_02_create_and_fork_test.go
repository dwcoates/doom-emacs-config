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
// not carry BEFORE, and answers it.
func playtestAwaitNewTab(t *testing.T, s *playtestScenario, before []string, what string) string {
	t.Helper()
	s.E.AwaitEvalFor(emacsVerbBound, what, emacsWSTablineNamesForm,
		func(raw json.RawMessage) bool { return len(decodeStrings(raw)) == len(before)+1 })
	return playtestNewTabName(t, before, s.tabNames())
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
		"Plan A.5. `SPC TAB n` creates a workspace the daemon names from the initial prompt, and "+
			"`C-u SPC TAB n` creates a CHILD of the current one, whose row nests under its parent's "+
			"and whose tab stands immediately after it.")
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
	// `C-u SPC TAB n` makes the new workspace a child of THE CURRENT ONE, so
	// what Emacs is standing on is the whole input to the next step. Whether
	// creating SELECTED it is read off the product rather than assumed:
	// `agent-repl-verb-create`'s docstring says nothing happens on success.
	selectedByCreate := !playtestStandOn(t, s, createdName)
	// THE STANDING IS ASSERTED EITHER WAY. Whichever branch was taken, the
	// child create below reads `agent-repl--ws-current-name` itself, so its
	// input is checked here rather than assumed from the branch.
	playtestAwaitCurrent(t, s, createdName)
	p.note("Emacs made the created workspace the current one",
		fmt.Sprintf("`agent-repl--ws-current-name` is %q; creating it %s selected it, so a switch was %s",
			createdName,
			map[bool]string{true: "DID", false: "did NOT"}[selectedByCreate],
			map[bool]string{true: "not needed", false: "performed"}[selectedByCreate]))

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
			"row nests under its parent's, its tab follows its parent's, and its feed carries the "+
			"PARENT's prompt.")
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

	// AND WHAT IS NOT THERE, RECORDED RATHER THAN ASSERTED, which is the
	// pattern `playtest_00_feed_tail_test.go` uses for the open question it
	// inherited.
	//
	// PLAYTEST-PLAN.md A.6 asks for "parent history in feed". MEASURED here:
	// the fork's feed carries FOUR rows and none of them is the parent's --
	// the last unsatisfied probe read
	// "rows=4 ... Workspace brief / trace the lantern", the fork's own prompt
	// and its answer, with no bubble carrying the parent's text at all.
	//
	// THE CAUSE IS STRUCTURAL AND IS NOT A BUG IN THIS PLAYBOOK.
	// `verbs.forkTranscript` (daemon/internal/workspace/create.go:329) ports
	// the parent's VENDOR JSONL transcript into the child's config root under
	// a fresh vendor session id and the child resumes it, so the AGENT holds
	// the parent's context -- but the feed is composed from the daemon's OWN
	// conversation rows, which are keyed by workspace id and are not ported,
	// inherited or unioned anywhere: nothing under `daemon/internal` that
	// resolves a feed knows what a fork is. Making the parent's history
	// visible in the child's feed is therefore a decision about whose rows a
	// forked feed shows, not a leaf fix, so it is FILED for the lead and
	// nothing here invents an answer.
	//
	// THE ABSENCE IS ASSERTED, not merely narrated: a playbook that only
	// described the gap would go on passing silently on the day the product
	// closed it, and this open question must be reopened when that happens.
	forkRows, carriesParent := s.forkFeedFacts(t)
	if carriesParent {
		t.Fatalf("the fork's feed now carries the parent's prompt %q: PLAYTEST-PLAN.md A.6's "+
			"\"parent history in feed\" is satisfied, so this playbook's recorded open question is stale "+
			"and must become an assertion", playtestParentPrompt)
	}
	p.note("the fork's own feed read for the PARENT's history",
		fmt.Sprintf("the fork's feed carries %d rows and NONE of them carries the parent's prompt text %q: "+
			"the ported artifact is the vendor transcript, not the daemon's own conversation rows "+
			"(daemon/internal/workspace/create.go:329) -- FILED, not fixed here",
			forkRows, playtestParentPrompt))

	tabs := s.tabNames()
	p.capture("forked-feed", "the fork made current and its panel opened",
		fmt.Sprintf("the fork's OWN prompt bubble (%q) and a SETTLED response bubble are on its standing tail, "+
			"and its roster arm has settled", playtestForkPrompt),
		fmt.Sprintf("The tab bar carries %q AFTER its parent %q (the full drawn order is %v), and the feed "+
			"below shows the FORK's own conversation: the prompt bubble reading %q with a prose response "+
			"beneath it. THE FORKED TAB'S NAME CARRIES NO NESTING MARKER -- nesting is order here, not "+
			"spelling. WHAT THIS PICTURE DOES NOT SHOW, and the plan's A.6 asked for: the PARENT's own "+
			"prompt %q and its answer are NOT in this feed. The fork resumes the parent's ported vendor "+
			"transcript, so the agent has the context, but the daemon's own conversation rows are keyed "+
			"by workspace and are neither ported nor inherited, so the child's feed starts at its own "+
			"first turn. That is an OPEN QUESTION for the lead, recorded here rather than asserted.",
			forkName, parentName, tabs, playtestForkPrompt, playtestParentPrompt))
}

// forkFeedFacts answers how many rows the current workspace's feed is drawing
// and whether any of them carries the PARENT's prompt text.
//
// The two facts travel together because they are read in ONE probe: asking
// twice would let the page change between the count the note states and the
// absence the assertion makes, and a note that disagreed with its own
// assertion is exactly the kind of evidence a reviewer cannot use.
func (s *playtestScenario) forkFeedFacts(t *testing.T) (rows int, carriesParent bool) {
	t.Helper()
	s.E.Eval(`(setq agent-repl-playtest--js nil)`)
	script := `(function () {
                     var all = document.querySelectorAll('[data-feed-row]');
                     var carries = Array.prototype.some.call(
                       document.querySelectorAll('[data-feed-row][data-row-kind="userPrompt"]'),
                       function (row) { return row.textContent.indexOf(` + jsString(playtestParentPrompt) + `) !== -1; });
                     return all.length + ":" + (carries ? "parent" : "no-parent");
                   })()`
	raw := s.E.AwaitEvalFor(playtestPageBound, "the fork's feed row count and whether it carries the parent's prompt",
		`(agent-repl-playtest--probe `+elispString(s.Name)+` `+elispString(script)+`)`,
		func(raw json.RawMessage) bool { return strings.Contains(decodeString(raw), ":") })
	answer := decodeString(raw)
	var marker string
	if _, err := fmt.Sscanf(answer, "%d:%s", &rows, &marker); err != nil {
		t.Fatalf("the page answered %q for its feed facts, want \"<count>:<parent|no-parent>\": %v", answer, err)
	}
	switch marker {
	case "parent":
		return rows, true
	case "no-parent":
		return rows, false
	}
	t.Fatalf("the page answered %q for its feed facts: %q is neither \"parent\" nor \"no-parent\"", answer, marker)
	return 0, false
}
