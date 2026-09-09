//go:build playtest

package e2e

import (
	"encoding/json"
	"fmt"
	"testing"

	frontendv1 "agentrepl/proto/frontend/v1"
)

// OWNER 3 of PLAYTEST-PLAN.md's partition: A7-A10 -- switch, priority,
// close/reopen/kill, and copy.
//
// A.7 and A.9 are here; A.8 and A.10 are in
// `playtest_03_priority_and_copy_test.go`, which is the same owner's second
// file and shares this one's helpers.

// ---------------------------------------------------------------------------
// THE SHARED PANEL-FOLLOWS-THE-SELECTION ASSERTION
// ---------------------------------------------------------------------------

// awaitPanelFollows asserts that the PANEL IS SHOWING WS: the workspace's own
// webview buffer is in a window, and so is its own composer.
//
// WHY THIS IS THE ASSERTION FOR "THE WEBVIEW SWAPPED". Each workspace owns a
// webview buffer of its own (`agent-repl--frontend-webview-buffer-name`) and a
// composer of its own (`agent-repl--input-buffer`), and the panel is one pair
// of windows. So "the webview swapped content" and "the composer follows" are
// the same claim asked of two buffers: the windows that were displaying the
// OTHER workspace's pair are now displaying THIS one's. Reading which buffer a
// window holds is the module's own answer to that; asking the page for its
// name would ask the webapp a question Emacs already answered, and asking it
// of a page that never swapped would have nothing to say.
//
// It also asserts the previous workspace's pair is GONE from the windows,
// because "the panel shows A" and "the panel shows A and B at once" are
// different facts and only the first is a swap.
//
// The bound is `emacsVerbBound` -- the layer's own named bound for one verb's
// round trip, and a selection change is exactly that.
func awaitPanelFollows(t *testing.T, s *playtestScenario, ws, gone string) {
	t.Helper()
	awaitPanelShown(t, s, ws)
	awaitPanelHidden(t, s, gone)
}

// panelWindowsForm answers non-nil when WS's OWN panel pair -- its webview
// buffer and its composer -- are both in windows of the frame.
//
// It is ONE form used in both directions: `awaitPanelShown` waits for it to
// answer non-nil and `awaitPanelHidden` waits for it to answer nil, so "the
// panel is up" and "the panel is down" cannot drift into two different
// notions of what the panel IS.
func panelWindowsForm(ws string) string {
	return `(let ((shown (mapcar (lambda (w) (buffer-name (window-buffer w))) (window-list))))
             (and (member (agent-repl--frontend-webview-buffer-name ` + elispString(ws) + `) shown)
                  (member (buffer-name (agent-repl--input-buffer ` + elispString(ws) + `)) shown)
                  t))`
}

// awaitPanelShown asserts that WS's panel is ON THE FRAME, not merely that
// its buffers exist.
//
// WHY THIS IS A SEPARATE ASSERTION FROM THE ONES `openPanel` MAKES.
// `openPanel` waits for the webview WIDGET to be live and for the PAGE to
// have mounted, and both of those hold for a webview buffer that no window is
// displaying: the widget belongs to the buffer, and WebKit runs the page
// whether or not Emacs has laid that buffer out anywhere. So a panel that
// opened and was then laid over -- by the magit status buffer a project
// registration leaves on the frame, say -- satisfies every wait `openPanel`
// makes while PHOTOGRAPHING as an editor with no webapp in it at all. That is
// what this owner's first round of priority captures turned out to be
// pictures of, and it went unnoticed precisely because no step asserted the
// one fact those pictures were about.
//
// The bound is `emacsVerbBound`, the layer's own named bound for one verb's
// round trip: opening a panel is one verb.
func awaitPanelShown(t *testing.T, s *playtestScenario, ws string) {
	t.Helper()
	s.E.AwaitEvalFor(emacsVerbBound,
		fmt.Sprintf("%q's own webview and composer buffers to be displayed in windows of the frame", ws),
		panelWindowsForm(ws), func(raw json.RawMessage) bool { return !isJSONNull(raw) })
}

// awaitPanelHidden is the same claim negated: the frame is no longer showing
// WS's panel pair.
func awaitPanelHidden(t *testing.T, s *playtestScenario, ws string) {
	t.Helper()
	s.E.AwaitEvalFor(emacsVerbBound,
		fmt.Sprintf("the frame to stop showing %q's webview and composer", ws),
		panelWindowsForm(ws), func(raw json.RawMessage) bool { return isJSONNull(raw) })
}

// tablineDrawn answers the tab bar's rendered line as PLAIN TEXT.
//
// It reads `agent-repl-workspace-tabline-formatted` -- the function installed
// in `tab-bar-format`, and therefore the one that drives the VISIBLE bar --
// exactly the way `tabFaceFor` reads the faces off that same string. What it
// answers is what the module WROTE for the bar, so an assertion made on it
// beside a picture that disagrees splits the question cleanly: either the
// string was already wrong before any redisplay was involved, or the string
// was right and the glass never took it.
func tablineDrawn(t *testing.T, s *playtestScenario) string {
	t.Helper()
	return s.E.EvalString(`(substring-no-properties (agent-repl-workspace-tabline-formatted))`)
}

// awaitSidebarNamesCurrent asserts that the page in the webview now on the
// glass says, IN ITS OWN WORDS, which workspace is selected: the sidebar's
// roster row carrying `[data-current="true"]` names WS.
//
// This is the half `awaitPanelFollows` cannot answer. That one reads which
// buffer a window holds, which is Emacs's answer; this reads what the WEBAPP
// drew, which is the only thing that can say the content in the view is this
// workspace's and not the previous one's. `[data-roster-row][data-current]`
// is the webapp suite's own hook for exactly that (`webapp/src/sidebar/row.ts`).
func awaitSidebarNamesCurrent(t *testing.T, s *playtestScenario, ws string) {
	t.Helper()
	s.awaitInPage(t, fmt.Sprintf("the sidebar's current roster row to name %q", ws),
		`(function () { var row = document.querySelector('[data-roster-row][data-current="true"]');
                        return row && row.textContent.indexOf(`+jsString(ws)+`) !== -1; })()`)
}

// enterWorkspace re-points the scenario at WS after a switch and opens the
// panel on it, so every later page probe and every later submit acts on the
// workspace the playbook is now standing on rather than on the one it started
// from.
//
// THE PANEL OPEN IS NOT OPTIONAL HERE, and that is the module's own shape
// rather than a convenience. A workspace's composer and webview buffers are
// born when its PANEL is opened, not when it is registered or selected -- so a
// freshly registered workspace and a just-re-opened one both have no composer
// at all until this runs, and `awaitInputBuffer` on one of them waits for a
// buffer nothing has yet created. `agent-repl-frontend-open-panel` is
// idempotent, so a workspace whose panel is already up is merely re-entered,
// and `openPanel`'s own waits then assert that this workspace's page is live
// and mounted.
//
// AND THE PANEL IS ASSERTED ONTO THE FRAME, which `openPanel`'s waits do not
// say -- see `awaitPanelShown`. Every one of this owner's captures is a
// picture of an editor with the panel up, so the frame carrying it is a
// precondition of the pictures rather than a detail of one step.
func enterWorkspace(t *testing.T, s *playtestScenario, ws string) {
	t.Helper()
	// THE PANEL OPENS ON THE CURRENT WORKSPACE, so a caller that has not
	// actually landed the selection would open a panel on some OTHER
	// workspace and every later assertion would be about the wrong one. This
	// is the guard for that, and it fails right here rather than downstream.
	if got := s.E.EvalString(`(format "%s" (agent-repl--ws-current-name))`); got != ws {
		t.Fatalf("the current workspace is %q, want %q: `agent-repl-frontend-open-panel` opens the "+
			"CURRENT workspace's panel, so the selection must have landed first", got, ws)
	}
	s.Name = ws
	s.openPanel(t)
	awaitPanelShown(t, s, ws)
}

// ---------------------------------------------------------------------------
// A.7 -- SWITCH
// ---------------------------------------------------------------------------

// TestPlaytestSwitchBetweenWorkspaces is plan A.7: a second workspace, the
// selection moving between the two, and the webview, the composer and the tab
// bar all following it.
//
// BOTH VERBS ARE DRIVEN, because they are two different acts.
// `agent-repl-switch-to-project` (`SPC p p`) is a switch to a NAMED project
// root, and `agent-repl-open-most-recent-workspace` (`SPC TAB R`) is a switch
// to whichever workspace the ROSTER'S OWN when-column says was looked at last.
// A playbook that drove only the first would never touch the roster-ordered
// path at all.
func TestPlaytestSwitchBetweenWorkspaces(t *testing.T) {
	t.Parallel()
	s := newPlaytestScenario(t, "03-switch",
		"Plan A.7. Two workspaces on the tab bar, and the selection -- the tab bar's highlight, "+
			"the panel's webview and the composer -- moving between them.")
	p, e := s.Book, s.E

	first := s.repoAt(t, "repo-first")
	firstName := s.register(t, first.Dir)
	s.openPanel(t)
	awaitPanelShown(t, s, firstName)
	p.note("the first repository registered and its panel opened",
		"the composer buffer exists, the webapp drew its footer against this daemon, and the "+
			"panel's own webview and composer windows are on the frame")

	second := s.repoAt(t, "repo-second")
	secondName := s.register(t, second.Dir)
	// Registering SELECTS, which is one of Emacs's only two inputs to the
	// roster, so the assertion is on the module's own current-workspace
	// accessor rather than on anything drawn.
	e.AwaitEval("the second workspace to become the selected one",
		`(format "%s" (agent-repl--ws-current-name))`,
		func(raw json.RawMessage) bool { return decodeString(raw) == secondName })
	// The panel is opened on the newly selected workspace, so the swap
	// assertions below have two live pairs of buffers to move between rather
	// than one pair and a hole.
	enterWorkspace(t, s, secondName)
	awaitPanelFollows(t, s, secondName, firstName)
	awaitSidebarNamesCurrent(t, s, secondName)
	names := s.tabNames()
	if len(names) != 2 {
		t.Fatalf("the tab bar draws %v, want both workspaces", names)
	}
	p.capture("two-tabs", "a second repository registered through the same verb, and its panel opened",
		fmt.Sprintf("`agent-repl--ws-tabline-names` is %v, `agent-repl--ws-current-name` is %q, and the "+
			"panel's windows hold %q's own webview and composer buffers and NOT %q's, and that page's "+
			"sidebar draws %q's row as the current one",
			names, secondName, secondName, firstName, secondName),
		fmt.Sprintf("The tab bar must carry TWO workspace tabs, %q and %q, in that order, and the "+
			"SECOND must be the highlighted one — registering selects it.", names[0], names[1]))

	// `agent-repl-switch-to-project` takes a PROJECT ROOT PATH, not a
	// workspace name -- its own docstring says so -- and taking the target as
	// an argument is why the picker is neither the subject nor stubbed. The
	// binding is asserted first, because `SPC p p` with no argument would
	// prompt and a press would wedge the command loop.
	if want, got := "agent-repl-switch-to-project", e.LeaderBinding("p p"); got != want {
		t.Fatalf("SPC p p resolves to %q, want %q", got, want)
	}
	e.Eval(`(agent-repl-switch-to-project ` + elispString(first.Dir) + `)`)
	e.AwaitEval("the first workspace to become the selected one again",
		`(format "%s" (agent-repl--ws-current-name))`,
		func(raw json.RawMessage) bool { return decodeString(raw) == firstName })
	// AND THE PAGE IN THAT WEBVIEW IS THE ONE THAT BELONGS TO IT. The webview
	// buffer being displayed says Emacs swapped the window; the page's own
	// mount says the thing now on the glass is a live webapp rather than the
	// husk of a view that was torn down -- and `enterWorkspace` asserts that
	// mount for the workspace it re-points the scenario at.
	enterWorkspace(t, s, firstName)
	awaitPanelFollows(t, s, firstName, secondName)
	awaitSidebarNamesCurrent(t, s, firstName)
	p.capture("switched-back", "`agent-repl-switch-to-project` back to the first workspace",
		fmt.Sprintf("`agent-repl--ws-current-name` is %q, the panel's windows now hold %q's own webview "+
			"and composer and NOT %q's, that webview's page is mounted (its feed host is drawn and its "+
			"footer carries a status word), and that page's sidebar draws %q's row as the current one",
			firstName, firstName, secondName, firstName),
		fmt.Sprintf("The SAME two tabs in the SAME order, with the highlight moved back to %q. "+
			"The selection moved; the roster did not.", firstName))

	// `SPC TAB R` takes NO argument -- it picks from the roster's when-column
	// -- so it is PRESSED: real keymap lookup, real command. With two
	// workspaces and the first one selected, the only candidate is the second.
	if want, got := "agent-repl-open-most-recent-workspace", e.LeaderBinding("TAB R"); got != want {
		t.Fatalf("SPC TAB R resolves to %q, want %q", got, want)
	}
	e.Leader("TAB R")
	e.AwaitEval("`SPC TAB R` to land on the other workspace",
		`(format "%s" (agent-repl--ws-current-name))`,
		func(raw json.RawMessage) bool { return decodeString(raw) == secondName })
	enterWorkspace(t, s, secondName)
	awaitPanelFollows(t, s, secondName, firstName)
	awaitSidebarNamesCurrent(t, s, secondName)
	p.capture("switched-most-recent", "`SPC TAB R` (`agent-repl-open-most-recent-workspace`) pressed",
		fmt.Sprintf("`agent-repl--ws-current-name` is %q, the panel's windows hold %q's own webview and "+
			"composer and NOT %q's, that webview's page is mounted, and that page's sidebar draws %q's "+
			"row as the current one", secondName, secondName, firstName, secondName),
		fmt.Sprintf("The SAME two tabs in the SAME order once more, with the highlight back on %q. "+
			"`SPC TAB R` walks the roster's when-column, so it lands on the OTHER workspace and the bar "+
			"looks exactly as it did in the first capture.", secondName))
}

// ---------------------------------------------------------------------------
// A.9 -- CLOSE, RE-OPEN, CLOSE WITH A HELD PROMPT, KILL
// ---------------------------------------------------------------------------

// TestPlaytestCloseAndKillLeaveTheEditorAnswering is plan A.9's close and
// kill, and it is deliberately FUNCTIONAL-ONLY: what it proves is that the
// tab goes away, the daemon still holds the session after a close, and Emacs
// is still answering afterwards. None of that is a picture.
//
// The heartbeat is the real assertion behind the last of those, and it is
// armed for the whole life of the process: this is the sentinel/kill-buffer
// recursion, which manifests only as an editor that stops answering.
func TestPlaytestCloseAndKillLeaveTheEditorAnswering(t *testing.T) {
	t.Parallel()
	s := newPlaytestScenario(t, "03-close-and-kill",
		"Plan A.9. Close is a view act and kill never blocks, and neither wedges the editor. "+
			"FUNCTIONAL ONLY: nothing here has a visual subject, so nothing is captured.")
	p, e := s.Book, s.E

	repository := s.repoAt(t, "repo")
	name := s.register(t, repository.Dir)
	s.openPanel(t)
	awaitPanelShown(t, s, name)
	p.note("one repository registered with its panel open",
		"the panel's webview is live, the webapp drew its footer, and the panel's own webview and "+
			"composer windows are on the frame")

	// `SPC j d` takes the CURRENT workspace, so it is PRESSED: real keymap
	// lookup, real command.
	e.Leader("j d")
	e.AwaitEval("the closed workspace's tab to be gone",
		emacsWSTablineNamesForm,
		func(raw json.RawMessage) bool { return !containsString(decodeStrings(raw), name) })
	p.note("`SPC j d` pressed to close the workspace",
		"the name is gone from `agent-repl--ws-tabline-names`")

	// CLOSE IS A VIEW ACT BY CONTRACT, so the only way to say the daemon still
	// holds the workspace is to ask the daemon -- at the address Emacs's own
	// launcher published.
	awaitDaemonRoster(t, e.DaemonAddr(), emacsVerbBound,
		"the daemon to still hold the closed workspace",
		func(r *frontendv1.WorkspaceRoster) bool { return len(r.GetRepository().GetSections()) > 0 })
	p.note("the daemon asked for its own roster at the address the launcher published",
		"the daemon still carries the workspace: closing is a VIEW act and destroys nothing")

	e.AwaitEvalFor(emacsWedgeProbeBound, "emacs to still answer its command loop after the close and kill",
		`(and (emacs-pid) t)`,
		func(raw json.RawMessage) bool { return !isJSONNull(raw) })
	p.note("Emacs probed for liveness after the close",
		"the command loop still answers, and the heartbeat has not missed for the whole run")
}

// TestPlaytestReopenAClosedWorkspace is plan A.9's re-open: `SPC TAB o`
// (`agent-repl-open-workspace`), the tab coming back, and the panel with it.
//
// THE RE-OPEN IS A REQUEST TO THE DAEMON, not an editor-local switch --
// `agent-repl-switch-to-project`'s own docstring says so and refuses to offer
// a closed workspace. So the assertion that it worked is the TAB RETURNING,
// which only a roster push can produce: Emacs cannot put that tab back by
// itself.
//
// The verb picks from the roster's closed rows through `completing-read`, so
// the pick is stubbed FOR THE DURATION OF THE ONE CALL, the way this layer
// stubs every interactive pick. The stub asserts the offered candidates
// CONTAIN the closed workspace rather than answering blindly: a picker that
// was offered nothing would otherwise be answered with a name it never had.
func TestPlaytestReopenAClosedWorkspace(t *testing.T) {
	t.Parallel()
	s := newPlaytestScenario(t, "03-reopen",
		"Plan A.9's re-open. A closed workspace brought back through `SPC TAB o`, and the tab, "+
			"the panel and the composer that come back with it.")
	p, e := s.Book, s.E

	repository := s.repoAt(t, "repo")
	name := s.register(t, repository.Dir)
	s.openPanel(t)
	awaitPanelShown(t, s, name)
	p.note("one repository registered with its panel open",
		"the panel's webview is live, the webapp drew its footer, and the panel's own webview and "+
			"composer windows are on the frame")

	e.Leader("j d")
	e.AwaitEval("the closed workspace's tab to be gone",
		emacsWSTablineNamesForm,
		func(raw json.RawMessage) bool { return !containsString(decodeStrings(raw), name) })
	// The roster must actually be OFFERING the closed row before the pick is
	// answered, or the stub below would answer a picker that had nothing in
	// it and the verb would fail on its own `user-error` instead.
	e.AwaitEval("the roster to list the closed workspace as re-openable",
		`(mapcar (lambda (row) (format "%s" (agent-repl-verbs--row-name row)))
                 (agent-repl-verbs--closed-rows))`,
		func(raw json.RawMessage) bool { return containsString(decodeStrings(raw), name) })
	p.note("`SPC j d` pressed to close the workspace",
		fmt.Sprintf("the tab is gone from `agent-repl--ws-tabline-names` and "+
			"`agent-repl-verbs--closed-rows` now offers %q as re-openable", name))

	if want, got := "agent-repl-open-workspace", e.LeaderBinding("TAB o"); got != want {
		t.Fatalf("SPC TAB o resolves to %q, want %q", got, want)
	}
	e.Eval(`(cl-letf (((symbol-function 'completing-read)
                        (lambda (_prompt candidates &rest _)
                          (unless (member ` + elispString(name) + ` candidates)
                            (error "the open picker offered %S, not the closed workspace" candidates))
                          ` + elispString(name) + `)))
               (agent-repl-open-workspace)
               t)`)

	// THE TAB COMES BACK ONLY THROUGH A ROSTER PUSH, which is what makes this
	// an assertion about the daemon having re-opened the workspace rather
	// than about Emacs having drawn something.
	e.AwaitEval("the re-opened workspace's tab to return",
		emacsWSTablineNamesForm,
		func(raw json.RawMessage) bool { return containsString(decodeStrings(raw), name) })
	// AND THE TAB CARRIES THE DAEMON-MINTED REF, which is the tab's identity:
	// a name back on the bar without one would be a husk that no verb could
	// address, and the panel open below is the first thing that would need it.
	e.AwaitEval("the re-opened workspace's tab to carry its daemon-minted ref",
		`(plist-get (agent-repl--ws-get `+elispString(name)+` :ref) :id)`,
		func(raw json.RawMessage) bool { return decodeString(raw) != "" })

	// RE-OPENING DOES NOT SELECT, and that is the verb's own contract rather
	// than a gap: `agent-repl-verb-open`'s docstring says the tab arrives
	// through the roster push, and the close left the editor standing on
	// Doom's own `main` perspective. So the switch onto the revived workspace
	// is a SEPARATE user act.
	//
	// IT IS THE PICKER ARM OF `SPC p p`, not the project-path arm A.7 drives.
	// `agent-repl-switch-to-project` with no argument completes over
	// `agent-repl--live-ws-names` -- the roster's own live workspaces -- and
	// switches to the chosen one, which is exactly the question here: the
	// revived workspace is live again, so it must be on offer. The path arm
	// goes through projectile's own project switch, which is a different
	// mechanism and one the close's perspective teardown leaves nothing for.
	e.Eval(`(cl-letf (((symbol-function 'completing-read)
                        (lambda (_prompt candidates &rest _)
                          (unless (member ` + elispString(name) + ` candidates)
                            (error "the switch picker offered %S, not the re-opened workspace" candidates))
                          ` + elispString(name) + `)))
               (agent-repl-switch-to-project)
               t)`)
	e.AwaitEval("the re-opened workspace to become the selected one",
		`(format "%s" (agent-repl--ws-current-name))`,
		func(raw json.RawMessage) bool { return decodeString(raw) == name })
	enterWorkspace(t, s, name)
	p.capture("reopened-tab", "`SPC TAB o` (`agent-repl-open-workspace`) answered with the closed workspace, then `SPC p p` picked back onto it",
		fmt.Sprintf("`agent-repl--ws-tabline-names` carries %q again and its tab carries the "+
			"daemon-minted ref -- which only a roster push can do -- and its panel re-opened with a "+
			"live webview and a mounted page", name),
		fmt.Sprintf("The tab bar carries the workspace tab %q once more, exactly as it did before the "+
			"close: a re-opened workspace is the same workspace and not a new one.", name))
}

// TestPlaytestCloseWithAHeldPromptKeepsTheTab is plan A.9's third act: a
// prompt held against a live turn, a close that must therefore be REFUSED,
// and the tab that stays.
//
// WHAT EMACS OWES HERE IS PRECISELY TO NOT ACT. The refusal is echo-area text
// by contract rather than a dialog, so the assertions are negative on both
// sides: the tab is still drawn and the held prompt is still in
// `agent-repl--prompt-queue`. Undelivered user intent may never be silently
// discarded, and only the second half says so.
//
// THE HELD PROMPT IS EMACS'S OWN QUEUE, not the daemon's hold tray. The two
// are different surfaces: `agent-repl--prompt-queue` is what the close
// consults and refuses on, and the webapp's `[data-component="hold-tray"]`
// draws the DAEMON's held items, which an Emacs-side deferred prompt never
// enters (`lisp/prompt-queue.el` delivers it on the finish edge, from Emacs).
// The tray is plan C.20's subject and owner 7's; asserting it here would
// assert the wrong mechanism's state about this step.
func TestPlaytestCloseWithAHeldPromptKeepsTheTab(t *testing.T) {
	t.Parallel()
	s := newPlaytestScenario(t, "03-held-close",
		"Plan A.9's held-prompt close. A prompt held against a live turn makes the close a REFUSAL, "+
			"and the tab stays on the bar. Kill then takes it, and never blocks.")
	p, e := s.Book, s.E

	repository := s.repoAt(t, "repo")
	name := s.register(t, repository.Dir)
	s.openPanel(t)
	awaitPanelShown(t, s, name)

	// The turn is parked with the fake SDK's `!hold` scenario so the close is
	// genuinely refused rather than merely slow: the held prompt is queued
	// against a turn that is actually in flight.
	s.submit(t, holdScenario)
	// A RUNNING ARM IS NOT YET A RUNNING TURN, and this step's whole premise
	// is the turn. `:submitting` is a running arm and it is ALSO what a prompt
	// that has not reached a session yet reads as: measured on the first round
	// of this capture, the arm await was satisfied while the `!hold` prompt was
	// still sitting in the DAEMON's hold tray drawn as "queued -- classifying /
	// held until the session is up", with the footer reading "idle / ready".
	// The close would then have been refused against a queued prompt rather
	// than against work in flight, which is a different fact from the one the
	// plan's line names.
	//
	// So the premise is asserted on the two surfaces that can only be in this
	// state once the session HAS the turn: the daemon's own tray is empty
	// again -- `.hold-tray-empty[data-empty]` is how `webapp/src/tray/tray.ts`
	// spells "nothing held" -- and the prompt has been drawn as a feed row of
	// the session's own turn.
	s.awaitInPage(t, "the daemon's hold tray to be empty again, so the prompt is no longer queued behind a booting session",
		`document.querySelector('[data-component="hold-tray"] .hold-tray-empty[data-empty]') !== null`)
	s.awaitInPage(t, "the submitted prompt to be drawn as a row of the session's own turn",
		`document.querySelector('[data-feed-row][data-row-kind="userPrompt"]') !== null`)
	arm := s.awaitArm(t, name, "the held turn to be in flight before anything is queued", emGHIRunningArms...)
	p.note("`!hold` submitted, parking a turn that will not conclude",
		fmt.Sprintf("the daemon's hold tray is EMPTY and the prompt is drawn as a feed row, so the "+
			"session has the turn rather than the prompt being queued ahead of one, and the "+
			"workspace's roster arm is %s -- one of the module's own RUNNING arms", arm))

	// The deferred prompt goes in through its own ordinary command, PRESSED
	// in the composer where a user would press it.
	typeIntoComposer(e, s.Input, "the deferred prompt for the playtest")
	if want, got := "agent-repl-queue-deferred-prompt", e.LeaderBinding("j RET"); got != want {
		t.Fatalf("SPC j RET resolves to %q, want %q", got, want)
	}
	e.KeysIn(s.Input, "SPC j RET")
	e.AwaitEval("the deferred prompt to be held",
		`(length (gethash `+elispString(name)+` agent-repl--prompt-queue))`,
		func(raw json.RawMessage) bool {
			var n int
			return !isJSONNull(raw) && json.Unmarshal(raw, &n) == nil && n >= 1
		})
	p.note("`SPC j RET` pressed in the composer to hold the prompt until the turn ends",
		fmt.Sprintf("`agent-repl--prompt-queue` holds at least one entry for %q", name))

	e.Leader("j d")
	// Waiting for the refusal's own message is what makes the surviving tab an
	// assertion rather than a race with the close: the refusal IS echo-area
	// text by contract, which is this layer's sanctioned rendered exception.
	e.AwaitEval("the blocked close to be answered",
		`(with-current-buffer "*Messages*"
                   (and (string-match-p "close blocked" (buffer-string)) t))`,
		func(raw json.RawMessage) bool { return !isJSONNull(raw) })
	if tabs := s.tabNames(); !containsString(tabs, name) {
		t.Fatalf("agent-repl--ws-tabline-names = %v, want %q still present: a blocked close draws no dialog and leaves the tab in place", tabs, name)
	}
	if n := e.EvalInt(`(length (gethash ` + elispString(name) + ` agent-repl--prompt-queue))`); n < 1 {
		t.Fatalf("agent-repl--prompt-queue holds %d entries for %q, want the held prompt still there: undelivered intent is never silently discarded", n, name)
	}
	p.capture("held-close-tab-stays", "`SPC j d` pressed while a prompt is held against the live turn",
		fmt.Sprintf("the close was REFUSED (`*Messages*` carries \"close blocked\"), %q is still in "+
			"`agent-repl--ws-tabline-names`, and its held prompt is still in `agent-repl--prompt-queue`", name),
		fmt.Sprintf("The tab bar STILL carries %q. The close was asked for and refused, so nothing about "+
			"the bar changed: there is no dialog, no missing tab and no gap where one was. The webapp's "+
			"HOLD TRAY reads \"nothing held\" -- the held prompt is EMACS's queue, not the daemon's, and a "+
			"tray with the `!hold` prompt still in it would mean the turn this close was refused against "+
			"had never started.", name))

	// KILL IS FORCED and takes no refusal path even with the turn in flight,
	// which is exactly the configuration the close just refused.
	if want, got := "agent-repl-kill-workspace", e.LeaderBinding("j x"); got != want {
		t.Fatalf("SPC j x resolves to %q, want %q", got, want)
	}
	e.Leader("j x")
	e.AwaitEval("the killed workspace's tab to go away",
		emacsWSTablineNamesForm,
		func(raw json.RawMessage) bool { return !containsString(decodeStrings(raw), name) })
	if e.EvalBool(`(with-current-buffer "*Messages*"
                          (and (string-match-p "kill blocked" (buffer-string)) t))`) {
		t.Fatal("*Messages* carries a kill refusal: kill is forced and never blocks")
	}
	p.note("`SPC j x` pressed to kill the workspace the close had refused to take",
		"the tab is gone and `*Messages*` carries NO kill refusal: kill is forced and never blocks")

	// THE WEDGE PROBE, which is the real subject of running close and kill
	// back to back on a workspace with a live panel: the sentinel/kill-buffer
	// recursion manifests only as an Emacs that stops answering.
	e.AwaitEvalFor(emacsWedgeProbeBound, "emacs to still answer its command loop after the refused close and the kill",
		`(and (emacs-pid) t)`,
		func(raw json.RawMessage) bool { return !isJSONNull(raw) })
	p.note("Emacs probed for liveness after the refused close and the kill",
		"the command loop still answers, and the heartbeat has not missed for the whole run")
}
