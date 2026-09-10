//go:build playtest

package e2e

import (
	"encoding/json"
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"claude-repld/integration/harness"
)

// OWNER 18 of PLAYTEST-PLAN.md's partition: I53-I56 -- panels, fullscreen,
// webview reload/rescue, and visit-file routing.
//
// Four playbooks, one world each:
//
//   - I.53 TestPlaytestPanelsOpenAndClose: both panel kinds (the webview
//     and the composer) opened into the main area, the composer focused,
//     the plain close hiding both and leaving the tab order alone, and the
//     toggle bringing them back.
//   - I.54 TestPlaytestFullscreenToggleAndRestore: both branches of
//     `SPC w f` -- inside the panels it focuses the composer and maximizes
//     nothing, and from an ordinary work window it maximizes that window and
//     then restores the layout it replaced.
//   - I.55 TestPlaytestReloadAndRescueWebview: `SPC o l` fetches the page
//     again and the feed's rows come back; `SPC o L` leaves a page that is
//     home alone and brings a page that navigated away back with its rows.
//   - I.56 TestPlaytestVisitFileRouting: a file under a registered worktree
//     lands in its owning workspace, an unroutable one records a refusal and
//     still opens. Functional; one picture of the routed layout.
//
// Every assertion here is one the Emacs layer's own scenarios already make
// (`emacs_panels_e2e_test.go`, `emacs_findfile_e2e_test.go`), and every
// capture follows its assertion.

// panelWindowSides answers the `window-side` parameter of every window
// showing one of the two panel buffer families, as DATA. A panel is "in the
// main area" when its side is nil; the module's own rule is that a panel is
// never a side window.
func panelWindowSides(e *Emacs, frontendPrefix, panelPrefix string) map[string]string {
	e.t.Helper()
	pairs := e.EvalStrings(`(let (out)
                              (dolist (w (window-list))
                                (let ((name (buffer-name (window-buffer w))))
                                  (push (format "%s\t%s" name (window-parameter w 'window-side)) out)))
                              out)`)
	sides := map[string]string{}
	for _, pair := range pairs {
		name, side, _ := strings.Cut(pair, "\t")
		if strings.HasPrefix(name, frontendPrefix) || strings.HasPrefix(name, panelPrefix) {
			sides[name] = side
		}
	}
	return sides
}

// requirePanelsInMainArea asserts that BOTH panel kinds are shown, and that
// each is a main-area window rather than a side window.
func requirePanelsInMainArea(t *testing.T, e *Emacs, frontendPrefix, panelPrefix string) {
	t.Helper()
	sides := panelWindowSides(e, frontendPrefix, panelPrefix)
	var sawFrontend, sawPanel bool
	for name, side := range sides {
		if side != "nil" {
			t.Fatalf("the panel window %q has window-side %q, want none: a panel is never a side window", name, side)
		}
		if strings.HasPrefix(name, frontendPrefix) {
			sawFrontend = true
		}
		if strings.HasPrefix(name, panelPrefix) {
			sawPanel = true
		}
	}
	if !sawFrontend {
		t.Fatalf("no window shows a %q buffer; the panel windows are %v", frontendPrefix, sides)
	}
	if !sawPanel {
		t.Fatalf("no window shows a %q buffer; the panel windows are %v", panelPrefix, sides)
	}
}

// requireTabBarHeightContract asserts the frame carries the module's own
// pinned tab-bar row count. `agent-repl--install-fixed-height-tab-bar` pins
// `agent-repl--tabline-row-count` onto every graphical frame, and a frame
// whose `tab-bar-lines` reads anything else draws the second row clipped --
// which would make every layout capture here a picture of that defect.
func requireTabBarHeightContract(t *testing.T, e *Emacs) (rows int) {
	t.Helper()
	rows = e.EvalInt(`agent-repl--tabline-row-count`)
	if got := e.EvalInt(`(or (frame-parameter (selected-frame) 'tab-bar-lines) 0)`); got != rows {
		t.Fatalf("the frame's tab-bar-lines is %d, want the module's pinned %d: the fixed-height tab bar contract is broken", got, rows)
	}
	return rows
}

// ---------------------------------------------------------------------------
// I.53 -- PANELS OPEN AND CLOSE
// ---------------------------------------------------------------------------

// TestPlaytestPanelsOpenAndClose is plan I.53: each panel kind opened into
// the main area, the composer focused, a plain close that hides both and
// leaves the tab order alone, and the toggle bringing them back.
//
// Two workspaces, so "the tab order is untouched" is an observable claim
// rather than a vacuous one.
func TestPlaytestPanelsOpenAndClose(t *testing.T) {
	t.Parallel()
	s := newPlaytestScenario(t, "18-panels-open-close",
		"Plan I.53. The webview and the composer opened into the main area, the composer focused "+
			"with `SPC o v`, both hidden by the plain close `SPC o c` with the tab order untouched, "+
			"and both brought back by the same toggle.")
	p, e := s.Book, s.E
	frontendPrefix, panelPrefix := bufferNamePrefixes(e)

	first := s.register(t, s.repoAt(t, "first").Dir)
	// The panel verbs act on the CURRENT workspace, which is the one most
	// recently added; point the scenario at it.
	s.Name = s.register(t, s.repoAt(t, "second").Dir)
	tabs := s.tabNames()
	if len(tabs) != 2 {
		t.Fatalf("the tab bar names %q after two registrations, want two tabs", tabs)
	}
	p.note("two worktrees registered through `SPC TAB C-n`",
		fmt.Sprintf("the tab bar enumerates both: %q", tabs))

	s.openPanel(t)
	awaitPanelWindows(e, frontendPrefix, panelPrefix)
	requirePanelsInMainArea(t, e, frontendPrefix, panelPrefix)
	rows := requireTabBarHeightContract(t, e)
	windows := e.EvalInt(`(length (window-list))`)
	p.capture("panels-open", "`agent-repl-frontend-open-panel` for the second workspace",
		fmt.Sprintf("a %q window and a %q window are both on the frame, neither is a side window, the "+
			"webview is live and its page mounted, the frame's tab-bar-lines is the pinned %d, and the "+
			"OTHER workspace's tab is drawn %s",
			frontendPrefix, panelPrefix, rows, s.tabPaintOf(t, first)),
		"The frame is SPLIT between the two panel kinds, both in the main area: the WEBAPP inside "+
			"the webview window -- workspace sidebar down one side, an empty feed, and the progress "+
			"footer with a status word along its bottom -- and a separate composer window beneath or "+
			"beside it. The tab bar across the top lists BOTH workspaces, `[1]` then `[2]`, with the "+
			"SECOND one selected. THE WEBAPP MUST NOT BE A BLANK WHITE RECTANGLE.")

	// THE COMPOSER, FOCUSED. Select the webview's window first so the
	// command has somewhere to move point FROM.
	e.Eval(`(progn (select-window (get-buffer-window (agent-repl--ws-get ` + elispString(s.Name) + ` :frontend-buffer))) t)`)
	if want, got := "agent-repl-focus-input", e.LeaderBinding("o v"); got != want {
		t.Fatalf("SPC o v resolves to %q, want %q", got, want)
	}
	e.Leader("o v")
	e.AwaitEvalFor(panelSettleBound, "the composer window to be selected",
		`(buffer-name (window-buffer (selected-window)))`,
		func(raw json.RawMessage) bool { return decodeString(raw) == s.Input })
	p.capture("composer-focused", "`SPC o v` (`agent-repl-focus-input`) from the webview's window",
		fmt.Sprintf("the selected window shows the composer buffer %q", s.Input),
		"The same split as before, with the CURSOR now in the composer window: the composer is the "+
			"selected window and the webapp is unchanged beside it.")

	// THE PLAIN CLOSE hides both and touches no tab.
	orderBefore := e.EvalStrings(`agent-repl-roster--tab-order`)
	if want, got := "agent-repl-simple", e.LeaderBinding("o c"); got != want {
		t.Fatalf("SPC o c resolves to %q, want %q", got, want)
	}
	e.Leader("o c")
	e.AwaitEvalFor(panelSettleBound, "the panel windows to go away",
		`(mapcar (lambda (w) (buffer-name (window-buffer w))) (window-list))`,
		func(raw json.RawMessage) bool {
			return panelBufferCount(decodeStrings(raw), frontendPrefix, panelPrefix) == 0
		})
	orderAfter := e.EvalStrings(`agent-repl-roster--tab-order`)
	if strings.Join(orderBefore, "\x00") != strings.Join(orderAfter, "\x00") {
		t.Fatalf("the plain close moved the tab order: %q -> %q; only the deprio close may touch it", orderBefore, orderAfter)
	}
	if got := s.tabNames(); strings.Join(got, "\x00") != strings.Join(tabs, "\x00") {
		t.Fatalf("the plain close changed the tab bar's names: %q -> %q", tabs, got)
	}
	if !e.EvalBool(`(and (buffer-live-p (agent-repl--ws-get ` + elispString(s.Name) + ` :frontend-buffer)) t)`) {
		t.Fatal("the plain close killed the webview buffer; it only hides the panels")
	}
	p.capture("panels-hidden", "`SPC o c` (`agent-repl-simple`), the plain close",
		fmt.Sprintf("no window shows a panel buffer, the webview buffer is still live, and the roster's "+
			"tab order is unchanged: %q", orderAfter),
		"NO webapp and NO composer are on the frame: the main area shows whatever ordinary buffer the "+
			"layout held before the panels (the work layout was restored). The tab bar still lists BOTH "+
			"workspaces in the SAME order as before, the second still selected -- the plain close "+
			"leaves the tab alone.")

	// THE TOGGLE BRINGS THEM BACK.
	e.Leader("o c")
	awaitPanelWindows(e, frontendPrefix, panelPrefix)
	requirePanelsInMainArea(t, e, frontendPrefix, panelPrefix)
	s.awaitPageMounted(t)
	if got := e.EvalInt(`(length (window-list))`); got != windows {
		t.Fatalf("the frame holds %d windows after the toggle reopened the panels, want the %d it held on the first open", got, windows)
	}
	p.capture("panels-reopened", "`SPC o c` again, toggling the panels back",
		fmt.Sprintf("both panel kinds are on the frame again in the main area, the page is mounted, "+
			"the frame holds its original %d windows, and the OTHER workspace's tab is drawn %s",
			windows, s.tabPaintOf(t, first)),
		"The split of the first capture is back: the webapp in the webview window with its sidebar and "+
			"footer, and the composer window beside it. The tab bar still lists BOTH workspaces in the "+
			"SAME order with the second selected. (What COLOR the first workspace's tab is drawn is "+
			"section B's subject, not this one; the row beside this sentence records the arm and the "+
			"face the module chose for it at the instant of the picture.)")
}

// tabPaintOf records, as DATA for the manifest, what the module decided to
// paint a workspace's tab with at this instant: its arm, the color its own
// table gives that arm, and every face the drawn tabline actually carries.
//
// It asserts nothing. Which color a tab OUGHT to be is section B's subject
// (the tab-arm playbooks), and a layout playbook that claimed one would be
// filing section B's defects from a picture it took for another reason. What
// it does is make a tab whose paint CHANGES between two of this playbook's
// pictures explicable from the manifest alone, instead of sending a reviewer
// to guess.
func (s *playtestScenario) tabPaintOf(t *testing.T, ws string) string {
	t.Helper()
	arm, color := s.armPaint(t, ws)
	return fmt.Sprintf("on arm %s (its table color: %s), and ITS OWN run of the drawn tabline -- from the "+
		"`[` of its badge to the end of its name -- carries %s",
		arm, color, s.tabRunFaceOf(t, ws))
}

// tabRunFaceOf answers the faces the drawn tabline carries over ONE
// workspace's own tab, in order, from the `[` of its index badge through the
// end of its name.
//
// The shared `tabFaceFor` answers the SET of faces in the whole line, which
// cannot tell two tabs apart -- and two pictures of a bar whose face set is
// identical can still draw a given tab differently, because what changed is
// which run got which face. That is exactly what happened here, so the
// manifest records the run rather than the set.
func (s *playtestScenario) tabRunFaceOf(t *testing.T, ws string) string {
	t.Helper()
	return s.E.EvalString(`(let* ((line (agent-repl-workspace-tabline-formatted))
                                  (at (string-match (regexp-quote ` + elispString(ws) + `) line)))
                             (if (null at)
                                 "<the drawn tabline does not carry this workspace's name at all>"
                               (let* ((start (or (cl-position ?\[ line :end at :from-end t) at))
                                      (end (+ at (length ` + elispString(ws) + `)))
                                      (i start)
                                      (out nil))
                                 (while (< i end)
                                   (let ((f (format "%S" (get-text-property i 'face line))))
                                     (unless (equal f (car out)) (push f out)))
                                   (setq i (1+ i)))
                                 (mapconcat #'identity (nreverse out) " then "))))`)
}

// ---------------------------------------------------------------------------
// I.54 -- FULLSCREEN TOGGLE AND RESTORE
// ---------------------------------------------------------------------------

// TestPlaytestFullscreenToggleAndRestore is plan I.54, and it follows BOTH
// branches of the product's one fullscreen key.
//
// `agent-repl-fullscreen-and-focus` maximizes only a NON-agent window: from
// inside a panel buffer it moves point to the composer, because the panels
// already fill the frame (fullscreen is the panels' sole display format).
// So the playbook presses the key twice over, in the two places a user
// presses it:
//
//   - from the webview, where it focuses the composer and maximizes nothing;
//   - from an ordinary work window, where it maximizes and then restores.
//
// AND THE WORK LAYOUT IS ARRANGED BEFORE THE PANELS OPEN. A buffer opened
// while the panels are visible CLOSES them -- `close-panels-on-open.el`
// advises `switch-to-buffer', `pop-to-buffer-same-window' and `find-file'
// so the panels get out of the way of the file a user just asked for -- so
// "an ordinary work window beside the panels" is not a state a user can
// reach, and a playbook that split one there would be photographing its own
// arrangement rather than the product. Arranged first, the split IS the
// layout the panels' open saves and the plain close restores.
func TestPlaytestFullscreenToggleAndRestore(t *testing.T) {
	t.Parallel()
	s := newPlaytestScenario(t, "18-fullscreen",
		"Plan I.54. `SPC w f` inside the panels focuses the composer and maximizes nothing; from an "+
			"ordinary work window it maximizes that window, and the same key restores the layout it "+
			"replaced.")
	p, e := s.Book, s.E
	frontendPrefix, panelPrefix := bufferNamePrefixes(e)

	// THE WORKSPACE IS REGISTERED FIRST, and the work layout arranged after
	// it. Registering opens the worktree's magit status in the selected
	// window, so a layout arranged ahead of it is not the one the panels'
	// open would save -- which is the layout the restore is judged against.
	//
	// AND THE REGISTRATION'S OWN LANDING IS WAITED OUT AND PUT AWAY BEFORE
	// THE ARRANGEMENT. A registered workspace comes up on its own panel
	// asynchronously (`agent-repl--arm-landing-panels`, drained when the
	// minted tab arrives), so a layout arranged while that show is still in
	// flight is arranged ON TOP of panels that are about to take the frame:
	// the `delete-other-windows` that starts the arrangement then falls on
	// the landing's own webview window, the delete-protected composer
	// survives it, and the reconciler remounts over the work buffers.
	// Measured, that left the frame showing the landing's panels at the
	// moment the work layout was read, and the close under test was then
	// judged against a "work layout" that was never on the frame.
	repository := s.repoAt(t, "repo")
	s.register(t, repository.Dir)
	// `s.register` already waited the landing out (landing_test.go); what is
	// left is to put it away, so the work layout below is arranged on a frame
	// the landing's own show has been cleared off.
	putTheLandingAway(e, s.Name)

	const workA, workB = "*playtest-work-a*", "*playtest-work-b*"
	e.Eval(`(progn
              (delete-other-windows)
              (switch-to-buffer (get-buffer-create ` + elispString(workA) + `))
              (with-current-buffer ` + elispString(workA) + `
                (erase-buffer)
                (insert "WORK WINDOW A. The playtest maximizes B beside it with SPC w f.\n"))
              (select-window (split-window))
              (switch-to-buffer (get-buffer-create ` + elispString(workB) + `))
              (with-current-buffer ` + elispString(workB) + `
                (erase-buffer)
                (insert "WORK WINDOW B. This is the window SPC w f maximizes.\n"))
              t)`)
	work := e.EvalStrings(`(mapcar (lambda (w) (buffer-name (window-buffer w))) (window-list))`)
	if len(work) != 2 {
		t.Fatalf("the work layout holds %q, want the two ordinary windows a restore is observable from", work)
	}
	p.note("two ordinary work windows split on the frame, before any panel exists",
		fmt.Sprintf("the frame holds exactly the two work windows: %q", work))

	s.openPanel(t)
	awaitPanelWindows(e, frontendPrefix, panelPrefix)
	requirePanelsInMainArea(t, e, frontendPrefix, panelPrefix)
	rows := requireTabBarHeightContract(t, e)
	p.capture("panels-fill-the-frame", "`agent-repl-frontend-open-panel` over the work layout",
		fmt.Sprintf("both panel kinds are on the frame in the main area, neither is a side window, and the "+
			"frame's tab-bar-lines is the module's pinned %d", rows),
		"The PANELS have taken the frame: the webapp in the webview window -- workspace sidebar down "+
			"one side, an empty feed, the progress footer with a status word along its bottom -- and the "+
			"composer window beneath it. Neither work window is visible. THE WEBAPP MUST NOT BE A BLANK "+
			"WHITE RECTANGLE.")

	// BRANCH ONE: from a panel buffer the key focuses the composer, and
	// maximizes nothing.
	e.Eval(`(progn (select-window (get-buffer-window (agent-repl--ws-get ` + elispString(s.Name) + ` :frontend-buffer))) t)`)
	if want, got := "agent-repl-fullscreen-and-focus", e.LeaderBinding("w f"); got != want {
		t.Fatalf("SPC w f resolves to %q, want %q", got, want)
	}
	e.Leader("w f")
	e.AwaitEvalFor(panelSettleBound, "the composer window to be selected",
		`(buffer-name (window-buffer (selected-window)))`,
		func(raw json.RawMessage) bool { return decodeString(raw) == s.Input })
	if !e.EvalBool(`(null agent-repl--window-fullscreen-config)`) {
		t.Fatal("`SPC w f` inside a panel buffer saved a window configuration; the panel branch maximizes nothing")
	}
	requirePanelsInMainArea(t, e, frontendPrefix, panelPrefix)
	p.note("`SPC w f` (`agent-repl-fullscreen-and-focus`) pressed from the webview window",
		fmt.Sprintf("point moved to the composer %q, `agent-repl--window-fullscreen-config` is still nil, and both panels are still on the frame", s.Input))

	// BACK TO THE WORK LAYOUT, through the product's own plain close.
	if want, got := "agent-repl-simple", e.LeaderBinding("o c"); got != want {
		t.Fatalf("SPC o c resolves to %q, want %q", got, want)
	}
	e.Leader("o c")
	e.AwaitEvalFor(panelSettleBound, "the panel windows to go away",
		`(mapcar (lambda (w) (buffer-name (window-buffer w))) (window-list))`,
		func(raw json.RawMessage) bool {
			return panelBufferCount(decodeStrings(raw), frontendPrefix, panelPrefix) == 0
		})
	restored := e.EvalStrings(`(mapcar (lambda (w) (buffer-name (window-buffer w))) (window-list))`)
	if strings.Join(restored, "\x00") != strings.Join(work, "\x00") {
		t.Fatalf("the plain close left %q on the frame, want the work layout it replaced, %q", restored, work)
	}
	p.capture("work-layout-restored", "`SPC o c` (`agent-repl-simple`), the plain close",
		fmt.Sprintf("no panel window is on the frame and the work layout is back verbatim: %q", restored),
		"TWO plain text windows, one above the other, carrying the sentences about WORK WINDOW A and "+
			"WORK WINDOW B. No webapp and no composer. The tab bar across the top still lists the "+
			"workspace.")

	// BRANCH TWO: from an ordinary work window the key maximizes.
	e.Eval(`(progn (select-window (get-buffer-window (get-buffer ` + elispString(workB) + `))) t)`)
	e.Leader("w f")
	e.AwaitTrue("the fullscreen configuration to be recorded",
		`(and agent-repl--window-fullscreen-config t)`)
	maximized := e.EvalStrings(`(mapcar (lambda (w) (buffer-name (window-buffer w))) (window-list))`)
	if len(maximized) != 1 || maximized[0] != workB {
		t.Fatalf("the frame holds %q after SPC w f, want exactly the maximized %q", maximized, workB)
	}
	p.capture("work-window-fullscreen", "`SPC w f` pressed from the work window",
		fmt.Sprintf("`agent-repl--window-fullscreen-config` is non-nil and the frame holds ONE window, showing %q", workB),
		"ONE window fills the whole frame below the tab bar: the plain text window carrying the WORK "+
			"WINDOW B sentence. Work window A is GONE from the frame.")

	// AND THE SAME KEY RESTORES.
	e.Leader("w f")
	e.AwaitEval("the fullscreen configuration to be released",
		`(and agent-repl--window-fullscreen-config t)`,
		func(raw json.RawMessage) bool { return isJSONNull(raw) })
	after := e.EvalStrings(`(mapcar (lambda (w) (buffer-name (window-buffer w))) (window-list))`)
	if strings.Join(after, "\x00") != strings.Join(restored, "\x00") {
		t.Fatalf("the frame holds %q after restoring, want the layout the maximize replaced, %q", after, restored)
	}
	p.capture("fullscreen-restored", "the same key again, restoring the layout",
		fmt.Sprintf("`agent-repl--window-fullscreen-config` is nil and the frame shows the layout the maximize replaced, verbatim: %q", after),
		"The two-window work layout of the previous picture is back, unchanged: WORK WINDOW A above "+
			"WORK WINDOW B. The toggle RESTORED the layout rather than rebuilding some other one.")
}

// ---------------------------------------------------------------------------
// I.55 -- RELOAD AND RESCUE
// ---------------------------------------------------------------------------

// playtestMarker is a global stamped onto the page's `window` before a
// reload. It is how "the page was fetched again" is told apart from "the
// page is still the old one": a navigation makes a new document, and a new
// document does not carry the old one's globals.
const playtestMarker = `window.__agentReplPlaytestMarker`

// TestPlaytestReloadAndRescueWebview is plan I.55: `SPC o l` fetches the page
// again and its rows come back; `SPC o L` leaves a page that is home alone,
// and brings one that navigated away back with its rows.
//
// "State intact" is the feed's rows: a turn is run first so the page has
// something to lose, and every step after a navigation waits for BOTH of its
// bubbles to be drawn again before the picture.
func TestPlaytestReloadAndRescueWebview(t *testing.T) {
	t.Parallel()
	s := newPlaytestScenario(t, "18-reload-rescue",
		"Plan I.55. A turn is run so the feed holds rows, then `SPC o l` reloads the page and the rows "+
			"come back; `SPC o L` on a page that is home does nothing, and on a page that navigated "+
			"away brings it home with its rows.")
	p, e := s.Book, s.E

	s.register(t, s.repoAt(t, "repo").Dir)
	s.openPanel(t)

	const prompt = "draw one plain prose answer so the reload has rows to keep"
	const promptRow = `document.querySelector('[data-feed-row][data-row-kind="userPrompt"]')`
	const responseRow = `document.querySelector('[data-feed-row][data-row-kind="activity"][data-unit="response"][data-state="success"]')`
	s.submit(t, prompt)
	s.awaitInPage(t, "the user's prompt bubble to arrive", promptRow)
	s.awaitInPage(t, "the assistant's response bubble to settle", responseRow)
	s.awaitArm(t, s.Name, "the turn to settle", emGHISettledArms...)
	p.note("one plain-prose prompt submitted with composer RET and settled",
		"the feed holds the prompt bubble and a settled response bubble, which is the state the reload must keep")

	homeURI := s.webviewURI()
	if !e.EvalBool(`(and (agent-repl--frontend-webview-at-home-p ` + elispString(s.Name) + ` ` + elispString(homeURI) + `) t)`) {
		t.Fatalf("the webview's own URI %q is not at home before anything was done to it", homeURI)
	}

	// RELOAD. The marker proves a NEW document came back rather than the
	// old one being looked at again.
	s.stampMarker(t)
	if want, got := "agent-repl-frontend-reload-webview", e.LeaderBinding("o l"); got != want {
		t.Fatalf("SPC o l resolves to %q, want %q", got, want)
	}
	e.Leader("o l")
	s.awaitInPage(t, "a new document to replace the stamped one after the reload", playtestMarker+` === undefined`)
	s.awaitPageMounted(t)
	s.awaitInPage(t, "the prompt bubble to be drawn again on the reloaded page", promptRow)
	s.awaitInPage(t, "the response bubble to be drawn again on the reloaded page", responseRow)
	s.requireHome(t, homeURI)
	p.capture("reloaded", "`SPC o l` (`agent-repl-frontend-reload-webview`)",
		"the page's marker global is gone (a new document was fetched), the page mounted again, both "+
			"feed rows are drawn again, and the webview's URI is at the daemon's own origin",
		"The webapp is drawn with the SAME feed as before the reload: the user's prompt bubble on the "+
			"right, and the fake SDK's answer beneath it on the left -- a prose line and the bubble "+
			"echoing the prompt back -- with the workspace sidebar, the topbar and the footer status "+
			"word around them. Nothing is blank and no failure card is shown.")

	// RESCUE, WHEN HOME: nothing happens. The marker survives because no
	// navigation happened.
	s.stampMarker(t)
	if want, got := "agent-repl-frontend-rescue-webview", e.LeaderBinding("o L"); got != want {
		t.Fatalf("SPC o L resolves to %q, want %q", got, want)
	}
	if !e.EvalBool(`(null (agent-repl-frontend-rescue-webview ` + elispString(s.Name) + `))`) {
		t.Fatal("rescuing a webview that is home answered non-nil: it navigated a page that was already home")
	}
	s.awaitInPage(t, "the stamped document to still be the page after a rescue that had nothing to do", playtestMarker+` === true`)
	s.requireHome(t, homeURI)
	p.note("`SPC o L` (`agent-repl-frontend-rescue-webview`) while the page is home",
		"the command answered nil, the stamped document is still the one on screen (no navigation), and the URI is unchanged")

	// RESCUE, WHEN ASTRAY. The page is sent to an address that is not the
	// daemon's, the way an external hyperlink would send it, then rescued.
	e.Eval(`(let* ((buf (agent-repl--ws-get ` + elispString(s.Name) + ` :frontend-buffer))
                   (xw (agent-repl--frontend-webview-live-widget buf)))
              (agent-repl--frontend-webview-navigate-widget xw "about:blank")
              t)`)
	e.AwaitEvalFor(playtestPageBound, "the webview to report it has left the daemon",
		`(agent-repl--frontend-webview-current-uri `+elispString(s.Name)+`)`,
		func(raw json.RawMessage) bool {
			uri := decodeString(raw)
			return uri != "" && !strings.HasPrefix(uri, "http")
		})
	stray := s.webviewURI()
	if e.EvalBool(`(and (agent-repl--frontend-webview-at-home-p ` + elispString(s.Name) + ` ` + elispString(stray) + `) t)`) {
		t.Fatalf("the webview at %q still reads as home; the rescue would have nothing to do", stray)
	}
	p.note("the webview navigated to `about:blank`, the way an external hyperlink navigates it away",
		fmt.Sprintf("the webview's URI is %q, which `agent-repl--frontend-webview-at-home-p` refuses", stray))

	e.Leader("o L")
	e.AwaitEvalFor(playtestPageBound, "the webview to be back at the daemon's origin",
		`(and (agent-repl--frontend-webview-at-home-p `+elispString(s.Name)+` (agent-repl--frontend-webview-current-uri `+elispString(s.Name)+`)) t)`,
		func(raw json.RawMessage) bool { return !isJSONNull(raw) })
	s.awaitPageMounted(t)
	s.awaitInPage(t, "the prompt bubble to be drawn again on the rescued page", promptRow)
	s.awaitInPage(t, "the response bubble to be drawn again on the rescued page", responseRow)
	s.requireHome(t, homeURI)
	p.capture("rescued", "`SPC o L` (`agent-repl-frontend-rescue-webview`) on the astray page",
		"the webview's URI is back at the daemon's origin, the page mounted, and both feed rows are drawn again",
		"The webapp is back where the reload left it, picture for picture: the same prompt bubble and "+
			"the same two answer bubbles beneath it, with the sidebar, the topbar and the footer status "+
			"word. No trace of the blank page remains.")
}

// webviewURI reads the URI the workspace's webview currently shows.
func (s *playtestScenario) webviewURI() string {
	s.E.t.Helper()
	return s.E.EvalString(`(or (agent-repl--frontend-webview-current-uri ` + elispString(s.Name) + `) "")`)
}

// stampMarker sets the page's marker global and waits until the page
// answers that it holds it, so a later "the marker is gone" is a statement
// about a new document and not about a stamp that never landed.
func (s *playtestScenario) stampMarker(t *testing.T) {
	t.Helper()
	s.awaitInPage(t, "the page to hold the playtest marker", `(`+playtestMarker+` = true) === true`)
	s.awaitInPage(t, "the marker to read back", playtestMarker+` === true`)
}

// requireHome asserts the webview's URI is at the daemon's own origin, and
// that the origin is the one the page started at. The path and query are
// deliberately NOT compared: the page rewrites its own query as the user
// moves about the webapp, and a freshly built URL carries a different build
// stamp than a still-correct page; what home means is the daemon's origin
// (`agent-repl--frontend-webview-at-home-p`).
func (s *playtestScenario) requireHome(t *testing.T, homeURI string) {
	t.Helper()
	uri := s.webviewURI()
	if !s.E.EvalBool(`(and (agent-repl--frontend-webview-at-home-p ` + elispString(s.Name) + ` ` + elispString(uri) + `) t)`) {
		t.Fatalf("the webview's URI %q is not at the daemon's origin", uri)
	}
	if got, want := originOf(uri), originOf(homeURI); got != want {
		t.Fatalf("the webview's origin is %q, want the one it started at, %q", got, want)
	}
}

// originOf cuts a URL down to its scheme and authority.
func originOf(uri string) string {
	scheme, rest, ok := strings.Cut(uri, "://")
	if !ok {
		return uri
	}
	authority, _, _ := strings.Cut(rest, "/")
	return scheme + "://" + authority
}

// ---------------------------------------------------------------------------
// I.56 -- VISIT-FILE ROUTING
// ---------------------------------------------------------------------------

// TestPlaytestVisitFileRouting is plan I.56: a file under a registered
// worktree routes into its owning workspace, and a file no workspace can own
// records a refusal and still opens.
//
// Root detection goes through the module's own documented injection point
// (`agent-repl-find-file-workspace-root-function`), exactly as
// `emacs_findfile_e2e_test.go` does: the scripted fake git answers the
// daemon's git surface, not `rev-parse --show-toplevel`, and no real git may
// run anywhere. Both repositories live under HOME, which is this Emacs's
// root, because the routing refuses roots outside it.
func TestPlaytestVisitFileRouting(t *testing.T) {
	t.Parallel()
	s := newPlaytestScenario(t, "18-visit-file",
		"Plan I.56. Visiting a file under a registered worktree routes into its owning workspace; "+
			"visiting a file whose root no workspace can be created for records a refusal and still "+
			"opens the file.")
	p, e := s.Book, s.E

	owned := harness.NewRepoAt(t, filepath.Join(e.Root, "owned"))
	e.ArtifactPaths = append(e.ArtifactPaths, filepath.Join(owned.Dir, ".claude"))
	ws := s.register(t, owned.Dir)
	ownedFile := writePlaytestFile(t, owned.Dir, "owned.txt", "one\ntwo\nthree\n")

	// Leave the owning workspace first, so "switched to it" is a change the
	// visit caused rather than where Emacs already was.
	e.Eval(`(tab-bar-new-tab)`)
	e.Eval(`(let ((agent-repl-find-file-workspace-root-function
                    (lambda (_dir) ` + elispString(owned.Dir) + `)))
              (find-file ` + elispString(ownedFile) + `)
              t)`)
	e.AwaitEvalFor(emacsFindFileBound, "the owning workspace to be selected",
		`(format "%s" (agent-repl--ws-current-name))`,
		func(raw json.RawMessage) bool { return decodeString(raw) == ws })
	side := e.EvalString(`(let* ((buf (get-file-buffer ` + elispString(ownedFile) + `))
                                 (win (and buf (get-buffer-window buf t))))
                            (format "%s" (and win (window-parameter win 'window-side))))`)
	if side != "nil" {
		t.Fatalf("the routed file's window has window-side %q, want none: a file is not placed in a side window", side)
	}
	if !e.EvalBool(`(and (get-buffer-window (get-file-buffer ` + elispString(ownedFile) + `) t) t)`) {
		t.Fatalf("the routed file %s is not displayed", ownedFile)
	}
	p.capture("file-routed", "`find-file` on a file under the registered worktree, from another tab",
		fmt.Sprintf("the current workspace is %q (the owner), and the file's window is a main-area window, not a side window", ws),
		"The file's three lines (one, two, three) are shown in an ordinary main-area window, and the "+
			"tab bar marks the owning workspace as the selected one.")

	// UNROUTABLE: the create verb refuses, the refusal is recorded, and the
	// file is not lost.
	orphan := harness.NewRepoAt(t, filepath.Join(e.Root, "unowned"))
	orphanFile := writePlaytestFile(t, orphan.Dir, "orphan.txt", "no workspace owns me\n")
	e.Eval(`(let ((agent-repl-find-file-workspace-root-function
                    (lambda (_dir) ` + elispString(orphan.Dir) + `))
                  (agent-repl-find-file-workspace-create-function
                    (lambda (_root) (error "the workspace could not be created"))))
              (find-file ` + elispString(orphanFile) + `)
              t)`)
	refused := hashKeys(e, "agent-repl--ffw-refused")
	if len(refused) != 1 {
		t.Fatalf("agent-repl--ffw-refused = %v, want exactly the one refused root", refused)
	}
	if !e.EvalBool(`(and (get-file-buffer ` + elispString(orphanFile) + `)
                          (get-buffer-window (get-file-buffer ` + elispString(orphanFile) + `) t)
                          t)`) {
		t.Fatalf("the file %s is not displayed after the refusal: a refused routing falls through, it never loses the file", orphanFile)
	}
	if got := e.EvalString(`(format "%s" (agent-repl--ws-current-name))`); got != ws {
		t.Fatalf("the refused visit moved the current workspace to %q, want it left on %q", got, ws)
	}
	p.note("`find-file` on a file under a root no workspace can be created for",
		fmt.Sprintf("`agent-repl--ffw-refused` records exactly that root (%q), the file is displayed in an ordinary window, and the current workspace is still %q", refused[0], ws))
}

// writePlaytestFile writes an ordinary text file, the thing a user visits.
func writePlaytestFile(t *testing.T, dir, name, body string) string {
	t.Helper()
	path := filepath.Join(dir, name)
	if err := os.WriteFile(path, []byte(body), 0o644); err != nil {
		t.Fatalf("write %s: %v", path, err)
	}
	return path
}
