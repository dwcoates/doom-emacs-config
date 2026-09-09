//go:build playtest

package e2e

import (
	"encoding/json"
	"fmt"
	"path/filepath"
	"testing"
)

// OWNER 3's second file: PLAYTEST-PLAN.md A.8 (priority, and the deprio
// close's tab shuffle) and A.10 (copy workspace name, copy file reference).
//
// It shares `playtest_03_switch_and_close_test.go`'s helpers and nothing
// else; the split is length, not scope.

// ---------------------------------------------------------------------------
// A.8 -- PRIORITY, AND THE DEPRIO CLOSE'S SHUFFLE
// ---------------------------------------------------------------------------

// playtestPriorityLabel is the label the priority picker is answered with,
// and it is one of `agent-repl-verbs-priority-levels`' OWN drawn labels
// rather than a string invented here -- the command reads its level by
// matching the label, so a label that is not in that table would clear the
// priority instead of setting it and the step would pass for the wrong
// reason.
const playtestPriorityLabel = "P1"

// TestPlaytestPriorityAndDeprioClose is plan A.8: a priority set, the same
// priority cleared, and then the deprioritized close that pushes a tab to the
// END of the tab order.
//
// TWO BINDINGS IN THE PLAN LINE ARE NOT THE BINDINGS THE CODE HAS, and the
// code is what is asserted here:
//
//   - The plan writes the priority verb as `SPC TAB p`. `lisp/keybindings.el`
//     puts `agent-repl-set-priority` on `SPC j m p` (the "modify workspace"
//     prefix under `SPC j`), and there is no `SPC TAB p` entry at all. So
//     `SPC j m p` is what is looked up.
//   - The plan writes "deprio close". The deprioritizing close is
//     `SPC o C` -> `agent-repl`, whose close branch runs
//     `agent-repl--hide-and-preserve-status` -> `agent-repl--on-close` ->
//     `agent-repl-workspace-push-to-back`. It is NOT `agent-repl-close-workspace`
//     (`SPC j d`), which is the roster-side view close and touches no order.
//
// THE SHUFFLE IS READ IN THE SAME EVAL THAT PERFORMS IT, and that is
// structural rather than tidy. `agent-repl-roster-move-tab-to-back`'s own
// docstring says the next accepted roster push RE-DERIVES the order from the
// daemon's walk -- so the local shuffle is by design a paint that a later
// push overwrites. A second eval could therefore read either answer, and a
// poll could be satisfied by either; asking for the order inside the same
// synchronous form as the command is the only read that cannot be racing the
// push.
func TestPlaytestPriorityAndDeprioClose(t *testing.T) {
	t.Parallel()
	s := newPlaytestScenario(t, "03-priority",
		"Plan A.8. A workspace's priority set and cleared through its own verb, and the "+
			"deprioritizing close that pushes a tab to the LAST slot of the tab order.")
	p, e := s.Book, s.E

	first := s.repoAt(t, "repo-alpha")
	firstName := s.register(t, first.Dir)
	s.openPanel(t)
	awaitPanelShown(t, s, firstName)
	second := s.repoAt(t, "repo-beta")
	s.register(t, second.Dir)
	third := s.repoAt(t, "repo-gamma")
	thirdName := s.register(t, third.Dir)
	// THE PANEL IS OPENED ON THE WORKSPACE THE PRIORITY GOES ON, because the
	// surface that draws a priority is the WEBAPP'S SIDEBAR ROW
	// (`webapp/src/sidebar/row.ts` puts the resolver's composed label on
	// `[data-priority]`), and a page of some other workspace would draw the
	// badge in a webview nobody is looking at.
	e.AwaitEval("the third workspace to be the selected one after its registration",
		`(format "%s" (agent-repl--ws-current-name))`,
		func(raw json.RawMessage) bool { return decodeString(raw) == thirdName })
	enterWorkspace(t, s, thirdName)
	// THREE workspaces, because "moves to the END" is only a claim when the
	// moved tab was not already last. The one that will be pushed is the
	// FIRST of the three.
	e.AwaitEval("all three workspaces to be on the tab bar",
		emacsWSTablineNamesForm,
		func(raw json.RawMessage) bool { return len(decodeStrings(raw)) == 3 })
	before := s.tabNames()
	if before[0] != firstName {
		t.Fatalf("the tab bar draws %v, want %q first: the shuffle's subject must not already be last",
			before, firstName)
	}
	p.note("three repositories registered, and the first one's panel opened",
		fmt.Sprintf("`agent-repl--ws-tabline-names` is %v, so the shuffle's subject %q is at the FRONT",
			before, firstName))

	// `agent-repl-set-priority` reads its LEVEL through `completing-read`, so
	// the pick is stubbed for the duration of the one call the way this layer
	// stubs every interactive pick; the WORKSPACE is passed as its argument.
	// The binding is looked up rather than pressed for exactly that reason.
	if want, got := "agent-repl-set-priority", e.LeaderBinding("j m p"); got != want {
		t.Fatalf("SPC j m p resolves to %q, want %q (the plan writes `SPC TAB p`, which is not bound)", got, want)
	}
	e.Eval(`(cl-letf (((symbol-function 'completing-read)
                        (lambda (_prompt candidates &rest _)
                          (unless (member ` + elispString(playtestPriorityLabel) + ` candidates)
                            (error "the priority picker offered %S, not the level this step sets" candidates))
                          ` + elispString(playtestPriorityLabel) + `)))
               (agent-repl-set-priority ` + elispString(thirdName) + `)
               t)`)

	// THE ACCESSOR IS THE MODULE'S OWN: `agent-repl-roster-row-priority-label`
	// reads the badge label off the roster row, so a set priority is one the
	// DAEMON acknowledged and pushed back rather than one Emacs remembers
	// having asked for.
	label := priorityLabelForm(thirdName)
	set := e.AwaitEval(fmt.Sprintf("the roster to carry a priority badge for %q", thirdName),
		label, func(raw json.RawMessage) bool { return decodeString(raw) != "" })
	// AND THE PAGE DREW IT. The roster label is the daemon's answer; this is
	// that answer rendered, on the one surface that renders it.
	s.awaitInPage(t, "the sidebar's roster row to draw the priority badge",
		`document.querySelector('[data-roster-row] [data-priority=`+jsString(decodeString(set))+`]')`)
	// AND THE TAB BAR'S OWN STRING CARRIES IT, which is the claim the PICTURE
	// is about and the one the roster accessor cannot make.
	//
	// `agent-repl--tab-badge-str` puts the roster row's priority label in the
	// run drawn BEFORE a tab's name, so the label the daemon acknowledged must
	// be IN the line `tab-bar-format` renders. Reading that line -- the way
	// `tabFaceFor` reads its faces -- is what makes the manifest sentence rest
	// on what the module WROTE for the bar rather than on what some other
	// surface was told. It also splits the failure: a line without the label
	// is a defect in `status.el` before any redisplay is involved, and a line
	// WITH it beside a picture without it is a defect in the paint.
	setLabel := decodeString(set)
	if drawn := tablineDrawn(t, s); !strings.Contains(drawn, setLabel) {
		t.Fatalf("the drawn tabline is %q and carries no %q: `agent-repl-roster-row-priority-label` "+
			"reports %q for %q, and `agent-repl--tab-badge-str` draws that label before the tab's name, "+
			"so the bar's own string must carry it", drawn, setLabel, setLabel, thirdName)
	}
	p.capture("priority-set", "`agent-repl-set-priority` answered with "+playtestPriorityLabel+
		" for the third workspace",
		fmt.Sprintf("`agent-repl-roster-row-priority-label` reports %q for %q -- so the daemon "+
			"acknowledged the priority and pushed it back -- and `agent-repl-workspace-tabline-formatted`, "+
			"the function installed in `tab-bar-format`, wrote a line that CARRIES %q: %q",
			setLabel, thirdName, setLabel, tablineDrawn(t, s)),
		fmt.Sprintf("The tab bar carries all THREE workspace tabs, with %q highlighted, and %q's tab "+
			"carries a %q PRIORITY BADGE before its name — the bar's own string carries that label, so a "+
			"tab without it here is the paint failing to take a string that was already right. The "+
			"webapp's WORKSPACES sidebar lists all three workspaces too, and %q's sidebar row carries the "+
			"same %q badge beside its name while the other two rows carry none.",
			thirdName, thirdName, setLabel, thirdName, setLabel))

	// CLEARING IS THE ABSENCE OF THE FIELD, never a sentinel level, and
	// `agent-repl-verbs-priority-clear-label` is the entry that spells it.
	e.Eval(`(cl-letf (((symbol-function 'completing-read)
                        (lambda (_prompt candidates &rest _)
                          (unless (member agent-repl-verbs-priority-clear-label candidates)
                            (error "the priority picker offered %S, with no clear entry" candidates))
                          agent-repl-verbs-priority-clear-label)))
               (agent-repl-set-priority ` + elispString(thirdName) + `)
               t)`)
	e.AwaitEval(fmt.Sprintf("the roster's priority badge for %q to go away", thirdName),
		label, func(raw json.RawMessage) bool { return decodeString(raw) == "" })
	s.awaitInPage(t, "the sidebar's roster rows to draw NO priority badge at all",
		`document.querySelectorAll('[data-roster-row] [data-priority]').length === 0`)
	p.capture("priority-cleared", "`agent-repl-set-priority` answered with the clear entry",
		fmt.Sprintf("`agent-repl-roster-row-priority-label` reports nothing for %q: clearing is the "+
			"ABSENCE of the field and the daemon pushed that absence back", thirdName),
		fmt.Sprintf("The same THREE tabs in the same order and the same three sidebar rows, and NO "+
			"priority badge anywhere — %q's row has lost the one it carried in `priority-set`. Clearing "+
			"is the absence of the field, so the bar and the sidebar look exactly as they did before the "+
			"priority was ever set.", thirdName))

	// The push acts on the CURRENT workspace, so the subject is selected
	// first -- and that switch is asserted, or the shuffle below would move
	// whichever workspace happened to be current.
	e.Eval(`(agent-repl-switch-to-project ` + elispString(first.Dir) + `)`)
	e.AwaitEval("the shuffle's subject to be the selected workspace",
		`(format "%s" (agent-repl--ws-current-name))`,
		func(raw json.RawMessage) bool { return decodeString(raw) == firstName })
	enterWorkspace(t, s, firstName)

	if want, got := "agent-repl", e.LeaderBinding("o C"); got != want {
		t.Fatalf("SPC o C resolves to %q, want %q: the deprioritizing close is `agent-repl`", got, want)
	}
	// ONE FORM: the command and the read of the order it produced. See the
	// docstring above -- a later roster push re-derives this order by design,
	// so a second eval would be reading a different question's answer.
	shuffled := e.EvalStrings(`(progn (agent-repl) (agent-repl--ws-tabline-names))`)
	if len(shuffled) != len(before) {
		t.Fatalf("the tab order is %v after the deprio close, want the same %d tabs as %v: the push "+
			"reorders the bar and takes nothing off it", shuffled, len(before), before)
	}
	if shuffled[len(shuffled)-1] != firstName {
		t.Fatalf("the tab order is %v after the deprio close, want %q LAST: `agent-repl-workspace-push-to-back` "+
			"moves the tab to the last slot and any slot short of last would leave it ahead of a workspace "+
			"it was just ranked below", shuffled, firstName)
	}
	// AND FOCUS MOVED, which is the point of the gesture: the user said they
	// are done here. The workspace that now occupies the vacated front slot
	// is the one that gets it.
	e.AwaitEval("focus to move off the workspace that was pushed to the back",
		`(format "%s" (agent-repl--ws-current-name))`,
		func(raw json.RawMessage) bool { return decodeString(raw) != firstName })
	// AND THE ORDER IS RE-READ AT CAPTURE TIME, for the same reason
	// `captureArm` re-reads an arm there: the shuffle is a transient by the
	// product's own design, so a sentence written from the atomic read alone
	// would send a reviewer looking for an order the module had already,
	// correctly, stopped holding.
	atCapture := s.tabNames()
	focus := e.EvalString(`(format "%s" (agent-repl--ws-current-name))`)
	p.capture("deprio-shuffled-to-the-end", "`SPC o C` (`agent-repl`) pressed on the front workspace",
		fmt.Sprintf("`agent-repl--ws-tabline-names`, read in the SAME form that ran the command, is %v: "+
			"%q moved from the FRONT to the LAST slot and no tab was lost. Read again at capture time it "+
			"is %v, and the selected workspace is %q.", shuffled, firstName, atCapture, focus),
		fmt.Sprintf("The tab bar still carries all THREE tabs, and the highlight is on %q rather than on "+
			"%q: the deprio gesture moved the user on, which is the half of it that lasts. "+
			"THE ORDER DRAWN HERE IS %v, and that is what the reviewer should see — NOT the shuffled %v. "+
			"OBSERVED AND FILED, NOT ASSERTED: `agent-repl-workspace-push-to-back` moves the tab to the "+
			"last slot, and the next accepted roster push then RE-DERIVES the order from the daemon's "+
			"walk (`agent-repl-roster-move-tab-to-back`'s own docstring says so, and `workspace.el` "+
			"declares client-authored ordering dead). So the shuffle is overwritten before a picture can "+
			"be taken of it, and `SPC o C`'s reordering has no lasting visible effect at all. Whether "+
			"that gesture should still reorder anything is a contract question about who owns tab order, "+
			"so it is filed rather than changed here.",
			focus, firstName, atCapture, shuffled))
}

// priorityLabelForm reads WS's priority badge label off its roster row, as a
// string, answering the empty string when the row carries no priority.
//
// It is the module's own accessor (`agent-repl-roster-row-priority-label`),
// which is why an unprioritized workspace answers "" rather than the reader
// having to distinguish nil from a level.
func priorityLabelForm(ws string) string {
	return `(let ((row (agent-repl-roster-row-for-ws ` + elispString(ws) + `)))
             (format "%s" (or (and row (agent-repl-roster-row-priority-label row)) "")))`
}

// ---------------------------------------------------------------------------
// A.10 -- COPY WORKSPACE NAME, COPY FILE REFERENCE
// ---------------------------------------------------------------------------

// playtestCopyFile is the file the reference is taken from, and
// playtestCopyLine is the line point sits on when it is taken. The expected
// reference is those two spliced by `agent-repl--format-file-ref`'s own
// `file:line` shape, which is what the assertion below is written from.
const (
	playtestCopyFile = "playtest-copy.txt"
	playtestCopyLine = 2
)

// TestPlaytestCopyNameAndReference is plan A.10: two copy verbs that must
// touch NOTHING but the kill ring and the echo area.
//
// THE NEGATIVE CAPTURE IS THE POINT. Both verbs are pure reads of state the
// user is already looking at, so their entire visible effect is one echo-area
// line that has already scrolled past by the time a picture is taken. The
// manifest's last sentence therefore asks the reviewer for the one thing a
// screenshot can say here: that the picture is INDISTINGUISHABLE from the one
// before it. A copy verb that reordered a tab, changed a highlight, moved a
// window or reopened a panel would be a defect, and only a picture of the
// whole frame can rule all four out at once.
func TestPlaytestCopyNameAndReference(t *testing.T) {
	t.Parallel()
	s := newPlaytestScenario(t, "03-copy",
		"Plan A.10. Copy workspace name and copy file reference: the kill ring and the echo area "+
			"change, and NOTHING ELSE does — which is what the negative capture is for.")
	p, e := s.Book, s.E

	repository := s.repoAt(t, "repo")
	name := s.register(t, repository.Dir)
	s.openPanel(t)

	// The file is minted and visited BEFORE the baseline picture, so the two
	// captures differ in nothing but the two copy verbs between them: a
	// `find-file` after the baseline would change the window layout and make
	// the negative capture unreadable.
	path := filepath.Join(repository.Dir, playtestCopyFile)
	fileBuffer := e.EvalString(`(let ((f ` + elispString(path) + `))
                                  (with-temp-file f (insert "one\ntwo\nthree\n"))
                                  (find-file f)
                                  (goto-char (point-min))
                                  (forward-line ` + fmt.Sprint(playtestCopyLine-1) + `)
                                  (buffer-name))`)
	if fileBuffer == "" {
		t.Fatalf("visiting %s answered no buffer name", path)
	}
	e.AwaitEval("the workspace's own file to be the visited buffer with point on its second line",
		`(with-current-buffer `+elispString(fileBuffer)+`
                   (and (equal (agent-repl--buffer-relative-path) `+elispString(playtestCopyFile)+`)
                        (= (line-number-at-pos (point)) `+fmt.Sprint(playtestCopyLine)+`)
                        t))`,
		func(raw json.RawMessage) bool { return !isJSONNull(raw) })
	baseline := s.tabNames()
	p.capture("before-the-copies", "a file inside the workspace visited, with point on its second line",
		fmt.Sprintf("`agent-repl--buffer-relative-path` is %q and point is on line %d; "+
			"`agent-repl--ws-tabline-names` is %v", playtestCopyFile, playtestCopyLine, baseline),
		fmt.Sprintf("THIS IS THE BASELINE THE NEXT PICTURE IS COMPARED AGAINST. The frame carries the "+
			"tab bar with %v on it and a window showing the file %q. Note what it looks like: the last "+
			"capture in this playbook must be indistinguishable from it.", baseline, playtestCopyFile))

	// COPY WORKSPACE NAME. It prompts for nothing, so it is PRESSED.
	if want, got := "agent-repl-copy-workspace-name", e.LeaderBinding("j h y"); got != want {
		t.Fatalf("SPC j h y resolves to %q, want %q", got, want)
	}
	e.KeysIn(fileBuffer, "SPC j h y")
	if got := e.EvalString(`(format "%s" (current-kill 0))`); got != name {
		t.Fatalf("(current-kill 0) is %q after `SPC j h y`, want the workspace name %q", got, name)
	}
	// The echo is read out of `*Messages*` rather than `current-message`: the
	// press and the read are two round trips, and the echo area has been
	// redisplayed between them. `*Messages*` is this layer's sanctioned
	// rendered exception, and the workspace name makes the line unambiguous.
	wantNameEcho := "Copied workspace name: " + name
	if !e.EvalBool(`(with-current-buffer "*Messages*"
                        (and (string-match-p (regexp-quote ` + elispString(wantNameEcho) + `) (buffer-string)) t))`) {
		t.Fatalf("*Messages* carries no %q line: the copy's whole visible effect is that echo", wantNameEcho)
	}
	assertCopyChangedNothing(t, s, name, baseline)
	p.note("`SPC j h y` (`agent-repl-copy-workspace-name`) pressed",
		fmt.Sprintf("`(current-kill 0)` is %q, `*Messages*` carries %q, and the tab names and the "+
			"current workspace are UNCHANGED", name, wantNameEcho))

	// COPY FILE REFERENCE, from the file buffer, with no region: the shape is
	// `file:line` by `agent-repl--format-file-ref`'s own contract.
	if want, got := "agent-repl-copy-reference", e.LeaderBinding("o r"); got != want {
		t.Fatalf("SPC o r resolves to %q, want %q", got, want)
	}
	e.KeysIn(fileBuffer, "SPC o r")
	wantRef := fmt.Sprintf("%s:%d", playtestCopyFile, playtestCopyLine)
	if got := e.EvalString(`(format "%s" (current-kill 0))`); got != wantRef {
		t.Fatalf("(current-kill 0) is %q after `SPC o r`, want the reference %q", got, wantRef)
	}
	if !e.EvalBool(`(with-current-buffer "*Messages*"
                        (and (string-match-p (regexp-quote ` + elispString("Copied: "+wantRef) + `) (buffer-string)) t))`) {
		t.Fatalf("*Messages* carries no %q line: the copy's whole visible effect is that echo", "Copied: "+wantRef)
	}
	assertCopyChangedNothing(t, s, name, baseline)
	p.note("`SPC o r` (`agent-repl-copy-reference`) pressed in that file buffer",
		fmt.Sprintf("`(current-kill 0)` is %q — the `file:line` shape — `*Messages*` carries "+
			"\"Copied: %s\", and the tab names and the current workspace are UNCHANGED", wantRef, wantRef))

	p.capture("after-the-copies-unchanged", "nothing further done; the two copy verbs are the only acts since the baseline",
		fmt.Sprintf("the kill ring's head moved to %q and `*Messages*` gained two lines, while "+
			"`agent-repl--ws-tabline-names` is still %v and `agent-repl--ws-current-name` is still %q",
			wantRef, baseline, name),
		"THE NEGATIVE CAPTURE. This picture must look IDENTICAL to `before-the-copies`: the same tab "+
			"bar with the same tabs in the same order and the same highlight, the same windows in the "+
			"same places, and the same file on screen. The only thing either copy verb may change is "+
			"the kill ring and one echo-area line, and the echo area has already been redrawn. ANY "+
			"visible difference between these two pictures is a defect.")
}

// assertCopyChangedNothing is A.10's negative assertion, made after each copy
// verb: the roster's tabs and the selected workspace are exactly what they
// were. A copy is a READ, and a read that reorders the bar or moves the
// selection is a defect the picture alone would only hint at.
func assertCopyChangedNothing(t *testing.T, s *playtestScenario, ws string, baseline []string) {
	t.Helper()
	if got := s.tabNames(); !sameStrings(got, baseline) {
		t.Fatalf("`agent-repl--ws-tabline-names` is %v after a copy verb, want the unchanged %v: "+
			"copying is a read and touches no roster state", got, baseline)
	}
	if got := s.E.EvalString(`(format "%s" (agent-repl--ws-current-name))`); got != ws {
		t.Fatalf("`agent-repl--ws-current-name` is %q after a copy verb, want the unchanged %q", got, ws)
	}
}

// sameStrings answers whether two name lists are equal element for element.
// Order matters: the tab bar's order IS part of what a copy verb must not
// change.
func sameStrings(a, b []string) bool {
	if len(a) != len(b) {
		return false
	}
	for i := range a {
		if a[i] != b[i] {
			return false
		}
	}
	return true
}
