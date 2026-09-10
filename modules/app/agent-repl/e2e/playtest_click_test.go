//go:build playtest

package e2e

import (
	"strings"
	"testing"
)

// The substrate's click builder, `pageClickOnce`.
//
// THE DEFECT IT EXISTS FOR. The page probe is two evals: every poll RE-ISSUES
// the script and answers what the previous issue's callback stored. A
// predicate does not care how often it runs; a click does. Measured from the
// page's own client log in the permission-mode picker's playbook, the mode
// reveal opened at 02:14:42.173 and closed at 02:14:42.195 with no topbar
// push in between -- the second poll clicked the toggle again.
//
// The behaviour in the real page is pinned by
// `TestPlaytestPermissionModePicker`, which opens a real toggle with one
// `clickOnce` and asserts the reveal is still open. What is checked here is
// the part that can be checked without a page: that the script cannot click
// twice for one act, and that two acts are two tokens.

func TestPageClickOnceGuardsTheClick(t *testing.T) {
	// ARRANGE / ACT.
	script := pageClickOnce(`document.querySelector('.topbar-mode-button')`)

	// ASSERT: the token is consulted before anything is clicked, recorded
	// before the click, and the click is the last thing that happens.
	guard := strings.Index(script, "if (done[")
	record := strings.Index(script, "] = true;")
	click := strings.Index(script, "el.click();")
	if guard < 0 || record < 0 || click < 0 {
		t.Fatalf("pageClickOnce built a script without a guard, a record and a click:\n%s", script)
	}
	if !(guard < record && record < click) {
		t.Fatalf("pageClickOnce orders guard=%d record=%d click=%d, want guard before record before click:\n%s",
			guard, record, click, script)
	}
}

func TestPageClickOnceLooksUpTheElementAfterTheGuard(t *testing.T) {
	// ARRANGE / ACT: an expression that would be wrong to re-evaluate once
	// the act is done -- an ordinal into a live NodeList.
	const expr = `document.querySelectorAll('[data-feed-row]')[3]`
	script := pageClickOnce(expr)

	// ASSERT: the element is looked up only past the guard, so a re-issue
	// after the click never touches the page at all.
	if strings.Index(script, expr) < strings.Index(script, "if (done[") {
		t.Fatalf("pageClickOnce evaluates the element expression before its guard:\n%s", script)
	}
}

func TestPageClickOnceMintsOneTokenPerAct(t *testing.T) {
	// ARRANGE / ACT: two acts on the SAME element.
	const expr = `document.querySelector('#same')`
	first, second := pageClickOnce(expr), pageClickOnce(expr)

	// ASSERT: two clicks on one element are two acts, so the second is not
	// swallowed by the first's record.
	if first == second {
		t.Fatalf("two acts built the same script, so the second click would be a no-op:\n%s", first)
	}
}
