//go:build playtest

package e2e

import (
	"strings"
	"testing"
)

// THE SUBSTRATE'S OWN UNIT TESTS. Host-only, no Emacs, no sandbox: what is
// under test here is the WORDING and the ESCAPING every owner's manifest
// inherits, and both are pure functions of strings.

// TestPlaytestArmSentenceNamesTheIndexBadgeAndNeverADisc holds the inherited
// sentence to what `agent-repl--render-tab` actually paints: the arm's color
// lands on the bracketed index drawn BEFORE the name, and the module draws no
// status disc anywhere.
func TestPlaytestArmSentenceNamesTheIndexBadgeAndNeverADisc(t *testing.T) {
	// Every arm the tab-bar color table carries, with the color name that
	// table gives it -- the `none` family included, because that arm is the
	// one whose sentence is written by the other branch.
	tests := []struct {
		name string
		arm  string
		col  string
	}{
		{"idle is green", ":idle", "green"},
		{"working is yellow", ":working", "yellow"},
		{"attention is red", ":attention", "red"},
		{"vendor-blocked is purple", ":vendor-blocked", "purple"},
		{"merging is purple in the tab bar", ":merging", "purple"},
		{"merge-queued is purple in the tab bar", ":merge-queued", "purple"},
		{"merge-running is purple in the tab bar", ":merge-running", "purple"},
		{"none is the uncolored arm", ":none", "none"},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			got := armSentence("ws-a", tc.arm, tc.col)

			if strings.Contains(strings.ToLower(got), "disc") {
				t.Errorf("the sentence for %s says %q; it names a status disc, and the module paints "+
					"the bracketed index badge instead", tc.arm, got)
			}
			if !strings.Contains(got, tabBadgeShape) {
				t.Errorf("the sentence for %s says %q; it never names the %s index badge the arm color "+
					"is actually carried on", tc.arm, got, tabBadgeShape)
			}
			if !strings.Contains(got, tc.arm) {
				t.Errorf("the sentence for %s says %q; it never names the arm", tc.arm, got)
			}
		})
	}
}

// TestPlaytestAColoredArmSentenceSaysTheNameCarriesSelectionNotTheArm keeps
// the half of the correction that stops a reviewer reading the NAME's color
// as the arm's.
func TestPlaytestAColoredArmSentenceSaysTheNameCarriesSelectionNotTheArm(t *testing.T) {
	got := armSentence("ws-a", ":idle", "green")

	if !strings.Contains(got, "selection") {
		t.Errorf("the colored-arm sentence says %q, and never tells the reviewer the name beside the "+
			"badge carries the SELECTION face rather than the arm color", got)
	}
}
