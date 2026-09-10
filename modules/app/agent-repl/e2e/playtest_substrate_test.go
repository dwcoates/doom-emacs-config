//go:build playtest

package e2e

import (
	"os"
	"path/filepath"
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
			got := armSentence("ws-a", tc.arm, tc.col, tabPaint{BracketBg: "#111111", NameBg: "#111111"})

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

// TestPlaytestASelectedColoredArmSentenceSaysTheNameCarriesSelectionNotTheArm
// keeps the half of the correction that stops a reviewer reading the SELECTED
// tab's name color as its arm's: `agent-repl--tab-face` hands that one name
// region Doom's selected-tab face, so the arm reaches the badge and stops.
func TestPlaytestASelectedColoredArmSentenceSaysTheNameCarriesSelectionNotTheArm(t *testing.T) {
	got := armSentence("ws-a", ":idle", "green", tabPaint{Selected: true, SelectedBg: "#c0c0c0", BracketBg: "#1a7a1a", NameBg: "#b4eeb4"})

	if !strings.Contains(got, "SELECTION") {
		t.Errorf("the selected colored-arm sentence says %q, and never tells the reviewer the name "+
			"beside the badge carries the SELECTION rather than the arm color", got)
	}
}

// TestPlaytestAnUnselectedColoredArmSentenceSaysTheWholeEntryCarriesTheArm is
// the other half, and it is the one a reviewer was being lied to about.
//
// An UNSELECTED tab takes its palette row's `:bg`, which IS the arm color, so
// `agent-repl--render-tab` paints its bracket AND its name region with it.
// Measured off owner 20's K.61 capture at the tab bar's own scanline: the
// unselected `:thinking` tab ran `#cc3333` unbroken from x=265 to x=378,
// bracket through name, while the sentence said the name did not carry the arm
// color -- a manifest telling a reviewer that the correct picture is a defect.
func TestPlaytestAnUnselectedColoredArmSentenceSaysTheWholeEntryCarriesTheArm(t *testing.T) {
	got := armSentence("ws-a", ":idle", "green", tabPaint{BracketBg: "#1a7a1a", NameBg: "#1a7a1a"})

	if !strings.Contains(got, "WHOLE ENTRY") {
		t.Errorf("the unselected colored-arm sentence says %q, and never tells the reviewer the arm "+
			"color reaches the name region too", got)
	}
	if strings.Contains(got, "SELECTED") {
		t.Errorf("the unselected colored-arm sentence says %q; it describes a selected tab's dimming "+
			"on a tab that is not selected", got)
	}
}

// TestPlaytestAManifestCellCarryingAPipeIsEscapedAndTheTableKeepsItsColumns
// is the second defect: `tabFaceFor` joins faces with " | ", the joined
// string is written into a table cell, and an unescaped pipe silently splits
// that row into extra columns.
func TestPlaytestAManifestCellCarryingAPipeIsEscapedAndTheTableKeepsItsColumns(t *testing.T) {
	// Arrange: a playbook whose manifest is a plain file -- no Emacs is
	// involved in writing a row, so none is started.
	dir := t.TempDir()
	path := filepath.Join(dir, playtestManifestFile)
	file, err := os.Create(path)
	if err != nil {
		t.Fatalf("create the manifest under test: %v", err)
	}
	t.Cleanup(func() {
		if err := file.Close(); err != nil {
			t.Errorf("close the manifest under test: %v", err)
		}
	})
	p := &playbook{t: t, Name: "unit", Dir: dir, manifest: file}
	p.write("| # | the act | asserted | image | what the image must show |\n|---|---|---|---|---|\n")

	// Act: a row whose "asserted" cell carries the exact join tabFaceFor
	// produces.
	face := `(:background "#98be65") | doom-modeline-panel`
	p.note("switch to ws-a", "the drawn tabline carries "+face)

	// Assert.
	body, err := os.ReadFile(path)
	if err != nil {
		t.Fatalf("read the manifest under test: %v", err)
	}
	lines := strings.Split(strings.TrimRight(string(body), "\n"), "\n")
	row := lines[len(lines)-1]

	if !strings.Contains(row, `\|`) {
		t.Errorf("the row is %q; the pipe in the face join was not escaped", row)
	}
	if want, got := columnCount(lines[0]), columnCount(row); got != want {
		t.Errorf("the row splits into %d columns, want the header's %d: %q", got, want, row)
	}
}

// columnCount counts a markdown row's cells the way a renderer does: on
// UNESCAPED pipes only.
func columnCount(row string) int {
	n := 0
	for i := 0; i < len(row); i++ {
		if row[i] == '\\' {
			i++
			continue
		}
		if row[i] == '|' {
			n++
		}
	}
	return n
}

// TestPlaytestAnUnselectedBracketOnlyArmSentenceStopsTheColorAtTheBadge is the
// third way a tab is painted, and the one that made a correct picture read as
// a defect.
//
// `agent-repl--render-tab-entry` takes the BRACKET-ONLY spec whenever
// `agent-repl--ws-display-state` suppresses the full-tab color -- a workspace
// whose panels are dismissed, or a `:ready` one already viewed -- and
// `agent-repl--tab-spec-bracket-only` then leaves `:bg` unspecified, so the
// arm reaches the badge and stops even though the tab is not selected.
// Measured, in owner 20's K.63: the `:merging` workspace's badge was `#a21caf`
// while its name region was `#14141a`, under a sentence promising purple
// across the whole entry.
func TestPlaytestAnUnselectedBracketOnlyArmSentenceStopsTheColorAtTheBadge(t *testing.T) {
	got := armSentence("ws-a", ":merging", "purple",
		tabPaint{BracketBg: "#a21caf", NameBg: "#14141a"})

	if strings.Contains(got, "WHOLE ENTRY") {
		t.Errorf("the bracket-only sentence says %q; it promises the whole entry is the arm color on a "+
			"tab whose name region is a different background entirely", got)
	}
	if !strings.Contains(got, "ALONE") {
		t.Errorf("the bracket-only sentence says %q, and never tells the reviewer the arm color stops "+
			"at the badge", got)
	}
	if !strings.Contains(got, "#14141a") {
		t.Errorf("the bracket-only sentence says %q, and never names the ground the name region is "+
			"actually drawn on", got)
	}
	if strings.Contains(got, "SELECTION") {
		t.Errorf("the bracket-only sentence says %q; it blames the selection for a tab that is not "+
			"selected, and the reviewer would then look for a highlight that is not there", got)
	}
}

// TestPlaytestASelectedNoneArmBadgeSaysTheDarkerGroundIsTheSelection is the
// half a reviewer would otherwise file as a defect: a `none`-arm badge on the
// SELECTED tab sits on `agent-repl--color-selected-bg`, visibly darker than
// the bar, and a sentence promising "the tab bar's own ordinary background"
// says the product is painting something it should not be.
func TestPlaytestASelectedNoneArmBadgeSaysTheDarkerGroundIsTheSelection(t *testing.T) {
	const selectedBg = "#c0c0c0"

	got := armSentence("ws-a", ":none", "none", tabPaint{Selected: true, SelectedBg: selectedBg, BracketBg: selectedBg, NameBg: "#b4eeb4"})

	if !strings.Contains(got, selectedBg) {
		t.Errorf("the selected none-arm sentence says %q, and never names the %s ground the badge is "+
			"actually drawn on", got, selectedBg)
	}
	if !strings.Contains(got, "SELECTION") {
		t.Errorf("the selected none-arm sentence says %q, and never tells the reviewer the darker "+
			"ground is the selection rather than an arm color", got)
	}
}

// TestPlaytestAnUnselectedNoneArmBadgeSaysTheBarsOwnBackground is the other
// case, and it must not borrow the selected wording: an unselected tab's badge
// really is on the bar's own ground, and naming a grey there would send a
// reviewer hunting a darker patch that is not in the picture.
func TestPlaytestAnUnselectedNoneArmBadgeSaysTheBarsOwnBackground(t *testing.T) {
	got := armSentence("ws-a", ":none", "none", tabPaint{BracketBg: "unspecified", NameBg: "#14141a"})

	if !strings.Contains(got, "the tab bar's own ordinary background") {
		t.Errorf("the unselected none-arm sentence says %q, and never names the bar's own background", got)
	}
	if strings.Contains(got, "SELECTED") {
		t.Errorf("the unselected none-arm sentence says %q; it describes a selected tab's darker "+
			"ground on a tab that is not selected", got)
	}
}
