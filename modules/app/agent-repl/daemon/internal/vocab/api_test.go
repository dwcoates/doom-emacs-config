package vocab

import (
	"encoding/json"
	"os"
	"path/filepath"
	"sort"
	"testing"

	frontendv1 "agentrepl/proto/frontend/v1"
)

// repoVocabDir is the checked-in vocabulary directory, which is the contract
// every test here asserts against.
const repoVocabDir = "../../../proto/vocab"

// mergeArms is the merge pipeline's RosterRow.status arms: the arms that take
// a glyph treatment instead of a lifecycle color.
var mergeArms = []string{
	"merging",
	"merge_queued",
	"merge_failed",
	"merged",
}

func loadColors(t *testing.T) RenderColors {
	t.Helper()
	c, err := LoadRenderColors(repoVocabDir)
	if err != nil {
		t.Fatalf("LoadRenderColors: %v", err)
	}
	return c
}

// writeColors renders a render-colors.json into a temp dir so a test can state
// exactly one malformed row.
func writeColors(t *testing.T, mutate func(m map[string]any)) string {
	t.Helper()
	raw, err := os.ReadFile(filepath.Join(repoVocabDir, RenderColorsFile))
	if err != nil {
		t.Fatalf("reading the contract: %v", err)
	}
	var m map[string]any
	if err := json.Unmarshal(raw, &m); err != nil {
		t.Fatalf("parsing the contract: %v", err)
	}
	mutate(m)
	dir := t.TempDir()
	out, err := json.Marshal(m)
	if err != nil {
		t.Fatalf("rendering: %v", err)
	}
	if err := os.WriteFile(filepath.Join(dir, RenderColorsFile), out, 0o644); err != nil {
		t.Fatalf("writing: %v", err)
	}
	return dir
}

func TestLoadRenderColorsReadsTheContract(t *testing.T) {
	// Act.
	c := loadColors(t)

	// Assert.
	if c.FeedMergeHeadGlyph != "merge" {
		t.Fatalf("feed_merge_head_glyph = %q, want merge", c.FeedMergeHeadGlyph)
	}
}

func TestLoadRenderColorsFailsOnAMissingFile(t *testing.T) {
	// Act.
	_, err := LoadRenderColors(t.TempDir())

	// Assert.
	if err == nil {
		t.Fatal("LoadRenderColors accepted a missing file")
	}
}

func TestLoadRenderColorsFailsOnUnparsableJSON(t *testing.T) {
	// Arrange.
	dir := t.TempDir()
	if err := os.WriteFile(filepath.Join(dir, RenderColorsFile), []byte("{not json"), 0o644); err != nil {
		t.Fatalf("writing: %v", err)
	}

	// Act.
	_, err := LoadRenderColors(dir)

	// Assert.
	if err == nil {
		t.Fatal("LoadRenderColors accepted unparsable JSON")
	}
}

func TestLoadRenderColorsFailsOnAMissingRosterRow(t *testing.T) {
	// Arrange.
	dir := writeColors(t, func(m map[string]any) {
		delete(m["roster_status"].(map[string]any), "ready")
		// merge_glyphs still names arms, so drop nothing else.
	})

	// Act.
	c, err := LoadRenderColors(dir)

	// Assert: the file still loads (its own rows are consistent), and the
	// arm assertion is what catches the gap.
	if err != nil {
		t.Fatalf("LoadRenderColors: %v", err)
	}
	if err := c.AssertRosterStatusArms([]string{"ready"}); err == nil {
		t.Fatal("AssertRosterStatusArms accepted a missing arm")
	}
}

func TestLoadRenderColorsFailsOnAnUnknownColor(t *testing.T) {
	// Arrange.
	dir := writeColors(t, func(m map[string]any) {
		m["roster_status"].(map[string]any)["ready"] = "chartreuse"
	})

	// Act.
	_, err := LoadRenderColors(dir)

	// Assert.
	if err == nil {
		t.Fatal("LoadRenderColors accepted a color outside the closed set")
	}
}

func TestLoadRenderColorsFailsWhenAnUndeclaredMergeArmSpendsAColor(t *testing.T) {
	// Arrange: merging keeps its color but loses its declaration.
	dir := writeColors(t, func(m map[string]any) {
		var kept []any
		for _, arm := range m["colored_merge_arms"].([]any) {
			if arm != "merging" {
				kept = append(kept, arm)
			}
		}
		m["colored_merge_arms"] = kept
	})

	// Act.
	_, err := LoadRenderColors(dir)

	// Assert.
	if err == nil {
		t.Fatal("LoadRenderColors accepted a merge arm that spends a color")
	}
}

func TestLoadRenderColorsAcceptsADeclaredColoredMergeArm(t *testing.T) {
	// Arrange: merge_failed spends turquoise, and colored_merge_arms declares it.
	c := loadColors(t)

	// Act.
	got := c.RosterStatus["merge_failed"]

	// Assert.
	if got != "turquoise" {
		t.Fatalf("roster_status[merge_failed] = %q, want turquoise (owner ruling, 2026-09-28)", got)
	}
}

func TestLoadRenderColorsPaintsMergedGreen(t *testing.T) {
	// Arrange.
	c := loadColors(t)

	// Act.
	got := c.RosterStatus["merged"]

	// Assert.
	if got != "green" {
		t.Fatalf("roster_status[merged] = %q, want green (owner ruling, 2026-09-28)", got)
	}
}

func TestLoadRenderColorsPaintsAMergedFooterGreen(t *testing.T) {
	// Arrange.
	c := loadColors(t)

	// Act.
	got := c.FooterStatus["merged"]

	// Assert.
	if got != "green" {
		t.Fatalf("footer_status[merged] = %q, want green (owner ruling, 2026-09-28)", got)
	}
}

func TestLoadRenderColorsFailsWhenADeclaredColoredMergeArmTakesNoColor(t *testing.T) {
	// Arrange.
	dir := writeColors(t, func(m map[string]any) {
		m["roster_status"].(map[string]any)["merge_failed"] = "none"
	})

	// Act.
	_, err := LoadRenderColors(dir)

	// Assert.
	if err == nil {
		t.Fatal("LoadRenderColors accepted a colored_merge_arms entry that takes no color")
	}
}

func TestLoadRenderColorsFailsWhenAColoredMergeArmIsNotAMergeArm(t *testing.T) {
	// Arrange.
	dir := writeColors(t, func(m map[string]any) {
		m["colored_merge_arms"] = append(m["colored_merge_arms"].([]any), "ready")
	})

	// Act.
	_, err := LoadRenderColors(dir)

	// Assert.
	if err == nil {
		t.Fatal("LoadRenderColors accepted a colored_merge_arms entry that names no merge arm")
	}
}

func TestLoadRenderColorsFailsOnAMergeArmWithNoGlyph(t *testing.T) {
	// Arrange.
	dir := writeColors(t, func(m map[string]any) {
		m["merge_glyphs"].(map[string]any)["merging"] = ""
	})

	// Act.
	_, err := LoadRenderColors(dir)

	// Assert.
	if err == nil {
		t.Fatal("LoadRenderColors accepted an empty glyph name")
	}
}

func TestLoadRenderColorsFailsOnAnEmptyFeedMergeHeadGlyph(t *testing.T) {
	// Arrange.
	dir := writeColors(t, func(m map[string]any) { m["feed_merge_head_glyph"] = "" })

	// Act.
	_, err := LoadRenderColors(dir)

	// Assert.
	if err == nil {
		t.Fatal("LoadRenderColors accepted an empty feed merge head glyph")
	}
}

func TestLoadRenderColorsFailsOnATopbarToneOutsideTheClosedSet(t *testing.T) {
	// Arrange.
	dir := writeColors(t, func(m map[string]any) {
		m["topbar_connectivity"].(map[string]any)["connected"] = "teal"
	})

	// Act.
	_, err := LoadRenderColors(dir)

	// Assert.
	if err == nil {
		t.Fatal("LoadRenderColors accepted a tone outside topbar_tones")
	}
}

func TestLoadRenderColorsFailsOnAnUndeclaredOverrideState(t *testing.T) {
	// Arrange.
	dir := writeColors(t, func(m map[string]any) {
		m["surface_overrides"] = map[string]any{"emacs_tab_bar": map[string]any{"invented": "purple"}}
	})

	// Act.
	_, err := LoadRenderColors(dir)

	// Assert.
	if err == nil {
		t.Fatal("LoadRenderColors accepted an override for no roster arm")
	}
}

func TestLoadRenderColorsFailsOnAMissingFailureSide(t *testing.T) {
	// Arrange.
	dir := writeColors(t, func(m map[string]any) {
		delete(m["failure_sides"].(map[string]any), "vendor")
	})

	// Act.
	_, err := LoadRenderColors(dir)

	// Assert.
	if err == nil {
		t.Fatal("LoadRenderColors accepted a missing failure side")
	}
}

func TestLoadRenderColorsFailsWhenPrecedenceDivergesFromColors(t *testing.T) {
	// Arrange.
	dir := writeColors(t, func(m map[string]any) {
		m["precedence"] = []any{"blue", "purple", "red", "yellow"}
	})

	// Act.
	_, err := LoadRenderColors(dir)

	// Assert.
	if err == nil {
		t.Fatal("LoadRenderColors accepted a precedence that does not cover the colors")
	}
}

func TestRosterStatusArmsCoverTheProtoContract(t *testing.T) {
	// Arrange.
	c := loadColors(t)
	arms, err := OneofArmNames((&frontendv1.RosterRow{}).ProtoReflect().Descriptor(), "status")
	if err != nil {
		t.Fatalf("OneofArmNames: %v", err)
	}

	// Act.
	err = c.AssertRosterStatusArms(arms)

	// Assert.
	if err != nil {
		t.Fatalf("roster_status diverges from RosterRow.status: %v", err)
	}
}

func TestFooterStatusArmsCoverTheProtoContract(t *testing.T) {
	// Arrange.
	c := loadColors(t)
	arms, err := OneofArmNames((&frontendv1.FooterStatus{}).ProtoReflect().Descriptor(), "status")
	if err != nil {
		t.Fatalf("OneofArmNames: %v", err)
	}

	// Act.
	err = c.AssertFooterStatusArms(arms)

	// Assert.
	if err != nil {
		t.Fatalf("footer_status diverges from FooterStatus.status: %v", err)
	}
}

func TestFooterAllowanceArmsCoverTheProtoContract(t *testing.T) {
	// Arrange.
	c := loadColors(t)
	arms, err := OneofArmNames((&frontendv1.FooterAllowance{}).ProtoReflect().Descriptor(), "status")
	if err != nil {
		t.Fatalf("OneofArmNames: %v", err)
	}

	// Act.
	err = c.AssertFooterAllowanceArms(arms)

	// Assert.
	if err != nil {
		t.Fatalf("footer_allowance diverges from FooterAllowance.status: %v", err)
	}
}

func TestEveryMergeArmIsARosterArmWithAGlyph(t *testing.T) {
	// Arrange.
	c := loadColors(t)
	arms, err := OneofArmNames((&frontendv1.RosterRow{}).ProtoReflect().Descriptor(), "status")
	if err != nil {
		t.Fatalf("OneofArmNames: %v", err)
	}
	present := map[string]bool{}
	for _, arm := range arms {
		present[arm] = true
	}

	for _, arm := range mergeArms {
		t.Run(arm, func(t *testing.T) {
			// Assert.
			if !present[arm] {
				t.Fatalf("%q is not a RosterRow.status arm", arm)
			}
			if !contains(c.ColoredMergeArms, arm) && c.RosterStatus[arm] != ColorNone {
				t.Fatalf("roster_status[%q] = %q, want none for a merge arm colored_merge_arms does not declare", arm, c.RosterStatus[arm])
			}
			if c.MergeGlyphs[arm] == "" {
				t.Fatalf("merge_glyphs has no glyph for %q", arm)
			}
		})
	}
}

func TestAssertMergeGlyphArmsCoversExactlyTheMergeArms(t *testing.T) {
	// Arrange.
	c := loadColors(t)

	// Act.
	err := c.AssertMergeGlyphArms(mergeArms)

	// Assert.
	if err != nil {
		t.Fatalf("merge_glyphs diverges from the merge arms: %v", err)
	}
}

func TestAssertRosterStatusArmsRejectsAnUnknownRow(t *testing.T) {
	// Arrange: a table row that names no arm.
	c := loadColors(t)
	arms, err := OneofArmNames((&frontendv1.RosterRow{}).ProtoReflect().Descriptor(), "status")
	if err != nil {
		t.Fatalf("OneofArmNames: %v", err)
	}

	// Act: pretend the contract lost an arm, so the table has an extra row.
	err = c.AssertRosterStatusArms(arms[1:])

	// Assert.
	if err == nil {
		t.Fatal("AssertRosterStatusArms accepted a table row naming no arm")
	}
}

func TestOneofArmNamesRejectsAnUnknownOneof(t *testing.T) {
	// Act.
	_, err := OneofArmNames((&frontendv1.RosterRow{}).ProtoReflect().Descriptor(), "invented")

	// Assert.
	if err == nil {
		t.Fatal("OneofArmNames accepted a oneof the message does not declare")
	}
}

func TestFooterAllowanceColorAnswersEachArm(t *testing.T) {
	tests := []struct {
		arm  string
		want string
	}{
		{arm: "allowed", want: "green"},
		{arm: "allowed_warning", want: "yellow"},
		{arm: "rejected", want: "red"},
	}
	c := loadColors(t)
	for _, tc := range tests {
		t.Run(tc.arm, func(t *testing.T) {
			// Act.
			got, err := c.FooterAllowanceColor(tc.arm)

			// Assert.
			if err != nil {
				t.Fatalf("FooterAllowanceColor: %v", err)
			}
			if got != tc.want {
				t.Fatalf("FooterAllowanceColor(%q) = %q, want %q", tc.arm, got, tc.want)
			}
		})
	}
}

func TestFooterAllowanceColorFailsOnAnUnknownArm(t *testing.T) {
	// Arrange.
	c := loadColors(t)

	// Act.
	_, err := c.FooterAllowanceColor("invented")

	// Assert.
	if err == nil {
		t.Fatal("FooterAllowanceColor answered an arm the table does not carry")
	}
}

func TestRosterStatusColorHonorsADeclaredSurfaceOverride(t *testing.T) {
	// Arrange: a contract whose tab bar declares one divergence.
	dir := writeColors(t, func(m map[string]any) {
		m["surface_overrides"] = map[string]any{"emacs_tab_bar": map[string]any{"ready": "yellow"}}
	})
	c, err := LoadRenderColors(dir)
	if err != nil {
		t.Fatalf("LoadRenderColors: %v", err)
	}

	// Act.
	got, err := c.RosterStatusColor("emacs_tab_bar", "ready")

	// Assert.
	if err != nil {
		t.Fatalf("RosterStatusColor: %v", err)
	}
	if got != "yellow" {
		t.Fatalf("RosterStatusColor = %q, want the declared yellow", got)
	}
}

func TestRosterStatusColorTakesTheSharedAssignmentForASurfaceWithNoOverrides(t *testing.T) {
	// Arrange.
	c := loadColors(t)

	// Act: vendor_blocked is blue in the shared assignment and the webapp
	// declares no override, so it inherits blue verbatim.
	got, err := c.RosterStatusColor("webapp", "vendor_blocked")

	// Assert.
	if err != nil {
		t.Fatalf("RosterStatusColor: %v", err)
	}
	if got != "blue" {
		t.Fatalf("RosterStatusColor = %q, want blue", got)
	}
}

func TestRosterStatusColorFailsOnAnUnknownArm(t *testing.T) {
	// Arrange.
	c := loadColors(t)

	// Act.
	_, err := c.RosterStatusColor("webapp", "invented")

	// Assert.
	if err == nil {
		t.Fatal("RosterStatusColor answered an arm the table does not carry")
	}
}

func TestFooterStatusColorFailsOnAnUnknownArm(t *testing.T) {
	// Arrange.
	c := loadColors(t)

	// Act.
	_, err := c.FooterStatusColor("invented")

	// Assert.
	if err == nil {
		t.Fatal("FooterStatusColor answered an arm the table does not carry")
	}
}

func TestTopbarToneAnswersALinkState(t *testing.T) {
	// Arrange.
	c := loadColors(t)

	// Act.
	got, err := c.TopbarTone("severed")

	// Assert.
	if err != nil {
		t.Fatalf("TopbarTone: %v", err)
	}
	if got != "blue" {
		t.Fatalf("TopbarTone = %q, want blue", got)
	}
}

func TestTopbarToneFailsOnAnUnknownLinkState(t *testing.T) {
	// Arrange.
	c := loadColors(t)

	// Act.
	_, err := c.TopbarTone("invented")

	// Assert.
	if err == nil {
		t.Fatal("TopbarTone answered a link state the table does not carry")
	}
}

func TestFailureSideColorFailsOnAnUnknownSide(t *testing.T) {
	// Arrange.
	c := loadColors(t)

	// Act.
	_, err := c.FailureSideColor("invented")

	// Assert.
	if err == nil {
		t.Fatal("FailureSideColor answered a side the table does not carry")
	}
}

func TestLoadPaintClassesReadsTheContract(t *testing.T) {
	// Act.
	p, err := LoadPaintClasses(repoVocabDir)

	// Assert.
	if err != nil {
		t.Fatalf("LoadPaintClasses: %v", err)
	}
	if p.Plain != PlainClass {
		t.Fatalf("Plain = %q, want the empty string", p.Plain)
	}
	if len(p.ANSIPrecedence) == 0 {
		t.Fatal("ansi_precedence is empty")
	}
}

func TestLoadPaintClassesFailsOnAMissingFile(t *testing.T) {
	// Act.
	_, err := LoadPaintClasses(t.TempDir())

	// Assert.
	if err == nil {
		t.Fatal("LoadPaintClasses accepted a missing file")
	}
}

func TestLoadPaintClassesFailsOnAMissingPlainRow(t *testing.T) {
	// Arrange.
	dir := t.TempDir()
	body := `{"ansi":["ansi-bold"],"syntax":["keyword"],"ansi_precedence":["bold"]}`
	if err := os.WriteFile(filepath.Join(dir, PaintClassesFile), []byte(body), 0o644); err != nil {
		t.Fatalf("writing: %v", err)
	}

	// Act.
	_, err := LoadPaintClasses(dir)

	// Assert.
	if err == nil {
		t.Fatal("LoadPaintClasses accepted a file with no plain row")
	}
}

func TestLoadPaintClassesFailsOnANonEmptyPlainRow(t *testing.T) {
	// Arrange.
	dir := t.TempDir()
	body := `{"plain":"default","ansi":["ansi-bold"],"syntax":["keyword"],"ansi_precedence":["bold"]}`
	if err := os.WriteFile(filepath.Join(dir, PaintClassesFile), []byte(body), 0o644); err != nil {
		t.Fatalf("writing: %v", err)
	}

	// Act.
	_, err := LoadPaintClasses(dir)

	// Assert.
	if err == nil {
		t.Fatal("LoadPaintClasses accepted a named plain class")
	}
}

func TestLoadPaintClassesFailsOnADuplicateClass(t *testing.T) {
	// Arrange.
	dir := t.TempDir()
	body := `{"plain":"","ansi":["ansi-bold"],"syntax":["ansi-bold"],"ansi_precedence":["bold"]}`
	if err := os.WriteFile(filepath.Join(dir, PaintClassesFile), []byte(body), 0o644); err != nil {
		t.Fatalf("writing: %v", err)
	}

	// Act.
	_, err := LoadPaintClasses(dir)

	// Assert.
	if err == nil {
		t.Fatal("LoadPaintClasses accepted a class listed twice")
	}
}

func TestLoadPaintClassesFailsOnAnEmptyAnsiInventory(t *testing.T) {
	// Arrange.
	dir := t.TempDir()
	body := `{"plain":"","ansi":[],"syntax":["keyword"],"ansi_precedence":["bold"]}`
	if err := os.WriteFile(filepath.Join(dir, PaintClassesFile), []byte(body), 0o644); err != nil {
		t.Fatalf("writing: %v", err)
	}

	// Act.
	_, err := LoadPaintClasses(dir)

	// Assert.
	if err == nil {
		t.Fatal("LoadPaintClasses accepted an empty ansi inventory")
	}
}

func TestContainsAcceptsPlainAndInventoryClasses(t *testing.T) {
	tests := []struct {
		name  string
		class string
		want  bool
	}{
		{name: "plain", class: "", want: true},
		{name: "ansi attribute", class: "ansi-bold", want: true},
		{name: "ansi bright foreground", class: "ansi-fg-bright-cyan", want: true},
		{name: "ansi background", class: "ansi-bg-magenta", want: true},
		{name: "syntax", class: "keyword", want: true},
		{name: "unknown", class: "invented", want: false},
		{name: "named plain", class: "plain", want: false},
	}
	p, err := LoadPaintClasses(repoVocabDir)
	if err != nil {
		t.Fatalf("LoadPaintClasses: %v", err)
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Act.
			got := p.Contains(tc.class)

			// Assert.
			if got != tc.want {
				t.Fatalf("Contains(%q) = %v, want %v", tc.class, got, tc.want)
			}
		})
	}
}

func TestTheComposerClosesOnBlueAndPurpleOnly(t *testing.T) {
	// Arrange.
	c := loadColors(t)

	// Act.
	got := append([]string(nil), c.ComposerClosedColors...)
	sort.Strings(got)

	// Assert.
	if len(got) != 2 || got[0] != "blue" || got[1] != "purple" {
		t.Fatalf("composer_closed_colors = %v, want [blue purple] (owner ruling, 2026-09-28)", got)
	}
}

func TestLoadRenderColorsRefusesABadComposerClosedColors(t *testing.T) {
	tests := []struct {
		name  string
		value any
	}{
		{name: "an empty list", value: []any{}},
		{name: "a color outside the closed set", value: []any{"blue", "teal"}},
		{name: "none, which is no color", value: []any{"none"}},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			dir := writeColors(t, func(m map[string]any) { m["composer_closed_colors"] = tc.value })

			// Act.
			_, err := LoadRenderColors(dir)

			// Assert.
			if err == nil {
				t.Fatalf("LoadRenderColors accepted composer_closed_colors = %v", tc.value)
			}
		})
	}
}

func TestTheColorAssignmentsFollowTheOwnersColorMeaningsWithNothingParked(t *testing.T) {
	// The owner's ruling (2026-09-28): blue is an UNUSABLE workspace,
	// turquoise a usable one with something wrong, purple a merge in
	// progress. One row per arm the ruling moved.
	tests := []struct {
		name  string
		table string
		arm   string
		want  string
	}{
		{name: "a failed turn is turquoise on the roster", table: "roster", arm: "turn_failed", want: "turquoise"},
		{name: "a failed merge is turquoise on the roster", table: "roster", arm: "merge_failed", want: "turquoise"},
		{name: "a degraded view is turquoise on the roster", table: "roster", arm: "degraded", want: "turquoise"},
		{name: "a vendor or account block is blue on the roster", table: "roster", arm: "vendor_blocked", want: "blue"},
		{name: "a starting route is blue on the roster", table: "roster", arm: "init", want: "blue"},
		{name: "a queued merge is purple on the roster", table: "roster", arm: "merge_queued", want: "purple"},
		{name: "a running merge is purple on the roster", table: "roster", arm: "merging", want: "purple"},
		{name: "a landed merge is green on the roster", table: "roster", arm: "merged", want: "green"},
		{name: "a failed turn is turquoise on the footer", table: "footer", arm: "turn_failed", want: "turquoise"},
		{name: "a failed merge is turquoise on the footer", table: "footer", arm: "merge_failed", want: "turquoise"},
		{name: "a degraded view is turquoise on the footer", table: "footer", arm: "degraded", want: "turquoise"},
		{name: "a vendor or account block is blue on the footer", table: "footer", arm: "blocked", want: "blue"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			c := loadColors(t)
			table := c.RosterStatus
			if tc.table == "footer" {
				table = c.FooterStatus
			}

			// Act.
			got := table[tc.arm]

			// Assert.
			if got != tc.want {
				t.Fatalf("%s[%s] = %q, want %q", tc.table, tc.arm, got, tc.want)
			}
		})
	}
}
