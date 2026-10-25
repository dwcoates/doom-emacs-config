package vocab

import (
	"encoding/json"
	"fmt"
	"os"
	"path/filepath"
	"sort"
	"strings"
	"testing"

	frontendv1 "agentrepl/proto/frontend/v1"
	"google.golang.org/protobuf/reflect/protoreflect"
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
	// Arrange: merge_failed spends green, and colored_merge_arms declares it.
	c := loadColors(t)

	// Act.
	got := c.RosterStatus["merge_failed"]

	// Assert.
	if got != "green" {
		t.Fatalf("roster_status[merge_failed] = %q, want green (owner ruling, 2026-10-13)", got)
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

	// Act: network_fault is blue in the shared assignment and the webapp
	// declares no override, so it inherits blue verbatim.
	got, err := c.RosterStatusColor("webapp", "network_fault")

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

func TestTheComposerClosesOnBlueOnly(t *testing.T) {
	// Arrange.
	c := loadColors(t)

	// Act.
	got := append([]string(nil), c.ComposerClosedColors...)
	sort.Strings(got)

	// Assert.
	if len(got) != 1 || got[0] != "blue" {
		t.Fatalf("composer_closed_colors = %v, want [blue] (owner ruling, 2026-10-01)", got)
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
		{name: "a failed merge is green on the roster", table: "roster", arm: "merge_failed", want: "green"},
		{name: "a degraded view is turquoise on the roster", table: "roster", arm: "degraded", want: "turquoise"},
		{name: "a vendor or account block is turquoise on the roster", table: "roster", arm: "vendor_blocked", want: "turquoise"},
		{name: "a vendor that will not start is turquoise on the roster", table: "roster", arm: "vendor_fault", want: "turquoise"},
		{name: "a call the vendor retries is turquoise on the roster", table: "roster", arm: "api_retrying", want: "turquoise"},
		{name: "an unreachable network is blue on the roster", table: "roster", arm: "network_fault", want: "blue"},
		{name: "a booting session is grey on the roster", table: "roster", arm: "init", want: "none"},
		{name: "an incidentally inactive workspace is grey on the roster", table: "roster", arm: "inactive", want: "none"},
		{name: "a cold gate is green on the roster", table: "roster", arm: "cold_gate", want: "green"},
		{name: "a queued merge is purple on the roster", table: "roster", arm: "merge_queued", want: "purple"},
		{name: "a running merge is purple on the roster", table: "roster", arm: "merging", want: "purple"},
		{name: "a landed merge is green on the roster", table: "roster", arm: "merged", want: "green"},
		{name: "a failed turn is turquoise on the footer", table: "footer", arm: "turn_failed", want: "turquoise"},
		{name: "a failed merge is green on the footer", table: "footer", arm: "merge_failed", want: "green"},
		{name: "a booting session is grey on the footer", table: "footer", arm: "starting", want: "none"},
		{name: "a degraded view is turquoise on the footer", table: "footer", arm: "degraded", want: "turquoise"},
		{name: "a vendor fault is turquoise on the footer", table: "footer", arm: "vendor_fault", want: "turquoise"},
		{name: "an agent-repl fault is blue on the footer", table: "footer", arm: "agent_repl_fault", want: "blue"},
		{name: "a network fault is blue on the footer", table: "footer", arm: "network_fault", want: "blue"},
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

// A VENDOR FAULT OPENS THE COMPOSER BY ITS COLOR (owner ruling, 2026-10-02),
// so the one exception there was (`blocked · api_retrying`) is gone. The one
// declared today is `agent_repl_fault · turn_died` (owner ruling, 2026-10-06):
// the fault stands until the next turn starts, so a closed composer would make
// it permanent.
func TestTheTurnDiedFaultIsTheOneComposerOpenSubstatus(t *testing.T) {
	// Arrange.
	c := loadColors(t)

	// Act.
	got := c.ComposerOpenSubstatuses

	// Assert.
	if len(got) != 1 || len(got["agent_repl_fault"]) != 1 || got["agent_repl_fault"][0] != "turn_died" {
		t.Fatalf("composer_open_substatuses = %v, want exactly agent_repl_fault: [turn_died]", got)
	}
}

func TestComposerOpenSubstatusesNameRealSubstatusArms(t *testing.T) {
	// Arrange.
	c := loadColors(t)
	armsOf := func(status string) ([]string, error) {
		fd := (&frontendv1.FooterStatus{}).ProtoReflect().Descriptor().Fields().ByName(protoreflect.Name(status))
		if fd == nil || fd.Message() == nil {
			return nil, fmt.Errorf("FooterStatus has no %q arm", status)
		}
		return OneofArmNames(fd.Message(), "substatus")
	}

	// Act.
	err := c.AssertComposerOpenSubstatusArms(armsOf)

	// Assert.
	if err != nil {
		t.Fatalf("composer_open_substatuses diverges from the footer protos: %v", err)
	}
}

func TestAssertComposerOpenSubstatusArmsRefusesAnInventedSubstatus(t *testing.T) {
	// Arrange.
	c := RenderColors{ComposerOpenSubstatuses: map[string][]string{"agent_repl_fault": {"invented"}}}

	// Act.
	err := c.AssertComposerOpenSubstatusArms(func(string) ([]string, error) { return []string{"starting"}, nil })

	// Assert.
	if err == nil {
		t.Fatalf("an invented substatus was accepted")
	}
}

func TestLoadRenderColorsRefusesABadComposerOpenSubstatuses(t *testing.T) {
	tests := []struct {
		name  string
		value any
	}{
		{name: "a status that is not a footer arm", value: map[string]any{"invented": []any{"api_retrying"}}},
		{name: "a status whose color leaves the composer open", value: map[string]any{"working": []any{"api_retrying"}}},
		{name: "an empty substatus list", value: map[string]any{"agent_repl_fault": []any{}}},
		{name: "the empty substatus", value: map[string]any{"agent_repl_fault": []any{""}}},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			dir := writeColors(t, func(m map[string]any) { m["composer_open_substatuses"] = tc.value })

			// Act.
			_, err := LoadRenderColors(dir)

			// Assert.
			if err == nil {
				t.Fatalf("LoadRenderColors accepted composer_open_substatuses = %v", tc.value)
			}
		})
	}
}

func TestTheGlyphInksAreAFailedMergesRedAndCompactionsOrange(t *testing.T) {
	// Arrange.
	c := loadColors(t)

	// Act.
	got := c.GlyphInks

	// Assert.
	if len(got) != 2 || got["merge_failed"] != "red" || got["compacting"] != "orange" {
		t.Fatalf("glyph_inks = %v, want merge_failed: red and compacting: orange (owner ruling, 2026-10-13)", got)
	}
}

func TestTheOneShotGlyphDrawsOnlyWhileTheOneShotWorks(t *testing.T) {
	// Arrange.
	c := loadColors(t)

	// Act.
	got := append([]string{}, c.OneShotGlyphArms...)
	sort.Strings(got)

	// Assert.
	want := []string{"clearing", "submitting", "thinking"}
	if fmt.Sprint(got) != fmt.Sprint(want) {
		t.Fatalf("one_shot_glyph_arms = %v, want %v (owner ruling, 2026-10-13)", got, want)
	}
}

func TestTheAsyncMergeMarkIsABreathingPurpleMergeGlyph(t *testing.T) {
	// Arrange.
	c := loadColors(t)

	// Act.
	got := c.AsyncMergeMark

	// Assert.
	want := AsyncMergeMark{Glyph: "merge", Ink: "purple", Motion: "breathes", TabBracket: "purple"}
	if got != want {
		t.Fatalf("async_merge_mark = %+v, want %+v (owner requests, 2026-10-13 and 2026-10-14)", got, want)
	}
}

func TestThePausedMergeMarkIsAStillGreyInk(t *testing.T) {
	// Arrange.
	c := loadColors(t)

	// Act.
	got := c.PausedMergeMark

	// Assert.
	want := PausedMergeMark{Ink: "amber", Motion: "still"}
	if got != want {
		t.Fatalf("paused_merge_mark = %+v, want %+v (owner request, 2026-10-25)", got, want)
	}
}

func TestLoadRenderColorsRefusesAMalformedGlyphTable(t *testing.T) {
	tests := []struct {
		name   string
		mutate func(m map[string]any)
	}{
		{name: "an ink row naming no roster arm", mutate: func(m map[string]any) {
			m["glyph_inks"].(map[string]any)["no_such_arm"] = "red"
		}},
		{name: "an ink that is neither a color nor glyph-only", mutate: func(m map[string]any) {
			m["glyph_inks"].(map[string]any)["merge_failed"] = "chartreuse"
		}},
		{name: "an ink of none", mutate: func(m map[string]any) {
			m["glyph_inks"].(map[string]any)["merge_failed"] = "none"
		}},
		{name: "an ink equal to the arm's own color", mutate: func(m map[string]any) {
			m["glyph_inks"].(map[string]any)["merge_failed"] = "green"
		}},
		{name: "a glyph-only color that is already a color", mutate: func(m map[string]any) {
			m["glyph_only_colors"] = []any{"orange", "red"}
		}},
		{name: "a status taking a glyph-only color", mutate: func(m map[string]any) {
			m["roster_status"].(map[string]any)["compacting"] = "orange"
		}},
		{name: "an empty one-shot arm list", mutate: func(m map[string]any) {
			m["one_shot_glyph_arms"] = []any{}
		}},
		{name: "a one-shot arm naming no roster arm", mutate: func(m map[string]any) {
			m["one_shot_glyph_arms"] = []any{"thinking", "no_such_arm"}
		}},
		{name: "an absent async-merge mark", mutate: func(m map[string]any) {
			delete(m, "async_merge_mark")
		}},
		{name: "an async-merge glyph no merge arm draws", mutate: func(m map[string]any) {
			m["async_merge_mark"].(map[string]any)["glyph"] = "exclaim"
		}},
		{name: "an absent async-merge glyph", mutate: func(m map[string]any) {
			delete(m["async_merge_mark"].(map[string]any), "glyph")
		}},
		{name: "an async-merge ink that is not a color", mutate: func(m map[string]any) {
			m["async_merge_mark"].(map[string]any)["ink"] = "chartreuse"
		}},
		{name: "an async-merge ink of none", mutate: func(m map[string]any) {
			m["async_merge_mark"].(map[string]any)["ink"] = "none"
		}},
		{name: "an async-merge motion the rail has no animation for", mutate: func(m map[string]any) {
			m["async_merge_mark"].(map[string]any)["motion"] = "spins"
		}},
		{name: "an async-merge tab bracket that is not a color", mutate: func(m map[string]any) {
			m["async_merge_mark"].(map[string]any)["tab_bracket"] = "orange"
		}},
		{name: "an absent paused-merge mark", mutate: func(m map[string]any) {
			delete(m, "paused_merge_mark")
		}},
		{name: "a paused-merge ink that is neither a color nor glyph-only", mutate: func(m map[string]any) {
			m["paused_merge_mark"].(map[string]any)["ink"] = "chartreuse"
		}},
		{name: "a paused-merge ink of none", mutate: func(m map[string]any) {
			m["paused_merge_mark"].(map[string]any)["ink"] = "none"
		}},
		{name: "a paused-merge motion the rail has no animation for", mutate: func(m map[string]any) {
			m["paused_merge_mark"].(map[string]any)["motion"] = "spins"
		}},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			dir := writeColors(t, tc.mutate)

			// Act.
			_, err := LoadRenderColors(dir)

			// Assert.
			if err == nil {
				t.Fatalf("LoadRenderColors accepted %s", tc.name)
			}
		})
	}
}

func TestRosterStatusArmsAreTheProtosOneof(t *testing.T) {
	// Arrange.
	want, err := OneofArmNames((&frontendv1.RosterRow{}).ProtoReflect().Descriptor(), "status")
	if err != nil {
		t.Fatalf("OneofArmNames: %v", err)
	}

	// Act.
	got := RosterStatusArms()

	// Assert.
	if fmt.Sprint(got) != fmt.Sprint(want) {
		t.Fatalf("RosterStatusArms() = %v, want %v", got, want)
	}
}

func TestRosterMergeArmsAreExactlyTheMergePipelinesArms(t *testing.T) {
	// Act.
	got := append([]string{}, RosterMergeArms()...)
	sort.Strings(got)

	// Assert.
	want := append([]string{}, mergeArms...)
	sort.Strings(want)
	if fmt.Sprint(got) != fmt.Sprint(want) {
		t.Fatalf("RosterMergeArms() = %v, want %v", got, want)
	}
}

func TestIsMergeArm(t *testing.T) {
	tests := []struct {
		arm  string
		want bool
	}{
		{arm: "merging", want: true},
		{arm: "merge_queued", want: true},
		{arm: "merge_failed", want: true},
		{arm: "merged", want: true},
		{arm: "thinking", want: false},
		{arm: "ready", want: false},
		{arm: "inactive", want: false},
	}
	for _, tc := range tests {
		t.Run(tc.arm, func(t *testing.T) {
			// Act / Assert.
			if got := IsMergeArm(tc.arm); got != tc.want {
				t.Fatalf("IsMergeArm(%q) = %v, want %v", tc.arm, got, tc.want)
			}
		})
	}
}

func TestFooterStatusArmsAreTheProtosOneof(t *testing.T) {
	// Arrange.
	want, err := OneofArmNames((&frontendv1.FooterStatus{}).ProtoReflect().Descriptor(), "status")
	if err != nil {
		t.Fatalf("OneofArmNames: %v", err)
	}

	// Act.
	got := FooterStatusArms()

	// Assert.
	if fmt.Sprint(got) != fmt.Sprint(want) {
		t.Fatalf("FooterStatusArms() = %v, want %v", got, want)
	}
}

// THE RESOLVERS READ THEIR ARMS FROM THE ONE READER: no production file keeps
// a hand-typed copy of a status oneof for the vocabulary to be asserted
// against (vocab.RosterStatusArms, vocab.FooterStatusArms). A production file
// declaring one by the old name fails here.
func TestNoResolverKeepsAHandTypedStatusArmList(t *testing.T) {
	// Arrange.
	root := "../.."
	var found []string

	// Act.
	err := filepath.WalkDir(root, func(path string, d os.DirEntry, err error) error {
		if err != nil {
			return err
		}
		if d.IsDir() || filepath.Ext(path) != ".go" || strings.HasSuffix(path, "_test.go") {
			return nil
		}
		raw, err := os.ReadFile(path)
		if err != nil {
			return err
		}
		for _, name := range []string{"statusArms", "mergeArms"} {
			if strings.Contains(string(raw), "var "+name+" = []string{") {
				found = append(found, path)
			}
		}
		return nil
	})

	// Assert.
	if err != nil {
		t.Fatalf("walking the module: %v", err)
	}
	if len(found) != 0 {
		t.Fatalf("hand-typed status arm lists in %v, want every resolver on the vocab readers", found)
	}
}

func TestInkColorTakesAColorOrAGlyphOnlyColorAndNeverNone(t *testing.T) {
	c := loadColors(t)
	tests := []struct {
		ink  string
		want bool
	}{
		{ink: "red", want: true},
		{ink: "orange", want: true},
		{ink: "amber", want: true},
		{ink: ColorNone, want: false},
		{ink: "chartreuse", want: false},
	}
	for _, tc := range tests {
		t.Run(tc.ink, func(t *testing.T) {
			// Act, Assert.
			if got := c.inkColor(tc.ink); got != tc.want {
				t.Fatalf("inkColor(%q) = %v, want %v", tc.ink, got, tc.want)
			}
		})
	}
}

func TestCheckMotionNamesTheSectionOfAMotionTheRailCannotDraw(t *testing.T) {
	// Act.
	err := checkMotion("paused_merge_mark", "spins")

	// Assert.
	if err == nil || !strings.Contains(err.Error(), "paused_merge_mark.motion") {
		t.Fatalf("checkMotion = %v, want a refusal naming the section", err)
	}
}

func TestCheckMotionTakesEveryMarkMotion(t *testing.T) {
	for _, motion := range markMotions {
		t.Run(motion, func(t *testing.T) {
			// Act, Assert.
			if err := checkMotion("async_merge_mark", motion); err != nil {
				t.Fatalf("checkMotion(%q) = %v, want nil", motion, err)
			}
		})
	}
}

// TestEveryMarkSectionChecksItsMotionThroughCheckMotion fails a mark section
// whose motion is checked by hand rather than through the one check.
func TestEveryMarkSectionChecksItsMotionThroughCheckMotion(t *testing.T) {
	// Arrange.
	raw, err := os.ReadFile("api.go")
	if err != nil {
		t.Fatalf("read api.go: %v", err)
	}

	// Act.
	handRolled := strings.Count(string(raw), "contains(markMotions,")

	// Assert: the one membership test is checkMotion's own.
	if handRolled != 1 {
		t.Fatalf("api.go tests markMotions membership %d times, want once, inside checkMotion", handRolled)
	}
}

func TestLoadRenderColorsReadsTheCompactionOrangePerTheme(t *testing.T) {
	// Arrange / Act.
	c := loadColors(t)

	// Assert.
	for _, theme := range ColorThemes {
		if !hexColor.MatchString(c.ColorValues[theme]["orange"]) {
			t.Errorf("color_values[%q][\"orange\"] = %q", theme, c.ColorValues[theme]["orange"])
		}
	}
}

func TestLoadRenderColorsReadsEachColorsOneValuePerTheme(t *testing.T) {
	// Arrange / Act.
	c := loadColors(t)

	// Assert.
	for _, theme := range ColorThemes {
		for _, color := range c.Colors {
			if !hexColor.MatchString(c.ColorValues[theme][color]) {
				t.Errorf("color_values[%q][%q] = %q", theme, color, c.ColorValues[theme][color])
			}
		}
	}
}

func TestLoadRenderColorsRefusesMalformedColorValues(t *testing.T) {
	tests := []struct {
		name   string
		mutate func(values map[string]any)
	}{
		{"a missing theme", func(v map[string]any) { delete(v, "dark") }},
		{"an extra theme", func(v map[string]any) { v["sepia"] = v["dark"] }},
		{"a theme missing a color", func(v map[string]any) { delete(v["dark"].(map[string]any), "green") }},
		{"a theme naming an unknown color", func(v map[string]any) { v["dark"].(map[string]any)["chartreuse"] = "#7fff00" }},
		{"a theme missing an ink the other gives", func(v map[string]any) { delete(v["dark"].(map[string]any), "orange") }},
		{"a value that is no hex", func(v map[string]any) { v["dark"].(map[string]any)["green"] = "green" }},
		{"an uppercase hex value", func(v map[string]any) { v["dark"].(map[string]any)["green"] = "#5DD87C" }},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			dir := writeColors(t, func(m map[string]any) { tc.mutate(m["color_values"].(map[string]any)) })

			// Act.
			_, err := LoadRenderColors(dir)

			// Assert.
			if err == nil || !strings.Contains(err.Error(), "color_values") {
				t.Fatalf("LoadRenderColors error = %v, want a color_values refusal", err)
			}
		})
	}
}
