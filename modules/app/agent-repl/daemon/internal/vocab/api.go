// Package vocab reads and asserts the two cross-language vocabulary files,
// proto/vocab/render-colors.json and proto/vocab/paint-classes.json.
//
// The daemon owns both files; the webapp and Emacs consume them. Every
// producer asserts its emitted names against the loaded inventory, which is
// the only mechanism that makes a divergence fail loudly. See ARCHITECTURE.md
// "Paint classes".
package vocab

import (
	"encoding/json"
	"fmt"
	"maps"
	"os"
	"path/filepath"
	"regexp"
	"slices"
	"sort"
	"strings"

	frontendv1 "agentrepl/proto/frontend/v1"
	"google.golang.org/protobuf/reflect/protoreflect"
)

// RenderColorsFile is render-colors.json's file name in the vocab directory.
const RenderColorsFile = "render-colors.json"

// PaintClassesFile is paint-classes.json's file name in the vocab directory.
const PaintClassesFile = "paint-classes.json"

// ColorNone is the color value meaning "this state spends no color". It is a
// real answer, never a gap: the merge arms carry glyphs instead, and a
// workspace with no session has no lifecycle verdict to paint.
const ColorNone = "none"

// PlainClass is the wire spelling of unstyled text. The empty string is the
// only spelling with that meaning.
const PlainClass = ""

// FailureSides is the closed set of failure sides render-colors.json must
// color. A side landing without a color is a divergence, not a default.
var FailureSides = []string{"machinery", "vendor", "client_local"}

// RenderColors is render-colors.json: which of the six colors each state
// takes, the one value of each color per theme, and the glyph names the merge
// states carry beside their color.
type RenderColors struct {
	// ColorValues maps each theme ("light", "dark") to each color's #rrggbb
	// value: the one value every renderer paints the color with (owner ruling,
	// 2026-10-17). The daemon paints nothing, so it only validates them.
	ColorValues map[string]map[string]string
	// Colors is the closed set of color names.
	Colors []string
	// Precedence is the six-color precedence, strongest claim first.
	Precedence []string
	// RosterStatus maps each RosterRow.status arm name to its color.
	RosterStatus map[string]string
	// MergeGlyphs maps each merge arm of RosterRow.status to a glyph NAME.
	MergeGlyphs map[string]string
	// ColoredMergeArms are the merge arms DECLARED to spend a lifecycle color
	// beside their glyph (owner rulings, 2026-09-28). Every other merge arm
	// takes ColorNone.
	ColoredMergeArms []string
	// FeedMergeHeadGlyph is the glyph name a merge bubble's head carries.
	FeedMergeHeadGlyph string
	// GlyphInks maps a RosterRow.status arm whose drawn mark is inked apart
	// from the arm's own color to that ink (owner ruling, 2026-10-13): the
	// arm's color stays the workspace's status, the ink is the mark's.
	GlyphInks map[string]string
	// GlyphOnlyColors are the ink names no status may take, which only
	// GlyphInks may name.
	GlyphOnlyColors []string
	// OneShotGlyphArms are the RosterRow.status arms on which a one-shot's
	// glyph replaces the status mark, and the only ones.
	OneShotGlyphArms []string
	// AsyncMergeMark is how both surfaces draw RosterRowAsyncMerging: the
	// workspace's own branch merging async while the row keeps its status.
	AsyncMergeMark AsyncMergeMark
	// PausedMergeMark is how the sidebar draws RosterRowMergePaused: the
	// merge indicators of a row whose merge's queue is paused.
	PausedMergeMark PausedMergeMark
	// FooterStatus maps each FooterStatus.status arm name to its color.
	FooterStatus map[string]string
	// ComposerClosedColors are the footer status colors under which a
	// composer is CLOSED (owner ruling, 2026-09-28): blue, an unusable
	// workspace, and purple, a merge holding it. Every other color is a
	// usable workspace whose composer is open.
	ComposerClosedColors []string
	// ComposerOpenSubstatuses are the DECLARED exceptions to the composer
	// invariant: per footer status arm whose color closes the composer, the
	// substatus arms under which it stays open (owner ruling, 2026-10-01:
	// `blocked · api_retrying`, whose prompt interrupts the retry).
	ComposerOpenSubstatuses map[string][]string
	// TopbarConnectivity maps each daemon link state to a topbar tone.
	TopbarConnectivity map[string]string
	// TopbarTones is the closed set of tone names the topbar may serve.
	TopbarTones []string
	// SurfaceOverrides are the DECLARED per-surface divergences, keyed by
	// surface name then by state name.
	SurfaceOverrides map[string]map[string]string
	// FailureSides maps each failure side (machinery, vendor, client_local) to
	// its color.
	FailureSides map[string]string
}

// AsyncMergeMark is the shared drawing of an async merge of the workspace's
// own branch (frontend.v1.RosterRowAsyncMerging): the merge glyph, ink and
// motion the sidebar draws in place of its plain status dot, and the
// background of the Emacs tab's [N] run. Every renderer reads it here, so no
// surface picks its own purple.
type AsyncMergeMark struct {
	Glyph      string `json:"glyph"`
	Ink        string `json:"ink"`
	Motion     string `json:"motion"`
	TabBracket string `json:"tab_bracket"`
}

// PausedMergeMark is the shared drawing of a row whose merge's queue is
// paused (frontend.v1.RosterRowMergePaused, owner ruling 2026-10-14): the ink
// every merge indicator of the row takes, and the motion the merge's breath
// on the row's name takes. Every renderer reads it here, so no surface picks
// its own grey.
type PausedMergeMark struct {
	Ink    string `json:"ink"`
	Motion string `json:"motion"`
}

// markMotions are the motions a mark may take: the rail's one animation, or
// none.
var markMotions = []string{"breathes", "still"}

// renderColorsJSON is the file's on-disk shape. It is separate from
// RenderColors so an absent table is distinguishable from an empty one.
type renderColorsJSON struct {
	ColorValues             map[string]map[string]string `json:"color_values"`
	Colors                  []string                     `json:"colors"`
	Precedence              []string                     `json:"precedence"`
	RosterStatus            map[string]string            `json:"roster_status"`
	MergeGlyphs             map[string]string            `json:"merge_glyphs"`
	ColoredMergeArms        []string                     `json:"colored_merge_arms"`
	FeedMergeHeadGlyph      string                       `json:"feed_merge_head_glyph"`
	GlyphInks               map[string]string            `json:"glyph_inks"`
	GlyphOnlyColors         []string                     `json:"glyph_only_colors"`
	OneShotGlyphArms        []string                     `json:"one_shot_glyph_arms"`
	AsyncMergeMark          AsyncMergeMark               `json:"async_merge_mark"`
	PausedMergeMark         PausedMergeMark              `json:"paused_merge_mark"`
	FooterStatus            map[string]string            `json:"footer_status"`
	ComposerClosedColors    []string                     `json:"composer_closed_colors"`
	ComposerOpenSubstatuses map[string][]string          `json:"composer_open_substatuses"`
	TopbarConnectivity      map[string]string            `json:"topbar_connectivity"`
	TopbarTones             []string                     `json:"topbar_tones"`
	SurfaceOverrides        map[string]map[string]string `json:"surface_overrides"`
	FailureSides            map[string]string            `json:"failure_sides"`
}

// PaintClasses is paint-classes.json: the closed inventory of paint_class
// names for FeedCodeSpan and FeedMergeTestSpan.
type PaintClasses struct {
	// Plain is the class name for unstyled text.
	Plain string
	// ANSI is the SGR-derived inventory (attributes plus fg-/bg- names).
	ANSI []string
	// Syntax is the language-neutral highlight inventory.
	Syntax []string
	// ANSIPrecedence is the order adjacent split spans are emitted in when one
	// run carries several ANSI attributes.
	ANSIPrecedence []string
}

type paintClassesJSON struct {
	Plain          *string  `json:"plain"`
	ANSI           []string `json:"ansi"`
	Syntax         []string `json:"syntax"`
	ANSIPrecedence []string `json:"ansi_precedence"`
}

// LoadRenderColors reads render-colors.json from the vocab directory. A file
// that does not parse, or a table with a missing row, is an error: the daemon
// refuses to serve an unpainted state.
func LoadRenderColors(vocabDir string) (RenderColors, error) {
	path := filepath.Join(vocabDir, RenderColorsFile)
	raw, err := os.ReadFile(path)
	if err != nil {
		return RenderColors{}, fmt.Errorf("vocab: reading %s: %w", path, err)
	}
	var parsed renderColorsJSON
	if err := json.Unmarshal(raw, &parsed); err != nil {
		return RenderColors{}, fmt.Errorf("vocab: parsing %s: %w", path, err)
	}
	// renderColorsJSON carries exactly RenderColors' fields in exactly its
	// order, differing only in the json tags, so the conversion is total: a
	// field added to one and not the other stops compiling here.
	c := RenderColors(parsed)
	if err := c.validate(); err != nil {
		return RenderColors{}, fmt.Errorf("vocab: %s: %w", path, err)
	}
	return c, nil
}

// ColorThemes are the themes color_values must carry, and the only ones.
var ColorThemes = []string{"light", "dark"}

// hexColor is the one spelling a color value may take.
var hexColor = regexp.MustCompile(`^#[0-9a-f]{6}$`)

// validateColorValues holds color_values to exactly ColorThemes, each giving
// exactly the closed set of colors a lowercase #rrggbb value.
func (c RenderColors) validateColorValues() error {
	themes := make([]string, 0, len(c.ColorValues))
	for theme := range c.ColorValues {
		themes = append(themes, theme)
	}
	if !sameSet(themes, ColorThemes) {
		return fmt.Errorf("color_values themes %v are not %v", themes, ColorThemes)
	}
	for _, theme := range ColorThemes {
		values := c.ColorValues[theme]
		names := make([]string, 0, len(values))
		for name, value := range values {
			names = append(names, name)
			if !hexColor.MatchString(value) {
				return fmt.Errorf("color_values[%q][%q] = %q, which is not a lowercase #rrggbb value", theme, name, value)
			}
		}
		if !sameSet(names, c.Colors) {
			return fmt.Errorf("color_values[%q] names %v, not the colors %v", theme, names, c.Colors)
		}
	}
	return nil
}

func (c RenderColors) validate() error {
	if len(c.Colors) == 0 {
		return fmt.Errorf("colors is empty")
	}
	if !sameSet(c.Colors, c.Precedence) {
		return fmt.Errorf("precedence %v does not cover colors %v", c.Precedence, c.Colors)
	}
	if err := c.validateColorValues(); err != nil {
		return err
	}
	for _, table := range []struct {
		name string
		rows map[string]string
	}{
		{"roster_status", c.RosterStatus},
		{"footer_status", c.FooterStatus},
		{"failure_sides", c.FailureSides},
	} {
		if len(table.rows) == 0 {
			return fmt.Errorf("%s is empty", table.name)
		}
		for state, color := range table.rows {
			if !c.knownColor(color) {
				return fmt.Errorf("%s[%q] = %q, which is not a color", table.name, state, color)
			}
		}
	}
	if len(c.ComposerClosedColors) == 0 {
		return fmt.Errorf("composer_closed_colors is empty")
	}
	for _, color := range c.ComposerClosedColors {
		if color == ColorNone || !c.knownColor(color) {
			return fmt.Errorf("composer_closed_colors names %q, which is not a color", color)
		}
	}
	for status, substatuses := range c.ComposerOpenSubstatuses {
		color, ok := c.FooterStatus[status]
		if !ok {
			return fmt.Errorf("composer_open_substatuses[%q] names no footer_status arm", status)
		}
		if !contains(c.ComposerClosedColors, color) {
			return fmt.Errorf("composer_open_substatuses[%q]: its color %q does not close the composer, so it has nothing to open", status, color)
		}
		if len(substatuses) == 0 {
			return fmt.Errorf("composer_open_substatuses[%q] is empty", status)
		}
		for _, sub := range substatuses {
			if sub == "" {
				return fmt.Errorf("composer_open_substatuses[%q] names the empty substatus", status)
			}
		}
	}
	if len(c.MergeGlyphs) == 0 {
		return fmt.Errorf("merge_glyphs is empty")
	}
	for arm, glyph := range c.MergeGlyphs {
		if glyph == "" {
			return fmt.Errorf("merge_glyphs[%q] is empty", arm)
		}
		color, ok := c.RosterStatus[arm]
		if !ok {
			return fmt.Errorf("merge_glyphs[%q] names no roster_status arm", arm)
		}
		colored := contains(c.ColoredMergeArms, arm)
		if !colored && color != ColorNone {
			return fmt.Errorf("merge arm %q takes color %q; a merge arm spends no color unless colored_merge_arms declares it", arm, color)
		}
		if colored && color == ColorNone {
			return fmt.Errorf("merge arm %q is declared in colored_merge_arms but takes no color", arm)
		}
	}
	for _, arm := range c.ColoredMergeArms {
		if _, ok := c.MergeGlyphs[arm]; !ok {
			return fmt.Errorf("colored_merge_arms names %q, which is not a merge arm", arm)
		}
	}
	if c.FeedMergeHeadGlyph == "" {
		return fmt.Errorf("feed_merge_head_glyph is empty")
	}
	for _, ink := range c.GlyphOnlyColors {
		if c.knownColor(ink) {
			return fmt.Errorf("glyph_only_colors names %q, which is already a color", ink)
		}
	}
	for arm, ink := range c.GlyphInks {
		color, ok := c.RosterStatus[arm]
		if !ok {
			return fmt.Errorf("glyph_inks[%q] names no roster_status arm", arm)
		}
		if !c.inkColor(ink) {
			return fmt.Errorf("glyph_inks[%q] = %q, which is neither a color nor a glyph-only color", arm, ink)
		}
		if ink == color {
			return fmt.Errorf("glyph_inks[%q] = %q, the arm's own color; an ink row says only where the two differ", arm, ink)
		}
	}
	for _, table := range []map[string]string{c.RosterStatus, c.FooterStatus, c.FailureSides} {
		for state, color := range table {
			if contains(c.GlyphOnlyColors, color) {
				return fmt.Errorf("%q takes %q, which only a glyph may", state, color)
			}
		}
	}
	if len(c.OneShotGlyphArms) == 0 {
		return fmt.Errorf("one_shot_glyph_arms is empty")
	}
	for _, arm := range c.OneShotGlyphArms {
		if _, ok := c.RosterStatus[arm]; !ok {
			return fmt.Errorf("one_shot_glyph_arms names %q, which is no roster_status arm", arm)
		}
	}
	if m := c.AsyncMergeMark; !slices.Contains(slices.Collect(maps.Values(c.MergeGlyphs)), m.Glyph) {
		return fmt.Errorf("async_merge_mark.glyph = %q, which no merge_glyphs arm draws", m.Glyph)
	}
	if m := c.AsyncMergeMark; m.Ink == ColorNone || !contains(c.Colors, m.Ink) {
		return fmt.Errorf("async_merge_mark.ink = %q, which is not a color", m.Ink)
	}
	if err := checkMotion("async_merge_mark", c.AsyncMergeMark.Motion); err != nil {
		return err
	}
	if m := c.AsyncMergeMark; m.TabBracket == ColorNone || !contains(c.Colors, m.TabBracket) {
		return fmt.Errorf("async_merge_mark.tab_bracket = %q, which is not a color", m.TabBracket)
	}
	if m := c.PausedMergeMark; !c.inkColor(m.Ink) {
		return fmt.Errorf("paused_merge_mark.ink = %q, which is neither a color nor a glyph-only color", m.Ink)
	}
	if err := checkMotion("paused_merge_mark", c.PausedMergeMark.Motion); err != nil {
		return err
	}
	if len(c.TopbarTones) == 0 {
		return fmt.Errorf("topbar_tones is empty")
	}
	if len(c.TopbarConnectivity) == 0 {
		return fmt.Errorf("topbar_connectivity is empty")
	}
	for state, tone := range c.TopbarConnectivity {
		if !contains(c.TopbarTones, tone) {
			return fmt.Errorf("topbar_connectivity[%q] = %q, which is not a topbar tone", state, tone)
		}
	}
	for surface, overrides := range c.SurfaceOverrides {
		for state, color := range overrides {
			if _, ok := c.RosterStatus[state]; !ok {
				return fmt.Errorf("surface_overrides[%q][%q] names no roster_status arm", surface, state)
			}
			if !c.knownColor(color) {
				return fmt.Errorf("surface_overrides[%q][%q] = %q, which is not a color", surface, state, color)
			}
		}
	}
	for _, side := range FailureSides {
		if _, ok := c.FailureSides[side]; !ok {
			return fmt.Errorf("failure_sides has no row for %q", side)
		}
	}
	if len(c.FailureSides) != len(FailureSides) {
		return fmt.Errorf("failure_sides has %d rows, want %d", len(c.FailureSides), len(FailureSides))
	}
	return nil
}

func (c RenderColors) knownColor(color string) bool {
	return color == ColorNone || contains(c.Colors, color)
}

// inkColor reports whether INK may ink a mark: one of the colors or one of
// the glyph-only colors, and never none.
func (c RenderColors) inkColor(ink string) bool {
	return ink != ColorNone && (contains(c.Colors, ink) || contains(c.GlyphOnlyColors, ink))
}

// checkMotion refuses a SECTION whose MOTION is none of markMotions.
func checkMotion(section, motion string) error {
	if !contains(markMotions, motion) {
		return fmt.Errorf("%s.motion = %q, which is none of %v", section, motion, markMotions)
	}
	return nil
}

// LoadPaintClasses reads paint-classes.json from the vocab directory.
func LoadPaintClasses(vocabDir string) (PaintClasses, error) {
	path := filepath.Join(vocabDir, PaintClassesFile)
	raw, err := os.ReadFile(path)
	if err != nil {
		return PaintClasses{}, fmt.Errorf("vocab: reading %s: %w", path, err)
	}
	var parsed paintClassesJSON
	if err := json.Unmarshal(raw, &parsed); err != nil {
		return PaintClasses{}, fmt.Errorf("vocab: parsing %s: %w", path, err)
	}
	if parsed.Plain == nil {
		return PaintClasses{}, fmt.Errorf("vocab: %s: no plain row", path)
	}
	p := PaintClasses{
		Plain:          *parsed.Plain,
		ANSI:           parsed.ANSI,
		Syntax:         parsed.Syntax,
		ANSIPrecedence: parsed.ANSIPrecedence,
	}
	if err := p.validate(); err != nil {
		return PaintClasses{}, fmt.Errorf("vocab: %s: %w", path, err)
	}
	return p, nil
}

func (p PaintClasses) validate() error {
	if p.Plain != PlainClass {
		return fmt.Errorf("plain is %q; the empty string is the only spelling of unstyled text", p.Plain)
	}
	if len(p.ANSI) == 0 {
		return fmt.Errorf("ansi is empty")
	}
	if len(p.Syntax) == 0 {
		return fmt.Errorf("syntax is empty")
	}
	if len(p.ANSIPrecedence) == 0 {
		return fmt.Errorf("ansi_precedence is empty")
	}
	seen := map[string]bool{}
	for _, class := range append(append([]string{}, p.ANSI...), p.Syntax...) {
		if class == "" {
			return fmt.Errorf("an inventory entry is the empty string, which means plain")
		}
		if seen[class] {
			return fmt.Errorf("class %q appears twice in the inventory", class)
		}
		seen[class] = true
	}
	return nil
}

// AssertRosterStatusArms checks that the loaded roster_status table covers
// exactly the RosterRow.status arm names it is given, row for row. A new arm
// landing without a color fails here rather than drawing an unpainted dot.
func (c RenderColors) AssertRosterStatusArms(arms []string) error {
	return assertTable("roster_status", c.RosterStatus, arms)
}

// AssertFooterStatusArms is AssertRosterStatusArms for FooterStatus.status.
func (c RenderColors) AssertFooterStatusArms(arms []string) error {
	return assertTable("footer_status", c.FooterStatus, arms)
}

// AssertComposerOpenSubstatusArms checks that every substatus declared in
// composer_open_substatuses is a real arm of that footer status's substatus
// oneof. armsOf answers a footer status arm's substatus arm names.
func (c RenderColors) AssertComposerOpenSubstatusArms(armsOf func(status string) ([]string, error)) error {
	for status, substatuses := range c.ComposerOpenSubstatuses {
		arms, err := armsOf(status)
		if err != nil {
			return fmt.Errorf("composer_open_substatuses[%q]: %w", status, err)
		}
		for _, sub := range substatuses {
			if !contains(arms, sub) {
				return fmt.Errorf("composer_open_substatuses[%q] names %q, which is not a substatus arm of it", status, sub)
			}
		}
	}
	return nil
}

// AssertMergeGlyphArms checks that every merge arm it is given carries a glyph
// treatment: color 'none' in roster_status and a glyph name in merge_glyphs.
// A merge arm landing without a glyph would draw as nothing at all.
func (c RenderColors) AssertMergeGlyphArms(arms []string) error {
	return assertTable("merge_glyphs", c.MergeGlyphs, arms)
}

func assertTable(name string, rows map[string]string, arms []string) error {
	var missing, extra []string
	want := map[string]bool{}
	for _, arm := range arms {
		want[arm] = true
		if _, ok := rows[arm]; !ok {
			missing = append(missing, arm)
		}
	}
	for key := range rows {
		if !want[key] {
			extra = append(extra, key)
		}
	}
	if len(missing) == 0 && len(extra) == 0 {
		return nil
	}
	sort.Strings(missing)
	sort.Strings(extra)
	return fmt.Errorf("vocab: %s diverges from the contract: missing %v, unknown %v", name, missing, extra)
}

// RosterStatusColor answers the color of one RosterRow.status arm on a given
// surface, honoring that surface's DECLARED overrides. A surface with no
// overrides takes roster_status verbatim.
func (c RenderColors) RosterStatusColor(surface, arm string) (string, error) {
	if override, ok := c.SurfaceOverrides[surface][arm]; ok {
		return override, nil
	}
	color, ok := c.RosterStatus[arm]
	if !ok {
		return "", fmt.Errorf("vocab: roster_status has no row for %q", arm)
	}
	return color, nil
}

// FooterStatusColor answers the color of one FooterStatus.status arm.
func (c RenderColors) FooterStatusColor(arm string) (string, error) {
	color, ok := c.FooterStatus[arm]
	if !ok {
		return "", fmt.Errorf("vocab: footer_status has no row for %q", arm)
	}
	return color, nil
}

// TopbarTone answers the topbar tone of one daemon link state.
func (c RenderColors) TopbarTone(linkState string) (string, error) {
	tone, ok := c.TopbarConnectivity[linkState]
	if !ok {
		return "", fmt.Errorf("vocab: topbar_connectivity has no row for %q", linkState)
	}
	return tone, nil
}

// FailureSideColor answers the color a failure card takes for its side.
func (c RenderColors) FailureSideColor(side string) (string, error) {
	color, ok := c.FailureSides[side]
	if !ok {
		return "", fmt.Errorf("vocab: failure_sides has no row for %q", side)
	}
	return color, nil
}

// Contains reports whether class is in the inventory. The paint package asserts
// every class it emits.
func (p PaintClasses) Contains(class string) bool {
	if class == PlainClass {
		return true
	}
	return contains(p.ANSI, class) || contains(p.Syntax, class)
}

// OneofArmNames answers the field names of one oneof of a message descriptor,
// in declaration order. It is how a caller states the contract's arm set from
// the generated descriptors rather than from a hand-kept list. A message with
// no such oneof is an error.
func OneofArmNames(msg protoreflect.MessageDescriptor, oneof string) ([]string, error) {
	od := msg.Oneofs().ByName(protoreflect.Name(oneof))
	if od == nil {
		return nil, fmt.Errorf("vocab: %s has no oneof %q", msg.FullName(), oneof)
	}
	fields := od.Fields()
	names := make([]string, 0, fields.Len())
	for i := 0; i < fields.Len(); i++ {
		names = append(names, string(fields.Get(i).Name()))
	}
	return names, nil
}

func contains(haystack []string, needle string) bool {
	for _, s := range haystack {
		if s == needle {
			return true
		}
	}
	return false
}

func sameSet(a, b []string) bool {
	if len(a) != len(b) {
		return false
	}
	seen := map[string]bool{}
	for _, s := range a {
		seen[s] = true
	}
	for _, s := range b {
		if !seen[s] {
			return false
		}
		delete(seen, s)
	}
	return len(seen) == 0
}

// RosterStatusArms is every RosterRow.status arm, by its proto name, read off
// the generated descriptor in declaration order. It is the ONE list of the
// roster's arms on the daemon's side: the resolver asserts the vocabulary's
// tables against it, so an arm landing in the proto without a color fails
// there. The descriptor always declares the oneof, so a failure to read it
// is a broken build and panics.
func RosterStatusArms() []string {
	arms, err := OneofArmNames((&frontendv1.RosterRow{}).ProtoReflect().Descriptor(), "status")
	if err != nil {
		panic(fmt.Sprintf("vocab: RosterRow.status is unreadable: %v", err))
	}
	return arms
}

// FooterStatusArms is every FooterStatus.status arm, by its proto name, read
// off the generated descriptor in declaration order: the footer resolver's
// one list, against which footer_status is asserted. An arm the contract keeps
// only so an older frame decodes (`turn_failed`) is in it, and painted, like
// any other. A failure to read it is a broken build and panics.
func FooterStatusArms() []string {
	arms, err := OneofArmNames((&frontendv1.FooterStatus{}).ProtoReflect().Descriptor(), "status")
	if err != nil {
		panic(fmt.Sprintf("vocab: FooterStatus.status is unreadable: %v", err))
	}
	return arms
}

// IsMergeArm reports whether a RosterRow.status arm is one of the merge
// pipeline's: every such arm, and only such an arm, is spelled `merg...`
// (merging, merge_queued, merge_failed, merged), and each draws the merge
// glyph rather than a lifecycle dot.
func IsMergeArm(arm string) bool {
	return strings.HasPrefix(arm, "merg")
}

// RosterMergeArms is every merge arm of RosterRow.status (IsMergeArm), in
// declaration order.
func RosterMergeArms() []string {
	var out []string
	for _, arm := range RosterStatusArms() {
		if IsMergeArm(arm) {
			out = append(out, arm)
		}
	}
	return out
}
