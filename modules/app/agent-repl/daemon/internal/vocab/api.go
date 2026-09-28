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
	"os"
	"path/filepath"
	"sort"

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
// takes, plus the glyph names the merge states carry beside their color.
// Values are the names the file spells; nothing here holds a hex value,
// because each renderer keeps its own.
type RenderColors struct {
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
	// FooterStatus maps each FooterStatus.status arm name to its color.
	FooterStatus map[string]string
	// ComposerClosedColors are the footer status colors under which a
	// composer is CLOSED (owner ruling, 2026-09-28): blue, an unusable
	// workspace, and purple, a merge holding it. Every other color is a
	// usable workspace whose composer is open.
	ComposerClosedColors []string
	// FooterAllowance maps each FooterAllowance.status arm name to its color.
	FooterAllowance map[string]string
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

// renderColorsJSON is the file's on-disk shape. It is separate from
// RenderColors so an absent table is distinguishable from an empty one.
type renderColorsJSON struct {
	Colors               []string                     `json:"colors"`
	Precedence           []string                     `json:"precedence"`
	RosterStatus         map[string]string            `json:"roster_status"`
	MergeGlyphs          map[string]string            `json:"merge_glyphs"`
	ColoredMergeArms     []string                     `json:"colored_merge_arms"`
	FeedMergeHeadGlyph   string                       `json:"feed_merge_head_glyph"`
	FooterStatus         map[string]string            `json:"footer_status"`
	ComposerClosedColors []string                     `json:"composer_closed_colors"`
	FooterAllowance      map[string]string            `json:"footer_allowance"`
	TopbarConnectivity   map[string]string            `json:"topbar_connectivity"`
	TopbarTones          []string                     `json:"topbar_tones"`
	SurfaceOverrides     map[string]map[string]string `json:"surface_overrides"`
	FailureSides         map[string]string            `json:"failure_sides"`
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

func (c RenderColors) validate() error {
	if len(c.Colors) == 0 {
		return fmt.Errorf("colors is empty")
	}
	if !sameSet(c.Colors, c.Precedence) {
		return fmt.Errorf("precedence %v does not cover colors %v", c.Precedence, c.Colors)
	}
	for _, table := range []struct {
		name string
		rows map[string]string
	}{
		{"roster_status", c.RosterStatus},
		{"footer_status", c.FooterStatus},
		{"footer_allowance", c.FooterAllowance},
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

// AssertFooterAllowanceArms is AssertRosterStatusArms for
// FooterAllowance.status: allowed, allowed_warning, rejected.
func (c RenderColors) AssertFooterAllowanceArms(arms []string) error {
	return assertTable("footer_allowance", c.FooterAllowance, arms)
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

// FooterAllowanceColor answers the color of one FooterAllowance.status arm. An
// arm the table does not carry is an error, never a default color.
func (c RenderColors) FooterAllowanceColor(arm string) (string, error) {
	color, ok := c.FooterAllowance[arm]
	if !ok {
		return "", fmt.Errorf("vocab: footer_allowance has no row for %q", arm)
	}
	return color, nil
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
