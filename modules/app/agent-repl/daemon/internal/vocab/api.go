// Package vocab reads and asserts the two cross-language vocabulary files,
// proto/vocab/render-colors.json and proto/vocab/paint-classes.json.
//
// The daemon owns both files; the webapp and Emacs consume them. Every
// producer asserts its emitted names against the loaded inventory, which is
// the only mechanism that makes a divergence fail loudly. See ARCHITECTURE.md
// "Paint classes".
package vocab

import (
	"claude-repld/internal/notimpl"
)

// RenderColors is render-colors.json: which of the five colors each state
// takes, plus the glyph names the merge states carry instead of a color.
// Values are the names the file spells; nothing here holds a hex value,
// because each renderer keeps its own.
type RenderColors struct {
	// Colors is the closed set of color names.
	Colors []string
	// Precedence is the five-color precedence, strongest claim first.
	Precedence []string
	// RosterStatus maps each RosterRow.status arm name to its color.
	RosterStatus map[string]string
	// MergeGlyphs maps each merge arm of RosterRow.status to a glyph NAME.
	MergeGlyphs map[string]string
	// FeedMergeHeadGlyph is the glyph name a merge bubble's head carries.
	FeedMergeHeadGlyph string
	// FooterStatus maps each FooterStatus.status arm name to its color.
	FooterStatus map[string]string
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

// LoadRenderColors reads render-colors.json from the vocab directory. A file
// that does not parse, or a table with a missing row, is an error: the daemon
// refuses to serve an unpainted state.
func LoadRenderColors(vocabDir string) (RenderColors, error) {
	return RenderColors{}, notimpl.Err
}

// LoadPaintClasses reads paint-classes.json from the vocab directory.
func LoadPaintClasses(vocabDir string) (PaintClasses, error) {
	return PaintClasses{}, notimpl.Err
}

// AssertRosterStatusArms checks that the loaded roster_status table covers
// exactly the RosterRow.status arm names it is given, row for row. A new arm
// landing without a color fails here rather than drawing an unpainted dot.
func (c RenderColors) AssertRosterStatusArms(arms []string) error {
	return notimpl.Err
}

// AssertFooterStatusArms is AssertRosterStatusArms for FooterStatus.status.
func (c RenderColors) AssertFooterStatusArms(arms []string) error {
	return notimpl.Err
}

// Contains reports whether class is in the inventory. The paint package asserts
// every class it emits.
func (p PaintClasses) Contains(class string) bool {
	return false
}
