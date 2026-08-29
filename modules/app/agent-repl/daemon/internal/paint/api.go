// Package paint turns raw text into paint spans: the ANSI escape parser and
// the syntax highlighter.
//
// THE CLIENT NEVER PARSES ESCAPES — the daemon does it once and the client
// paints classes. A span carries exactly one class, so an ANSI run that is
// both bold and red becomes adjacent spans over the same text. Every emitted
// class is asserted against the paint-classes.json inventory. See
// ARCHITECTURE.md "Paint classes".
package paint

import (
	"claude-repld/internal/notimpl"
	"claude-repld/internal/vocab"
)

// Span is one painted run of text.
type Span struct {
	// Text is the run, always the substance.
	Text string
	// Class is the run's single paint_class name, from the inventory. Empty
	// means the inventory's plain class.
	Class string
}

// Spans is a painted sequence. Concatenating every Text reproduces the input
// exactly: painting never adds, drops or reorders a byte.
type Spans []Span

// Painter emits spans and asserts them against the loaded inventory.
type Painter interface {
	// ParseANSI turns process output carrying SGR escape sequences into spans,
	// splitting a run that carries several attributes into adjacent spans in
	// the inventory's declared precedence order. Escapes it does not model are
	// dropped from the text and produce no class.
	ParseANSI(text string) (Spans, error)
	// Highlight turns a code block into spans using the language-neutral
	// highlight inventory. An unrecognized language yields one plain span
	// rather than an error.
	Highlight(language, code string) (Spans, error)
}

// New builds a Painter that asserts every class it emits against classes.
func New(classes vocab.PaintClasses) (Painter, error) {
	return nil, notimpl.Err
}
