// Package paint turns raw text into paint spans: the ANSI escape parser and
// the syntax highlighter.
//
// THE CLIENT NEVER PARSES ESCAPES — the daemon does it once and the client
// paints classes. A span carries exactly one class, so an ANSI run that is
// both bold and red is emitted under the strongest class the inventory's
// ansi_precedence names. Every emitted class is asserted against the
// paint-classes.json inventory. See ARCHITECTURE.md "Paint classes".
package paint

import (
	"fmt"

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

// Text concatenates every span's text, which is the input the spans were
// painted from with its escape sequences removed.
func (s Spans) Text() string {
	out := ""
	for _, span := range s {
		out += span.Text
	}
	return out
}

// Painter emits spans and asserts them against the loaded inventory.
type Painter interface {
	// ParseANSI turns process output carrying SGR escape sequences into spans,
	// splitting the text at every attribute change and painting each run under
	// the strongest class the inventory's ansi_precedence names. Escapes it
	// does not model are dropped from the text and produce no class.
	ParseANSI(text string) (Spans, error)
	// Highlight turns a code block into spans using the language-neutral
	// highlight inventory. An unrecognized language yields one plain span
	// rather than an error.
	Highlight(language, code string) (Spans, error)
}

// painter is the one Painter. It holds the inventory it asserts against and
// the ANSI precedence it splits by.
type painter struct {
	classes  vocab.PaintClasses
	ansiRank []string
}

// New builds a Painter that asserts every class it emits against classes.
func New(classes vocab.PaintClasses) (Painter, error) {
	if len(classes.ANSI) == 0 || len(classes.Syntax) == 0 {
		return nil, fmt.Errorf("paint: the paint-classes inventory is empty")
	}
	if len(classes.ANSIPrecedence) == 0 {
		return nil, fmt.Errorf("paint: the paint-classes inventory declares no ansi_precedence")
	}
	for _, slot := range classes.ANSIPrecedence {
		if !knownPrecedenceSlot(slot) {
			return nil, fmt.Errorf("paint: ansi_precedence names %q, which this parser does not model", slot)
		}
	}
	// Every class the highlighter can emit must exist, so a drifted inventory
	// fails at construction rather than mid-stream.
	for _, class := range highlightClasses {
		if !classes.Contains(class) {
			return nil, fmt.Errorf("paint: the inventory has no highlight class %q", class)
		}
	}
	return &painter{classes: classes, ansiRank: classes.ANSIPrecedence}, nil
}

// emit appends a span, asserting its class against the inventory first. A
// class outside the inventory is a producer bug and is surfaced, never drawn.
func (p *painter) emit(spans Spans, text, class string) (Spans, error) {
	if text == "" {
		return spans, nil
	}
	if !p.classes.Contains(class) {
		return nil, fmt.Errorf("paint: class %q is not in the paint-classes inventory", class)
	}
	if n := len(spans); n > 0 && spans[n-1].Class == class {
		spans[n-1].Text += text
		return spans, nil
	}
	return append(spans, Span{Text: text, Class: class}), nil
}
