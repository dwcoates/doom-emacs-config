package prose

import (
	"fmt"
	"strings"
	"sync"

	"agent-repld/internal/dlog"

	tldrprompt "agentrepl/tldrprompt"

	frontendv1 "agentrepl/proto/frontend/v1"

	ts "github.com/tree-sitter/go-tree-sitter"
)

// promptLanguage is the prompt grammar, which reads a prompt's entries.
var promptLanguage = ts.NewLanguage(tldrprompt.Language())

// promptParsers pools tree-sitter parsers of the prompt grammar, which are not
// safe for concurrent use.
var promptParsers = sync.Pool{New: func() any {
	p := ts.NewParser()
	if err := p.SetLanguage(promptLanguage); err != nil {
		panic(fmt.Sprintf("prose: the tree-sitter-tldr prompt grammar does not load: %v", err))
	}
	return p
}}

// entrySpan is one entry of a prompt's text: the byte span of its section,
// its label, and where what it says begins (after the label and any
// separator).
type entrySpan struct {
	start, end int
	label      string
	textStart  int
}

// ParsePrompt parses a PROMPT's TEXT into the ParsedProse the webapp draws:
// every entry answering a label (`3.1.2 - my answer`) as an entry block, and
// the text around the entries exactly as Parse reads it, trees and all. A
// prompt is settled text. Its source is the text as written: a prompt's
// labels are the person's, and nothing renumbers them.
//
// A failure is a bug, recorded on LOG once and then a panic, as Parse's is.
func ParsePrompt(log dlog.Logger, text string) (parsed *frontendv1.ParsedProse) {
	defer func() {
		if r := recover(); r != nil {
			log.Error(invariantOperation, "the daemon could not parse a prompt it serves", dlog.Context{
				"function": "ParsePrompt",
				"length":   len(text),
				"cause":    fmt.Sprint(r),
			})
			panic(r)
		}
	}()
	return parsePromptText(text)
}

// parsePromptText is the parse ParsePrompt records the failures of; a test
// replaces it to reach ParsePrompt's failure path.
var parsePromptText = parsePrompt

// parsePrompt is ParsePrompt without the record: a failure panics.
func parsePrompt(text string) *frontendv1.ParsedProse {
	entries, err := readEntries(text)
	if err != nil {
		panic(err.Error())
	}
	if len(entries) == 0 {
		parsed := parse(text, Settled)
		parsed.Source = &frontendv1.ProseSource{Markdown: text}
		return parsed
	}
	out := &frontendv1.ParsedProse{Source: &frontendv1.ProseSource{Markdown: text}}
	at := 0
	for _, e := range entries {
		if e.start > at {
			out.Blocks = append(out.Blocks, parse(text[at:e.start], Settled).Blocks...)
		}
		says := strings.TrimRight(strings.TrimLeft(text[e.textStart:e.end], " \t\n"), "\n")
		out.Blocks = append(out.Blocks, &frontendv1.ProseBlock{Block: &frontendv1.ProseBlock_Entry{Entry: &frontendv1.ProseEntry{
			Label: &frontendv1.ProseEntryLabel{Text: e.label},
			Text:  &frontendv1.ProseEntryText{Markdown: says},
		}}})
		at = e.end
	}
	if at < len(text) {
		out.Blocks = append(out.Blocks, parse(text[at:], Settled).Blocks...)
	}
	return out
}

// readEntries answers TEXT's entries, in order: every section of the prompt
// grammar's parse whose heading carries no marker. A heading with a marker
// (`1 🔧 text`) opens a pasted tree's root, which is the tree's, not an entry.
// A parse the grammar could not complete is an error: it accepts every text
// whole by design.
func readEntries(text string) ([]entrySpan, error) {
	p := promptParsers.Get().(*ts.Parser)
	defer promptParsers.Put(p)
	src := []byte(text)
	tree := p.Parse(src, nil)
	if tree == nil {
		return nil, fmt.Errorf("the prompt grammar returned no tree for a %d-byte text", len(text))
	}
	defer tree.Close()
	root := tree.RootNode()
	if root.HasError() {
		sexp := root.ToSexp()
		if len(sexp) > 500 {
			sexp = sexp[:500]
		}
		return nil, fmt.Errorf("the prompt grammar could not parse a %d-byte text, which it accepts whole by design: %s", len(text), sexp)
	}
	var entries []entrySpan
	for i := uint(0); i < root.NamedChildCount(); i++ {
		section := root.NamedChild(i)
		if section.Kind() != "section" {
			continue
		}
		heading := childOfKind(section, "heading")
		if heading == nil {
			return nil, fmt.Errorf("a section of the prompt grammar's parse has no heading: %s", section.ToSexp())
		}
		if childOfKind(heading, "marker") != nil {
			continue
		}
		label := childOfKind(heading, "label")
		if label == nil {
			return nil, fmt.Errorf("a heading of the prompt grammar's parse has no label: %s", heading.ToSexp())
		}
		textStart := int(label.EndByte())
		if sep := childOfKind(heading, "separator"); sep != nil {
			textStart = int(sep.EndByte())
		}
		entries = append(entries, entrySpan{
			start:     int(section.StartByte()),
			end:       int(section.EndByte()),
			label:     strings.TrimSpace(text[label.StartByte():label.EndByte()]),
			textStart: textStart,
		})
	}
	return entries, nil
}

// ReplaceEntry answers TEXT with the first entry answering LABEL saying
// SAYS instead: its label, its separator and the whitespace after them kept,
// and every other word of TEXT unchanged. ok is false, and TEXT is answered
// unchanged, when no entry of TEXT answers LABEL.
func ReplaceEntry(text, label, says string) (string, bool) {
	entries, err := readEntries(text)
	if err != nil {
		panic(err.Error())
	}
	for _, e := range entries {
		if e.label != label {
			continue
		}
		content := text[e.textStart:e.end]
		lead := content[:len(content)-len(strings.TrimLeft(content, " \t\n"))]
		trail := content[len(strings.TrimRight(content, "\n")):]
		if lead == "" && says != "" {
			lead = " "
		}
		return text[:e.textStart] + lead + says + trail + text[e.end:], true
	}
	return text, false
}
