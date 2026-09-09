package feed

import (
	"errors"
	"strings"

	"claude-repld/internal/dlog"
	"claude-repld/internal/treefmt"
)

// responseTreeWidth is the column limit a settled response's tree is wrapped
// to before it is served. It is the formatter's own default — the width the
// owner's PR bodies are wrapped to — because the two clients that draw the
// tree cannot be told apart here: the webapp re-flows each line inside its
// own prefix/content split and is indifferent to where the daemon broke it,
// and Emacs draws the text as it arrives, so the daemon's break is Emacs's
// break.
const responseTreeWidth = treefmt.DefaultWidth

// formatResponseTree wraps the tree a settled response IS.
//
// Under the metaprompt the whole response is one numbered Unicode tree with
// nothing outside it but the header line, so the tree arrives BARE — not
// inside a fence or a <pre> — and the formatter's document entry point, which
// looks only inside such blocks, would touch nothing. The block entry point
// is applied to the response body instead. A fenced code block attached
// beneath a bullet (the metaprompt allows exactly that, undecorated by
// connectors) is passed through opaque: its lines are code, and a code line
// that happened to start with a number and a space must never be wrapped as
// a branch.
//
// The formatter is the owner's format_trees.py, ported one to one; nothing
// about HOW a line wraps is decided here. What is decided here is what to do
// when it cannot: a branch whose prefix alone exceeds the width is the one
// condition the formatter refuses, and a response must still be served, so
// the refusal is recorded at WARN with the formatter's own sentence and the
// markdown goes out as it arrived. Words that cannot fit are the formatter's
// warnings, recorded at DEBUG because the line is served wider rather than
// truncated and nothing is lost.
func formatResponseTree(log dlog.Logger, unit string, markdown string) string {
	if markdown == "" {
		return markdown
	}
	lines := strings.Split(markdown, "\n")
	var out []string
	var overflows, tooWide []string
	index := 0
	for index < len(lines) {
		line := lines[index]
		closer := fenceCloser(line)
		if closer == nil {
			// Collect the run of tree lines up to the next fence and wrap it
			// as one block, so a wrapped branch can still see its children.
			start := index
			for index < len(lines) && fenceCloser(lines[index]) == nil {
				index++
			}
			formatted, blockOverflows, err := treefmt.FormatBlock(lines[start:index], responseTreeWidth)
			if err != nil {
				var overflow *treefmt.OverflowError
				if errors.As(err, &overflow) {
					log.Warn("daemon.feed.response_tree_unformattable",
						"a settled response's tree could not be wrapped to the column limit and is served as it arrived",
						dlog.Context{"unit": unit, "width": responseTreeWidth, "error": overflow.Message})
					return markdown
				}
				log.Warn("daemon.feed.response_tree_unformattable",
					"a settled response's tree could not be wrapped and is served as it arrived",
					dlog.Context{"unit": unit, "width": responseTreeWidth, "error": err.Error()})
				return markdown
			}
			out = append(out, formatted...)
			overflows = append(overflows, blockOverflows...)
			for _, candidate := range formatted {
				if treefmt.VisibleWidth(candidate) > responseTreeWidth {
					tooWide = append(tooWide, candidate)
				}
			}
			continue
		}
		// A fenced block: the opener, its body, and its closer pass through
		// untouched. An unterminated fence runs to the end of the response.
		out = append(out, line)
		index++
		for index < len(lines) && !closer(lines[index]) {
			out = append(out, lines[index])
			index++
		}
		if index < len(lines) {
			out = append(out, lines[index])
			index++
		}
	}
	if len(overflows) > 0 || len(tooWide) > 0 {
		log.Debug("daemon.feed.response_tree_overflow",
			"a settled response's tree holds words wider than the column limit; they are served wider, never truncated",
			dlog.Context{"unit": unit, "width": responseTreeWidth, "overflows": len(overflows), "too_wide": len(tooWide)})
	}
	return strings.Join(out, "\n")
}

// fenceCloser is the formatter's own fence rule (block_closer) restricted to
// triple-backtick and tilde fences: the metaprompt's attached code blocks are
// fenced, never <pre>.
func fenceCloser(line string) func(string) bool {
	trimmed := strings.TrimSpace(line)
	var marker string
	switch {
	case strings.HasPrefix(trimmed, "```"):
		marker = "```"
	case strings.HasPrefix(trimmed, "~~~"):
		marker = "~~~"
	default:
		return nil
	}
	rest := strings.TrimLeft(trimmed, string(marker[0]))
	if strings.ContainsAny(strings.TrimSpace(rest), " \t") {
		// The formatter accepts one info word after the fence and nothing
		// more; a line with several words is prose, not a fence.
		return nil
	}
	return func(candidate string) bool {
		return strings.HasPrefix(strings.TrimSpace(candidate), marker)
	}
}
