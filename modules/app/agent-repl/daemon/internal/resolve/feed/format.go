package feed

import (
	"fmt"
	"path/filepath"
	"strconv"
	"strings"
)

// The daemon formats every figure a client draws. A client that formatted one
// itself would be a second authority on the same number, and two authorities
// eventually disagree — so every string a row carries is composed here.

// trimZero renders one fractional digit and drops it when it is zero, so
// "18.0k" reads "18k".
func trimZero(v float64) string {
	s := strconv.FormatFloat(v, 'f', 1, 64)
	return strings.TrimSuffix(s, ".0")
}

// formatRuntime renders a settled call's clock — "ran 4.2 s", "ran 2m 14s",
// "ran 340 ms" — from the milliseconds between its start and its settle. A
// negative span is impossible to draw honestly, so it reports false and the
// card shows no elapsed figure at all.
func formatRuntime(startMs, endMs int64) (string, bool) {
	if startMs <= 0 || endMs <= 0 || endMs < startMs {
		return "", false
	}
	return "ran " + formatDuration(endMs-startMs), true
}

// formatDuration renders a millisecond span for a reader.
func formatDuration(ms int64) string {
	switch {
	case ms < 1_000:
		return fmt.Sprintf("%d ms", ms)
	case ms < 60_000:
		return trimZero(float64(ms)/1_000) + " s"
	default:
		minutes := ms / 60_000
		seconds := (ms % 60_000) / 1_000
		return fmt.Sprintf("%dm %ds", minutes, seconds)
	}
}

// formatOmittedExact renders the truncation line for a list the daemon knows
// the exact remainder of: "42 more not shown".
func formatOmittedExact(count uint64, noun string) string {
	return fmt.Sprintf("%s more %s not shown", formatCount(count), noun)
}

// formatOmittedAtLeast renders the truncation line for a FLOOR — a different
// claim from an exact remainder, and the only safe one when the search stopped
// counting: "at least 42 more paths not shown".
func formatOmittedAtLeast(count uint64, noun string) string {
	return fmt.Sprintf("at least %s more %s not shown", formatCount(count), noun)
}

// formatShowingOf renders a head read's truncation line: "showing 200 of
// 4,312 lines".
func formatShowingOf(shown, total uint64) string {
	return fmt.Sprintf("showing %s of %s lines", formatCount(shown), formatCount(total))
}

// formatLineRange renders an offset read's truncation line: "lines 400-499 of
// 4,312".
func formatLineRange(first, count, total uint64) string {
	last := first + count - 1
	if count == 0 {
		last = first
	}
	return fmt.Sprintf("lines %s-%s of %s", formatCount(first), formatCount(last), formatCount(total))
}

// formatEarlierLines renders a capped spool's truncation line: "1,204 earlier
// lines not shown".
func formatEarlierLines(lines uint64) string {
	return fmt.Sprintf("%s earlier lines not shown", formatCount(lines))
}

// formatCount renders an integer with thousands separators, which is how every
// figure in a drawn line is written.
func formatCount(n uint64) string {
	s := strconv.FormatUint(n, 10)
	if len(s) <= 3 {
		return s
	}
	var out strings.Builder
	lead := len(s) % 3
	if lead > 0 {
		out.WriteString(s[:lead])
	}
	for i := lead; i < len(s); i += 3 {
		if out.Len() > 0 {
			out.WriteByte(',')
		}
		out.WriteString(s[i : i+3])
	}
	return out.String()
}

// langFromPath picks the highlighter's grammar from a file's extension. An
// unrecognized extension yields the empty language, which the painter draws as
// one plain span rather than refusing.
//
// It lives here rather than in paint because it is a DRAWING decision about a
// read card — which grammar the daemon chose for this path — and paint's job
// is to highlight whatever grammar it is handed.
func langFromPath(path string) string {
	switch strings.ToLower(filepath.Ext(path)) {
	case ".go":
		return "go"
	case ".ts", ".tsx":
		return "typescript"
	case ".js", ".jsx", ".mjs", ".cjs":
		return "javascript"
	case ".py":
		return "python"
	case ".rs":
		return "rust"
	case ".c", ".h":
		return "c"
	case ".cc", ".cpp", ".hpp", ".cxx":
		return "cpp"
	case ".el":
		return "elisp"
	case ".sh", ".bash", ".zsh":
		return "shell"
	case ".json":
		return "json"
	case ".yaml", ".yml":
		return "yaml"
	case ".proto":
		return "proto"
	case ".md", ".markdown":
		return "markdown"
	case ".html", ".htm":
		return "html"
	case ".css":
		return "css"
	case ".sql":
		return "sql"
	case ".toml":
		return "toml"
	}
	return ""
}

// countLines counts the lines a blob holds, counting a trailing newline as
// ending the last line rather than starting an empty one.
func countLines(text string) uint64 {
	if text == "" {
		return 0
	}
	n := uint64(strings.Count(text, "\n"))
	if !strings.HasSuffix(text, "\n") {
		n++
	}
	return n
}
