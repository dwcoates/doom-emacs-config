package convert

// diff.go — THE PRODUCER DIFFS.
//
// A write hands over the file's whole new contents: the vendor states
// `originalFile` (null for a creation) and `content`, and its own
// `structuredPatch` is EMPTY for the create case. Nothing upstream says what
// CHANGED, so the producer diffs the two versions here, once, at the moment the
// change is recorded — the same thing the stream plane's `diffHunks` does, so
// both planes mint the identical patch for one write.
//
// The algorithm is a longest-common-prefix and suffix trim rather than a full
// diff: it is exact for the shape a write actually takes (a replaced region),
// it is linear, and it never invents an alignment inside the changed region
// that a reader would take for real.

import (
	"strings"

	conversationv1 "agentrepl/proto/conversation/v1"
)

// diffContext is how many unchanged lines ride on either side of the change.
const diffContext = 3

// diffHunks states the hunks between two whole versions of a file. Two
// identical versions changed nothing, which is no hunks rather than an empty
// one.
func diffHunks(before, after string) []*conversationv1.FilePatchHunk {
	if before == after {
		return nil
	}
	oldLines := splitLines(before)
	newLines := splitLines(after)

	prefix := 0
	for prefix < len(oldLines) && prefix < len(newLines) && oldLines[prefix] == newLines[prefix] {
		prefix++
	}
	suffix := 0
	for suffix < len(oldLines)-prefix && suffix < len(newLines)-prefix &&
		oldLines[len(oldLines)-1-suffix] == newLines[len(newLines)-1-suffix] {
		suffix++
	}

	removed := oldLines[prefix : len(oldLines)-suffix]
	added := newLines[prefix : len(newLines)-suffix]
	contextBefore := oldLines[max(0, prefix-diffContext):prefix]
	contextAfter := oldLines[len(oldLines)-suffix : min(len(oldLines), len(oldLines)-suffix+diffContext)]

	lines := make([]string, 0, len(contextBefore)+len(removed)+len(added)+len(contextAfter))
	for _, line := range contextBefore {
		lines = append(lines, " "+line)
	}
	for _, line := range removed {
		lines = append(lines, "-"+line)
	}
	for _, line := range added {
		lines = append(lines, "+"+line)
	}
	for _, line := range contextAfter {
		lines = append(lines, " "+line)
	}

	start := uint32(max(1, prefix-len(contextBefore)+1))
	return []*conversationv1.FilePatchHunk{{
		OldRange: &conversationv1.FilePatchHunkRange{
			Start: start,
			Lines: uint32(len(contextBefore) + len(removed) + len(contextAfter)),
		},
		NewRange: &conversationv1.FilePatchHunkRange{
			Start: start,
			Lines: uint32(len(contextBefore) + len(added) + len(contextAfter)),
		},
		Lines: lines,
	}}
}

// splitLines reads a file version as its lines. An EMPTY version has no lines
// at all, rather than one empty line a diff would draw as a change.
//
// A FILE'S TERMINATING NEWLINE IS NOT A LINE. Text files end with one, so a
// bare Split leaves a final empty element that is the terminator rather than
// any content — and a creation then drew a one-line file as TWO additions,
// the second of them blank, and stated "+1,2" for it. `diff` itself counts
// "one\n" as one line, and so does the card now. A version that genuinely
// ends in a blank line is "a\n\n", which keeps its blank line here because
// only ONE trailing empty element is dropped.
func splitLines(text string) []string {
	if text == "" {
		return nil
	}
	lines := strings.Split(text, "\n")
	if last := len(lines) - 1; lines[last] == "" {
		lines = lines[:last]
	}
	return lines
}
