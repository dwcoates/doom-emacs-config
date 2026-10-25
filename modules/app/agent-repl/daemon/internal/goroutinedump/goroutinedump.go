// Package goroutinedump renders every goroutine's stack for a durable record.
//
// It is the daemon's ONE rendering of a goroutine dump: the SIGQUIT handler and
// the wedged-boot bound (cmd/agent-repld/diagnose.go) and the lock stall
// watchdog (internal/lockwatch) all put this text into a record, so a reader
// sees the same shape whichever of them wrote it.
package goroutinedump

import (
	"regexp"
	"runtime"
	"runtime/pprof"
	"strconv"
	"strings"
)

// Cap bounds how much of a goroutine dump goes into ONE record.
//
// The record is JSON on one line and the logs are read by tooling that reads
// them a line at a time, so an unbounded dump from a daemon with thousands of
// goroutines would be a single multi-megabyte line. The truncation says so in
// the text rather than silently.
const Cap = 1 << 20

// listedIDs bounds how many goroutine ids a group names; the count is always
// whole.
const listedIDs = 8

// Render renders every goroutine's stack, grouped, and says how many there
// were.
//
// debug=2 is the same rendering the runtime writes on an uncaught panic or on
// an unhandled SIGQUIT: every goroutine, with its state and its full stack,
// which is what tells a wedged call from an idle one.
func Render() (string, int) {
	count := runtime.NumGoroutine()
	var out strings.Builder
	if err := pprof.Lookup("goroutine").WriteTo(&out, 2); err != nil {
		// THE ERROR IS THE DUMP. A profile that cannot be rendered is itself
		// the diagnostic, and returning an empty string would report a daemon
		// with no goroutines.
		return "the goroutine profile could not be rendered: " + err.Error(), count
	}
	return truncate(group(out.String())), count
}

var (
	// header is a goroutine's first line: its id, its state and, when it has
	// waited a while, how long.
	header = regexp.MustCompile(`^goroutine (\d+) \[([^,\]]+)(?:, [^\]]+)?\]:$`)
	// offset is a source line's pc offset.
	offset = regexp.MustCompile(` \+0x[0-9a-f]+$`)
	// creator is the goroutine a "created by" line names.
	creator = regexp.MustCompile(` in goroutine \d+$`)
)

// group renders the goroutines that share a state and a stack ONCE, with how
// many there are and which.
//
// A DAEMON'S DUMP IS MOSTLY IDENTICAL STACKS. A thousand goroutines are mostly
// one stream's, one pool's or one store's waiters, written out one by one; on
// 2026-10-24 that ran a stall's dump past Cap and cut off the goroutine that
// held the lock. Argument words, pc offsets, wait durations and creating ids
// differ per goroutine and say nothing about where it is stuck, so they are
// left out of the key and the stack is printed without them. Groups keep the
// order their first goroutine appeared in.
func group(dump string) string {
	type stack struct {
		state string
		ids   []string
		text  string
	}
	var order []string
	groups := map[string]*stack{}
	for block := range strings.SplitSeq(strings.TrimSpace(dump), "\n\n") {
		lines := strings.Split(block, "\n")
		m := header.FindStringSubmatch(lines[0])
		if m == nil {
			// NOT A GOROUTINE: kept whole and alone, never folded into a
			// group, so an unexpected rendering is still all there. Its key
			// is its position, which no stack's key can be.
			key := "\x00" + strconv.Itoa(len(order))
			order = append(order, key)
			groups[key] = &stack{text: block}
			continue
		}
		frames := make([]string, 0, len(lines)-1)
		for _, line := range lines[1:] {
			if strings.HasPrefix(line, "\t") {
				line = offset.ReplaceAllString(line, "")
			} else {
				line = creator.ReplaceAllString(withoutArgs(line), "")
			}
			frames = append(frames, line)
		}
		text := strings.Join(frames, "\n")
		key := m[2] + "\n" + text
		g, ok := groups[key]
		if !ok {
			g = &stack{state: m[2], text: text}
			groups[key] = g
			order = append(order, key)
		}
		g.ids = append(g.ids, m[1])
	}
	var out strings.Builder
	for i, key := range order {
		if i > 0 {
			out.WriteString("\n\n")
		}
		g := groups[key]
		if g.ids == nil {
			out.WriteString(g.text)
			continue
		}
		out.WriteString(groupHeader(len(g.ids), g.state, g.ids))
		out.WriteString("\n")
		out.WriteString(g.text)
	}
	return out.String()
}

// withoutArgs replaces a frame's argument list, its LAST parenthesized group,
// with "(...)": the words in it differ per goroutine. The receiver's "(*T)" is
// part of the function's name and stays. A line not ending in a group, a
// "created by" line, is answered as it is.
func withoutArgs(line string) string {
	if !strings.HasSuffix(line, ")") {
		return line
	}
	depth := 0
	for i := len(line) - 1; i >= 0; i-- {
		switch line[i] {
		case ')':
			depth++
		case '(':
			depth--
			if depth == 0 {
				return line[:i] + "(...)"
			}
		}
	}
	return line
}

// groupHeader names a group: its size, its state and its first ids.
func groupHeader(n int, state string, ids []string) string {
	listed := ids
	if len(listed) > listedIDs {
		listed = listed[:listedIDs]
	}
	noun := "goroutines"
	if n == 1 {
		noun = "goroutine"
	}
	head := strconv.Itoa(n) + " " + noun + " [" + state + "]: " + strings.Join(listed, ", ")
	if len(ids) > listedIDs {
		head += ", ..."
	}
	return head
}

// truncate cuts a rendering at Cap and says that it did.
func truncate(text string) string {
	if len(text) <= Cap {
		return text
	}
	return text[:Cap] + "\n... the goroutine dump was truncated at " + strconv.Itoa(Cap) + " bytes"
}
