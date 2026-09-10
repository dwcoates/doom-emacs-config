//go:build realtest

package realtest

import (
	"fmt"
	"regexp"
	"strings"
)

// EMACS'S OWN *Messages* BUFFER is the one source in the harvest that is not a
// structured log: it is a live buffer inside the running process, its lines
// carry no timestamps, and everything from the module's own warning rung to a
// Doom package's byte-compile complaint to a lisp backtrace lands in it as
// prose. See AGENTS.md "Logs" for how it is read.
//
// The window problem is solved by the SHAPE of a realtest rather than by
// filtering: a realtest that measures a cold start starts the Emacs process
// itself, so the whole buffer IS the run window and there is nothing to
// exclude. A realtest that runs against an already-standing Emacs records
// `(buffer-size)` in *Messages* first and reads only the tail added after —
// TailFrom is that offset.
//
// A pattern here is a claim that a line is a problem, so each one carries its
// reason. There is no allowlist on the other side: a matching line is a
// finding, full stop.

// messagePattern is one severity shape, with why it counts.
type messagePattern struct {
	re     *regexp.Regexp
	reason string
}

var messagePatterns = []messagePattern{
	{
		// The module's own rungs. `agent-repl--warn` and `agent-repl--error`
		// prepend these tags themselves (lisp/core.el), so a call site never
		// spells them and a match is unambiguous.
		re:     regexp.MustCompile(`\b(WARNING|ERROR):`),
		reason: "the module's own warn or error rung wrote this line",
	},
	{
		// `display-warning` / `lwarn`, which is how Emacs and every package
		// in Doom report a non-fatal problem.
		re:     regexp.MustCompile(`^Warning \(|^⛔ Warning \(`),
		reason: "Emacs's own warning machinery (display-warning) reported this",
	},
	{
		// An unhandled signal reaching a process filter, a timer, or a hook.
		// These are the ones that never surface anywhere else: the operation
		// is abandoned and only this line says so.
		re:     regexp.MustCompile(`error in process (filter|sentinel)|Error running timer|error during redisplay|Error in post-command-hook|Error in pre-command-hook`),
		reason: "a lisp error escaped an asynchronous callback, abandoning whatever it was doing",
	},
	{
		// The signal spellings themselves, which is what an escaped error
		// reads as when Emacs echoes it plainly.
		re:     regexp.MustCompile(`Wrong type argument|Symbol's (value as variable|function definition) is void|Args out of range|Invalid function|Wrong number of arguments|Attempt to modify a read-only|Selecting deleted buffer|Invalid face`),
		reason: "an unhandled lisp signal was echoed",
	},
	{
		// The debugger opening means a form aborted with backtracing on.
		re:     regexp.MustCompile(`^Debugger entered`),
		reason: "the lisp debugger opened, so a form aborted",
	},
	{
		// Doom's own loader complaining. A module that failed to load leaves
		// the editor missing features with no other signal.
		re:     regexp.MustCompile(`Failed to load|failed to load|could not be loaded|Doom encountered an error`),
		reason: "something failed to load",
	},
}

// HarvestMessages returns a finding for every line in the *Messages* text at or
// after TailFrom that matches a severity shape.
//
// text is the buffer verbatim, as read through emacsclient. tailFrom is a BYTE
// offset into it — the value `(buffer-size)` reported before the run — and 0
// means the whole buffer, which is the cold-start case.
//
// Attribution: the module's own lines carry a `ws=<name>` token, which is a
// workspace NAME rather than an id, so it is resolved through the state
// database's names. A line naming nothing is global.
func HarvestMessages(text string, tailFrom int, workspaces []Workspace) []Finding {
	if tailFrom > 0 && tailFrom <= len(text) {
		text = text[tailFrom:]
	}
	byName := make(map[string]string, len(workspaces))
	for _, ws := range workspaces {
		if ws.Name != "" {
			byName[ws.Name] = ws.ID
		}
	}

	var findings []Finding
	for i, line := range strings.Split(text, "\n") {
		if strings.TrimSpace(line) == "" {
			continue
		}
		for _, pattern := range messagePatterns {
			if !pattern.re.MatchString(line) {
				continue
			}
			findings = append(findings, Finding{
				Kind:      KindMessagesLine,
				Source:    "*Messages*",
				Path:      "(emacs buffer)",
				Line:      i + 1,
				Workspace: messagesWorkspace(line, byName),
				Raw:       line,
				Note:      pattern.reason,
			})
			break
		}
	}
	return findings
}

// wsTokenRe finds the `ws=<name>` token the module's log format carries.
var wsTokenRe = regexp.MustCompile(`\bws=([^\s]+)`)

func messagesWorkspace(line string, byName map[string]string) string {
	m := wsTokenRe.FindStringSubmatch(line)
	if m == nil {
		return GlobalWorkspace
	}
	name := strings.TrimRight(m[1], ".,;:")
	if id, ok := byName[name]; ok {
		return id
	}
	// A name the state database does not hold is carried through rather than
	// erased: it names something the run saw and the database does not, which
	// is itself worth reading.
	return fmt.Sprintf("unknown-workspace:%s", name)
}
