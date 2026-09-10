package classifier

import "strings"

// ExplicitInterruptReason is the evidence an explicit-interrupt verdict
// carries. It is a constant because the tray shows it and a test pins it.
const ExplicitInterruptReason = "the prompt opens with an explicit interrupt word"

// ExplicitInterrupt reports whether a prompt takes the explicit-interrupt fast
// path: its FIRST word is one of ExplicitInterrupts, case-insensitively and
// ignoring surrounding punctuation.
//
// AGENTS.md's `-fake` section spells the rule as "whose first word is stop or
// abort (the explicit-interrupt fast path, also without --fake)", and that is
// the spelling the integration suite is written against, so first-word is what
// this implements. The seam comment's "exactly one of these words" is the
// single-word case of it.
func ExplicitInterrupt(text string) bool {
	fields := strings.Fields(text)
	if len(fields) == 0 {
		return false
	}
	first := strings.ToLower(strings.Trim(fields[0], ".,!?;:\"'"))
	for _, word := range ExplicitInterrupts {
		if first == word {
			return true
		}
	}
	return false
}
