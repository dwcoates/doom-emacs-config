// Package usersetup names the one place a person running agent-repl sets the
// values that belong to them: their accounts, their browser profiles, their
// hardware. agent-repl carries no person's value as a default, so every such
// value is required, and every error raised for a missing or unmatched one
// points at the same Setup section of the user guide.
package usersetup

import "fmt"

// Doc is the Setup section every per-user value is documented in.
const Doc = "the Setup section of modules/app/agent-repl/docs/USER-GUIDE.md"

// Errorf is fmt.Errorf with the pointer to Doc appended, so every error about a
// per-user value tells its reader where the value is set.
func Errorf(format string, args ...any) error {
	return fmt.Errorf(format+" (see %s)", append(args, Doc)...)
}
