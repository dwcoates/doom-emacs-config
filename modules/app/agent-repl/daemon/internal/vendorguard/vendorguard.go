// Package vendorguard refuses to exec a vendor (Claude/Anthropic) binary when
// the daemon is running under a test harness.
//
// WHY: a test that reaches the real `claude` CLI spends the user's tokens and
// makes the suite non-hermetic and non-deterministic. Every Go test harness in
// this repo sets AGENT_REPL_FORBID_VENDOR_CALLS, and this package turns that
// variable into a hard refusal at each vendor-exec site.
//
// The refusal is an ERROR RETURN, never a skip and never a fallback verdict:
// callers already surface their spawn errors, so a tripped guard shows up as a
// loud failure naming the variable rather than as a plausible-looking result.
// Production code must never set the variable.
package vendorguard

import (
	"fmt"
	"os"
)

// EnvVar is the environment variable that forbids real vendor calls.
const EnvVar = "AGENT_REPL_FORBID_VENDOR_CALLS"

// Forbidden reports whether vendor calls are currently forbidden, i.e. EnvVar
// is set to any non-empty value.
func Forbidden() bool { return os.Getenv(EnvVar) != "" }

// Check returns a non-nil error naming EnvVar when vendor calls are forbidden.
// site names the call path being blocked, so a failure says WHICH vendor entry
// a test tripped instead of just that one did.
func Check(site string) error {
	if !Forbidden() {
		return nil
	}
	return fmt.Errorf(
		"%s is set: this process is running in test mode and real Claude/Anthropic calls are forbidden (blocked at: %s)",
		EnvVar, site)
}
