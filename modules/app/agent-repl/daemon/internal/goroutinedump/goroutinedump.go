// Package goroutinedump renders every goroutine's stack for a durable record.
//
// It is the daemon's ONE rendering of a goroutine dump: the SIGQUIT handler and
// the wedged-boot bound (cmd/claude-repld/diagnose.go) and the lock stall
// watchdog (internal/lockwatch) all put this text into a record, so a reader
// sees the same shape whichever of them wrote it.
package goroutinedump

import (
	"runtime"
	"runtime/pprof"
	"strconv"
	"strings"
)

// Cap bounds how much of a goroutine dump goes into ONE record.
//
// The record is JSON on one line and the logs are read by tooling that reads
// them a line at a time, so an unbounded dump from a daemon with thousands of
// goroutines would be a single multi-megabyte line. A megabyte is far past
// every dump this daemon has produced — a wedged boot's is a few kilobytes —
// and the truncation says so in the text rather than silently.
const Cap = 1 << 20

// Render renders every goroutine's stack and says how many there were.
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
	return truncate(out.String()), count
}

// truncate cuts a rendering at Cap and says that it did.
func truncate(text string) string {
	if len(text) <= Cap {
		return text
	}
	return text[:Cap] + "\n... the goroutine dump was truncated at " + strconv.Itoa(Cap) + " bytes"
}
