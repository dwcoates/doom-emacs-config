package main

import (
	"os"
	"os/signal"
	"runtime"
	"runtime/pprof"
	"strings"
	"syscall"

	"claude-repld/internal/dlog"
)

// dumpCap bounds how much of a goroutine dump goes into ONE run-log record.
//
// The record is JSON on one line and the run log is read by tooling that reads
// it a line at a time, so an unbounded dump from a daemon with thousands of
// goroutines would be a single multi-megabyte line. A megabyte is far past
// every dump this daemon has produced — a wedged boot's is a few kilobytes —
// and the truncation says so in the record rather than silently.
const dumpCap = 1 << 20

// goroutineDump renders every goroutine's stack and says how many there were.
//
// debug=2 is the same rendering the runtime writes on an uncaught panic or on
// an unhandled SIGQUIT: every goroutine, with its state and its full stack,
// which is what tells a wedged boot's blocking call from an idle one.
func goroutineDump() (string, int) {
	count := runtime.NumGoroutine()
	var out strings.Builder
	if err := pprof.Lookup("goroutine").WriteTo(&out, 2); err != nil {
		// THE ERROR IS THE DUMP. A profile that cannot be rendered is itself
		// the diagnostic, and returning an empty string would report a daemon
		// with no goroutines.
		return "the goroutine profile could not be rendered: " + err.Error(), count
	}
	text := out.String()
	if len(text) > dumpCap {
		text = text[:dumpCap] + "\n... the goroutine dump was truncated at " + itoa(dumpCap) + " bytes"
	}
	return text, count
}

// itoa spells a whole number without pulling strconv into this file's imports
// for one call.
func itoa(n int) string {
	if n == 0 {
		return "0"
	}
	var digits [20]byte
	i := len(digits)
	for n > 0 {
		i--
		digits[i] = byte('0' + n%10)
		n /= 10
	}
	return string(digits[i:])
}

// recordGoroutineDump puts a goroutine dump into the run log at ERROR.
//
// IT GOES IN THE RUN LOG, not on stderr. Emacs launches the daemon with its
// stderr discarded, so the runtime's own SIGQUIT dump — the one thing that
// would have named the blocking call in pid 31984's ten-hour accept wedge —
// went nowhere. The run log is the daemon's narrative and it is the only place
// a later diagnosis can read.
func recordGoroutineDump(log dlog.Logger, operation, message string, context dlog.Context) {
	dump, count := goroutineDump()
	if context == nil {
		context = dlog.Context{}
	}
	context["goroutines"] = count
	context["goroutine_dump"] = dump
	log.Error(operation, message, context)
}

// armGoroutineDump takes SIGQUIT over from the runtime so the dump lands in the
// run log, and then ends the process non-zero.
//
// NON-ZERO ON PURPOSE: SIGQUIT is what an operator sends a daemon that is not
// answering, and Emacs respawns a daemon that exited. A caught SIGQUIT that
// left the process running would turn the one gesture that diagnoses a wedge
// into a gesture that also hides it.
//
// The returned function disarms the handler.
func armGoroutineDump(log dlog.Logger) func() {
	quit := make(chan os.Signal, 1)
	signal.Notify(quit, syscall.SIGQUIT)
	go func() {
		if _, ok := <-quit; !ok {
			return
		}
		recordGoroutineDump(log, "daemon.cmd.sigquit", "SIGQUIT: dumping every goroutine before exiting", dlog.Context{
			"pid": os.Getpid(),
		})
		os.Exit(exitFailure)
	}()
	return func() {
		signal.Stop(quit)
		close(quit)
	}
}
