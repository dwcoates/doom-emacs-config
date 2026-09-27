package main

import (
	"os"
	"os/signal"
	"syscall"

	"claude-repld/internal/dlog"
	"claude-repld/internal/goroutinedump"
)

// recordGoroutineDump puts a goroutine dump into the run log at ERROR.
//
// IT GOES IN THE RUN LOG, not on stderr. Emacs launches the daemon with its
// stderr discarded, so the runtime's own SIGQUIT dump — the one thing that
// would have named the blocking call in pid 31984's ten-hour accept wedge —
// went nowhere. The run log is the daemon's narrative and it is the only place
// a later diagnosis can read.
func recordGoroutineDump(log dlog.Logger, operation, message string, context dlog.Context) {
	dump, count := goroutinedump.Render()
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
