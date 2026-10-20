package procmetrics

import (
	"agent-repld/internal/dlog"
	"agent-repld/internal/streamslot"
)

// EmacsProcess is the pid of the Emacs whose WatchDaemon stream stands on this
// daemon (agentrepl.v1.WatchDaemonEmacs.process), SCOPED TO THAT STREAM: the
// stream attaches it, its end releases it, and a later stream supersedes it.
// The collector measures that Emacs, and the WebKit processes it is
// responsible for, as long as it stands.
type EmacsProcess struct {
	log  dlog.Logger
	slot streamslot.Slot[int]
}

// NewEmacsProcess builds a holder with no Emacs standing.
func NewEmacsProcess(log dlog.Logger) *EmacsProcess {
	if log == nil {
		panic("procmetrics: NewEmacsProcess requires a logger")
	}
	return &EmacsProcess{log: log}
}

// Attach stands the pid an Emacs stream stated, answering the release the
// stream calls when it ends.
func (e *EmacsProcess) Attach(pid int) (release func()) {
	generation, superseded := e.slot.Attach(pid)
	e.log.Info(Operation, "an Emacs stream stated its process; it is measured while the stream stands", dlog.Context{
		"pid": pid, "generation": generation, "superseded": superseded,
	})
	return func() {
		if !e.slot.Release(generation) {
			return
		}
		e.log.Info(Operation, "the measured Emacs's stream ended; no Emacs is measured", dlog.Context{
			"pid": pid, "generation": generation,
		})
	}
}

// PID answers the standing Emacs's pid, and whether one stands.
func (e *EmacsProcess) PID() (int, bool) { return e.slot.Get() }
