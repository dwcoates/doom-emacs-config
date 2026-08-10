package main

import (
	"sync"
	"time"

	"claude-repld/internal/dlog"
)

// bootPhases narrates the daemon's boot as a sequence of measured phases.
//
// IT EXISTS BECAUSE A SLOW BOOT WAS UNATTRIBUTABLE. Emacs's startup restore
// probes the daemon by asking the frontend UDS for a connect snapshot, and when
// that probe's budget expired there was nothing in the log that said WHICH of
// the boot's dozen serial phases had spent the time. Every phase now emits one
// canonical record — its own elapsed_ms and the elapsed_ms since the process
// started — so a regression names itself rather than needing a bisect.
//
// Deferred phases (work moved off the serial boot path to run after the
// listeners serve) report through Deferred/Finish, which carries the same two
// numbers plus the outcome, so an asynchronous phase is measured exactly as
// loudly as a serial one.
type bootPhases struct {
	log   *dlog.Logger
	now   func() time.Time
	start time.Time

	mu   sync.Mutex
	last time.Time
}

// newBootPhases starts the boot clock. now is injected so tests measure a
// deterministic clock rather than the wall clock.
func newBootPhases(log *dlog.Logger, now func() time.Time) *bootPhases {
	if log == nil || now == nil {
		panic("claude-repld: boot phase tracking requires a logger and a clock")
	}
	start := now()
	return &bootPhases{log: log, now: now, start: start, last: start}
}

// Mark closes the phase that ended here: its duration is the time since the
// previous mark (or since process start, for the first).
func (b *bootPhases) Mark(phase string) {
	b.mu.Lock()
	at := b.now()
	elapsed := at.Sub(b.last)
	b.last = at
	b.mu.Unlock()
	b.log.With("operation", "boot.phase", "phase", phase,
		"elapsed_ms", elapsed.Milliseconds(),
		"since_boot_ms", at.Sub(b.start).Milliseconds()).
		Log("claude-repld: boot phase %s completed in %dms (%dms since process start)",
			phase, elapsed.Milliseconds(), at.Sub(b.start).Milliseconds())
}

// Deferred opens a phase that runs OFF the serial boot path. The returned
// function closes it, and must be called on every exit path including failure:
// a deferred phase that never reports is indistinguishable from one that hung.
func (b *bootPhases) Deferred(phase string) func(error) {
	began := b.now()
	b.log.With("operation", "boot.phase.deferred.start", "phase", phase,
		"since_boot_ms", began.Sub(b.start).Milliseconds()).
		Log("claude-repld: deferred boot phase %s started; the listeners are already serving", phase)
	return func(err error) {
		at := b.now()
		entry := b.log.With("operation", "boot.phase.deferred", "phase", phase,
			"elapsed_ms", at.Sub(began).Milliseconds(),
			"since_boot_ms", at.Sub(b.start).Milliseconds())
		if err != nil {
			// A DEFERRED FAILURE IS AS LOUD AS A SERIAL ONE. Moving work off
			// the boot path must not move its failures out of the log.
			entry.With("outcome", "failed", "error", err.Error()).
				LogError("claude-repld: deferred boot phase %s FAILED after %dms: %v",
					phase, at.Sub(began).Milliseconds(), err)
			return
		}
		entry.With("outcome", "ok").
			Log("claude-repld: deferred boot phase %s completed in %dms (%dms since process start)",
				phase, at.Sub(began).Milliseconds(), at.Sub(b.start).Milliseconds())
	}
}
