package sessionwatcher

import (
	"time"

	"agent-repld/internal/dlog"
)

// A REFUSED WATCH ON EXPECTED WORK IS RE-OPENED ON A TIMER (owner ruling,
// 2026-10-08).
//
// A watch open the shim REFUSES (not_found, failed_precondition) is the
// bring-up race: the store holds no rows for the handle YET. The re-open used
// to wait for an edge that might never come -- a repeated announcement, a
// fleet re-open, a session frame -- and none of them comes for a shell that
// has already ended. MEASURED 2026-10-08T18:15:27, workspace `doom`: a shell a
// backgrounded subagent launched was refused once while the sidecar had not
// yet written its rows, the watch was never re-opened, its terminal never
// arrived, the shell stood live for good, and a merge waiting for the
// workspace to fall free waited forever with the owner's prompt held behind
// it.
//
// So every refused open of an EXPECTED handle schedules its own re-open on the
// watcher's clock: reopenFloor doubling per consecutive refusal, capped at
// reopenCeiling. The timer is the edge, and it is guaranteed to come. The
// refusal's existing bound (openRefusalLimit) still decides when the refusal
// is reported as a fault, at WARN, exactly as before; the re-opens go on past
// it, at the ceiling, until the watch opens (which closes the fault) or the
// work concludes (which reaps the watch and cancels its pending re-open).

// reopenFloor is the first refused open's wait before its re-open. The
// sidecar writes a shell's first rows when it next reads the spool, measured
// at ~500ms after the conclusion in the 2026-10-08 incident, so the first
// re-open lands inside that window and the second after it.
const reopenFloor = 250 * time.Millisecond

// reopenCeiling caps the wait between re-opens of one refused watch. A shim
// that refuses for good costs one refused round trip per ceiling.
const reopenCeiling = 30 * time.Second

// reopenDelay is the wait before the re-open that follows the n-th
// consecutive refusal (n >= 1).
func reopenDelay(refusals int) time.Duration {
	delay := reopenFloor
	for i := 1; i < refusals && delay < reopenCeiling; i++ {
		delay *= 2
	}
	if delay > reopenCeiling {
		return reopenCeiling
	}
	return delay
}

// scheduleReopenLocked arms the re-open of one refused watch. pending is the
// watch's own mark that a re-open is armed, so a refusal never arms a second
// timer beside one already waiting. reopen runs under mu when the timer
// fires; it decides afresh whether the watch still wants opening.
func (w *watcher) scheduleReopenLocked(operation, handle string, refusals int, pending *bool, reopen func()) {
	if *pending {
		w.log.Debug("daemon.sessionwatcher.reopen_scheduled", "a re-open of this refused watch is already armed", dlog.Context{
			"operation": operation, "handle": handle, "refusals": refusals,
		})
		return
	}
	*pending = true
	delay := reopenDelay(refusals)
	gen := w.gen
	w.log.Info("daemon.sessionwatcher.reopen_scheduled", "a refused watch on expected work will be re-opened after its backoff", dlog.Context{
		"operation": operation, "handle": handle, "refusals": refusals, "delay_ms": delay.Milliseconds(),
	})
	fired := w.clock.After(delay)
	w.retries.Add(1)
	go func() {
		defer w.retries.Done()
		select {
		case <-fired:
		case <-w.ctx.Done():
			return
		}
		w.mu.Lock()
		*pending = false
		if w.stale(gen) {
			w.log.Debug("daemon.sessionwatcher.reopen_fired", "a refused watch's backoff elapsed in a fleet that moved on; the fleet's own open stands", dlog.Context{
				"operation": operation, "handle": handle, "generation": gen, "current_generation": w.gen, "closed": w.closed,
			})
			w.mu.Unlock()
			return
		}
		reopen()
		w.mu.Unlock()
		w.flushTurnEnds()
	}()
}

// reopenRefusedShellLocked is the timer's decision for a shell watch: re-open
// it while it is still the registered, unreaped, stream-less watch with no
// open in flight.
func (w *watcher) reopenRefusedShellLocked(s *shellWatch) {
	key := s.work.GetValue()
	switch {
	case s.done || w.shells[key] != s:
		w.log.Debug("daemon.sessionwatcher.reopen_fired", "a refused shell watch's backoff elapsed after its work concluded; nothing is re-opened", dlog.Context{
			"operation": "watch_bash", "handle": key,
		})
	case s.stream != nil || w.inFlightLocked(s.opening):
		w.log.Debug("daemon.sessionwatcher.reopen_fired", "a refused shell watch's backoff elapsed after another occasion re-opened it", dlog.Context{
			"operation": "watch_bash", "handle": key,
		})
	default:
		w.log.Info("daemon.sessionwatcher.reopen_fired", "a refused shell watch's backoff elapsed; it is re-opened", dlog.Context{
			"operation": "watch_bash", "handle": key, "refusals": s.refusals,
		})
		w.openShellStreamLocked(s)
	}
}

// reopenRefusedAgentLocked is the timer's decision for an agent watch (the
// main agent's, or a detached subagent's), as for a shell.
func (w *watcher) reopenRefusedAgentLocked(a *agentWatch) {
	key := watchKey(a.id)
	switch {
	case a.done || !w.agentExpectedLocked(a):
		w.log.Debug("daemon.sessionwatcher.reopen_fired", "a refused agent watch's backoff elapsed after its work concluded; nothing is re-opened", dlog.Context{
			"operation": "watch_agent", "handle": key,
		})
	case a.stream != nil || w.inFlightLocked(a.opening):
		w.log.Debug("daemon.sessionwatcher.reopen_fired", "a refused agent watch's backoff elapsed after another occasion re-opened it", dlog.Context{
			"operation": "watch_agent", "handle": key,
		})
	default:
		w.log.Info("daemon.sessionwatcher.reopen_fired", "a refused agent watch's backoff elapsed; it is re-opened", dlog.Context{
			"operation": "watch_agent", "handle": key, "refusals": a.refusals,
		})
		w.openAgentStreamLocked(a)
	}
}
