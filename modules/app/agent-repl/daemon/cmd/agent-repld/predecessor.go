package main

import (
	"agent-repld/internal/daemonaddr"
	"agent-repld/internal/dlog"
)

// opPredecessorDied is the operation a daemon files another's disorderly
// death under: the one record of a daemon that died without its orderly
// exit, which every check's digest reads.
const opPredecessorDied = "daemon.boot.predecessor_died"

// predecessorDeath is how a daemon of this state root died, when it died
// without the orderly exit that removes its life lock.
type predecessorDeath struct {
	// Instance is its instance id.
	Instance string
	// PID is its process id, 0 when its life lock named none.
	PID int
	// Panic is the panic it died of, when the stdio log holds one of its.
	Panic    dlog.Panic
	Panicked bool
}

// readPredecessorDeath records DEATH, a daemon whose life lock a reaper took
// still standing with no holder (daemonaddr.LiveOthers, daemonaddr.AwaitGone):
// every orderly end removes its file before it releases its lock, so the
// kernel freed this one with a holder that never ran it. Each death is
// reaped, and so recorded, exactly once. Its panic is read by its pid from
// the stdio log under LOGSDIR. The death is recorded at ERROR through LOG,
// its panic and site with it, so the crash is in every digest the deploy
// mediator reads; a stdio log that cannot be read is recorded with the death,
// never in place of it. A life lock that named no pid (an older build's) is
// recorded with no pid and no panic.
func readPredecessorDeath(logsDir string, death daemonaddr.Death, log dlog.Logger) predecessorDeath {
	d := predecessorDeath{Instance: death.Instance}
	fields := dlog.Context{"instance": death.Instance}
	if death.PIDKnown {
		d.PID = death.PID
		fields["pid"] = death.PID
		p, found, err := dlog.LastPanic(logsDir, d.PID)
		switch {
		case err != nil:
			fields["stdio_log_error"] = err.Error()
		case found:
			d.Panic, d.Panicked = p, true
			fields["panic"], fields["site"] = p.Message, p.Site
		}
	}
	message := "the daemon before this one died without its orderly exit, leaving its life lock standing, and its stdio log holds no panic of its (killed, or its output lost)"
	if d.Panicked {
		message = "the daemon before this one died of a panic, leaving its life lock standing"
	}
	log.Error(opPredecessorDied, message, fields)
	return d
}
