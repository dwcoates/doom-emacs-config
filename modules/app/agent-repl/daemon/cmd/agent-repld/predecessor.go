package main

import (
	"agent-repld/internal/daemonaddr"
	"agent-repld/internal/dlog"
)

// opPredecessorDied is the operation a boot files its predecessor's
// disorderly death under: the one record of a daemon that died without its
// orderly exit, which every check's digest reads.
const opPredecessorDied = "daemon.boot.predecessor_died"

// predecessorDeath is how the daemon before this one died, when it died
// without the orderly exit that withdraws its advertisement.
type predecessorDeath struct {
	// Address is the address its standing advertisement named.
	Address string
	// PID is its process id, 0 when its advertisement named none.
	PID int
	// Panic is the panic it died of, when the stdio log holds one of its.
	Panic    dlog.Panic
	Panicked bool
}

// readPredecessorDeath records the death of the daemon whose advertisement
// AD, read from ADDRPATH, a boot holding the boot claim found standing
// (daemonaddr.PredecessorAdvertisement): with no other daemon alive on the
// state root and every orderly exit withdrawing its advertisement, the one
// still there is a predecessor's that died without its orderly exit. No dial
// judges it: the address is the shared one the socket holder keeps open, which
// answers for a dead daemon too. Its panic is read from the stdio log under
// LOGSDIR. The death is recorded at ERROR through LOG, its panic and site with
// it, so the crash is in every digest the deploy mediator reads; a stdio log
// that cannot be read is recorded with the death, never in place of it.
func readPredecessorDeath(addrPath, logsDir string, ad daemonaddr.Advertisement, log dlog.Logger) predecessorDeath {
	d := predecessorDeath{Address: ad.Address}
	fields := dlog.Context{"stale_address": ad.Address, "addr_path": addrPath}
	if ad.Private != "" {
		fields["private_address"] = ad.Private
	}
	if ad.PIDKnown {
		d.PID = ad.PID
		fields["pid"] = ad.PID
	}
	if d.PID != 0 {
		p, found, err := dlog.LastPanic(logsDir, d.PID)
		switch {
		case err != nil:
			fields["stdio_log_error"] = err.Error()
		case found:
			d.Panic, d.Panicked = p, true
			fields["panic"], fields["site"] = p.Message, p.Site
		}
	}
	message := "the daemon before this one died without its orderly exit, leaving its advertisement stale, and its stdio log holds no panic of its (killed, or its output lost)"
	if d.Panicked {
		message = "the daemon before this one died of a panic, leaving its advertisement stale"
	}
	log.Error(opPredecessorDied, message, fields)
	return d
}
