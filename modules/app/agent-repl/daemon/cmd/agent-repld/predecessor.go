package main

import (
	"os"

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
	// Address is the address its stale advertisement named.
	Address string
	// PID is its process id, 0 when its advertisement named none.
	PID int
	// Panic is the panic it died of, when the stdio log holds one of its.
	Panic    dlog.Panic
	Panicked bool
}

// readPredecessorDeath reads the advertisement at ADDRPATH, which a boot
// found STALE (daemonaddr.StaleAdvertisement): the daemon that wrote it died
// without its orderly exit, since every orderly exit withdraws it. Its panic
// is read from the stdio log under LOGSDIR. The death is recorded at ERROR
// through LOG, its panic and site with it, so the crash is in every digest
// the deploy mediator reads; a stdio log that cannot be read is recorded
// with the death, never in place of it.
func readPredecessorDeath(addrPath, logsDir, stale string, log dlog.Logger) predecessorDeath {
	d := predecessorDeath{Address: stale}
	fields := dlog.Context{"stale_address": stale, "addr_path": addrPath}
	raw, err := os.ReadFile(addrPath)
	if err != nil {
		fields["advertisement_error"] = err.Error()
	} else if ad := daemonaddr.ParseAdvertisement(string(raw)); ad.PIDKnown {
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
