package vendortraffic

import "fmt"

// Counts is a pair of byte totals: what was received and what was sent.
type Counts struct {
	Received uint64
	Sent     uint64
}

// Plus answers the sum of the two totals.
func (c Counts) Plus(o Counts) Counts {
	return Counts{Received: c.Received + o.Received, Sent: c.Sent + o.Sent}
}

// IsZero reports whether neither total holds a byte.
func (c Counts) IsZero() bool { return c.Received == 0 && c.Sent == 0 }

// ledger accounts ONE subscription's sources: the last cumulative counts each
// live socket reported, so every update adds only what is new.
//
// THE UNIT IS THE SOCKET, NOT THE PROCESS. A socket's counters only grow, and
// a closing socket reports its final counts before it leaves, so the sum of
// per-socket deltas is every byte the process moved through sockets this
// subscription saw — across any number of connections opened and closed, and
// with nothing counted twice. A replaced process (a shim's vendor child
// restarting) is a NEW subscription with a ledger of its own, so its counters
// starting again from zero is never a reset to reconcile: the old process's
// sockets were counted to their closing update, and the new one's are counted
// from theirs.
type ledger struct {
	last map[uint64]Counts
}

// newLedger builds an empty ledger.
func newLedger() *ledger { return &ledger{last: map[uint64]Counts{}} }

// observe takes one source's cumulative counts and answers the bytes to count.
//
// A source seen for the first time counts IN FULL — every byte its socket
// moved is new to this subscription — unless baseline says its history
// belongs to somebody else (a socket that was already open when a restarted
// daemon subscribed to a process the daemon before it was measuring): then its
// counts are recorded and nothing is counted until it moves again.
//
// A closing update is the source's last, and its entry is dropped.
//
// A COUNTER THAT SHRINKS IS THE KERNEL BREAKING ITS OWN CONTRACT, never a
// condition to absorb: the error says so, nothing is counted, and the source
// is re-anchored at what it now reports so one bad reading does not poison
// every later one.
func (l *ledger) observe(ref uint64, current Counts, closing, baseline bool) (Counts, error) {
	previous, seen := l.last[ref]
	if closing {
		delete(l.last, ref)
	} else {
		l.last[ref] = current
	}
	if !seen {
		if baseline {
			return Counts{}, nil
		}
		return current, nil
	}
	if current.Received < previous.Received || current.Sent < previous.Sent {
		return Counts{}, fmt.Errorf(
			"vendortraffic: source %d reported %d received / %d sent after %d / %d; a socket's counters never shrink",
			ref, current.Received, current.Sent, previous.Received, previous.Sent)
	}
	return Counts{Received: current.Received - previous.Received, Sent: current.Sent - previous.Sent}, nil
}

// remove drops a source the kernel removed. A removal for a source this ledger
// never saw is ordinary: removals are not filtered by pid, so every socket on
// the machine announces its departure to every subscription.
func (l *ledger) remove(ref uint64) {
	delete(l.last, ref)
}

// sources counts the live sources the ledger holds.
func (l *ledger) sources() int { return len(l.last) }
