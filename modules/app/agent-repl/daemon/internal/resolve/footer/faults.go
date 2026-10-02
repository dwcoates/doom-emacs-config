package footer

import (
	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
)

// THE FOOTER'S FAULT CELL. The owner's ruling of 2026-09-13 is that every
// daemon fault kind reaches the footer. The health package decides WHICH cell
// each kind claims and hands the verdict here; this file is the accumulation
// and the precedence, and nothing in it derives a mapping of its own.

// OpenFault takes one fault the daemon opened. A daemon-scoped fault (an
// empty workspace) reaches every workspace's strip.
//
// THE TWO FAULT FAMILIES LAND IN DIFFERENT TIERS. An ESCALATING fault (one
// that claims `disconnected` or `blocked`) decides the status and stands as
// that arm's salient `fault` line until it is retracted. A NON-ESCALATING
// fault (no status claimed) blocks neither the turn nor the user, so it is
// announced ONCE, as the transient `fault` line, and never stands: its
// standing record is the fault store's and the topbar's, not a line pinned
// over the session's live feedback (owner ruling, 2026-09-28).
func (r *resolver) OpenFault(ws ids.WorkspaceID, fault Fault) {
	ctx := dlog.Context{
		"fault": fault.ID, "kind": fault.Kind,
		"status": fault.Status, "substatus": fault.SubStatus,
		"escalating": fault.Status != "",
	}
	if fault.Status == "" {
		r.announceFault(ws, fault, ctx)
		return
	}
	if ws == "" {
		r.mutateAll("daemon.footer.open_fault",
			"the footer took a daemon-scoped fault onto every strip", ctx,
			func(*wsState) {}, func() { r.daemonFaults = appendFault(r.daemonFaults, fault) })
		return
	}
	r.mutate(ws, "daemon.footer.open_fault", "the footer took a standing fault", ctx,
		func(s *wsState) { s.faults = appendFault(s.faults, fault) })
}

// announceFault raises a non-escalating fault's transient line: on its own
// workspace's strip, or on every strip for a daemon-scoped one.
func (r *resolver) announceFault(ws ids.WorkspaceID, fault Fault, ctx dlog.Context) {
	if ws == "" {
		r.mutateAll("daemon.footer.open_fault",
			"the footer announced a non-escalating daemon-scoped fault on every strip", ctx,
			func(s *wsState) { r.raiseFault(s.id, s, fault) }, func() {})
		return
	}
	r.mutate(ws, "daemon.footer.open_fault", "the footer announced a non-escalating fault", ctx,
		func(s *wsState) { r.raiseFault(ws, s, fault) })
}

// CloseFault retracts a standing fault by its record id.
func (r *resolver) CloseFault(ws ids.WorkspaceID, id string) {
	ctx := dlog.Context{"fault": id}
	if ws == "" {
		r.mutateAll("daemon.footer.close_fault",
			"the footer retracted a daemon-scoped fault from every strip", ctx,
			func(*wsState) {}, func() { r.daemonFaults = removeFault(r.daemonFaults, id) })
		return
	}
	r.mutate(ws, "daemon.footer.close_fault", "the footer retracted a standing fault", ctx,
		func(s *wsState) { s.faults = removeFault(s.faults, id) })
}

// appendFault adds a fault, replacing an entry with the same id so a re-opened
// record refreshes its line rather than standing twice.
func appendFault(faults []Fault, fault Fault) []Fault {
	for i := range faults {
		if faults[i].ID == fault.ID {
			faults[i] = fault
			return faults
		}
	}
	return append(faults, fault)
}

// removeFault drops the fault with this id, keeping the order of the rest.
func removeFault(faults []Fault, id string) []Fault {
	for i := range faults {
		if faults[i].ID == id {
			return append(faults[:i:i], faults[i+1:]...)
		}
	}
	return faults
}

// faultRank orders the standing faults by how strong a claim they make on the
// strip. A workspace with several open faults draws ONE line, and it is the
// one that says the most about why the session cannot be used.
//
// The order is the partition's own: a session that never came up outranks one
// that died, which outranks a severed link, which outranks a daemon that
// cannot serve it. Only escalating faults stand, so nothing ranks below.
func faultRank(f Fault) int {
	switch {
	// A VENDOR THAT DID NOT START ranks with a session that never came up
	// (footer.proto: the vendor_start line "ranks with start_failed").
	case f.Status == "disconnected" && (f.SubStatus == "start_failed" || vendorFault(&f)):
		return 4
	case f.Status == "disconnected" && f.SubStatus == "dead":
		return 3
	case f.Status == "disconnected":
		return 2
	default:
		return 1
	}
}

// standingFault is the ONE fault the strip draws for this workspace: the
// strongest of its own and the daemon-scoped ones, and among equals the one
// that was opened last, because the newest evidence is the live one. Nil when
// nothing stands.
func (r *resolver) standingFault(s *wsState) *Fault {
	var best *Fault
	consider := func(faults []Fault) {
		for i := range faults {
			f := &faults[i]
			if best == nil || faultRank(*f) > faultRank(*best) ||
				(faultRank(*f) == faultRank(*best) && f.At.After(best.At)) {
				best = f
			}
		}
	}
	consider(s.faults)
	consider(r.daemonFaults)
	return best
}

// vendorFault reports whether a standing fault is one of the three vendor-start
// faults, by the bucket the health partition gave it.
func vendorFault(f *Fault) bool {
	if f == nil || f.Status != "disconnected" {
		return false
	}
	switch f.SubStatus {
	case "vendor_retry", "vendor_rejection", "vendor_failed":
		return true
	}
	return false
}
