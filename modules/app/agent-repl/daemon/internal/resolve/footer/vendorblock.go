package footer

import (
	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
)

// A MID-SESSION VENDOR BLOCK is the vendor or the account refusing a session
// that is UP (owner ruling, 2026-10-06): a standing account or vendor block —
// `vendor_fault · auth`, `usage_limit`, `billing`, `vendor_error` — or a
// standing API retry holding the running turn — `vendor_fault · api_retrying`.
// A vendor that will not START is not one: no session is up, and the prompt
// queue reads that from the fleet (promptqueue.Deps.SessionStarted).
//
// While one stands, the prompt queue holds every submitted prompt on the
// after-reconnect hold, unclassified, and THE EDGE ON WHICH IT STOPS STANDING
// is what releases them (WithVendorServes). The edge is read off this one
// accumulation, in the one mutation that clears it, so every fact that lifts
// the strip's vendor fault lifts the hold with it: a session (re)start, a
// rate-limit verdict that is not rejected, a new turn opening (the daemon's
// own or one the vendor started), the retried call answered, and the retried
// turn ending.

// Vendor-block names, as VendorBlock answers them and the records carry them.
const (
	VendorBlockAuth        = "auth"
	VendorBlockUsageLimit  = "usage_limit"
	VendorBlockVendorError = "vendor_error"
	VendorBlockBilling     = "billing"
	VendorBlockQueryDied   = "query_died"
	VendorBlockAPIRetrying = "api_retrying"
)

// WithVendorServes installs the listener told when a workspace's mid-session
// vendor block stops standing, with the block it was. It is called once per
// such edge, AFTER the resolver's lock is released, on the goroutine that made
// the change — which can be the prompt queue's own, under its delivery lock —
// so the listener must not block on anything that change's caller holds.
func WithVendorServes(fn func(ws ids.WorkspaceID, was string)) Option {
	return func(o *options) { o.vendorServes = fn }
}

// VendorBlock reports the workspace's standing mid-session vendor block by
// name, false when none stands. A workspace the footer has never seen has
// none.
func (r *resolver) VendorBlock(ws ids.WorkspaceID) (string, bool) {
	r.mu.Lock()
	defer r.mu.Unlock()
	s, ok := r.states[ws]
	if !ok {
		return "", false
	}
	block := s.vendorBlock()
	return block, block != ""
}

// vendorBlock names the standing mid-session vendor block, empty when none
// stands. A standing block outranks a retry, as the strip draws them.
func (s *wsState) vendorBlock() string {
	if s.blocked != nil {
		return s.blocked.kind.name()
	}
	if s.retryBlocks() {
		return VendorBlockAPIRetrying
	}
	return ""
}

// name renders a blocked kind as VendorBlock answers it.
func (k blockedKind) name() string {
	switch k {
	case blockedAuth:
		return VendorBlockAuth
	case blockedUsageLimit:
		return VendorBlockUsageLimit
	case blockedVendorError:
		return VendorBlockVendorError
	case blockedBilling:
		return VendorBlockBilling
	case blockedQueryDied:
		return VendorBlockQueryDied
	default:
		return "blocked_kind_unknown"
	}
}

// vendorServed is one workspace whose mid-session vendor block stopped
// standing in a mutation.
type vendorServed struct {
	ws  ids.WorkspaceID
	was string
	log dlog.Logger
}

// servedEdge reports the edge between a block observed before a mutation and
// the state after it.
func servedEdge(ws ids.WorkspaceID, before string, s *wsState, log dlog.Logger) (vendorServed, bool) {
	if before == "" || s.vendorBlock() != "" {
		return vendorServed{}, false
	}
	return vendorServed{ws: ws, was: before, log: log}, true
}

// tellVendorServes records each edge at INFO and tells the listener. The
// caller has released the resolver's lock.
func (r *resolver) tellVendorServes(operation string, edges []vendorServed) {
	for _, e := range edges {
		e.log.Info("daemon.footer.vendor_serves", "the mid-session vendor block stopped standing; the vendor serves again",
			dlog.Context{"vendor_block": e.was, "cause": operation, "listener": r.opts.vendorServes != nil})
		if r.opts.vendorServes != nil {
			r.opts.vendorServes(e.ws, e.was)
		}
	}
}
