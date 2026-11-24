package rollout

import (
	"context"
	"sort"
	"sync"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
)

// THE SUCCESSOR ADOPTS WHAT THE USER IS LOOKING AT FIRST. Every workspace a
// handover moves is adopted on its own goroutine (adoptClaimed), and each
// adoption reads and writes the state database's one connection, so with
// nothing to order them the connection decided: on the 2026-11-23 15:35
// handover the workspace on screen, unify-worker-dashboard, was adopted 44th
// of 49 pages, 12.3s after its transfer, and was unusable for 22.6s.
//
// THE ORDER COMES FROM THE MANIFEST, never from a guess on this side. The
// outgoing daemon writes, for each session, whether it is the user's current
// workspace (ManifestSession.Viewed, read off the registry's is_current row,
// wsm.Workspace.Current, in the list the handover begins from) and whether it
// has work in flight (ManifestSession.WorkInFlight, read off Freeness at the
// same instant). The successor ranks by those two facts alone.

// adoptionRank is where one workspace's adoption stands in the order: lower
// ranks are adopted first.
type adoptionRank int

const (
	// rankViewed is the workspace the user is viewing.
	rankViewed adoptionRank = iota
	// rankWorkInFlight is a workspace with a turn or detached work running.
	rankWorkInFlight
	// rankRest is every other workspace.
	rankRest
)

// String names the rank as its records spell it.
func (r adoptionRank) String() string {
	switch r {
	case rankViewed:
		return "viewed"
	case rankWorkInFlight:
		return "work_in_flight"
	default:
		return "rest"
	}
}

// rankOf is a manifest session's rank.
func rankOf(session ManifestSession) adoptionRank {
	switch {
	case session.Viewed:
		return rankViewed
	case session.WorkInFlight:
		return rankWorkInFlight
	default:
		return rankRest
	}
}

// orderState is where one ordered adoption stands.
type orderState int

const (
	// orderPending is an adoption registered and not yet known to wait on
	// anything: it holds every lower rank back, so no lower-ranked adoption
	// can pass one whose goroutine has not yet run.
	orderPending orderState = iota
	// orderAwaitingRelease is an adoption waiting on the incumbent (its
	// serving release, or the test seam's participants): it holds nothing
	// back, so one busy workspace never holds the handover (2026-11-14 16:10).
	orderAwaitingRelease
	// orderReleased is an adoption whose release came, waiting to be
	// admitted: it holds every lower rank back.
	orderReleased
	// orderAdmitted is an adoption running toward ownership: it holds every
	// lower rank back.
	orderAdmitted
	// orderDone is an adoption that owns its workspace, or ended: it holds
	// nothing back.
	orderDone
)

// holdsBack reports whether an adoption in this state keeps lower ranks from
// being admitted.
func (s orderState) holdsBack() bool {
	return s == orderPending || s == orderReleased || s == orderAdmitted
}

// adoptionOrder admits a handover's adoptions by rank: an adoption whose
// release has come runs only once no adoption of a lower rank is pending,
// released or running. Adoptions of one rank run side by side.
//
// IT IS STRUCTURAL, NOT A HEAD START. Every adoption of a claim is
// registered before any of their goroutines starts (adoptClaimed), and a
// registered adoption holds lower ranks back until it is known to be waiting
// on the incumbent, so no scheduling of the goroutines lets a lower rank pass.
// A workspace no claim registered (a retry, the takeover's straggler pass) is
// not ordered.
type adoptionOrder struct {
	log dlog.Logger

	mu      sync.Mutex
	entries map[ids.WorkspaceID]*orderEntry
	// changed is closed and replaced at every state change, waking every
	// admission that waits.
	changed chan struct{}
	// owned counts the ordered adoptions that reached ownership, so each
	// names its place among them.
	owned int
}

// orderEntry is one ordered adoption.
type orderEntry struct {
	rank  adoptionRank
	state orderState
}

func newAdoptionOrder(log dlog.Logger) *adoptionOrder {
	return &adoptionOrder{log: log, entries: map[ids.WorkspaceID]*orderEntry{}, changed: make(chan struct{})}
}

// register orders RANKS' adoptions, each pending, and records the order.
func (o *adoptionOrder) register(ranks map[ids.WorkspaceID]adoptionRank) {
	if len(ranks) == 0 {
		return
	}
	counts := map[adoptionRank]int{}
	var viewed []string
	o.mu.Lock()
	for ws, rank := range ranks {
		o.entries[ws] = &orderEntry{rank: rank, state: orderPending}
		counts[rank]++
		if rank == rankViewed {
			viewed = append(viewed, string(ws))
		}
	}
	o.broadcastLocked()
	o.mu.Unlock()
	sort.Strings(viewed)
	o.log.Info(opAdopt, "the handover's adoptions are ordered: the workspace the user views first, then those with work in flight, then the rest", dlog.Context{
		"viewed": viewed, "work_in_flight": counts[rankWorkInFlight], "rest": counts[rankRest],
	})
}

// awaiting records that WS's adoption waits on the incumbent; an unordered
// workspace is left alone.
func (o *adoptionOrder) awaiting(ws ids.WorkspaceID) {
	o.set(ws, orderAwaitingRelease)
}

// admit records that WS's release came and returns once no lower rank holds
// it back, or with the cause when CTX ends first. An unordered workspace, and
// one whose ordered adoption already ended (a retry's), is admitted at once.
func (o *adoptionOrder) admit(ctx context.Context, ws ids.WorkspaceID) error {
	o.mu.Lock()
	e, ordered := o.entries[ws]
	if !ordered || e.state == orderDone {
		o.mu.Unlock()
		return nil
	}
	e.state = orderReleased
	o.broadcastLocked()
	for {
		blocker, blocked := o.blockerLocked(e.rank)
		if !blocked {
			e.state = orderAdmitted
			o.broadcastLocked()
			o.mu.Unlock()
			return nil
		}
		changed := o.changed
		o.mu.Unlock()
		o.log.Debug(opAdopt, "the adoption waits for a higher-ranked adoption", dlog.Context{
			"workspace": string(ws), "adoption_rank": e.rank.String(), "behind": string(blocker),
		})
		select {
		case <-ctx.Done():
			return context.Cause(ctx)
		case <-changed:
		}
		o.mu.Lock()
	}
}

// owns records that WS's adoption reached ownership, and answers its rank and
// its place among the ordered adoptions that did (1 is first); ok is false
// for an unordered workspace.
func (o *adoptionOrder) owns(ws ids.WorkspaceID) (rank adoptionRank, nth int, ok bool) {
	o.mu.Lock()
	defer o.mu.Unlock()
	e, ordered := o.entries[ws]
	if !ordered || e.state == orderDone {
		return 0, 0, false
	}
	e.state = orderDone
	o.owned++
	o.broadcastLocked()
	return e.rank, o.owned, true
}

// end records that WS's adoption ended, owned or not: it holds nothing back
// from here. Ending an ended or unordered adoption changes nothing.
func (o *adoptionOrder) end(ws ids.WorkspaceID) {
	o.set(ws, orderDone)
}

// set moves an ordered adoption to STATE, an unordered one not at all.
func (o *adoptionOrder) set(ws ids.WorkspaceID, state orderState) {
	o.mu.Lock()
	defer o.mu.Unlock()
	e, ordered := o.entries[ws]
	if !ordered || e.state == orderDone || e.state == state {
		return
	}
	e.state = state
	o.broadcastLocked()
}

// blockerLocked answers an adoption of a rank lower than RANK that holds it
// back, if any. Called with mu held.
func (o *adoptionOrder) blockerLocked(rank adoptionRank) (ids.WorkspaceID, bool) {
	for ws, e := range o.entries {
		if e.rank < rank && e.state.holdsBack() {
			return ws, true
		}
	}
	return "", false
}

// broadcastLocked wakes every waiting admission. Called with mu held.
func (o *adoptionOrder) broadcastLocked() {
	close(o.changed)
	o.changed = make(chan struct{})
}
