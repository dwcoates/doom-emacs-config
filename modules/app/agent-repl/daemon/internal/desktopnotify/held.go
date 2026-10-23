package desktopnotify

import (
	"maps"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
)

// opClaim is the operation the held banners' records carry: each banner held
// or let go, and every claim a focus gain made.
const opClaim = "daemon.desktopnotify.claim"

// Claim is what a claim came to: the newest held banner's target, or none.
type Claim struct {
	// Held is whether a banner was held.
	Held bool
	// Target is the newest held banner's workspace. Zero unless Held.
	Target Target
}

// heldBanner is one banner the notifier made and still holds.
type heldBanner struct {
	// seq orders the banners by when each was made.
	seq    uint64
	target Target
}

// held is the banners the notifier made since the last claim or selection,
// the newest of each workspace's: a workspace's banner replaces its previous
// one on the desktop (the macOS -group), so it does here too.
type held struct {
	seq     uint64
	banners map[ids.WorkspaceID]heldBanner
}

// hold holds a banner just made for target, answering the seq it is held by.
func (n *Notifier) hold(target Target, log dlog.Logger) uint64 {
	n.heldMu.Lock()
	n.held.seq++
	seq := n.held.seq
	n.held.banners[target.ID] = heldBanner{seq: seq, target: target}
	n.heldMu.Unlock()
	log.Debug(opClaim, "holding the made banner for a focus claim", dlog.Context{"seq": seq})
	return seq
}

// settle records what a held banner's program came to: a handed banner stays
// held until a claim, a selection or a later banner of its workspace lets it
// go, and any other (a read-back banner whose program stopped showing it) has
// left the desktop. A banner already let go is left alone.
func (n *Notifier) settle(ws ids.WorkspaceID, seq uint64, handed bool, log dlog.Logger) {
	n.heldMu.Lock()
	banner, ok := n.held.banners[ws]
	current := ok && banner.seq == seq
	if current && !handed {
		delete(n.held.banners, ws)
	}
	n.heldMu.Unlock()
	log.Debug(opClaim, "settled the held banner", dlog.Context{"seq": seq, "handed": handed, "still_held": current && handed})
}

// Claim answers the workspace of the newest held banner and lets go of every
// banner held: each banner steers one return to Emacs at most. It is made by
// a FOCUS GAIN, under the very report that is the gain, so it never asks
// whether Emacs is focused; and it never asks the platform which banners it
// still lists, a program run on the path from focus to switch (see
// agentrepl.v1.ReportEditorFocusRequest). The answer is memory alone.
func (n *Notifier) Claim() Claim {
	n.heldMu.Lock()
	held := len(n.held.banners)
	claim := Claim{}
	var newest uint64
	for banner := range maps.Values(n.held.banners) {
		if !claim.Held || banner.seq > newest {
			claim = Claim{Held: true, Target: banner.target}
			newest = banner.seq
		}
	}
	clear(n.held.banners)
	n.heldMu.Unlock()
	n.deps.Log.Info(opClaim, "a focus gain claimed the newest held banner", dlog.Context{
		"held": held, "claimed": claim.Held, "workspace": string(claim.Target.ID),
	})
	return claim
}

// Forget lets go of every held banner: the user chose a workspace (a
// selection, a banner's click), and no banner made before that choice may
// later move them elsewhere. reason is the choice, for the record.
func (n *Notifier) Forget(reason string) {
	n.heldMu.Lock()
	count := len(n.held.banners)
	clear(n.held.banners)
	n.heldMu.Unlock()
	n.deps.Log.Debug(opClaim, "let go of every held banner", dlog.Context{"reason": reason, "held": count})
}
