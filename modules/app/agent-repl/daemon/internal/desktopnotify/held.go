package desktopnotify

import (
	"cmp"
	"context"
	"errors"
	"fmt"
	"slices"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
)

// opClaim is the operation the held banners' records carry: each banner held
// or let go, and every ClaimLiveNotification answered.
const opClaim = "daemon.desktopnotify.claim"

// ErrEditorUnfocused is a claim made while Emacs reads as unfocused: the user
// is not looking at Emacs, so no banner may move its tab.
var ErrEditorUnfocused = errors.New("Emacs is not focused")

// Claim is what a claim came to: the newest live banner's target, or none.
type Claim struct {
	// Live is whether a held banner was still live.
	Live bool
	// Target is the live banner's workspace. Zero unless Live.
	Target Target
}

// heldBanner is one banner the notifier made and still holds.
type heldBanner struct {
	// seq orders the banners by when each was made.
	seq    uint64
	target Target
	// handed is a banner the platform holds: it is live while the platform
	// still lists it. One that is not handed is a read-back banner whose
	// program is still showing it, which is live by being held at all.
	handed bool
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
// held for the platform to vouch for, and any other has left the desktop. A
// banner a claim, a selection or a later banner of its workspace already let
// go is left alone.
func (n *Notifier) settle(ws ids.WorkspaceID, seq uint64, handed bool, log dlog.Logger) {
	n.heldMu.Lock()
	banner, ok := n.held.banners[ws]
	current := ok && banner.seq == seq
	switch {
	case current && handed:
		banner.handed = true
		n.held.banners[ws] = banner
	case current:
		delete(n.held.banners, ws)
	}
	n.heldMu.Unlock()
	log.Debug(opClaim, "settled the held banner", dlog.Context{"seq": seq, "handed": handed, "still_held": current})
}

// Claim answers the workspace of the newest held banner that is still live,
// and lets go of every banner it read, live or not: each banner steers one
// return to Emacs at most. ErrEditorUnfocused while Emacs reads as unfocused,
// and any failure to ask the platform which banners it still lists, let go of
// nothing.
func (n *Notifier) Claim(ctx context.Context) (Claim, error) {
	if !n.deps.Focus.Focused() {
		n.deps.Log.Info(opClaim, "a claim arrived while Emacs reads as unfocused; it was refused", nil)
		return Claim{}, ErrEditorUnfocused
	}
	banners := n.heldNewestFirst()
	delivered, err := n.delivered(ctx, banners)
	if err != nil {
		return Claim{}, err
	}
	claim := Claim{}
	for _, banner := range banners {
		if !banner.handed || delivered[banner.target.ID] {
			claim = Claim{Live: true, Target: banner.target}
			break
		}
	}
	n.letGo(banners)
	n.deps.Log.Info(opClaim, "answered a focus claim", dlog.Context{
		"held": len(banners), "live": claim.Live, "workspace": string(claim.Target.ID),
	})
	return claim, nil
}

// heldNewestFirst answers every held banner, the newest first.
func (n *Notifier) heldNewestFirst() []heldBanner {
	n.heldMu.Lock()
	banners := make([]heldBanner, 0, len(n.held.banners))
	for _, banner := range n.held.banners {
		banners = append(banners, banner)
	}
	n.heldMu.Unlock()
	slices.SortFunc(banners, func(a, b heldBanner) int { return cmp.Compare(b.seq, a.seq) })
	return banners
}

// delivered asks the platform which workspaces' banners it still lists, when
// any of banners was handed to it; a claim of read-back banners alone asks
// nothing.
func (n *Notifier) delivered(ctx context.Context, banners []heldBanner) (map[ids.WorkspaceID]bool, error) {
	if !slices.ContainsFunc(banners, func(b heldBanner) bool { return b.handed }) {
		return nil, nil
	}
	if n.deps.Backend == nil {
		// A handed banner was made by a backend; holding one without a backend
		// is a broken notifier, never a runtime condition.
		panic("desktopnotify: a handed banner is held with no banner backend")
	}
	delivered, err := n.deps.Backend.Delivered(ctx)
	if err != nil {
		return nil, fmt.Errorf("ask the banner program %s which banners it still holds: %w", n.deps.Backend.Program(), err)
	}
	return delivered, nil
}

// letGo lets go of banners, sparing any of their workspaces' banners made
// since they were read.
func (n *Notifier) letGo(banners []heldBanner) {
	n.heldMu.Lock()
	defer n.heldMu.Unlock()
	for _, banner := range banners {
		if n.held.banners[banner.target.ID].seq == banner.seq {
			delete(n.held.banners, banner.target.ID)
		}
	}
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
