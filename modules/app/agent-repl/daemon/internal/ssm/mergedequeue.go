package ssm

import (
	"crypto/rand"
	"encoding/hex"
	"fmt"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"claude-repld/internal/workspace/merge"
)

// This file owns the MERGE DEQUEUE OFFER: the question an interrupt raises
// instead of silently taking a workspace's merge off the queue.
//
// IT LIVES HERE FOR THE SAME REASON merge_status does. The offer is published
// on WorkspaceState, and every field of WorkspaceState is stamped in this
// package's ONE construction funnel (workspaceMessageLocked); an offer held
// anywhere else would have to be stamped onto frames from outside that funnel,
// which is precisely the arrangement that lets a frame go out without it.
//
// IT IS IN MEMORY, DELIBERATELY, exactly like the retained MergeStatus beside
// it. An unanswered question does not survive the process that asked it: the
// merge a bounce interrupts is resumed from its durable queue entry, the user
// is looking at a fresh connection, and re-presenting a card whose interrupt
// happened before a daemon restart would ask about a state nobody is in any
// more. The user interrupts again if they still want to.

// mergeDequeueOfferIDBytes is the width of a minted offer id. The id only has
// to be unguessable-by-accident across the offers one daemon holds at once, so
// eight bytes is generous; it is not a security token.
const mergeDequeueOfferIDBytes = 8

// RaiseMergeDequeueOffer publishes the dequeue question for workspace and
// returns the offer as it now stands.
//
// standing is the workspace's place on its repository's queue, read by the
// merge coordinator. WHICH arm the offer carries is decided from it here rather
// than by the caller, so "position 1 means the running arm" is stated once.
//
// A SECOND RAISE UPDATES THE STANDING AND KEEPS THE OFFER ID. Interrupting
// twice while a card is up is an ordinary thing to do — the first press may not
// have looked like it did anything — and minting a rival question for it would
// leave two cards whose answers race. The refresh is also what keeps a card
// honest as the queue advances beneath it: an offer raised at position 3 says
// position 1 once its merge reaches the head.
//
// The push is UNCONDITIONAL (pushCurrentLocked), because the offer is its own
// pushed field and nothing else about the workspace need have moved for a
// question to have appeared.
func (m *Manager) RaiseMergeDequeueOffer(workspace string, standing merge.Standing) (*frontendv1.MergeDequeueOffer, error) {
	if workspace == "" {
		return nil, fmt.Errorf("ssm: RaiseMergeDequeueOffer got an empty workspace")
	}
	m.mu.Lock()
	defer m.mu.Unlock()

	existing := m.mergeDequeueOffers[workspace]
	offerID := ""
	if existing != nil {
		offerID = existing.GetOfferId()
	}
	if offerID == "" {
		minted, err := newMergeDequeueOfferID()
		if err != nil {
			// No id, no offer. Publishing one with an empty id would produce a
			// card whose answer can never be matched, which is a question the
			// user cannot get out of.
			m.warn("ssm: merge dequeue offer id MINT FAILED ws=%s: %v — no offer is raised; the interrupt's queue half did not happen", workspace, err)
			return nil, fmt.Errorf("ssm: mint merge dequeue offer id for %q: %w", workspace, err)
		}
		offerID = minted
	}

	offer := &frontendv1.MergeDequeueOffer{
		OfferId:    offerID,
		RunId:      standing.RunID,
		RaisedAtMs: m.nextAt(),
	}
	if standing.Head {
		// The running arm carries the run's own account of what it is doing.
		// A nil status is not an error and not a placeholder: the run publishes
		// its first status as it starts, so a head observed in the gap between
		// admission and that first publication genuinely has nothing to say
		// yet, and the card reads "merging" from the arm alone.
		offer.Standing = &frontendv1.MergeDequeueOffer_Running{
			Running: &frontendv1.MergeDequeueRunning{Status: m.pipelineStatus[workspace]},
		}
	} else {
		offer.Standing = &frontendv1.MergeDequeueOffer_Waiting{
			Waiting: &frontendv1.MergeDequeueWaiting{
				Ahead:    int32(standing.Ahead()),
				Position: int32(standing.Position),
				Depth:    int32(standing.Depth),
			},
		}
	}
	if m.mergeDequeueOffers == nil {
		m.mergeDequeueOffers = make(map[string]*frontendv1.MergeDequeueOffer)
	}
	m.mergeDequeueOffers[workspace] = offer
	m.logf("ssm: merge dequeue offer RAISED ws=%s offer=%s run=%s head=%t position=%d depth=%d refreshed=%t",
		workspace, offerID, standing.RunID, standing.Head, standing.Position, standing.Depth, existing != nil)
	if err := m.pushCurrentLocked(workspace); err != nil {
		return nil, err
	}
	return offer, nil
}

// ClearMergeDequeueOffer takes workspace's outstanding question down and
// reports whether there was one. why names what took it down, for the log.
//
// CLEARING IS HOW THE CARD COMES DOWN, and it is the only way. There is no
// dismissal channel on the frontend side: a card the user answered, a merge
// that reached its own terminal, and a merge that left the queue by some other
// route all converge here, so the question can never outlive the thing it was
// about.
func (m *Manager) ClearMergeDequeueOffer(workspace, why string) (bool, error) {
	if workspace == "" {
		return false, fmt.Errorf("ssm: ClearMergeDequeueOffer got an empty workspace")
	}
	m.mu.Lock()
	defer m.mu.Unlock()
	if !m.clearMergeDequeueOfferLocked(workspace, why) {
		return false, nil
	}
	if err := m.pushCurrentLocked(workspace); err != nil {
		return true, err
	}
	return true, nil
}

// clearMergeDequeueOfferLocked drops the retained offer WITHOUT pushing, and
// reports whether one was there. Caller holds mu.
//
// The push is the caller's because the two callers need different ones: an
// answered offer is its own edge and pushes unconditionally, while a merge
// transition that clears the offer is already about to push the very frame the
// clear belongs on.
func (m *Manager) clearMergeDequeueOfferLocked(workspace, why string) bool {
	offer, ok := m.mergeDequeueOffers[workspace]
	if !ok {
		return false
	}
	delete(m.mergeDequeueOffers, workspace)
	m.logf("ssm: merge dequeue offer CLEARED ws=%s offer=%s run=%s why=%s",
		workspace, offer.GetOfferId(), offer.GetRunId(), why)
	return true
}

// MergeDequeueOfferID reports the id of workspace's outstanding offer, if any.
// It is what an answer is checked against.
func (m *Manager) MergeDequeueOfferID(workspace string) (string, bool) {
	m.mu.Lock()
	defer m.mu.Unlock()
	offer, ok := m.mergeDequeueOffers[workspace]
	if !ok {
		return "", false
	}
	return offer.GetOfferId(), true
}

// stampMergeDequeueOfferLocked writes merge_dequeue_offer onto a WorkspaceState
// about to be pushed. Caller holds mu.
//
// It rides stampMergeFactsLocked with the rest of the merge fields, so a frame
// that omits an outstanding question is unrepresentable rather than merely
// unlikely — the same funnel discipline merge_lease_held and merge_status are
// held to.
func (m *Manager) stampMergeDequeueOfferLocked(workspace string, msg *frontendv1.WorkspaceState) {
	if offer, ok := m.mergeDequeueOffers[workspace]; ok {
		msg.MergeDequeueOffer = offer
	}
}

// newMergeDequeueOfferID mints an offer id.
func newMergeDequeueOfferID() (string, error) {
	var b [mergeDequeueOfferIDBytes]byte
	if _, err := rand.Read(b[:]); err != nil {
		return "", err
	}
	return hex.EncodeToString(b[:]), nil
}
