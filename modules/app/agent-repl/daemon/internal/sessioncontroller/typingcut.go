// typingcut.go — RETIRING A PREVIEW NOTHING WILL EVER COMPLETE.
//
// # The defect
//
// A live typing preview is retired by the AUTHORITATIVE RECORD of the block it
// previews: the record lands, claims the preview standing for it, and replaces
// the fragment with the real prose (webapp streaming.ts, previewIndexFor). That
// is the ONLY thing that retires one.
//
// So when the record can never arrive — the query was torn down mid-block, the
// session died, the shim rolled — nothing retires it. The bubble spins
// "streaming input…" for the life of the page with no body, and no later event
// corrects it: the conversation moves on around a card that is permanently
// mid-sentence.
//
// # Why the daemon owns the cut
//
// A CUT IS A FACT, NOT A TIMEOUT. The daemon is the party that learns the
// stream ended without its record; the client cannot distinguish that from a
// slow block, and any deadline the client picked would be wrong for exactly the
// long tool calls users most want to watch. So the client never guesses, and
// there is deliberately no timer anywhere in this path.
//
// # Why an address set rather than a preview ledger
//
// What is tracked is the SURFACE a preview was opened on — the top-level feed
// (""), or the async bubble it was folded into — and the set only has to be a
// SUPERSET of the surfaces that could still be showing one. A cut for a preview
// that its own record already retired is a documented no-op on the client, so
// over-cutting costs nothing; under-cutting leaves the spinning bubble this
// exists to prevent. Tracking retirement precisely here would duplicate the
// client's own claim logic and could only create a way for the two to disagree.
package sessioncontroller

import (
	"sort"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"
)

// notePreviewOpened records the surface a live typing preview was opened on.
func (c *consumer) notePreviewOpened(bubbleID string) {
	c.mu.Lock()
	if c.previewSurfaces == nil {
		c.previewSurfaces = make(map[string]struct{}, 2)
	}
	c.previewSurfaces[bubbleID] = struct{}{}
	c.mu.Unlock()
}

// cutOpenPreviews retires every preview this consumer opened, naming why.
//
// The set is DRAINED as it is cut: those surfaces have been retired, and a
// second teardown has nothing left to say about them. That is also what makes
// this idempotent — the repeated call a teardown path can easily make emits
// nothing rather than a second round of cuts.
func (c *consumer) cutOpenPreviews(reason string) {
	c.mu.Lock()
	surfaces := c.previewSurfaces
	c.previewSurfaces = nil
	c.mu.Unlock()
	if len(surfaces) == 0 {
		return
	}
	// Sorted so one teardown's records read the same way twice, and so the
	// top-level feed ("") is always cut first.
	addresses := make([]string, 0, len(surfaces))
	for bubbleID := range surfaces {
		addresses = append(addresses, bubbleID)
	}
	sort.Strings(addresses)
	fence := c.fence()
	for _, bubbleID := range addresses {
		c.logf("session-controller: CUTTING an unretirable typing preview session=%s ws=%q bubble=%q reason=%s — the authoritative record that would retire this preview can no longer arrive, so the daemon retires it rather than leaving a bubble streaming with no body",
			c.sessionID, c.workspace, bubbleID, reason)
		c.push.PushTypingCut(&frontendv1.TypingCut{
			Workspace: c.workspace,
			BubbleId:  bubbleID,
			Fence:     fence,
		})
	}
}
