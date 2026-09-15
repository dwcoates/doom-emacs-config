package convert

// frames.go — WRAPPING A UNIT AS THE ENTRY THAT LANDS IT.
//
// One place decides, for every converted unit, whether it becomes a page line of
// some agent's book or a never-served keep-alive item. Keep-alive is a property
// of the TURN, not of the unit, so it cannot be decided at each item's own call
// site without every one of them remembering to ask.

import (
	"strings"

	conversationv1 "agentrepl/proto/conversation/v1"
	storev1 "agentrepl/proto/store/v1"
	"agentrepl/shim-claude-sidecar/internal/logging"
)

// updateFrame wraps an AgentUpdate as the agent's frame.
func updateFrame(agent string, update *conversationv1.AgentUpdate) *conversationv1.AgentFrame {
	return &conversationv1.AgentFrame{
		AgentId: agentID(agent),
		Result:  &conversationv1.AgentFrame_Update{Update: update},
	}
}

// activityFrame wraps one unit of work as the agent's frame.
func activityFrame(agent string, activity *conversationv1.AgentActivity) *conversationv1.AgentFrame {
	return updateFrame(agent, &conversationv1.AgentUpdate{
		Update: &conversationv1.AgentUpdate_Activity{Activity: activity},
	})
}

// activityEntry lands one unit of work, respecting the keep-alive bit.
//
// `discriminatorIndex` separates the several entries one record mints — the
// block index within the assistant message — so two units read at the same file
// offset mint different write ids.
func (c *Converter) activityEntry(at Attribution, env envelope, agent, unitID string, discriminatorIndex int, activity *conversationv1.AgentActivity) *storev1.StoreEntry {
	return c.landFrame(at, agent, ActivityKey(unitID), "block:"+itoa(discriminatorIndex), activityFrame(agent, activity))
}

// settledEntry lands the SETTLED state of a unit under the same identity its
// announcement used. A settle is an UPSERT of the whole unit, never a child.
func (c *Converter) settledEntry(at Attribution, agent, unitID string, activity *conversationv1.AgentActivity) *storev1.StoreEntry {
	return c.landFrame(at, agent, ActivityKey(unitID), "settle:"+unitID, activityFrame(agent, activity))
}

// landFrame is the ONE place a frame becomes an entry.
//
// A KEEP-ALIVE TURN'S ITEMS ARE NEVER PAGE LINES. They land on the keepalive
// arm, which is structurally unable to appear in any page — so no page filters
// them out at read time and no activity routes onward from one.
func (c *Converter) landFrame(at Attribution, agent, upsertKey, discriminator string, frame *conversationv1.AgentFrame) *storev1.StoreEntry {
	if c.keepalive {
		c.log.With(at.ctxFor("keepalive")).With(logging.Context{UpsertKey: upsertKey}).
			LogVerbose("converted while the keep-alive bit is set; landing as a never-served item")
		return Keepalive(at, discriminator, upsertKey, agent, &storev1.StoreAgentItem{
			Item: &storev1.StoreAgentItem_AgentFrame{AgentFrame: frame},
		})
	}
	if agent == "" {
		// A frame must name its agent: the store reads the book from the frame
		// and never invents one. A frame with no agent is residue, loudly.
		c.log.With(at.ctxError("attribution")).With(logging.Context{UpsertKey: upsertKey}).
			Log("frame names no agent; the record has no book and is stored as unknown residue")
		return UnknownEntry(at, "unattributed_frame", "agent_id", map[string]any{
			"upsert_key": upsertKey,
			"path":       at.Path,
			"offset":     float64(at.Offset),
		})
	}
	return PageLine(at, discriminator, upsertKey, agent, frame)
}

// landPrompt is landFrame's counterpart for a PROMPT: the one place an
// AgentPrompt becomes an entry, respecting the same keep-alive and
// names-no-book invariants a frame does.
//
// A KEEP-ALIVE TURN'S PROMPT IS NEVER A PAGE LINE, exactly as its frames are
// not: it lands on the keepalive arm, structurally unable to appear in any
// page. A prompt naming no book is residue, loudly — the store reads the book
// from the prompt's recipient and never invents one.
func (c *Converter) landPrompt(at Attribution, agent, upsertKey, discriminator string, prompt *conversationv1.AgentPrompt) *storev1.StoreEntry {
	if c.keepalive {
		c.log.With(at.ctxFor("keepalive")).With(logging.Context{UpsertKey: upsertKey}).
			LogVerbose("converted while the keep-alive bit is set; landing the prompt as a never-served item")
		return Keepalive(at, discriminator, upsertKey, agent, &storev1.StoreAgentItem{
			Item: &storev1.StoreAgentItem_AgentPrompt{AgentPrompt: prompt},
		})
	}
	if agent == "" {
		c.log.With(at.ctxError("attribution")).With(logging.Context{UpsertKey: upsertKey}).
			Log("prompt names no recipient; the record has no book and is stored as unknown residue")
		return UnknownEntry(at, "unattributed_prompt", "agent_id", map[string]any{
			"upsert_key": upsertKey,
			"path":       at.Path,
			"offset":     float64(at.Offset),
		})
	}
	return PromptLine(at, discriminator, upsertKey, agent, prompt)
}

// landPeerMessage is landPrompt's counterpart for a PEER MESSAGE: the one place
// a PeerMessage becomes an entry, respecting the same names-no-book invariant.
//
// A peer message is never a keep-alive turn's own record — it does not open a
// turn — so there is no keepalive arm here; a message naming no book is residue,
// loudly, exactly as a prompt naming no recipient is.
func (c *Converter) landPeerMessage(at Attribution, agent, upsertKey, discriminator string, peer *conversationv1.PeerMessage) *storev1.StoreEntry {
	if agent == "" {
		c.log.With(at.ctxError("attribution")).With(logging.Context{UpsertKey: upsertKey}).
			Log("peer message names no recipient; the record has no book and is stored as unknown residue")
		return UnknownEntry(at, "unattributed_peer", "agent_id", map[string]any{
			"upsert_key": upsertKey,
			"path":       at.Path,
			"offset":     float64(at.Offset),
		})
	}
	return PeerLine(at, discriminator, upsertKey, agent, peer)
}

// ---------------------------------------------------------------------------
// keep-alive
// ---------------------------------------------------------------------------

// noteKeepalive reads a user prompt's keep-alive marker and sets or clears the
// bit for every record that follows in this file.
//
// ONE REMEMBERED BOOL. The marker opens the state and the NEXT non-keepalive
// user prompt closes it, so nothing accumulates and a restart re-derives the bit
// from the same records in the same order.
func (c *Converter) noteKeepalive(message map[string]any, at Attribution) {
	text := firstText(message)
	marked := strings.HasPrefix(text, KeepaliveMarker)
	if marked == c.keepalive {
		return
	}
	c.keepalive = marked
	if marked {
		c.log.With(at.ctxFor("keepalive")).
			Log("keep-alive prompt opens a turn whose records are never served")
		return
	}
	c.log.With(at.ctxFor("keepalive")).
		Log("ordinary prompt closes the keep-alive turn; records are served again")
}

// firstText returns the text of a user message's first text block, or the whole
// content when the vendor wrote it as a bare string.
func firstText(message map[string]any) string {
	switch content := message["content"].(type) {
	case string:
		return content
	case []any:
		for _, raw := range content {
			block := obj(raw)
			if block != nil && str(block["type"]) == "text" {
				return str(block["text"])
			}
		}
	}
	return ""
}

// ---------------------------------------------------------------------------
// case-insensitive helpers, used by the notice and error classifiers
// ---------------------------------------------------------------------------

func equalFold(a, b string) bool { return strings.EqualFold(a, b) }

func containsFold(haystack, needle string) bool {
	return strings.Contains(strings.ToLower(haystack), strings.ToLower(needle))
}
