package convert

// frames.go — WRAPPING A UNIT AS THE ENTRY THAT LANDS IT.
//
// One place decides, for every converted unit, whether it becomes a page line of
// some agent's book or residue that names no book. Whether a record is a
// keep-alive's is decided once per RECORD, ahead of all of this (Converter.Line,
// keepalive.go), because keep-alive is a property of the turn, not of the unit.

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

// activityEntry lands one unit of work.
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
func (c *Converter) landFrame(at Attribution, agent, upsertKey, discriminator string, frame *conversationv1.AgentFrame) *storev1.StoreEntry {
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
// AgentPrompt becomes an entry, respecting the same names-no-book invariant a
// frame does. A prompt naming no book is residue, loudly — the store reads the
// book from the prompt's recipient and never invents one.
func (c *Converter) landPrompt(at Attribution, agent, upsertKey, discriminator string, prompt *conversationv1.AgentPrompt) *storev1.StoreEntry {
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
// A message naming no book is residue, loudly, exactly as a prompt naming no
// recipient is.
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
