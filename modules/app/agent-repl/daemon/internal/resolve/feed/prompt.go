package feed

import (
	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/feedid"
	"claude-repld/internal/ids"
)

// ① THE PROMPT ROWS. Two kinds, one shape of thing: what a PERSON typed, and
// what an AGENT addressed to another agent. The daemon resolves the author
// label and the drawn blocks; the client draws what it is handed.

// drawAgentPrompt draws the row(s) one delivered prompt produces. A prompt
// delivered to the MAIN agent is a user prompt; a prompt delivered to a
// subagent is an agent prompt, drawn at BOTH ENDS — the outgoing send on the
// sender's feed and the delivered prompt on the recipient's — because it is
// one kind with two address lines.
func (r *resolver) drawAgentPrompt(s *wsState, agent *conversationv1.AgentId, prompt *conversationv1.AgentPrompt) {
	log := r.logger(s.id)
	recipient := prompt.GetAgent()
	if recipient.GetValue() == "" {
		recipient = agent
	}
	turn := prompt.GetId()
	blocks := r.drawUserBlocks(s, prompt.GetSaid().GetContent())

	subFeedKey, isSub := s.agentFeeds[recipient.GetValue()]
	if !isSub || s.address != nil {
		// The main agent's prompt, or any prompt while a lease holder's output
		// address is in force: one user-prompt row, placed by the address.
		at := r.place(s, recipient)
		row := &frontendv1.FeedRow{
			Id:   r.rowID(s.id, at.feed, feedid.RowKey{Kind: feedid.KindPrompt, ID: turn.GetValue()}),
			Turn: turn,
			Row: &frontendv1.FeedRow_UserPrompt{UserPrompt: &frontendv1.FeedUserPrompt{
				Author: &frontendv1.FeedUserPromptAuthor{Label: AuthorLabel(prompt.GetOrigin())},
				Result: &frontendv1.FeedUserPrompt_Success{Success: &frontendv1.FeedUserPromptSuccess{
					Body: &frontendv1.FeedUserPromptBody{Blocks: blocks},
				}},
			}},
		}
		// THE TURN THE SESSION IS RUNNING, learned from the prompt that opened
		// it: every later row this turn produces is stamped with it, and its
		// terminal row is what clears it.
		if turn.GetValue() != "" {
			running := ids.TurnID(turn.GetValue())
			s.turnInFlight = &running
			s.turnStamp = &running
		}
		log.Debug("daemon.feed.user_prompt",
			"a delivered prompt was drawn as a user-prompt row",
			dlog.Context{"turn": turn.GetValue(), "origin": prompt.GetOrigin().String(), "blocks": len(blocks)})
		r.upsert(s, at, row, true)
		return
	}

	// An agent-addressed prompt: the same content at both ends.
	recipientFeed := feedid.Feed{Agent: recipient}
	senderFeedKey := "root"
	if head, ok := s.subFeeds[subFeedKey]; ok {
		senderFeedKey = head.parentFeed
	}
	senderAddr, senderKnown := s.feedAddrs[senderFeedKey]
	if !senderKnown {
		senderAddr = feedid.Feed{Root: true}
	}

	recipientLabel := feedLabel(s, subFeedKey)
	senderLabel := feedLabel(s, senderFeedKey)

	outgoing := &frontendv1.FeedRow{
		Id:   r.rowID(s.id, senderAddr, feedid.RowKey{Kind: feedid.KindPrompt, ID: turn.GetValue(), Sub: "out"}),
		Turn: turn,
		Row: &frontendv1.FeedRow_AgentPrompt{AgentPrompt: &frontendv1.FeedAgentPrompt{
			Address: &frontendv1.FeedAgentPromptAddress{Text: "→ " + recipientLabel},
			Body:    &frontendv1.FeedAgentPromptBody{Blocks: agentBlocks(blocks)},
		}},
	}
	delivered := &frontendv1.FeedRow{
		Id:   r.rowID(s.id, recipientFeed, feedid.RowKey{Kind: feedid.KindPrompt, ID: turn.GetValue(), Sub: "in"}),
		Turn: turn,
		Row: &frontendv1.FeedRow_AgentPrompt{AgentPrompt: &frontendv1.FeedAgentPrompt{
			Address: &frontendv1.FeedAgentPromptAddress{Text: "from " + senderLabel},
			Body:    &frontendv1.FeedAgentPromptBody{Blocks: agentBlocks(blocks)},
		}},
	}

	log.Debug("daemon.feed.agent_prompt",
		"an agent-addressed prompt was drawn at both ends",
		dlog.Context{"turn": turn.GetValue(), "sender_feed": senderFeedKey, "recipient_feed": subFeedKey})
	r.upsert(s, placement{feed: senderAddr}, outgoing, true)
	r.upsert(s, placement{feed: recipientFeed}, delivered, true)
}

// feedLabel names a feed for an address line: a sub-feed by its bubble's
// label, the root by the main thread.
func feedLabel(s *wsState, key string) string {
	if head, ok := s.subFeeds[key]; ok && head.label != "" {
		return head.label
	}
	return "the main agent"
}

// AuthorLabel names WHO a prompt row is drawn as being from. The origin is the
// closed attribution vocabulary the record carries precisely so a replayed
// merge-born row routes and a restart re-drive labels, rather than both being
// drawn as fresh user turns.
//
// It is EXPORTED because the prompt queue's mirror of an accepted prompt draws
// the same row under the same FeedId; two copies of this table would let the
// mirror and the resolver's own re-draw disagree about who spoke.
func AuthorLabel(origin conversationv1.PromptOrigin) string {
	switch origin {
	case conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_CONFLICT_REPAIR,
		conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_TEST_REPAIR,
		conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_BEFORE_ACTION,
		conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_AFTER_ACTION,
		conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_DISPLACED_TURN_RESUME:
		return "Merge"
	case conversationv1.PromptOrigin_PROMPT_ORIGIN_RESUME_AFTER_RESTART:
		return "Resumed after restart"
	case conversationv1.PromptOrigin_PROMPT_ORIGIN_WORKSPACE_CREATED:
		return "Workspace brief"
	}
	return "You"
}

// drawUserBlocks resolves what a person composed into drawable blocks: the
// text with the host's sentinel spans STRIPPED (the full text stays on the
// record), each image reference resolved to a src the webview can load, and
// anything unmodeled named rather than dropped.
func (r *resolver) drawUserBlocks(s *wsState, content *conversationv1.UserContent) []*frontendv1.FeedUserPromptBlock {
	log := r.logger(s.id)
	blocks := make([]*frontendv1.FeedUserPromptBlock, 0, len(content.GetBlocks()))
	for _, block := range content.GetBlocks() {
		switch b := block.GetBlock().(type) {
		case *conversationv1.UserContentBlock_Text:
			drawn := r.deps.StripSentinels(b.Text.GetText())
			blocks = append(blocks, &frontendv1.FeedUserPromptBlock{
				Block: &frontendv1.FeedUserPromptBlock_Text{Text: &frontendv1.FeedTextBlock{Text: drawn}},
			})
		case *conversationv1.UserContentBlock_Image:
			src, alt, err := r.resolveImage(b.Image)
			if err != nil {
				log.Warn("daemon.feed.image_unresolved",
					"an image reference could not be resolved to a drawable src; the block is drawn as unsupported",
					dlog.Context{"media_type": b.Image.GetMediaType(), "cause": err.Error()})
				blocks = append(blocks, &frontendv1.FeedUserPromptBlock{
					Block: &frontendv1.FeedUserPromptBlock_Unsupported{
						Unsupported: &frontendv1.FeedUnsupportedBlock{Kind: "image"},
					},
				})
				continue
			}
			blocks = append(blocks, &frontendv1.FeedUserPromptBlock{
				Block: &frontendv1.FeedUserPromptBlock_Image{Image: &frontendv1.FeedImageBlock{Src: src, Alt: alt}},
			})
		case *conversationv1.UserContentBlock_Unsupported:
			blocks = append(blocks, &frontendv1.FeedUserPromptBlock{
				Block: &frontendv1.FeedUserPromptBlock_Unsupported{
					Unsupported: &frontendv1.FeedUnsupportedBlock{Kind: b.Unsupported.GetKind()},
				},
			})
		}
	}
	return blocks
}

// resolveImage runs the injected resolver, refusing rather than guessing when
// none was injected: a src the daemon invented would be a broken image on
// every client.
func (r *resolver) resolveImage(block *conversationv1.ImageBlock) (string, string, error) {
	if r.deps.ResolveImage == nil {
		return "", "", errNoImageResolver
	}
	return r.deps.ResolveImage(block)
}

// errNoImageResolver is the refusal for an image with no resolver wired.
var errNoImageResolver = errImageResolver{}

// errImageResolver is the typed absence of an image resolver.
type errImageResolver struct{}

// Error names the missing dependency.
func (errImageResolver) Error() string { return "feed: no image resolver is wired" }

// agentBlocks re-wraps drawn blocks in the agent-prompt vocabulary. The two
// block unions are deliberately separate types carrying the same arms, so the
// re-wrap is explicit rather than a shared union nothing constrains.
func agentBlocks(blocks []*frontendv1.FeedUserPromptBlock) []*frontendv1.FeedAgentPromptBlock {
	out := make([]*frontendv1.FeedAgentPromptBlock, 0, len(blocks))
	for _, block := range blocks {
		switch b := block.GetBlock().(type) {
		case *frontendv1.FeedUserPromptBlock_Text:
			out = append(out, &frontendv1.FeedAgentPromptBlock{
				Block: &frontendv1.FeedAgentPromptBlock_Text{Text: b.Text},
			})
		case *frontendv1.FeedUserPromptBlock_Image:
			out = append(out, &frontendv1.FeedAgentPromptBlock{
				Block: &frontendv1.FeedAgentPromptBlock_Image{Image: b.Image},
			})
		case *frontendv1.FeedUserPromptBlock_Unsupported:
			out = append(out, &frontendv1.FeedAgentPromptBlock{
				Block: &frontendv1.FeedAgentPromptBlock_Unsupported{Unsupported: b.Unsupported},
			})
		}
	}
	return out
}
