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
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row condition", dlog.Context{"function": "feed", "condition": "recipient.GetValue() == \"\""})
		recipient = agent
	}
	turn := prompt.GetId()
	blocks := r.drawUserBlocks(s, prompt.GetSaid().GetContent())

	subFeedKey, isSub := s.agentFeeds[recipient.GetValue()]
	if !isSub || s.address != nil {
		// The main agent's prompt, or any prompt while a lease holder's output
		// address is in force: one user-prompt row, placed by the address.
		// AN UNPLACEABLE PROMPT STILL OPENS ITS TURN: only its row is not drawn
		// (place has reported why).
		at, placed := r.place(s, recipient)
		// THE TURN THE SESSION IS RUNNING, learned from the prompt that opened
		// it: every later row this turn produces is stamped with it, and its
		// terminal row is what clears it. This is set even for a directive turn,
		// whose bubble is suppressed, because the divider and terminal that
		// follow attribute to this turn.
		if turn.GetValue() != "" {
			r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row condition", dlog.Context{"function": "feed", "condition": "turn.GetValue() != \"\""})
			running := ids.TurnID(turn.GetValue())
			// THE REPLAY LEAVES THE TURN IT STOOD IN, and a turn its page never
			// ended is ended from its durable close BEFORE this prompt's row, so
			// the ending lands under the turn it ends.
			if s.plane.replayed() {
				r.endReplayedTurn(s, running)
			}
			s.turnInFlight = &running
			s.turnStamp = &running
			s.knowTurn(running)
			// A replayed prompt is also the turn its page's next UNSTAMPED
			// terminal ends, and the page has now drawn a prompt.
			if s.plane.replayed() {
				s.replayTurn = &running
				s.replayPromptDrawn = true
			}
			if s.plane == planeInherited {
				// A FORK'S INHERITED TURN RAN IN ITS PARENT and ended there: no
				// terminal of it is owed to this feed, so its prompt is drawn
				// settled — the same as a ported prompt — and it retires no fault
				// of the fork's own, because it starts no turn here.
				s.endedTurns[running] = true
			} else {
				// A STANDING FINAL-ANSWER FAULT IS ABOUT THE TURN THAT ENDED, and
				// the next turn beginning is what retires it. Both turn-start
				// sites call it: a prompt drawn from history replay opens a turn
				// here without ever passing OnTurnOpened.
				r.turnStarted(s, running)
			}
			// THE PROMPT ITSELF MARKS ITS TURN A DIRECTIVE. The prompt is the
			// turn's FIRST frame on every path, so recognising the /clear or
			// /compact literal here registers the turn before its response and
			// terminal are drawn — the suppression no longer depends on the
			// ContextCut frame arriving first (live OnClearReceived) and so holds
			// on a fresh-resolver history replay too.
			if isContextCutDirective(prompt.GetSaid()) {
				s.directiveTurns[running] = true
			}
		}
		// A CONTEXT-CUT DIRECTIVE DRAWS NO PROMPT BUBBLE. /clear and /compact are
		// directives, not conversational prompts; their only visible outcome is
		// the separation bar and the feed they clear. The shim still emits the
		// directive's prompt frame — live and on replay — so this is where the
		// bubble it would draw is dropped.
		if turn.GetValue() != "" && s.directiveTurns[ids.TurnID(turn.GetValue())] {
			log.Debug("daemon.feed.directive_prompt_suppressed",
				"a context-cut directive's prompt frame drew no user-prompt bubble",
				dlog.Context{"turn": turn.GetValue(), "origin": prompt.GetOrigin().String()})
			return
		}
		if !placed {
			return
		}
		row := r.userPromptRow(s, at, turn, prompt.GetOrigin(), blocks)
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
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row condition", dlog.Context{"function": "feed", "condition": "head, ok := s.subFeeds[subFeedKey]; ok"})
		senderFeedKey = head.parentFeed
	}
	senderAddr, senderKnown := s.feedAddrs[senderFeedKey]
	if !senderKnown {
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row condition", dlog.Context{"function": "feed", "condition": "!senderKnown"})
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

// userPromptRow composes THE user-prompt row. It is one function because the
// same row is drawn from three places — a delivered prompt, a replayed one,
// and a fork's ported parent conversation — and three spellings of one row
// would let them disagree about its identity or its author.
func (r *resolver) userPromptRow(
	s *wsState,
	at placement,
	turn *conversationv1.TurnId,
	origin conversationv1.PromptOrigin,
	blocks []*frontendv1.FeedUserPromptBlock,
) *frontendv1.FeedRow {
	return &frontendv1.FeedRow{
		Id:   r.rowID(s.id, at.feed, feedid.RowKey{Kind: feedid.KindPrompt, ID: turn.GetValue()}),
		Turn: turn,
		Row: &frontendv1.FeedRow_UserPrompt{UserPrompt: &frontendv1.FeedUserPrompt{
			Author: &frontendv1.FeedUserPromptAuthor{Label: AuthorLabel(origin)},
			Result: &frontendv1.FeedUserPrompt_Success{Success: &frontendv1.FeedUserPromptSuccess{
				Body: &frontendv1.FeedUserPromptBody{Blocks: blocks},
			}},
		}},
	}
}

// ① THE PROMPT'S WORKING FLAG. FeedUserPrompt.working / FeedAgentPrompt.working
// state whether the prompt's turn is still running — a DAEMON FACT the client
// draws the prompt wave from verbatim. It has two edges and no others:
//
//   - SET from the row's first draw, by stampPromptWorking on every upsert of
//     a prompt row whose turn has not ended;
//   - UNSET at the turn's terminal, by settleTurnPrompts, which records the
//     turn as ended and RE-PUBLISHES every prompt row stamped with it.
//
// An interim response or a final answer drawn before the terminal moves
// neither edge: only the terminal ends the turn.

// stampPromptWorking states on a prompt row whether its turn is working. A row
// that is not a prompt is untouched; a prompt naming no turn is not working,
// because there is no turn for it to be waiting on.
func stampPromptWorking(s *wsState, row *frontendv1.FeedRow) {
	turn := row.GetTurn().GetValue()
	working := turn != "" && !s.endedTurns[ids.TurnID(turn)]
	switch kind := row.GetRow().(type) {
	case *frontendv1.FeedRow_UserPrompt:
		kind.UserPrompt.Working = working
	case *frontendv1.FeedRow_AgentPrompt:
		kind.AgentPrompt.Working = working
	}
}

// settleTurnPrompts is the turn's END for its prompts: the turn is recorded as
// ended and every prompt row stamped with it, on every feed, is re-published so
// its `working` flag falls on the same edge the terminal is drawn on. A row
// already settled upserts to an equal snapshot, which upsert drops as churn, so
// a terminal replayed across planes re-publishes nothing.
func (r *resolver) settleTurnPrompts(s *wsState, turn ids.TurnID) {
	s.endedTurns[turn] = true
	settled := 0
	for key, f := range s.feeds {
		for _, id := range f.order {
			row := f.rows[id]
			if row.GetTurn().GetValue() != string(turn) {
				continue
			}
			if !row.GetUserPrompt().GetWorking() && !row.GetAgentPrompt().GetWorking() {
				continue
			}
			r.upsert(s, placement{feed: s.feedAddrs[key]}, row, !f.nonDurable[id])
			settled++
		}
	}
	r.logger(s.id).Debug("daemon.feed.turn_prompts_settled",
		"the turn ended; its prompt rows were re-published no longer working",
		dlog.Context{"turn": string(turn), "rows": settled})
}

// drawPortedPrompt draws one row of a FORK'S PORTED CONVERSATION: a question
// the PARENT was asked, carried over under the child's own turn id.
//
// IT NEVER OPENS A TURN. A delivered prompt teaches the resolver which turn
// the session is running; a ported one is history that was settled in another
// workspace, and treating it as in flight would leave the child stamping its
// own rows with a turn that ended before it existed.
func (r *resolver) drawPortedPrompt(s *wsState, prompt PortedPrompt) {
	turn := &conversationv1.TurnId{Value: prompt.Turn}
	// A PORTED TURN ENDED IN THE PARENT, so its prompt is drawn settled: no
	// terminal for it will ever reach this workspace to end it here.
	if prompt.Turn != "" {
		s.endedTurns[ids.TurnID(prompt.Turn)] = true
		s.knowTurn(ids.TurnID(prompt.Turn))
	}
	at := r.outputPlacement(s)
	blocks := r.drawUserBlocks(s, SaidText(prompt.Text).GetContent())
	row := r.userPromptRow(s, at, turn, prompt.Origin, blocks)
	r.logger(s.id).Debug("daemon.feed.ported_prompt",
		"a prompt ported from the parent workspace was drawn on the fork's feed",
		dlog.Context{"turn": prompt.Turn, "origin": prompt.Origin.String(), "blocks": len(blocks)})
	r.upsert(s, at, row, true)
}

// SaidText composes the one canonical prompt form from plain text. A ported
// row carries the text the parent recorded and nothing else, so the content it
// is drawn from is built here rather than stored twice.
func SaidText(text string) *conversationv1.UserSaid {
	return &conversationv1.UserSaid{
		Content: &conversationv1.UserContent{
			Blocks: []*conversationv1.UserContentBlock{{
				Block: &conversationv1.UserContentBlock_Text{
					Text: &conversationv1.TextBlock{Text: text},
				},
			}},
		},
	}
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

// drawUserBlocks resolves what a person composed into drawable blocks, with
// this resolver's own strip and image resolution.
func (r *resolver) drawUserBlocks(s *wsState, content *conversationv1.UserContent) []*frontendv1.FeedUserPromptBlock {
	return DrawUserBlocks(content, r.deps.StripSentinels, r.resolveImage, r.logger(s.id))
}

// DrawUserBlocks resolves what a person composed into drawable blocks: the
// text with the host's sentinel spans STRIPPED (the full text stays on the
// record), each image reference resolved to a src the webview can load, and
// anything unmodeled named rather than dropped.
//
// IT IS EXPORTED BECAUSE TWO PRODUCERS DRAW THE SAME ROW. The resolver draws
// a user prompt when history replays it; the prompt queue MIRRORS the same
// row the instant a submission is accepted, so the person sees what they
// said without waiting for the shim. Those are one row under one key, and
// when they were two implementations they disagreed: the mirror dropped every
// image block, so an attached image was invisible for the whole live session
// and appeared only on the next page that replayed history. One function is
// the only arrangement in which they cannot disagree again.
func DrawUserBlocks(
	content *conversationv1.UserContent,
	strip func(string) string,
	resolveImage ImageResolver,
	log dlog.Logger,
) []*frontendv1.FeedUserPromptBlock {
	blocks := make([]*frontendv1.FeedUserPromptBlock, 0, len(content.GetBlocks()))
	for _, block := range content.GetBlocks() {
		switch b := block.GetBlock().(type) {
		case *conversationv1.UserContentBlock_Text:
			drawn := strip(b.Text.GetText())
			// A block whose whole text was sentinel span is NOTHING TO DRAW,
			// and an empty text block draws as an empty line in the bubble.
			if drawn == "" {
				continue
			}
			blocks = append(blocks, &frontendv1.FeedUserPromptBlock{
				Block: &frontendv1.FeedUserPromptBlock_Text{Text: &frontendv1.FeedTextBlock{Text: drawn}},
			})
		case *conversationv1.UserContentBlock_Image:
			src, alt, err := resolveImage(b.Image)
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
