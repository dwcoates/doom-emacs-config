package feed

import (
	"agent-repld/internal/prose"
	"fmt"
	"strings"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/feedid"
	"agent-repld/internal/ids"
	"agent-repld/internal/promptorigin"
	"agent-repld/internal/replyquote"
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
	own := turnOf(turn)
	if _, addressed := s.turnAddresses[ids.TurnID(turn.GetValue())]; !isSub || addressed {
		// The main agent's prompt, or any prompt of an addressed turn: one
		// user-prompt row, placed by the prompt's OWN turn (a folded prompt
		// too: it is drawn where the turn its origin recorded draws).
		// AN UNPLACEABLE PROMPT STILL OPENS ITS TURN: only its row is not drawn
		// (place has reported why).
		at, placed := r.placeTurn(s, recipient, own)
		// A PROMPT FOLDED INTO A RUNNING TURN OPENS NO TURN. The vendor took it
		// into the turn it names at a tool boundary, so its bubble belongs to
		// that turn: stamped with it, working while it runs and settled by its
		// terminal, and the turn the feed is running is left as it stands.
		if joined := prompt.GetFoldedInto(); joined.GetValue() != "" {
			if !placed {
				return
			}
			row := r.userPromptRow(s, at, turn, joined, prompt.GetOrigin(), prompt.GetSaid(), blocks)
			log.Debug("daemon.feed.folded_prompt",
				"a prompt folded into a running turn was drawn under that turn",
				dlog.Context{"turn": turn.GetValue(), "folded_into": joined.GetValue(), "blocks": len(blocks)})
			r.upsert(s, at, row, true)
			return
		}
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
				// A FORK'S INHERITED TURN RAN IN ITS PARENT and ended there: its
				// prompt is drawn settled, and it retires no fault of the fork's
				// own, because it starts no turn here.
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
		// A VENDOR-STARTED TURN'S PROMPT ROW IS AN EDGE, NOT WORDS. Nobody said
		// anything: the shim wrote it only to open a turn the vendor began on
		// its own, and what drove that turn (a hand-back, a task notification)
		// is its own row. The turn opened above; no bubble is drawn.
		if prompt.GetOrigin() == conversationv1.PromptOrigin_PROMPT_ORIGIN_VENDOR_STARTED {
			log.Debug("daemon.feed.vendor_started_prompt_suppressed",
				"a vendor-started turn's prompt row opened its turn and drew no user-prompt bubble",
				dlog.Context{"turn": turn.GetValue()})
			return
		}
		// A RESUMPTION AFTER RESTART, OR A MERGE QUEUE NOTICE, IS THE DAEMON'S
		// WORD, NOT WORDS ANYONE SAID (promptqueue/restartredrive.go,
		// internal/mergenotice). Its turn opened above and runs as any turn;
		// it is shown only as the status a running turn shows, so no bubble
		// is drawn, live or on replay.
		if PromptRowHidden(prompt.GetOrigin()) {
			log.Debug("daemon.feed.resume_prompt_suppressed",
				"a prompt of an origin the feed draws no row for opened its turn and drew no user-prompt bubble",
				dlog.Context{"turn": turn.GetValue(), "origin": prompt.GetOrigin().String()})
			return
		}
		if !placed {
			return
		}
		row := r.userPromptRow(s, at, turn, turn, prompt.GetOrigin(), prompt.GetSaid(), blocks)
		if turn.GetValue() != "" && !s.rolledBack[ids.TurnID(turn.GetValue())] {
			// THE PROMPT AS SAID, kept so a rollback can hand it back to the
			// composer whole (rollback.go): the row holds only drawn blocks.
			s.promptSaid[ids.TurnID(turn.GetValue())] = prompt.GetSaid()
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
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row condition", dlog.Context{"function": "feed", "condition": "head, ok := s.subFeeds[subFeedKey]; ok"})
		senderFeedKey = head.parentFeed
	}
	senderAddr, senderKnown := s.feedAddrs[senderFeedKey]
	if !senderKnown {
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row condition", dlog.Context{"function": "feed", "condition": "!senderKnown"})
		senderAddr = feedid.Feed{Root: true}
	}

	outgoing := &frontendv1.FeedRow{
		Id:   r.rowID(s.id, senderAddr, feedid.RowKey{Kind: feedid.KindPrompt, ID: turn.GetValue(), Sub: "out"}),
		Turn: turn,
		Row: &frontendv1.FeedRow_AgentPrompt{AgentPrompt: &frontendv1.FeedAgentPrompt{
			Address: sentToAgent(r.agentNamed(s, recipient.GetValue())),
			Body:    &frontendv1.FeedAgentPromptBody{Blocks: agentBlocks(blocks)},
		}},
	}
	delivered := &frontendv1.FeedRow{
		Id:   r.rowID(s.id, recipientFeed, feedid.RowKey{Kind: feedid.KindPrompt, ID: turn.GetValue(), Sub: "in"}),
		Turn: turn,
		Row: &frontendv1.FeedRow_AgentPrompt{AgentPrompt: &frontendv1.FeedAgentPrompt{
			Address: receivedFromAgent(r.agentOfFeed(s, senderFeedKey)),
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
// same row is drawn from four places — a delivered prompt, a replayed one, a
// prompt folded into a running turn, and a fork's ported parent conversation —
// and four spellings of one row would let them disagree about its identity or
// its author. The row is keyed by the prompt's own id and stamped with the
// turn it belongs to, which differ only for a folded prompt.
func (r *resolver) userPromptRow(
	s *wsState,
	at placement,
	prompt *conversationv1.TurnId,
	turn *conversationv1.TurnId,
	origin conversationv1.PromptOrigin,
	said *conversationv1.UserSaid,
	blocks []*frontendv1.FeedUserPromptBlock,
) *frontendv1.FeedRow {
	row := PromptRow(origin, r.promptHeader(s, ids.TurnID(prompt.GetValue()), origin), said, blocks)
	row.Id = r.rowID(s.id, at.feed, feedid.RowKey{Kind: feedid.KindPrompt, ID: prompt.GetValue()})
	row.Turn = turn
	return row
}

// PromptRow is what a delivered prompt's row DRAWS, before its identity and
// turn are stamped on: a user-prompt bubble carrying the drawn blocks under its
// author; for a detached merge's handback (PROMPT_ORIGIN_MERGE_HANDBACK), the
// handback badge alone; and for a prompt another workspace's agent sent
// (PROMPT_ORIGIN_AGENT_SENT), the agent-prompt bubble under HEADER, the
// sending workspace (PromptHeader). HEADER is nil for every other origin.
//
// It is EXPORTED because the prompt queue's mirror of an accepted prompt draws
// the same row under the same FeedId; the two draws compose the row here, so
// they can never disagree about what a prompt is drawn as.
func PromptRow(origin conversationv1.PromptOrigin, header *frontendv1.FeedAgentPromptAddress, said *conversationv1.UserSaid, blocks []*frontendv1.FeedUserPromptBlock) *frontendv1.FeedRow {
	if origin == conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_HANDBACK {
		return &frontendv1.FeedRow{Row: &frontendv1.FeedRow_MergeHandback{MergeHandback: &frontendv1.FeedMergeHandbackBadge{
			Label: &frontendv1.FeedMergeHandbackBadgeLabel{Text: MergeHandbackLabel(said)},
		}}}
	}
	if err := promptorigin.CheckSender(origin, header != nil); err != nil {
		panic(fmt.Sprintf("feed.PromptRow: a prompt drawn under the wrong header: %v", err))
	}
	if header != nil {
		return &frontendv1.FeedRow{Row: &frontendv1.FeedRow_AgentPrompt{AgentPrompt: &frontendv1.FeedAgentPrompt{
			Address: header,
			Body:    &frontendv1.FeedAgentPromptBody{Blocks: agentBlocks(blocks)},
		}}}
	}
	return &frontendv1.FeedRow{Row: &frontendv1.FeedRow_UserPrompt{UserPrompt: &frontendv1.FeedUserPrompt{
		Author: &frontendv1.FeedUserPromptAuthor{Label: AuthorLabel(origin)},
		Result: &frontendv1.FeedUserPrompt_Success{Success: &frontendv1.FeedUserPromptSuccess{
			Body: &frontendv1.FeedUserPromptBody{Blocks: blocks},
		}},
	}}}
}

// MergeHandbackLabel is a merge handback's badge text: the FIRST LINE of the
// handback's first text block. The handback brief
// (prompts/merge-detached-handback.md) leads with exactly that line ("merge of
// fix-flaky landed"), so the badge states the outcome in the daemon's words
// and the rest of the report is the agent's to read.
func MergeHandbackLabel(said *conversationv1.UserSaid) string {
	for _, block := range said.GetContent().GetBlocks() {
		if text := block.GetText().GetText(); text != "" {
			first, _, _ := strings.Cut(text, "\n")
			return strings.TrimSpace(first)
		}
	}
	return ""
}

// ① THE PROMPT'S WORKING FLAG. FeedUserPrompt.working / FeedAgentPrompt.working
// state whether the prompt's turn is still running — a DAEMON FACT the client
// draws the prompt wave from. It has two edges and no others:
//
//   - SET from the row's first draw, by stampPromptWorking on every upsert of
//     a prompt row whose turn has not ended;
//   - UNSET at the turn's terminal, by settleTurnPrompts, which records the
//     turn as ended and RE-PUBLISHES every prompt row stamped with it.
//
// An interim response or a final answer drawn before the terminal moves
// neither edge: only the terminal ends the turn.
//
// ①' THE PROMPT'S AWAITING-USER FLAG. FeedUserPrompt.awaiting_user /
// FeedAgentPrompt.awaiting_user state that the working turn is BLOCKED ON THE
// USER — a question or a consent stamped with it stands open — and the client
// pauses the wave for exactly that span. stampPromptWorking states it from the
// asks the resolver holds, and restateTurnPrompts re-publishes the turn's
// prompts on each of its edges: an ask of the turn opening, and one settling.

// stampPromptWorking states on a prompt row whether its turn is working, and
// whether that working turn is waiting on the user. A row that is not a prompt
// is untouched; a prompt naming no turn is not working, because there is no
// turn for it to be waiting on.
func stampPromptWorking(s *wsState, row *frontendv1.FeedRow) {
	turn := row.GetTurn().GetValue()
	working := turn != "" && !s.endedTurns[ids.TurnID(turn)]
	awaiting := working && turnAwaitingUser(s, ids.TurnID(turn))
	switch kind := row.GetRow().(type) {
	case *frontendv1.FeedRow_UserPrompt:
		kind.UserPrompt.Working = working
		kind.UserPrompt.AwaitingUser = awaiting
	case *frontendv1.FeedRow_AgentPrompt:
		kind.AgentPrompt.Working = working
		kind.AgentPrompt.AwaitingUser = awaiting
	}
}

// turnAwaitingUser answers whether TURN is blocked on the user: a consent card
// or a question card stamped with it stands open.
func turnAwaitingUser(s *wsState, turn ids.TurnID) bool {
	for _, p := range s.permissionRows {
		if p.card.GetOpen() != nil && p.turn.GetValue() == string(turn) {
			return true
		}
	}
	for _, q := range s.questionAsks {
		if q.open && q.turn.GetValue() == string(turn) {
			return true
		}
	}
	return false
}

// restateTurnPrompts re-publishes every prompt row stamped with TURN, so each
// restates its `awaiting_user` after one of the turn's asks opened or settled.
// A prompt whose flags did not move upserts to an equal snapshot, which upsert
// drops as churn, so an ask frame that changes nothing about the turn's
// blocking re-publishes nothing. An unstamped ask names no turn to restate.
func (r *resolver) restateTurnPrompts(s *wsState, turn *conversationv1.TurnId) {
	if turn.GetValue() == "" {
		return
	}
	restated := r.republishRows(s, func(row *frontendv1.FeedRow) bool {
		return isTurnPrompt(row, turn.GetValue())
	})
	r.logger(s.id).Debug("daemon.feed.turn_prompts_restated",
		"an ask of the turn opened or settled; its prompt rows were re-published",
		dlog.Context{"turn": turn.GetValue(), "rows": restated,
			"awaiting_user": turnAwaitingUser(s, ids.TurnID(turn.GetValue()))})
}

// settleTurnPrompts is the turn's END for its prompts: the turn is recorded as
// ended and every prompt row stamped with it, on every feed, is re-published so
// its `working` flag falls on the same edge the terminal is drawn on. A row
// already settled upserts to an equal snapshot, which upsert drops as churn, so
// a terminal replayed across planes re-publishes nothing.
func (r *resolver) settleTurnPrompts(s *wsState, turn ids.TurnID) {
	s.endedTurns[turn] = true
	settled := r.republishRows(s, func(row *frontendv1.FeedRow) bool {
		return isTurnPrompt(row, string(turn)) &&
			(row.GetUserPrompt().GetWorking() || row.GetAgentPrompt().GetWorking())
	})
	r.logger(s.id).Debug("daemon.feed.turn_prompts_settled",
		"the turn ended; its prompt rows were re-published no longer working",
		dlog.Context{"turn": string(turn), "rows": settled})
}

// isTurnPrompt answers whether ROW is a prompt row (a user's or an agent's)
// stamped with TURN: the rows whose `working` and `awaiting_user` flags that
// turn's edges move.
func isTurnPrompt(row *frontendv1.FeedRow, turn string) bool {
	return row.GetTurn().GetValue() == turn &&
		(row.GetUserPrompt() != nil || row.GetAgentPrompt() != nil)
}

// republishRows re-publishes, in place on its own feed, every stored row of
// the workspace that MATCH accepts, so each restates whatever upsert stamps
// from state that just changed (a prompt's `working`, its `commands`). A row
// whose snapshot comes out equal is dropped by upsert as churn. It answers how
// many rows MATCH accepted.
func (r *resolver) republishRows(s *wsState, match func(*frontendv1.FeedRow) bool) int {
	matched := 0
	for key, f := range s.feeds {
		for _, id := range f.order {
			row := f.rows[id]
			if !match(row) {
				continue
			}
			r.upsert(s, placement{feed: s.feedAddrs[key]}, row, !f.nonDurable[id])
			matched++
		}
	}
	return matched
}

// SaidText composes the one canonical prompt form from plain text.
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
		conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_DISPLACED_TURN_RESUME,
		conversationv1.PromptOrigin_PROMPT_ORIGIN_WORKSPACE_RETARGETED:
		return "Merge"
	case conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_REVERT:
		return "Merge revert"
	case conversationv1.PromptOrigin_PROMPT_ORIGIN_RESUME_AFTER_RESTART:
		return "Resumed after restart"
	case conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_QUEUE_NOTICE:
		// promptorigin.Unseen: no row is drawn for it; the label only keeps
		// any other reader from naming the user as its author.
		return "Merge queue"
	case conversationv1.PromptOrigin_PROMPT_ORIGIN_WORKSPACE_NOTICE,
		conversationv1.PromptOrigin_PROMPT_ORIGIN_DERIVED_WORKSPACE_NOTICE:
		// promptorigin.Unseen, as the merge queue's notice is.
		return "Workspace notice"
	case conversationv1.PromptOrigin_PROMPT_ORIGIN_OWNER_NOTICE:
		return "Owner notice"
	case conversationv1.PromptOrigin_PROMPT_ORIGIN_AGENT_SENT:
		return "Agent"
	case conversationv1.PromptOrigin_PROMPT_ORIGIN_WORKSPACE_CREATED:
		return "Workspace brief"
	case conversationv1.PromptOrigin_PROMPT_ORIGIN_VENDOR_STARTED:
		// drawAgentPrompt draws no row for this origin; the label only keeps
		// any other reader from naming the user as the author of a turn the
		// vendor began on its own.
		return "Vendor"
	}
	return "You"
}

// PromptRowHidden reports whether a prompt of origin draws no user-prompt row
// in the feed: the daemon's resumption after restart, which is an instruction
// the daemon gives, not words anyone said, and every prompt drawn nowhere at
// all (promptorigin.Unseen: a merge queue notice). The queue's mirror of an
// accepted prompt reads the SAME answer, so the mirror and the replayed draw
// agree.
func PromptRowHidden(origin conversationv1.PromptOrigin) bool {
	return origin == conversationv1.PromptOrigin_PROMPT_ORIGIN_RESUME_AFTER_RESTART || promptorigin.Unseen(origin)
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
				Block: &frontendv1.FeedUserPromptBlock_Text{Text: &frontendv1.FeedTextBlock{Prose: prose.ParsePrompt(log, drawn)}},
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
		case *conversationv1.UserContentBlock_Quote:
			// A QUOTED BUBBLE IS ITS OWN DRAWN BLOCK, unstripped and taken
			// apart into its parts: the daemon composed it (replyquote), so
			// it holds no host sentinel span, and the client draws each part
			// in its own place without parsing text.
			blocks = append(blocks, &frontendv1.FeedUserPromptBlock{
				Block: &frontendv1.FeedUserPromptBlock_Quote{Quote: drawQuote(b.Quote, log)},
			})
		}
	}
	return blocks
}

// drawQuote is QUOTE taken apart for drawing (replyquote.Split). Every quote
// block is composed by replyquote.Block, so one that does not split is an
// invariant violation: recorded once and panicked on, never drawn as a guess.
func drawQuote(quote *conversationv1.UserQuoteBlock, log dlog.Logger) *frontendv1.FeedQuoteBlock {
	parts, err := replyquote.Split(quote.GetText())
	if err != nil {
		log.Error("daemon.feed.quote_unsplittable",
			"a prompt's quote block was not composed by replyquote.Block, so it cannot be drawn",
			dlog.Context{"characters": len(quote.GetText()), "cause": err.Error()})
		panic(fmt.Sprintf("feed: a quote block did not split: %v", err))
	}
	return &frontendv1.FeedQuoteBlock{Heading: parts.Heading, Quoted: parts.Quoted, Closing: parts.Closing}
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
		case *frontendv1.FeedUserPromptBlock_Quote:
			out = append(out, &frontendv1.FeedAgentPromptBlock{
				Block: &frontendv1.FeedAgentPromptBlock_Quote{Quote: b.Quote},
			})
		}
	}
	return out
}
