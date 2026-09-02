package promptqueue

import (
	"context"
	"fmt"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/feedid"
	"claude-repld/internal/ids"
	"claude-repld/internal/resolve/feed"
	"claude-repld/internal/resolve/footer"
	"claude-repld/internal/wsm"
)

// deliver sends a submission to the shim as the session's turn: the durable
// turn record first (so the origin survives a restart even if the daemon dies
// mid-call), then StartTurn, then the handover to the watcher.
func (q *queue) deliver(ctx context.Context, sub Submission, sender Sender, watcher Watcher, log dlog.Logger) (Disposition, error) {
	record := wsm.Turn{
		ID:        sub.Turn,
		Workspace: sub.WS,
		Text:      saidText(sub.Said),
		Origin:    sub.Origin.String(),
		StartedAt: q.deps.Now(),
	}
	if err := q.deps.DB.PutTurn(ctx, record); err != nil {
		log.Error(opDeliver, "could not record the turn before delivering it", dlog.Context{"cause": err.Error()})
		return Disposition{}, fmt.Errorf("record turn %q on %q: %w", sub.Turn, sub.WS, err)
	}

	// THE ACCEPTED PROMPT'S FEED ROW, DRAWN BEFORE StartTurn. The prompt is
	// mirrored to the user the moment it is accepted for delivery, not once
	// the shim answers — a composer that cleared its text has nothing to show
	// for the round trip otherwise. The row carries the same FeedId the main
	// watch's own history entry will carry, so the two upsert onto one row
	// rather than drawing the prompt twice.
	q.mirrorAccepted(sub.WS, sub.Turn, sub.Said, sub.Origin)

	// THE ROSTER'S TURN FACT IS THE DAEMON'S OWN. Nothing on the shim's streams
	// says a turn was accepted — its first frame is an activity, by which time
	// `submitting` is over — so a roster left to infer it reads `ready` for a
	// workspace whose turn is running.
	q.deps.Sidebar.SetTurn(sub.WS, &footer.TurnStarted{At: q.deps.Now(), Act: footer.ActPrompt})

	success, err := sender.StartTurn(ctx, sub.Turn, sub.Said, sub.Origin)
	if err != nil {
		q.deps.Sidebar.SetTurn(sub.WS, nil)
		log.Error(opDeliver, "the shim refused the turn", dlog.Context{"cause": err.Error()})
		return Disposition{}, fmt.Errorf("start turn %q on %q: %w", sub.Turn, sub.WS, err)
	}

	if agent := success.GetPrompt().GetAgent(); agent.GetValue() != "" {
		watcher.SetMainAgent(agent)
	}
	watcher.OnTurnOpened(sub.WS, success.GetPrompt(), success.GetPage())

	q.touchEngagement(ctx, sub.WS, log)
	log.Info(opDeliver, "delivered the prompt to the shim", dlog.Context{
		"agent": success.GetPrompt().GetAgent().GetValue(),
	})
	return Disposition{Delivered: true}, nil
}

// deliverToAgent sends a bubble composer's prompt to the addressed agent.
func (q *queue) deliverToAgent(ctx context.Context, sub Submission, sender Sender, log dlog.Logger) (Disposition, error) {
	agent := sub.Target.Feed.Agent
	if agent == nil && sub.Target.Row.Sub != "" {
		// A subagent BUBBLE row addresses its own sub-feed: the created agent
		// is the row key's secondary key.
		agent = &conversationv1.AgentId{Value: sub.Target.Row.Sub}
	}
	if agent.GetValue() == "" {
		log.Error(opDeliver, "the addressed feed row names no agent", nil)
		return Disposition{}, fmt.Errorf("submit to %q: the addressed feed row names no agent", sub.WS)
	}
	if err := sender.PromptAgent(ctx, agent, sub.Said); err != nil {
		log.Error(opDeliver, "the shim refused the agent-addressed prompt", dlog.Context{
			"agent": agent.GetValue(), "cause": err.Error(),
		})
		return Disposition{}, fmt.Errorf("prompt agent %q on %q: %w", agent.GetValue(), sub.WS, err)
	}
	q.touchEngagement(ctx, sub.WS, log)
	log.Info(opDeliver, "delivered the prompt to the addressed agent", dlog.Context{"agent": agent.GetValue()})
	return Disposition{Delivered: true}, nil
}

// touchEngagement records that the user just engaged this session. IT IS WHAT
// THE IDLE SWEEP MEASURES: without it every session's engagement stands still
// at whatever the record was created with, so an actively used workspace is
// hibernated out from under its user — and a session revived BY a prompt is
// hibernated again before the prompt's own turn has run.
//
// A failure to record it is a warning, never the submission's failure: the
// prompt was delivered, and the worst a lost stamp costs is one early
// hibernation.
func (q *queue) touchEngagement(ctx context.Context, ws ids.WorkspaceID, log dlog.Logger) {
	if err := q.deps.DB.TouchEngagement(ctx, ws, q.deps.Now()); err != nil {
		log.Warn(opDeliver, "could not record the session's engagement",
			dlog.Context{"cause": err.Error()})
	}
}

// mirrorAccepted draws an accepted prompt's user_prompt row. The sentinel
// spans are STRIPPED from the drawn text; the full text stays on the record and
// on what the shim received.
func (q *queue) mirrorAccepted(ws ids.WorkspaceID, turn ids.TurnID, said *conversationv1.UserSaid, origin conversationv1.PromptOrigin) {
	root := feedid.Feed{Root: true}
	row := &frontendv1.FeedRow{
		Id:   feedid.Encode(feedid.Ref{WS: ws, Feed: root, Row: feedid.RowKey{Kind: feedid.KindPrompt, ID: string(turn)}}),
		Turn: &conversationv1.TurnId{Value: string(turn)},
		Row: &frontendv1.FeedRow_UserPrompt{UserPrompt: &frontendv1.FeedUserPrompt{
			Author: &frontendv1.FeedUserPromptAuthor{Label: feed.AuthorLabel(origin)},
			Result: &frontendv1.FeedUserPrompt_Success{Success: &frontendv1.FeedUserPromptSuccess{
				Body: &frontendv1.FeedUserPromptBody{Blocks: q.mirrorBlocks(said)},
			}},
		}},
	}
	q.deps.Feed.UpsertSynthesized(ws, root, row)
}

// mirrorBlocks renders a submission's content as drawn blocks. Only text is
// mirrored: an image's `src` is the feed resolver's to resolve, and the
// resolver's own draw of the same row replaces this one the moment the prompt
// comes back on the watch.
func (q *queue) mirrorBlocks(said *conversationv1.UserSaid) []*frontendv1.FeedUserPromptBlock {
	blocks := make([]*frontendv1.FeedUserPromptBlock, 0, len(said.GetContent().GetBlocks()))
	for _, block := range said.GetContent().GetBlocks() {
		text, ok := block.GetBlock().(*conversationv1.UserContentBlock_Text)
		if !ok {
			continue
		}
		drawn := q.deps.StripSentinels(text.Text.GetText())
		if drawn == "" {
			continue
		}
		blocks = append(blocks, &frontendv1.FeedUserPromptBlock{
			Block: &frontendv1.FeedUserPromptBlock_Text{Text: &frontendv1.FeedTextBlock{Text: drawn}},
		})
	}
	return blocks
}
