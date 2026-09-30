package promptqueue

import (
	"context"
	"fmt"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
	shimv1 "agentrepl/proto/shim/v1"

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
//
// A SUBMISSION WHOSE TEXT IS A CONTEXT CUT IS DELIVERED AS ONE, whatever path
// it came by (a held /compact, an edit that became one, a caller that never
// went through recognition): runContextCut runs it, so the queue records it as
// the running session act and nothing can interject it or be popped into it.
func (q *queue) deliver(ctx context.Context, sub Submission, sender Sender, watcher Watcher, log dlog.Logger) (Disposition, error) {
	if command, arg, ok := contextCutOf(sub); ok {
		act := Act{Kind: actKindOf(command), Value: arg, Turn: sub.Turn, Origin: sub.Origin}
		log.Info(opDeliver, "the prompt is a session act; it is delivered as one", dlog.Context{
			"session_act": command.String(),
		})
		if err := q.runContextCut(ctx, sub.WS, act, sender, log); err != nil {
			return Disposition{}, err
		}
		return Disposition{Delivered: true}, nil
	}
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

	// THE TURN FACT IS THE DAEMON'S OWN, AND IT IS PUBLISHED ON RECEIPT — before
	// the shim is asked. Nothing on the shim's streams says a turn was accepted —
	// its first frame is an activity, by which time `submitting` is over — so a
	// client left to infer it reads `ready`/idle for a workspace whose turn is
	// starting. Both the footer and the roster take `thinking · submitting` now,
	// each as its own immediate publish, so the STALL of the (blocking) StartTurn
	// below is shown as submitting rather than as idle.
	submitting := &footer.TurnStarted{At: q.deps.Now(), Act: footer.ActPrompt, Prompt: record.Text}
	q.deps.Footer.SetTurn(sub.WS, submitting)
	q.deps.Sidebar.SetTurn(sub.WS, submitting)

	// THE WATCHER LEARNS THE TURN BEFORE THE SHIM DOES. The shim can put the
	// turn's frames — its terminal included — on the agent stream before this
	// call returns; a terminal routed while no turn stands in flight is
	// attributable to nothing, and every AwaitTurnEnd on that turn hangs.
	watcher.OnTurnOpening(sub.WS, sub.Turn)
	success, err := sender.StartTurn(ctx, sub.Turn, sub.Said, sub.Origin)
	if err != nil {
		// A refusal is surfaced to the caller (which answers the rpc with it)
		// rather than swallowed, and the footer and roster drop the submitting
		// turn so no workspace is left showing a `submitting` phase for a turn
		// that never ran.
		q.retireOpenedTurn(sub, watcher)
		log.Error(opDeliver, "the shim refused the turn", dlog.Context{"cause": err.Error()})
		return Disposition{}, fmt.Errorf("start turn %q on %q: %w", sub.Turn, sub.WS, err)
	}

	q.acceptOpenedTurn(ctx, sub, success, watcher, log)
	return Disposition{Delivered: true}, nil
}

// acceptOpenedTurn is the handover a successful StartTurn owes: the
// submitting window closes, the main agent is named, and the accepted turn is
// handed to the watcher that will see it end.
func (q *queue) acceptOpenedTurn(ctx context.Context, sub Submission, success *shimv1.StartTurnSuccess, watcher Watcher, log dlog.Logger) {
	// The shim TOOK the turn: the `submitting` window is over.
	q.deps.Sidebar.AckTurn(sub.WS)

	if agent := success.GetPrompt().GetAgent(); agent.GetValue() != "" {
		watcher.SetMainAgent(agent)
	}
	watcher.OnTurnOpened(sub.WS, success.GetPrompt(), success.GetPage())

	q.touchEngagement(ctx, sub.WS, log)
	log.Info(opDeliver, "delivered the prompt to the shim", dlog.Context{
		"agent": success.GetPrompt().GetAgent().GetValue(),
	})
}

// retireOpenedTurn drops the submitting turn a StartTurn opened but the shim
// then refused: the watcher's opening record is retired and the footer and
// roster clear the submitting phase.
func (q *queue) retireOpenedTurn(sub Submission, watcher Watcher) {
	watcher.OnTurnOpenFailed(sub.WS, sub.Turn)
	q.deps.Footer.SetTurn(sub.WS, nil)
	q.deps.Sidebar.SetTurn(sub.WS, nil)
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

// logRepeatedStart records a submission whose turn is ALREADY the turn in
// flight, which the caller answers as delivered without starting anything.
//
// A TURN ID IS STARTED ONCE. Only a retry of an idempotency claim the queue
// never stamped accepted comes back under a turn id it already used (the
// prompt handler re-drives it under the SAME id), so finding that turn running
// means the original DID reach the shim and the process lost the stamp -- the
// crash window the re-drive exists to close. Holding the retry behind the
// running turn would queue the turn behind itself and start it again when it
// ends; delivering it would ask the shim to start it twice. Both are the
// double delivery, so it is answered as the delivery the original already
// was, and recorded at ERROR because a repeated start is an invariant
// violation whichever side catches it.
func (q *queue) logRepeatedStart(log dlog.Logger, verb string, turn ids.TurnID) {
	log.Error(opSubmit, "a submission repeated the turn already in flight; it is answered as the delivery the original was and nothing is started again",
		dlog.Context{"turn": string(turn), "verb": verb, "original_state": "in_flight"})
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
	row := &frontendv1.FeedRow{
		Turn: &conversationv1.TurnId{Value: string(turn)},
		Row: &frontendv1.FeedRow_UserPrompt{UserPrompt: &frontendv1.FeedUserPrompt{
			Author: &frontendv1.FeedUserPromptAuthor{Label: feed.AuthorLabel(origin)},
			Result: &frontendv1.FeedUserPrompt_Success{Success: &frontendv1.FeedUserPromptSuccess{
				Body: &frontendv1.FeedUserPromptBody{Blocks: q.mirrorBlocks(said)},
			}},
		}},
	}
	// THE MIRROR LANDS AT THE SESSION'S OUTPUT ADDRESS, never unconditionally
	// on the root feed: while a merge lease has addressed the session at one of
	// its tabs, the guidance the user types belongs on that tab, and the
	// resolver's own draw of the same row key lands there too — so the mirror
	// and the draw are one row.
	q.deps.Feed.UpsertAtOutputAddress(ws, feedid.RowKey{Kind: feedid.KindPrompt, ID: string(turn)}, row)
}

// mirrorBlocks renders a submission's content as drawn blocks, through the
// SAME function the feed resolver draws a replayed user prompt with. A live
// session sees only this mirror -- nothing brings a delivered user prompt
// back on the watch -- so a mirror that drew less than the resolver drew LESS
// THAN THE PERSON SAID, for the whole session. It dropped every image block.
func (q *queue) mirrorBlocks(said *conversationv1.UserSaid) []*frontendv1.FeedUserPromptBlock {
	return feed.DrawUserBlocks(said.GetContent(), q.deps.StripSentinels, q.deps.ResolveImage,
		q.deps.Log.Global())
}
