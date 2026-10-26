package promptqueue

import (
	"context"
	"errors"
	"fmt"
	"maps"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
	shimv1 "agentrepl/proto/shim/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/feedid"
	"agent-repld/internal/ids"
	"agent-repld/internal/resolve/feed"
	"agent-repld/internal/resolve/footer"
	"agent-repld/internal/wsm"
)

// deliver sends a submission to the shim as the session's turn: the durable
// turn record first (so the origin survives a restart even if the daemon dies
// mid-call), then StartTurn, then the handover to the watcher.
//
// A SUBMISSION WHOSE TEXT IS A CONTEXT CUT IS DELIVERED AS ONE, whatever path
// it came by (a held /compact, an edit that became one, a caller that never
// went through recognition): runContextCut runs it, so the queue records it as
// the running session act and nothing can interject it or be popped into it.
//
// THE StartTurn IS MADE WITH THE DELIVERY LOCK RELEASED (call.go). Everything
// before it -- the turn record, the mirrored row, the submitting phase, the
// watcher's opening record -- is the claim the turn stands on while the lock
// is free: a prompt submitted meanwhile is held behind this turn.
func (q *queue) deliver(ctx context.Context, d *delivery, sub Submission, sender Sender, watcher Watcher, log dlog.Logger) (Disposition, error) {
	// A HELD ACT IS APPLIED, NOT STARTED: it opens no turn, so the pop that
	// delivered it goes on to the entry behind it.
	if sub.Act != nil {
		log.Info(opDeliver, "the entry is a held session act; it is applied now", dlog.Context{
			"session_act": sub.Act.Kind, "value": sub.Act.Value,
		})
		if err := q.runAct(ctx, d, actOfHeld(sub), sub.claims(), log); err != nil {
			return Disposition{}, err
		}
		return Disposition{Delivered: true}, nil
	}
	if command, arg, ok := contextCutOf(sub); ok {
		act := Act{Kind: actKindOf(command), Value: arg, Turn: sub.Turn, Origin: sub.Origin}
		log.Info(opDeliver, "the prompt is a session act; it is delivered as one", dlog.Context{
			"session_act": command.String(),
		})
		if err := q.runContextCut(ctx, d, act, sender, sub.claims(), log); err != nil {
			return Disposition{}, err
		}
		return Disposition{Delivered: true}, nil
	}
	record := wsm.Turn{
		ID:        sub.Turn,
		Workspace: sub.WS,
		Text:      saidText(sub.Said),
		Origin:    sub.Origin.String(),
		SentBy:    sub.SentBy,
		StartedAt: q.deps.Now(),
		Question:  threadQuestionOf(sub),
	}
	if err := q.recordTurn(ctx, record, log); err != nil {
		log.Error(opDeliver, "could not record the turn before delivering it", dlog.Context{"cause": err.Error()})
		return Disposition{}, fmt.Errorf("record turn %q on %q: %w", sub.Turn, sub.WS, err)
	}

	// THE ACCEPTED PROMPT'S FEED ROW, DRAWN BEFORE StartTurn. The prompt is
	// mirrored to the user the moment it is accepted for delivery, not once
	// the shim answers — a composer that cleared its text has nothing to show
	// for the round trip otherwise. The row carries the same FeedId the main
	// watch's own history entry will carry, so the two upsert onto one row
	// rather than drawing the prompt twice.
	q.mirrorAccepted(sub)

	// THE TURN FACT IS THE DAEMON'S OWN, AND IT IS PUBLISHED ON RECEIPT — before
	// the shim is asked. Nothing on the shim's streams says a turn was accepted —
	// its first frame is an activity, by which time `submitting` is over — so a
	// client left to infer it reads `ready`/idle for a workspace whose turn is
	// starting. Both the footer and the roster take `thinking · submitting` now,
	// each as its own immediate publish, so the STALL of the (blocking) StartTurn
	// below is shown as submitting rather than as idle.
	submitting := &footer.TurnStarted{At: q.deps.Now(), Act: footer.ActPrompt}
	q.deps.Footer.SetTurn(sub.WS, submitting)
	q.deps.Sidebar.SetTurn(sub.WS, submitting)

	// THE WATCHER LEARNS THE TURN BEFORE THE SHIM DOES. The shim can put the
	// turn's frames — its terminal included — on the agent stream before this
	// call returns; a terminal routed while no turn stands in flight is
	// attributable to nothing, and every AwaitTurnEnd on that turn hangs.
	watcher.OnTurnOpening(sub.WS, sub.Turn)
	var success *shimv1.StartTurnSuccess
	var err error
	d.outside(shimCall{what: "start_turn", turn: sub.Turn, opensTurn: true, holds: sub.claims()}, log, func() {
		success, err = q.startTurn(ctx, sender, sub, log)
	})
	if err != nil {
		// A refusal is surfaced to the caller (which answers the rpc with it)
		// rather than swallowed, and the footer and roster drop the submitting
		// turn so no workspace is left showing a `submitting` phase for a turn
		// that never ran.
		q.retireOpenedTurn(sub, watcher)
		if errors.Is(err, ErrShimHasNoSession) || errors.Is(err, ErrShimDied) || errors.Is(err, ErrSessionNotWatched) {
			return q.holdForReconnect(ctx, sub, err, log)
		}
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
	q.ackTurn(sub.WS)
	handOver(sub.WS, success, watcher)
	q.touchEngagement(ctx, sub.WS, log)
	log.Info(opDeliver, "delivered the prompt to the shim", dlog.Context{
		"agent": success.GetPrompt().GetAgent().GetValue(),
	})
}

// handOver gives the watcher a turn the shim accepted: the main agent it
// named, and the turn with its opening page. Shared by a started turn, a
// context cut and a joining prompt.
func handOver(ws ids.WorkspaceID, success *shimv1.StartTurnSuccess, watcher Watcher) {
	if agent := success.GetPrompt().GetAgent(); agent.GetValue() != "" {
		watcher.SetMainAgent(agent)
	}
	watcher.OnTurnOpened(ws, success.GetPrompt(), success.GetPage())
}

// retireOpenedTurn drops the submitting turn a StartTurn opened but the shim
// then refused: the watcher's opening record is retired and the footer and
// roster clear the submitting phase.
func (q *queue) retireOpenedTurn(sub Submission, watcher Watcher) {
	watcher.OnTurnOpenFailed(sub.WS, sub.Turn)
	q.deps.Footer.SetTurn(sub.WS, nil)
	q.deps.Sidebar.SetTurn(sub.WS, nil)
}

// deliverToAgent sends a bubble composer's prompt to the addressed agent, with
// the delivery lock released for the call (call.go).
func (q *queue) deliverToAgent(ctx context.Context, d *delivery, sub Submission, sender Sender, log dlog.Logger) (Disposition, error) {
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
	var err error
	d.outside(shimCall{what: "agent_prompt", turn: sub.Turn, holds: sub.claims()}, log, func() {
		err = sender.PromptAgent(ctx, agent, sub.Said)
	})
	if err != nil {
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

// recordTurn records a turn this queue opens, stamped with the address it
// draws at (turnAddress), and hands that same address to the feed. It is the
// ONE write of a turn's record here and the one place a turn's address is
// chosen, so the live draw (feed.Resolver.AddressTurn) and every replay
// (feed.Deps.TurnAddresses) place the turn from one fact.
func (q *queue) recordTurn(ctx context.Context, t wsm.Turn, log dlog.Logger) error {
	t.Mirror = q.turnMirror(t)
	t.Address = q.turnAddress(t, t.Mirror != nil, log)
	if err := q.deps.DB.PutTurn(ctx, t); err != nil {
		return err
	}
	q.deps.Feed.AddressTurn(t.Workspace, t.ID, t.Address)
	q.deps.Feed.MirrorTurn(t.Workspace, t.ID, t.Mirror)
	q.deps.Feed.SenderTurn(t.Workspace, t.ID, t.SentBy)
	return nil
}

// turnMirror chooses where else a turn draws, BY ITS ORIGIN, exactly as
// turnAddress chooses where it draws: a turn the merge orchestrator starts
// itself takes the mirror the merge stands (an async merge workspace's turn,
// mirrored into the requester's bubble), and every other turn takes none.
func (q *queue) turnMirror(t wsm.Turn) *wsm.MirrorAddress {
	if !startedByMerge(t.Origin) {
		return nil
	}
	return q.deps.Feed.OutputMirror(t.Workspace)
}

// turnAddress chooses where a turn draws, BY ITS ORIGIN. A turn the merge
// orchestrator starts itself (startedByMerge) draws at the output address the
// merge stands at, in its tab; every other turn -- the user's own prompts sent
// while a merge runs, the displaced turn resumed, a vendor-started turn --
// draws on the root feed. A merge turn opened once no address stands (its
// merge already ended) draws on the root feed, and that is said at INFO —
// unless it is MIRRORED: an async merge's workspace draws its merge turns on
// its own root feed by design, and the mirror is where the merge reads them.
func (q *queue) turnAddress(t wsm.Turn, mirrored bool, log dlog.Logger) *wsm.OutputAddress {
	if !startedByMerge(t.Origin) {
		return nil
	}
	addr := q.deps.Feed.OutputAddress(t.Workspace)
	if addr == nil && !mirrored {
		log.Info(opDeliver, "a merge's own turn opened with no merge address standing; it draws on the root feed",
			dlog.Context{"turn": string(t.ID), "origin": t.Origin})
	}
	return addr
}

// mergeStartedOrigins are the origins of the turns the merge orchestrator
// starts in its own tabs: an async merge's pick, a conflict's resolution, a
// suite's fix, and the configured pre and post prompts. MERGE_DISPLACED_TURN_RESUME is not one: it
// is the user's own interrupted turn, put back once the merge has ended.
var mergeStartedOrigins = map[string]bool{
	conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_CONFLICT_REPAIR.String(): true,
	conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_TEST_REPAIR.String():     true,
	conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_BEFORE_ACTION.String():   true,
	conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_AFTER_ACTION.String():    true,
	conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_PICK.String():            true,
}

// startedByMerge reports whether a recorded origin is one the merge
// orchestrator starts its own turns with.
func startedByMerge(origin string) bool {
	return mergeStartedOrigins[origin]
}

// mergeQueueOrigins are the origins of EVERY turn the merge queue causes: the
// turns it starts in its own tabs (mergeStartedOrigins), and the handback that
// tells a detached merge's requester how its merge ended, the compaction a
// landed detached merge asked for after it, and a subscription's notice of a
// queue change. The merge queue's
// own two banners -- put in line, and resolved (merge.Banners) -- are all it
// raises, so none of these turns raises a banner at its end.
var mergeQueueOrigins = func() map[string]bool {
	origins := maps.Clone(mergeStartedOrigins)
	origins[conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_HANDBACK.String()] = true
	origins[conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_COMPACT_AFTER.String()] = true
	origins[conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_QUEUE_NOTICE.String()] = true
	return origins
}()

// causedByMergeQueue reports whether a recorded origin is one of a turn the
// merge queue caused.
func causedByMergeQueue(origin string) bool {
	return mergeQueueOrigins[origin]
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
//
// THE STAMP OUTLIVES ITS CALLER. It follows a delivery that already happened,
// so a submitter that leaves now (a merge suspended mid-step, its run's
// context cut) does not take the stamp with it.
func (q *queue) touchEngagement(ctx context.Context, ws ids.WorkspaceID, log dlog.Logger) {
	if err := q.deps.DB.TouchEngagement(context.WithoutCancel(ctx), ws, q.deps.Now()); err != nil {
		log.Warn(opDeliver, "could not record the session's engagement",
			dlog.Context{"cause": err.Error()})
	}
}

// mirrorAccepted draws an accepted prompt's user_prompt row. The sentinel
// spans are STRIPPED from the drawn text; the full text stays on the record and
// on what the shim received.
func (q *queue) mirrorAccepted(sub Submission) {
	ws, turn, said, origin := sub.WS, sub.Turn, sub.Said, sub.Origin
	if feed.PromptRowHidden(origin) {
		return
	}
	// THE HEADER IS THE FEED'S, read off the sender recordTurn just handed it,
	// so this row and the resolver's own draw of the prompt name one sender.
	row := feed.PromptRow(origin, q.deps.Feed.PromptHeader(ws, turn, origin), said, q.mirrorBlocks(ws, said))
	row.Turn = &conversationv1.TurnId{Value: string(turn)}
	// THE ACCEPTED PROMPT LANDS WHERE ITS TURN DRAWS (recordTurn), never
	// unconditionally on the root feed: a merge's own prompt belongs in its
	// tab and the user's on the root, and the resolver's own draw of the same
	// row key lands there too, so the accepted row and the draw are one row.
	q.deps.Feed.UpsertAtTurnAddress(ws, turn, feedid.RowKey{Kind: feedid.KindPrompt, ID: string(turn)}, row)
}

// retractAccepted takes back the user_prompt row mirrorAccepted drew for TURN,
// whose prompt is held again rather than delivered: the tray draws it now, and
// the feed must not keep showing it as accepted. THE ONE RETRACTION of a
// mirrored row, so every path that takes an accepted prompt back takes back the
// same row.
func (q *queue) retractAccepted(ws ids.WorkspaceID, turn ids.TurnID) {
	q.deps.Feed.OnPromptRetired(ws, &conversationv1.AgentPrompt{Id: &conversationv1.TurnId{Value: string(turn)}})
}

// mirrorBlocks renders a submission's content as drawn blocks, through the
// SAME function the feed resolver draws a replayed user prompt with. A live
// session sees only this mirror -- nothing brings a delivered user prompt
// back on the watch -- so a mirror that drew less than the resolver drew LESS
// THAN THE PERSON SAID, for the whole session. It dropped every image block.
func (q *queue) mirrorBlocks(ws ids.WorkspaceID, said *conversationv1.UserSaid) []*frontendv1.FeedUserPromptBlock {
	return feed.DrawUserBlocks(said.GetContent(), q.deps.StripSentinels, q.deps.ResolveImage,
		q.deps.Feed.PullTarget(ws), q.deps.Log.Global())
}

// interruptionNote is what the agent is told, alone, with a prompt that
// interrupted its running turn (StartTurnRequest.vendor_note). The vendor
// records the cut as the user rejecting the work; this says why it happened,
// so the agent follows the prompt rather than reading it as a rejection.
const interruptionNote = "The work you were doing was interrupted to deliver this message, because the user's message changes that work. " +
	"It is not a rejection of what you did: read the message and carry on as it directs."

// startTurn opens SUB's turn: with the interruption note when the prompt
// interrupted the running turn, as an ordinary start otherwise.
func (q *queue) startTurn(ctx context.Context, sender Sender, sub Submission, log dlog.Logger) (*shimv1.StartTurnSuccess, error) {
	sentBy, err := q.sentByOf(ctx, sub, log)
	if err != nil {
		return nil, err
	}
	if !sub.interjected {
		return sender.StartTurn(ctx, sub.Turn, sub.Said, sub.Origin, sentBy)
	}
	log.Info(opDeliver, "the prompt interrupted the running turn; it is delivered with a note telling the agent why", nil)
	return sender.StartInterjection(ctx, sub.Turn, sub.Said, sub.Origin, sentBy, interruptionNote)
}

// holdForReconnect parks a delivery the SHIM refused because it holds no
// session: the daemon believed the session up, and it was not. The prompt is
// never lost (2026-10-02: one was drawn in the feed and then refused
// `no_session`): the row mirrored on acceptance is taken back down, and the
// prompt waits under the reconnect hold -- its standing hold re-stamped, or a
// new one recorded -- for the session's coming up to deliver it.
func (q *queue) holdForReconnect(ctx context.Context, sub Submission, cause error, log dlog.Logger) (Disposition, error) {
	q.retractAccepted(sub.WS, sub.Turn)
	why := "the shim holds no session; the prompt is held until the session reconnects"
	switch {
	case errors.Is(cause, ErrShimDied):
		why = "the shim died under the delivery; the prompt is held until the revived session comes up"
	case errors.Is(cause, ErrSessionNotWatched):
		why = "no watch stood to see the turn; the prompt is held until the session comes up again"
	}
	log.Info(opDeliver, why, dlog.Context{"cause": cause.Error()})
	if !sub.fromHold {
		return q.hold(ctx, sub, "", &leaseHold{kind: wsm.HoldReconnect}, log)
	}
	kind := wsm.HoldReconnect
	if err := q.deps.DB.UpdateHeldPromptHold(ctx, sub.Turn, &kind, ""); err != nil {
		log.Error(opDeliver, "could not stamp the refused hold to wait for the session to reconnect", dlog.Context{"cause": err.Error()})
		return Disposition{}, fmt.Errorf("hold %q on %q for the reconnect: %w", sub.Turn, sub.WS, err)
	}
	if err := q.pushTray(ctx, sub.WS, log); err != nil {
		return Disposition{}, err
	}
	return Disposition{Held: &kind}, nil
}

// ackTurn tells both status surfaces that the shim took the turn: one call, so
// the footer and the roster leave `submitting` on the same edge.
func (q *queue) ackTurn(ws ids.WorkspaceID) {
	q.deps.Footer.AckTurn(ws)
	q.deps.Sidebar.AckTurn(ws)
	q.deps.Edges.TurnStarted(ws)
}

// setTurn tells both status surfaces the turn the daemon accepted, nil when
// none stands: one call, so the footer, whose status the roster projects, never
// lacks a turn the roster was told of.
func (q *queue) setTurn(ws ids.WorkspaceID, turn *footer.TurnStarted) {
	q.deps.Footer.SetTurn(ws, turn)
	q.deps.Sidebar.SetTurn(ws, turn)
}

// threadQuestionOf is a thread question's record on its turn: the words the
// user typed, as they stand at delivery (an edit while held is already in
// them), and where they asked them from. Nil for every other submission.
func threadQuestionOf(sub Submission) *wsm.ThreadQuestion {
	if sub.Ask == nil {
		return nil
	}
	return &wsm.ThreadQuestion{Text: saidText(sub.Said), Parent: sub.Ask.Parent}
}
