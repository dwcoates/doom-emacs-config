package promptqueue

import (
	"context"
	"errors"
	"fmt"
	"time"

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
	success, err := sender.StartTurn(ctx, sub.Turn, sub.Said, sub.Origin)
	if err != nil {
		// A KEEP-ALIVE COLLISION IS TRANSIENT, NOT A REFUSAL. The shim opens
		// keep-alive turns internally and no daemon queue can see them; a user
		// StartTurn that lands during one is refused through no fault of the
		// daemon's, and the ping closes on its own (in milliseconds normally,
		// but tens of seconds during a vendor 5xx storm). The submitting status
		// STAYS UP and the prompt is re-driven in the background with the SAME
		// idempotency key until the keep-alive closes and the turn starts — the
		// rpc answers `submitting` at once rather than hanging on the retry.
		if isKeepaliveCollision(err) {
			log.Info(opDeliver, "the shim refused the turn behind an in-flight keep-alive; re-driving the prompt",
				dlog.Context{"cause": err.Error()})
			q.redriveBehindKeepalive(context.WithoutCancel(ctx), sub, sender, watcher, log)
			return Disposition{Delivered: true}, nil
		}
		// A non-transient refusal is surfaced to the caller (which answers the
		// rpc with it) rather than swallowed, and the footer and roster drop the
		// submitting turn so no workspace is left showing a `submitting` phase
		// for a turn that never ran.
		q.retireOpenedTurn(sub, watcher)
		log.Error(opDeliver, "the shim refused the turn", dlog.Context{"cause": err.Error()})
		return Disposition{}, fmt.Errorf("start turn %q on %q: %w", sub.Turn, sub.WS, err)
	}

	q.acceptOpenedTurn(ctx, sub, success, watcher, log)
	return Disposition{Delivered: true}, nil
}

// acceptOpenedTurn is the handover a successful StartTurn owes, whether it
// succeeded on the first call or after re-driving behind a keep-alive: the
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
// then refused for a non-transient reason: the watcher's opening record is
// retired and the footer and roster clear the submitting phase.
func (q *queue) retireOpenedTurn(sub Submission, watcher Watcher) {
	watcher.OnTurnOpenFailed(sub.WS, sub.Turn)
	q.deps.Footer.SetTurn(sub.WS, nil)
	q.deps.Sidebar.SetTurn(sub.WS, nil)
}

// keepaliveRedriveMaxAttempts bounds the background re-drive: past it the
// prompt is surfaced as a real error rather than re-driven forever. The
// schedule below sums to comfortably more than the tens of seconds a keep-alive
// stays open through a vendor 5xx storm (each retry_after is ~35-39s and the
// cadence pauses while the ping is open, so at most one ping is ever outlasted),
// so the bound is only reached when the turn is genuinely stuck.
const keepaliveRedriveMaxAttempts = 24

// keepaliveRedriveDelay is the capped backoff before re-drive attempt n
// (1-based): 250ms, 500ms, 1s, 2s, 4s, then 5s thereafter.
func keepaliveRedriveDelay(attempt int) time.Duration {
	const (
		base = 250 * time.Millisecond
		max  = 5 * time.Second
	)
	d := base
	for i := 1; i < attempt; i++ {
		d *= 2
		if d >= max {
			return max
		}
	}
	return d
}

// redriveHandle is the interrupt's handle on one in-flight keep-alive re-drive.
// The re-drive registers it on entry and consumes it on exit; an interrupt
// cancels it. `canceled` and the goroutine's own consume of the handle meet
// under q.mu, so the accept and the cancel commit against each other atomically:
// whichever takes q.mu first at the commit point wins, and the loser observes
// it rather than both acting.
type redriveHandle struct {
	// turn is the turn this re-drive is retrying, so a stale interrupt naming a
	// different turn does not cancel the one that is actually re-driving.
	turn ids.TurnID
	// cancel breaks the re-drive's backoff wait the moment an interrupt lands,
	// rather than letting it sleep out the whole cadence before it notices.
	cancel context.CancelFunc
	// canceled records that an interrupt has claimed this re-drive. The
	// goroutine reads it at the commit point to decide whether to accept the
	// turn or stop it.
	canceled bool
}

// redriveBehindKeepalive re-drives a prompt the shim refused behind an
// in-flight keep-alive turn, OFF the request path. The submitting status raised
// at acceptance is left standing across every retry; it clears only when the
// turn genuinely starts (acceptOpenedTurn), the bound is exhausted
// (failRedrive), or an interrupt cancels the queued turn (closeCancelledRedrive).
// Exactly the SAME idempotency key (sub.Turn) is used, so a re-drive is a retry
// of the one turn, never a second turn.
func (q *queue) redriveBehindKeepalive(ctx context.Context, sub Submission, sender Sender, watcher Watcher, log dlog.Logger) {
	// THE RE-DRIVE'S OWN CANCELLATION IS SEPARATE FROM ITS RETRY DRIVE. `cancelCtx`
	// only breaks the backoff wait when an interrupt lands; the StartTurn calls
	// and the durable closes ride `ctx` (already uncancelable — deliver hands us
	// context.WithoutCancel), so a cancel that lands mid-StartTurn never aborts
	// the shim call or a durable write halfway.
	cancelCtx, cancel := context.WithCancel(ctx)
	q.mu.Lock()
	q.redrives[sub.WS] = &redriveHandle{turn: sub.Turn, cancel: cancel}
	q.mu.Unlock()

	q.redriving.Add(1)
	go func() {
		defer q.redriving.Done()
		defer cancel()
		for attempt := 1; attempt <= keepaliveRedriveMaxAttempts; attempt++ {
			select {
			case <-cancelCtx.Done():
			case <-q.deps.After(keepaliveRedriveDelay(attempt)):
			}
			// An interrupt that landed during the wait cancels the queued turn
			// BEFORE it is started: the user asked for it not to run, and it has
			// not yet reached the shim, so nothing needs killing.
			if q.claimCancelledRedrive(sub.WS, sub.Turn) {
				q.closeCancelledRedrive(ctx, sub, watcher, log)
				return
			}
			success, err := sender.StartTurn(ctx, sub.Turn, sub.Said, sub.Origin)
			if err == nil {
				q.commitRedriveSuccess(ctx, sub, success, sender, watcher, log, attempt)
				return
			}
			if isKeepaliveCollision(err) {
				log.Debug(opDeliver, "the keep-alive is still in flight; will re-drive again",
					dlog.Context{"attempt": attempt})
				continue
			}
			// A DIFFERENT refusal is not transient: surface it rather than
			// re-driving into a wall.
			log.Error(opDeliver, "the re-driven turn was refused for a non-transient reason",
				dlog.Context{"attempt": attempt, "cause": err.Error()})
			q.dropRedrive(sub.WS, sub.Turn)
			q.failRedrive(ctx, sub, watcher, log, err.Error())
			return
		}
		log.Error(opDeliver, "the keep-alive never closed within the re-drive bound; the prompt could not be started",
			dlog.Context{"attempts": keepaliveRedriveMaxAttempts})
		q.dropRedrive(sub.WS, sub.Turn)
		q.failRedrive(ctx, sub, watcher, log, "the keep-alive turn never closed within the re-drive bound")
	}()
}

// commitRedriveSuccess is the re-drive's decision the moment StartTurn opened
// the turn: accept it, UNLESS an interrupt claimed the re-drive first. The
// decision meets CancelKeepaliveRedrive under q.mu (consumeRedrive), so exactly
// one of the two wins. When the interrupt won the race — the turn opened in the
// same instant it was cancelled — the turn is accepted so its terminal is
// attributable and then killed, so a stopped turn does not run on unnoticed.
//
// THE KILL IS UNFORCED. It answers the user's interrupt, and an interrupt ends
// only the synchronous turn: whatever detached work the turn managed to spawn
// runs on until its own per-task stop, exactly as for any other interrupt.
func (q *queue) commitRedriveSuccess(ctx context.Context, sub Submission, success *shimv1.StartTurnSuccess, sender Sender, watcher Watcher, log dlog.Logger, attempt int) {
	if q.consumeRedrive(sub.WS, sub.Turn) {
		log.Info(opDeliver, "the re-driven turn opened as an interrupt cancelled it; stopping it",
			dlog.Context{"attempts": attempt})
		q.acceptOpenedTurn(ctx, sub, success, watcher, log)
		if err := sender.KillTurn(ctx, sub.Turn, false); err != nil {
			log.Error(opDeliver, "could not stop the interrupt-cancelled turn that had just opened",
				dlog.Context{"cause": err.Error()})
		}
		return
	}
	log.Info(opDeliver, "re-drove the prompt once the keep-alive closed",
		dlog.Context{"attempts": attempt})
	q.acceptOpenedTurn(ctx, sub, success, watcher, log)
}

// CancelKeepaliveRedrive cancels a turn re-driving behind a keep-alive. See the
// Queue interface. It reports whether it claimed such a re-drive; the re-drive
// goroutine does the durable close and status clear, so the two never both
// mutate the turn.
func (q *queue) CancelKeepaliveRedrive(_ context.Context, ws ids.WorkspaceID, turn ids.TurnID) bool {
	q.mu.Lock()
	defer q.mu.Unlock()
	h, ok := q.redrives[ws]
	if !ok || h.turn != turn || h.canceled {
		return false
	}
	h.canceled = true
	h.cancel()
	return true
}

// claimCancelledRedrive reports whether an interrupt has claimed this re-drive,
// removing the handle when it has. It is the goroutine's read of the cancel flag
// at a point where the turn has NOT been started this iteration, so a true
// answer means nothing reached the shim.
func (q *queue) claimCancelledRedrive(ws ids.WorkspaceID, turn ids.TurnID) bool {
	q.mu.Lock()
	defer q.mu.Unlock()
	h, ok := q.redrives[ws]
	if !ok || h.turn != turn || !h.canceled {
		return false
	}
	delete(q.redrives, ws)
	return true
}

// consumeRedrive removes this re-drive's handle and reports whether an interrupt
// had claimed it. It is the commit point a successful StartTurn meets
// CancelKeepaliveRedrive at: the handle is gone afterwards either way, so a
// cancel that arrives after it reports false and the interrupt kills the now-open
// turn itself.
func (q *queue) consumeRedrive(ws ids.WorkspaceID, turn ids.TurnID) (canceled bool) {
	q.mu.Lock()
	defer q.mu.Unlock()
	h, ok := q.redrives[ws]
	if !ok || h.turn != turn {
		return false
	}
	delete(q.redrives, ws)
	return h.canceled
}

// dropRedrive removes this re-drive's handle without reading the cancel flag,
// for the exit paths that are not a cancel — a non-transient refusal and the
// exhausted bound.
func (q *queue) dropRedrive(ws ids.WorkspaceID, turn ids.TurnID) {
	q.mu.Lock()
	defer q.mu.Unlock()
	if h, ok := q.redrives[ws]; ok && h.turn == turn {
		delete(q.redrives, ws)
	}
}

// closeCancelledRedrive stamps a turn an interrupt cancelled before it started.
// The rpc that submitted it has already answered `submitting`, so the cancel is
// surfaced by clearing that status and stamping the durable turn KILLED — the
// user's own stop, never a failure and never a silent drop. The turn never
// reached the shim (the race in which it opened as it was cancelled is handled
// by commitRedriveSuccess through the ordinary terminal), so there is nothing to
// kill here.
func (q *queue) closeCancelledRedrive(ctx context.Context, sub Submission, watcher Watcher, log dlog.Logger) {
	q.retireOpenedTurn(sub, watcher)
	// The roster's turn fact is the daemon's own, so the killed close is too.
	q.deps.Sidebar.SetTurnEnded(sub.WS, wsm.CloseKilled)
	if err := q.deps.DB.CloseTurn(ctx, sub.Turn, q.deps.Now(), wsm.CloseKilled); err != nil {
		log.Error(opDeliver, "could not stamp the re-drive's cancelled close", dlog.Context{"cause": err.Error()})
	}
	log.Info(opDeliver, "an interrupt cancelled the queued turn before the keep-alive closed",
		dlog.Context{"turn": string(sub.Turn)})
}

// failRedrive surfaces a re-drive that could not start the turn. The rpc has
// already answered `submitting`, so the failure is surfaced by clearing that
// status and stamping the durable turn FAILED — never by silently dropping it.
func (q *queue) failRedrive(ctx context.Context, sub Submission, watcher Watcher, log dlog.Logger, cause string) {
	q.retireOpenedTurn(sub, watcher)
	// The roster's turn fact is the daemon's own, so the failed close is too.
	q.deps.Sidebar.SetTurnEnded(sub.WS, wsm.CloseFailed)
	if err := q.deps.DB.CloseTurn(ctx, sub.Turn, q.deps.Now(), wsm.CloseFailed); err != nil {
		log.Error(opDeliver, "could not stamp the re-drive's failed close", dlog.Context{"cause": err.Error()})
	}
	log.Error(opDeliver, "the prompt could not be delivered behind the keep-alive", dlog.Context{"cause": cause})
}

// isKeepaliveCollision reports whether a StartTurn refusal is the one the queue
// re-drives: a turn_already_open caused by an in-flight KEEP-ALIVE turn. It is
// matched STRUCTURALLY through a package-local interface so the queue never
// imports internal/workspace, which is where the typed refusal is minted.
func isKeepaliveCollision(err error) bool {
	var collision interface{ KeepaliveTurnAlreadyOpen() bool }
	if errors.As(err, &collision) {
		return collision.KeepaliveTurnAlreadyOpen()
	}
	return false
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
