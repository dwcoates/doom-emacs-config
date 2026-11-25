package workspace

import (
	"context"
	"fmt"
	"strings"

	conversationv1 "agentrepl/proto/conversation/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/resolve/footer"
	"agent-repld/internal/wsm"
)

// Close tears a workspace's editor state down. It REQUIRES QUIET — no turn in
// flight, no live work, no held prompts, no queued merge — because undelivered
// user intent is never silently discarded. A standing cold gate or a parked
// session does NOT block: neither holds anything the user still owes an answer
// to.
//
// A refusal MANIFESTS IN THE FOOTER as well as in the answer, so the user sees
// why the workspace would not close where they are looking.
//
// BY says who asked for the close (the user, an agent, a merge, the daemon),
// and the close record keeps it (wsm.MarkClosed).
//
// A WORKSPACE WITH OPEN CHILDREN IS NOT CLOSED: the close is refused with the
// children named (wsm.HasOpenChildrenError, has_open_children).
//
// Closing leaves the SESSION alone: Close is view-level, Kill is what ends a
// session, and the worktree is untouched either way.
func (v *verbs) Close(ctx context.Context, ws ids.WorkspaceID, by wsm.CloseCause) error {
	return v.close(ctx, ws, by, true)
}

// closeQuietly is Close with no turn interrupted: a turn in flight refuses
// it, as every other quietness blocker does. A standing close request is
// carried out this way (ReconsiderCloseRequest), so a turn that started since
// the request was judged is never cut short by it.
func (v *verbs) closeQuietly(ctx context.Context, ws ids.WorkspaceID, by wsm.CloseCause) error {
	return v.close(ctx, ws, by, false)
}

// close is Close, interrupting a running turn first when INTERRUPT says so.
func (v *verbs) close(ctx context.Context, ws ids.WorkspaceID, by wsm.CloseCause, interrupt bool) error {
	record, log, err := v.owned(ctx, "CloseWorkspace", ws)
	if err != nil {
		return err
	}

	// A RUNNING TURN IS INTERRUPTED AHEAD OF THE CLOSE (owner ruling,
	// 2026-11-24), so the close is never refused for it.
	interrupted := false
	if interrupt {
		interrupted, err = v.interruptBeforeLeaving(ctx, log, ws, opClose, "close")
		if err != nil {
			return err
		}
	}
	if err := v.refuseClose(ctx, log, record, by, interrupted); err != nil {
		return err
	}

	if err := v.deps.DB.MarkClosed(ctx, ws, wsm.Closure{At: v.now(), By: by}); err != nil {
		// A PARENT WITH OPEN CHILDREN STAYS OPEN (wsm/family.go): the refusal
		// is the answer, mapped onto has_open_children. refuseClose read the
		// rule ahead of the write; the write's own transaction is the
		// guarantee, for a child opened in between.
		if refusedByFamily(log, opClose, err) {
			return err
		}
		log.Error(opClose, "could not record the close", dlog.Context{"cause": err.Error()})
		return fmt.Errorf("close %q: %w", ws, err)
	}
	// The refusal the footer may still be drawing is retired: the workspace
	// closed, so nothing is blocking it any more.
	v.deps.Footer.SetClosing(ws, nil)

	// THE CLOSE IS RECORDED BEFORE THE SINKS ARE EVICTED. This logger writes
	// through the workspace's own durable sink, and eviction releases exactly
	// that: recorded afterwards, the one record explaining why the workspace's
	// log ends here would be written into a sink nobody holds any more, and
	// the workspace's log would simply stop mid-sentence.
	log.Info(opClose, "closed the workspace", dlog.Context{"dir": record.Dir})

	// The workspace's durable log sinks are evicted, which releases the shared
	// descriptors the closed workspace no longer writes through; the canonical
	// links and their targets stay on disk. A failed eviction is a LEAK rather
	// than a correctness failure, so it warns instead of refusing a close the
	// record already says happened.
	if err := v.deps.Log.Evict(record.Dir); err != nil {
		log.Warn(opClose, "could not evict the workspace log sinks", dlog.Context{
			"dir": record.Dir, "cause": err.Error(),
		})
	}

	v.republishRegistry(ctx, log, opClose)
	return nil
}

// refuseClose judges EVERY RULE a close of RECORD by BY applies
// (closeWaits), answering the refusal Close would answer, a read that failed
// (recorded at ERROR), or nil when the close may go ahead. OWNTURN discounts
// the turn in flight (blockedClose). The quietness blockers answer first and
// stand in the footer; the family rule answers has_open_children.
//
// IT IS THE ONE JUDGMENT Close, CloseSelf and CloseStanding make, so a close
// is never accepted to stand, or to happen at a turn's end, on a reading the
// close itself would not share.
func (v *verbs) refuseClose(ctx context.Context, log dlog.Logger, record wsm.Workspace, by wsm.CloseCause, ownTurn bool) error {
	waits, err := v.closeWaits(ctx, log, record, by, ownTurn)
	switch {
	case err != nil:
		return err
	case waits.Blocked != nil:
		return v.refuseBlockedClose(log, record.ID, waits.Blocked)
	case waits.Family != nil:
		refusedByFamily(log, opClose, waits.Family)
		return waits.Family
	}
	return nil
}

// isCloseRefusal reports whether ERR, from Close, is a refusal (the close's
// own `blocked` arm or a family rule) rather than a failure.
func isCloseRefusal(err error) bool {
	_, refused := AsRefusal(err)
	return refused || wsm.IsFamilyRefusal(err)
}

// refuseBlockedClose stands BLOCKED in the footer and answers the close's
// `blocked` refusal.
func (v *verbs) refuseBlockedClose(log dlog.Logger, ws ids.WorkspaceID, blocked *footer.CloseBlocked) error {
	v.deps.Footer.SetClosing(ws, blocked)
	// `blocked` IS A LANDED CloseWorkspaceError ARM, so this refusal is an
	// ordinary answer the client reads, not a warning.
	log.Info(opClose, "refused a close that is not quiet", dlog.Context{
		"reason": blocked.Reason, "detail": blocked.Detail,
	})
	// THE EVIDENCE RIDES THE ARM (landing 7): a caller with no footer reads
	// why, and `summary` is the SAME composed sentence the footer's activity
	// line draws -- one composer, never two.
	return &Refusal{
		Rpc:    "CloseWorkspace",
		Arm:    "blocked",
		Reason: blocked.Detail,
		Fields: messageFields(closeBlockedProto(blocked)),
	}
}

// interruptBeforeLeaving interrupts WS's running turn ahead of a close or a
// backlog (owner ruling, 2026-11-24: the workspace's page unloads and its
// shim parks, so a turn left running would run on unseen). It answers whether
// a turn was interrupted; a workspace with none is left as it is. The
// interrupt is confirmed, so live detached agents stop with the turn. OP and
// WHAT name the verb for the record.
func (v *verbs) interruptBeforeLeaving(ctx context.Context, log dlog.Logger, ws ids.WorkspaceID, op, what string) (bool, error) {
	running, live := v.deps.Freeness(ws)
	if !live || running.Turn == nil {
		return false, nil
	}
	if _, err := v.Interrupt(ctx, ws, InterruptTarget{Turn: true}, true); err != nil {
		log.Error(op, "could not interrupt the running turn ahead of the verb; nothing was changed", dlog.Context{
			"verb": what, "turn_id": string(*running.Turn), "cause": err.Error()})
		return false, fmt.Errorf("%s %q: interrupt the running turn: %w", what, ws, err)
	}
	log.Info(op, "interrupted the running turn ahead of the verb", dlog.Context{"verb": what, "turn_id": string(*running.Turn)})
	return true, nil
}

// CloseSelf is a workspace's agent closing its own workspace, and it always
// STANDS (agentrepl.v1.CloseWorkspaceRequest.stand): the asking turn is never
// a blocker, so a workspace with nothing blocking it and no turn running
// closes now (answering false and nil). Otherwise the close is registered as
// the workspace's standing close request (standClose), carried out by the
// standing closes once nothing blocks it: it answers true when the asking
// turn is all it waits on (closes_when_turn_ends), and what else it waits on
// otherwise (stands).
func (v *verbs) CloseSelf(ctx context.Context, ws ids.WorkspaceID) (bool, *CloseWaits, error) {
	record, log, err := v.owned(ctx, "CloseWorkspace", ws)
	if err != nil {
		return false, nil, err
	}
	waits, err := v.closeWaits(ctx, log, record, wsm.CloseByAgent, true)
	if err != nil {
		return false, nil, err
	}
	if waits.None() && !v.turnRunning(ws) {
		return false, nil, v.Close(ctx, ws, wsm.CloseByAgent)
	}
	if err := v.standClose(ctx, log, ws, wsm.CloseByAgent, waits); err != nil {
		return false, nil, err
	}
	if waits.None() {
		log.Info(opClose, "the workspace asked to close itself; it closes when its asking turn ends", nil)
		return true, nil, nil
	}
	return false, &waits, nil
}

// turnRunning reports whether a turn runs in WS now.
func (v *verbs) turnRunning(ws ids.WorkspaceID) bool {
	running, live := v.deps.Freeness(ws)
	return live && running.Turn != nil
}

// BriefOwnedPullRequestsMerged is the notice a workspace gets when the last
// pull request it owns merges.
const BriefOwnedPullRequestsMerged = "workspace-pull-requests-merged"

// TellOwnedPullRequestsMerged tells WS, once, that every pull request it owns
// has merged, and that closing it is its own to do once its work is done
// (owner ruling, 2026-11-16: a workspace is never closed for its merged pull
// requests; it closed torch-human-performance mid-work, its session still
// running turns). A workspace that owns none, owns one not yet merged, is
// already closed, is its repository's main workspace or is a merge workspace
// is told nothing. The notice is the merge's: the poller calls this once,
// at the subscription's end, when the pull request that ended it merged.
func (v *verbs) TellOwnedPullRequestsMerged(ctx context.Context, ws ids.WorkspaceID) error {
	record, log, err := v.owned(ctx, "TellOwnedPullRequestsMerged", ws)
	if err != nil {
		return err
	}
	if record.Closed || record.MergeOf != nil {
		return nil
	}
	main, err := isMainWorkspace(ctx, v.deps.DB, record)
	if err != nil {
		log.Error(opClose, "could not read whether the workspace is its repository's main workspace; it is not told its pull requests merged", dlog.Context{"cause": err.Error()})
		return fmt.Errorf("tell %q its pull requests merged: %w", ws, err)
	}
	if main {
		return nil
	}
	links, err := v.deps.DB.WorkspaceLinks(ctx, ws)
	if err != nil {
		log.Error(opClose, "could not read the workspace's links; it is not told its pull requests merged", dlog.Context{"cause": err.Error()})
		return fmt.Errorf("tell %q its pull requests merged: read the links: %w", ws, err)
	}
	if _, merged := wsm.OwnedPullRequestsMerged(links); !merged {
		log.Info(opClose, "an owned pull request merged, and others the workspace owns have not; it is told nothing more", nil)
		return nil
	}
	var urls []string
	for _, link := range links {
		if link.Owned && link.PullRequest != nil {
			urls = append(urls, link.URL)
		}
	}
	notice, err := v.composeBrief(ctx, log, opClose, BriefOwnedPullRequestsMerged, map[string]string{
		"pull_requests": strings.Join(urls, ", "),
	})
	if err != nil {
		return err
	}
	turn, _, err := v.queueDeferredBrief(ctx, log, opClose, ws, notice, conversationv1.PromptOrigin_PROMPT_ORIGIN_DERIVED_WORKSPACE_NOTICE,
		"the workspace's", "the notice that its pull requests merged")
	if err != nil {
		return err
	}
	log.Info(opClose, "every pull request the workspace owns has merged; it is told so, and closing it is its own", dlog.Context{"turn": string(turn)})
	return nil
}
