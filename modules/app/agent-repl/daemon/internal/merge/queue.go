package merge

import (
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"maps"
	"slices"
	"sort"
	"strings"
	"sync"

	"time"

	frontendv1 "agentrepl/proto/frontend/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/feedid"
	"agent-repld/internal/ids"
	"agent-repld/internal/inflight"
	"agent-repld/internal/resolve/footer"
	"agent-repld/internal/wsm"
)

// This file is the PER-REPO QUEUE: pause and eviction, the queue's publication,
// and the admission pump that starts the front. A request's refusals and its
// wait for the requesting turn's end are request.go's.
//
// The queue is durable in WSM and keyed by the TARGET repository's common dir,
// so a bounce re-enqueues exactly what was waiting in the order it was waiting
// in. The kernel flock is what keeps two daemons from running one repository's
// queue; the durable rows keep the order, and the lock keeps the exclusivity.

// Pause stops the queue from admitting new merges; an in-flight merge runs on.
//
// A nil scope pauses EVERY repository, which is the daemon-wide switch an
// UNSET request ref means. A scope names one repository, whose queue is the
// only one touched. BY is recorded on the pause, with its instant.
func (o *orchestrator) Pause(ctx context.Context, scope *RepositoryScope, by Pauser) error {
	const op = "daemon.merge.pause"
	o.pauseWrites.Lock()
	defer o.pauseWrites.Unlock()
	pause, err := o.pauseRecord(ctx, op, by, false)
	if err != nil {
		return err
	}
	return o.setPaused(ctx, scope, pause)
}

// Unpause resumes admitting merges, scoped exactly as Pause is.
func (o *orchestrator) Unpause(ctx context.Context, scope *RepositoryScope) error {
	o.pauseWrites.Lock()
	defer o.pauseWrites.Unlock()
	return o.setPaused(ctx, scope, nil)
}

// ResumeOwnPause resumes the ONE queue SCOPE names when the pause standing on
// it is workspace WS's own and was made to land a change, and answers whether
// it resumed it. A queue that runs, or stands paused by the user, by the host,
// by another workspace, by a pause written before its pauser's id was kept,
// or by WS's own pause made for any other reason, is left as it stands.
//
// A PAUSE THE OWNER ASKED FOR SURVIVES THE TURN'S END (2026-11-14 20:47:10:
// the owner told merge-queue-stall-triage to pause the queue to save power,
// and the let-through resumed it the moment that turn ended).
func (o *orchestrator) ResumeOwnPause(ctx context.Context, scope RepositoryScope, ws ids.WorkspaceID) (bool, error) {
	const op = "daemon.merge.resume_own_pause"
	o.pauseWrites.Lock()
	defer o.pauseWrites.Unlock()
	repo, err := o.queueKeyOf(ctx, op, "a resume of a workspace's own pause", scope)
	if err != nil {
		return false, err
	}
	pause, err := o.deps.DB.MergeQueuePause(ctx, repo)
	if err != nil {
		o.deps.Log.Global().Error(op, "could not read whose pause stands on the queue, so it is left as it stands", dlog.Context{
			"repo": string(repo), "workspace": string(ws), "error": err.Error()})
		return false, fmt.Errorf("merge: read the pause of %s: %w", repo, err)
	}
	if pause == nil || pause.By.Kind != wsm.MergeQueuePauserWorkspace || pause.By.Workspace != ws {
		fields := dlog.Context{"repo": string(repo), "workspace": string(ws), "paused": pause != nil}
		if pause != nil {
			fields["by_kind"] = int(pause.By.Kind)
			fields["by_name"] = pause.By.Name
			fields["by_workspace"] = string(pause.By.Workspace)
		}
		o.deps.Log.Global().Info(op, "the queue stands no pause of the workspace's own, so it is left as it stands", fields)
		return false, nil
	}
	if !pause.ToLand {
		o.deps.Log.Global().Info(op, "the workspace's own pause was not made to land a change, so it stands until it is resumed", dlog.Context{
			"repo": string(repo), "workspace": string(ws), "hard": pause.Hard})
		return false, nil
	}
	if err := o.setPaused(ctx, &scope, nil); err != nil {
		return false, err
	}
	o.deps.Log.Global().Info(op, "resumed the queue the workspace had paused", dlog.Context{"repo": string(repo), "workspace": string(ws), "hard": pause.Hard})
	return true, nil
}

// pauseRecord is the record a pause BY writes now: who asked, by the asking
// workspace's name as it stands, whether it is a hard stop, and whether it was
// made to land a change. A workspace
// the registry cannot read is refused, recorded under OP.
func (o *orchestrator) pauseRecord(ctx context.Context, op string, by Pauser, hard bool) (*wsm.MergeQueuePause, error) {
	pause := &wsm.MergeQueuePause{At: o.deps.Now(), By: wsm.MergeQueuePauser{Kind: wsm.MergeQueuePauserUser}, Hard: hard, ToLand: by.ToLand}
	if by.By == RequestedByUser {
		return pause, nil
	}
	ws, err := o.deps.DB.Workspace(ctx, by.Asker)
	if err != nil {
		o.deps.Log.Global().Error(op, "could not read the workspace that asked for a pause, so the queue is not paused", dlog.Context{
			"asker": string(by.Asker), "error": err.Error()})
		return nil, fmt.Errorf("merge: read the pausing workspace %s: %w", by.Asker, err)
	}
	pause.By = wsm.MergeQueuePauser{Kind: wsm.MergeQueuePauserWorkspace, Name: ws.Name, Workspace: by.Asker}
	return pause, nil
}

// setPaused is the one pause-write path: PAUSE pauses the scope's queues with
// its record, and nil resumes them. Both refuse a no-op the same way: pausing
// a paused queue is a refusal rather than a quiet success, because the
// operator asked for a change that did not happen.
func (o *orchestrator) setPaused(ctx context.Context, scope *RepositoryScope, pause *wsm.MergeQueuePause) error {
	paused := pause != nil
	op := "daemon.merge.pause"
	if !paused {
		op = "daemon.merge.unpause"
	}
	repos, err := o.pauseScope(ctx, op, scope, paused)
	if err != nil {
		return err
	}
	return o.pauseRepos(ctx, op, repos, pause, scope != nil)
}

// pauseRepos is setPaused's write over the queues of REPOS, resolved: PAUSE
// pauses them with its record, nil resumes them, and a change that would
// change none of them is refused. SCOPED is whether the asker named one
// repository, for the record.
func (o *orchestrator) pauseRepos(ctx context.Context, op string, repos []wsm.RepoKey, pause *wsm.MergeQueuePause, scoped bool) error {
	paused := pause != nil
	var changed []wsm.RepoKey
	for _, repo := range repos {
		was, err := o.deps.DB.MergeQueuePaused(ctx, repo)
		if err != nil {
			return err
		}
		if was == paused {
			continue
		}
		if err := o.deps.DB.SetMergeQueuePaused(ctx, repo, pause); err != nil {
			return err
		}
		changed = append(changed, repo)
	}
	if len(changed) == 0 {
		return o.refuseNoopPause(op, paused, "refused a pause change that would not change anything", "the merge queue is not paused", "the merge queue is already paused")
	}
	o.deps.Log.Global().Debug(op, "changed the merge queue's pause state", dlog.Context{"paused": paused, "repos": len(changed), "scoped": scoped})
	if paused {
		suspended := o.suspendHeld(ctx, repos, suspendPause)
		return errors.Join(suspended, o.redrawPaused(ctx, op, changed))
	}
	redrawn := o.redrawPaused(ctx, op, changed)
	for _, repo := range repos {
		o.kick(repo)
	}
	return redrawn
}

// holdForHostFault pauses REPO's queue in the host's name, naming CAUSE: a
// merge's test gate whose runner could not be built or started. The pause
// suspends the merge whose gate it was, as any pause does, and stands until
// it is resumed; the merge's gate then runs again from its start, with no
// fixing attempt spent. A queue already paused is left as it stands: its
// pause suspends the merge the same way.
func (o *orchestrator) holdForHostFault(ctx context.Context, repo wsm.RepoKey, cause string) error {
	o.pauseWrites.Lock()
	defer o.pauseWrites.Unlock()
	pause := &wsm.MergeQueuePause{At: o.deps.Now(), By: wsm.MergeQueuePauser{Kind: wsm.MergeQueuePauserHost, Cause: cause}}
	return o.pauseRepos(ctx, "daemon.merge.pause", []wsm.RepoKey{repo}, pause, true)
}

// HardStop pauses the queues SCOPE names as Pause does, and stops the work
// their merges are doing now (agentrepl.v1.UpdateMergeQueuePause.hard).
//
// A PAUSE LEAVES A STEP'S TURN RUNNING. It suspends each merge at the step
// it stands on -- its git stopped, its test gate's run ended with the run's
// context -- but the agent turn a fixing attempt or a conflict resolution
// drives goes on in its session, test runs and all (2026-10-26: a fixing
// attempt's full test run went on for minutes after a pause). A hard stop
// interrupts those turns, and the turn of every waiting merge's background
// rebase. The turn of any other step (a pick, a deploy-stage prompt) is left
// to finish: those steps read an interrupted turn as the user's.
//
// A queue already paused is not refused: the hard stop stops what the pause
// left running, and its pause is recorded anew as a hard stop BY its asker.
// Every turn is attempted; the failures are answered joined.
func (o *orchestrator) HardStop(ctx context.Context, scope *RepositoryScope, by Pauser) error {
	const op = "daemon.merge.hard_stop"
	o.pauseWrites.Lock()
	defer o.pauseWrites.Unlock()
	repos, err := o.pauseScope(ctx, op, scope, true)
	if err != nil {
		return err
	}
	pause, err := o.pauseRecord(ctx, op, by, true)
	if err != nil {
		return err
	}
	background := o.backgroundRunsIn(repos)
	var errs []error
	unpaused := false
	var alreadyPaused []wsm.RepoKey
	for _, repo := range repos {
		paused, err := o.deps.DB.MergeQueuePaused(ctx, repo)
		if err != nil {
			return fmt.Errorf("merge: hard stop: read whether %s is paused: %w", repo, err)
		}
		unpaused = unpaused || !paused
		if paused {
			alreadyPaused = append(alreadyPaused, repo)
		}
	}
	for _, repo := range alreadyPaused {
		if err := o.deps.DB.SetMergeQueuePaused(ctx, repo, pause); err != nil {
			o.deps.Log.Global().Error(op, "could not record a paused queue's pause as a hard stop", dlog.Context{
				"repo": string(repo), "error": err.Error()})
			errs = append(errs, fmt.Errorf("merge: hard stop: record the hard stop of %s: %w", repo, err))
		}
	}
	if unpaused {
		if err := o.setPaused(ctx, scope, pause); err != nil {
			errs = append(errs, err)
		}
	} else {
		o.deps.Log.Global().Info(op, "the queue is already paused; the hard stop stops the work the pause left running", dlog.Context{"repos": len(repos)})
	}
	for _, repo := range repos {
		stored, err := o.deps.DB.MergeQueue(ctx, repo)
		if err != nil {
			o.deps.Log.Global().Error(op, "could not read the queue whose merges' turns the hard stop interrupts", dlog.Context{
				"repo": string(repo), "error": err.Error()})
			errs = append(errs, fmt.Errorf("merge: hard stop: read the queue of %s: %w", repo, err))
			continue
		}
		for _, entry := range inLine(stored) {
			if err := o.stopRecordedTurn(ctx, op, entry.Workspace); err != nil {
				errs = append(errs, err)
			}
		}
	}
	for _, ws := range background {
		if err := o.interruptFor(ctx, op, ws, "a waiting merge's background rebase"); err != nil {
			errs = append(errs, err)
		}
	}
	o.deps.Log.Global().Info(op, "hard-stopped the merge queue", dlog.Context{
		"repos": len(repos), "background_rebases": len(background), "scoped": scope != nil, "failures": len(errs)})
	return errors.Join(errs...)
}

// backgroundRunsIn answers the workspaces whose waiting merge's background
// rebase runs now in one of REPOS, sorted.
func (o *orchestrator) backgroundRunsIn(repos []wsm.RepoKey) []ids.WorkspaceID {
	in := map[wsm.RepoKey]bool{}
	for _, repo := range repos {
		in[repo] = true
	}
	o.mu.Lock()
	var out []ids.WorkspaceID
	for ws, r := range o.runsByWorkspace {
		if r.background && in[r.repo] {
			out = append(out, ws)
		}
	}
	o.mu.Unlock()
	sort.Slice(out, func(i, j int) bool { return out[i] < out[j] })
	return out
}

// stopRecordedTurn interrupts the turn WS's suspended merge recorded, when it
// stands at a fixing attempt or a conflict resolution and that turn has not
// ended, writing it down as the turn the hard stop interrupted FIRST, so the
// resume reads its kill as the queue's.
func (o *orchestrator) stopRecordedTurn(ctx context.Context, op string, ws ids.WorkspaceID) error {
	fields := dlog.Context{"workspace": string(ws)}
	lease, held, err := o.deps.DB.Lease(ctx, ws)
	if err != nil {
		o.deps.Log.Global().Error(op, "could not read the lease of a merge the hard stop reaches", withField(fields, "error", err.Error()))
		return fmt.Errorf("merge: hard stop: read the lease of %q: %w", ws, err)
	}
	doc, recorded, err := o.loadProgress(ctx, ws, lease, held)
	if err != nil {
		return err
	}
	if !recorded || doc.Turn == "" {
		o.deps.Log.Global().Debug(op, "the merge records no turn; there is nothing of its to interrupt", fields)
		return nil
	}
	fields["step"], fields["turn"] = doc.Step, doc.Turn
	if doc.Step != TabFixes && doc.Step != TabConflicts {
		o.deps.Log.Global().Info(op, "the merge's turn belongs to a step that reads an interrupt as the user's; it is left to finish", fields)
		return nil
	}
	turn := ids.TurnID(doc.Turn)
	closes, err := o.deps.DB.TurnCloses(ctx, ws, []ids.TurnID{turn})
	if err != nil {
		o.deps.Log.Global().Error(op, "could not read whether the merge's turn ended", withField(fields, "error", err.Error()))
		return fmt.Errorf("merge: hard stop: read the close of %q's turn: %w", ws, err)
	}
	if _, closed := closes[turn]; closed {
		o.deps.Log.Global().Debug(op, "the merge's turn already ended; there is nothing to interrupt", fields)
		return nil
	}
	doc.StoppedTurn = doc.Turn
	encoded, err := json.Marshal(doc)
	if err != nil {
		return fmt.Errorf("merge: hard stop: encode the progress of %q: %w", ws, err)
	}
	if err := o.deps.DB.PutMergeProgress(ctx, wsm.MergeProgress{
		Workspace: ws, Lease: lease.ID, UpdatedAt: o.deps.Now(), Document: encoded,
	}); err != nil {
		o.deps.Log.Global().Error(op, "could not record the turn the hard stop interrupts; it is not interrupted, so its resume cannot read the kill as the user's",
			withField(fields, "error", err.Error()))
		return fmt.Errorf("merge: hard stop: record the stopped turn of %q: %w", ws, err)
	}
	return o.interruptFor(ctx, op, ws, "a merge's "+doc.Step+" turn")
}

// interruptFor interrupts the turn running in WS's session for the hard stop;
// WHAT names the turn in the records.
func (o *orchestrator) interruptFor(ctx context.Context, op string, ws ids.WorkspaceID, what string) error {
	fields := dlog.Context{"workspace": string(ws), "what": what}
	running, err := o.deps.InterruptTurn(ctx, ws)
	if err != nil {
		o.log(ctx, ws).Error(op, "the hard stop could not interrupt the turn", withField(fields, "error", err.Error()))
		return fmt.Errorf("merge: hard stop: interrupt %q: %w", ws, err)
	}
	o.log(ctx, ws).Info(op, "the hard stop interrupted the turn", withField(fields, "running", running))
	return nil
}

// redrawPaused redraws every bubble on each of REPOS' queues once its pause
// changed, so each head's paused badge and each queue tab's banner follow the
// pause at once (frontend.v1.FeedMergeUpdate.paused,
// frontend.v1.FeedMergeQueue.paused). A queue that cannot be redrawn is
// recorded under OP and the rest are still redrawn; the errors are answered
// joined.
func (o *orchestrator) redrawPaused(ctx context.Context, op string, repos []wsm.RepoKey) error {
	var errs []error
	for _, repo := range repos {
		if err := o.republishQueue(ctx, repo); err != nil {
			o.deps.Log.Global().Error(op, "could not redraw the merge queue's bubbles after its pause changed", dlog.Context{
				"repo": string(repo), "error": err.Error()})
			errs = append(errs, err)
		}
	}
	return errors.Join(errs...)
}

// pauseScope resolves WHICH queues one pause or resume addresses: every
// repository that has a queue for a nil scope, and exactly the named
// repository's queue for a set one. A scope naming a repository the registry
// does not hold is REFUSED rather than silently pausing a key nobody owns.
func (o *orchestrator) pauseScope(ctx context.Context, op string, scope *RepositoryScope, paused bool) ([]wsm.RepoKey, error) {
	if scope != nil {
		repo, err := o.queueKeyOf(ctx, op, "a pause change", *scope)
		if err != nil {
			return nil, err
		}
		return []wsm.RepoKey{repo}, nil
	}
	queues, err := o.deps.DB.AllMergeQueues(ctx)
	if err != nil {
		return nil, err
	}
	keys := make([]wsm.RepoKey, 0, len(queues))
	for repo := range queues {
		keys = append(keys, repo)
	}
	if len(keys) == 0 {
		return nil, o.refuseNoopPause(op, paused, "refused a pause change with no queue to change", "no repository has a merge queue to resume", "no repository has a merge queue to pause")
	}
	return keys, nil
}

// queueKeyOf resolves the queue key of the ONE repository SCOPE names: the
// registered repository its ref matches, keyed by its common dir as every
// queue is. It is the one resolution every request scoped to a repository's
// queue makes (a pause or a resume, a subscription), so they cannot key one
// repository two ways. A ref naming nothing, or no registered repository, is
// the unknown_repository refusal, recorded under OP as WHAT was refused.
func (o *orchestrator) queueKeyOf(ctx context.Context, op, what string, scope RepositoryScope) (wsm.RepoKey, error) {
	_, key, err := o.scopedRepository(ctx, op, what, scope)
	return key, err
}

// scopedRepository is queueKeyOf's resolution, answering the registered
// repository SCOPE names as well as its queue key.
func (o *orchestrator) scopedRepository(ctx context.Context, op, what string, scope RepositoryScope) (wsm.Repository, wsm.RepoKey, error) {
	if scope.ID == "" && scope.Dir == "" {
		o.deps.Log.Global().Warn(op, "refused "+what+" scoped by an empty repository ref", dlog.Context{"arm": ArmUnknownRepository})
		return wsm.Repository{}, "", &RefusalError{Arm: ArmUnknownRepository, Reason: "the request's repository ref names neither an id nor a dir"}
	}
	repos, err := o.deps.DB.ListRepositories(ctx)
	if err != nil {
		return wsm.Repository{}, "", err
	}
	if repo, found := wsm.RepositoryNamed(repos, scope.ID, scope.Dir); found {
		// THE QUEUE IS KEYED BY THE COMMON DIR (repoKeyFor), never by the
		// registry's worktree dir: keying a scoped pause by repo.Dir wrote
		// and read a key no queue is ever stored under, so a scoped pause
		// never saw what an unscoped one had done.
		common, err := o.deps.Git.CommonDir(ctx, repo.Dir)
		if err != nil {
			return wsm.Repository{}, "", fmt.Errorf("merge: resolving the repository of %s: %w", repo.Dir, err)
		}
		return repo, wsm.RepoKey(common), nil
	}
	o.deps.Log.Global().Warn(op, "refused "+what+" for a repository the registry does not hold", dlog.Context{"arm": ArmUnknownRepository, "repository": string(scope.ID), "dir": scope.Dir})
	return wsm.Repository{}, "", &RefusalError{Arm: ArmUnknownRepository, Reason: "no registered repository matches the request's repository ref"}
}

// AbandonCause names WHY a queued merge left its queue before it ever ran.
// `FeedMergeError` carries ONE `abandoned` arm for every one of them, so the
// cause reaches a reader ONLY as `FeedMergeAbandoned.summary` — which is why
// every cause is declared here, beside the one sentence it resolves to, rather
// than spelled at each call site. The value is also the durable drop reason
// `RemoveMergeQueueEntry` records, so the queue row and the bubble agree.
type AbandonCause string

const (
	// CauseUserDrop is the user taking a queued merge off the queue through
	// UpdateMergeQueue's evict.
	CauseUserDrop AbandonCause = "evicted"
	// CauseUserDequeue is the user releasing the slot in answer to the
	// interrupt's dequeue offer.
	CauseUserDequeue AbandonCause = "dequeued"
	// CauseWorkspaceClosed is the workspace itself being torn down — killed or
	// nuked — while its merge was still waiting.
	CauseWorkspaceClosed AbandonCause = wsm.MergeLeftWorkspaceClosed
	// CauseDaemonShutdown is the daemon going away under a waiting merge: the
	// queue is durable, so this is only reached at the RESTORE, for a merge the
	// restart could not put back on its queue.
	CauseDaemonShutdown AbandonCause = "daemon_shutdown"
	// CauseAheadDidNotLand is the merge this one was placed behind leaving the
	// queue without landing (wsm.MergeQueueEntry.AheadFailed): a merge placed
	// behind another depends on it, so it never runs without it.
	CauseAheadDidNotLand AbandonCause = "ahead_did_not_land"
	// CauseAgentGaveUp is the merge's own agent giving it up from a repair
	// turn the queue gave it (GiveUp).
	CauseAgentGaveUp AbandonCause = "gave_up"
	// CauseColdGate is a merge being worked on whose workspace's session
	// parked at its cold gate. It is NO END of the merge: it ends the run and
	// its bubble, and puts the merge back at its place in line, skipped until
	// the gate is passed (coldgate.go, requeues).
	CauseColdGate AbandonCause = "cold_gate"
	// CauseHiddenAtColdGate is the user hiding a workspace whose merge stood
	// at its cold gate: hiding passes the gate without an answer, and takes
	// the merge off its queue.
	CauseHiddenAtColdGate AbandonCause = "hidden_at_cold_gate"
)

// The causes a queue entry leaves its queue for that are NOT abandonments:
// the durable drop reason RemoveMergeQueueEntry records for a merge that ran
// to its end, a merge the boot recovery failed or put back in line, and an
// async merge that could not be handed to its merge workspace or prepared
// there. Each is spelled once here, so a reader of the cause (the reports of
// a queue's changes) cannot disagree with the writer.
const (
	// causeMerged is a merge that landed.
	causeMerged = wsm.MergeLeftLanded
	// causeFailed is a merge that ran and gave up.
	causeFailed = "failed"
	// causeRestartFailed is a merge a restart left unfinished that could not
	// run again.
	causeRestartFailed = "restart_failed"
	// causeRestartResume is a merge a restart left unfinished, taken out of
	// line only to be put straight back in it, at the back, to run again.
	causeRestartResume = wsm.MergeLeftToReturn
	// causePickFailed is an async merge whose preparing pick failed.
	causePickFailed = "pick_failed"
	// causeDetachedForkFailed is an async request that could not be handed
	// to a merge workspace.
	causeDetachedForkFailed = "detached_fork_failed"
	// causeFinishFailed is a deploy whose finish stage failed before it was
	// put in line (finish.go).
	causeFinishFailed = "finish_failed"
)

// abandonSummaries is the resolved sentence each cause draws as the bubble's
// collapsed line, exactly as `FeedMergeFailed.summary` is drawn.
var abandonSummaries = map[AbandonCause]string{
	CauseUserDrop:         "the operator evicted this merge from the queue",
	CauseUserDequeue:      "the user released this merge's queue slot",
	CauseWorkspaceClosed:  "the workspace was closed while this merge was waiting in the queue",
	CauseDaemonShutdown:   "the daemon shut down while this merge was waiting in the queue, and the restart could not put it back",
	CauseAheadDidNotLand:  "the merge this one was placed behind left the queue without landing",
	CauseAgentGaveUp:      "the merge's own agent gave it up",
	CauseColdGate:         "the workspace's session parked at its cold gate; the merge waits at its place in line until the gate is passed",
	CauseHiddenAtColdGate: "the workspace was hidden at its cold gate while this merge was waiting in the queue",
}

// runningSummaries are the resolved sentences of a merge abandoned AFTER it
// was admitted. They are separate from abandonSummaries because those say
// "while this merge was waiting in the queue", which is false of a merge that
// ran (2026-09-28: "a merge left the queue without running" was logged for one
// that had run three repair rounds).
var runningSummaries = map[AbandonCause]string{
	CauseUserDrop:         "the operator evicted this merge while it was running",
	CauseUserDequeue:      "the user took this merge out of the queue while it was running",
	CauseWorkspaceClosed:  "the workspace was closed while this merge was running",
	CauseAgentGaveUp:      "the merge's own agent gave it up from its repair turn",
	CauseColdGate:         "the workspace's session parked at its cold gate while this merge was running; it waits at its place in line until the gate is passed, and runs again from its start",
	CauseHiddenAtColdGate: "the workspace was hidden at its cold gate while this merge was running",
}

// releases reports whether C is a RELEASE the user or the operator asked for
// (an evict, a dequeue), as against a merge given up under something else
// ending (its workspace closing, the daemon shutting down). A released merge's
// bubble is closed (closeReleasedBubble).
func (c AbandonCause) releases() bool {
	return c == CauseUserDrop || c == CauseUserDequeue || c == CauseHiddenAtColdGate
}

// requeues reports whether C puts the merge back at its place in line rather
// than ending it (CauseColdGate): its run and its bubble end, its entry stays.
func (c AbandonCause) requeues() bool {
	return c == CauseColdGate
}

// summaryRunning resolves the sentence of a merge abandoned while it ran.
func (c AbandonCause) summaryRunning() (string, bool) {
	sentence, declared := runningSummaries[c]
	return sentence, declared
}

// summary resolves one cause's sentence. The bool is false for a cause with no
// declared sentence, which is a programming error rather than an outcome: the
// caller reports it and still ends the bubble, because a reader losing the
// terminal entirely is worse than reading an unpolished one.
func (c AbandonCause) summary() (string, bool) {
	sentence, declared := abandonSummaries[c]
	return sentence, declared
}

// Evict removes a queued workspace from the queue. It is one of the FOUR
// DISTINCT ENDS a merge can have before it runs — evict is the user's own
// drop, dequeue is the user's answer to the interrupt offer, and the workspace
// close and the daemon shutdown are the merge's give-up under something else
// ending — and each records its own cause.
func (o *orchestrator) Evict(ctx context.Context, ws ids.WorkspaceID) error {
	return o.dequeue(ctx, ws, CauseUserDrop)
}

// Move moves a workspace's waiting merge one place up or down its queue, and
// redraws every bubble on that queue when the order changed.
func (o *orchestrator) Move(ctx context.Context, ws ids.WorkspaceID, move wsm.MergeMove) (bool, error) {
	return o.reorder(ctx, ws, reordering{
		op:      "daemon.merge.move",
		fields:  dlog.Context{"move": move.String()},
		failed:  "could not move a merge in its queue",
		unmoved: "a merge asked to move has no waiting merge to pass on that side, and stays where it is",
		moved:   "moved a merge one place in its queue",
		do: func(repo wsm.RepoKey) (bool, error) {
			return o.deps.DB.MoveMerge(ctx, repo, ws, move)
		},
	})
}

// MoveTo moves WS's waiting merge directly to SIDE of ANCHOR's waiting merge
// on the same queue (agentrepl.v1.UpdateMergeQueueMoveTo).
func (o *orchestrator) MoveTo(ctx context.Context, ws, anchor ids.WorkspaceID, side wsm.MergeSide) (bool, error) {
	return o.reorder(ctx, ws, reordering{
		op:      "daemon.merge.move_to",
		fields:  dlog.Context{"anchor": string(anchor), "side": side.String()},
		failed:  "could not move a merge to a place in its queue",
		unmoved: "a merge asked to move to a place already stands there, or it or its anchor is not waiting, and it stays where it is",
		moved:   "moved a merge to a place in its queue",
		do: func(repo wsm.RepoKey) (bool, error) {
			return o.deps.DB.MoveMergeTo(ctx, repo, ws, anchor, side)
		},
	})
}

// reordering is one way a waiting merge's place in its queue is changed: the
// store's reorder, and the records each of its outcomes leaves.
type reordering struct {
	op string
	// fields are the reordering's own context, beside the workspace and the
	// repository every record carries.
	fields  dlog.Context
	failed  string
	unmoved string
	moved   string
	do      func(repo wsm.RepoKey) (bool, error)
}

// reorder runs one reordering of WS's waiting merge, answering whether it
// moved, and redraws every bubble on that queue when the order changed. A
// store failure is recorded at ERROR here, once; a workspace with nothing
// queued is the queue's refusal, answered as it is.
func (o *orchestrator) reorder(ctx context.Context, ws ids.WorkspaceID, r reordering) (bool, error) {
	log := o.log(ctx, ws)
	repo, err := o.queueOf(ctx, ws)
	if err != nil {
		return false, err
	}
	fields := dlog.Context{"workspace": string(ws), "repo": string(repo)}
	maps.Copy(fields, r.fields)
	moved, err := r.do(repo)
	if err != nil {
		fields["error"] = err.Error()
		log.Error(r.op, r.failed, fields)
		return false, err
	}
	if !moved {
		log.Info(r.op, r.unmoved, fields)
		return false, nil
	}
	log.Info(r.op, r.moved, fields)
	return true, o.republishQueue(ctx, repo)
}

// dequeue takes one workspace's merge out, whatever it is doing. A merge still
// WAITING (or only requested) leaves its queue; a merge that is RUNNING is
// ABANDONED through the one release path. Before this a dequeue only ever dropped the queue
// row, so a running merge's lease, queue entry and repository lock outlived
// it and blocked its repository's queue until the daemon restarted
// (2026-09-28, lease c8a3a664006f46c1).
//
// AN ABANDONMENT AND A SUSPENSION ARE DECIDED UNDER ONE LOCK, the one every
// suspension marks its run under (suspendLocked, suspendFor): whichever is
// decided first stands. An abandonment decided first is the run's end, and no
// later suspension but the daemon's exit takes the run (suspendFor,
// stopAtStepBoundary, claimRace). A suspension decided first stands too, and
// the abandonment is left for its take-back (AdoptWorkspace), which drops the
// merge instead of resuming it -- unless the suspension hands the merge to
// another daemon or the next boot, which owns it from then on, so the
// abandonment is refused.
func (o *orchestrator) dequeue(ctx context.Context, ws ids.WorkspaceID, cause AbandonCause) error {
	const op = "daemon.merge.abandon"
	o.mu.Lock()
	r, running := o.runsByWorkspace[ws]
	away, takenAway := o.takenAway[ws]
	switch {
	case running && !r.suspending:
		if r.abandonedBy == "" {
			r.abandonedBy = cause
		}
		o.mu.Unlock()
		return o.abandonRunning(ctx, r, cause)
	case running || takenAway:
		why := away
		if running {
			why = r.suspendedBy
		}
		if !why.takenBack() {
			o.mu.Unlock()
			err := fmt.Errorf("merge: the merge of %s was taken by %s, which owns it from here; it is abandoned there", ws, why.by)
			o.log(ctx, ws).Info(op, "refused to abandon a merge a suspension handed on; it is abandoned where it went", dlog.Context{
				"workspace": string(ws), "cause": string(cause), "by": why.by, "error": err.Error()})
			return err
		}
		if !takenAway {
			o.mu.Unlock()
			panic(fmt.Sprintf("merge: the run of %s is suspended by %s with no take-back recorded", ws, why.by))
		}
		if _, recorded := o.abandonsAtTakeBack[ws]; !recorded {
			o.abandonsAtTakeBack[ws] = cause
		}
		o.mu.Unlock()
		o.log(ctx, ws).Info(op, "a suspension took the merge first; the abandonment ends it at its take-back", dlog.Context{
			"workspace": string(ws), "cause": string(cause), "by": why.by})
		return nil
	}
	// A MERGE A RESTART TOOK BACK BUT HAS NOT RESUMED is admitted, holds its
	// merge lease and has no run: it ends as an admitted merge, never as a
	// waiting one, or its lease would outlive its entry (dropPendingResume).
	doc, pending := o.resumes[ws]
	delete(o.resumes, ws)
	o.forgetRemediationLocked(ws)
	o.mu.Unlock()
	if pending {
		return o.dropPendingResume(ctx, ws, doc, cause)
	}
	return o.dropQueued(ctx, ws, cause)
}

// dropPendingResume ends a merge the boot recovery or a handover took back
// and that has not resumed yet, the caller having taken it out of o.resumes:
// its queue entry, its merge lease and its progress record go in one store
// transaction (wsm.DropAdmittedMerge, the owner of "a merge lease stands
// with exactly one queue entry"), its scratch trees are swept, and its bubble
// ends on the abandoned terminal a running merge reads. A drop that fails
// changed nothing: the merge is put back to resume as it was.
func (o *orchestrator) dropPendingResume(ctx context.Context, ws ids.WorkspaceID, doc progressDoc, cause AbandonCause) error {
	const op = "daemon.merge.drop_pending_resume"
	log := o.log(ctx, ws)
	repo := wsm.RepoKey(doc.Repo)
	o.mu.Lock()
	lease, known := o.ledgerOf[ws]
	o.mu.Unlock()
	if !known {
		panic(fmt.Sprintf("merge: the merge of %s was taken back for its resume with no lease recorded", ws))
	}
	fields := dlog.Context{"workspace": string(ws), "repo": string(repo), "lease": string(lease), "cause": string(cause), "step": doc.Step}
	putBack := func() {
		o.mu.Lock()
		o.resumes[ws] = doc
		o.mu.Unlock()
	}
	entry, err := o.entryOf(ctx, repo, ws)
	if err != nil {
		putBack()
		log.Error(op, "could not read the queue entry of a merge taken back for its resume; it is left to resume", withField(fields, "error", err.Error()))
		return err
	}
	if cause.requeues() {
		return o.requeuePendingResume(ctx, ws, repo, lease, doc, entry, cause, fields, putBack)
	}
	if err := o.deps.DB.DropAdmittedMerge(ctx, repo, ws, lease, string(cause)); err != nil {
		putBack()
		log.Error(op, "could not drop a merge taken back for its resume; it is left to resume", withField(fields, "error", err.Error()))
		return err
	}
	o.deps.Queue.OnLeaseChanged(ws)
	o.sweepTrees(ctx, ws, doc.Subject.TargetDir, lease, true)
	o.mu.Lock()
	delete(o.repoOf, ws)
	delete(o.ledgerOf, ws)
	o.mu.Unlock()
	summary := o.abandonedSentence(ws, cause, true)
	log.Info(op, "dropped a merge a restart took back before it resumed: its entry, its lease and its progress record together", fields)
	o.publishAbandonedWith(ctx, ws, lease, entry, cause, summary)
	o.clearOffer(ws)
	if err := o.republishQueue(ctx, repo); err != nil {
		return err
	}
	o.kick(repo)
	return nil
}

// requeuePendingResume puts a merge a restart took back, and that has not
// resumed, back at its place in line under CAUSE (AbandonCause.requeues):
// its merge lease and progress record go and its entry waits, marked, in ONE
// store transaction (wsm.RequeueColdGatedMerge), its scratch trees are swept,
// and its bubble ends, closed, for the new bubble it stands under from here.
// A write that fails changed nothing: the merge is put back to resume.
func (o *orchestrator) requeuePendingResume(ctx context.Context, ws ids.WorkspaceID, repo wsm.RepoKey, lease ids.LeaseID, doc progressDoc,
	entry wsm.MergeQueueEntry, cause AbandonCause, fields dlog.Context, putBack func()) error {
	const op = "daemon.merge.drop_pending_resume"
	log := o.log(ctx, ws)
	ledger := ids.LeaseID(wsm.NewLeaseID())
	if err := o.lineWrite(repo, func() error {
		return o.deps.DB.RequeueColdGatedMerge(ctx, repo, ws, lease, ledger)
	}); err != nil {
		putBack()
		log.Error(op, "could not put a merge taken back for its resume back at its place in line; it is left to resume", withField(fields, "error", err.Error()))
		return err
	}
	o.deps.Queue.OnLeaseChanged(ws)
	o.sweepTrees(ctx, ws, doc.Subject.TargetDir, lease, true)
	o.mu.Lock()
	o.ledgerOf[ws] = ledger
	o.mu.Unlock()
	log.Info(op, "put a merge a restart took back before it resumed back at its place in line: its lease and progress record go, its entry waits", withField(fields, "ledger", string(ledger)))
	o.endAbandonedBubble(ctx, ws, lease, entry, cause, o.abandonedSentence(ws, cause, true))
	if err := o.republishQueue(ctx, repo); err != nil {
		return err
	}
	o.kick(repo)
	return nil
}

// OnWorkspaceClosed abandons a workspace's WAITING merge when the workspace
// itself is torn down. A queued merge whose workspace is killed or nuked can
// never run, and left on the queue it would block the repository's queue
// forever on a workspace that is not there; abandoning it records WHY in the
// bubble instead. Nothing is dropped when the merge has already been admitted:
// a run in flight ends on its own terminal, not on this one. The workspace's
// async requests not yet handed off are dropped with it: their fork would hold
// a conversation that has ended for good.
func (o *orchestrator) OnWorkspaceClosed(ctx context.Context, ws ids.WorkspaceID) {
	const op = "daemon.merge.workspace_closed"
	// THE CLOSE ENDED ANY REQUEST THE WORKSPACE HELD OFF ITS QUEUE
	// (wsm.markClosedInTx), so the roster stops drawing it.
	o.publishOffQueue(ctx)
	o.dropAsyncRequestsOf(ctx, ws)
	// A REMEDIATION CLOSED BEFORE ITS FIX DEPLOYS HANDS ITS HOLD BACK.
	o.remediationClosed(ctx, ws)
	if _, running := o.runFor(ws); running {
		// A RUNNING MERGE ENDS WITH ITS WORKSPACE TOO. Left alone, a merge
		// running on a workspace that is gone would hold its lease for ever.
		if err := o.dequeue(ctx, ws, CauseWorkspaceClosed); err != nil {
			o.log(ctx, ws).Error(op, "could not abandon the running merge of a torn-down workspace",
				dlog.Context{"workspace": string(ws), "error": err.Error()})
		}
		return
	}
	if _, err := o.queueOf(ctx, ws); err != nil {
		// A REFUSAL IS THE ORDINARY ANSWER — this workspace simply had no
		// queued merge. Anything else is a store failure and is surfaced.
		if _, refused := Refused(err); !refused {
			o.log(ctx, ws).Error(op, "could not tell whether a torn-down workspace had a queued merge",
				dlog.Context{"workspace": string(ws), "error": err.Error()})
		}
		return
	}
	if err := o.dequeue(ctx, ws, CauseWorkspaceClosed); err != nil {
		o.log(ctx, ws).Error(op, "could not abandon the queued merge of a torn-down workspace",
			dlog.Context{"workspace": string(ws), "error": err.Error()})
	}
}

// dropQueued takes one workspace off its queue with the cause it was dropped
// for, and publishes the abandoned terminal. A merge only REQUESTED -- its
// requesting turn still running -- has its wait withdrawn and draws nothing:
// it was never reported, so there is no bubble to end.
func (o *orchestrator) dropQueued(ctx context.Context, ws ids.WorkspaceID, cause AbandonCause) error {
	const op = "daemon.merge.drop_queued"
	log := o.log(ctx, ws)
	repo, err := o.queueOf(ctx, ws)
	if err != nil {
		return err
	}
	// A MERGE WAITING IN LINE IS NOT ENDED BY A CAUSE THAT REQUEUES it: the
	// take-back that found it waiting leaves it where it stands, and the
	// gate's raise marks it (coldGateRaised).
	if cause.requeues() {
		o.coldGateRaised(ctx, ws)
		return nil
	}
	o.withdrawRequest(ws)
	entry, err := o.takeOffQueue(ctx, repo, ws, cause)
	if gone := (*entryGone)(nil); errors.As(err, &gone) {
		return o.leftFirst(ctx, ws, repo, cause, gone)
	}
	if err != nil {
		return err
	}
	o.mu.Lock()
	delete(o.repoOf, ws)
	// THE LEDGER IDENTITY IS KEPT LONG ENOUGH TO END THE BUBBLE. It addresses
	// the queued merge's own bubble, and a bubble that simply stopped
	// mid-queue-tab would leave a reader with no terminal at all — so the
	// terminal is drawn against it before the identity is dropped.
	ledger := o.ledgerOf[ws]
	delete(o.ledgerOf, ws)
	o.mu.Unlock()
	// A RELEASE THE USER OR OPERATOR ASKED FOR IS NO WARNING: it is the queue
	// doing what it was told. A drop under something else ending stays WARN.
	if cause.releases() {
		log.Info(op, "released a queued merge", dlog.Context{"workspace": string(ws), "repo": string(repo), "cause": string(cause)})
	} else {
		log.Warn(op, "dropped a queued merge", dlog.Context{"workspace": string(ws), "repo": string(repo), "cause": string(cause)})
	}
	o.publishAbandonedWith(ctx, ws, ledger, entry, cause, o.queuedSentence(ws, cause, entry))
	o.clearOffer(ws)
	if err := o.republishQueue(ctx, repo); err != nil {
		return err
	}
	o.kick(repo)
	return nil
}

// takeOffQueue reads WS's entry in REPO and removes it for CAUSE, under WS's
// drop lock (dropLocks), so a second drop of the same merge waits for the
// first and then reads the entry gone. It answers the entry it removed, or,
// when the entry had already left -- before the read, or between the read
// and the removal through an exit that is no drop -- an *entryGone for
// leftFirst to explain. Every other failure is recorded here.
func (o *orchestrator) takeOffQueue(ctx context.Context, repo wsm.RepoKey, ws ids.WorkspaceID, cause AbandonCause) (wsm.MergeQueueEntry, error) {
	const op = "daemon.merge.drop_queued"
	log := o.log(ctx, ws)
	drop := o.dropLock(ws)
	if o.onDropWait != nil {
		o.onDropWait(ws)
	}
	drop.Lock()
	defer drop.Unlock()
	entry, err := o.entryOf(ctx, repo, ws)
	if errors.Is(err, errNoQueueEntry) {
		return wsm.MergeQueueEntry{}, &entryGone{found: err}
	}
	if err != nil {
		log.Error(op, "could not read a queued merge's queue entry", dlog.Context{"workspace": string(ws), "repo": string(repo), "error": err.Error()})
		return wsm.MergeQueueEntry{}, err
	}
	// AN ADMITTED MERGE WITH NO RUN AND NO RESUME HERE is not a waiting one:
	// another daemon drives it, a handover is moving it, or its run is
	// starting. Dropping only its entry would leave its lease over none, so
	// it is refused, loudly.
	if entry.State == wsm.MergeAdmitted {
		err := fmt.Errorf("merge: the merge of %s is admitted with no run in this daemon; it is taken off where it runs", ws)
		log.Error(op, "refused to drop an admitted merge as a waiting one", dlog.Context{
			"workspace": string(ws), "repo": string(repo), "cause": string(cause), "error": err.Error()})
		return wsm.MergeQueueEntry{}, err
	}
	if ledger, ok := o.leaseOf(ws); ok {
		if err := o.settleLeavingQueueTab(ctx, repo, ws, ledger, string(cause)); err != nil {
			log.Error(op, "could not settle a dropped merge's queue tab", dlog.Context{"workspace": string(ws), "repo": string(repo), "cause": string(cause), "error": err.Error()})
		}
	}
	err = o.deps.DB.RemoveMergeQueueEntry(ctx, repo, ws, string(cause))
	if errors.Is(err, wsm.ErrNotFound) {
		return wsm.MergeQueueEntry{}, &entryGone{found: err}
	}
	if err != nil {
		log.Error(op, "could not drop a queued merge", dlog.Context{"workspace": string(ws), "repo": string(repo), "cause": string(cause), "error": err.Error()})
		return wsm.MergeQueueEntry{}, err
	}
	return entry, nil
}

// entryGone is takeOffQueue's answer for an entry that had already left its
// queue: FOUND is the read or the removal that found it gone.
type entryGone struct{ found error }

func (e *entryGone) Error() string { return e.found.Error() }
func (e *entryGone) Unwrap() error { return e.found }

// dropLock answers the lock WS's drops are serialized by (dropLocks).
func (o *orchestrator) dropLock(ws ids.WorkspaceID) *sync.Mutex {
	return keyedLock(o, o.dropLocks, ws)
}

// leftFirst answers a drop of WS's merge for CAUSE that found its entry in
// REPO already gone (GONE): the drop is idempotent when the store records
// why the merge left -- its request standing off the queue (an earlier drop,
// an eviction, a failure), its workspace's close, or its landing -- and that
// is recorded at INFO, with the why. An entry gone with none of those is an
// inconsistency between this daemon and its store, recorded at ERROR and
// answered as an error. Either way this daemon's note of the queue it stood
// in is forgotten: no entry of it stands there.
func (o *orchestrator) leftFirst(ctx context.Context, ws ids.WorkspaceID, repo wsm.RepoKey, cause AbandonCause, gone *entryGone) error {
	const op = "daemon.merge.drop_queued"
	log := o.log(ctx, ws)
	fields := dlog.Context{"workspace": string(ws), "repo": string(repo), "cause": string(cause), "found": gone.Error()}
	o.mu.Lock()
	if o.repoOf[ws] == repo {
		delete(o.repoOf, ws)
	}
	o.mu.Unlock()
	off, kept, err := o.deps.DB.OffQueueMergeOf(ctx, ws)
	if err != nil {
		log.Error(op, "could not read why a merge whose entry was gone when its drop came left its queue", withField(fields, "error", err.Error()))
		return err
	}
	if kept {
		fields["left_cause"], fields["left_at"] = off.Cause, off.LeftAt.UTC().Format(time.RFC3339Nano)
		log.Info(op, "the merge had already left its queue when the drop came, its request standing off the queue; nothing is taken off", fields)
		return nil
	}
	record, err := o.deps.DB.Workspace(ctx, ws)
	if err != nil {
		log.Error(op, "could not read the workspace of a merge whose entry was gone when its drop came", withField(fields, "error", err.Error()))
		return err
	}
	switch {
	case record.MergedAt != nil:
		fields["merged_at"] = record.MergedAt.UTC().Format(time.RFC3339Nano)
		log.Info(op, "the merge had already landed when the drop came; nothing is taken off", fields)
		return nil
	case record.Closed:
		log.Info(op, "the merge had already left its queue with its workspace's close when the drop came; nothing is taken off", fields)
		return nil
	}
	err = fmt.Errorf("merge: the merge of %s has no entry in %s and the store records no leaving of it: %w", ws, repo, gone)
	log.Error(op, "a merge this daemon knew as queued has no queue entry and no record of leaving it", withField(fields, "error", err.Error()))
	return err
}

// queuedSentence is the abandoned terminal's sentence of a merge that left
// its queue before it ran: the cause's own sentence, naming the merge ahead
// and how it left when the merge ahead's leaving is the cause.
func (o *orchestrator) queuedSentence(ws ids.WorkspaceID, cause AbandonCause, entry wsm.MergeQueueEntry) string {
	if cause == CauseAheadDidNotLand {
		return fmt.Sprintf("the merge %s this one was placed behind left the queue without landing (%s)", entry.WaitsOnName, entry.WaitsOnLeft)
	}
	return o.abandonedSentence(ws, cause, false)
}

// queueOf resolves the queue a workspace's merge is waiting on, refusing when
// nothing of that workspace is queued.
func (o *orchestrator) queueOf(ctx context.Context, ws ids.WorkspaceID) (wsm.RepoKey, error) {
	o.mu.Lock()
	repo, known := o.repoOf[ws]
	o.mu.Unlock()
	if known {
		return repo, nil
	}
	queues, err := o.deps.DB.AllMergeQueues(ctx)
	if err != nil {
		return "", err
	}
	for key, entries := range queues {
		for _, entry := range entries {
			if entry.Workspace == ws {
				return key, nil
			}
		}
	}
	return "", refuse(ArmNoSuchQueuedMerge, ws, "no merge of this workspace is on any queue")
}

// republishQueue redraws the queue tab of every bubble on one repository's
// queue, and every WAITING merge's facts. The snapshot is replaced whole on
// every queue change and on every step change of the merge being worked on, so
// a waiting user always sees the current order and what the merge ahead is
// doing.
//
// A REQUESTED MERGE IS IN NOBODY'S LINE: its requesting turn has not ended, so
// it is on no snapshot and has no facts.
func (o *orchestrator) republishQueue(ctx context.Context, repo wsm.RepoKey) error {
	o.publishRosterQueue(ctx, repo)
	stored, err := o.deps.DB.MergeQueue(ctx, repo)
	if err != nil {
		return err
	}
	entries := inLine(stored)
	view, err := o.viewQueue(ctx, repo, entries)
	if err != nil {
		return err
	}
	o.mu.Lock()
	front := o.running[repo]
	o.mu.Unlock()
	var ahead *frontendv1.FooterStatusActivityMergeStep
	var aheadAt time.Time
	if front != nil {
		ahead, aheadAt = front.enqueuedLine(view.names[front.ws])
	}
	// A MERGE BACK IN LINE FROM ITS REPAIR is admitted but waits like any
	// merge in line until it heads the queue (drawBackInLine), so it is
	// counted and drawn as waiting.
	waits := func(entry wsm.MergeQueueEntry) bool {
		return entry.State == wsm.MergeQueued || o.backInLine(entry.Workspace)
	}
	waiting := 0
	for _, entry := range entries {
		if waits(entry) {
			waiting++
		}
	}
	place := 0
	var drawn, unledgered []string
	for _, entry := range entries {
		// EVERY MERGE IN LINE HAS A BUBBLE, from the moment it is queued, PINNED
		// to the feed's bottom until it resolves (owner ruling, 2026-10-10;
		// publishHead), so every row the workspace writes while the merge
		// waits lands above it. Its ledger identity is minted as it takes its
		// place, so the head has its queue tab to hang on from the start.
		if lease, ok := o.leaseOf(entry.Workspace); ok && o.backInLine(entry.Workspace) {
			// ITS HEAD WAITS, counted from when it was queued; its queue tab
			// settled at its first admission and is not drawn again.
			queuedMS, err := enqueuedMS(entry)
			if err != nil {
				o.deps.Log.Global().Error("daemon.merge.queue", "a queued merge carries no queued time; the queue is not drawn",
					dlog.Context{"repo": string(repo), "workspace": string(entry.Workspace), "error": err.Error()})
				return err
			}
			o.publishHead(entry.Workspace, lease, o.bubbleLabel(ctx, entry), queuedMS, headWaiting.onPaused(view.paused), headQueue)
		} else if ok {
			clock, stage, clocked, err := o.headClockMS(entry)
			if err != nil {
				o.deps.Log.Global().Error("daemon.merge.queue", "a queued merge carries no queued time; the queue is not drawn",
					dlog.Context{"repo": string(repo), "workspace": string(entry.Workspace), "error": err.Error()})
				return err
			}
			if clocked {
				o.publishHead(entry.Workspace, lease, o.bubbleLabel(ctx, entry), clock, stage.onPaused(view.paused), headQueue)
			}
			state, shown, err := o.queueTabState(entry)
			if err != nil {
				o.deps.Log.Global().Error("daemon.merge.queue", "could not resolve when a queued merge's queue tab began; the queue is not drawn",
					dlog.Context{"repo": string(repo), "workspace": string(entry.Workspace), "error": err.Error()})
				return err
			}
			if shown {
				o.publishRoundTab(entry.Workspace, lease, TabQueue, 1, queueTab(view.snapshot(entries, entry.Workspace), state))
				drawn = append(drawn, string(entry.Workspace))
			} else {
				o.log(ctx, entry.Workspace).Debug("daemon.merge.queue", "an admitted merge's run is not registered yet; its admission draws its queue tab",
					dlog.Context{"repo": string(repo), "workspace": string(entry.Workspace)})
			}
		} else {
			// A MERGE IN LINE WITH NO BUBBLE IDENTITY IN THIS DAEMON has a
			// queue tab nothing here can redraw: the stale tab a reader
			// would otherwise find with no record of why.
			unledgered = append(unledgered, string(entry.Workspace))
		}
		if !waits(entry) {
			continue
		}
		// ENQUEUED k/n COUNTS ONLY THE WAITING: the merge being worked on is
		// not counted, so 1 means next and there is never a 0.
		place++
		o.publish(repo, entry.Workspace, MergeFacts{
			State: StateQueued, QueuedAt: entry.EnqueuedAt, Step: footer.StepEnqueued, QueuePlace: place, QueueWaiting: waiting,
			Line: ahead, LineAt: aheadAt,
		})
	}
	if len(unledgered) > 0 {
		o.deps.Log.Global().Info("daemon.merge.queue", "merges in line have no bubble identity in this daemon; their queue tabs are not redrawn",
			dlog.Context{"repo": string(repo), "workspaces": strings.Join(unledgered, ",")})
	}
	o.deps.Log.Global().Debug("daemon.merge.queue", "redrew the queue tab of every merge in line", dlog.Context{
		"repo": string(repo), "in_line": len(entries), "drawn": strings.Join(drawn, ",")})
	return nil
}

// queueView is what every drawing of one repository's queue reads: whether
// the queue is paused, each in-line workspace's name and directory, the
// worktree its merge lands in, an async merge's bubble, and each entry's
// standing, all keyed by the entry's own workspace.
//
// AN ASYNC MERGE IS NAMED FOR ITSELF AND OPENS ITS BUBBLE (owner ruling,
// 2026-10-17). Its entry's workspace is the hidden merge workspace the daemon
// forked to carry it: the entry carries that workspace's own name (the merge's
// name, `land-...`), and its BUBBLE -- the requester and the merge's head row
// on the requester's feed -- which is what a click selects and opens, so the
// hidden merge workspace is never selected or revealed from the queue. A sync
// merge's entry is the requesting workspace itself and carries no bubble.
type queueView struct {
	paused    bool
	names     map[ids.WorkspaceID]string
	dirs      map[ids.WorkspaceID]string
	targets   map[ids.WorkspaceID]string
	bubbles   map[ids.WorkspaceID]*frontendv1.FeedMergeQueueBubble
	standings []*frontendv1.FeedMergeQueueEntry
}

// viewQueue resolves the queue view of one repository's in-line entries.
func (o *orchestrator) viewQueue(ctx context.Context, repo wsm.RepoKey, entries []wsm.MergeQueueEntry) (queueView, error) {
	paused, err := o.deps.DB.MergeQueuePaused(ctx, repo)
	if err != nil {
		return queueView{}, fmt.Errorf("merge: read whether the queue of %s is paused: %w", repo, err)
	}
	view := queueView{
		paused:  paused,
		names:   map[ids.WorkspaceID]string{},
		dirs:    map[ids.WorkspaceID]string{},
		targets: map[ids.WorkspaceID]string{},
		bubbles: map[ids.WorkspaceID]*frontendv1.FeedMergeQueueBubble{},
	}
	for _, entry := range entries {
		record, err := o.deps.DB.Workspace(ctx, entry.Workspace)
		if err != nil {
			return queueView{}, err
		}
		view.names[entry.Workspace] = record.Name
		view.dirs[entry.Workspace] = record.Dir
		target, err := o.targetDirOf(ctx, entry.Workspace, entry.Source)
		if err != nil {
			return queueView{}, fmt.Errorf("merge: resolve the worktree the merge of %s lands in: %w", entry.Workspace, err)
		}
		view.targets[entry.Workspace] = target
		if record.MergeOf == nil {
			continue
		}
		bubble, err := o.asyncMergeBubble(ctx, entry.Workspace, record.MergeOf.Requester)
		if err != nil {
			return queueView{}, err
		}
		view.bubbles[entry.Workspace] = bubble
	}
	standings, err := o.queueStandings(entries)
	if err != nil {
		o.deps.Log.Global().Error("daemon.merge.queue", "could not resolve when a queued merge entered its stage; the queue is not drawn",
			dlog.Context{"repo": string(repo), "entries": len(entries), "error": err.Error()})
		return queueView{}, err
	}
	view.standings = standings
	return view, nil
}

// asyncMergeBubble answers where merge workspace WS's bubble stands: its
// requester, and the bubble's head row on the requester's root feed. A merge
// with no bubble identity in this daemon (no ledger entry) answers the
// requester alone, so its click still selects the requester and never the
// hidden merge workspace; it is recorded, since there is no bubble to open.
func (o *orchestrator) asyncMergeBubble(ctx context.Context, ws, requester ids.WorkspaceID) (*frontendv1.FeedMergeQueueBubble, error) {
	record, err := o.deps.DB.Workspace(ctx, requester)
	if err != nil {
		return nil, fmt.Errorf("read the requester of merge workspace %s: %w", ws, err)
	}
	bubble := &frontendv1.FeedMergeQueueBubble{
		Requester: record.Ref(),
	}
	lease, ok := o.leaseOf(ws)
	if !ok {
		o.log(ctx, ws).Info("daemon.merge.queue", "an async merge in line has no bubble identity in this daemon; its queue entry selects the requester and opens no bubble",
			dlog.Context{"workspace": string(ws), "requester": string(requester)})
		return bubble, nil
	}
	bubble.Row = feedid.Encode(headRef(requester, lease))
	return bubble, nil
}

// snapshot is the queue as self sees it.
func (v queueView) snapshot(entries []wsm.MergeQueueEntry, self ids.WorkspaceID) *frontendv1.FeedMergeQueue {
	return queueSnapshot(entries, self, v)
}

// settleLeavingQueueTab draws a merge's own queue tab ONE LAST TIME as it
// leaves the queue -- landed, failed, abandoned or dropped -- before its entry
// is removed: SETTLED, with the queue as it stands at that moment and itself
// as the current entry. Its tab is no longer redrawn once it has left, because
// the queue it would show no longer holds it; every merge still in line is
// redrawn by the republish that follows the removal (owner ruling,
// 2026-10-12; AGENTS.md, "The merge queue's test gate and remediation
// contract").
//
// A merge only REQUESTED has no bubble and draws nothing.
func (o *orchestrator) settleLeavingQueueTab(ctx context.Context, repo wsm.RepoKey, ws ids.WorkspaceID, ledger ids.LeaseID, why string) error {
	const op = "daemon.merge.queue"
	stored, err := o.deps.DB.MergeQueue(ctx, repo)
	if err != nil {
		return err
	}
	entries := inLine(stored)
	idx := slices.IndexFunc(entries, func(e wsm.MergeQueueEntry) bool { return e.Workspace == ws })
	if idx < 0 || ledger == "" {
		o.log(ctx, ws).Debug(op, "a merge left a queue it never stood in line in; it has no queue tab to settle",
			dlog.Context{"workspace": string(ws), "repo": string(repo), "why": why})
		return nil
	}
	view, err := o.viewQueue(ctx, repo, entries)
	if err != nil {
		return err
	}
	state, shown, err := o.queueTabState(entries[idx])
	if err != nil {
		return err
	}
	if !shown || !state.settled {
		// IT LEFT WHILE STILL WAITING: its wait ends now.
		state = tabState{startedMS: state.startedMS, settled: true, endedMS: o.nowMS()}
		if !shown {
			queued, err := enqueuedMS(entries[idx])
			if err != nil {
				return err
			}
			state.startedMS = queued
		}
	}
	o.publishRoundTab(ws, ledger, TabQueue, 1, queueTab(view.snapshot(entries, ws), state))
	o.log(ctx, ws).Info(op, "settled the leaving merge's queue tab on the queue as it stood when it left", dlog.Context{
		"workspace": string(ws), "repo": string(repo), "ledger": string(ledger), "why": why, "in_line": len(entries)})
	return nil
}

// inLine answers a repository's stored queue less its REQUESTED merges and the
// merges REMEDIATING out of line (remediate.go), with the positions renumbered
// over what is left.
func inLine(stored []wsm.MergeQueueEntry) []wsm.MergeQueueEntry {
	entries := make([]wsm.MergeQueueEntry, 0, len(stored))
	for _, entry := range stored {
		if entry.State == wsm.MergeRequested || entry.State == wsm.MergeRemediating {
			continue
		}
		entry.Position = len(entries) + 1
		entries = append(entries, entry)
	}
	return entries
}

// admissible answers LINE less the merges whose workspace stands at its cold
// gate (wsm.MergeQueueEntry.ColdGated): each keeps its place in line, and
// the queue skips it, so it is never admitted, never joins a chain, and is
// no merge for another to yield to. It is THE ONE FILTER every admission
// decision reads the line through (mergecoldgate.go). Positions are left as
// LINE numbered them, its place in line.
func admissible(line []wsm.MergeQueueEntry) []wsm.MergeQueueEntry {
	out := make([]wsm.MergeQueueEntry, 0, len(line))
	for _, entry := range line {
		if entry.ColdGated {
			continue
		}
		out = append(out, entry)
	}
	return out
}

// outOfLine answers the merges of a repository's stored queue remediating out
// of line (remediate.go), in the order they stepped aside.
func outOfLine(stored []wsm.MergeQueueEntry) []wsm.MergeQueueEntry {
	var out []wsm.MergeQueueEntry
	for _, entry := range stored {
		if entry.State == wsm.MergeRemediating {
			out = append(out, entry)
		}
	}
	return out
}

// bubbleLabel is a queued merge's head line, read off what it merges. A label
// that cannot be read is recorded and the workspace's own id stands in: a
// label must not stop the queue from being drawn.
func (o *orchestrator) bubbleLabel(ctx context.Context, entry wsm.MergeQueueEntry) string {
	if r, running := o.runFor(entry.Workspace); running {
		return r.label()
	}
	label, err := o.sourceLabel(ctx, entry.Workspace, entry.Source)
	switch {
	case err != nil && cutUnder(ctx, err):
		// THE READ ENDED WITH ITS CONTEXT, which says nothing about the merge
		// or its branch: the publish it served was cut short (a shutdown, a
		// caller gone), and the next publish reads the label again.
		o.log(ctx, entry.Workspace).Info("daemon.merge.queue", "the read of a queued merge's bubble label ended with its context; the workspace's id stands in",
			dlog.Context{"workspace": string(entry.Workspace), "cause": err.Error()})
		return string(entry.Workspace)
	case err != nil:
		o.log(ctx, entry.Workspace).Error("daemon.merge.queue", "could not read what a queued merge lands for its bubble's label",
			dlog.Context{"workspace": string(entry.Workspace), "error": err.Error()})
		return string(entry.Workspace)
	}
	return label
}

// leaseOf reports the LEDGER identity a workspace's merge bubble is keyed by.
// It is minted at ENQUEUE, so a merge that is only queued already has a bubble
// -- the one its queue tab is drawn on -- and the occupancy lease taken at
// admission carries the same identity, which is what makes the queued bubble
// and the running one one bubble.
func (o *orchestrator) leaseOf(ws ids.WorkspaceID) (ids.LeaseID, bool) {
	o.mu.Lock()
	defer o.mu.Unlock()
	id, ok := o.ledgerOf[ws]
	return id, ok
}

// mintLedger mints a workspace's merge ledger identity, or answers the one it
// already has: a merge put back in line keeps its bubble. ONE LEDGER PER
// MERGE: it is minted only as the merge takes its place in line (the
// request) or for a row an earlier build queued without one
// (ledgerOfEntry); every later step reads the one its queue entry carries
// (ledgerOfEntry), never this daemon's memory, which a restart or a handover
// empties (2026-11-15: a waiting merge's background rebase minted a second
// ledger, and its bubble was drawn twice).
func (o *orchestrator) mintLedger(ws ids.WorkspaceID) ids.LeaseID {
	o.mu.Lock()
	defer o.mu.Unlock()
	if id, ok := o.ledgerOf[ws]; ok {
		return id
	}
	id := ids.LeaseID(wsm.NewLeaseID())
	o.ledgerOf[ws] = id
	return id
}

// kick starts the admission pump for one repository, unless one is already
// running for it.
func (o *orchestrator) kick(repo wsm.RepoKey) {
	if !o.async {
		if o.onKick != nil {
			o.onKick(repo)
		}
		return
	}
	o.mu.Lock()
	// A DRAINING DAEMON ADMITS NOTHING. The shutdown drain is bounded, and a
	// merge admitted inside it would be starting its first phase against a
	// state client that is about to close.
	if o.draining {
		o.mu.Unlock()
		return
	}
	// A KICK TO A RUNNING PUMP IS REMEMBERED, NOT DROPPED. The pump reads the
	// mark under this lock before it goes idle, so a merge put in line in the
	// instant between the pump's last look and its exit is still admitted.
	if o.pumping[repo] {
		o.kicked[repo] = true
		o.mu.Unlock()
		// THE CHAIN GROWS BEHIND A RUNNING HEAD: the pump is busy with the
		// head, so the merge put in line (or taken back) behind it is offered
		// to the chain here.
		o.extendChainAsync(repo)
		return
	}
	o.pumping[repo] = true
	o.kicked[repo] = false
	o.mu.Unlock()
	o.extendChainAsync(repo)
	o.live.Add(1)
	inflight.Standing(func() {
		defer o.live.Done()
		// admitted counts what this burst ran, and the burst's own terminal
		// record carries it. THE RECORD IS THE END OF THE WHOLE MERGE, not of
		// its terminal row: the terminal is published partway through `finish`,
		// and the landing's durable stamps, the lease release, the queue
		// entry's removal and every synchronous republish they trigger all
		// follow it INSIDE pumpOnce, with this pump the last thing holding
		// them. Nothing else names that moment, so an observer that must not
		// disturb a merge in flight has nowhere else to wait; a burst that
		// admitted nothing is distinguishable by the count.
		admitted := 0
		for {
			ran, err := o.pumpOnce(context.Background(), repo)
			if err != nil {
				o.mu.Lock()
				o.pumping[repo] = false
				o.mu.Unlock()
				o.deps.Log.Global().Error("daemon.merge.pump", "the admission pump stopped on an error",
					dlog.Context{"repo": string(repo), "admitted": admitted, "error": err.Error()})
				return
			}
			if !ran {
				// THE DRAIN RUNS BEFORE THE PUMP GOES IDLE, while it still
				// holds the repository's admissions: a merge put in line
				// meanwhile is remembered by its kick, and admitted after.
				o.settleDrained(context.Background(), repo)
				o.mu.Lock()
				again := o.kicked[repo]
				o.kicked[repo] = false
				if !again {
					o.pumping[repo] = false
				}
				o.mu.Unlock()
				if again {
					continue
				}
				o.deps.Log.Global().Debug("daemon.merge.pump", "the admission pump went idle",
					dlog.Context{"repo": string(repo), "admitted": admitted})
				return
			}
			admitted++
		}
	})
}

// pumpOnce admits and runs at most one merge for a repository, and returns once
// it has ended. It reports whether it ran one, so the pump loop ends on an
// empty or paused queue rather than spinning.
//
// NOTHING PARKS, so a run holds its repository's slot from its admission to
// its end: a merge that gives up fails, leaves the queue at once, and gives the
// slot back through the one release path.
func (o *orchestrator) pumpOnce(ctx context.Context, repo wsm.RepoKey) (bool, error) {
	// A REPAIR SLOT A WRITE RELEASED is granted in that write (wsm's slot
	// derivation); every such write kicks this pump, which starts the
	// granted merge's repair here (armRepairs) as it looks at the line.
	o.armRepairs(ctx, repo)
	front, promoted, admitted, err := o.admitFront(ctx, repo)
	if err != nil || !admitted {
		return false, err
	}
	if promoted != nil {
		// A MERGE PROMOTED FROM BEHIND THE HEAD RUNS ON ITS OWN GOROUTINE,
		// already started; the pump holds the slot for it until it ends. The
		// merge now behind it joins the race at once, as behind an admitted
		// head (run.admit).
		o.extendChain(context.WithoutCancel(ctx), repo)
		<-promoted.finished
		return true, nil
	}
	if err := o.runAdmitted(ctx, repo, front); err != nil {
		// ONE MERGE'S FAILURE IS NOT THE QUEUE'S. A run that reached its own
		// terminal has already recorded the failure at ERROR, published the
		// bubble's terminal and left the queue -- so the pump goes on to the
		// next entry rather than stranding every merge behind this one.
		//
		// The test is whether the entry is STILL THERE: a failure early enough
		// to leave it admitted has no terminal and no teardown, and continuing
		// would re-admit the same entry forever. That one stops the pump, as
		// the caller's ERROR record says. A draining daemon's store may be
		// closing under the check, so the check is not made and the pump stops
		// on the failure, which the caller's ERROR record states.
		if !o.enterAdmission() {
			return true, err
		}
		still, checkErr := o.stillQueued(ctx, repo, front)
		o.leaveAdmission()
		if checkErr != nil || still {
			return true, err
		}
		o.deps.Log.Global().Debug("daemon.merge.pump", "a merge ended on its own terminal; the queue continues",
			dlog.Context{"repo": string(repo), "workspace": string(front), "error": err.Error()})
	}
	return true, nil
}

// backInLine reports whether WS's merge is back in line from an out-of-line
// repair, waiting for the head with no run (drawBackInLine): its record says
// so and no run of it stands in this daemon.
func (o *orchestrator) backInLine(ws ids.WorkspaceID) bool {
	o.mu.Lock()
	defer o.mu.Unlock()
	doc, resuming := o.resumes[ws]
	return resuming && doc.BackInLine && o.runsByWorkspace[ws] == nil
}

// isDraining reports whether the daemon's orderly exit has begun.
func (o *orchestrator) isDraining() bool {
	o.mu.Lock()
	defer o.mu.Unlock()
	return o.draining
}

// enterAdmission registers one admission step against the shutdown drain,
// and reports false -- registering nothing -- once the daemon is draining.
// The caller ends the step with o.leaveAdmission().
//
// IT IS WHAT MAKES "draining" AN EXCLUSION RATHER THAN A HINT. The pump used
// to READ the flag and then go on to read the store, so a drain that began in
// between closed the state client under the pump's next read (measured:
// `daemon.wsm.merge_queue: refused the read ... sql: database is closed` in
// TestStoppingTheDaemonInsideAMergesTerminalStampsTheLandingWithNoFailedWrites).
// The step is registered under the same lock the drain sets the flag under,
// and the drain waits for every registered step before it returns.
func (o *orchestrator) enterAdmission() bool {
	o.mu.Lock()
	defer o.mu.Unlock()
	if o.draining {
		return false
	}
	o.admissions++
	return true
}

// leaveAdmission ends one step enterAdmission registered, and releases a drain
// waiting on the last of them. A leave with nothing registered is an
// accounting defect and panics: the count would otherwise go negative and the
// drain would stop waiting for steps that are still running.
func (o *orchestrator) leaveAdmission() {
	o.mu.Lock()
	defer o.mu.Unlock()
	if o.admissions <= 0 {
		panic("merge: leaveAdmission with no admission step registered")
	}
	o.admissions--
	if o.admissions == 0 && o.admissionsIdle != nil {
		close(o.admissionsIdle)
		o.admissionsIdle = nil
	}
}

// admitFront admits the repository's queue front, as ONE admission step the
// shutdown drain waits for: every store read and the admission write happen
// inside it. It answers the admitted workspace, or the speculative run it
// promoted into the slot, or admitted=false when there is nothing to admit (a
// draining daemon, a paused or empty queue, a run already in flight, another
// daemon's lock). The repository's kernel lock is held from here
// (holdRepoLock) for as long as any run of its chain stands.
func (o *orchestrator) admitFront(ctx context.Context, repo wsm.RepoKey) (ids.WorkspaceID, *run, bool, error) {
	const op = "daemon.merge.admit"
	// The pump loop re-enters this every iteration: a drain that began while a
	// burst was mid-flight stops the burst here rather than after it has taken
	// the next entry's lease.
	if !o.enterAdmission() {
		return "", nil, false, nil
	}
	defer o.leaveAdmission()
	line := o.lineLock(repo)
	line.Lock()
	defer line.Unlock()
	o.mu.Lock()
	// A MAIN SYNC HOLDING THE TARGET admits nothing until it releases
	// (synchold.go), and its release kicks this pump.
	busy := o.running[repo] != nil || o.syncing[repo]
	o.mu.Unlock()
	if busy {
		return "", nil, false, nil
	}
	// A PAUSED QUEUE ADVANCES NOTHING (owner ruling, 2026-10-12): merges
	// still enqueue, but nothing near its head runs -- no admission, no
	// promotion of the merge behind a head that ended, and no resume of a
	// merge a restart or the pause itself suspended; those wait, recorded,
	// until the queue is resumed.
	stopped, err := o.queueHeld(ctx, repo)
	if err != nil {
		return "", nil, false, err
	}
	if stopped {
		return "", nil, false, nil
	}
	// A MERGE A RESTART INTERRUPTED GOES FIRST: it was already running, and
	// its entry is admitted already. Of several, the first in line is the
	// head; the others resume behind it in its chain (extendChain).
	if ws, resuming, err := o.resumeIn(ctx, repo); err != nil {
		return "", nil, false, err
	} else if resuming {
		taken, err := o.holdRepoLock(repo)
		if err != nil {
			return "", nil, false, err
		}
		if !taken {
			o.deps.Log.Global().Warn(op, "another daemon holds this repository's merge queue", dlog.Context{"repo": string(repo)})
			return "", nil, false, nil
		}
		return ws, nil, true, nil
	}
	entries, err := o.deps.DB.MergeQueue(ctx, repo)
	if err != nil {
		return "", nil, false, err
	}
	// THE MERGE RIGHT BEHIND A HEAD THAT ENDED IS PROMOTED: it was admitted
	// when it began rebasing behind the head.
	if r, promoted := o.promote(repo, entries); promoted {
		return r.ws, r, true, nil
	}
	// ONE MERGE PER REPOSITORY AT A TIME, ACROSS DAEMONS. An entry admitted
	// with no run here is a merge another daemon is driving, or one a
	// handover is moving between them: nothing is admitted behind it until it
	// ends or resumes here.
	for _, entry := range entries {
		if entry.State == wsm.MergeAdmitted {
			o.deps.Log.Global().Debug(op, "a merge admitted elsewhere holds this repository's slot; nothing is admitted behind it",
				dlog.Context{"repo": string(repo), "workspace": string(entry.Workspace)})
			return "", nil, false, nil
		}
	}
	// A MERGE WHOSE MERGE AHEAD LEFT WITHOUT LANDING FAILS before anything is
	// admitted: it was placed behind that merge because it needs it. Its drop
	// kicks the pump, which comes back here.
	if failed, found := aheadFailed(entries); found {
		o.failBehind(ctx, repo, failed)
		return "", nil, false, nil
	}
	// A MERGE WHOSE WORKSPACE THIS DAEMON DOES NOT SERVE IS PASSED OVER, at
	// any place in line (owner ruling, 2026-11-15): an older daemon still
	// serving it, or one it moved to and this daemon has not adopted yet, may
	// hold it for as long as a turn runs there, and the merges behind it never
	// wait on that. It keeps its place, and its adoption here kicks this pump,
	// so it is the next admitted.
	front, found, passed := nextInLine(entries, func(ws ids.WorkspaceID) bool {
		return !o.movedAway(ws) && o.deps.Standing.ServesWorkspace(ws)
	})
	o.notePassedOver(repo, passed)
	if !found {
		return "", nil, false, nil
	}
	// A FRONT WHOSE BACKGROUND REBASE IS STILL RUNNING is admitted once it
	// settles back into line (rebasewaiting.go kicks the pump then).
	o.mu.Lock()
	held := o.runsByWorkspace[front.Workspace]
	rebasing := held != nil && held.background
	o.mu.Unlock()
	if rebasing {
		o.deps.Log.Global().Debug(op, "the queue front is rebasing in the background; it is admitted once that settles",
			dlog.Context{"repo": string(repo), "workspace": string(front.Workspace)})
		return "", nil, false, nil
	}
	// A FRONT ALREADY STARTING BEHIND THE HEAD is its chain's: it is promoted
	// once its admission is written, never started a second time here.
	if o.inChain(repo, front.Workspace) {
		o.deps.Log.Global().Debug(op, "the queue front is starting in its chain; it is promoted once admitted",
			dlog.Context{"repo": string(repo), "workspace": string(front.Workspace)})
		return "", nil, false, nil
	}
	taken, err := o.holdRepoLock(repo)
	if err != nil {
		return "", nil, false, err
	}
	if !taken {
		o.deps.Log.Global().Warn(op, "another daemon holds this repository's merge queue", dlog.Context{"repo": string(repo)})
		return "", nil, false, nil
	}
	// A MERGE WORKSPACE'S PICK RUNS AT THE HEAD, holding the slot: the merge
	// has STARTED, so its pick -- and the fork its first turn brings up -- sees
	// the maker as it stands now. Nothing is admitted until the pick settles:
	// a pick that succeeds clears itself and kicks the pump, which then admits
	// the merge; one that fails takes the merge off the queue.
	if front.Source.Pick != "" {
		o.mu.Lock()
		_, picking := o.picks[front.Workspace]
		o.mu.Unlock()
		if !picking && front.Source.PickStartedAt == nil {
			if err := o.startPick(ctx, front.Workspace, repo, front); err != nil {
				o.releaseRepoLockIfIdle(ctx, repo)
				return "", nil, false, err
			}
		}
		o.deps.Log.Global().Debug(op, "the queue front is a merge workspace whose pick runs at the head; it is admitted once the pick succeeds",
			dlog.Context{"repo": string(repo), "workspace": string(front.Workspace), "picking": picking})
		return "", nil, false, nil
	}
	// THE LEASE GOES FIRST, THE ADMISSION AFTER, as for a merge behind the
	// head (chain.go): an admitted entry always has a run holding a lease.
	// A WORKSPACE ANOTHER HOLDER HOLDS (a shim restart, a drain, a
	// hibernation) is waited for, unadmitted: its lease's next change kicks
	// the queue (LeaseChanged). Admitting first once left a head admitted with
	// no run when a restart held its workspace, which read as a merge another
	// daemon drives, and the queue stood still behind it (2026-10-24 19:33).
	lease, err := o.deps.DB.AcquireLeaseAs(ctx, front.Workspace, o.ledgerOfEntry(front), wsm.HolderMerge, wsm.PolicyHold)
	if other, held := heldByAnother(err); held {
		o.releaseRepoLockIfIdle(ctx, repo)
		o.mu.Lock()
		o.leaseWaits[front.Workspace] = repo
		o.mu.Unlock()
		o.deps.Log.Global().Info(op, "the queue front's workspace is held by another lease; it is admitted once that lease changes",
			dlog.Context{"repo": string(repo), "workspace": string(front.Workspace), "holder": other.Holder.String(), "lease": string(other.Lease)})
		return "", nil, false, nil
	}
	if err != nil {
		o.releaseRepoLockIfIdle(ctx, repo)
		return "", nil, false, err
	}
	if err := o.deps.DB.AdmitMerge(ctx, repo, front.Workspace); err != nil {
		if releaseErr := o.deps.DB.ReleaseLease(ctx, lease.ID); releaseErr != nil {
			o.deps.Log.Global().Error(op, "could not release the lease of a merge whose admission failed",
				dlog.Context{"workspace": string(front.Workspace), "lease": string(lease.ID), "error": releaseErr.Error()})
		}
		o.releaseRepoLockIfIdle(ctx, repo)
		return "", nil, false, err
	}
	o.mu.Lock()
	o.headLeases[front.Workspace] = lease
	o.mu.Unlock()
	return front.Workspace, nil, true, nil
}

// LeaseChanged is told each lease change of a workspace (the prompt queue's
// lease sink, promptqueue.Deps.Leases). A queue front that waited for that
// workspace's lease is offered to its queue again.
func (o *orchestrator) LeaseChanged(ws ids.WorkspaceID) {
	o.mu.Lock()
	repo, waiting := o.leaseWaits[ws]
	delete(o.leaseWaits, ws)
	o.mu.Unlock()
	if !waiting {
		return
	}
	o.deps.Log.Global().Info("daemon.merge.admit", "the lease the queue front waited for changed; the queue is offered it again",
		dlog.Context{"repo": string(repo), "workspace": string(ws)})
	o.kick(repo)
}

// Relabel implements Orchestrator: the queue view reads each entry's name when
// it is composed (viewQueue), so republishing the queues WS stands in is the
// whole relabel.
func (o *orchestrator) Relabel(ctx context.Context, ws ids.WorkspaceID) error {
	const op = "daemon.merge.relabel"
	queues, err := o.deps.DB.AllMergeQueues(ctx)
	if err != nil {
		o.deps.Log.Global().Error(op, "could not read the merge queues to relabel a renamed workspace", dlog.Context{
			"workspace": string(ws), "error": err.Error()})
		return fmt.Errorf("merge: relabel %s: read the queues: %w", ws, err)
	}
	var errs []error
	for repo, entries := range queues {
		if !slices.ContainsFunc(entries, func(entry wsm.MergeQueueEntry) bool { return entry.Workspace == ws }) {
			continue
		}
		if err := o.republishQueue(ctx, repo); err != nil {
			o.deps.Log.Global().Error(op, "could not republish the merge queue a renamed workspace stands in", dlog.Context{
				"workspace": string(ws), "repo": string(repo), "error": err.Error()})
			errs = append(errs, fmt.Errorf("merge: relabel %s in %s: %w", ws, repo, err))
			continue
		}
		o.deps.Log.Global().Info(op, "republished the merge queue a renamed workspace stands in", dlog.Context{
			"workspace": string(ws), "repo": string(repo)})
	}
	return errors.Join(errs...)
}

// resumeIn answers the merge of one repository the boot recovery handed the
// pump to resume as its head.
//
// THE HEAD AFTER A RESTART IS THE FIRST ADMITTED ENTRY IN QUEUE ORDER, never
// whichever recovery path ran first. The boot recovery takes back the merges
// whose leases it finds at once, while a merge that was mid-handover comes
// back only when its workspace's adoption takes it (AdoptWorkspace), possibly
// after a merge behind it was already handed over. So when the first admitted
// entry is not yet a resume here -- and has no run here either -- nothing of
// the repository resumes until it is: its adoption's kick resumes it as the
// head, and the merges behind it resume in its chain (extendChain). A merge
// resumed ahead of it would have stranded it, its bubble showing a step with
// no run behind it.
func (o *orchestrator) resumeIn(ctx context.Context, repo wsm.RepoKey) (ids.WorkspaceID, bool, error) {
	const op = "daemon.merge.resume"
	o.mu.Lock()
	any := false
	for _, doc := range o.resumes {
		if doc.Repo == string(repo) {
			any = true
			break
		}
	}
	o.mu.Unlock()
	if !any {
		return "", false, nil
	}
	entries, err := o.deps.DB.MergeQueue(ctx, repo)
	if err != nil {
		return "", false, err
	}
	// A MERGE A RESTART SENT BACK IN LINE STILL STANDS AHEAD OF EVERY MERGE
	// BEHIND IT (owner ruling, 2026-11-16). A head the restart cut before
	// its first step is put back waiting at its own place (recoverEntry),
	// and the merges behind it were admitted in its chain, rebased onto its
	// branch: resuming them first landed them ahead of it, on a base it no
	// longer has. So when a merge the admission would take waits ahead of
	// the first merge to resume, every merge to resume behind it goes back
	// to its place too, and the line starts over from its head, in order.
	if waiting, behind := startsOverBehind(entries, o.resumesOf(repo), func(ws ids.WorkspaceID) bool {
		return !o.movedAway(ws) && o.deps.Standing.ServesWorkspace(ws)
	}); waiting != "" {
		for _, ws := range behind {
			if err := o.requeueAdmitted(ctx, op, repo, ws, requeueWhy{
				unleased: "the merge to resume behind a merge that starts over has no merge lease to drop; nothing of the repository is admitted until it is settled",
				failed:   "the merge to resume behind a merge that starts over could not be put back in line; nothing of the repository is admitted until it is settled",
				done:     "a merge to resume stands behind a merge the restart put back in line, so it goes back to its own place and starts again behind it",
			}); err != nil {
				return "", false, err
			}
		}
		return "", false, nil
	}
	o.mu.Lock()
	for _, entry := range entries {
		if entry.State != wsm.MergeAdmitted {
			continue
		}
		if doc, ok := o.resumes[entry.Workspace]; ok && doc.Repo == string(repo) {
			o.mu.Unlock()
			return entry.Workspace, true, nil
		}
		running := o.runsByWorkspace[entry.Workspace] != nil || o.inChainLocked(repo, entry.Workspace)
		var waiting []string
		for ws, doc := range o.resumes {
			if doc.Repo == string(repo) {
				waiting = append(waiting, string(ws))
			}
		}
		o.mu.Unlock()
		if !running {
			sort.Strings(waiting)
			o.deps.Log.Global().Info(op, "the queue's first admitted merge is not taken back yet; nothing of the repository resumes until its workspace's adoption resumes it as the head", dlog.Context{
				"repo": string(repo), "head": string(entry.Workspace), "waiting": strings.Join(waiting, ",")})
		}
		return "", false, nil
	}
	defer o.mu.Unlock()
	// A RESUME WITH NO ADMITTED QUEUE ENTRY is answered as before: its run
	// reads the entry it lands and ends on the missing one, loudly. A merge
	// remediating out of line is no such resume: it waits for its fixing
	// turn, then returns admitted (remediate.go).
	for ws, doc := range o.resumes {
		if doc.Repo == string(repo) && !o.isRemediatingLocked(ws) {
			return ws, true, nil
		}
	}
	return "", false, nil
}

// resumesOf answers the workspaces whose merges in REPO wait to resume.
func (o *orchestrator) resumesOf(repo wsm.RepoKey) map[ids.WorkspaceID]bool {
	o.mu.Lock()
	defer o.mu.Unlock()
	out := map[ids.WorkspaceID]bool{}
	for ws, doc := range o.resumes {
		if doc.Repo == string(repo) {
			out[ws] = true
		}
	}
	return out
}

// startsOverBehind answers the merge the admission would take that waits
// ahead of the first merge to resume in ENTRIES, and every admitted merge to
// resume (RESUMING) behind it, in line order; "" when the first merge to
// resume stands ahead of every merge waiting that the admission would take.
func startsOverBehind(entries []wsm.MergeQueueEntry, resuming map[ids.WorkspaceID]bool, servedHere func(ids.WorkspaceID) bool) (ids.WorkspaceID, []ids.WorkspaceID) {
	var waiting ids.WorkspaceID
	var behind []ids.WorkspaceID
	for _, entry := range entries {
		switch {
		case entry.State == wsm.MergeQueued && waiting == "" && !entry.ColdGated && !entry.HeldBehind() && !entry.AheadFailed() && servedHere(entry.Workspace):
			waiting = entry.Workspace
		case entry.State == wsm.MergeAdmitted && resuming[entry.Workspace]:
			if waiting == "" {
				return "", nil
			}
			behind = append(behind, entry.Workspace)
		}
	}
	if len(behind) == 0 {
		return "", nil
	}
	return waiting, behind
}

// passReason is why the admission passed a merge waiting in line over.
type passReason string

const (
	// passHeldBehind: it was placed behind a merge still standing in the
	// queue, and is neither admitted nor picked until that merge leaves.
	passHeldBehind passReason = "held_behind"
	// passServedElsewhere: another daemon serves its workspace, or this one
	// has not adopted it yet.
	passServedElsewhere passReason = "served_elsewhere"
)

// passedOver is one merge waiting in line ahead of the one admitted, and why
// it was passed over.
type passedOver struct {
	entry  wsm.MergeQueueEntry
	reason passReason
}

// nextInLine answers the first entry of a queue that is waiting in line, not
// standing at its cold gate (admissible), and whose workspace SERVEDHERE says this daemon serves, and every merge waiting
// ahead of it that it passed over, with why: a requested merge is in nobody's
// line, an admitted one is already running, and a merge whose merge ahead
// failed is failed before this runs (failBehind).
func nextInLine(entries []wsm.MergeQueueEntry, servedHere func(ids.WorkspaceID) bool) (wsm.MergeQueueEntry, bool, []passedOver) {
	var passed []passedOver
	for _, entry := range admissible(entries) {
		if entry.State != wsm.MergeQueued || entry.AheadFailed() {
			continue
		}
		switch {
		case entry.HeldBehind():
			passed = append(passed, passedOver{entry: entry, reason: passHeldBehind})
		case !servedHere(entry.Workspace):
			passed = append(passed, passedOver{entry: entry, reason: passServedElsewhere})
		default:
			return entry, true, passed
		}
	}
	return wsm.MergeQueueEntry{}, false, passed
}

// notePassedOver records each merge the admission of REPO's queue passed
// over, at INFO, once per reason: a merge passed over keeps its place, and
// the record is why the merges behind it run first (2026-11-16: a head
// passed over for hours left no record above DEBUG). A merge no longer
// passed over is forgotten, so passing it over again is recorded again.
func (o *orchestrator) notePassedOver(repo wsm.RepoKey, passed []passedOver) {
	now := make(map[ids.WorkspaceID]passReason, len(passed))
	var fresh []passedOver
	o.mu.Lock()
	before := o.passedOverOf[repo]
	for _, p := range passed {
		now[p.entry.Workspace] = p.reason
		if before[p.entry.Workspace] != p.reason {
			fresh = append(fresh, p)
		}
	}
	if len(now) == 0 {
		delete(o.passedOverOf, repo)
	} else {
		o.passedOverOf[repo] = now
	}
	o.mu.Unlock()
	for _, p := range fresh {
		fields := dlog.Context{"repo": string(repo), "workspace": string(p.entry.Workspace), "reason": string(p.reason)}
		switch p.reason {
		case passHeldBehind:
			fields["behind"], fields["behind_name"] = string(p.entry.WaitsOn), p.entry.WaitsOnName
			o.deps.Log.Global().Info("daemon.merge.behind", "a merge in line is passed over: it waits for the merge it was placed behind to leave the queue, and the merges behind it are admitted past it", fields)
		case passServedElsewhere:
			o.deps.Log.Global().Info("daemon.merge.admit", "a merge in line is passed over: this daemon does not serve its workspace yet, so it keeps its place and is admitted once its workspace is served here", fields)
		}
	}
}

// aheadFailed answers the first merge waiting in line whose merge ahead left
// the queue without landing (wsm.MergeQueueEntry.AheadFailed).
func aheadFailed(entries []wsm.MergeQueueEntry) (wsm.MergeQueueEntry, bool) {
	for _, entry := range entries {
		if entry.State == wsm.MergeQueued && entry.AheadFailed() {
			return entry, true
		}
	}
	return wsm.MergeQueueEntry{}, false
}

// failBehind ends a merge waiting in line whose merge ahead left the queue
// without landing, as a merge that never ran (dropQueued), naming the merge
// ahead and the cause it left for.
func (o *orchestrator) failBehind(ctx context.Context, repo wsm.RepoKey, entry wsm.MergeQueueEntry) {
	const op = "daemon.merge.behind"
	fields := dlog.Context{"repo": string(repo), "workspace": string(entry.Workspace), "behind": string(entry.WaitsOn), "behind_name": entry.WaitsOnName, "behind_left": entry.WaitsOnLeft}
	o.log(ctx, entry.Workspace).Info(op, "the merge this one was placed behind left the queue without landing; this merge fails", fields)
	o.mu.Lock()
	o.repoOf[entry.Workspace] = repo
	o.mu.Unlock()
	if err := o.dropQueued(ctx, entry.Workspace, CauseAheadDidNotLand); err != nil {
		o.log(ctx, entry.Workspace).Error(op, "could not fail a merge whose merge ahead left the queue without landing", withField(fields, "error", err.Error()))
	}
}

// stillQueued reports whether a workspace's entry is still on its repository's
// queue, which is how the pump tells a merge that ended from one that never
// started.
func (o *orchestrator) stillQueued(ctx context.Context, repo wsm.RepoKey, ws ids.WorkspaceID) (bool, error) {
	entries, err := o.deps.DB.MergeQueue(ctx, repo)
	if err != nil {
		return false, err
	}
	for _, entry := range entries {
		if entry.Workspace == ws {
			return true, nil
		}
	}
	return false, nil
}

// queueTab builds the queue tab: LIVE while this workspace waits, SETTLED the
// moment it reaches the front — the tab's whole job is the wait, so reaching
// the front is what completes it.
func queueTab(snapshot *frontendv1.FeedMergeQueue, st tabState) *frontendv1.FeedMergeTab {
	inner := &frontendv1.FeedMergeTabQueue{Queue: snapshot}
	if live, settled := st.badge(); live != nil {
		inner.State = &frontendv1.FeedMergeTabQueue_Live{Live: live}
	} else {
		inner.State = &frontendv1.FeedMergeTabQueue_Settled{Settled: settled}
	}
	return &frontendv1.FeedMergeTab{Kind: &frontendv1.FeedMergeTab_Queue{Queue: inner}}
}

// queueSpan is one in-line merge's wait: when it was queued and, once its run
// was admitted, when that run began. Every drawing of an in-line merge's
// times (its queue tab, its head's clock) reads it, so they agree on when the
// wait ended.
type queueSpan struct {
	queuedMS   int64
	admitted   bool
	admittedMS int64
}

// queueSpanOf resolves one in-line merge's wait. A zero queued time is an
// invariant violation (enqueuedMS).
//
// An ADMITTED entry whose run is not registered yet is mid-admission (the
// pump admitted it and the run has not taken its slot): there is no start to
// end the wait at, and nothing is drawn from it (false); the admission's own
// drawing follows the moment the run exists.
func (o *orchestrator) queueSpanOf(entry wsm.MergeQueueEntry) (queueSpan, bool, error) {
	queuedMS, err := enqueuedMS(entry)
	if err != nil {
		return queueSpan{}, false, err
	}
	if entry.State != wsm.MergeAdmitted {
		return queueSpan{queuedMS: queuedMS}, true, nil
	}
	r, running := o.runFor(entry.Workspace)
	if !running {
		return queueSpan{}, false, nil
	}
	return queueSpan{queuedMS: queuedMS, admitted: true, admittedMS: r.startedMS}, true, nil
}

// queueTabState resolves one queued merge's queue tab badge. THE WAIT BEGAN
// WHEN THE MERGE WAS QUEUED, so the tab starts at the entry's queued time, and
// it ends the moment the merge reached the front: its run's start, which
// admission stamps. The end is that one instant on every republish, so a
// settled queue tab's drawn run time stays fixed while the run moves on.
func (o *orchestrator) queueTabState(entry wsm.MergeQueueEntry) (tabState, bool, error) {
	span, drawn, err := o.queueSpanOf(entry)
	if err != nil || !drawn {
		return tabState{}, drawn, err
	}
	return tabState{startedMS: span.queuedMS, settled: span.admitted, endedMS: span.admittedMS}, true, nil
}

// headClockMS resolves the instant one in-line merge's head clock counts from
// (frontend.v1.FeedMergeRuntime): WHEN IT WAS QUEUED while it waits, so a
// queue change never restarts it, and WHEN ITS RUN WAS ADMITTED once it has
// left the queue, so the head shows only the time it has been worked on. A
// mid-admission entry's head is not redrawn (false); the run draws it the
// moment it starts. The head's stage switches on the same admission: WAITING
// while it waits, MERGING once admitted (liveHead).
func (o *orchestrator) headClockMS(entry wsm.MergeQueueEntry) (int64, liveHead, bool, error) {
	span, clocked, err := o.queueSpanOf(entry)
	if err != nil || !clocked {
		return 0, liveHead{}, clocked, err
	}
	if span.admitted {
		return span.admittedMS, headMerging, true, nil
	}
	return span.queuedMS, headWaiting, true, nil
}

// queueStandings resolves each in-line entry's standing for the queue
// snapshot, in queue order: the FRONT's active tab and when it began, and for
// every entry behind it, that it waits and since when it was queued.
//
// The front's stage is its run's ACTIVE ROUND -- its label and the instant
// its ledger interval began, read whole -- so the snapshot's duration starts
// over at zero whenever the front's active tab changes. A front with no run
// (a paused queue, an admission not yet registered, a run already torn down)
// or whose run is still in its queue round is in its QUEUE stage, which began
// when it was queued: the same start its own queue tab carries.
//
// A missing queued time is an invariant violation (the column is NOT NULL and
// every enqueue stamps it), answered as an error; it is never sent as zero.
func (o *orchestrator) queueStandings(entries []wsm.MergeQueueEntry) ([]*frontendv1.FeedMergeQueueEntry, error) {
	standings := make([]*frontendv1.FeedMergeQueueEntry, len(entries))
	for i, entry := range entries {
		queuedMS, err := enqueuedMS(entry)
		if err != nil {
			return nil, err
		}
		if i > 0 {
			standings[i] = &frontendv1.FeedMergeQueueEntry{Status: &frontendv1.FeedMergeQueueEntry_Waiting{
				Waiting: &frontendv1.FeedMergeQueueWaiting{StageEnteredAtMs: queuedMS}}}
			continue
		}
		merging := &frontendv1.FeedMergeQueueMerging{ActiveTab: tabLabel(TabQueue, 1), StageEnteredAtMs: queuedMS}
		if r, running := o.runFor(entry.Workspace); running {
			if active := r.activeRound(); active.kind != "" && active.kind != TabQueue {
				merging = &frontendv1.FeedMergeQueueMerging{
					ActiveTab:        frontTabLabel(active),
					StageEnteredAtMs: active.started.UnixMilli(),
				}
			}
		}
		standings[i] = &frontendv1.FeedMergeQueueEntry{Status: &frontendv1.FeedMergeQueueEntry_Merging{Merging: merging}}
	}
	return standings, nil
}

// enqueuedMS answers when an entry was queued, in epoch milliseconds. A zero
// time is an invariant violation, never a stamp.
func enqueuedMS(entry wsm.MergeQueueEntry) (int64, error) {
	if entry.EnqueuedAt.IsZero() {
		return 0, fmt.Errorf("merge: the queue entry of %s in %s carries no queued time", entry.Workspace, entry.Repo)
	}
	return entry.EnqueuedAt.UnixMilli(), nil
}

// nowMS is the clock in the epoch milliseconds every drawn stamp uses.
func (o *orchestrator) nowMS() int64 { return o.deps.Now().UnixMilli() }

// heldByAnother answers the lease holding the workspace when ERR is a merge
// lease refused because a holder other than a merge holds it: a shim restart,
// a drain or a hibernation. That workspace is busy, not broken, and the merge
// waits for the lease to change. A merge lease held by another merge is not
// that case.
func heldByAnother(err error) (*wsm.LeaseHeldError, bool) {
	var other *wsm.LeaseHeldError
	if errors.As(err, &other) && other.Holder != wsm.HolderMerge {
		return other, true
	}
	return nil, false
}

// refuseNoopPause refuses a pause change that would change nothing: PAUSED
// names the asked-for state, so a pause refuses as already_paused and a resume
// as not_paused, with the matching reason. The refusal is the asker's typed
// answer rather than a daemon fault, so it is recorded at INFO under OP.
func (o *orchestrator) refuseNoopPause(op string, paused bool, message, notPausedReason, alreadyPausedReason string) *RefusalError {
	arm, reason := ArmNotPaused, notPausedReason
	if paused {
		arm, reason = ArmAlreadyPaused, alreadyPausedReason
	}
	o.deps.Log.Global().Info(op, message, dlog.Context{"arm": arm})
	return &RefusalError{Arm: arm, Reason: reason}
}
