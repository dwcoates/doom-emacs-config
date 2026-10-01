package merge

import (
	"context"
	"fmt"
	"os"
	"path/filepath"
	"strconv"
	"strings"
	"sync"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/feedid"
	"claude-repld/internal/gitclient"
	"claude-repld/internal/ids"
	"claude-repld/internal/promptqueue"
	"claude-repld/internal/prompts"
	"claude-repld/internal/resolve/footer"
	"claude-repld/internal/wsm"
)

// This file is ONE MERGE, from admission to its method.
//
// A MERGE RUNS IN THE WORKSPACE THAT ASKED FOR IT. r.ws is the REQUESTER: its
// feed carries the bubble, its footer and roster row the status, and its own
// session every prompt the merge submits -- the configured before- and
// after-merge prompts, the conflict resolution and the test fixing. What is
// merged is r.subject, which the request's source resolved (subject.go).
//
// THE SHIM THAT RESOLVES A MERGE IS THE REQUESTER'S OWN (owner ruling,
// 2026-09-28, kept by the 2026-09-30 model). Every brief a run submits is
// addressed to r.ws and to nothing else: a run holds no other session address,
// so there is no second shim for anything to reach.
//
// TWO METHODS, keyed by whether the target is the daemon's own repository. The
// EMACS REPO rebases the branch onto the target's tip, gates the rebased
// branch, and lands it as a non-fast-forward merge commit (process.go); EVERY
// OTHER REPO runs its configured prompts and nothing else -- landing, tests
// and PR work are the prompts' job there. A branch ALREADY MERGED UPSTREAM
// updates the default branch in the repository's main worktree, in any
// repository.

// run is one merge in flight.
type run struct {
	o *orchestrator
	// ws is the REQUESTING workspace, which the merge runs in.
	ws ids.WorkspaceID
	// source is what the request asked to merge; subject is what it resolved
	// to at admission.
	source  wsm.MergeSource
	subject subject
	// job is the requester's creation job: its configured prompts. A requester
	// with none (a registered checkout nothing created) has no configured
	// prompts, and its repository's policy decides.
	job wsm.CreationJob
	// repo is the queue this merge was admitted from.
	repo wsm.RepoKey
	// lease is the occupancy lease held for the merge's whole duration.
	lease wsm.Lease
	// lock is the repository's queue lock, held while the run holds its
	// repository's slot. It is guarded by the orchestrator's mutex; see slot.go.
	lock *repoLock
	// ctx is the run's own context. An ABANDON cancels it with the cause, so
	// whatever step the run is in stops, and the run ends on the abandoned
	// terminal. cancel is its cancel.
	ctx    context.Context
	cancel context.CancelCauseFunc
	// finished is closed once the run is wholly over, its teardown done. An
	// abandon waits on it, so its caller's answer is the released state.
	finished chan struct{}
	// displaces reports that the USER asked for this merge, which alone lets
	// the admission displace the workspace's turn in flight.
	displaces bool
	// releaseOccupancy drops the shim client's occupancy guard, nil when the
	// workspace merges sessionless.
	releaseOccupancy func()
	// emacsRepo reports whether the target is this daemon's own repository,
	// which is what selects the method.
	emacsRepo bool
	// selfCheckout reports whether the target is the daemon's OWN checkout
	// rather than a sibling worktree of it. Only the checkout itself triggers
	// the self-reload.
	selfCheckout bool
	// policy is where the requester's repository states its merge policy.
	policy prompts.Source
	// startedMS is the head's running clock.
	startedMS int64
	// displaced is the user turn this merge displaced, captured at admission
	// with the text it carried.
	displaced *Displaced
	// afterDrain reports that this run reached its terminal AFTER the shutdown
	// drain had already taken its snapshot. Set once, under the orchestrator's
	// mutex, by enterTerminal.
	afterDrain bool
	// queueRound is the ledger round of the QUEUE tab, opened at admission and
	// closed when the run leaves the queue for its first step.
	queueRound int

	mu sync.Mutex
	// rounds counts each tab kind's opened rounds, which is what makes a second
	// pass a second tab.
	rounds map[string]int
	// opened remembers when each round opened.
	opened map[string]time.Time
	// tab is the tab kind currently live.
	tab string
	// openRounds are the tab rounds whose ledger interval is still open, so an
	// abandon closes exactly those and nothing twice.
	openRounds map[string]bool
	// facts are this merge's standing facts, the footer's and the roster's.
	facts footer.MergeFacts
	// stepAt is when the current step began, which the merges waiting behind
	// this one date their "enqueued" line by.
	stepAt time.Time
	// tree is the current attempt's scratch tree, empty between attempts.
	tree string
	// attempts counts the scratch trees made, one per committing attempt.
	attempts int
	// machinery is the merge machinery the branch ALREADY changed when the
	// first repair began, nil until then. A repair that changes more of it is
	// refused (owner ruling, 2026-09-28): the merge must not change the gate
	// that is judging it.
	machinery map[string]bool
}

// activeTab reports the tab currently live, for the queue snapshot's front
// entry.
func (r *run) activeTab() string {
	r.mu.Lock()
	defer r.mu.Unlock()
	return r.tab
}

// roundOf reports a tab kind's current round.
func (r *run) roundOf(kind string) int {
	r.mu.Lock()
	defer r.mu.Unlock()
	if n := r.rounds[kind]; n > 0 {
		return n
	}
	return 1
}

// openTab opens the next round of one tab kind and records the interval's
// start in the ledger. Tabs are append-only, so opening is always a NEW round
// rather than a reopened tab.
func (r *run) openTab(ctx context.Context, kind string) int {
	started := r.o.deps.Now()
	r.mu.Lock()
	r.rounds[kind]++
	round := r.rounds[kind]
	r.tab = kind
	r.opened[roundKey(kind, round)] = started
	r.openRounds[roundKey(kind, round)] = true
	r.mu.Unlock()
	if err := r.o.deps.DB.RecordTabInterval(ctx, r.lease.ID, wsm.TabInterval{
		Round: round, Kind: kind, StartedAt: started,
	}); err != nil {
		r.o.log(ctx, r.ws).Error("daemon.merge.tab_open", "could not record a tab interval",
			dlog.Context{"workspace": string(r.ws), "tab": kind, "round": round, "error": err.Error()})
	}
	r.o.log(ctx, r.ws).Debug("daemon.merge.tab_open", "opened a merge tab",
		dlog.Context{"workspace": string(r.ws), "tab": kind, "round": round})
	return round
}

// closeTab records a tab interval's end and its outcome. The ledger holds the
// intervals and NOTHING of the content: step history is feed content the
// daemon synthesizes, never a WSM column.
func (r *run) closeTab(ctx context.Context, kind string, round int, outcome string) {
	ended := r.o.deps.Now()
	r.mu.Lock()
	started := r.opened[roundKey(kind, round)]
	delete(r.openRounds, roundKey(kind, round))
	r.mu.Unlock()
	if err := r.o.deps.DB.RecordTabInterval(ctx, r.lease.ID, wsm.TabInterval{
		Round: round, Kind: kind, StartedAt: started, EndedAt: &ended, Outcome: outcome,
	}); err != nil {
		r.o.log(ctx, r.ws).Error("daemon.merge.tab_close", "could not record a tab interval's end",
			dlog.Context{"workspace": string(r.ws), "tab": kind, "round": round, "error": err.Error()})
	}
}

// address stamps the requester's session output at one tab: the rows its
// session produces land on the merge sub-feed, parented to that tab's row. A
// REPAIR -- conflict resolution, test fixing -- is mirrored: its rows are ALSO
// drawn on the main feed as ordinary turns (owner ruling, 2026-09-29), while
// a configured prompt's stay in its tab. The feed resolver is merge-agnostic
// and applies the address without knowing what a merge is.
func (r *run) address(kind string, round int) {
	ref := tabRef(r.ws, r.lease.ID, kind, round)
	repair := kind == TabConflicts || kind == TabFixes
	r.o.deps.Feed.SetOutputAddress(r.ws, &wsm.OutputAddress{Feed: mergeFeed(r.lease.ID), Parent: &ref, Mirror: repair})
}

// upsert publishes one tab row.
func (r *run) upsert(kind string, round int, tab *frontendv1.FeedMergeTab) {
	r.o.deps.Feed.UpsertSynthesized(r.ws, mergeFeed(r.lease.ID), tabRow(r.ws, r.lease.ID, kind, round, tab))
}

// head publishes the bubble's head row with the state arm in force.
func (r *run) head(result any) {
	r.o.deps.Feed.UpsertSynthesized(r.ws, feedid.Feed{Root: true}, headRow(r.ws, r.lease.ID, r.label(), r.startedMS, result))
}

// label is the bubble's head line: what is merged, and where to.
func (r *run) label() string {
	return branchLabel(r.subject.branch, r.subject.targetLabel())
}

// runAdmitted is one admitted merge, from its start to its end: the run, then
// the release of everything it held. It answers the error the run ended on.
func (o *orchestrator) runAdmitted(ctx context.Context, repo wsm.RepoKey, ws ids.WorkspaceID, lock *repoLock) error {
	r, err := o.start(ctx, repo, ws, lock)
	if r == nil {
		return err
	}
	close(r.finished)
	return err
}

// start admits one workspace's merge: it takes the lease and the occupancy,
// opens the ledger, captures the displaced turn when the user asked for the
// merge, resolves what is merged, and runs the method the target selects. It
// answers the run, nil when it failed before one existed.
func (o *orchestrator) start(ctx context.Context, repo wsm.RepoKey, ws ids.WorkspaceID, lock *repoLock) (*run, error) {
	const op = "daemon.merge.start"
	log := o.log(ctx, ws)
	source, err := o.sourceOf(ctx, repo, ws)
	if err != nil {
		if releaseErr := lock.Release(); releaseErr != nil {
			log.Error(op, "could not release the repository lock of a merge that could not start", dlog.Context{"workspace": string(ws), "error": releaseErr.Error()})
		}
		log.Error(op, "could not read what the merge lands", dlog.Context{"workspace": string(ws), "error": err.Error()})
		return nil, err
	}
	job, _, err := o.deps.DB.CreationJob(ctx, ws)
	if err != nil {
		if releaseErr := lock.Release(); releaseErr != nil {
			log.Error(op, "could not release the repository lock of a merge that could not start", dlog.Context{"workspace": string(ws), "error": releaseErr.Error()})
		}
		log.Error(op, "could not read the requester's creation job", dlog.Context{"workspace": string(ws), "error": err.Error()})
		return nil, err
	}
	// THE OCCUPANCY IS TAKEN UNDER THE LEDGER IDENTITY minted when the merge
	// was put in line, so the queued bubble and the running one are one bubble.
	ledger := o.mintLedger(ws)
	lease, err := o.deps.DB.AcquireLeaseAs(ctx, ws, ledger, wsm.HolderMerge, wsm.PolicyRefuse)
	if err != nil {
		if releaseErr := lock.Release(); releaseErr != nil {
			log.Error(op, "could not release the repository lock of a merge that could not start", dlog.Context{"workspace": string(ws), "error": releaseErr.Error()})
		}
		log.Error(op, "could not take the merge lease", dlog.Context{"workspace": string(ws), "error": err.Error()})
		return nil, err
	}
	runCtx, cancel := context.WithCancelCause(ctx)
	o.mu.Lock()
	displaces := o.displaces[ws]
	delete(o.displaces, ws)
	o.mu.Unlock()
	r := &run{
		o: o, ws: ws, source: source, job: job, repo: repo, lease: lease, lock: lock,
		ctx: runCtx, cancel: cancel, finished: make(chan struct{}), displaces: displaces,
		startedMS:  o.deps.Now().UnixMilli(),
		rounds:     map[string]int{},
		opened:     map[string]time.Time{},
		openRounds: map[string]bool{},
	}
	return r, r.admit(runCtx)
}

// sourceOf reads what a queued merge lands off its queue entry.
func (o *orchestrator) sourceOf(ctx context.Context, repo wsm.RepoKey, ws ids.WorkspaceID) (wsm.MergeSource, error) {
	entries, err := o.deps.DB.MergeQueue(ctx, repo)
	if err != nil {
		return wsm.MergeSource{}, err
	}
	for _, entry := range entries {
		if entry.Workspace == ws {
			return entry.Source, nil
		}
	}
	return wsm.MergeSource{}, fmt.Errorf("merge: no queue entry of %s stands in %s", ws, repo)
}

// admit is start's body once the run exists: every failure from here ends the
// run through its one classification (end).
func (r *run) admit(ctx context.Context) error {
	const op = "daemon.merge.start"
	o, ws := r.o, r.ws
	log := o.log(ctx, ws)
	o.mu.Lock()
	o.running[r.repo] = r
	o.runsByWorkspace[ws] = r
	o.mu.Unlock()

	subject, err := o.resolveSubject(ctx, r)
	if err != nil {
		r.end(ctx, fmt.Errorf("could not resolve what the merge lands: %w", err))
		return err
	}
	r.subject = subject
	same, err := o.deps.Git.SameRepo(ctx, subject.targetDir, o.deps.SelfRepoDir)
	if err != nil {
		r.end(ctx, fmt.Errorf("could not identify the target repository: %w", err))
		return err
	}
	r.emacsRepo = same
	r.selfCheckout = same && sameDir(subject.targetDir, o.deps.SelfRepoDir)

	// THE REPOSITORY MAY STATE ITS OWN MERGE ACTIONS. Which directory those
	// come from is a fact about the requester's repository, so it is resolved
	// once here rather than re-derived by each step.
	policy, err := o.policyFor(ctx, ws)
	if err != nil {
		r.end(ctx, fmt.Errorf("could not resolve the repository's merge policy: %w", err))
		return err
	}
	r.policy = policy

	if release, ok, err := o.deps.Occupy(ws, holderMerge); err != nil {
		r.end(ctx, fmt.Errorf("could not take the session's occupancy: %w", err))
		return err
	} else if ok {
		r.releaseOccupancy = release
	}
	if err := o.deps.DB.OpenMergeLedger(ctx, ws, r.lease.ID); err != nil {
		r.end(ctx, fmt.Errorf("could not open the merge ledger: %w", err))
		return err
	}
	// THE TURN IN FLIGHT IS DISPLACED ONLY FOR THE USER'S OWN ASK. An agent's
	// ask was put in line only once its turn ended, and nothing is marked for
	// resubmission.
	if r.displaces {
		if displaced, captured, err := o.deps.CaptureDisplaced(ctx, ws); err != nil {
			r.end(ctx, fmt.Errorf("could not capture the displaced turn: %w", err))
			return err
		} else if captured {
			r.displaced = &displaced
		}
	} else {
		log.Debug(op, "the merge was not asked for by the user; the turn in flight is waited for, never displaced",
			dlog.Context{"workspace": string(ws), "lease": string(r.lease.ID)})
	}
	// THE TEST SEAM, NIL IN PRODUCTION. See Deps.PauseAfterCapture.
	if o.deps.PauseAfterCapture != nil {
		o.deps.PauseAfterCapture(ctx, ws)
	}

	// THE QUEUE IS A TAB. The merge is the one being worked on now, so it is
	// no longer counted among the waiting; it stands "next" until its first
	// step begins.
	r.queueRound = r.openTab(ctx, TabQueue)
	r.setStep(ctx, footer.StepEnqueued, func(f *footer.MergeFacts) { f.QueuePlace, f.QueueWaiting = 1, 1 })
	r.head(nil)
	if err := o.republishQueue(ctx, r.repo); err != nil {
		r.end(ctx, fmt.Errorf("could not publish the queue: %w", err))
		return err
	}
	log.Info(op, "admitted a merge", dlog.Context{
		"workspace": string(ws), "repo": string(r.repo), "lease": string(r.lease.ID),
		"source": r.source.Kind.String(), "branch": subject.branch, "worktree": subject.dir, "target": subject.targetDir,
		"method": methodName(r.emacsRepo, r.source), "self_checkout": r.selfCheckout, "displaces": r.displaces,
	})
	if err := r.awaitFree(ctx, ws); err != nil {
		r.end(ctx, err)
		return err
	}
	if r.subject.other != "" {
		if err := r.awaitFree(ctx, r.subject.other); err != nil {
			r.end(ctx, err)
			return err
		}
	}
	return r.execute(ctx)
}

// awaitFree holds an admitted merge until one workspace is free: no turn in
// flight and no live detached work -- the requester, whose session the merge
// drives, and another workspace whose worktree it rebases in.
//
// THE MERGE WAITS; IT NEVER KILLS. Detached work ends only by its own per-task
// stop or a forced kill the user explicitly asked for, so the merge waits, on
// the same watcher-driven freeness the rollout's relaunch waits on, for as
// long as the work runs.
//
// A wait that ends without the workspace falling free is returned, never
// swallowed; the caller records it once, as an abort or a stop.
func (r *run) awaitFree(ctx context.Context, ws ids.WorkspaceID) error {
	const op = "daemon.merge.await_free"
	log := r.o.log(ctx, r.ws)
	fields := dlog.Context{"workspace": string(r.ws), "awaited": string(ws), "lease": string(r.lease.ID)}
	if r.o.deps.Freeness.Free(ws) {
		log.Debug(op, "the workspace is free; the merge proceeds", fields)
		return nil
	}
	log.Info(op, "the merge waits for the workspace's turn and detached work to end; nothing is stopped to hurry it", fields)
	if err := r.o.deps.Freeness.AwaitFree(ctx, ws); err != nil {
		return fmt.Errorf("the workspace %s never fell free: %w", ws, err)
	}
	log.Info(op, "the workspace fell free; the merge proceeds", fields)
	return nil
}

// roundKey addresses one tab round's recorded start.
func roundKey(kind string, round int) string { return fmt.Sprintf("%s/%d", kind, round) }

// splitRoundKey reads a roundKey back into its kind and round.
func splitRoundKey(key string) (string, int, bool) {
	kind, n, found := strings.Cut(key, "/")
	if !found {
		return "", 0, false
	}
	round, err := strconv.Atoi(n)
	if err != nil {
		return "", 0, false
	}
	return kind, round, true
}

// methodName names the method a run took, for the log record.
func methodName(emacsRepo bool, source wsm.MergeSource) string {
	switch {
	case source.Kind == wsm.MergeSourceMergedUpstream:
		return "merged_upstream"
	case emacsRepo:
		return "emacs_repo"
	}
	return "other_repo"
}

// sameDir reports whether two directory spellings name one directory. The
// self-reload fires for the daemon's OWN checkout only: a sibling worktree of
// the same repository shares its common dir but is not the tree the running
// binary was built from.
func sameDir(a, b string) bool {
	ra, err := filepath.EvalSymlinks(filepath.Clean(a))
	if err != nil {
		ra = filepath.Clean(a)
	}
	rb, err := filepath.EvalSymlinks(filepath.Clean(b))
	if err != nil {
		rb = filepath.Clean(b)
	}
	return ra == rb
}

// execute runs the method the source and the target select, and lands
// whatever end it reaches on the one terminal path.
func (r *run) execute(ctx context.Context) error {
	// The run leaves the queue here: the wait is over and the first step
	// begins, so the queue interval ends.
	r.closeTab(ctx, TabQueue, r.queueRound, "succeeded")
	outcome, err := r.method(ctx)
	if err != nil {
		r.end(ctx, err)
		return err
	}
	// THE TERMINAL IS NOT THE STEP'S TO CANCEL. An abandon arriving once the
	// run has concluded changes nothing: the landing or failure is recorded
	// whole.
	return r.finish(context.WithoutCancel(ctx), outcome)
}

// end is the ONE classification of a run that stopped short of its own
// conclusion, and every such stop comes through it: an ABANDON (the run's
// context carries the abandon cause), a git THIS daemon cancelled, and
// everything else -- a real failure, in the area "other". Each takes the one
// teardown, on a context the stop itself cannot cancel.
func (r *run) end(ctx context.Context, err error) {
	settle := context.WithoutCancel(ctx)
	if cause, abandoned := abandonCauseOf(ctx); abandoned {
		r.abandonTerminal(settle, cause)
		return
	}
	if r.stopped(settle, err) {
		return
	}
	r.abort(settle, err.Error())
}

// outcome is how a method ended.
type outcome struct {
	// landed is the commit the target now carries, empty when nothing landed
	// locally.
	landed string
	// commits are what the merge brought in, for the self-reload's classifier.
	commits []gitclient.Commit
	// failed is the failure's one-line account, empty on success; area is
	// where it failed.
	failed string
	area   footer.MergeFailedArea
	// abandoned is the cause a RUNNING merge was taken out for, empty unless
	// it was.
	abandoned AbandonCause
	// alreadyOn names the target branch a source branch was ALREADY
	// contained in: the merge concludes as merged with nothing to land.
	alreadyOn string
}

// failedIn is the outcome of a merge that gave up in one area.
func failedIn(area footer.MergeFailedArea, summary string) outcome {
	return outcome{failed: summary, area: area}
}

// method dispatches to the run's method.
func (r *run) method(ctx context.Context) (outcome, error) {
	if r.source.Kind == wsm.MergeSourceMergedUpstream {
		// A BRANCH ALREADY MERGED UPSTREAM: updating main, then the after-merge
		// prompts. There is nothing to prepare, rebase or gate.
		out, err := r.updateMain(ctx)
		if err != nil || out.failed != "" {
			return out, err
		}
		r.afterAction(ctx)
		return out, nil
	}
	if out, gaveUp, err := r.prePrompt(ctx); err != nil || gaveUp {
		return out, err
	}
	if !r.emacsRepo {
		// EVERY OTHER REPO: nothing between the two prompts. Landing, tests and
		// PR work belong to the prompts there.
		r.afterAction(ctx)
		return outcome{}, nil
	}
	out, err := r.process(ctx)
	if err != nil || out.failed != "" {
		return out, err
	}
	r.afterAction(ctx)
	return out, nil
}

// afterAction runs the after-merge prompts for EITHER method. A FAILURE never
// fails the run -- every commit has landed by now -- it is surfaced as this
// WARN, with the failure's own text, and the post-prompt tab has already
// settled failed with its composed summary.
func (r *run) afterAction(ctx context.Context) {
	if err := r.postPrompt(ctx); err != nil {
		r.o.log(ctx, r.ws).Warn("daemon.merge.post_prompt", "the post-merge prompt failed; the merge still landed",
			dlog.Context{"workspace": string(r.ws), "error": err.Error()})
	}
}

// actions answers the prompts one merge step submits: the ones the CREATION
// recorded, and otherwise the repository's own policy brief when it states
// one.
//
// A WIRE-SUPPLIED ACTION WINS. The repository's file fills an EMPTY slot; it
// never overrides an action a create configured.
//
// An ABSENT policy brief is not an error. A brief that is present and will not
// load IS one: a stated policy that cannot run is a fault, never a silent skip.
func (r *run) actions(configured []string, brief string) ([]string, error) {
	if len(configured) > 0 {
		return configured, nil
	}
	if r.policy.Dir == "" {
		return nil, nil
	}
	if len(r.o.deps.Policy.Missing(r.policy.Dir, []string{brief})) > 0 {
		return nil, nil
	}
	text, err := r.o.deps.Policy.Text(r.policy.Dir, brief)
	if err != nil {
		return nil, fmt.Errorf("merge: reading the repository's %s policy in %s: %w", brief, r.policy.Dir, err)
	}
	return []string{text}, nil
}

// prePrompt runs the configured before-merge prompts: the "preprocessing"
// step. THEIR FAILURE FAILS THE MERGE (area "other"): they are the
// requester's own precondition for merging, so merging past one would land
// work its author said was not ready.
func (r *run) prePrompt(ctx context.Context) (outcome, bool, error) {
	actions, err := r.actions(r.job.Actions.Before, prompts.PolicyMergeBefore)
	if err != nil {
		return outcome{}, false, err
	}
	for _, text := range actions {
		round := r.openTab(ctx, TabPrePrompt)
		r.address(TabPrePrompt, round)
		r.setStep(ctx, footer.StepPreprocessing, func(f *footer.MergeFacts) { f.Line = promptLine(footer.StepPreprocessing, text) })
		r.upsert(TabPrePrompt, round, promptTab(TabPrePrompt, live(), 0, ""))
		close, err := r.runConfiguredPrompt(ctx, text, conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_BEFORE_ACTION)
		if err != nil || close.Failed() {
			summary := "the before-merge prompt did not complete"
			r.upsert(TabPrePrompt, round, promptTab(TabPrePrompt, nil, r.o.nowMS(), summary))
			r.closeTab(ctx, TabPrePrompt, round, "failed")
			if err != nil {
				return outcome{}, false, err
			}
			r.o.log(ctx, r.ws).Warn("daemon.merge.pre_prompt", "the before-merge prompt did not complete; the merge fails",
				dlog.Context{"workspace": string(r.ws), "close": close.String()})
			return failedIn(footer.FailedOther, summary), true, nil
		}
		r.upsert(TabPrePrompt, round, promptTab(TabPrePrompt, nil, r.o.nowMS(), ""))
		r.closeTab(ctx, TabPrePrompt, round, "succeeded")
	}
	return outcome{}, false, nil
}

// postPrompt runs the configured after-merge prompts: the "postprocessing"
// step. THEIR FAILURE NEVER FAILS THE RUN -- every commit has landed by now.
func (r *run) postPrompt(ctx context.Context) error {
	actions, err := r.actions(r.job.Actions.After, prompts.PolicyMergeAfter)
	if err != nil {
		return err
	}
	for _, text := range actions {
		round := r.openTab(ctx, TabPostPrompt)
		r.address(TabPostPrompt, round)
		r.setStep(ctx, footer.StepPostprocessing, func(f *footer.MergeFacts) { f.Line = promptLine(footer.StepPostprocessing, text) })
		r.upsert(TabPostPrompt, round, promptTab(TabPostPrompt, live(), 0, ""))
		close, err := r.runConfiguredPrompt(ctx, text, conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_AFTER_ACTION)
		if err != nil || close.Failed() {
			summary := "the after-merge prompt did not complete"
			r.upsert(TabPostPrompt, round, promptTab(TabPostPrompt, nil, r.o.nowMS(), summary))
			r.closeTab(ctx, TabPostPrompt, round, "failed")
			if err != nil {
				return err
			}
			return fmt.Errorf("merge: %s", summary)
		}
		r.upsert(TabPostPrompt, round, promptTab(TabPostPrompt, nil, r.o.nowMS(), ""))
		r.closeTab(ctx, TabPostPrompt, round, "succeeded")
	}
	return nil
}

// runConfiguredPrompt starts a session if the workspace has none -- a
// configured prompt is what revives it -- then submits the prompt and waits
// for its turn to end. THE ACTION IS THE PROMPT, NOT A PROMPT'S NAME: the
// recorded text is submitted verbatim.
func (r *run) runConfiguredPrompt(ctx context.Context, prompt string, origin conversationv1.PromptOrigin) (wsm.TurnClose, error) {
	if err := r.ensureSession(ctx); err != nil {
		return wsm.CloseFailed, fmt.Errorf("merge: starting a session for a configured prompt: %w", err)
	}
	return r.submit(ctx, prompt, origin)
}

// ensureSession starts the requester's session when it has none, under the
// lease: a merge's prompt or repair is what revives it.
func (r *run) ensureSession(ctx context.Context) error {
	if _, found, err := r.o.deps.DB.Session(ctx, r.ws); err != nil {
		return err
	} else if !found {
		return r.o.deps.StartSession(ctx, r.ws)
	}
	return nil
}

// submit sends one prompt down the queue's ONE delivery path, to the
// REQUESTER's own session, and waits for its turn to end. The merge's prompts
// are ordinary submissions with a merge origin; nothing about them bypasses
// the queue.
func (r *run) submit(ctx context.Context, text string, origin conversationv1.PromptOrigin) (wsm.TurnClose, error) {
	turn := wsm.NewTurnID()
	disposition, err := r.o.deps.Queue.Submit(ctx, promptqueue.Submission{
		WS: r.ws, Turn: turn, Said: saidText(text), Origin: origin,
	})
	if err != nil {
		return wsm.CloseFailed, err
	}
	if disposition.RefusedArm != "" {
		return wsm.CloseFailed, fmt.Errorf("merge: the queue refused a merge prompt: %s", disposition.RefusedArm)
	}
	return r.o.deps.AwaitTurnEnd(ctx, r.ws, turn)
}

// promptTab builds an agentic prompt tab's kind arm.
func promptTab(kind string, liveState *frontendv1.FeedMergeTabLive, endedMS int64, failure string) *frontendv1.FeedMergeTab {
	settled := func() *frontendv1.FeedMergeTabSettled {
		if failure != "" {
			return settledFailed(endedMS, failure)
		}
		return settledOK(endedMS)
	}
	if kind == TabPostPrompt {
		inner := &frontendv1.FeedMergeTabPostPrompt{}
		if liveState != nil {
			inner.State = &frontendv1.FeedMergeTabPostPrompt_Live{Live: liveState}
		} else {
			inner.State = &frontendv1.FeedMergeTabPostPrompt_Settled{Settled: settled()}
		}
		return &frontendv1.FeedMergeTab{Kind: &frontendv1.FeedMergeTab_PostPrompt{PostPrompt: inner}}
	}
	inner := &frontendv1.FeedMergeTabPrePrompt{}
	if liveState != nil {
		inner.State = &frontendv1.FeedMergeTabPrePrompt_Live{Live: liveState}
	} else {
		inner.State = &frontendv1.FeedMergeTabPrePrompt_Settled{Settled: settled()}
	}
	return &frontendv1.FeedMergeTab{Kind: &frontendv1.FeedMergeTab_PrePrompt{PrePrompt: inner}}
}

// readEscalation reports whether the fixing agent wrote the escalation record,
// and returns what it said. The marker must be the file's FIRST line: the
// daemon parses exactly the constant it substituted, so an edited brief cannot
// drift into instructing an agent to write a record nothing reads.
func readEscalation(dir string) (string, bool) {
	body, err := os.ReadFile(filepath.Join(dir, EscalationFile))
	if err != nil {
		return "", false
	}
	text := string(body)
	first, rest, _ := strings.Cut(text, "\n")
	if strings.TrimSpace(first) != EscalationMarker {
		return "", false
	}
	return strings.TrimSpace(rest), true
}
