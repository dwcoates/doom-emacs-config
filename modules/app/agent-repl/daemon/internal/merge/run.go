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
	"claude-repld/internal/wsm"
)

// This file is ONE MERGE, from admission to teardown.
//
// TWO METHODS, keyed by whether the target is the daemon's own repository. The
// EMACS REPO runs pre-prompt → no-ff merge → conflicts → tests → fixes →
// post-prompt: a merge commit is one commit to apply and one to revert, and the
// gate runs on the tree that commit produced. EVERY OTHER REPO runs pre-prompt
// → post-prompt and nothing else — landing, tests and PR work are the prompts'
// job there, because a repository whose changes go through a CI merge queue
// would have its commits duplicated by a local landing.
//
// The per-repo queue and the terminal/teardown path are IDENTICAL for both.
//
// THE QUEUE OWNS THE TARGET. The Emacs-repo method never merges in the target
// checkout: every ATTEMPT makes a scratch tree of the queue's own at the
// target's tip, merges and gates there, and only a gate that passed moves the
// target, by fast-forward. A failed, parked or abandoned merge leaves the
// target exactly as it was (2026-09-28: the merge commit, a repair commit and
// an escalation file all landed in the live master checkout mid-run). Repairs
// are the workspace agent's, on its own branch in its own worktree, so the
// next attempt -- after a repair, after a park, after the target moved --
// is simply the merge made again on the target's current tip.
//
// THE SHIM THAT RESOLVES A MERGE IS THE WORKSPACE AGENT'S OWN (owner ruling,
// 2026-09-28). Every brief a run submits and every guidance it routes is
// addressed to r.ws -- the merging workspace -- and to nothing else: a run
// holds no other session address, so there is no second shim for anything
// to reach. merge_test's TestEveryMergeDeliveryReachesTheWorkspacesOwnSession
// holds that.

// run is one merge in flight.
type run struct {
	o   *orchestrator
	ws  ids.WorkspaceID
	job wsm.CreationJob
	// repo is the queue this merge was admitted from.
	repo wsm.RepoKey
	// lease is the occupancy lease held for the merge's whole duration.
	lease wsm.Lease
	// lock is the repository's queue lock, held while the run holds its
	// repository's slot and nil while it does not (parked). It is guarded by
	// the orchestrator's mutex; see slot.go.
	lock *repoLock
	// tenancy is the grant of the slot the run holds now, nil while it holds
	// none. The pump that granted it waits on it. Guarded by o.mu.
	tenancy *tenancy
	// granted is signalled when the pump hands a waiting run its slot back.
	granted chan struct{}
	// ctx is the run's own context. An ABANDON cancels it with the cause, so
	// whatever phase the run is in stops, and the run ends on the abandoned
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
	// policy is where this workspace's repository states its merge policy.
	// The DIRECTORY is resolved at admission; the briefs in it are read at
	// use time, so editing one takes effect on the next merge.
	policy prompts.Source
	// startedMS is the head's running clock.
	startedMS int64
	// displaced is the user turn this merge displaced, captured at admission
	// with the text it carried.
	displaced *Displaced
	// afterDrain reports that this run reached its terminal AFTER the shutdown
	// drain had already taken its snapshot, so NOTHING is waiting for it and
	// the state client is closing under it. Set once, under the
	// orchestrator's mutex, by enterTerminal.
	afterDrain bool
	// queueRound is the ledger round of the QUEUE tab, opened at admission and
	// closed when the run leaves the queue for its first phase. The queue is a
	// tab like every other, so its interval is recorded like every other's.
	queueRound int

	mu sync.Mutex
	// rounds counts each tab kind's opened rounds, which is what makes a second
	// pass a second tab.
	rounds map[string]int
	// opened remembers when each round opened, so a closed ledger interval is a
	// real interval rather than an instant.
	opened map[string]time.Time
	// tab is the tab kind currently live.
	tab string
	// openRounds are the tab rounds whose ledger interval is still open, so
	// an abandon closes exactly those and nothing twice.
	openRounds map[string]bool
	// parked reports that the run is parked: it holds no slot and waits for
	// its guidance. Guarded by mu.
	parked bool
	// attempts counts the merge's attempts, each a scratch tree of its own.
	attempts int
	// tree is the current attempt's scratch tree, empty between attempts.
	tree string
	// machinery is the merge machinery the branch ALREADY changed when the
	// first repair began, nil until then. A repair that changes more of it is
	// refused (owner ruling, 2026-09-28): the merge must not change the gate
	// that is judging it.
	machinery map[string]bool
	// guidance carries a parked submission from RouteParked into the waiting
	// phase. It is unbuffered: guidance is delivered to a phase that is waiting
	// for it, never queued behind one that is not. Each carries its own reply,
	// so an answer is never left for a caller that stopped listening.
	guidance chan guidance
	// conflictBriefed records the conflict the agent has already been handed,
	// so a conflict reaches the agent EXACTLY ONCE per source tip.
	conflictBriefed map[string]bool
}

// isParked reports whether the run is parked right now.
func (r *run) isParked() bool {
	r.mu.Lock()
	defer r.mu.Unlock()
	return r.parked
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
	r.facts(StateMerging, "")
	return round
}

// closeTab records a tab interval's end and its outcome. The ledger holds the
// intervals and NOTHING of the content: phase history is feed content the
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

// facts republishes this merge's facts with the tab in force.
func (r *run) facts(state, parkedLine string) {
	r.mu.Lock()
	tab, round := r.tab, r.rounds[r.tab]
	r.mu.Unlock()
	existing, _ := r.o.Facts(r.ws)
	existing.State = state
	existing.Round = round
	existing.ActiveTab = tab
	existing.ParkedLine = parkedLine
	r.o.publish(r.ws, existing)
}

// address stamps the session's output at one tab: rows the lease's session
// produces land on the merge sub-feed, parented to that tab's row. The feed
// resolver is merge-agnostic and applies the address without knowing what a
// merge is.
func (r *run) address(kind string, round int) {
	ref := tabRef(r.ws, r.lease.ID, kind, round)
	r.o.deps.Feed.SetOutputAddress(r.ws, &wsm.OutputAddress{Feed: mergeFeed(r.lease.ID), Parent: &ref})
}

// upsert publishes one tab row.
func (r *run) upsert(kind string, round int, tab *frontendv1.FeedMergeTab) {
	r.o.deps.Feed.UpsertSynthesized(r.ws, mergeFeed(r.lease.ID), tabRow(r.ws, r.lease.ID, kind, round, tab))
}

// head publishes the bubble's head row with the state arm in force.
func (r *run) head(result any) {
	r.o.deps.Feed.UpsertSynthesized(r.ws, feedid.Feed{Root: true}, headRow(r.ws, r.lease.ID,
		branchLabel(r.job.Layout.SourceBranch, r.job.Layout.TargetDir), r.startedMS, result))
}

// runAdmitted is one admitted merge's goroutine: the run, then the report of
// its end to whichever grant of the slot it holds by then. A run that parked
// and was granted the slot again reports to that newer grant; a run that ends
// holding none (abandoned while parked) reports to nobody, because nobody
// waits.
func (o *orchestrator) runAdmitted(ctx context.Context, repo wsm.RepoKey, ws ids.WorkspaceID, lock *repoLock, t *tenancy) {
	r, err := o.start(ctx, repo, ws, lock, t)
	if r == nil {
		t.back <- tenancyEnd{err: err}
		return
	}
	if held := r.takeTenancy(); held != nil {
		held.back <- tenancyEnd{err: err}
	}
	close(r.finished)
}

// start admits one workspace's merge: it takes the lease and the occupancy,
// opens the ledger, addresses the session's output at the bubble, captures the
// displaced turn when the user asked for the merge, and runs the method the
// target selects. It answers the run, nil when it failed before one existed.
func (o *orchestrator) start(ctx context.Context, repo wsm.RepoKey, ws ids.WorkspaceID, lock *repoLock, t *tenancy) (*run, error) {
	const op = "daemon.merge.start"
	log := o.log(ctx, ws)
	job, err := o.layoutFor(ctx, ws)
	if err != nil {
		lock.Release()
		log.Error(op, "could not read the merge's geometry", dlog.Context{"workspace": string(ws), "error": err.Error()})
		return nil, err
	}
	// THE OCCUPANCY IS TAKEN UNDER THE LEDGER IDENTITY minted at enqueue, so
	// the queued bubble and the running one are one bubble.
	ledger := o.mintLedger(ws)
	lease, err := o.deps.DB.AcquireLeaseAs(ctx, ws, ledger, wsm.HolderMerge, wsm.PolicyRefuse)
	if err != nil {
		lock.Release()
		log.Error(op, "could not take the merge lease", dlog.Context{"workspace": string(ws), "error": err.Error()})
		return nil, err
	}
	runCtx, cancel := context.WithCancelCause(ctx)
	o.mu.Lock()
	displaces := o.displaces[ws]
	delete(o.displaces, ws)
	o.mu.Unlock()
	r := &run{
		o: o, ws: ws, job: job, repo: repo, lease: lease, lock: lock, tenancy: t,
		ctx: runCtx, cancel: cancel, finished: make(chan struct{}), displaces: displaces,
		startedMS:       o.deps.Now().UnixMilli(),
		rounds:          map[string]int{},
		opened:          map[string]time.Time{},
		openRounds:      map[string]bool{},
		guidance:        make(chan guidance),
		conflictBriefed: map[string]bool{},
	}
	return r, r.admit(runCtx)
}

// admit is start's body once the run exists: every failure from here ends the
// run through its one classification (end).
func (r *run) admit(ctx context.Context) error {
	const op = "daemon.merge.start"
	o, ws, job := r.o, r.ws, r.job
	log := o.log(ctx, ws)
	same, err := o.deps.Git.SameRepo(ctx, job.Layout.TargetDir, o.deps.SelfRepoDir)
	if err != nil {
		// A git WE cancelled is not a repository we could not identify: at
		// shutdown that abort recorded a merge failure that never happened.
		r.end(ctx, fmt.Errorf("could not identify the target repository: %w", err))
		return err
	}
	r.emacsRepo = same
	r.selfCheckout = same && sameDir(job.Layout.TargetDir, o.deps.SelfRepoDir)

	// THE REPOSITORY MAY STATE ITS OWN MERGE ACTIONS. Which directory those
	// come from is a fact about the workspace's repository, so it is resolved
	// once here rather than re-derived by each phase.
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
	// ask comes from that very turn, so its merge waits for the turn to end
	// (awaitFree, below) and nothing is marked for resubmission.
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
	// THE TEST SEAM, NIL IN PRODUCTION. See Deps.PauseAfterCapture: it holds a
	// run in the window between the capture and everything that would close it,
	// which is what makes the crash-after-capture recovery testable.
	if o.deps.PauseAfterCapture != nil {
		o.deps.PauseAfterCapture(ctx, ws)
	}
	o.mu.Lock()
	o.running[r.repo] = r
	o.runsByWorkspace[ws] = r
	o.mu.Unlock()

	// THE QUEUE IS A TAB. Opening it through openTab is what puts its interval
	// in the ledger; setting the fields by hand recorded nothing at all.
	r.queueRound = r.openTab(ctx, TabQueue)
	r.head(nil)
	if err := o.republishQueue(ctx, r.repo); err != nil {
		r.end(ctx, fmt.Errorf("could not publish the queue: %w", err))
		return err
	}
	log.Debug(op, "admitted a merge", dlog.Context{
		"workspace": string(ws), "repo": string(r.repo), "lease": string(r.lease.ID),
		"method": methodName(r.emacsRepo), "self_checkout": r.selfCheckout, "displaces": r.displaces,
	})
	if err := r.awaitFree(ctx); err != nil {
		r.end(ctx, err)
		return err
	}
	return r.execute(ctx)
}

// awaitFree holds an admitted merge in its queue until the workspace is free:
// no turn in flight and no live detached work.
//
// THE MERGE WAITS; IT NEVER KILLS. The displaced turn was ended unforced, so
// whatever it spawned — background agents, shells, monitors — runs on. The
// merge is about to drive this session with its own briefs and, on landing, to
// stop it and remove its worktree, so it cannot proceed underneath that work:
// it would race it for the conversation and then take its working directory
// away. And it may not stop it either, because detached work ends only by its
// own per-task stop or a forced kill the user explicitly asked for. So it
// waits, on the same watcher-driven freeness the rollout's relaunch waits on,
// for as long as the work runs. The user ends the wait by letting the work
// finish or by stopping it themselves.
//
// A wait that ends without the workspace falling free is returned, never
// swallowed; the caller records it once, as an abort or a stop.
func (r *run) awaitFree(ctx context.Context) error {
	const op = "daemon.merge.await_free"
	log := r.o.log(ctx, r.ws)
	fields := dlog.Context{"workspace": string(r.ws), "lease": string(r.lease.ID)}
	if r.o.deps.Freeness.Free(r.ws) {
		log.Debug(op, "the workspace is free; the merge proceeds", fields)
		return nil
	}
	log.Info(op, "the merge waits for the workspace's turn and detached work to end; nothing is stopped to hurry it", fields)
	if err := r.o.deps.Freeness.AwaitFree(ctx, r.ws); err != nil {
		return fmt.Errorf("the workspace never fell free: %w", err)
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
func methodName(emacsRepo bool) string {
	if emacsRepo {
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

// execute runs the method the target selects, and lands whatever end it
// reaches on the one terminal path.
func (r *run) execute(ctx context.Context) error {
	// The run leaves the queue here: the wait is over and the first phase
	// begins, so the queue interval ends.
	r.closeTab(ctx, TabQueue, r.queueRound, "succeeded")
	outcome, err := r.method(ctx)
	if err != nil {
		r.end(ctx, err)
		return err
	}
	// THE TERMINAL IS NOT THE PHASE'S TO CANCEL. An abandon arriving once the
	// run has concluded changes nothing: the landing or failure is recorded
	// whole.
	return r.finish(context.WithoutCancel(ctx), outcome)
}

// end is the ONE classification of a run that stopped short of its own
// conclusion, and every such stop comes through it: an ABANDON (the run's
// context carries the abandon cause), the daemon leaving a PARKED run behind,
// a git THIS daemon cancelled, and everything else -- a real failure. Each
// takes the one teardown, on a context the stop itself cannot cancel.
func (r *run) end(ctx context.Context, err error) {
	settle := context.WithoutCancel(ctx)
	if cause, abandoned := abandonCauseOf(ctx); abandoned {
		r.abandonTerminal(settle, cause)
		return
	}
	if ctx.Err() != nil && r.isParked() {
		// A PARKED RUN THE DAEMON LEFT is not a failure: nothing of it was
		// running, and the boot recovery owns its lease.
		r.enterTerminal(terminalOwedFailed)
		defer r.leaveTerminal()
		r.abandonToRecovery(settle, terminalOwedFailed)
		return
	}
	if r.stopped(settle, err) {
		return
	}
	r.abort(settle, err.Error())
}

// outcome is how a method ended.
type outcome struct {
	// landed is the merge commit, empty when nothing landed locally.
	landed string
	// commits are what the merge brought in, for the self-reload's classifier.
	commits []gitclient.Commit
	// failed is the failure's one-line account, empty on success.
	failed string
	// abandoned is the cause a RUNNING merge was taken out for, empty unless
	// it was.
	abandoned AbandonCause
	// alreadyOn names the target branch a source branch was ALREADY
	// contained in: the merge concludes as merged with nothing to land.
	alreadyOn string
}

// method dispatches to the run's method.
func (r *run) method(ctx context.Context) (outcome, error) {
	if err := r.prePrompt(ctx); err != nil {
		return outcome{}, err
	}
	if !r.emacsRepo {
		// EVERY OTHER REPO: nothing between the two prompts. Landing, tests and
		// PR work belong to the prompts there.
		r.afterAction(ctx)
		return outcome{}, nil
	}
	out, err := r.emacsMethod(ctx)
	if err != nil || out.failed != "" {
		return out, err
	}
	r.afterAction(ctx)
	return out, nil
}

// afterAction runs the post-merge prompts for EITHER method. It is one helper
// on purpose: the two methods once carried their own spelling of this and drifted,
// and every other repository's merge failed on an after-action the contract says
// can never fail a run. A FAILURE never fails the run — it is surfaced as this
// WARN, with the failure's own text, and the post-prompt tab has already settled
// failed with its composed summary.
func (r *run) afterAction(ctx context.Context) {
	if err := r.postPrompt(ctx); err != nil {
		r.o.log(ctx, r.ws).Warn("daemon.merge.post_prompt", "the post-merge prompt failed; the merge still landed",
			dlog.Context{"workspace": string(r.ws), "error": err.Error()})
	}
}

// emacsMethod makes the merge, attempt after attempt, until one lands. An
// attempt that asks for a repair or a park is followed by a fresh attempt on
// the target's tip as it then stands, which is what makes a resumed parked
// merge rebase onto a target the merges behind it moved (owner ruling,
// 2026-09-28).
func (r *run) emacsMethod(ctx context.Context) (outcome, error) {
	for {
		next, err := r.attempt(ctx)
		if err != nil {
			return outcome{}, err
		}
		switch {
		case next.done != nil:
			return *next.done, nil
		case next.park != nil:
			if err := r.parkUntilResumed(ctx, *next.park); err != nil {
				return outcome{}, err
			}
			if err := r.reacquire(ctx); err != nil {
				return outcome{}, err
			}
		}
	}
}

// actions answers the prompts one merge phase submits: the ones the CREATION
// recorded, and otherwise the repository's own policy brief when it states
// one.
//
// A WIRE-SUPPLIED ACTION WINS. The repository's file fills an EMPTY slot; it
// never overrides an action a create configured, because the create's actions
// are that workspace's own statement and the file is the repository's default
// for workspaces that made none.
//
// An ABSENT policy brief is not an error — it is what every repository looked
// like before this policy existed. A brief that is present and will not load
// IS one: a stated policy that cannot run is a fault, never a silent skip.
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

// prePrompt runs the configured before-merge prompts. THEIR FAILURE FAILS THE
// RUN: they are the workspace's own precondition for merging, so merging past
// one would land work its author said was not ready.
func (r *run) prePrompt(ctx context.Context) error {
	actions, err := r.actions(r.job.Actions.Before, prompts.PolicyMergeBefore)
	if err != nil {
		return err
	}
	for _, name := range actions {
		round := r.openTab(ctx, TabPrePrompt)
		r.address(TabPrePrompt, round)
		r.upsert(TabPrePrompt, round, promptTab(TabPrePrompt, &frontendv1.FeedMergeTabLive{}, 0, ""))
		close, err := r.runConfiguredPrompt(ctx, name, conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_BEFORE_ACTION)
		if err != nil || close.Failed() {
			summary := fmt.Sprintf("the before-merge prompt %q did not complete", name)
			r.upsert(TabPrePrompt, round, promptTab(TabPrePrompt, nil, r.o.nowMS(), summary))
			r.closeTab(ctx, TabPrePrompt, round, "failed")
			if err != nil {
				return err
			}
			return fmt.Errorf("merge: %s", summary)
		}
		r.upsert(TabPrePrompt, round, promptTab(TabPrePrompt, nil, r.o.nowMS(), ""))
		r.closeTab(ctx, TabPrePrompt, round, "succeeded")
	}
	return nil
}

// postPrompt runs the configured after-merge prompts. THEIR FAILURE NEVER FAILS
// THE RUN — every commit has landed by now, so there is nothing left to refuse
// — and rides the terminal status instead.
func (r *run) postPrompt(ctx context.Context) error {
	actions, err := r.actions(r.job.Actions.After, prompts.PolicyMergeAfter)
	if err != nil {
		return err
	}
	for _, name := range actions {
		round := r.openTab(ctx, TabPostPrompt)
		r.address(TabPostPrompt, round)
		r.upsert(TabPostPrompt, round, promptTab(TabPostPrompt, &frontendv1.FeedMergeTabLive{}, 0, ""))
		close, err := r.runConfiguredPrompt(ctx, name, conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_AFTER_ACTION)
		if err != nil || close.Failed() {
			summary := fmt.Sprintf("the after-merge prompt %q did not complete", name)
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

// runConfiguredPrompt starts a session if the workspace has none — a configured
// prompt is what revives it, which is why revival is implicit rather than a
// verb — then submits the brief and waits for its turn to end.
func (r *run) runConfiguredPrompt(ctx context.Context, prompt string, origin conversationv1.PromptOrigin) (wsm.TurnClose, error) {
	if _, found, err := r.o.deps.DB.Session(ctx, r.ws); err != nil {
		return wsm.CloseFailed, err
	} else if !found {
		if err := r.o.deps.StartSession(ctx, r.ws); err != nil {
			return wsm.CloseFailed, fmt.Errorf("merge: starting a session for the configured prompt %q: %w", prompt, err)
		}
	}
	// THE ACTION IS THE PROMPT, NOT A PROMPT'S NAME.
	// CreateWorkspaceMergeActions carries `conversation.v1.UserSaid` for both
	// arms -- "the pre-merge prompt", the words themselves -- so the recorded
	// text is submitted verbatim. Reading it as a prompts-directory file name
	// failed every configured action whose text was not also a file there,
	// which is every one of them.
	return r.submit(ctx, prompt, origin)
}

// submit sends one prompt down the queue's ONE delivery path and waits for its
// turn to end. The merge's prompts are ordinary submissions with a merge
// origin; nothing about them bypasses the queue.
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

// readEscalation reports whether the fixes agent wrote the escalation record,
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
