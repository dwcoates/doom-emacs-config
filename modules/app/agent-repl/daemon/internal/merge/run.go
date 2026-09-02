package merge

import (
	"context"
	"fmt"
	"os"
	"path/filepath"
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

// run is one merge in flight.
type run struct {
	o   *orchestrator
	ws  ids.WorkspaceID
	job wsm.CreationJob
	// repo is the queue this merge was admitted from.
	repo wsm.RepoKey
	// lease is the occupancy lease held for the merge's whole duration.
	lease wsm.Lease
	// lock is the repository's queue lock, held for the same duration.
	lock *repoLock
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
	// startedMS is the head's running clock.
	startedMS int64
	// displaced is the user turn this merge displaced, captured at admission.
	displaced *ids.TurnID

	mu sync.Mutex
	// rounds counts each tab kind's opened rounds, which is what makes a second
	// pass a second tab.
	rounds map[string]int
	// opened remembers when each round opened, so a closed ledger interval is a
	// real interval rather than an instant.
	opened map[string]time.Time
	// tab is the tab kind currently live.
	tab string
	// guidance carries a parked submission from RouteParked into the waiting
	// phase. It is unbuffered: guidance is delivered to a phase that is waiting
	// for it, never queued behind one that is not.
	guidance chan *conversationv1.UserSaid
	// answered carries the delivery's answer back to RouteParked.
	answered chan error
	// conflictBriefed records the conflict the agent has already been handed,
	// so a conflict reaches the agent EXACTLY ONCE.
	conflictBriefed map[string]bool
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

// start admits one workspace's merge: it takes the lease and the occupancy,
// opens the ledger, addresses the session's output at the bubble, captures the
// displaced turn, and runs the method the target selects.
func (o *orchestrator) start(ctx context.Context, repo wsm.RepoKey, ws ids.WorkspaceID, lock *repoLock) error {
	const op = "daemon.merge.start"
	log := o.log(ctx, ws)
	job, err := o.layoutFor(ctx, ws)
	if err != nil {
		lock.Release()
		return err
	}
	// THE OCCUPANCY IS TAKEN UNDER THE LEDGER IDENTITY minted at enqueue, so
	// the queued bubble and the running one are one bubble.
	ledger := o.mintLedger(ws)
	lease, err := o.deps.DB.AcquireLeaseAs(ctx, ws, ledger, wsm.HolderMerge, wsm.PolicyRefuse)
	if err != nil {
		lock.Release()
		log.Error(op, "could not take the merge lease", dlog.Context{"workspace": string(ws), "error": err.Error()})
		return err
	}
	r := &run{
		o: o, ws: ws, job: job, repo: repo, lease: lease, lock: lock,
		startedMS:       o.deps.Now().UnixMilli(),
		rounds:          map[string]int{},
		opened:          map[string]time.Time{},
		guidance:        make(chan *conversationv1.UserSaid),
		answered:        make(chan error),
		conflictBriefed: map[string]bool{},
	}
	same, err := o.deps.Git.SameRepo(ctx, job.Layout.TargetDir, o.deps.SelfRepoDir)
	if err != nil {
		r.abort(ctx, fmt.Sprintf("could not identify the target repository: %v", err))
		return err
	}
	r.emacsRepo = same
	r.selfCheckout = same && sameDir(job.Layout.TargetDir, o.deps.SelfRepoDir)

	if release, ok, err := o.deps.Occupy(ws, holderMerge); err != nil {
		r.abort(ctx, fmt.Sprintf("could not take the session's occupancy: %v", err))
		return err
	} else if ok {
		r.releaseOccupancy = release
	}
	if err := o.deps.DB.OpenMergeLedger(ctx, ws, lease.ID); err != nil {
		r.abort(ctx, fmt.Sprintf("could not open the merge ledger: %v", err))
		return err
	}
	if turn, captured, err := o.deps.CaptureDisplaced(ctx, ws); err != nil {
		r.abort(ctx, fmt.Sprintf("could not capture the displaced turn: %v", err))
		return err
	} else if captured {
		r.displaced = &turn
	}
	o.mu.Lock()
	o.running[repo] = r
	o.runsByWorkspace[ws] = r
	o.mu.Unlock()

	r.mu.Lock()
	r.tab = TabQueue
	r.rounds[TabQueue] = 1
	r.mu.Unlock()
	r.head(nil)
	if err := o.republishQueue(ctx, repo); err != nil {
		r.abort(ctx, fmt.Sprintf("could not publish the queue: %v", err))
		return err
	}
	log.Debug(op, "admitted a merge", dlog.Context{
		"workspace": string(ws), "repo": string(repo), "lease": string(lease.ID),
		"method": methodName(r.emacsRepo), "self_checkout": r.selfCheckout,
	})
	return r.execute(ctx)
}

// roundKey addresses one tab round's recorded start.
func roundKey(kind string, round int) string { return fmt.Sprintf("%s/%d", kind, round) }

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
	outcome, err := r.method(ctx)
	if err != nil {
		r.abort(ctx, err.Error())
		return err
	}
	if outcome.parked {
		// A parked merge keeps the lease and the lock: it is stopped, not
		// finished, and the conversational parked flow is its only resume.
		return nil
	}
	return r.finish(ctx, outcome)
}

// outcome is how a method ended.
type outcome struct {
	// landed is the merge commit, empty when nothing landed locally.
	landed string
	// commits are what the merge brought in, for the self-reload's classifier.
	commits []gitclient.Commit
	// failed is the failure's one-line account, empty on success.
	failed string
	// parked reports that the run stopped for the user and holds its lease.
	parked bool
}

// method dispatches to the run's method.
func (r *run) method(ctx context.Context) (outcome, error) {
	if err := r.prePrompt(ctx); err != nil {
		return outcome{}, err
	}
	if !r.emacsRepo {
		// EVERY OTHER REPO: nothing between the two prompts. Landing, tests and
		// PR work belong to the prompts there.
		if parked, err := r.postPrompt(ctx); err != nil || parked {
			return outcome{parked: parked}, err
		}
		return outcome{}, nil
	}
	out, err := r.emacsMethod(ctx)
	if err != nil || out.parked || out.failed != "" {
		return out, err
	}
	if _, err := r.postPrompt(ctx); err != nil {
		// A post-prompt failure NEVER fails the run: it rides the terminal.
		r.o.log(ctx, r.ws).Warn("daemon.merge.post_prompt", "the post-merge prompt failed; the merge still landed",
			dlog.Context{"workspace": string(r.ws), "error": err.Error()})
	}
	return out, nil
}

// emacsMethod is the no-ff merge, its conflicts, the gate and its fixes.
func (r *run) emacsMethod(ctx context.Context) (outcome, error) {
	commit, parked, err := r.mergeTab(ctx)
	if err != nil || parked {
		return outcome{parked: parked}, err
	}
	commits, err := r.o.deps.Git.LandedRange(ctx, r.job.Layout.TargetDir, commit)
	if err != nil {
		return outcome{}, fmt.Errorf("merge: reading what %s landed: %w", commit, err)
	}
	failed, parked, err := r.gate(ctx, commit)
	if err != nil || parked {
		return outcome{landed: commit, commits: commits, parked: parked}, err
	}
	if failed != "" {
		return outcome{landed: commit, commits: commits, failed: failed}, nil
	}
	return outcome{landed: commit, commits: commits}, nil
}

// prePrompt runs the configured before-merge prompts. THEIR FAILURE FAILS THE
// RUN: they are the workspace's own precondition for merging, so merging past
// one would land work its author said was not ready.
func (r *run) prePrompt(ctx context.Context) error {
	for _, name := range r.job.Actions.Before {
		round := r.openTab(ctx, TabPrePrompt)
		r.address(TabPrePrompt, round)
		r.upsert(TabPrePrompt, round, promptTab(TabPrePrompt, &frontendv1.FeedMergeTabLive{}, 0, ""))
		close, err := r.runConfiguredPrompt(ctx, name, conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_BEFORE_ACTION)
		if err != nil || close == wsm.CloseFailed {
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
func (r *run) postPrompt(ctx context.Context) (bool, error) {
	for _, name := range r.job.Actions.After {
		round := r.openTab(ctx, TabPostPrompt)
		r.address(TabPostPrompt, round)
		r.upsert(TabPostPrompt, round, promptTab(TabPostPrompt, &frontendv1.FeedMergeTabLive{}, 0, ""))
		close, err := r.runConfiguredPrompt(ctx, name, conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_AFTER_ACTION)
		if err != nil || close == wsm.CloseFailed {
			summary := fmt.Sprintf("the after-merge prompt %q did not complete", name)
			r.upsert(TabPostPrompt, round, promptTab(TabPostPrompt, nil, r.o.nowMS(), summary))
			r.closeTab(ctx, TabPostPrompt, round, "failed")
			if err != nil {
				return false, err
			}
			return false, fmt.Errorf("merge: %s", summary)
		}
		r.upsert(TabPostPrompt, round, promptTab(TabPostPrompt, nil, r.o.nowMS(), ""))
		r.closeTab(ctx, TabPostPrompt, round, "succeeded")
	}
	return false, nil
}

// runConfiguredPrompt starts a session if the workspace has none — a configured
// prompt is what revives it, which is why revival is implicit rather than a
// verb — then submits the brief and waits for its turn to end.
func (r *run) runConfiguredPrompt(ctx context.Context, name string, origin conversationv1.PromptOrigin) (wsm.TurnClose, error) {
	if _, found, err := r.o.deps.DB.Session(ctx, r.ws); err != nil {
		return wsm.CloseFailed, err
	} else if !found {
		if err := r.o.deps.StartSession(ctx, r.ws); err != nil {
			return wsm.CloseFailed, fmt.Errorf("merge: starting a session for the configured prompt %q: %w", name, err)
		}
	}
	text, err := r.o.deps.Briefs(name, map[string]string{})
	if err != nil {
		return wsm.CloseFailed, fmt.Errorf("merge: reading the configured prompt %q: %w", name, err)
	}
	return r.submit(ctx, text, origin)
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
func readEscalation(targetDir string) (string, bool) {
	body, err := os.ReadFile(filepath.Join(targetDir, EscalationFile))
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
