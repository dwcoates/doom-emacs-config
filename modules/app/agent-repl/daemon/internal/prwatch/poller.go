package prwatch

import (
	"context"
	"errors"
	"fmt"
	"time"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	conversationv1 "agentrepl/proto/conversation/v1"

	"agent-repld/internal/briefprompt"
	"agent-repld/internal/clock"
	"agent-repld/internal/dlog"
	"agent-repld/internal/ghclient"
	"agent-repld/internal/ids"
	"agent-repld/internal/intakegate"
	"agent-repld/internal/merge"
	"agent-repld/internal/prompthandler"
	"agent-repld/internal/untoldnotice"
	"agent-repld/internal/wsm"
)

// The operations this package records under.
const (
	opPoll    = "daemon.prwatch.poll"
	opNotice  = "daemon.prwatch.notice"
	opGate    = "daemon.prwatch.gate"
	opComment = "daemon.prwatch.comment"
	opFollow  = "daemon.prwatch.follow"
	opLinked  = "daemon.prwatch.linked"
)

// DefaultEvery is the cadence between polls (owner ruling, 2026-10-30).
const DefaultEvery = 30 * time.Second

// BriefNotice is the brief a notice is composed from.
const BriefNotice = "pull-request-notice"

// Origin is every notice's prompt origin.
const Origin = conversationv1.PromptOrigin_PROMPT_ORIGIN_PULL_REQUEST_NOTICE

// Store is the part of the state client the watcher uses (wsm, pullrequests.go).
type Store interface {
	LivePullRequestSubscriptions(ctx context.Context) ([]wsm.PullRequestSubscription, error)
	WorkspacePullRequestSubscriptions(ctx context.Context, subscriber ids.WorkspaceID) ([]wsm.PullRequestSubscription, error)
	SubscribePullRequest(ctx context.Context, sub wsm.PullRequestSubscription) (bool, error)
	AdvancePullRequestTold(ctx context.Context, a wsm.PullRequestAdvance) (bool, error)
	RecordPullRequestReadFailure(ctx context.Context, pr wsm.PullRequestKey, f wsm.PullRequestFailure) ([]ids.WorkspaceID, error)
	OwePullRequestNotice(ctx context.Context, subscriber ids.WorkspaceID, pr wsm.PullRequestKey, sentences []string) error
	OwedPullRequestNotices(ctx context.Context) ([]wsm.PullRequestOwed, error)
	RecordPullRequestNoticeSubmitted(ctx context.Context, subscriber ids.WorkspaceID, pr wsm.PullRequestKey, turn ids.TurnID, sentences []string, took int) error
	LiveGithubMergeQueueSubscriptions(ctx context.Context) ([]wsm.GithubMergeQueueSubscription, error)
	AdvanceGithubMergeQueueTold(ctx context.Context, a wsm.GithubMergeQueueAdvance) (bool, error)
	RecordGithubMergeQueueReadFailure(ctx context.Context, queue wsm.GithubMergeQueueKey, f wsm.PullRequestFailure) ([]ids.WorkspaceID, error)
	OweGithubMergeQueueNotice(ctx context.Context, subscriber ids.WorkspaceID, queue wsm.GithubMergeQueueKey, sentences []string) error
	OwedGithubMergeQueueNotices(ctx context.Context) ([]wsm.GithubMergeQueueOwed, error)
	RecordGithubMergeQueueNoticeSubmitted(ctx context.Context, subscriber ids.WorkspaceID, queue wsm.GithubMergeQueueKey, turn ids.TurnID, sentences []string, took int) error
	LinkedPullRequests(ctx context.Context) ([]wsm.PullRequestKey, error)
	RecordPullRequestState(ctx context.Context, pr wsm.PullRequestKey, state wsm.PullRequestState, at time.Time) ([]ids.WorkspaceID, error)
	LiveGithubWorkflowRunSubscriptions(ctx context.Context) ([]wsm.GithubWorkflowRunSubscription, error)
	RecordGithubWorkflowRunRead(ctx context.Context, run wsm.GithubWorkflowRunKey, readAt time.Time) error
	OweGithubWorkflowRunCompletion(ctx context.Context, c wsm.GithubWorkflowRunCompletion) (bool, error)
	RecordGithubWorkflowRunReadFailure(ctx context.Context, run wsm.GithubWorkflowRunKey, f wsm.PullRequestFailure) ([]ids.WorkspaceID, error)
	OweGithubWorkflowRunNotice(ctx context.Context, subscriber ids.WorkspaceID, run wsm.GithubWorkflowRunKey, sentences []string) error
	OwedGithubWorkflowRunNotices(ctx context.Context) ([]wsm.GithubWorkflowRunOwed, error)
	RecordGithubWorkflowRunNoticeSubmitted(ctx context.Context, subscriber ids.WorkspaceID, run wsm.GithubWorkflowRunKey, turn ids.TurnID, sentences []string, took int) error
}

// Deps are the watcher's collaborators: everything but the prompt handler,
// which is handed to Run.
type Deps struct {
	Store Store
	GH    ghclient.Client
	// Holds retires a notice a newer one supersedes.
	Holds untoldnotice.Holds
	// Briefs loads and splices a brief at use time.
	Briefs merge.BriefLoader
	// Notices measures every notice this service submits
	// (briefprompt.Composer.Meter). REQUIRED.
	Notices briefprompt.Meter
	// MintKey mints an idempotency key for one submission.
	MintKey func(prefix string) (string, error)
	// Serves reports whether this daemon serves (rollout.Controller's
	// ServesIntake): only the serving daemon polls and tells.
	Serves func() bool
	Clock  clock.Clock
	Every  time.Duration
	// Deployments is told, after every poll, each owned pull request standing
	// in a GitHub merge queue (deployments.go).
	Deployments DeploymentSink
	// Ended is told each workspace one of whose followed pull requests
	// merged or closed, and whether it merged (workspace.Verbs.PullRequestEnded).
	Ended func(ctx context.Context, ws ids.WorkspaceID, merged bool) error
	// Comments is told each comment a subscriber is owed that someone other
	// than the user wrote (comments.go), which raises its desktop banner.
	Comments CommentSink
	// LinksRedrawn is told each workspace a link of which names a pull
	// request whose recorded state changed, so its topbar menu draws the new
	// badge (workspace.Verbs.RepublishLinks).
	LinksRedrawn func(ctx context.Context, ws ids.WorkspaceID) error
	// Log is the global logger: a pull request is no workspace's.
	Log dlog.Logger
}

// Watcher is the pull-request watcher.
type Watcher struct {
	deps   Deps
	reader *reader
	gate   *intakegate.Gate
	// places is each owned pull request's GitHub merge queue place as its
	// latest successful read found it (deployments.go). Only the one Tick
	// running at a time touches it.
	places map[wsm.PullRequestKey]*placed
	// linkedResetsAt is when a rate limit a linked pull request's state read
	// ran into resets; no state read is made before it (pollLinked). Only
	// the one Tick running at a time touches it.
	linkedResetsAt time.Time
	// wake asks the Run loop to deliver what is owed now (Wake); one pending
	// wake stands for any number asked.
	wake chan struct{}
}

// New builds the watcher, refusing a missing collaborator.
func New(deps Deps) (*Watcher, error) {
	switch {
	case deps.Store == nil, deps.GH == nil, deps.Holds == nil, deps.Briefs == nil, deps.MintKey == nil, deps.Serves == nil, deps.Clock == nil, deps.Deployments == nil, deps.Ended == nil, deps.Comments == nil, deps.LinksRedrawn == nil, deps.Log == nil:
		return nil, errors.New("prwatch: every collaborator is required")
	case deps.Every <= 0:
		return nil, fmt.Errorf("prwatch: the cadence must be positive, not %v", deps.Every)
	}
	return &Watcher{deps: deps, reader: newReader(deps.GH), places: map[wsm.PullRequestKey]*placed{}, wake: make(chan struct{}, 1),
		gate: intakegate.NewFor(deps.Serves, deps.Log, opGate, "the pull-request polls")}, nil
}

// Run polls and tells on the cadence until ctx ends: once at once, then
// every Every, one tick after the other. A Wake between ticks delivers what
// is owed at once, on this same loop, so a delivery never runs beside a
// tick's and no notice is submitted twice.
func (w *Watcher) Run(ctx context.Context, prompts prompthandler.Submitter) {
	w.Tick(ctx, prompts)
	next := w.deps.Clock.After(w.deps.Every)
	for {
		select {
		case <-ctx.Done():
			w.deps.Log.Debug(opPoll, "the pull-request watcher stopped with its context", nil)
			return
		case <-next:
			w.Tick(ctx, prompts)
			next = w.deps.Clock.After(w.deps.Every)
		case <-w.wake:
			w.DeliverNow(ctx, prompts)
		}
	}
}

// DeliverNow submits every owed notice, when this daemon serves, without
// polling: what a Wake runs.
func (w *Watcher) DeliverNow(ctx context.Context, prompts prompthandler.Submitter) {
	if !w.gate.Admits() {
		return
	}
	w.deliverOwed(ctx, prompts)
}

// Tick polls every followed pull request, the state of every other linked
// one, and every followed merge queue and workflow run once, then submits
// every notice owed, when this daemon serves. A queue both kinds need is read
// once (queueReads).
func (w *Watcher) Tick(ctx context.Context, prompts prompthandler.Submitter) {
	if !w.gate.Admits() {
		return
	}
	queues := newQueueReads(w.deps.GH)
	polled := w.poll(ctx, queues)
	w.pollLinked(ctx, polled)
	w.pollQueues(ctx, queues)
	w.pollRuns(ctx)
	w.deliverOwed(ctx, prompts)
}

// poll reads each followed pull request once and advances every subscription
// to it, answering every followed pull request, read or waiting out a rate
// limit: their states are this poll's to record, never pollLinked's.
func (w *Watcher) poll(ctx context.Context, queues *queueReads) map[wsm.PullRequestKey]bool {
	followed := map[wsm.PullRequestKey]bool{}
	subs, err := w.deps.Store.LivePullRequestSubscriptions(ctx)
	if err != nil && ctx.Err() != nil {
		// The daemon is stopping: no fact about the store.
		w.deps.Log.Debug(opPoll, "the poll stopped with its context", nil)
		return followed
	}
	if err != nil {
		w.deps.Log.Error(opPoll, "could not read the pull-request subscriptions; nothing is polled this tick", dlog.WithCause(nil, err))
		return followed
	}
	byPR := map[wsm.PullRequestKey][]wsm.PullRequestSubscription{}
	var order []wsm.PullRequestKey
	for _, sub := range subs {
		if _, seen := byPR[sub.PullRequest]; !seen {
			order = append(order, sub.PullRequest)
		}
		byPR[sub.PullRequest] = append(byPR[sub.PullRequest], sub)
		followed[sub.PullRequest] = true
	}
	var read, queued []wsm.PullRequestKey
	for _, pr := range order {
		if resets, waiting := w.rateLimitWait(byPR[pr]); waiting {
			w.deps.Log.Debug(opPoll, "the rate limit is spent; the pull request is read once it resets", dlog.Context{"pull_request": pr.String(), "resets_at": resets})
			continue
		}
		read = append(read, pr)
		if wantsQueue(byPR[pr]) {
			queued = append(queued, pr)
		}
	}
	facts := w.readQueueFacts(ctx, queued)
	for _, pr := range read {
		if ctx.Err() != nil {
			return followed
		}
		snap, ok := w.pollOne(ctx, pr, byPR[pr], facts.ask(pr), queues)
		if _, owned := ownerOf(byPR[pr]); owned && ok {
			w.places[pr] = w.placeOf(pr, snap)
		}
	}
	w.tellDeployments(ctx, order, byPR)
	return followed
}

// pollLinked reads the state of every linked pull request no subscription
// follows (wsm.LinkedPullRequests): a reference link, or an owned one whose
// subscription ended or was ended, so its menu row's badge says how it
// stands. FOLLOWED holds the pull requests poll already read. A read that
// fails leaves the recorded state as it stands; the next tick reads again,
// and a spent rate limit is waited out.
func (w *Watcher) pollLinked(ctx context.Context, followed map[wsm.PullRequestKey]bool) {
	prs, err := w.deps.Store.LinkedPullRequests(ctx)
	if err != nil && ctx.Err() != nil {
		w.deps.Log.Debug(opLinked, "the linked pull requests' reads stopped with their context", nil)
		return
	}
	if err != nil {
		w.deps.Log.Error(opLinked, "could not read the linked pull requests; no state is read this tick", dlog.WithCause(nil, err))
		return
	}
	for _, pr := range prs {
		if followed[pr] {
			continue
		}
		if ctx.Err() != nil {
			return
		}
		fields := dlog.Context{"pull_request": pr.String()}
		if now := w.deps.Clock.Now(); now.Before(w.linkedResetsAt) {
			w.deps.Log.Debug(opLinked, "the rate limit is spent; the linked pull requests' states are read once it resets", dlog.Merge(fields, dlog.Context{"resets_at": w.linkedResetsAt}))
			return
		}
		state, err := w.reader.readState(ctx, pr)
		if err != nil {
			if ctx.Err() != nil {
				return
			}
			f := failureOf(err, w.deps.Clock.Now())
			if f.Kind == wsm.PullRequestFailureRateLimited {
				w.linkedResetsAt = f.ResetsAt
			}
			// The leaf recorded a GitHub failure itself; this records what it cost.
			w.deps.Log.Info(opLinked, "could not read the linked pull request's state; its badge stands until a later read",
				dlog.Merge(fields, dlog.Context{"kind": string(f.Kind), "cause": err.Error()}))
			continue
		}
		w.recordState(ctx, pr, state)
	}
}

// recordState records PR's STATE as a read just found it, and redraws the
// links of every workspace whose menu the change reaches.
func (w *Watcher) recordState(ctx context.Context, pr wsm.PullRequestKey, state wsm.PullRequestState) {
	fields := dlog.Context{"pull_request": pr.String(), "state": string(state)}
	changed, err := w.deps.Store.RecordPullRequestState(ctx, pr, state, w.deps.Clock.Now())
	if err != nil {
		if ctx.Err() != nil {
			return
		}
		w.deps.Log.Error(opLinked, "could not record the pull request's state; its links' badges stand until a later read", dlog.WithCause(fields, err))
		return
	}
	if len(changed) == 0 {
		return
	}
	fields["workspaces"] = len(changed)
	w.deps.Log.Info(opLinked, "the pull request's state changed; every workspace linking it draws its links again", fields)
	for _, ws := range changed {
		if err := w.deps.LinksRedrawn(ctx, ws); err != nil {
			w.deps.Log.Error(opLinked, "could not draw a workspace's links again after its pull request's state changed",
				dlog.WithCause(dlog.Merge(fields, dlog.Context{"workspace": string(ws)}), err))
		}
	}
}

// rateLimitWait answers when a spent rate limit resets, and true while any
// of SUBS waits on one. A SPENT RATE LIMIT IS WAITED OUT: no read is made
// before GitHub resets it, since every one would be refused.
func (w *Watcher) rateLimitWait(subs []wsm.PullRequestSubscription) (time.Time, bool) {
	failing := make([]*wsm.PullRequestFailure, len(subs))
	for i, sub := range subs {
		failing[i] = sub.Failing
	}
	return w.rateLimitWaitOf(failing)
}

// rateLimitWaitOf is rateLimitWait over the failing stretches of any GitHub
// subscriptions.
func (w *Watcher) rateLimitWaitOf(failing []*wsm.PullRequestFailure) (time.Time, bool) {
	for _, f := range failing {
		if f != nil && f.Kind == wsm.PullRequestFailureRateLimited && w.deps.Clock.Now().Before(f.ResetsAt) {
			return f.ResetsAt, true
		}
	}
	return time.Time{}, false
}

// wantsQueue reports whether any of SUBS asks for the merge queue.
func wantsQueue(subs []wsm.PullRequestSubscription) bool {
	for _, sub := range subs {
		if sub.Events.Has(wsm.PullRequestEventMergeQueue) {
			return true
		}
	}
	return false
}

// pollOne reads PR, its merge queue facts when QUEUE is set, and advances
// SUBS, answering the read and whether it succeeded.
func (w *Watcher) pollOne(ctx context.Context, pr wsm.PullRequestKey, subs []wsm.PullRequestSubscription, queue *queueAsk, queues *queueReads) (snapshot, bool) {
	fields := dlog.Context{"pull_request": pr.String(), "subscribers": len(subs)}
	checks := false
	for _, sub := range subs {
		checks = checks || sub.Events.Has(wsm.PullRequestEventChecks)
	}
	snap, err := w.readSnapshot(ctx, pr, checks, queue, queues)
	if err != nil {
		if ctx.Err() != nil {
			return snap, false
		}
		w.recordFailure(ctx, pr, err, fields)
		return snap, false
	}
	now := w.deps.Clock.Now()
	for _, sub := range subs {
		w.advance(ctx, sub, snap, now)
	}
	w.recordState(ctx, pr, stateOf(snap.PR))
	return snap, true
}

// queueAsk is what a tick read of one pull request's merge queue facts
// found: the facts, or why they could not be read.
type queueAsk struct {
	facts remotePRQueue
	err   error
}

// queueFacts is one tick's reads of the merge queue facts of the pull
// requests that ask for them.
type queueFacts map[wsm.PullRequestKey]*queueAsk

// ask is PR's; nil when PR does not ask for the queue.
func (f queueFacts) ask(pr wsm.PullRequestKey) *queueAsk { return f[pr] }

// readQueueFacts reads the merge queue facts of PRS, one query per
// repository. A query that fails is every one of its pull requests' failure.
func (w *Watcher) readQueueFacts(ctx context.Context, prs []wsm.PullRequestKey) queueFacts {
	out := queueFacts{}
	byRepo := map[repository][]uint32{}
	var order []repository
	for _, pr := range prs {
		repo := repository{pr.Owner, pr.Repo}
		if _, seen := byRepo[repo]; !seen {
			order = append(order, repo)
		}
		byRepo[repo] = append(byRepo[repo], pr.Number)
	}
	for _, repo := range order {
		facts, failures, err := readPRQueues(ctx, w.deps.GH, repo, byRepo[repo])
		for _, n := range byRepo[repo] {
			key := wsm.PullRequestKey{Owner: repo.Owner, Repo: repo.Repo, Number: n}
			switch {
			case err != nil:
				out[key] = &queueAsk{err: err}
			case failures[n] != nil:
				out[key] = &queueAsk{err: failures[n]}
			default:
				out[key] = &queueAsk{facts: facts[n]}
			}
		}
	}
	return out
}

// readSnapshot reads PR's lists, its head's checks when CHECKS, and, when
// QUEUE is set, its place in its merge queue, reading the queue through
// QUEUES and its merge group's checks.
func (w *Watcher) readSnapshot(ctx context.Context, pr wsm.PullRequestKey, checks bool, queue *queueAsk, queues *queueReads) (snapshot, error) {
	snap, err := w.reader.read(ctx, pr, checks)
	if err != nil || queue == nil {
		return snap, err
	}
	if queue.err != nil {
		return snap, queue.err
	}
	facts := queue.facts
	q := &prQueueSnapshot{Events: facts.Events, Entry: facts.Entry}
	if facts.Entry != nil {
		read, err := queues.get(ctx, wsm.GithubMergeQueueKey{Owner: pr.Owner, Repo: pr.Repo, Branch: facts.Base})
		if err != nil {
			return snap, err
		}
		if p, ok := positionOf(read, pr.Number); ok {
			q.Position = &p
		}
		if head := facts.Entry.GroupHead; head != "" {
			if q.GroupChecks, err = w.reader.readChecks(ctx, fmt.Sprintf("repos/%s/%s/commits/%s/check-runs", pr.Owner, pr.Repo, head)); err != nil {
				return snap, err
			}
			// The merge group tests the pull request's head on its base, so
			// the head's config names its gates.
			if q.Gates, err = w.reader.gateChecks(ctx, pr.Owner, pr.Repo, snap.PR.Head); err != nil {
				return snap, err
			}
		}
	}
	snap.Queue = q
	return snap, nil
}

// advance tells SUB what SNAP holds that it was not told.
func (w *Watcher) advance(ctx context.Context, sub wsm.PullRequestSubscription, snap snapshot, now time.Time) {
	fields := dlog.Context{"pull_request": sub.PullRequest.String(), "subscriber": string(sub.Subscriber)}
	prev, anchored, err := decodeTold(sub.Told)
	if err != nil {
		// The snapshot is the daemon's own encoding; one it cannot read is a
		// defect, and the subscription cannot be advanced past it.
		w.deps.Log.Error(opPoll, "the subscription's told snapshot is unreadable; it is not advanced", dlog.WithCause(fields, err))
		return
	}
	next, activity := diff(snap, prev, anchored)
	// A SUBSCRIPTION TAKEN WITHOUT A READ IS ANCHORED BY ITS FIRST ONE
	// (FollowFromFirstRead): what is already on the pull request counts as
	// told, exactly as Anchor's read counts it. Only its end is still said.
	if prev.AnchorOnRead {
		activity = nil
	}
	var told []*agentreplv1.PullRequestActivity
	for _, a := range activity {
		if sub.Events.Has(eventOf(a)) {
			told = append(told, a)
		}
	}
	owed := sentencesOf(told)
	end := endOf(snap.PR)
	if end != nil {
		owed = append(owed, endedSentence)
	}
	// A RECOVERY IS NO ACTIVITY: reads that work again are never said, not
	// even as a preface to what the pull request did meanwhile. The advance
	// still runs, so the stored health clears.
	recovered := sub.Failing != nil
	to := encodeTold(next)
	if to == sub.Told && len(owed) == 0 && !recovered {
		return
	}
	applied, err := w.deps.Store.AdvancePullRequestTold(ctx, wsm.PullRequestAdvance{
		Subscriber: sub.Subscriber, PullRequest: sub.PullRequest, From: sub.Told, To: to,
		Owed: owed, Status: statusWords(statusProto(snap)), End: end, ReadAt: now})
	fields["owed"], fields["ends"] = len(owed), end != nil
	if err == nil && applied {
		w.bannerComments(ctx, sub.Subscriber, told, fields)
	}
	switch {
	case err != nil:
		w.deps.Log.Error(opPoll, "could not advance the subscription; the next poll reads again", dlog.WithCause(fields, err))
	case !applied:
		w.deps.Log.Info(opPoll, "the subscription moved since it was read; the next poll reads again", fields)
	case end != nil:
		w.deps.Log.Info(opPoll, "the pull request ended; its subscription ended after telling it", fields)
		// AN END CHANGES WHAT THE WORKSPACE'S ROW DRAWS, and a merge may be the
		// last of its owned pull requests, which tells the workspace so;
		// closing it is its own (owner ruling, 2026-11-16). Whatever the
		// subscription's origin: an owned
		// pull request its workspace follows again with narrowed events is
		// followed as `requested`, and the verb judges ownership from the
		// workspace's links.
		if err := w.deps.Ended(ctx, sub.Subscriber, end.Merged); err != nil {
			w.deps.Log.Error(opPoll, "could not act on the end of the workspace's pull request", dlog.WithCause(fields, err))
		}
	case len(owed) > 0:
		w.deps.Log.Info(opPoll, "the pull request's activity is owed to its subscriber", fields)
	case recovered:
		w.deps.Log.Info(opPoll, "the pull request's reads work again and it did nothing meanwhile; its subscriber is told nothing", fields)
	}
}

// recordFailure records a failed read for every subscription to PR.
func (w *Watcher) recordFailure(ctx context.Context, pr wsm.PullRequestKey, err error, fields dlog.Context) {
	w.recordReadFailure(ctx, opPoll, "pull request", err, fields,
		func(f wsm.PullRequestFailure) ([]ids.WorkspaceID, error) {
			return w.deps.Store.RecordPullRequestReadFailure(ctx, pr, f)
		},
		func(subscriber ids.WorkspaceID, sentences []string) error {
			return w.deps.Store.OwePullRequestNotice(ctx, subscriber, pr, sentences)
		})
}

// recordReadFailure records a failed read of a SUBJECT (`pull request`,
// `merge queue`) through RECORD for every subscription to it.
//
// A READ THE DAEMON COULD NOT MAKE IS TOLD TO NO WORKSPACE (owner ruling,
// 2026-11-10). It is no change of the pull request or the queue, only of the
// daemon's own reach, so it is the daemon's record and the subscription's
// health, never a notice: telling it woke every subscriber on each outage
// with nothing to act on, and the noise had subscribers end their
// subscriptions (2026-11-05), which then missed a review that mattered.
//
// THE ONE EXCEPTION IS A REJECTED CREDENTIAL (owner ruling, 2026-11-13): it
// never heals by waiting, and only the user can renew it, so each
// subscription whose failure BEGINS with it is owed one brief sentence
// through OWE, for its workspace to surface to the user. A failure that
// stands is not told again.
func (w *Watcher) recordReadFailure(ctx context.Context, op, subject string, err error, fields dlog.Context,
	record func(wsm.PullRequestFailure) ([]ids.WorkspaceID, error),
	owe func(subscriber ids.WorkspaceID, sentences []string) error) {
	f := failureOf(err, w.deps.Clock.Now())
	began, rerr := record(f)
	if rerr != nil {
		w.deps.Log.Error(op, "could not record the "+subject+"'s failed read", dlog.WithCause(dlog.Merge(fields, dlog.Context{"read_error": err.Error()}), rerr))
		return
	}
	if f.Kind == wsm.PullRequestFailureUnauthorized {
		for _, subscriber := range began {
			if oerr := owe(subscriber, []string{credentialRejectedSentence}); oerr != nil {
				w.deps.Log.Error(op, "could not owe a subscriber the notice that GitHub rejected the credentials", dlog.WithCause(dlog.Merge(fields, dlog.Context{"subscriber": string(subscriber)}), oerr))
			}
		}
		w.deps.Log.Info(op, "GitHub rejected the credentials; each subscription whose failure began is owed a notice to surface it", dlog.Merge(fields, dlog.Context{"kind": string(f.Kind), "began": len(began), "cause": err.Error()}))
		return
	}
	// The leaf recorded a GitHub failure itself; this records what it cost.
	w.deps.Log.Info(op, "the "+subject+" could not be read; its subscriptions are failing, and no subscriber is told", dlog.Merge(fields, dlog.Context{"kind": string(f.Kind), "began": len(began), "cause": err.Error()}))
}

// credentialRejectedSentence is the one sentence a subscriber is owed when
// GitHub rejects agent-repl's credentials.
const credentialRejectedSentence = "GitHub rejected agent-repl's credentials (401 Bad credentials), so this is no longer followed until the user renews the GitHub token; tell the user in one sentence."

// failureOf is a read's error as the store records it.
func failureOf(err error, now time.Time) wsm.PullRequestFailure {
	f := wsm.PullRequestFailure{Since: now, Kind: wsm.PullRequestFailureUnreachable, Detail: err.Error()}
	if errors.Is(err, ErrNoMergeQueue) {
		f.Kind, f.Detail = wsm.PullRequestFailureNotFound, ""
		return f
	}
	var gh *ghclient.Failure
	if errors.As(err, &gh) {
		f.Detail = gh.Message
		switch gh.Kind {
		case ghclient.KindUnauthorized:
			f.Kind, f.Detail = wsm.PullRequestFailureUnauthorized, ""
		case ghclient.KindNotFound:
			f.Kind, f.Detail = wsm.PullRequestFailureNotFound, ""
		case ghclient.KindRateLimited:
			f.Kind, f.Detail, f.ResetsAt = wsm.PullRequestFailureRateLimited, "", gh.ResetsAt
		case ghclient.KindRefused:
			f.Detail = fmt.Sprintf("GitHub refused the read (HTTP %d: %s)", gh.Status, gh.Message)
		}
	}
	return f
}

// deliverOwed submits every owed notice, each saying first what the
// subscriber's superseded notice said.
func (w *Watcher) deliverOwed(ctx context.Context, prompts prompthandler.Submitter) {
	if !deliverKind(ctx, w, prompts, "pull-request", w.deps.Store.OwedPullRequestNotices, pullRequestNotice) {
		return
	}
	if !deliverKind(ctx, w, prompts, "merge-queue", w.deps.Store.OwedGithubMergeQueueNotices, githubMergeQueueNotice) {
		return
	}
	deliverKind(ctx, w, prompts, "workflow-run", w.deps.Store.OwedGithubWorkflowRunNotices, githubWorkflowRunNotice)
}

// deliverKind submits every notice of one GitHub subscription kind LOAD
// answers, each as NOTICE spells it, answering false when they could not be
// read: the delivery then stops for the tick, and a later one tells them.
func deliverKind[T any](ctx context.Context, w *Watcher, prompts prompthandler.Submitter, noun string,
	load func(context.Context) ([]T, error), notice func(Store, T) owedNotice) bool {
	owed, err := load(ctx)
	if err != nil && ctx.Err() != nil {
		w.deps.Log.Debug(opNotice, "the delivery stopped with its context", nil)
		return false
	}
	if err != nil {
		w.deps.Log.Error(opNotice, "could not read the owed "+noun+" notices; none is told this tick", dlog.WithCause(nil, err))
		return false
	}
	for _, o := range owed {
		w.deliver(ctx, prompts, notice(w.deps.Store, o))
	}
	return true
}

// owedNotice is one notice owed to one subscriber, whatever GitHub subject
// it is about: what it was last submitted as, the sentences owed since, and
// how to compose it and record its submission.
type owedNotice struct {
	subscriber ids.WorkspaceID
	// turn and sentences are the last submitted notice; turn is empty when
	// its submission failed.
	turn      ids.TurnID
	sentences []string
	// pending is every sentence owed and not yet submitted, oldest first.
	pending []string
	// status is the subject as it last stood, in words.
	status string
	// label names the subject on the notice's first line.
	label string
	// noun names the notice's kind in records (`pull-request`), and subject
	// what it is about (`pull request`).
	noun, subject string
	// brief and origin are the notice's brief and prompt origin.
	brief  string
	origin conversationv1.PromptOrigin
	// fields identify the notice in every record.
	fields dlog.Context
	// record records that TURN said SENTENCES, taking the first TOOK pending
	// ones.
	record func(ctx context.Context, turn ids.TurnID, sentences []string, took int) error
}

// pullRequestNotice is O as the notice delivery submits it.
func pullRequestNotice(store Store, o wsm.PullRequestOwed) owedNotice {
	return owedNotice{subscriber: o.Subscriber, turn: o.Turn, sentences: o.Sentences, pending: o.Pending, status: o.Status,
		label: "pull request `" + o.PullRequest.String() + "`", noun: "pull-request", subject: "pull request", brief: BriefNotice, origin: Origin,
		fields: dlog.Context{"subscriber": string(o.Subscriber), "pull_request": o.PullRequest.String()},
		record: func(ctx context.Context, turn ids.TurnID, sentences []string, took int) error {
			return store.RecordPullRequestNoticeSubmitted(ctx, o.Subscriber, o.PullRequest, turn, sentences, took)
		}}
}

// deliver submits N to its subscriber and records what was submitted.
func (w *Watcher) deliver(ctx context.Context, prompts prompthandler.Submitter, n owedNotice) {
	fields := dlog.Merge(n.fields, dlog.Context{"pending": len(n.pending)})
	prev := untoldnotice.Notice{Turn: n.turn, Sentences: n.sentences}
	carried := untoldnotice.Carried(ctx, w.deps.Holds, w.deps.Log, opNotice, n.subscriber, prev, n.turn != "" || len(n.sentences) > 0, fields)
	sentences := append(append([]string{}, carried...), n.pending...)
	fields["sentences"] = len(sentences)
	turn, failure, err := w.submit(ctx, prompts, n, sentences)
	if err != nil {
		w.deps.Log.Error(opNotice, failure, dlog.WithCause(fields, err))
		// What the superseded notice said is kept to be said again; the
		// pending sentences stay pending for the next tick.
		if len(carried) > 0 {
			if rerr := n.record(ctx, "", carried, 0); rerr != nil {
				w.deps.Log.Error(opNotice, "could not keep the superseded notice's sentences; they are not said again", dlog.WithCause(fields, rerr))
			}
		}
		return
	}
	if err := n.record(ctx, turn, sentences, len(n.pending)); err != nil {
		w.deps.Log.Error(opNotice, "could not record the submitted notice; its sentences may be told again", dlog.WithCause(fields, err))
		return
	}
	w.deps.Log.Info(opNotice, "told a subscriber about its "+n.subject, dlog.Merge(fields, dlog.Context{"turn": string(turn)}))
}

// submit composes and submits one notice, answering its turn, or the record a
// failure is written with and why.
func (w *Watcher) submit(ctx context.Context, prompts prompthandler.Submitter, n owedNotice, sentences []string) (ids.TurnID, string, error) {
	turn, err := briefprompt.Composer{Briefs: w.deps.Briefs, MintKey: w.deps.MintKey, Meter: w.deps.Notices}.Submit(ctx, prompts, n.subscriber, n.brief, map[string]string{
		"label":    n.label,
		"activity": untoldnotice.Sentences(sentences),
		"status":   n.status,
	}, n.brief, n.origin)
	switch briefprompt.StepOf(err) {
	case briefprompt.StepCompose:
		return "", "could not compose a " + n.noun + " notice; the subscriber is told on a later tick", err
	case briefprompt.StepMint:
		return "", "could not mint a " + n.noun + " notice's idempotency key; the subscriber is told on a later tick", err
	case briefprompt.StepSubmit:
		return "", "could not submit a " + n.noun + " notice to its subscriber; it is told on a later tick", err
	}
	return turn, "", nil
}
