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
	// MintKey mints an idempotency key for one submission.
	MintKey func(prefix string) (string, error)
	// Serves reports whether this daemon serves (rollout.Controller's
	// ServesIntake): only the serving daemon polls and tells.
	Serves func() bool
	Clock  clock.Clock
	Every  time.Duration
	// Log is the global logger: a pull request is no workspace's.
	Log dlog.Logger
}

// Watcher is the pull-request watcher.
type Watcher struct {
	deps   Deps
	reader *reader
	gate   *intakegate.Gate
}

// New builds the watcher, refusing a missing collaborator.
func New(deps Deps) (*Watcher, error) {
	switch {
	case deps.Store == nil, deps.GH == nil, deps.Holds == nil, deps.Briefs == nil, deps.MintKey == nil, deps.Serves == nil, deps.Clock == nil, deps.Log == nil:
		return nil, errors.New("prwatch: every collaborator is required")
	case deps.Every <= 0:
		return nil, fmt.Errorf("prwatch: the cadence must be positive, not %v", deps.Every)
	}
	return &Watcher{deps: deps, reader: newReader(deps.GH),
		gate: intakegate.NewFor(deps.Serves, deps.Log, opGate, "the pull-request polls")}, nil
}

// Run polls and tells on the cadence until ctx ends: once at once, then
// every Every, one tick after the other.
func (w *Watcher) Run(ctx context.Context, prompts prompthandler.Submitter) {
	tick := func(ctx context.Context) { w.Tick(ctx, prompts) }
	tick(ctx)
	_ = clock.Repeat(ctx, w.deps.Clock, w.deps.Every, w.deps.Every, tick)
	w.deps.Log.Debug(opPoll, "the pull-request watcher stopped with its context", nil)
}

// Tick polls every followed pull request once, then submits every notice
// owed, when this daemon serves.
func (w *Watcher) Tick(ctx context.Context, prompts prompthandler.Submitter) {
	if !w.gate.Admits() {
		return
	}
	w.poll(ctx)
	w.deliverOwed(ctx, prompts)
}

// poll reads each followed pull request once and advances every subscription
// to it.
func (w *Watcher) poll(ctx context.Context) {
	subs, err := w.deps.Store.LivePullRequestSubscriptions(ctx)
	if err != nil && ctx.Err() != nil {
		// The daemon is stopping: no fact about the store.
		w.deps.Log.Debug(opPoll, "the poll stopped with its context", nil)
		return
	}
	if err != nil {
		w.deps.Log.Error(opPoll, "could not read the pull-request subscriptions; nothing is polled this tick", dlog.WithCause(nil, err))
		return
	}
	byPR := map[wsm.PullRequestKey][]wsm.PullRequestSubscription{}
	var order []wsm.PullRequestKey
	for _, sub := range subs {
		if _, seen := byPR[sub.PullRequest]; !seen {
			order = append(order, sub.PullRequest)
		}
		byPR[sub.PullRequest] = append(byPR[sub.PullRequest], sub)
	}
	for _, pr := range order {
		if ctx.Err() != nil {
			return
		}
		w.pollOne(ctx, pr, byPR[pr])
	}
}

// pollOne reads PR and advances SUBS.
func (w *Watcher) pollOne(ctx context.Context, pr wsm.PullRequestKey, subs []wsm.PullRequestSubscription) {
	fields := dlog.Context{"pull_request": pr.String(), "subscribers": len(subs)}
	checks := false
	for _, sub := range subs {
		checks = checks || sub.Events.Has(wsm.PullRequestEventChecks)
		// A SPENT RATE LIMIT IS WAITED OUT: no read is made before GitHub
		// resets it, since every one would be refused.
		if f := sub.Failing; f != nil && f.Kind == wsm.PullRequestFailureRateLimited && w.deps.Clock.Now().Before(f.ResetsAt) {
			w.deps.Log.Debug(opPoll, "the rate limit is spent; the pull request is read once it resets", dlog.Merge(fields, dlog.Context{"resets_at": f.ResetsAt}))
			return
		}
	}
	snap, err := w.reader.read(ctx, pr, checks)
	if err != nil {
		if ctx.Err() != nil {
			return
		}
		w.recordFailure(ctx, pr, err, fields)
		return
	}
	now := w.deps.Clock.Now()
	for _, sub := range subs {
		w.advance(ctx, sub, snap, now)
	}
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
	// A RECOVERY IS NO ACTIVITY: reads that work again are said only as the
	// preface to what the pull request did meanwhile, never as a notice of
	// their own. The advance still runs, so the stored health clears.
	recovered := sub.Failing != nil
	if recovered && len(owed) > 0 {
		owed = append([]string{recoveredSentence}, owed...)
	}
	to := encodeTold(next)
	if to == sub.Told && len(owed) == 0 && !recovered {
		return
	}
	applied, err := w.deps.Store.AdvancePullRequestTold(ctx, wsm.PullRequestAdvance{
		Subscriber: sub.Subscriber, PullRequest: sub.PullRequest, From: sub.Told, To: to,
		Owed: owed, Status: statusWords(statusProto(snap.PR)), End: end, ReadAt: now})
	fields["owed"], fields["ends"] = len(owed), end != nil
	switch {
	case err != nil:
		w.deps.Log.Error(opPoll, "could not advance the subscription; the next poll reads again", dlog.WithCause(fields, err))
	case !applied:
		w.deps.Log.Info(opPoll, "the subscription moved since it was read; the next poll reads again", fields)
	case end != nil:
		w.deps.Log.Info(opPoll, "the pull request ended; its subscription ended after telling it", fields)
	case len(owed) > 0:
		w.deps.Log.Info(opPoll, "the pull request's activity is owed to its subscriber", fields)
	case recovered:
		w.deps.Log.Info(opPoll, "the pull request's reads work again and it did nothing meanwhile; its subscriber is told nothing", fields)
	}
}

// recordFailure records a failed read for every subscription to PR, and owes
// the subscribers whose failing stretch it began one sentence saying so.
func (w *Watcher) recordFailure(ctx context.Context, pr wsm.PullRequestKey, err error, fields dlog.Context) {
	f := failureOf(err, w.deps.Clock.Now())
	began, rerr := w.deps.Store.RecordPullRequestReadFailure(ctx, pr, f)
	if rerr != nil {
		w.deps.Log.Error(opPoll, "could not record the pull request's failed read", dlog.WithCause(dlog.Merge(fields, dlog.Context{"read_error": err.Error()}), rerr))
		return
	}
	// The leaf recorded the failure itself; this records what it cost.
	w.deps.Log.Info(opPoll, "the pull request could not be read; its subscriptions are failing", dlog.Merge(fields, dlog.Context{"kind": string(f.Kind), "began": len(began)}))
	for _, subscriber := range began {
		if err := w.deps.Store.OwePullRequestNotice(ctx, subscriber, pr, []string{failureSentence(f)}); err != nil {
			w.deps.Log.Error(opPoll, "could not owe the subscriber the failure's notice", dlog.WithCause(dlog.Merge(fields, dlog.Context{"subscriber": string(subscriber)}), err))
		}
	}
}

// failureOf is a read's error as the store records it.
func failureOf(err error, now time.Time) wsm.PullRequestFailure {
	f := wsm.PullRequestFailure{Since: now, Kind: wsm.PullRequestFailureUnreachable, Detail: err.Error()}
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
	owed, err := w.deps.Store.OwedPullRequestNotices(ctx)
	if err != nil && ctx.Err() != nil {
		w.deps.Log.Debug(opNotice, "the delivery stopped with its context", nil)
		return
	}
	if err != nil {
		w.deps.Log.Error(opNotice, "could not read the owed pull-request notices; none is told this tick", dlog.WithCause(nil, err))
		return
	}
	for _, o := range owed {
		w.deliver(ctx, prompts, o)
	}
}

// deliver submits O to its subscriber and records what was submitted.
func (w *Watcher) deliver(ctx context.Context, prompts prompthandler.Submitter, o wsm.PullRequestOwed) {
	fields := dlog.Context{"subscriber": string(o.Subscriber), "pull_request": o.PullRequest.String(), "pending": len(o.Pending)}
	prev := untoldnotice.Notice{Turn: o.Turn, Sentences: o.Sentences}
	carried := untoldnotice.Carried(ctx, w.deps.Holds, w.deps.Log, opNotice, o.Subscriber, prev, o.Turn != "" || len(o.Sentences) > 0, fields)
	sentences := append(append([]string{}, carried...), o.Pending...)
	fields["sentences"] = len(sentences)
	turn, failure, err := w.submit(ctx, prompts, o, sentences)
	if err != nil {
		w.deps.Log.Error(opNotice, failure, dlog.WithCause(fields, err))
		// What the superseded notice said is kept to be said again; the
		// pending sentences stay pending for the next tick.
		if len(carried) > 0 {
			if rerr := w.deps.Store.RecordPullRequestNoticeSubmitted(ctx, o.Subscriber, o.PullRequest, "", carried, 0); rerr != nil {
				w.deps.Log.Error(opNotice, "could not keep the superseded notice's sentences; they are not said again", dlog.WithCause(fields, rerr))
			}
		}
		return
	}
	if err := w.deps.Store.RecordPullRequestNoticeSubmitted(ctx, o.Subscriber, o.PullRequest, turn, sentences, len(o.Pending)); err != nil {
		w.deps.Log.Error(opNotice, "could not record the submitted notice; its sentences may be told again", dlog.WithCause(fields, err))
		return
	}
	w.deps.Log.Info(opNotice, "told a subscriber about its pull request", dlog.Merge(fields, dlog.Context{"turn": string(turn)}))
}

// submit composes and submits one notice, answering its turn, or the record a
// failure is written with and why.
func (w *Watcher) submit(ctx context.Context, prompts prompthandler.Submitter, o wsm.PullRequestOwed, sentences []string) (ids.TurnID, string, error) {
	turn, err := briefprompt.Composer{Briefs: w.deps.Briefs, MintKey: w.deps.MintKey}.Submit(ctx, prompts, o.Subscriber, BriefNotice, map[string]string{
		"label":    "pull request `" + o.PullRequest.String() + "`",
		"activity": untoldnotice.Sentences(sentences),
		"status":   o.Status,
	}, "pull-request-notice", Origin)
	switch briefprompt.StepOf(err) {
	case briefprompt.StepCompose:
		return "", "could not compose a pull-request notice; the subscriber is told on a later tick", err
	case briefprompt.StepMint:
		return "", "could not mint a pull-request notice's idempotency key; the subscriber is told on a later tick", err
	case briefprompt.StepSubmit:
		return "", "could not submit a pull-request notice to its subscriber; it is told on a later tick", err
	}
	return turn, "", nil
}
