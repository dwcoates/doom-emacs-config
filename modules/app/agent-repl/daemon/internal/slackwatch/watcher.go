// Package slackwatch follows Slack threads for workspaces and fires the
// reaction initiator: the daemon's successor to the gns-sockets plugin.
//
// A SUBSCRIBED THREAD IS STREAMED BY ONE WORKER, for as long as its
// subscription stands: the worker reads the channel's echo pipe through the
// user's `gns` (gnsclient) in bounded windows, and every message it reads is
// stored as pending with the cursor, in one transaction
// (wsm.RecordSlackThreadMessages). The workspace's own posts are dropped
// there, by their nonce.
//
// PENDING MESSAGES ARE TOLD AS ONE DIGEST once the thread has been quiet
// (DueAt): the short quiet period when any is urgent (Urgent), the long one
// otherwise, and never later than the cap from the oldest. The digest is a
// prompt of origin PROMPT_ORIGIN_SLACK_THREAD_NOTICE (digest.go).
//
// ONLY THE SERVING DAEMON STREAMS AND TELLS (intakegate): during a handover
// the incumbent stops at its next window and the successor resumes from the
// durable cursor.
package slackwatch

import (
	"context"
	"errors"
	"fmt"
	"sync"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"

	"agent-repld/internal/briefprompt"
	"agent-repld/internal/clock"
	"agent-repld/internal/dlog"
	"agent-repld/internal/gnsclient"
	"agent-repld/internal/ids"
	"agent-repld/internal/inflight"
	"agent-repld/internal/intakegate"
	"agent-repld/internal/merge"
	"agent-repld/internal/prompthandler"
	"agent-repld/internal/untoldnotice"
	"agent-repld/internal/wsm"
)

// The operations this package records under.
const (
	opStream    = "daemon.slackwatch.stream"
	opNotice    = "daemon.slackwatch.notice"
	opGate      = "daemon.slackwatch.gate"
	opVerb      = "daemon.slackwatch.verb"
	opReactions = "daemon.slackwatch.reactions"
)

// Origin is every digest's prompt origin.
const Origin = conversationv1.PromptOrigin_PROMPT_ORIGIN_SLACK_THREAD_NOTICE

// StreamWindow is how long one `gns pipe read --follow` runs before the
// worker ends it and starts the next from the durable cursor. It is a
// rotation, not a failure bound: it is what bounds how long a stream that
// hangs without a word goes unnoticed, and what lets the serving daemon hand
// a thread on. The gns-sockets plugin rotated its streams on the same 30s.
const StreamWindow = 30 * time.Second

// FailureBackoff is how long a worker waits after a failed window before the
// next, so a `gns` that fails at once is not run in a tight loop: one window.
const FailureBackoff = StreamWindow

// Store is the part of the state client the watcher uses (wsm,
// slackthreads.go and slackreactions.go).
type Store interface {
	SubscribeSlackThread(ctx context.Context, sub wsm.SlackThreadSubscription) (wsm.SlackThreadMode, bool, error)
	SetSlackThreadMode(ctx context.Context, subscriber ids.WorkspaceID, thread wsm.SlackThreadKey, mode wsm.SlackThreadMode, resetPosts bool) (wsm.SlackThreadMode, error)
	UnsubscribeSlackThread(ctx context.Context, subscriber ids.WorkspaceID, thread wsm.SlackThreadKey) (bool, error)
	SlackThreadSubscriptions(ctx context.Context) ([]wsm.SlackThreadSubscription, error)
	WorkspaceSlackThreadSubscriptions(ctx context.Context, subscriber ids.WorkspaceID) ([]wsm.SlackThreadSubscription, error)
	ReserveSlackThreadPost(ctx context.Context, subscriber ids.WorkspaceID, thread wsm.SlackThreadKey, nonce string, at time.Time) error
	RecordSlackThreadPost(ctx context.Context, subscriber ids.WorkspaceID, thread wsm.SlackThreadKey, nonce, ts string, counted bool) (int, error)
	SlackThreadPostNonces(ctx context.Context, subscriber ids.WorkspaceID, thread wsm.SlackThreadKey) ([]string, error)
	RecordSlackThreadMessages(ctx context.Context, r wsm.SlackThreadRead) ([]wsm.SlackThreadMessage, error)
	RecordSlackThreadStreamFailure(ctx context.Context, subscriber ids.WorkspaceID, thread wsm.SlackThreadKey, at time.Time, detail string) (bool, error)
	SlackThreadPending(ctx context.Context, subscriber ids.WorkspaceID, thread wsm.SlackThreadKey) ([]wsm.SlackThreadMessage, error)
	RecordSlackThreadDigest(ctx context.Context, d wsm.SlackThreadDigest) error
	SlackReactionSettings(ctx context.Context) (wsm.SlackReactionSettings, error)
	MarkSlackReactionFired(ctx context.Context, t wsm.SlackReactionTrigger, at time.Time) (bool, error)
	RecordSlackReactionBaseline(ctx context.Context, standing []wsm.SlackReactionTrigger, at time.Time) error
}

// Deps are the watcher's collaborators: everything but the prompt handler,
// which is handed to Run.
type Deps struct {
	Store Store
	GNS   gnsclient.Client
	// Holds retires a digest a newer one supersedes.
	Holds untoldnotice.Holds
	// Briefs loads and splices a brief at use time.
	Briefs merge.BriefLoader
	// MintKey mints an idempotency key for one submission.
	MintKey func(prefix string) (string, error)
	// NewNonce mints a post's nonce.
	NewNonce func() (string, error)
	// Serves reports whether this daemon serves (rollout.Controller's
	// ServesIntake): only the serving daemon streams, tells and polls.
	Serves func() bool
	// Intakes creates a fired reaction's intake workspace (reactions.go).
	Intakes IntakeCreator
	// Notices measures every digest told (briefprompt.Composer.Meter).
	Notices briefprompt.Meter
	Clock   clock.Clock
	// Log is the global logger: a thread is no one workspace's.
	Log dlog.Logger
}

// Watcher follows every subscribed thread and runs the reaction initiator.
type Watcher struct {
	deps Deps
	gate *intakegate.Gate

	mu sync.Mutex
	// life is Run's context, nil before Run; a worker started by a verb runs
	// under it.
	life context.Context
	// workers are the running workers, by subscription, each with its stop.
	workers map[subKey]context.CancelFunc
	// due is when each subscription's digest is owed.
	due map[subKey]time.Time
	// wake is pinged whenever due changes.
	wake chan struct{}
	// username is the GNS login once read, and loginErr the latest failure
	// to read it.
	username string
	loginErr error
	// polls is the reaction initiator's latest poll, for the status.
	polls PollState
}

// subKey names one subscription.
type subKey struct {
	ws     ids.WorkspaceID
	thread wsm.SlackThreadKey
}

func keyOf(sub wsm.SlackThreadSubscription) subKey {
	return subKey{ws: sub.Subscriber, thread: sub.Thread}
}

func (k subKey) fields() dlog.Context {
	return dlog.Context{"subscriber": string(k.ws), "thread": k.thread.String()}
}

// New builds the watcher, refusing a missing collaborator.
func New(deps Deps) (*Watcher, error) {
	if deps.Store == nil || deps.GNS == nil || deps.Holds == nil || deps.Briefs == nil || deps.MintKey == nil || deps.NewNonce == nil ||
		deps.Serves == nil || deps.Intakes == nil || deps.Notices == nil || deps.Clock == nil || deps.Log == nil {
		return nil, errors.New("slackwatch: every collaborator is required")
	}
	return &Watcher{deps: deps, gate: intakegate.NewFor(deps.Serves, deps.Log, opGate, "the Slack streams and polls"),
		workers: map[subKey]context.CancelFunc{}, due: map[subKey]time.Time{}, wake: make(chan struct{}, 1)}, nil
}

// Run streams every subscribed thread, tells every digest when it is due,
// and polls the reaction initiator, until ctx ends.
func (w *Watcher) Run(ctx context.Context, prompts prompthandler.Submitter) {
	w.mu.Lock()
	w.life = ctx
	w.mu.Unlock()
	subs, err := w.deps.Store.SlackThreadSubscriptions(ctx)
	if err != nil && ctx.Err() == nil {
		w.deps.Log.Error(opStream, "could not read the Slack-thread subscriptions; none is streamed until one is taken", dlog.WithCause(nil, err))
	}
	for _, sub := range subs {
		w.startWorker(keyOf(sub))
		if sub.Pending > 0 {
			w.reschedule(ctx, keyOf(sub))
		}
	}
	var wg sync.WaitGroup
	wg.Add(1)
	// THE INITIATOR'S POLL LOOP STANDS for Run's life, and Run joins it.
	inflight.Standing(func() {
		defer wg.Done()
		w.runReactions(ctx)
	})
	for {
		wait, ok := w.untilNextDue()
		var timer <-chan time.Time
		if ok {
			timer = w.deps.Clock.After(wait)
		}
		select {
		case <-ctx.Done():
			w.stopWorkers()
			wg.Wait()
			w.deps.Log.Debug(opStream, "the Slack watcher stopped with its context", nil)
			return
		case <-w.wake:
		case <-timer:
		}
		w.FlushDue(ctx, prompts)
	}
}

// untilNextDue answers how long until the earliest owed digest, false when
// none is owed.
func (w *Watcher) untilNextDue() (time.Duration, bool) {
	w.mu.Lock()
	defer w.mu.Unlock()
	var next time.Time
	for _, at := range w.due {
		if next.IsZero() || at.Before(next) {
			next = at
		}
	}
	if next.IsZero() {
		return 0, false
	}
	return max(next.Sub(w.deps.Clock.Now()), 0), true
}

// ping wakes Run's loop without blocking.
func (w *Watcher) ping() {
	select {
	case w.wake <- struct{}{}:
	default:
	}
}

// startWorker starts the worker of KEY when Run has started and none runs.
func (w *Watcher) startWorker(key subKey) {
	w.mu.Lock()
	defer w.mu.Unlock()
	if w.life == nil || w.workers[key] != nil {
		return
	}
	ctx, cancel := context.WithCancel(w.life)
	w.workers[key] = cancel
	// A THREAD'S STREAM STANDS until its subscription ends or Run's does;
	// what each message it reads causes is counted where it is told.
	inflight.Standing(func() {
		w.work(ctx, key)
		w.mu.Lock()
		delete(w.workers, key)
		w.mu.Unlock()
		cancel()
	})
}

// stopWorker stops KEY's worker, if one runs.
func (w *Watcher) stopWorker(key subKey) {
	w.mu.Lock()
	defer w.mu.Unlock()
	if cancel := w.workers[key]; cancel != nil {
		cancel()
	}
	delete(w.due, key)
}

func (w *Watcher) stopWorkers() {
	w.mu.Lock()
	defer w.mu.Unlock()
	for _, cancel := range w.workers {
		cancel()
	}
}

// work streams KEY's thread window after window until its subscription ends
// or ctx does.
func (w *Watcher) work(ctx context.Context, key subKey) {
	for ctx.Err() == nil {
		sub, ok, err := w.lookup(ctx, key)
		switch {
		case ctx.Err() != nil:
			return
		case err != nil:
			w.deps.Log.Error(opStream, "could not read the subscription; the thread is read again after a window", dlog.WithCause(key.fields(), err))
			w.wait(ctx, FailureBackoff)
			continue
		case !ok:
			w.deps.Log.Debug(opStream, "the subscription ended; its worker stops", key.fields())
			return
		}
		if !w.gate.Admits() {
			w.wait(ctx, StreamWindow)
			continue
		}
		if err := w.StreamOnce(ctx, sub); err != nil {
			w.recordStreamFailure(ctx, key, err)
			w.wait(ctx, FailureBackoff)
		}
	}
}

// wait waits D or until ctx ends.
func (w *Watcher) wait(ctx context.Context, d time.Duration) {
	select {
	case <-ctx.Done():
	case <-w.deps.Clock.After(d):
	}
}

// lookup reads KEY's subscription, false when it no longer stands.
func (w *Watcher) lookup(ctx context.Context, key subKey) (wsm.SlackThreadSubscription, bool, error) {
	subs, err := w.deps.Store.WorkspaceSlackThreadSubscriptions(ctx, key.ws)
	if err != nil {
		return wsm.SlackThreadSubscription{}, false, err
	}
	for _, sub := range subs {
		if sub.Thread == key.thread {
			return sub, true, nil
		}
	}
	return wsm.SlackThreadSubscription{}, false, nil
}

// login answers the GNS username, reading it once and again after a failure.
func (w *Watcher) login(ctx context.Context) (string, error) {
	w.mu.Lock()
	username := w.username
	w.mu.Unlock()
	if username != "" {
		return username, nil
	}
	username, err := w.deps.GNS.Whoami(ctx)
	w.mu.Lock()
	defer w.mu.Unlock()
	w.username, w.loginErr = username, err
	return username, err
}

// StreamOnce streams SUB's thread for one window, storing every message it
// reads as it arrives, and the window's last cursor at its end. A failure is
// the window's: the messages it read before failing are stored.
func (w *Watcher) StreamOnce(ctx context.Context, sub wsm.SlackThreadSubscription) error {
	username, err := w.login(ctx)
	if err != nil {
		return fmt.Errorf("read the gns login: %w", err)
	}
	window, cancel := context.WithTimeout(ctx, StreamWindow)
	defer cancel()
	key := keyOf(sub)
	last, err := w.deps.GNS.Stream(window, gnsclient.PipeFor(username, sub.Thread.Channel), sub.Cursor, sub.Thread.ThreadTS, func(m gnsclient.PipeMessage) {
		w.ingest(ctx, key, []gnsclient.PipeMessage{m}, m.Seq)
	})
	if ctx.Err() != nil {
		return nil
	}
	// The window's end advances the cursor past entries that carried no
	// message (an edit, a deletion), which the stream reads and skips.
	if last > sub.Cursor {
		w.ingest(ctx, key, nil, last)
	}
	return err
}

// ingest stores MSGS for KEY as pending with CURSOR, and schedules the
// subscription's digest when any was new.
func (w *Watcher) ingest(ctx context.Context, key subKey, msgs []gnsclient.PipeMessage, cursor int64) {
	now := w.deps.Clock.Now()
	read := wsm.SlackThreadRead{Subscriber: key.ws, Thread: key.thread, Cursor: cursor, At: now}
	for _, m := range msgs {
		read.Messages = append(read.Messages, wsm.SlackThreadMessage{TS: m.TS, Author: m.Author, Text: m.Text, Urgent: Urgent(m.Text), ReceivedAt: now})
	}
	stored, err := w.deps.Store.RecordSlackThreadMessages(ctx, read)
	if err != nil {
		if ctx.Err() == nil {
			w.deps.Log.Error(opStream, "could not store the thread's messages; the stream reads them again from the stored cursor", dlog.WithCause(dlog.Merge(key.fields(), dlog.Context{"read": len(msgs), "cursor": cursor}), err))
		}
		return
	}
	if len(stored) == 0 {
		return
	}
	w.deps.Log.Debug(opStream, "stored the thread's new messages for the next digest", dlog.Merge(key.fields(), dlog.Context{"stored": len(stored), "cursor": cursor}))
	w.reschedule(ctx, key)
}

// recordStreamFailure records a failed window of KEY. `gns`'s failure is
// recorded by gnsclient; this records the subscription's failing stretch.
func (w *Watcher) recordStreamFailure(ctx context.Context, key subKey, cause error) {
	began, err := w.deps.Store.RecordSlackThreadStreamFailure(ctx, key.ws, key.thread, w.deps.Clock.Now(), cause.Error())
	switch {
	case err != nil && ctx.Err() == nil:
		w.deps.Log.Error(opStream, "could not record the thread's failing stream", dlog.WithCause(key.fields(), err))
	case began:
		w.deps.Log.Info(opStream, "the thread's stream began failing; it is read again each window until it succeeds", dlog.WithCause(key.fields(), cause))
	}
}

// reschedule computes when KEY's digest is owed from its pending messages,
// and wakes Run.
func (w *Watcher) reschedule(ctx context.Context, key subKey) {
	pending, err := w.deps.Store.SlackThreadPending(ctx, key.ws, key.thread)
	if err != nil {
		if ctx.Err() == nil {
			w.deps.Log.Error(opNotice, "could not read the thread's pending messages; its digest is owed at once", dlog.WithCause(key.fields(), err))
			w.schedule(key, w.deps.Clock.Now())
		}
		return
	}
	if len(pending) == 0 {
		return
	}
	w.schedule(key, DueAt(pending))
}

func (w *Watcher) schedule(key subKey, at time.Time) {
	w.mu.Lock()
	w.due[key] = at
	w.mu.Unlock()
	w.ping()
}

// FlushDue tells every digest owed by now.
func (w *Watcher) FlushDue(ctx context.Context, prompts prompthandler.Submitter) {
	if !w.gate.Admits() {
		return
	}
	now := w.deps.Clock.Now()
	var owed []subKey
	w.mu.Lock()
	for key, at := range w.due {
		if !at.After(now) {
			owed = append(owed, key)
			delete(w.due, key)
		}
	}
	w.mu.Unlock()
	for _, key := range owed {
		if ctx.Err() != nil {
			return
		}
		if !w.deliver(ctx, prompts, key) {
			w.schedule(key, now.Add(LowQuiet))
		}
	}
}
