// Package mergenotice tells each MERGE QUEUE SUBSCRIBER's agent about the
// changes of the queues it subscribed to (agentrepl.v1.UpdateMergeQueueSubscribe).
//
// The merge orchestrator reports every change of every queue it makes, with
// the queue as it stands after it (merge.QueueObserver). This package keeps
// those changes in order in memory, and one loop delivers them: it reads the
// subscriptions at delivery time, picks each subscriber whose subscription
// admits the change at its detail, and submits ONE notice per subscriber.
//
// THE NOTICE TAKES THE PATH EVERY UNDER-THE-HOOD REPORT TO AN AGENT TAKES: a
// deferred prompt submitted through the prompt handler, exactly as an async
// merge's handback is (mergeworkspace.Service.HandBack), under its own origin
// (PROMPT_ORIGIN_MERGE_QUEUE_NOTICE), which the feed draws no row for. It
// waits behind a running turn and wakes an idle agent.
//
// A SUBSCRIBER IS NEVER TOLD A QUEUE THAT NO LONGER STANDS, AND ITS NOTICES
// NEVER PILE UP. A notice waits behind the subscriber's running turn, and a
// busy subscriber was handed each change as its own turn long after the queue
// had moved on ("pt1-path-test testing ... now" arriving twenty minutes after
// pt1-path-test landed, 2026-10-22). So each subscriber holds at most ONE
// untold notice per queue: a new change supersedes it (promptqueue
// Supersede) and the new notice says everything the superseded one said, then
// the change, then the queue as it now stands. The prompt queue's delivery
// lock decides a race between the supersession and the delivery, so every
// change is told exactly once.
//
// THE ORCHESTRATOR NEVER WAITS ON A DELIVERY. QueueChanged only appends, so a
// queue write is never held up by, and never re-enters through, the prompt
// queue; the loop does the delivering. The loop outlives the daemon's merge
// drain and delivers everything pending before it ends, so no change a
// finishing merge reports is lost to the exit.
package mergenotice

import (
	"context"
	"fmt"
	"path/filepath"
	"strings"

	conversationv1 "agentrepl/proto/conversation/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/merge"
	"agent-repld/internal/noticeloop"
	"agent-repld/internal/prompthandler"
	"agent-repld/internal/resolve/feed"
	"agent-repld/internal/wsm"
)

// op is the operation every record of this package carries.
const op = "daemon.mergenotice"

// BriefNotice is the brief a notice is composed from. Its first line is the
// notice's headline.
const BriefNotice = "merge-queue-notice"

// Origin is every notice's prompt origin.
const Origin = conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_QUEUE_NOTICE

// Subscriptions reads the merge queue subscriptions.
type Subscriptions interface {
	MergeQueueSubscriptions(ctx context.Context) ([]wsm.MergeQueueSubscription, error)
}

// Holds retires a subscriber's untold notice (promptqueue.Queue's Supersede):
// true when it was retired, false when it was already told.
type Holds interface {
	Supersede(ctx context.Context, ws ids.WorkspaceID, turn ids.TurnID) (bool, error)
}

// Deps are the service's collaborators: everything but the prompt handler,
// which is built after the merge orchestrator this service is handed to, and
// is handed to Run instead.
type Deps struct {
	Subscriptions Subscriptions
	// Holds retires a notice a newer one supersedes.
	Holds Holds
	// Briefs loads and splices a brief at use time.
	Briefs merge.BriefLoader
	// MintKey mints an idempotency key for one submission
	// (prompthandler.MintKey): every notice is its own submission.
	MintKey func(prefix string) (string, error)
	Log     dlog.Surfaces
}

// Service is the subscriptions' notifier. It implements merge.QueueObserver.
type Service struct {
	deps Deps
	loop *noticeloop.Loop[merge.QueueChange]
	// pending is each subscriber's last notice per queue that may not have been
	// told yet. ONLY THE LOOP touches it (Run's one delivering goroutine), so
	// it needs no lock.
	pending map[noticeKey]untoldNotice
}

// noticeKey is one subscriber's notices of one queue.
type noticeKey struct {
	subscriber ids.WorkspaceID
	repo       wsm.RepoKey
}

// untoldNotice is a notice submitted and perhaps not yet told: the turn it
// was submitted as, empty when its submission failed, and every change it
// says, oldest first.
type untoldNotice struct {
	turn    ids.TurnID
	changes []merge.QueueChange
}

var _ merge.QueueObserver = (*Service)(nil)

// New builds the service.
func New(deps Deps) *Service {
	s := &Service{deps: deps, pending: map[noticeKey]untoldNotice{}}
	s.loop = noticeloop.New(s.untold)
	return s
}

// QueueChanged implements merge.QueueObserver: it appends CHANGE for the loop
// and never blocks.
func (s *Service) QueueChanged(change merge.QueueChange) {
	s.loop.Report(change)
}

// Run delivers every reported change, in order, through PROMPTS until ctx
// ends, and then delivers every change still pending before it returns. The
// daemon ends it only after its merge drain, so a merge landing inside the
// drain is still told. A change reported after Run has returned is never
// told (untold).
func (s *Service) Run(ctx context.Context, prompts prompthandler.Submitter) {
	s.loop.Run(ctx, func(ctx context.Context, change merge.QueueChange) { s.deliver(ctx, prompts, change) })
	s.deps.Log.Global().Debug(op, "the notifier delivered every pending merge queue change and stopped", nil)
}

// untold records a change reported after the notifier stopped. The daemon is
// leaving and the change is one a stood-down merge made; the merge's
// successor resumes it, and its next notice carries the queue as it then
// stands, as the subscription's contract states.
func (s *Service) untold(change merge.QueueChange) {
	s.deps.Log.Global().Info(op, "a merge queue change came after the notifier stopped, as the daemon stood down; it is not told, and the next notice carries its effect", changeFields(change))
}

// deliver tells CHANGE to every subscriber whose subscription admits it, once
// each.
func (s *Service) deliver(ctx context.Context, prompts prompthandler.Submitter, change merge.QueueChange) {
	log := s.deps.Log.Global()
	subs, err := s.deps.Subscriptions.MergeQueueSubscriptions(ctx)
	if err != nil {
		log.Error(op, "could not read the merge queue subscriptions; nobody is told of the change", dlog.WithCause(changeFields(change), err))
		return
	}
	for _, told := range Recipients(subs, change) {
		s.notify(ctx, prompts, told, change)
	}
}

// Recipient is one subscriber a change is told to, at the detail of the
// subscription that admitted it.
type Recipient struct {
	Subscriber ids.WorkspaceID
	Detail     wsm.MergeQueueDetail
}

// Recipients answers who is told CHANGE: every subscriber holding a
// subscription in the change's queue's scope (that repository, or every
// repository) whose detail admits it, ONCE each, in the subscriptions' order.
// A subscriber holding both a scoped and an unscoped subscription that admit
// the change is told at the finer of the two details.
func Recipients(subs []wsm.MergeQueueSubscription, change merge.QueueChange) []Recipient {
	var out []Recipient
	at := map[ids.WorkspaceID]int{}
	for _, sub := range subs {
		if sub.Repo != wsm.EveryRepository && sub.Repo != change.Queue.Repo {
			continue
		}
		if !Admits(sub.Detail, change) {
			continue
		}
		if i, seen := at[sub.Subscriber]; seen {
			if sub.Detail < out[i].Detail {
				out[i].Detail = sub.Detail
			}
			continue
		}
		at[sub.Subscriber] = len(out)
		out = append(out, Recipient{Subscriber: sub.Subscriber, Detail: sub.Detail})
	}
	return out
}

// Admits reports whether a subscription at DETAIL is told CHANGE: every
// change at every_change, a merge leaving at advances, and a leave that
// emptied the line at depletes.
func Admits(detail wsm.MergeQueueDetail, change merge.QueueChange) bool {
	switch detail {
	case wsm.MergeQueueDetailEveryChange:
		return true
	case wsm.MergeQueueDetailAdvances:
		return change.Kind.Leaves()
	case wsm.MergeQueueDetailDepletes:
		return change.Kind.Leaves() && change.Depleted
	}
	panic(fmt.Sprintf("mergenotice: a subscription holds the undeclared detail %d", int(detail)))
}

// notify composes and submits one notice of CHANGE to TOLD, saying first
// every change of the subscriber's untold notice it supersedes.
func (s *Service) notify(ctx context.Context, prompts prompthandler.Submitter, told Recipient, change merge.QueueChange) {
	log := s.deps.Log.Global()
	fields := changeFields(change)
	fields["subscriber"], fields["detail"] = string(told.Subscriber), told.Detail.String()
	key := noticeKey{subscriber: told.Subscriber, repo: change.Queue.Repo}
	changes := append(s.carried(ctx, key, fields), change)
	fields["changes"] = len(changes)
	text, err := s.deps.Briefs(BriefNotice, Values(told.Detail, changes))
	if err != nil {
		log.Error(op, "could not compose a merge queue notice; the subscriber is not told of the change", dlog.WithCause(fields, err))
		s.keepUntold(key, "", changes)
		return
	}
	submitKey, err := s.deps.MintKey("merge-queue-notice")
	if err != nil {
		log.Error(op, "could not mint a merge queue notice's idempotency key; the subscriber is not told of the change", dlog.WithCause(fields, err))
		s.keepUntold(key, "", changes)
		return
	}
	outcome, err := prompts.Submit(ctx, told.Subscriber, feed.SaidText(text), submitKey, Origin, nil, wsm.DeliveryDeferred, nil)
	if err != nil {
		log.Error(op, "could not submit a merge queue notice to its subscriber", dlog.WithCause(fields, err))
		s.keepUntold(key, "", changes)
		return
	}
	s.keepUntold(key, outcome.Turn, changes)
	fields["turn"] = string(outcome.Turn)
	log.Info(op, "told a subscriber's agent of a merge queue change", fields)
}

// carried answers the changes the subscriber's untold notice of KEY's queue
// said, which the next notice must say again: all of them when the notice was
// retired before it was told (or never submitted), none when it was told. A
// notice that could not be retired stands and is still told, so nothing it
// said is carried.
func (s *Service) carried(ctx context.Context, key noticeKey, fields dlog.Context) []merge.QueueChange {
	log := s.deps.Log.Global()
	prev, ok := s.pending[key]
	if !ok {
		return nil
	}
	delete(s.pending, key)
	if prev.turn == "" {
		log.Info(op, "the subscriber's last notice was never submitted; the next one says its changes", dlog.Context{
			"subscriber": fields["subscriber"], "repo": fields["repo"], "carried": len(prev.changes)})
		return prev.changes
	}
	retired, err := s.deps.Holds.Supersede(ctx, key.subscriber, prev.turn)
	if err != nil {
		log.Error(op, "could not supersede the subscriber's untold notice; it stands and is told, and the next one says only the new change", dlog.WithCause(dlog.Context{
			"subscriber": fields["subscriber"], "repo": fields["repo"], "superseded_turn": string(prev.turn)}, err))
		return nil
	}
	if !retired {
		log.Debug(op, "the subscriber's last notice was told; the next one says only the new change", dlog.Context{
			"subscriber": fields["subscriber"], "repo": fields["repo"], "told_turn": string(prev.turn)})
		return nil
	}
	log.Info(op, "the subscriber's untold notice was superseded; the next one says its changes first", dlog.Context{
		"subscriber": fields["subscriber"], "repo": fields["repo"], "superseded_turn": string(prev.turn), "carried": len(prev.changes)})
	return prev.changes
}

// keepUntold records the subscriber's latest notice of KEY's queue as perhaps
// untold: submitted as TURN, or never submitted when TURN is empty.
func (s *Service) keepUntold(key noticeKey, turn ids.TurnID, changes []merge.QueueChange) {
	s.pending[key] = untoldNotice{turn: turn, changes: changes}
}

// Values are the brief's placeholders for one notice of CHANGES, oldest
// first: the latest change's headline, every change stated in full, the queue
// as the latest change left it, and the detail the subscriber asked for.
func Values(detail wsm.MergeQueueDetail, changes []merge.QueueChange) map[string]string {
	if len(changes) == 0 {
		panic("mergenotice: a notice was composed of no change")
	}
	latest := changes[len(changes)-1]
	return map[string]string{
		"label":  Headline(latest),
		"change": Sentences(changes),
		"queue":  Queue(latest.Queue),
		"detail": detailWords(detail),
	}
}

// Sentences states CHANGES in full, oldest first: one change as its sentence,
// several as a list under how many there were.
func Sentences(changes []merge.QueueChange) string {
	if len(changes) == 1 {
		return Sentence(changes[0])
	}
	var b strings.Builder
	b.WriteString(fmt.Sprintf("It changed %d times since the last notice you were told, oldest first:", len(changes)))
	for _, change := range changes {
		b.WriteString("\n- " + Sentence(change))
	}
	return b.String()
}

// Headline is a notice's first line: the queue and what happened to it.
func Headline(change merge.QueueChange) string {
	head := "merge queue of " + repositoryName(change.Queue.Repo) + ": "
	switch change.Kind {
	case merge.QueueChangePaused:
		return head + "paused"
	case merge.QueueChangeResumed:
		return head + "resumed"
	}
	line := head + change.Subject.Name + " " + kindWords(change)
	if change.Depleted {
		line += "; the queue is empty"
	}
	return line
}

// Sentence states the change in full.
func Sentence(change merge.QueueChange) string {
	subject := fmt.Sprintf("The %s merge of `%s` (branch `%s`)", change.Subject.Mode, change.Subject.Name, change.Subject.Branch)
	switch change.Kind {
	case merge.QueueChangeEnqueued:
		return subject + " was put in line."
	case merge.QueueChangeRequeued:
		return subject + ", which a daemon restart cut short, was put back in line to run again."
	case merge.QueueChangeStarted:
		return subject + " started."
	case merge.QueueChangeStepped:
		return subject + " moved to its " + change.Subject.Step.Words() + " step."
	case merge.QueueChangeLanded:
		return subject + " landed and left the queue."
	case merge.QueueChangeFailed:
		return subject + " failed and left the queue."
	case merge.QueueChangeDequeued:
		return subject + " was taken off the queue (" + strings.ReplaceAll(change.Cause, "_", " ") + ")."
	case merge.QueueChangeReordered:
		return subject + " moved to a new place in line."
	case merge.QueueChangePaused:
		return "The queue was paused: nothing new starts until it is resumed."
	case merge.QueueChangeResumed:
		return "The queue was resumed."
	}
	panic(fmt.Sprintf("mergenotice: a queue change of undeclared kind %d", int(change.Kind)))
}

// Queue states the queue as it stands, every member front first.
func Queue(queue merge.QueueSnapshot) string {
	state := "running"
	if queue.Paused {
		state = "paused"
	}
	if len(queue.Members) == 0 {
		return fmt.Sprintf("The queue of `%s` is now empty (%s).", repositoryDir(queue.Repo), state)
	}
	var b strings.Builder
	b.WriteString(fmt.Sprintf("The queue of `%s` now (%s), front first:", repositoryDir(queue.Repo), state))
	for i, member := range queue.Members {
		standing := "waiting"
		switch {
		case member.Started && member.Step != "":
			standing = "started, " + member.Step.Words()
		case member.Started:
			standing = "started"
		}
		b.WriteString(fmt.Sprintf("\n%d. `%s`: branch `%s`, %s, %s", i+1, member.Name, member.Branch, member.Mode, standing))
	}
	return b.String()
}

// kindWords is the headline's verb for a change about one merge.
func kindWords(change merge.QueueChange) string {
	switch change.Kind {
	case merge.QueueChangeEnqueued:
		return "put in line"
	case merge.QueueChangeRequeued:
		return "put back in line"
	case merge.QueueChangeStarted:
		return "started"
	case merge.QueueChangeStepped:
		return change.Subject.Step.Words()
	case merge.QueueChangeLanded:
		return "landed"
	case merge.QueueChangeFailed:
		return "failed"
	case merge.QueueChangeDequeued:
		return "taken off the queue"
	case merge.QueueChangeReordered:
		return "moved in line"
	}
	panic(fmt.Sprintf("mergenotice: a queue change of kind %s has no headline verb", change.Kind))
}

// detailWords names the detail a subscriber asked for, as the notice says it.
func detailWords(detail wsm.MergeQueueDetail) string {
	switch detail {
	case wsm.MergeQueueDetailEveryChange:
		return "every change"
	case wsm.MergeQueueDetailAdvances:
		return "each merge leaving the queue"
	case wsm.MergeQueueDetailDepletes:
		return "the queue emptying"
	}
	panic(fmt.Sprintf("mergenotice: a subscription holds the undeclared detail %d", int(detail)))
}

// repositoryDir is the repository's main checkout a queue key names: the key
// is the repository's common dir, its `.git`.
func repositoryDir(repo wsm.RepoKey) string {
	key := string(repo)
	if filepath.Base(key) == ".git" {
		return filepath.Dir(key)
	}
	return key
}

// repositoryName is the repository's directory name, for a headline.
func repositoryName(repo wsm.RepoKey) string {
	return filepath.Base(repositoryDir(repo))
}

// changeFields are the record fields that identify one change.
func changeFields(change merge.QueueChange) dlog.Context {
	return dlog.Context{
		"repo": string(change.Queue.Repo), "change": change.Kind.String(), "workspace": string(change.Subject.Workspace),
		"depleted": change.Depleted, "members": len(change.Queue.Members),
	}
}
