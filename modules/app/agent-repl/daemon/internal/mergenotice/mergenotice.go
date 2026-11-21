// Package mergenotice tells each MERGE QUEUE SUBSCRIBER's agent about the
// changes of the queues it subscribed to (agentrepl.v1.UpdateMergeQueueSubscribe).
//
// The merge orchestrator reports every change of every queue it makes, with
// the queue as it stands after it (merge.QueueObserver). This package keeps
// those changes in order in memory, and one loop delivers them: it reads the
// subscriptions at delivery time, picks each subscriber with a subscription in
// the change's scope, and submits ONE notice per subscriber.
//
// ONLY THREE CHANGES ARE TOLD: the queue DEPLETING, being PAUSED, and being
// RESUMED (Admits). A queue-wide subscriber waits on the queue as a whole. A
// notice wakes its subscriber and costs it a full read of its context, and
// what one merge does (put in line, started, stepped, landed, failed, taken
// off, deployed) is told by the per-workspace subscription to that merge's
// workspace (agentrepl.v1.SubscribeWorkspace), so the orchestrator still
// reports those changes (other observers read them) and this package drops
// them.
//
// A NOTICE IS ONE SHORT LINE: the queue, the change, and the queue's state in
// a few words (Notice). It never lists the queue; `merge-queue status` does.
//
// THE NOTICE TAKES THE PATH EVERY UNDER-THE-HOOD REPORT TO AN AGENT TAKES: a
// deferred prompt submitted through the prompt handler, exactly as an async
// merge's handback is (mergeworkspace.Service.HandBack), under its own origin
// (PROMPT_ORIGIN_MERGE_QUEUE_NOTICE), which the feed draws no row for. It is
// delivered on arrival (promptqueue's arrival.go): it joins a running turn
// after its current tool call and wakes an idle agent, and is never held.
//
// A SUBSCRIBER IS NEVER TOLD A QUEUE THAT NO LONGER STANDS, AND ITS NOTICES
// NEVER PILE UP. A notice can wait under a named arrival wait (the session not
// up, its cold gate, a merge driving it), and before notices were delivered
// on arrival a busy subscriber was handed each change as its own turn long
// after the queue had moved on ("pt1-path-test testing ... now" arriving
// twenty minutes after pt1-path-test landed, 2026-10-22). So each subscriber holds at most ONE
// untold notice per queue, recorded DURABLY (wsm UntoldMergeNotice) so a
// restart cannot forget it: a new change supersedes it (promptqueue
// Supersede) and the new notice says everything the superseded one said, then
// the change, then the queue as it now stands. The prompt queue's delivery
// lock decides a race between the supersession and the delivery, so every
// change is told exactly once.
//
// A SUBSCRIPTION ENDS WITH ITS QUEUE'S DEPLETION, ON THE DEPLETION ITSELF. A
// notice wakes its subscriber and costs it a full read of its context, so one
// left standing after the work it watched has landed costs that at every
// later change. Delivering a change that depleted a queue (a merge left and
// none is in line, so nothing runs) ends every subscription in that queue's
// scope, that repository's or every repository's, BEFORE the notice is
// submitted, and the notice says it is the subscription's last. Every detail
// admits a depletion, so every ended subscription is told. A subscription
// ends only as it was read (wsm EndMergeQueueSubscription): one its
// subscriber takes again after reading the last notice stands.
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
	"sync"

	conversationv1 "agentrepl/proto/conversation/v1"

	"agent-repld/internal/briefprompt"
	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/merge"
	"agent-repld/internal/noticeloop"
	"agent-repld/internal/prompthandler"
	"agent-repld/internal/resolve/sidebar"
	"agent-repld/internal/untoldnotice"
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
	// EndMergeQueueSubscription ends one subscription as it was read,
	// answering whether it ended it.
	EndMergeQueueSubscription(ctx context.Context, sub wsm.MergeQueueSubscription) (bool, error)
}

// Untold reads and records each subscriber's untold notice per queue (wsm
// UntoldMergeNotice), the durable half of the supersession.
type Untold interface {
	UntoldMergeNotice(ctx context.Context, subscriber ids.WorkspaceID, repo wsm.RepoKey) (wsm.UntoldMergeNotice, bool, error)
	RecordUntoldMergeNotice(ctx context.Context, n wsm.UntoldMergeNotice) error
}

// Deps are the service's collaborators: everything but the prompt handler,
// which is built after the merge orchestrator this service is handed to, and
// is handed to Run instead.
type Deps struct {
	Subscriptions Subscriptions
	// Holds retires a notice a newer one supersedes.
	Holds untoldnotice.Holds
	// Untold keeps each subscriber's untold notice across restarts.
	Untold Untold
	// Briefs loads and splices a brief at use time.
	Briefs merge.BriefLoader
	// Notices measures every notice this service submits
	// (briefprompt.Composer.Meter). REQUIRED.
	Notices briefprompt.Meter
	// MintKey mints an idempotency key for one submission
	// (prompthandler.MintKey): every notice is its own submission.
	MintKey func(prefix string) (string, error)
	Log     dlog.Surfaces
}

// Service is the subscriptions' notifier. It implements merge.QueueObserver.
type Service struct {
	deps Deps
	loop *noticeloop.Loop[merge.QueueChange]

	mu sync.Mutex
	// through is, for each subscription this daemon saw taken, the Seq of
	// the last change made before it (merge.QueueObserver.Subscribed). A
	// subscription with no entry was taken before this daemon made any
	// change, and is told every one.
	through map[subscriptionKey]uint64
}

// subscriptionKey names one subscription: its subscriber and its scope.
type subscriptionKey struct {
	subscriber ids.WorkspaceID
	repo       wsm.RepoKey
}

var _ merge.QueueObserver = (*Service)(nil)

// New builds the service.
func New(deps Deps) *Service {
	s := &Service{deps: deps, through: map[subscriptionKey]uint64{}}
	s.loop = noticeloop.New(s.untold)
	return s
}

// Subscribed implements merge.QueueObserver: it records where in the
// changes' order the subscription was taken, and never blocks.
func (s *Service) Subscribed(subscriber ids.WorkspaceID, repo wsm.RepoKey, through uint64) {
	s.mu.Lock()
	defer s.mu.Unlock()
	s.through[subscriptionKey{subscriber, repo}] = through
}

// takenBefore answers the subscriptions among SUBS taken before CHANGE was
// made: a subscription taken after it (its answer already showing the
// change's effect) is neither told the change nor ended by it, however late
// the change is delivered.
func (s *Service) takenBefore(subs []wsm.MergeQueueSubscription, change merge.QueueChange) []wsm.MergeQueueSubscription {
	s.mu.Lock()
	defer s.mu.Unlock()
	var out []wsm.MergeQueueSubscription
	for _, sub := range subs {
		through, seen := s.through[subscriptionKey{sub.Subscriber, sub.Repo}]
		if !seen || change.Seq == 0 || change.Seq > through {
			out = append(out, sub)
		}
	}
	return out
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

// deliver tells CHANGE to every subscriber in its scope, once each. A change
// no subscription is told (Admits) is dropped before the subscriptions are
// read.
func (s *Service) deliver(ctx context.Context, prompts prompthandler.Submitter, change merge.QueueChange) {
	log := s.deps.Log.Global()
	if !Admits(change) {
		log.Debug(op, "a merge queue change no subscription is told; nobody is notified", changeFields(change))
		return
	}
	subs, err := s.deps.Subscriptions.MergeQueueSubscriptions(ctx)
	if err != nil {
		log.Error(op, "could not read the merge queue subscriptions; nobody is told of the change", dlog.WithCause(changeFields(change), err))
		return
	}
	// THE SUBSCRIPTIONS ARE READ AS THEY STOOD WHEN THE CHANGE WAS MADE, not
	// when it is delivered (2026-10-31).
	subs = s.takenBefore(subs, change)
	ended := s.endDepleted(ctx, subs, change)
	for _, subscriber := range Recipients(subs, change) {
		s.notify(ctx, prompts, subscriber, ended[subscriber], change)
	}
}

// endDepleted ends every subscription in the scope of a queue CHANGE
// depleted, and answers each subscriber whose every subscription in that
// scope it ended: that subscriber's notice is its last. A change that
// depleted nothing ends nothing.
func (s *Service) endDepleted(ctx context.Context, subs []wsm.MergeQueueSubscription, change merge.QueueChange) map[ids.WorkspaceID]bool {
	if !change.Depleted {
		return nil
	}
	log := s.deps.Log.Global()
	ended := map[ids.WorkspaceID]bool{}
	for _, sub := range subs {
		if !InScope(sub, change) {
			continue
		}
		fields := changeFields(change)
		fields["subscriber"], fields["scope"] = string(sub.Subscriber), string(sub.Repo)
		done, err := s.deps.Subscriptions.EndMergeQueueSubscription(ctx, sub)
		switch {
		case err != nil:
			log.Error(op, "could not end a merge queue subscription as its queue depleted; it stands, and its notice does not say it ended", dlog.WithCause(fields, err))
		case !done:
			log.Info(op, "a merge queue subscription was taken again after the depletion was read; the new one stands", fields)
		default:
			log.Info(op, "ended a merge queue subscription as its queue depleted", fields)
		}
		if prior, seen := ended[sub.Subscriber]; !seen || prior {
			ended[sub.Subscriber] = err == nil && done
		}
	}
	return ended
}

// InScope reports whether SUB watches the queue CHANGE is about: its own
// repository's, or every repository's.
func InScope(sub wsm.MergeQueueSubscription, change merge.QueueChange) bool {
	return sub.Repo == wsm.EveryRepository || sub.Repo == change.Queue.Repo
}

// Recipients answers who is told CHANGE: every subscriber holding a
// subscription in the change's queue's scope (that repository, or every
// repository), ONCE each, in the subscriptions' order, and nobody for a change
// no subscription is told.
func Recipients(subs []wsm.MergeQueueSubscription, change merge.QueueChange) []ids.WorkspaceID {
	if !Admits(change) {
		return nil
	}
	var out []ids.WorkspaceID
	seen := map[ids.WorkspaceID]bool{}
	for _, sub := range subs {
		if !InScope(sub, change) || seen[sub.Subscriber] {
			continue
		}
		seen[sub.Subscriber] = true
		out = append(out, sub.Subscriber)
	}
	return out
}

// Admits reports whether CHANGE is told to the subscriptions in its scope: a
// merge leaving that left no merge in line (the queue DEPLETED), a pause, or
// a resume. A merge put in line, a merge leaving while others wait, a
// merge's start, its steps, a reorder and a restart's requeue (a merge that
// never left the line) are not.
func Admits(change merge.QueueChange) bool {
	switch change.Kind {
	case merge.QueueChangePaused, merge.QueueChangeResumed:
		return true
	case merge.QueueChangeEnqueued, merge.QueueChangeRequeued, merge.QueueChangeStarted, merge.QueueChangeStepped, merge.QueueChangeReordered:
		return false
	}
	if change.Kind.Leaves() {
		return change.Depleted
	}
	panic(fmt.Sprintf("mergenotice: a queue change of undeclared kind %d", int(change.Kind)))
}

// notify composes and submits one notice of CHANGE to SUBSCRIBER, saying
// first every change of the subscriber's untold notice it supersedes, and
// records it as the subscriber's untold notice of the queue. ENDED says the
// change ended the subscriber's subscription, so the notice is its last.
func (s *Service) notify(ctx context.Context, prompts prompthandler.Submitter, subscriber ids.WorkspaceID, ended bool, change merge.QueueChange) {
	log := s.deps.Log.Global()
	fields := changeFields(change)
	fields["subscriber"], fields["ended"] = string(subscriber), ended
	sentences := append(s.carried(ctx, subscriber, change.Queue.Repo), Sentence(change))
	fields["changes"] = len(sentences)
	turn, failure, err := s.submit(ctx, prompts, subscriber, ended, sentences, change)
	if err != nil {
		log.Error(op, failure, dlog.WithCause(fields, err))
	} else {
		fields["turn"] = string(turn)
		log.Info(op, "told a subscriber's agent of a merge queue change", fields)
	}
	untold := wsm.UntoldMergeNotice{Subscriber: subscriber, Repo: change.Queue.Repo, Turn: turn, Sentences: sentences}
	if err := s.deps.Untold.RecordUntoldMergeNotice(ctx, untold); err != nil {
		log.Error(op, "could not record the subscriber's untold notice; the next notice cannot supersede it", dlog.WithCause(fields, err))
	}
}

// submit composes the notice of SENTENCES, the latest being CHANGE's, and
// submits it to SUBSCRIBER, answering the turn it was submitted as, or the
// record a failure is written with and why.
func (s *Service) submit(ctx context.Context, prompts prompthandler.Submitter, subscriber ids.WorkspaceID, ended bool, sentences []string, change merge.QueueChange) (ids.TurnID, string, error) {
	turn, err := briefprompt.Composer{Briefs: s.deps.Briefs, MintKey: s.deps.MintKey, Meter: s.deps.Notices}.
		Submit(ctx, prompts, subscriber, BriefNotice, Values(ended, sentences, change), "merge-queue-notice", Origin)
	switch briefprompt.StepOf(err) {
	case briefprompt.StepCompose:
		return "", "could not compose a merge queue notice; the subscriber is not told of the change", err
	case briefprompt.StepMint:
		return "", "could not mint a merge queue notice's idempotency key; the subscriber is not told of the change", err
	case briefprompt.StepSubmit:
		return "", "could not submit a merge queue notice to its subscriber", err
	}
	return turn, "", nil
}

// carried answers the sentences the subscriber's untold notice of REPO's queue
// said that the next notice must say again (untoldnotice.Carried). A notice
// that could not be read stands and is told, so nothing it said is carried.
func (s *Service) carried(ctx context.Context, subscriber ids.WorkspaceID, repo wsm.RepoKey) []string {
	log := s.deps.Log.Global()
	fields := dlog.Context{"subscriber": string(subscriber), "repo": string(repo)}
	prev, found, err := s.deps.Untold.UntoldMergeNotice(ctx, subscriber, repo)
	if err != nil {
		log.Error(op, "could not read the subscriber's untold notice; the next one says only the new change", dlog.WithCause(fields, err))
		return nil
	}
	return untoldnotice.Carried(ctx, s.deps.Holds, log, op, subscriber, untoldnotice.Notice{Turn: prev.Turn, Sentences: prev.Sentences}, found, fields)
}

// Values are the brief's placeholders for one notice of SENTENCES, oldest
// first, the last being LATEST's, all on one line: the queue the notice is
// about, every change, the queue's state as the latest change left it in a
// few words, and, when ENDED says LATEST ended the subscription, that it
// ended.
func Values(ended bool, sentences []string, latest merge.QueueChange) map[string]string {
	if len(sentences) == 0 {
		panic("mergenotice: a notice was composed of no change")
	}
	return map[string]string{
		"scope":    "merge queue of " + repositoryName(latest.Queue.Repo),
		"changes":  Changes(sentences),
		"queue":    " " + QueueState(latest.Queue),
		"lifetime": Lifetime(ended),
	}
}

// Lifetime is a notice's closing words on its subscription: that this notice
// is its last, or nothing.
func Lifetime(ended bool) string {
	if ended {
		return " Subscription ended."
	}
	return ""
}

// Changes states every change of a notice, oldest first, as one sentence:
// one change alone, several counted and joined. A notice of no change is a
// caller's defect.
func Changes(sentences []string) string {
	switch len(sentences) {
	case 0:
		panic("mergenotice: a notice was composed of no change")
	case 1:
		return sentences[0] + "."
	}
	return fmt.Sprintf("%d changes: %s.", len(sentences), strings.Join(sentences, "; "))
}

// Sentence states one told change in a few words, with no closing stop:
// Changes joins and closes them. A depletion names the merge whose leaving
// emptied the queue, and how it left.
func Sentence(change merge.QueueChange) string {
	switch change.Kind {
	case merge.QueueChangePaused:
		return "queue paused"
	case merge.QueueChangeResumed:
		return "queue resumed"
	case merge.QueueChangeLanded, merge.QueueChangeFailed, merge.QueueChangeDequeued:
		if !change.Depleted {
			break
		}
		return "queue depleted: `" + change.Subject.Name + "` " + leftWords(change) + " and none is left in line"
	}
	panic(fmt.Sprintf("mergenotice: a queue change of kind %s, depleting %t, is never told", change.Kind, change.Depleted))
}

// leftWords states how the merge whose leaving depleted the queue left it.
func leftWords(change merge.QueueChange) string {
	switch change.Kind {
	case merge.QueueChangeLanded:
		return "landed"
	case merge.QueueChangeFailed:
		return "failed"
	}
	return "was taken off (" + strings.ReplaceAll(change.Cause, "_", " ") + ")"
}

// QueueState states the queue as it stands in a few words: how many merges
// are in line, the front one and how it stands, how many fix their tests out
// of line, and whether it is paused. It never lists the queue.
func QueueState(queue merge.QueueSnapshot) string {
	var line string
	if len(queue.Members) == 0 {
		line = "Queue empty"
	} else {
		front := queue.Members[0]
		standing := "next"
		switch {
		case front.Started && front.Step != "":
			standing = front.Step.Words()
		case front.Started:
			standing = "started"
		}
		line = fmt.Sprintf("%d in line; `%s` %s", len(queue.Members), front.Name, standing)
	}
	for _, member := range queue.Remediating {
		switch member.Repair {
		case sidebar.RepairFixingTests:
			line += fmt.Sprintf("; `%s` fixing tests", member.Name)
		case sidebar.RepairResolvingConflicts:
			line += fmt.Sprintf("; `%s` resolving conflicts", member.Name)
		}
	}
	if n := awaitingRepair(queue.Remediating); n > 0 {
		line += fmt.Sprintf("; %d awaiting a repair slot", n)
	}
	if queue.Paused {
		line += "; paused"
	}
	return line + "."
}

// awaitingRepair counts the merges out of line awaiting a repair slot.
func awaitingRepair(outOfLine []merge.QueueMember) int {
	n := 0
	for _, member := range outOfLine {
		if member.Repair == sidebar.RepairAwaitingFixingTests {
			n++
		}
	}
	return n
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

// repositoryName is the repository's directory name, for a notice.
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
