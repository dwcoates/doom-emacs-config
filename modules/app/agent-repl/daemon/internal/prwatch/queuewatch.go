package prwatch

import (
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"strings"
	"time"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	conversationv1 "agentrepl/proto/conversation/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ghclient"
	"agent-repld/internal/ids"
	"agent-repld/internal/wsm"
)

// The whole-queue follow: a workspace told each time a repository branch's
// GitHub merge queue moves (agentrepl/v1/github_merge_queue.proto). It reads
// the queue's entries in order, compares them with what each subscriber was
// last told, and reads each entry that left once more, to tell whether it
// merged or was removed.

// opQueuePoll is the operation the queue polls record under.
const opQueuePoll = "daemon.prwatch.queue_poll"

// BriefQueueNotice is the brief a queue notice is composed from.
const BriefQueueNotice = "github-merge-queue-notice"

// QueueOrigin is every queue notice's prompt origin.
const QueueOrigin = conversationv1.PromptOrigin_PROMPT_ORIGIN_GITHUB_MERGE_QUEUE_NOTICE

// queueTold is what one subscriber was told of a queue: a subscription's
// row's snapshot (wsm.GithubMergeQueueSubscription.Told), its entries head
// first.
type queueTold struct {
	Entries []queueToldEntry `json:"entries"`
}

// queueToldEntry is one entry as told.
type queueToldEntry struct {
	Number uint32 `json:"number"`
	Title  string `json:"title"`
}

// queueToldOf is Q as a snapshot.
func queueToldOf(q remoteQueue) queueTold {
	t := queueTold{Entries: []queueToldEntry{}}
	for _, e := range q.Entries {
		t.Entries = append(t.Entries, queueToldEntry{Number: e.Number, Title: e.Title})
	}
	return t
}

// encodeQueueTold writes a snapshot. A snapshot is numbers and strings, so
// it always encodes; a failure is a defect.
func encodeQueueTold(t queueTold) string {
	b, err := json.Marshal(t)
	if err != nil {
		panic(fmt.Sprintf("prwatch: encode a queue snapshot: %v", err))
	}
	return string(b)
}

// decodeQueueTold reads a row's snapshot, which a subscription always holds.
func decodeQueueTold(raw string) (queueTold, error) {
	var t queueTold
	if err := json.Unmarshal([]byte(raw), &t); err != nil {
		return t, fmt.Errorf("prwatch: the queue snapshot is unreadable: %w", err)
	}
	return t, nil
}

// departedFrom answers the entries PREV told that Q no longer holds.
func departedFrom(prev queueTold, q remoteQueue) []queueToldEntry {
	held := map[uint32]bool{}
	for _, e := range q.Entries {
		held[e.Number] = true
	}
	var out []queueToldEntry
	for _, e := range prev.Entries {
		if !held[e.Number] {
			out = append(out, e)
		}
	}
	return out
}

// diffQueueEntries answers how KEY's queue Q moved since PREV: each entry
// that left, by DEPARTED, in the order it stood; each that joined, in queue
// order; a new head; and an emptying. A departed entry DEPARTED does not
// hold left without a merge GitHub could name.
func diffQueueEntries(key wsm.GithubMergeQueueKey, prev queueTold, q remoteQueue, departed map[uint32]remoteDeparture) []*agentreplv1.GithubMergeQueueActivity {
	var out []*agentreplv1.GithubMergeQueueActivity
	for _, e := range departedFrom(prev, q) {
		ref := queuePullRef(key, e.Number)
		d, known := departed[e.Number]
		title := e.Title
		if known && d.Title != "" {
			title = d.Title
		}
		if known && d.Merged {
			out = append(out, &agentreplv1.GithubMergeQueueActivity{Activity: &agentreplv1.GithubMergeQueueActivity_Merged{Merged: &agentreplv1.GithubMergeQueueActivityMerged{
				PullRequest: ref, Title: title, MergeCommitSha: d.MergeSHA}}})
			continue
		}
		removed := &agentreplv1.GithubMergeQueueActivityRemoved{PullRequest: ref, Title: title}
		if known {
			removed.Reason = d.Reason
		}
		out = append(out, &agentreplv1.GithubMergeQueueActivity{Activity: &agentreplv1.GithubMergeQueueActivity_Removed{Removed: removed}})
	}
	told := map[uint32]bool{}
	for _, e := range prev.Entries {
		told[e.Number] = true
	}
	joined := map[uint32]bool{}
	for i, e := range q.Entries {
		if told[e.Number] {
			continue
		}
		joined[e.Number] = true
		out = append(out, &agentreplv1.GithubMergeQueueActivity{Activity: &agentreplv1.GithubMergeQueueActivity_Joined{Joined: &agentreplv1.GithubMergeQueueActivityJoined{
			Entry: queueEntryProto(key, e), Position: positionProto(position{Ahead: uint32(i), Total: uint32(len(q.Entries))})}}})
	}
	switch {
	case len(q.Entries) == 0 && len(prev.Entries) > 0:
		out = append(out, &agentreplv1.GithubMergeQueueActivity{Activity: &agentreplv1.GithubMergeQueueActivity_Emptied{Emptied: &agentreplv1.GithubMergeQueueActivityEmptied{}}})
	case len(q.Entries) > 0 && !joined[q.Entries[0].Number] && (len(prev.Entries) == 0 || prev.Entries[0].Number != q.Entries[0].Number):
		// A joiner at the head is told by its joining, which says so.
		out = append(out, &agentreplv1.GithubMergeQueueActivity{Activity: &agentreplv1.GithubMergeQueueActivity_HeadChanged{HeadChanged: &agentreplv1.GithubMergeQueueActivityHeadChanged{
			Entry: queueEntryProto(key, q.Entries[0])}}})
	}
	return out
}

// AnchorQueue reads REPO's merge queue for BRANCH (empty is the default
// branch) as it stands now, and answers the subscription a workspace takes
// to it, told every entry already there, with the queue's key, its branch
// resolved. A branch with no queue is ErrNoMergeQueue; any other failure is
// the leaf's *ghclient.Failure. Both answer the key as far as it resolved.
func (w *Watcher) AnchorQueue(ctx context.Context, subscriber ids.WorkspaceID, owner, repo, branch string) (wsm.GithubMergeQueueSubscription, *agentreplv1.GithubMergeQueueStatus, wsm.GithubMergeQueueKey, error) {
	key := wsm.GithubMergeQueueKey{Owner: owner, Repo: repo, Branch: branch}
	q, err := readQueue(ctx, w.deps.GH, repository{owner, repo}, branch)
	if q.Branch != "" {
		key.Branch = q.Branch
	}
	if err != nil {
		return wsm.GithubMergeQueueSubscription{}, nil, key, err
	}
	sub := wsm.GithubMergeQueueSubscription{Subscriber: subscriber, Queue: key, SubscribedAt: w.deps.Clock.Now(), Told: encodeQueueTold(queueToldOf(q))}
	return sub, queueStatusProto(key, q), key, nil
}

// pollQueues reads each followed queue once, through QUEUES, and advances
// every subscription to it.
func (w *Watcher) pollQueues(ctx context.Context, queues *queueReads) {
	subs, err := w.deps.Store.LiveGithubMergeQueueSubscriptions(ctx)
	if err != nil && ctx.Err() != nil {
		w.deps.Log.Debug(opQueuePoll, "the queue poll stopped with its context", nil)
		return
	}
	if err != nil {
		w.deps.Log.Error(opQueuePoll, "could not read the merge-queue subscriptions; no queue is polled this tick", dlog.WithCause(nil, err))
		return
	}
	byQueue := map[wsm.GithubMergeQueueKey][]wsm.GithubMergeQueueSubscription{}
	var order []wsm.GithubMergeQueueKey
	for _, sub := range subs {
		if _, seen := byQueue[sub.Queue]; !seen {
			order = append(order, sub.Queue)
		}
		byQueue[sub.Queue] = append(byQueue[sub.Queue], sub)
	}
	for _, key := range order {
		if ctx.Err() != nil {
			return
		}
		w.pollQueue(ctx, key, byQueue[key], queues)
	}
}

// pollQueue reads KEY's queue and advances SUBS.
func (w *Watcher) pollQueue(ctx context.Context, key wsm.GithubMergeQueueKey, subs []wsm.GithubMergeQueueSubscription, queues *queueReads) {
	fields := dlog.Context{"merge_queue": key.String(), "subscribers": len(subs)}
	failing := make([]*wsm.PullRequestFailure, len(subs))
	for i, sub := range subs {
		failing[i] = sub.Failing
	}
	if resets, waiting := w.rateLimitWaitOf(failing); waiting {
		w.deps.Log.Debug(opQueuePoll, "the rate limit is spent; the queue is read once it resets", dlog.Merge(fields, dlog.Context{"resets_at": resets}))
		return
	}
	q, err := queues.get(ctx, key)
	if err != nil {
		if ctx.Err() == nil {
			w.recordQueueFailure(ctx, key, err, fields)
		}
		return
	}
	departed, err := w.departuresOf(ctx, key, subs, q)
	if err != nil {
		if ctx.Err() == nil {
			w.recordQueueFailure(ctx, key, err, fields)
		}
		return
	}
	now := w.deps.Clock.Now()
	for _, sub := range subs {
		w.advanceQueue(ctx, sub, q, departed, now)
	}
}

// departuresOf reads what became of every entry any of SUBS was told that Q
// no longer holds. One GitHub could not find left without merging, so it is
// answered absent; any other failure to read one is the read's failure, and
// nothing is told until the entry can be read.
func (w *Watcher) departuresOf(ctx context.Context, key wsm.GithubMergeQueueKey, subs []wsm.GithubMergeQueueSubscription, q remoteQueue) (map[uint32]remoteDeparture, error) {
	seen := map[uint32]bool{}
	var numbers []uint32
	for _, sub := range subs {
		prev, err := decodeQueueTold(sub.Told)
		if err != nil {
			// advanceQueue reports the unreadable snapshot for its own row.
			continue
		}
		for _, e := range departedFrom(prev, q) {
			if !seen[e.Number] {
				seen[e.Number] = true
				numbers = append(numbers, e.Number)
			}
		}
	}
	if len(numbers) == 0 {
		return map[uint32]remoteDeparture{}, nil
	}
	departed, failures, err := readDepartures(ctx, w.deps.GH, repository{key.Owner, key.Repo}, numbers)
	if err != nil {
		return nil, err
	}
	for n, ferr := range failures {
		var gh *ghclient.Failure
		if errors.As(ferr, &gh) && gh.Kind == ghclient.KindNotFound {
			w.deps.Log.Info(opQueuePoll, "an entry that left the queue no longer exists; it is told removed", dlog.Context{"merge_queue": key.String(), "number": n})
			continue
		}
		return nil, ferr
	}
	return departed, nil
}

// advanceQueue tells SUB how Q moved since it was last told.
func (w *Watcher) advanceQueue(ctx context.Context, sub wsm.GithubMergeQueueSubscription, q remoteQueue, departed map[uint32]remoteDeparture, now time.Time) {
	fields := dlog.Context{"merge_queue": sub.Queue.String(), "subscriber": string(sub.Subscriber)}
	prev, err := decodeQueueTold(sub.Told)
	if err != nil {
		// The snapshot is the daemon's own encoding; one it cannot read is a
		// defect, and the subscription cannot be advanced past it.
		w.deps.Log.Error(opQueuePoll, "the subscription's told snapshot is unreadable; it is not advanced", dlog.WithCause(fields, err))
		return
	}
	owed := queueSentencesOf(diffQueueEntries(sub.Queue, prev, q, departed))
	recovered := sub.Failing != nil
	to := encodeQueueTold(queueToldOf(q))
	if to == sub.Told && len(owed) == 0 && !recovered {
		return
	}
	applied, err := w.deps.Store.AdvanceGithubMergeQueueTold(ctx, wsm.GithubMergeQueueAdvance{
		Subscriber: sub.Subscriber, Queue: sub.Queue, From: sub.Told, To: to,
		Owed: owed, Status: queueStatusWords(queueStatusProto(sub.Queue, q)), ReadAt: now})
	fields["owed"] = len(owed)
	switch {
	case err != nil:
		w.deps.Log.Error(opQueuePoll, "could not advance the subscription; the next poll reads again", dlog.WithCause(fields, err))
	case !applied:
		w.deps.Log.Info(opQueuePoll, "the subscription moved since it was read; the next poll reads again", fields)
	case len(owed) > 0:
		w.deps.Log.Info(opQueuePoll, "the queue's movement is owed to its subscriber", fields)
	case recovered:
		w.deps.Log.Info(opQueuePoll, "the queue's reads work again and it did not move meanwhile; its subscriber is told nothing", fields)
	}
}

// recordQueueFailure records a failed read for every subscription to KEY,
// and owes the subscribers whose failing stretch it began one sentence
// saying so.
func (w *Watcher) recordQueueFailure(ctx context.Context, key wsm.GithubMergeQueueKey, err error, fields dlog.Context) {
	w.recordReadFailure(ctx, opQueuePoll, "merge queue", err, fields,
		func(f wsm.PullRequestFailure) ([]ids.WorkspaceID, error) {
			return w.deps.Store.RecordGithubMergeQueueReadFailure(ctx, key, f)
		},
		func(subscriber ids.WorkspaceID, sentences []string) error {
			return w.deps.Store.OweGithubMergeQueueNotice(ctx, subscriber, key, sentences)
		})
}

// githubMergeQueueNotice is O as the notice delivery submits it.
func githubMergeQueueNotice(store Store, o wsm.GithubMergeQueueOwed) owedNotice {
	return owedNotice{subscriber: o.Subscriber, turn: o.Turn, sentences: o.Sentences, pending: o.Pending, status: o.Status,
		label: "GitHub merge queue `" + o.Queue.String() + "`", noun: "merge-queue", subject: "GitHub merge queue", brief: BriefQueueNotice, origin: QueueOrigin,
		fields: dlog.Context{"subscriber": string(o.Subscriber), "merge_queue": o.Queue.String()},
		record: func(ctx context.Context, turn ids.TurnID, sentences []string, took int) error {
			return store.RecordGithubMergeQueueNoticeSubmitted(ctx, o.Subscriber, o.Queue, turn, sentences, took)
		}}
}

// ---- The contract's spellings ----

// QueueRefProto is a key as the contract spells it.
func QueueRefProto(key wsm.GithubMergeQueueKey) *agentreplv1.GithubMergeQueueRef {
	return &agentreplv1.GithubMergeQueueRef{Repository: &agentreplv1.GithubRepositoryRef{Owner: key.Owner, Repo: key.Repo}, Branch: key.Branch}
}

// QueueKeyOf is a contract ref as the store keys it.
func QueueKeyOf(ref *agentreplv1.GithubMergeQueueRef) wsm.GithubMergeQueueKey {
	return wsm.GithubMergeQueueKey{Owner: ref.GetRepository().GetOwner(), Repo: ref.GetRepository().GetRepo(), Branch: ref.GetBranch()}
}

// QueueSubscriptionProto renders a stored subscription as the contract
// spells it. Its subscriber names the workspace by id; the caller that holds
// the registry fills the directory.
func QueueSubscriptionProto(sub wsm.GithubMergeQueueSubscription) *agentreplv1.GithubMergeQueueSubscription {
	return &agentreplv1.GithubMergeQueueSubscription{Subscriber: &workspacev1.WorkspaceRef{Id: string(sub.Subscriber)}, MergeQueue: QueueRefProto(sub.Queue),
		Health: healthProto(sub.LastRead, sub.Failing)}
}

// queuePullRef is pull request NUMBER of KEY's repository.
func queuePullRef(key wsm.GithubMergeQueueKey, number uint32) *agentreplv1.PullRequestRef {
	return &agentreplv1.PullRequestRef{Owner: key.Owner, Repo: key.Repo, Number: number}
}

func queueEntryProto(key wsm.GithubMergeQueueKey, e remoteQueueEntry) *agentreplv1.GithubMergeQueueEntry {
	return &agentreplv1.GithubMergeQueueEntry{PullRequest: queuePullRef(key, e.Number), Title: e.Title, Url: e.URL, State: entryStateProto(e.State)}
}

// queueStatusProto is Q as the contract spells it.
func queueStatusProto(key wsm.GithubMergeQueueKey, q remoteQueue) *agentreplv1.GithubMergeQueueStatus {
	out := &agentreplv1.GithubMergeQueueStatus{Url: q.URL}
	for _, e := range q.Entries {
		out.Entries = append(out.Entries, queueEntryProto(key, e))
	}
	return out
}

// ---- The words a queue notice is written in ----

// queueSentencesOf renders ACTIVITY as a notice's sentences, in order.
func queueSentencesOf(activity []*agentreplv1.GithubMergeQueueActivity) []string {
	out := make([]string, 0, len(activity))
	for _, a := range activity {
		out = append(out, queueActivitySentence(a))
	}
	return out
}

func queueActivitySentence(a *agentreplv1.GithubMergeQueueActivity) string {
	switch arm := a.GetActivity().(type) {
	case *agentreplv1.GithubMergeQueueActivity_Joined:
		j := arm.Joined
		return fmt.Sprintf("Pull request %s joined: it is %s.", entryWords(j.GetEntry().GetPullRequest(), j.GetEntry().GetTitle()), positionWords(j.GetPosition()))
	case *agentreplv1.GithubMergeQueueActivity_Merged:
		m := arm.Merged
		return fmt.Sprintf("Pull request %s merged, as commit %s.", entryWords(m.GetPullRequest(), m.GetTitle()), short(m.GetMergeCommitSha()))
	case *agentreplv1.GithubMergeQueueActivity_Removed:
		r := arm.Removed
		s := fmt.Sprintf("Pull request %s left the queue without merging", entryWords(r.GetPullRequest(), r.GetTitle()))
		if r.Reason != nil {
			s += ": " + r.GetReason()
		}
		return s + "."
	case *agentreplv1.GithubMergeQueueActivity_HeadChanged:
		e := arm.HeadChanged.GetEntry()
		return fmt.Sprintf("Pull request %s now heads the queue and merges next.", entryWords(e.GetPullRequest(), e.GetTitle()))
	case *agentreplv1.GithubMergeQueueActivity_Emptied:
		return "The queue is now empty."
	}
	panic(fmt.Sprintf("prwatch: queue activity %T has no words", a.GetActivity()))
}

// entryWords names a pull request by its number and its title, quoted: the
// title is whoever opened it's words.
func entryWords(ref *agentreplv1.PullRequestRef, title string) string {
	return fmt.Sprintf("#%d %q", ref.GetNumber(), title)
}

// queueStatusWords states the queue as it stands, the line every queue
// notice ends with.
func queueStatusWords(s *agentreplv1.GithubMergeQueueStatus) string {
	if len(s.GetEntries()) == 0 {
		return "The queue now stands empty: " + s.GetUrl()
	}
	parts := make([]string, 0, len(s.GetEntries()))
	for _, e := range s.GetEntries() {
		parts = append(parts, fmt.Sprintf("%s (%s)", entryWords(e.GetPullRequest(), e.GetTitle()), entryStateWords(e.GetState())))
	}
	noun := "entries"
	if len(parts) == 1 {
		noun = "entry"
	}
	return fmt.Sprintf("The queue now holds %d %s, head first: %s: %s", len(parts), noun, strings.Join(parts, "; "), s.GetUrl())
}
