package mergenotice

import (
	"context"
	"errors"
	"fmt"
	"reflect"
	"strings"
	"sync"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/feedid"
	"agent-repld/internal/ids"
	"agent-repld/internal/merge"
	"agent-repld/internal/prompthandler"
	"agent-repld/internal/prompts"
	"agent-repld/internal/resolve/footer"
	"agent-repld/internal/wsm"
)

// The repositories and workspaces the tests name.
const (
	repoDoom  = wsm.RepoKey("/repos/doom/.git")
	repoOther = wsm.RepoKey("/repos/other/.git")
	alice     = ids.WorkspaceID("ws-alice")
	bob       = ids.WorkspaceID("ws-bob")
)

// fakeSubscriptions answers a fixed subscription set, or err.
type fakeSubscriptions struct {
	subs []wsm.MergeQueueSubscription
	err  error
}

func (f fakeSubscriptions) MergeQueueSubscriptions(context.Context) ([]wsm.MergeQueueSubscription, error) {
	return f.subs, f.err
}

// submission is one captured notice.
type submission struct {
	ws       ids.WorkspaceID
	text     string
	key      string
	origin   conversationv1.PromptOrigin
	delivery wsm.Delivery
	target   *feedid.Ref
}

// fakePrompts captures every submission; failFor fails the named
// subscribers' submissions; each submission is also sent on submitted, when
// set.
type fakePrompts struct {
	mu        sync.Mutex
	got       []submission
	failFor   map[ids.WorkspaceID]error
	submitted chan submission
}

var _ prompthandler.Submitter = (*fakePrompts)(nil)

func (p *fakePrompts) Submit(_ context.Context, ws ids.WorkspaceID, said *conversationv1.UserSaid, key string, origin conversationv1.PromptOrigin, delivery wsm.Delivery, target *feedid.Ref) (prompthandler.Outcome, error) {
	if err := p.failFor[ws]; err != nil {
		return prompthandler.Outcome{}, err
	}
	sub := submission{ws: ws, text: said.GetContent().GetBlocks()[0].GetText().GetText(), key: key, origin: origin, delivery: delivery, target: target}
	p.mu.Lock()
	p.got = append(p.got, sub)
	p.mu.Unlock()
	if p.submitted != nil {
		p.submitted <- sub
	}
	return prompthandler.Outcome{Turn: ids.TurnID("turn-" + string(ws))}, nil
}

// submissions answers every captured submission.
func (p *fakePrompts) submissions() []submission {
	p.mu.Lock()
	defer p.mu.Unlock()
	return append([]submission(nil), p.got...)
}

// fixture is one service over fakes.
type fixture struct {
	s       *Service
	prompts *fakePrompts
	logs    *dlog.TestSurfaces
	keys    int
}

// newFixture builds a service whose subscriptions are SUBS, whose briefs
// splice the real brief's placeholders into "label|change|queue|detail",
// and whose keys count up.
func newFixture(subs fakeSubscriptions) *fixture {
	f := &fixture{prompts: &fakePrompts{}, logs: dlog.NewTestSurfaces()}
	f.s = New(Deps{
		Subscriptions: subs,
		Briefs: func(name string, values map[string]string) (string, error) {
			if name != BriefNotice {
				return "", fmt.Errorf("unknown brief %q", name)
			}
			return values["label"] + "|" + values["change"] + "|" + values["queue"] + "|" + values["detail"], nil
		},
		MintKey: func(prefix string) (string, error) {
			f.keys++
			return fmt.Sprintf("%s-%d", prefix, f.keys), nil
		},
		Log: f.logs,
	})
	return f
}

// member is a merge in line.
func member(ws ids.WorkspaceID, name string, started bool, step footer.MergeStep) merge.QueueMember {
	return merge.QueueMember{Workspace: ws, Name: name, Branch: "b-" + name, Mode: merge.MergeModeOneShot, Started: started, Step: step}
}

// change is a change of KIND about SUBJECT in REPO, leaving MEMBERS in line.
func change(kind merge.QueueChangeKind, repo wsm.RepoKey, subject merge.QueueMember, members ...merge.QueueMember) merge.QueueChange {
	return merge.QueueChange{Kind: kind, Subject: subject, Queue: merge.QueueSnapshot{Repo: repo, Members: members},
		Depleted: kind.Leaves() && len(members) == 0}
}

// sub is WS's subscription to REPO at DETAIL.
func sub(ws ids.WorkspaceID, repo wsm.RepoKey, detail wsm.MergeQueueDetail) wsm.MergeQueueSubscription {
	return wsm.MergeQueueSubscription{Subscriber: ws, Repo: repo, Detail: detail}
}

// landedLast is the last merge of doom's queue landing.
var landedLast = change(merge.QueueChangeLanded, repoDoom, member("ws-fix", "fix", true, ""))

func TestAdmitsEachChangeAtEachDetail(t *testing.T) {
	other := member("ws-other", "other", false, "")
	tests := []struct {
		name   string
		change merge.QueueChange
		want   map[wsm.MergeQueueDetail]bool
	}{
		{name: "a merge put in line", change: change(merge.QueueChangeEnqueued, repoDoom, other, other),
			want: map[wsm.MergeQueueDetail]bool{wsm.MergeQueueDetailEveryChange: true}},
		{name: "a merge started", change: change(merge.QueueChangeStarted, repoDoom, other, other),
			want: map[wsm.MergeQueueDetail]bool{wsm.MergeQueueDetailEveryChange: true}},
		{name: "a merge's new step", change: change(merge.QueueChangeStepped, repoDoom, other, other),
			want: map[wsm.MergeQueueDetail]bool{wsm.MergeQueueDetailEveryChange: true}},
		{name: "a pause", change: change(merge.QueueChangePaused, repoDoom, merge.QueueMember{}, other),
			want: map[wsm.MergeQueueDetail]bool{wsm.MergeQueueDetailEveryChange: true}},
		{name: "a landing with merges left", change: change(merge.QueueChangeLanded, repoDoom, member("ws-fix", "fix", true, ""), other),
			want: map[wsm.MergeQueueDetail]bool{wsm.MergeQueueDetailEveryChange: true, wsm.MergeQueueDetailAdvances: true}},
		{name: "a failure with merges left", change: change(merge.QueueChangeFailed, repoDoom, member("ws-fix", "fix", true, ""), other),
			want: map[wsm.MergeQueueDetail]bool{wsm.MergeQueueDetailEveryChange: true, wsm.MergeQueueDetailAdvances: true}},
		{name: "a dequeue emptying the line", change: change(merge.QueueChangeDequeued, repoDoom, other),
			want: map[wsm.MergeQueueDetail]bool{wsm.MergeQueueDetailEveryChange: true, wsm.MergeQueueDetailAdvances: true, wsm.MergeQueueDetailDepletes: true}},
		{name: "a landing emptying the line", change: landedLast,
			want: map[wsm.MergeQueueDetail]bool{wsm.MergeQueueDetailEveryChange: true, wsm.MergeQueueDetailAdvances: true, wsm.MergeQueueDetailDepletes: true}},
	}
	for _, tt := range tests {
		for _, detail := range []wsm.MergeQueueDetail{wsm.MergeQueueDetailEveryChange, wsm.MergeQueueDetailAdvances, wsm.MergeQueueDetailDepletes} {
			t.Run(tt.name+" at "+detail.String(), func(t *testing.T) {
				// Act.
				got := Admits(detail, tt.change)

				// Assert.
				if got != tt.want[detail] {
					t.Fatalf("Admits(%s) = %v, want %v", detail, got, tt.want[detail])
				}
			})
		}
	}
}

func TestAdmitsFailsHardOnAnUndeclaredDetail(t *testing.T) {
	// Arrange.
	defer func() {
		// Assert.
		if recover() == nil {
			t.Fatal("Admits accepted an undeclared detail")
		}
	}()

	// Act.
	Admits(wsm.MergeQueueDetail(9), landedLast)
}

func TestRecipientsAreTheSubscribersOfTheChangesScope(t *testing.T) {
	// Arrange.
	subs := []wsm.MergeQueueSubscription{
		sub(alice, repoDoom, wsm.MergeQueueDetailAdvances),
		sub(bob, wsm.EveryRepository, wsm.MergeQueueDetailAdvances),
		sub("ws-carol", repoOther, wsm.MergeQueueDetailAdvances),
	}

	// Act.
	got := Recipients(subs, landedLast)

	// Assert.
	want := []Recipient{{Subscriber: alice, Detail: wsm.MergeQueueDetailAdvances}, {Subscriber: bob, Detail: wsm.MergeQueueDetailAdvances}}
	if !reflect.DeepEqual(got, want) {
		t.Fatalf("Recipients = %+v, want the doom and every-repository subscribers", got)
	}
}

func TestRecipientsLeaveOutASubscriptionWhoseDetailDoesNotAdmitTheChange(t *testing.T) {
	// Arrange.
	started := change(merge.QueueChangeStarted, repoDoom, member("ws-fix", "fix", true, ""), member("ws-fix", "fix", true, ""))
	subs := []wsm.MergeQueueSubscription{sub(alice, repoDoom, wsm.MergeQueueDetailDepletes), sub(bob, repoDoom, wsm.MergeQueueDetailEveryChange)}

	// Act.
	got := Recipients(subs, started)

	// Assert.
	if want := []Recipient{{Subscriber: bob, Detail: wsm.MergeQueueDetailEveryChange}}; !reflect.DeepEqual(got, want) {
		t.Fatalf("Recipients = %+v, want only the every-change subscriber", got)
	}
}

func TestASubscriberHoldingTwoAdmittingSubscriptionsIsToldOnceAtTheFinerDetail(t *testing.T) {
	// Arrange.
	subs := []wsm.MergeQueueSubscription{sub(alice, wsm.EveryRepository, wsm.MergeQueueDetailDepletes), sub(alice, repoDoom, wsm.MergeQueueDetailAdvances)}

	// Act.
	got := Recipients(subs, landedLast)

	// Assert.
	if want := []Recipient{{Subscriber: alice, Detail: wsm.MergeQueueDetailAdvances}}; !reflect.DeepEqual(got, want) {
		t.Fatalf("Recipients = %+v, want alice once, at advances", got)
	}
}

func TestDeliverSubmitsOneDeferredHiddenNoticePerSubscriber(t *testing.T) {
	// Arrange.
	f := newFixture(fakeSubscriptions{subs: []wsm.MergeQueueSubscription{
		sub(alice, repoDoom, wsm.MergeQueueDetailAdvances), sub(bob, wsm.EveryRepository, wsm.MergeQueueDetailDepletes),
	}})

	// Act.
	f.s.deliver(context.Background(), f.prompts, landedLast)

	// Assert.
	got := f.prompts.submissions()
	if len(got) != 2 || got[0].ws != alice || got[1].ws != bob {
		t.Fatalf("submissions = %+v, want alice then bob", got)
	}
	for _, s := range got {
		if s.origin != Origin || s.delivery != wsm.DeliveryDeferred || s.target != nil {
			t.Fatalf("submission = %+v, want a deferred %v to the root feed", s, Origin)
		}
	}
	if got[0].key == got[1].key {
		t.Fatalf("keys = %q and %q, want one per notice", got[0].key, got[1].key)
	}
	if !strings.HasPrefix(got[0].text, "merge queue of doom: fix landed; the queue is empty|") {
		t.Fatalf("text = %q, want the headline first", got[0].text)
	}
}

func TestDeliverRecordsEachNoticeTold(t *testing.T) {
	// Arrange.
	f := newFixture(fakeSubscriptions{subs: []wsm.MergeQueueSubscription{sub(alice, repoDoom, wsm.MergeQueueDetailDepletes)}})

	// Act.
	f.s.deliver(context.Background(), f.prompts, landedLast)

	// Assert.
	record, ok := dlog.FindRecord(f.logs.Records(), dlog.RecordQuery{Level: "info", Operation: op, Message: "told a subscriber's agent of a merge queue change"})
	if !ok {
		t.Fatalf("no record of the notice: %+v", f.logs.Records())
	}
	want := dlog.Context{"subscriber": string(alice), "detail": "depletes", "change": "landed", "repo": string(repoDoom), "turn": "turn-ws-alice", "depleted": true}
	for k, v := range want {
		if record.Context[k] != v {
			t.Fatalf("record context %s = %v, want %v (%+v)", k, record.Context[k], v, record.Context)
		}
	}
}

func TestDeliverTellsNobodyWithoutAnAdmittingSubscription(t *testing.T) {
	// Arrange.
	f := newFixture(fakeSubscriptions{subs: []wsm.MergeQueueSubscription{sub(alice, repoOther, wsm.MergeQueueDetailEveryChange)}})

	// Act.
	f.s.deliver(context.Background(), f.prompts, landedLast)

	// Assert.
	if got := f.prompts.submissions(); len(got) != 0 {
		t.Fatalf("submissions = %+v, want none", got)
	}
}

func TestDeliverRecordsAnUnreadableSubscriptionSetAndTellsNobody(t *testing.T) {
	// Arrange.
	f := newFixture(fakeSubscriptions{err: errors.New("the store is gone")})

	// Act.
	f.s.deliver(context.Background(), f.prompts, landedLast)

	// Assert.
	if got := f.prompts.submissions(); len(got) != 0 {
		t.Fatalf("submissions = %+v, want none", got)
	}
	record, ok := dlog.FindRecord(f.logs.Records(), dlog.RecordQuery{Level: "error", Operation: op, Message: "could not read the merge queue subscriptions; nobody is told of the change"})
	if !ok || record.Context["cause"] != "the store is gone" || record.Context["change"] != "landed" {
		t.Fatalf("no error record naming the change and its cause: %+v", f.logs.Records())
	}
}

func TestAFailedSubmissionIsRecordedAndTheOtherSubscribersAreStillTold(t *testing.T) {
	// Arrange.
	f := newFixture(fakeSubscriptions{subs: []wsm.MergeQueueSubscription{
		sub(alice, repoDoom, wsm.MergeQueueDetailAdvances), sub(bob, repoDoom, wsm.MergeQueueDetailAdvances),
	}})
	f.prompts.failFor = map[ids.WorkspaceID]error{alice: errors.New("the session is parked at its cold gate")}

	// Act.
	f.s.deliver(context.Background(), f.prompts, landedLast)

	// Assert.
	if got := f.prompts.submissions(); len(got) != 1 || got[0].ws != bob {
		t.Fatalf("submissions = %+v, want bob told", got)
	}
	record, ok := dlog.FindRecord(f.logs.Records(), dlog.RecordQuery{Level: "error", Operation: op, Message: "could not submit a merge queue notice to its subscriber"})
	if !ok || record.Context["subscriber"] != string(alice) || record.Context["cause"] != "the session is parked at its cold gate" {
		t.Fatalf("no error record naming alice and the cause: %+v", f.logs.Records())
	}
}

func TestASubmissionCutShortByTheDaemonStandingDownIsRecordedAsSuch(t *testing.T) {
	// Arrange.
	f := newFixture(fakeSubscriptions{subs: []wsm.MergeQueueSubscription{sub(alice, repoDoom, wsm.MergeQueueDetailAdvances)}})
	f.prompts.failFor = map[ids.WorkspaceID]error{alice: context.Canceled}
	ctx, cancel := context.WithCancel(context.Background())
	cancel()

	// Act.
	f.s.deliver(ctx, f.prompts, landedLast)

	// Assert.
	if _, ok := dlog.FindRecord(f.logs.Records(), dlog.RecordQuery{Level: "error", Operation: op,
		Message: "a merge queue change was never told to its subscriber: the daemon stood down mid-delivery"}); !ok {
		t.Fatalf("no record of the cut-short delivery: %+v", f.logs.Records())
	}
}

func TestAnUncomposableNoticeIsRecordedAndNotSubmitted(t *testing.T) {
	// Arrange.
	f := newFixture(fakeSubscriptions{subs: []wsm.MergeQueueSubscription{sub(alice, repoDoom, wsm.MergeQueueDetailAdvances)}})
	f.s.deps.Briefs = func(string, map[string]string) (string, error) { return "", errors.New("the brief is missing") }

	// Act.
	f.s.deliver(context.Background(), f.prompts, landedLast)

	// Assert.
	if got := f.prompts.submissions(); len(got) != 0 {
		t.Fatalf("submissions = %+v, want none", got)
	}
	record, ok := dlog.FindRecord(f.logs.Records(), dlog.RecordQuery{Level: "error", Operation: op, Message: "could not compose a merge queue notice; the subscriber is not told of the change"})
	if !ok || record.Context["cause"] != "the brief is missing" || record.Context["subscriber"] != string(alice) {
		t.Fatalf("no error record of the composition: %+v", f.logs.Records())
	}
}

func TestAnUnmintableKeyIsRecordedAndNotSubmitted(t *testing.T) {
	// Arrange.
	f := newFixture(fakeSubscriptions{subs: []wsm.MergeQueueSubscription{sub(alice, repoDoom, wsm.MergeQueueDetailAdvances)}})
	f.s.deps.MintKey = func(string) (string, error) { return "", errors.New("no entropy") }

	// Act.
	f.s.deliver(context.Background(), f.prompts, landedLast)

	// Assert.
	if got := f.prompts.submissions(); len(got) != 0 {
		t.Fatalf("submissions = %+v, want none", got)
	}
	if _, ok := dlog.FindRecord(f.logs.Records(), dlog.RecordQuery{Level: "error", Operation: op, Message: "could not mint a merge queue notice's idempotency key; the subscriber is not told of the change"}); !ok {
		t.Fatalf("no error record of the key: %+v", f.logs.Records())
	}
}

func TestRunDeliversEveryReportedChangeInOrder(t *testing.T) {
	// Arrange.
	f := newFixture(fakeSubscriptions{subs: []wsm.MergeQueueSubscription{sub(alice, repoDoom, wsm.MergeQueueDetailEveryChange)}})
	f.prompts.submitted = make(chan submission)
	fix := member("ws-fix", "fix", false, "")
	changes := []merge.QueueChange{
		change(merge.QueueChangeEnqueued, repoDoom, fix, fix),
		change(merge.QueueChangeStarted, repoDoom, fix, fix),
		landedLast,
	}
	ctx, cancel := context.WithCancel(context.Background())
	done := make(chan struct{})
	go func() { f.s.Run(ctx, f.prompts); close(done) }()

	// Act.
	var headlines []string
	for _, c := range changes {
		f.s.QueueChanged(c)
	}
	for range changes {
		got := <-f.prompts.submitted
		headlines = append(headlines, strings.SplitN(got.text, "|", 2)[0])
	}
	cancel()
	<-done

	// Assert.
	want := []string{"merge queue of doom: fix put in line", "merge queue of doom: fix started", "merge queue of doom: fix landed; the queue is empty"}
	if !reflect.DeepEqual(headlines, want) {
		t.Fatalf("headlines = %q, want %q", headlines, want)
	}
}

func TestAChangeReportedAfterRunEndedIsRecordedUndelivered(t *testing.T) {
	// Arrange.
	f := newFixture(fakeSubscriptions{})
	ctx, cancel := context.WithCancel(context.Background())
	cancel()
	f.s.Run(ctx, f.prompts)

	// Act.
	f.s.QueueChanged(landedLast)

	// Assert.
	record, ok := dlog.FindRecord(f.logs.Records(), dlog.RecordQuery{Level: "error", Operation: op,
		Message: "a merge queue change was never told to its subscribers: the notifier had already stopped"})
	if !ok || record.Context["change"] != "landed" {
		t.Fatalf("no record of the undelivered change: %+v", f.logs.Records())
	}
}

func TestAChangePendingWhenRunEndsIsRecordedUndelivered(t *testing.T) {
	// Arrange.
	f := newFixture(fakeSubscriptions{})
	f.s.QueueChanged(landedLast)
	ctx, cancel := context.WithCancel(context.Background())
	cancel()

	// Act.
	f.s.Run(ctx, f.prompts)

	// Assert.
	if _, ok := dlog.FindRecord(f.logs.Records(), dlog.RecordQuery{Level: "error", Operation: op,
		Message: "a merge queue change was never told to its subscribers: the daemon stood down before delivering it"}); !ok {
		t.Fatalf("no record of the undelivered change: %+v", f.logs.Records())
	}
	if got := f.prompts.submissions(); len(got) != 0 {
		t.Fatalf("submissions = %+v, want none after the stand-down", got)
	}
}

func TestHeadlineNamesTheQueueAndWhatHappened(t *testing.T) {
	fix := member("ws-fix", "fix", true, footer.StepTesting)
	tests := []struct {
		change merge.QueueChange
		want   string
	}{
		{change(merge.QueueChangeEnqueued, repoDoom, fix, fix), "merge queue of doom: fix put in line"},
		{change(merge.QueueChangeRequeued, repoDoom, fix, fix), "merge queue of doom: fix put back in line"},
		{change(merge.QueueChangeStarted, repoDoom, fix, fix), "merge queue of doom: fix started"},
		{change(merge.QueueChangeStepped, repoDoom, fix, fix), "merge queue of doom: fix testing"},
		{change(merge.QueueChangeLanded, repoDoom, fix), "merge queue of doom: fix landed; the queue is empty"},
		{change(merge.QueueChangeFailed, repoDoom, fix, fix), "merge queue of doom: fix failed"},
		{change(merge.QueueChangeDequeued, repoDoom, fix, fix), "merge queue of doom: fix taken off the queue"},
		{change(merge.QueueChangeReordered, repoDoom, fix, fix), "merge queue of doom: fix moved in line"},
		{change(merge.QueueChangePaused, repoDoom, merge.QueueMember{}, fix), "merge queue of doom: paused"},
		{change(merge.QueueChangeResumed, repoDoom, merge.QueueMember{}, fix), "merge queue of doom: resumed"},
	}
	for _, tt := range tests {
		t.Run(tt.want, func(t *testing.T) {
			// Act.
			got := Headline(tt.change)

			// Assert.
			if got != tt.want {
				t.Fatalf("Headline = %q, want %q", got, tt.want)
			}
		})
	}
}

func TestSentenceStatesEachChangeInFull(t *testing.T) {
	fix := member("ws-fix", "fix", true, footer.StepConflictResolution)
	dequeued := change(merge.QueueChangeDequeued, repoDoom, fix)
	dequeued.Cause = "workspace_closed"
	tests := []struct {
		name   string
		change merge.QueueChange
		want   string
	}{
		{"enqueued", change(merge.QueueChangeEnqueued, repoDoom, fix, fix), "The one-shot merge of `fix` (branch `b-fix`) was put in line."},
		{"requeued", change(merge.QueueChangeRequeued, repoDoom, fix, fix), "The one-shot merge of `fix` (branch `b-fix`), which a daemon restart cut short, was put back in line to run again."},
		{"started", change(merge.QueueChangeStarted, repoDoom, fix, fix), "The one-shot merge of `fix` (branch `b-fix`) started."},
		{"stepped", change(merge.QueueChangeStepped, repoDoom, fix, fix), "The one-shot merge of `fix` (branch `b-fix`) moved to its conflict resolution step."},
		{"landed", change(merge.QueueChangeLanded, repoDoom, fix), "The one-shot merge of `fix` (branch `b-fix`) landed and left the queue."},
		{"failed", change(merge.QueueChangeFailed, repoDoom, fix), "The one-shot merge of `fix` (branch `b-fix`) failed and left the queue."},
		{"dequeued", dequeued, "The one-shot merge of `fix` (branch `b-fix`) was taken off the queue (workspace closed)."},
		{"reordered", change(merge.QueueChangeReordered, repoDoom, fix, fix), "The one-shot merge of `fix` (branch `b-fix`) moved to a new place in line."},
		{"paused", change(merge.QueueChangePaused, repoDoom, merge.QueueMember{}), "The queue was paused: nothing new starts until it is resumed."},
		{"resumed", change(merge.QueueChangeResumed, repoDoom, merge.QueueMember{}), "The queue was resumed."},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act.
			got := Sentence(tt.change)

			// Assert.
			if got != tt.want {
				t.Fatalf("Sentence = %q, want %q", got, tt.want)
			}
		})
	}
}

func TestQueueListsEveryMemberWithItsStanding(t *testing.T) {
	// Arrange.
	queue := merge.QueueSnapshot{Repo: repoDoom, Paused: true, Members: []merge.QueueMember{
		member("ws-fix", "fix", true, footer.StepTesting),
		member("ws-new", "new", true, ""),
		{Workspace: "ws-side", Name: "side", Branch: "side-branch", Mode: merge.MergeModeAsync},
	}}

	// Act.
	got := Queue(queue)

	// Assert.
	want := "The queue of `/repos/doom` now (paused), front first:\n" +
		"1. `fix`: branch `b-fix`, one-shot, started, testing\n" +
		"2. `new`: branch `b-new`, one-shot, started\n" +
		"3. `side`: branch `side-branch`, async, waiting"
	if got != want {
		t.Fatalf("Queue =\n%s\nwant\n%s", got, want)
	}
}

func TestQueueStatesAnEmptyQueue(t *testing.T) {
	// Act.
	got := Queue(merge.QueueSnapshot{Repo: repoDoom})

	// Assert.
	if want := "The queue of `/repos/doom` is now empty (running)."; got != want {
		t.Fatalf("Queue = %q, want %q", got, want)
	}
}

func TestTheValuesSpliceIntoTheShippedBrief(t *testing.T) {
	// Arrange: the brief as it ships, beside the daemon.
	brief, err := prompts.Load("../../../prompts", BriefNotice)
	if err != nil {
		t.Fatalf("load the shipped brief: %v", err)
	}

	// Act.
	text, err := brief.Splice(Values(wsm.MergeQueueDetailDepletes, landedLast))

	// Assert.
	if err != nil {
		t.Fatalf("Splice: %v", err)
	}
	if first, _, _ := strings.Cut(text, "\n"); first != Headline(landedLast) {
		t.Fatalf("first line = %q, want the headline", first)
	}
	if !strings.Contains(text, "you asked to be told of the queue emptying") {
		t.Fatalf("text = %q, want the detail named", text)
	}
}
