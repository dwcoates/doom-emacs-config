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
	"agent-repld/internal/resolve/sidebar"
	"agent-repld/internal/wsm"
)

// The repositories and workspaces the tests name.
const (
	repoDoom  = wsm.RepoKey("/repos/doom/.git")
	repoOther = wsm.RepoKey("/repos/other/.git")
	alice     = ids.WorkspaceID("ws-alice")
	bob       = ids.WorkspaceID("ws-bob")
)

// fakeSubscriptions answers a fixed subscription set, or err. It records
// every subscription it is asked to end; endErr fails those ends, and
// takenAgain answers each end as a subscription taken again since the read.
type fakeSubscriptions struct {
	subs       []wsm.MergeQueueSubscription
	err        error
	endErr     error
	takenAgain bool
	ended      []wsm.MergeQueueSubscription
}

func (f *fakeSubscriptions) MergeQueueSubscriptions(context.Context) ([]wsm.MergeQueueSubscription, error) {
	return f.subs, f.err
}

func (f *fakeSubscriptions) EndMergeQueueSubscription(_ context.Context, sub wsm.MergeQueueSubscription) (bool, error) {
	if f.endErr != nil {
		return false, f.endErr
	}
	if f.takenAgain {
		return false, nil
	}
	f.ended = append(f.ended, sub)
	return true, nil
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

func (p *fakePrompts) Submit(_ context.Context, ws ids.WorkspaceID, said *conversationv1.UserSaid, key string, origin conversationv1.PromptOrigin, _ *ids.WorkspaceID, delivery wsm.Delivery, target *feedid.Ref) (prompthandler.Outcome, error) {
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
	p.mu.Lock()
	n := 0
	for _, s := range p.got {
		if s.ws == ws {
			n++
		}
	}
	p.mu.Unlock()
	turn := "turn-" + string(ws)
	if n > 1 {
		turn = fmt.Sprintf("%s-%d", turn, n)
	}
	return prompthandler.Outcome{Turn: ids.TurnID(turn)}, nil
}

// submissions answers every captured submission.
func (p *fakePrompts) submissions() []submission {
	p.mu.Lock()
	defer p.mu.Unlock()
	return append([]submission(nil), p.got...)
}

// fakeHolds answers each Supersede with retired (or err) and records the
// turns it was asked to retire.
type fakeHolds struct {
	retired bool
	err     error
	asked   []ids.TurnID
}

func (h *fakeHolds) Supersede(_ context.Context, _ ids.WorkspaceID, turn ids.TurnID) (bool, error) {
	h.asked = append(h.asked, turn)
	return h.retired, h.err
}

// fakeUntold is the durable untold-notice store in memory; readErr and
// recordErr fail its reads and records.
type fakeUntold struct {
	rows      map[[2]string]wsm.UntoldMergeNotice
	readErr   error
	recordErr error
}

func (u *fakeUntold) UntoldMergeNotice(_ context.Context, subscriber ids.WorkspaceID, repo wsm.RepoKey) (wsm.UntoldMergeNotice, bool, error) {
	if u.readErr != nil {
		return wsm.UntoldMergeNotice{}, false, u.readErr
	}
	n, ok := u.rows[[2]string{string(subscriber), string(repo)}]
	return n, ok, nil
}

func (u *fakeUntold) RecordUntoldMergeNotice(_ context.Context, n wsm.UntoldMergeNotice) error {
	if u.recordErr != nil {
		return u.recordErr
	}
	u.rows[[2]string{string(n.Subscriber), string(n.Repo)}] = n
	return nil
}

// fixture is one service over fakes.
type fixture struct {
	s       *Service
	subs    *fakeSubscriptions
	prompts *fakePrompts
	holds   *fakeHolds
	untold  *fakeUntold
	logs    *dlog.TestSurfaces
	keys    int
}

// newFixture builds a service whose subscriptions are SUBS, whose briefs
// splice the real brief's placeholders into "scope|changes|queue|lifetime",
// and whose keys count up.
func newFixture(subs fakeSubscriptions) *fixture {
	f := &fixture{subs: &subs, prompts: &fakePrompts{}, holds: &fakeHolds{retired: true}, untold: &fakeUntold{rows: map[[2]string]wsm.UntoldMergeNotice{}}, logs: dlog.NewTestSurfaces()}
	f.s = New(Deps{
		Subscriptions: f.subs,
		Holds:         f.holds,
		Untold:        f.untold,
		Briefs: func(name string, values map[string]string) (string, error) {
			if name != BriefNotice {
				return "", fmt.Errorf("unknown brief %q", name)
			}
			return values["scope"] + "|" + values["changes"] + "|" + values["queue"] + "|" + values["lifetime"], nil
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

// sub is WS's subscription to REPO.
func sub(ws ids.WorkspaceID, repo wsm.RepoKey) wsm.MergeQueueSubscription {
	return wsm.MergeQueueSubscription{Subscriber: ws, Repo: repo}
}

// landedLast is the last merge of doom's queue landing.
var landedLast = change(merge.QueueChangeLanded, repoDoom, member("ws-fix", "fix", true, ""))

// queuedFix is fix's merge put in line, alone in it.
var queuedFix = change(merge.QueueChangeEnqueued, repoDoom, member("ws-fix", "fix", false, ""), member("ws-fix", "fix", false, ""))

// steppedFix is fix's merge moving to its testing step, a change never told.
var steppedFix = change(merge.QueueChangeStepped, repoDoom, member("ws-fix", "fix", true, footer.StepTesting),
	member("ws-fix", "fix", true, footer.StepTesting))

// fieldsOf splits a fixture notice into its scope, changes, queue and
// lifetime.
func fieldsOf(t *testing.T, text string) []string {
	t.Helper()
	parts := strings.Split(text, "|")
	if len(parts) != 4 {
		t.Fatalf("notice %q does not split into the brief's four placeholders", text)
	}
	return parts
}

func TestAdmitsOnlyTheChangesASubscriberWaitsOn(t *testing.T) {
	other := member("ws-other", "other", false, "")
	fix := member("ws-fix", "fix", true, "")
	tests := []struct {
		name   string
		change merge.QueueChange
		want   bool
	}{
		{name: "a merge put in line", change: change(merge.QueueChangeEnqueued, repoDoom, other, other), want: true},
		{name: "a merge landing with merges left", change: change(merge.QueueChangeLanded, repoDoom, fix, other), want: true},
		{name: "a merge landing that empties the line", change: landedLast, want: true},
		{name: "a merge failing", change: change(merge.QueueChangeFailed, repoDoom, fix, other), want: true},
		{name: "a merge taken off the queue", change: change(merge.QueueChangeDequeued, repoDoom, other), want: true},
		{name: "a deploy's end", change: deployed(merge.DeployEnd{Carried: []string{"fix-land"}}), want: true},
		{name: "a merge a restart put back in line", change: change(merge.QueueChangeRequeued, repoDoom, other, other), want: false},
		{name: "a merge starting", change: change(merge.QueueChangeStarted, repoDoom, fix, fix), want: false},
		{name: "a merge's new step", change: steppedFix, want: false},
		{name: "a reorder", change: change(merge.QueueChangeReordered, repoDoom, other, other), want: false},
		{name: "a pause", change: change(merge.QueueChangePaused, repoDoom, merge.QueueMember{}, other), want: false},
		{name: "a resume", change: change(merge.QueueChangeResumed, repoDoom, merge.QueueMember{}, other), want: false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act.
			got := Admits(tt.change)

			// Assert.
			if got != tt.want {
				t.Fatalf("Admits = %v, want %v", got, tt.want)
			}
		})
	}
}

func TestAdmitsFailsHardOnAnUndeclaredKind(t *testing.T) {
	// Arrange.
	defer func() {
		// Assert.
		if recover() == nil {
			t.Fatal("Admits judged an undeclared kind")
		}
	}()

	// Act.
	Admits(merge.QueueChange{Kind: merge.QueueChangeKind(99)})
}

func TestRecipientsAreTheSubscribersOfTheChangesScope(t *testing.T) {
	// Arrange.
	subs := []wsm.MergeQueueSubscription{sub(alice, repoDoom), sub(bob, wsm.EveryRepository), sub("ws-carol", repoOther)}

	// Act.
	got := Recipients(subs, landedLast)

	// Assert.
	if want := []ids.WorkspaceID{alice, bob}; !reflect.DeepEqual(got, want) {
		t.Fatalf("Recipients = %v, want the doom and every-repository subscribers", got)
	}
}

func TestRecipientsAreNobodyForAChangeNeverTold(t *testing.T) {
	// Arrange.
	subs := []wsm.MergeQueueSubscription{sub(alice, repoDoom), sub(bob, wsm.EveryRepository)}

	// Act.
	got := Recipients(subs, steppedFix)

	// Assert.
	if len(got) != 0 {
		t.Fatalf("Recipients = %v, want nobody told of a step", got)
	}
}

func TestASubscriberHoldingTwoSubscriptionsInScopeIsToldOnce(t *testing.T) {
	// Arrange.
	subs := []wsm.MergeQueueSubscription{sub(alice, wsm.EveryRepository), sub(alice, repoDoom)}

	// Act.
	got := Recipients(subs, landedLast)

	// Assert.
	if want := []ids.WorkspaceID{alice}; !reflect.DeepEqual(got, want) {
		t.Fatalf("Recipients = %v, want alice once", got)
	}
}

func TestDeliverSubmitsOneDeferredHiddenNoticePerSubscriber(t *testing.T) {
	// Arrange.
	f := newFixture(fakeSubscriptions{subs: []wsm.MergeQueueSubscription{sub(alice, repoDoom), sub(bob, wsm.EveryRepository)}})

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
	if want := "merge queue of doom|`fix` landed.| Queue empty.| Subscription ended."; got[0].text != want {
		t.Fatalf("text = %q, want %q", got[0].text, want)
	}
}

func TestDeliverRecordsEachNoticeTold(t *testing.T) {
	// Arrange.
	f := newFixture(fakeSubscriptions{subs: []wsm.MergeQueueSubscription{sub(alice, repoDoom)}})

	// Act.
	f.s.deliver(context.Background(), f.prompts, landedLast)

	// Assert.
	record, ok := dlog.FindRecord(f.logs.Records(), dlog.RecordQuery{Level: "info", Operation: op, Message: "told a subscriber's agent of a merge queue change"})
	if !ok {
		t.Fatalf("no record of the notice: %+v", f.logs.Records())
	}
	want := dlog.Context{"subscriber": string(alice), "change": "landed", "repo": string(repoDoom), "turn": "turn-ws-alice", "depleted": true}
	for k, v := range want {
		if record.Context[k] != v {
			t.Fatalf("record context %s = %v, want %v (%+v)", k, record.Context[k], v, record.Context)
		}
	}
}

func TestDeliverTellsNobodyOutsideTheChangesScope(t *testing.T) {
	// Arrange.
	f := newFixture(fakeSubscriptions{subs: []wsm.MergeQueueSubscription{sub(alice, repoOther)}})

	// Act.
	f.s.deliver(context.Background(), f.prompts, landedLast)

	// Assert.
	if got := f.prompts.submissions(); len(got) != 0 {
		t.Fatalf("submissions = %+v, want none", got)
	}
}

func TestDeliverDropsAChangeNeverToldWithoutReadingTheSubscriptions(t *testing.T) {
	// Arrange: a subscription read would fail, so a change that read them
	// would leave an error record.
	f := newFixture(fakeSubscriptions{subs: []wsm.MergeQueueSubscription{sub(alice, repoDoom)}, err: errors.New("the store is gone")})

	// Act.
	f.s.deliver(context.Background(), f.prompts, steppedFix)

	// Assert.
	if got := f.prompts.submissions(); len(got) != 0 {
		t.Fatalf("submissions = %+v, want none", got)
	}
	if _, ok := dlog.FindRecord(f.logs.Records(), dlog.RecordQuery{Level: "error"}); ok {
		t.Fatalf("a dropped change read the subscriptions: %+v", f.logs.Records())
	}
	record, ok := dlog.FindRecord(f.logs.Records(), dlog.RecordQuery{Level: "debug", Operation: op, Message: "a merge queue change no subscription is told; nobody is notified"})
	if !ok || record.Context["change"] != "stepped" {
		t.Fatalf("no debug record of the dropped change: %+v", f.logs.Records())
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
	f := newFixture(fakeSubscriptions{subs: []wsm.MergeQueueSubscription{sub(alice, repoDoom), sub(bob, repoDoom)}})
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

func TestAnUncomposableNoticeIsRecordedAndNotSubmitted(t *testing.T) {
	// Arrange.
	f := newFixture(fakeSubscriptions{subs: []wsm.MergeQueueSubscription{sub(alice, repoDoom)}})
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
	f := newFixture(fakeSubscriptions{subs: []wsm.MergeQueueSubscription{sub(alice, repoDoom)}})
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

func TestRunDeliversEveryToldChangeInOrder(t *testing.T) {
	// Arrange.
	f := newFixture(fakeSubscriptions{subs: []wsm.MergeQueueSubscription{sub(alice, repoDoom)}})
	f.holds.retired = false
	f.prompts.submitted = make(chan submission)
	fix := member("ws-fix", "fix", false, "")
	ctx, cancel := context.WithCancel(context.Background())
	done := make(chan struct{})
	go func() { f.s.Run(ctx, f.prompts); close(done) }()

	// Act: the start between the two is never told.
	f.s.QueueChanged(change(merge.QueueChangeEnqueued, repoDoom, fix, fix))
	f.s.QueueChanged(change(merge.QueueChangeStarted, repoDoom, fix, fix))
	f.s.QueueChanged(landedLast)
	var changes []string
	for range 2 {
		got := <-f.prompts.submitted
		changes = append(changes, fieldsOf(t, got.text)[1])
	}
	cancel()
	<-done

	// Assert.
	want := []string{"`fix` put in line.", "`fix` landed."}
	if !reflect.DeepEqual(changes, want) {
		t.Fatalf("changes = %q, want %q", changes, want)
	}
	if got := f.prompts.submissions(); len(got) != 2 {
		t.Fatalf("submissions = %+v, want the start left untold", got)
	}
}

func TestAChangeReportedAfterRunEndedIsRecordedUntold(t *testing.T) {
	// Arrange.
	f := newFixture(fakeSubscriptions{subs: []wsm.MergeQueueSubscription{sub(alice, repoDoom)}})
	ctx, cancel := context.WithCancel(context.Background())
	cancel()
	f.s.Run(ctx, f.prompts)

	// Act.
	f.s.QueueChanged(landedLast)

	// Assert.
	if got := f.prompts.submissions(); len(got) != 0 {
		t.Fatalf("submissions = %+v, want none after the notifier stopped", got)
	}
	record, ok := dlog.FindRecord(f.logs.Records(), dlog.RecordQuery{Level: "info", Operation: op,
		Message: "a merge queue change came after the notifier stopped, as the daemon stood down; it is not told, and the next notice carries its effect"})
	if !ok || record.Context["change"] != "landed" {
		t.Fatalf("no record of the untold change: %+v", f.logs.Records())
	}
}

func TestAChangePendingWhenRunIsEndedIsStillDelivered(t *testing.T) {
	// Arrange: a landing reported, and the daemon ending the notifier before
	// its loop took it.
	f := newFixture(fakeSubscriptions{subs: []wsm.MergeQueueSubscription{sub(alice, repoDoom)}})
	f.s.QueueChanged(landedLast)
	ctx, cancel := context.WithCancel(context.Background())
	cancel()

	// Act.
	f.s.Run(ctx, f.prompts)

	// Assert.
	if got := f.prompts.submissions(); len(got) != 1 || got[0].ws != alice {
		t.Fatalf("submissions = %+v, want the pending landing told to alice", got)
	}
}

func TestSentenceStatesEachToldChangeInAFewWords(t *testing.T) {
	fix := member("ws-fix", "fix", true, footer.StepConflictResolution)
	dequeued := change(merge.QueueChangeDequeued, repoDoom, fix)
	dequeued.Cause = "workspace_closed"
	tests := []struct {
		name   string
		change merge.QueueChange
		want   string
	}{
		{"enqueued", change(merge.QueueChangeEnqueued, repoDoom, fix, fix), "`fix` put in line"},
		{"landed", change(merge.QueueChangeLanded, repoDoom, fix), "`fix` landed"},
		{"failed", change(merge.QueueChangeFailed, repoDoom, fix), "`fix` failed"},
		{"dequeued", dequeued, "`fix` taken off (workspace closed)"},
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

func TestSentenceFailsHardOnAChangeNeverTold(t *testing.T) {
	// Arrange.
	defer func() {
		// Assert.
		if recover() == nil {
			t.Fatal("Sentence stated a change that is never told")
		}
	}()

	// Act.
	Sentence(steppedFix)
}

func TestChangesStatesOneChangeAsItsSentence(t *testing.T) {
	// Act.
	got := Changes([]string{"`fix` landed"})

	// Assert.
	if want := "`fix` landed."; got != want {
		t.Fatalf("Changes = %q, want %q", got, want)
	}
}

func TestChangesCountsAndJoinsSeveralChangesOldestFirst(t *testing.T) {
	// Act.
	got := Changes([]string{"`fix` put in line", "`old` landed", "deploy finished"})

	// Assert.
	if want := "3 changes: `fix` put in line; `old` landed; deploy finished."; got != want {
		t.Fatalf("Changes = %q, want %q", got, want)
	}
}

func TestChangesFailsHardOnNoChange(t *testing.T) {
	// Arrange.
	defer func() {
		// Assert.
		if recover() == nil {
			t.Fatal("Changes of no change must panic")
		}
	}()

	// Act.
	Changes(nil)
}

func TestQueueStateStatesTheQueueInAFewWords(t *testing.T) {
	fixing := member("ws-aside", "aside", true, "")
	fixing.Repair = sidebar.RepairFixingTests
	resolving := member("ws-conflict", "conflict", true, "")
	resolving.Repair = sidebar.RepairResolvingConflicts
	awaiting := member("ws-later", "later", true, "")
	awaiting.Repair = sidebar.RepairAwaitingFixingTests
	tests := []struct {
		name  string
		queue merge.QueueSnapshot
		want  string
	}{
		{name: "an empty queue", queue: merge.QueueSnapshot{Repo: repoDoom}, want: "Queue empty."},
		{name: "an empty paused queue", queue: merge.QueueSnapshot{Repo: repoDoom, Paused: true}, want: "Queue empty; paused."},
		{name: "a front merge at a step", queue: merge.QueueSnapshot{Repo: repoDoom, Members: []merge.QueueMember{
			member("ws-fix", "fix", true, footer.StepTesting), member("ws-new", "new", false, "")}}, want: "2 in line; `fix` testing."},
		{name: "a front merge started before its first step", queue: merge.QueueSnapshot{Repo: repoDoom, Members: []merge.QueueMember{
			member("ws-fix", "fix", true, "")}}, want: "1 in line; `fix` started."},
		{name: "a front merge not yet started", queue: merge.QueueSnapshot{Repo: repoDoom, Members: []merge.QueueMember{
			member("ws-fix", "fix", false, "")}}, want: "1 in line; `fix` next."},
		{name: "a merge fixing tests out of line", queue: merge.QueueSnapshot{Repo: repoDoom, Members: []merge.QueueMember{
			member("ws-fix", "fix", true, footer.StepTesting)}, Remediating: []merge.QueueMember{fixing}}, want: "1 in line; `fix` testing; `aside` fixing tests."},
		{name: "both repair slots held and a merge awaiting one", queue: merge.QueueSnapshot{Repo: repoDoom, Members: []merge.QueueMember{
			member("ws-fix", "fix", true, footer.StepTesting)}, Remediating: []merge.QueueMember{fixing, resolving, awaiting}},
			want: "1 in line; `fix` testing; `aside` fixing tests; `conflict` resolving conflicts; 1 awaiting a repair slot."},
		{name: "a paused queue", queue: merge.QueueSnapshot{Repo: repoDoom, Paused: true, Members: []merge.QueueMember{
			member("ws-fix", "fix", false, "")}}, want: "1 in line; `fix` next; paused."},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act.
			got := QueueState(tt.queue)

			// Assert.
			if got != tt.want {
				t.Fatalf("QueueState = %q, want %q", got, tt.want)
			}
		})
	}
}

// spliced is the shipped brief spliced with the values of one notice.
func spliced(t *testing.T, ended bool, sentences []string, latest merge.QueueChange) string {
	t.Helper()
	brief, err := prompts.Load("../../../prompts", BriefNotice)
	if err != nil {
		t.Fatalf("load the shipped brief: %v", err)
	}
	text, err := brief.Splice(Values(ended, sentences, latest))
	if err != nil {
		t.Fatalf("Splice: %v", err)
	}
	return text
}

func TestTheShippedBriefSplicesANoticeIntoOneLine(t *testing.T) {
	tests := []struct {
		name      string
		ended     bool
		sentences []string
		latest    merge.QueueChange
		want      string
	}{
		{name: "a landing with merges left", sentences: []string{"`fix` landed"},
			latest: change(merge.QueueChangeLanded, repoDoom, member("ws-fix", "fix", true, ""), member("ws-next", "next", true, footer.StepTesting)),
			want:   "merge queue of doom: `fix` landed. 1 in line; `next` testing."},
		{name: "the last notice", ended: true, sentences: []string{Sentence(landedLast)}, latest: landedLast,
			want: "merge queue of doom: `fix` landed. Queue empty. Subscription ended."},
		{name: "a deploy's end", sentences: []string{"deploy finished, carries `fix`"}, latest: deployed(merge.DeployEnd{Carried: []string{"fix"}}),
			want: "merge queue: deploy finished, carries `fix`."},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act.
			got := strings.TrimSpace(spliced(t, tt.ended, tt.sentences, tt.latest))

			// Assert.
			if got != tt.want {
				t.Fatalf("notice = %q, want %q", got, tt.want)
			}
		})
	}
}

func TestANoticeOfALongQueueStaysOneShortLine(t *testing.T) {
	// Arrange: a queue as long as a busy day's.
	var members []merge.QueueMember
	for i := range 12 {
		members = append(members, member(ids.WorkspaceID(fmt.Sprintf("ws-%d", i)), fmt.Sprintf("a-fairly-long-merge-name-%d", i), i == 0, footer.StepTesting))
	}
	latest := change(merge.QueueChangeEnqueued, repoDoom, members[len(members)-1], members...)

	// Act.
	got := strings.TrimSpace(spliced(t, false, []string{Sentence(latest)}, latest))

	// Assert.
	if strings.Contains(got, "\n") || len(got) > 160 {
		t.Fatalf("notice = %q (%d bytes), want one line of at most 160", got, len(got))
	}
}

func TestANewChangeSupersedesTheSubscribersUntoldNotice(t *testing.T) {
	// Arrange.
	f := newFixture(fakeSubscriptions{subs: []wsm.MergeQueueSubscription{sub(alice, repoDoom)}})
	f.s.deliver(context.Background(), f.prompts, queuedFix)

	// Act.
	f.s.deliver(context.Background(), f.prompts, landedLast)

	// Assert.
	if !reflect.DeepEqual(f.holds.asked, []ids.TurnID{"turn-ws-alice"}) {
		t.Fatalf("superseded = %v, want the first notice's turn", f.holds.asked)
	}
}

func TestTheSupersedingNoticeSaysEveryChangeOldestFirstThenTheQueueAsItNowStands(t *testing.T) {
	// Arrange.
	f := newFixture(fakeSubscriptions{subs: []wsm.MergeQueueSubscription{sub(alice, repoDoom)}})
	f.s.deliver(context.Background(), f.prompts, queuedFix)

	// Act.
	f.s.deliver(context.Background(), f.prompts, landedLast)

	// Assert.
	got := f.prompts.submissions()
	fields := fieldsOf(t, got[len(got)-1].text)
	want := []string{"merge queue of doom", "2 changes: `fix` put in line; `fix` landed.", " Queue empty."}
	if !reflect.DeepEqual(fields[:3], want) {
		t.Fatalf("notice = %q, want %q", fields[:3], want)
	}
}

func TestASupersessionIsRecordedWithWhatItCarries(t *testing.T) {
	// Arrange.
	f := newFixture(fakeSubscriptions{subs: []wsm.MergeQueueSubscription{sub(alice, repoDoom)}})
	f.s.deliver(context.Background(), f.prompts, queuedFix)

	// Act.
	f.s.deliver(context.Background(), f.prompts, landedLast)

	// Assert.
	record, ok := dlog.FindRecord(f.logs.Records(), dlog.RecordQuery{Level: "info", Operation: op, Message: "the subscriber's untold notice was superseded; the next one says its changes first"})
	if !ok || record.Context["superseded_turn"] != "turn-ws-alice" || record.Context["carried"] != 1 || record.Context["subscriber"] != string(alice) {
		t.Fatalf("no info record naming the superseded turn and what it carried: %+v", f.logs.Records())
	}
}

func TestANoticeAfterOneAlreadyToldSaysOnlyTheNewChange(t *testing.T) {
	// Arrange.
	f := newFixture(fakeSubscriptions{subs: []wsm.MergeQueueSubscription{sub(alice, repoDoom)}})
	f.holds.retired = false
	f.s.deliver(context.Background(), f.prompts, queuedFix)

	// Act.
	f.s.deliver(context.Background(), f.prompts, landedLast)

	// Assert.
	got := f.prompts.submissions()
	if changes := fieldsOf(t, got[len(got)-1].text)[1]; changes != Changes([]string{Sentence(landedLast)}) {
		t.Fatalf("changes = %q, want only the new change", changes)
	}
	if _, ok := dlog.FindRecord(f.logs.Records(), dlog.RecordQuery{Level: "debug", Operation: op, Message: "the subscriber's last notice was told; the next one says only the new change"}); !ok {
		t.Fatalf("no debug record of the told notice: %+v", f.logs.Records())
	}
}

func TestANoticeThatCannotBeSupersededStandsAndTheNextSaysOnlyTheNewChange(t *testing.T) {
	// Arrange.
	f := newFixture(fakeSubscriptions{subs: []wsm.MergeQueueSubscription{sub(alice, repoDoom)}})
	f.holds.err = errors.New("the store is gone")
	f.s.deliver(context.Background(), f.prompts, queuedFix)

	// Act.
	f.s.deliver(context.Background(), f.prompts, landedLast)

	// Assert.
	got := f.prompts.submissions()
	if changes := fieldsOf(t, got[len(got)-1].text)[1]; changes != Changes([]string{Sentence(landedLast)}) {
		t.Fatalf("changes = %q, want only the new change", changes)
	}
	record, ok := dlog.FindRecord(f.logs.Records(), dlog.RecordQuery{Level: "error", Operation: op, Message: "could not supersede the subscriber's untold notice; it stands and is told, and the next one says only the new change"})
	if !ok || record.Context["cause"] != "the store is gone" || record.Context["superseded_turn"] != "turn-ws-alice" {
		t.Fatalf("no error record naming the notice and the cause: %+v", f.logs.Records())
	}
}

func TestANoticeThatWasNeverSubmittedIsSaidByTheNextWithoutAskingTheQueue(t *testing.T) {
	tests := []struct {
		name string
		fail func(f *fixture)
		heal func(f *fixture)
	}{
		{
			name: "the submission failed",
			fail: func(f *fixture) { f.prompts.failFor = map[ids.WorkspaceID]error{alice: errors.New("parked")} },
			heal: func(f *fixture) { f.prompts.failFor = nil },
		},
		{
			name: "the brief could not be composed",
			fail: func(f *fixture) {
				f.s.deps.Briefs = func(string, map[string]string) (string, error) { return "", errors.New("missing") }
			},
			heal: func(f *fixture) { f.s.deps.Briefs = newFixture(fakeSubscriptions{}).s.deps.Briefs },
		},
		{
			name: "the key could not be minted",
			fail: func(f *fixture) {
				f.s.deps.MintKey = func(string) (string, error) { return "", errors.New("no entropy") }
			},
			heal: func(f *fixture) { f.s.deps.MintKey = func(p string) (string, error) { return p + "-ok", nil } },
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			f := newFixture(fakeSubscriptions{subs: []wsm.MergeQueueSubscription{sub(alice, repoDoom)}})
			tt.fail(f)
			f.s.deliver(context.Background(), f.prompts, queuedFix)
			tt.heal(f)

			// Act.
			f.s.deliver(context.Background(), f.prompts, landedLast)

			// Assert.
			got := f.prompts.submissions()
			if len(got) != 1 || len(f.holds.asked) != 0 {
				t.Fatalf("submissions = %+v, superseded = %v; want one notice and no supersession", got, f.holds.asked)
			}
			if changes := fieldsOf(t, got[0].text)[1]; !strings.Contains(changes, Sentence(queuedFix)) || !strings.Contains(changes, Sentence(landedLast)) {
				t.Fatalf("changes = %q, want both changes said", changes)
			}
		})
	}
}

func TestEachQueuesNoticesSupersedeOnlyTheirOwn(t *testing.T) {
	// Arrange.
	f := newFixture(fakeSubscriptions{subs: []wsm.MergeQueueSubscription{sub(alice, wsm.EveryRepository)}})
	f.s.deliver(context.Background(), f.prompts, queuedFix)

	// Act.
	f.s.deliver(context.Background(), f.prompts, change(merge.QueueChangeLanded, repoOther, member("ws-o", "o", true, "")))

	// Assert.
	if len(f.holds.asked) != 0 {
		t.Fatalf("superseded = %v, want the other queue's notice left alone", f.holds.asked)
	}
}

func TestValuesFailsHardOnANoticeOfNoChange(t *testing.T) {
	// Arrange.
	defer func() {
		if recover() == nil {
			t.Fatal("Values of no change must panic")
		}
	}()

	// Act.
	Values(false, nil, landedLast)
}

func TestTheUntoldNoticeSurvivesARestartAndIsSuperseded(t *testing.T) {
	// Arrange: the first daemon tells a change, then a restart builds a new
	// service over the same durable store.
	f := newFixture(fakeSubscriptions{subs: []wsm.MergeQueueSubscription{sub(alice, repoDoom)}})
	f.s.deliver(context.Background(), f.prompts, queuedFix)
	restarted := New(f.s.deps)

	// Act
	restarted.deliver(context.Background(), f.prompts, landedLast)

	// Assert
	got := f.prompts.submissions()
	if !reflect.DeepEqual(f.holds.asked, []ids.TurnID{"turn-ws-alice"}) || !strings.Contains(got[len(got)-1].text, Sentence(queuedFix)) {
		t.Fatalf("superseded = %v, last notice = %q; want the first daemon's notice superseded and said again", f.holds.asked, got[len(got)-1].text)
	}
}

func TestEachNoticeIsRecordedAsTheSubscribersUntoldNotice(t *testing.T) {
	// Arrange
	f := newFixture(fakeSubscriptions{subs: []wsm.MergeQueueSubscription{sub(alice, repoDoom)}})

	// Act
	f.s.deliver(context.Background(), f.prompts, queuedFix)

	// Assert
	n, ok := f.untold.rows[[2]string{string(alice), string(repoDoom)}]
	if !ok || n.Turn != "turn-ws-alice" || !reflect.DeepEqual(n.Sentences, []string{Sentence(queuedFix)}) {
		t.Fatalf("untold = %+v (%v), want the notice's turn and its one sentence", n, ok)
	}
}

func TestAnUnreadableUntoldNoticeIsRecordedAndTheNextSaysOnlyTheNewChange(t *testing.T) {
	// Arrange
	f := newFixture(fakeSubscriptions{subs: []wsm.MergeQueueSubscription{sub(alice, repoDoom)}})
	f.untold.readErr = errors.New("the store is gone")

	// Act
	f.s.deliver(context.Background(), f.prompts, landedLast)

	// Assert
	got := f.prompts.submissions()
	if len(got) != 1 || fieldsOf(t, got[0].text)[1] != Changes([]string{Sentence(landedLast)}) || len(f.holds.asked) != 0 {
		t.Fatalf("submissions = %+v, superseded %v; want the new change alone and nothing superseded", got, f.holds.asked)
	}
	record, ok := dlog.FindRecord(f.logs.Records(), dlog.RecordQuery{Level: "error", Operation: op, Message: "could not read the subscriber's untold notice; the next one says only the new change"})
	if !ok || record.Context["cause"] != "the store is gone" || record.Context["subscriber"] != string(alice) {
		t.Fatalf("no error record of the unread notice: %+v", f.logs.Records())
	}
}

func TestAnUnrecordableUntoldNoticeIsRecordedAndTheNoticeStillTold(t *testing.T) {
	// Arrange
	f := newFixture(fakeSubscriptions{subs: []wsm.MergeQueueSubscription{sub(alice, repoDoom)}})
	f.untold.recordErr = errors.New("disk full")

	// Act
	f.s.deliver(context.Background(), f.prompts, landedLast)

	// Assert
	if got := f.prompts.submissions(); len(got) != 1 {
		t.Fatalf("submissions = %+v, want the notice told", got)
	}
	record, ok := dlog.FindRecord(f.logs.Records(), dlog.RecordQuery{Level: "error", Operation: op, Message: "could not record the subscriber's untold notice; the next notice cannot supersede it"})
	if !ok || record.Context["cause"] != "disk full" {
		t.Fatalf("no error record of the unrecorded notice: %+v", f.logs.Records())
	}
}

// deployed is a deploy's end, as DeployEnded reports it: no queue's.
func deployed(end merge.DeployEnd) merge.QueueChange {
	return merge.QueueChange{Kind: merge.QueueChangeDeployed, Deploy: end}
}

func TestADeploysEndIsToldInAFewWords(t *testing.T) {
	tests := []struct {
		name string
		end  merge.DeployEnd
		want string
	}{
		{name: "a deploy whose build runs", end: merge.DeployEnd{Carried: []string{"fix-land", "docs-land"}},
			want: "deploy finished, carries `fix-land`, `docs-land`"},
		{name: "a deploy carrying no landing", end: merge.DeployEnd{}, want: "deploy finished"},
		{name: "a deploy that failed", end: merge.DeployEnd{Failure: "the build failed"},
			want: "deploy failed (the build failed); the running build does not carry it"},
		{name: "a fresh boot", end: merge.DeployEnd{Boot: true, Carried: []string{"fix-land"}}, want: "daemon booted on a new build, carries `fix-land`"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act.
			got := Sentence(deployed(tt.end))

			// Assert.
			if got != tt.want {
				t.Fatalf("Sentence = %q, want %q", got, tt.want)
			}
		})
	}
}

func TestADeploysEndReachesEverySubscriptionWhateverItsScope(t *testing.T) {
	// Arrange: subscriptions of every scope.
	subs := []wsm.MergeQueueSubscription{sub(alice, repoDoom), sub(bob, wsm.EveryRepository), sub("ws-carol", repoOther)}

	// Act.
	got := Recipients(subs, deployed(merge.DeployEnd{}))

	// Assert.
	if want := []ids.WorkspaceID{alice, bob, "ws-carol"}; !reflect.DeepEqual(got, want) {
		t.Fatalf("Recipients = %v, want every scope's subscriber", got)
	}
}

func TestADeploysNoticeNamesNoQueue(t *testing.T) {
	// Act.
	values := Values(false, []string{Sentence(deployed(merge.DeployEnd{}))}, deployed(merge.DeployEnd{}))

	// Assert.
	if values["scope"] != "merge queue" || values["queue"] != "" {
		t.Fatalf("scope = %q, queue = %q; want no repository and no queue state", values["scope"], values["queue"])
	}
}

func TestDeployEndedIsDeliveredToItsSubscribers(t *testing.T) {
	// Arrange.
	f := newFixture(fakeSubscriptions{subs: []wsm.MergeQueueSubscription{sub(alice, repoOther)}})
	ctx, cancel := context.WithCancel(context.Background())

	// Act: the end is reported, then the loop runs it and drains.
	f.s.DeployEnded(merge.DeployEnd{Carried: []string{"fix-land"}})
	cancel()
	f.s.Run(ctx, f.prompts)

	// Assert.
	got := f.prompts.submissions()
	if len(got) != 1 || got[0].ws != alice || !strings.Contains(got[0].text, "`fix-land`") {
		t.Fatalf("submissions = %+v, want alice told of the deploy carrying fix-land", got)
	}
}

// carol is a third subscriber.
const carol = ids.WorkspaceID("ws-carol")

func TestADepletionEndsEverySubscriptionInItsScopeAndTellsItsLastNotice(t *testing.T) {
	// Arrange
	scoped, unscoped, elsewhere := sub(alice, repoDoom), sub(bob, wsm.EveryRepository), sub(carol, repoOther)
	f := newFixture(fakeSubscriptions{subs: []wsm.MergeQueueSubscription{scoped, unscoped, elsewhere}})

	// Act
	f.s.deliver(context.Background(), f.prompts, landedLast)

	// Assert
	if want := []wsm.MergeQueueSubscription{scoped, unscoped}; !reflect.DeepEqual(f.subs.ended, want) {
		t.Fatalf("ended = %+v, want %+v", f.subs.ended, want)
	}
	got := f.prompts.submissions()
	if len(got) != 2 {
		t.Fatalf("submissions = %+v, want alice and bob told", got)
	}
	for _, told := range got {
		if lifetime := fieldsOf(t, told.text)[3]; lifetime != Lifetime(true) {
			t.Fatalf("%s's lifetime = %q, want its last notice", told.ws, lifetime)
		}
	}
}

func TestADepletionRecordsEachSubscriptionItEnds(t *testing.T) {
	// Arrange
	f := newFixture(fakeSubscriptions{subs: []wsm.MergeQueueSubscription{sub(alice, repoDoom)}})

	// Act
	f.s.deliver(context.Background(), f.prompts, landedLast)

	// Assert
	record, ok := dlog.FindRecord(f.logs.Records(), dlog.RecordQuery{Level: "info", Operation: op, Message: "ended a merge queue subscription as its queue depleted"})
	if !ok {
		t.Fatalf("no record of the ended subscription: %+v", f.logs.Records())
	}
	want := dlog.Context{"subscriber": string(alice), "scope": string(repoDoom), "repo": string(repoDoom), "change": "landed", "workspace": "ws-fix", "depleted": true}
	for k, v := range want {
		if record.Context[k] != v {
			t.Fatalf("record context %s = %v, want %v (%+v)", k, record.Context[k], v, record.Context)
		}
	}
	told, ok := dlog.FindRecord(f.logs.Records(), dlog.RecordQuery{Level: "info", Operation: op, Message: "told a subscriber's agent of a merge queue change"})
	if !ok || told.Context["ended"] != true {
		t.Fatalf("no record of the last notice marked ended: %+v", f.logs.Records())
	}
}

func TestAQueueLeftNonEmptyKeepsItsSubscriptions(t *testing.T) {
	tests := []struct {
		name   string
		change merge.QueueChange
	}{
		{name: "a merge left with another in line", change: change(merge.QueueChangeLanded, repoDoom, member("ws-fix", "fix", true, ""), member("ws-next", "next", false, ""))},
		{name: "a merge put in line", change: queuedFix},
		{name: "a deploy ended", change: deployed(merge.DeployEnd{})},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newFixture(fakeSubscriptions{subs: []wsm.MergeQueueSubscription{sub(alice, repoDoom)}})

			// Act
			f.s.deliver(context.Background(), f.prompts, tt.change)

			// Assert
			if len(f.subs.ended) != 0 {
				t.Fatalf("ended = %+v, want none", f.subs.ended)
			}
			got := f.prompts.submissions()
			if len(got) != 1 || fieldsOf(t, got[0].text)[3] != Lifetime(false) {
				t.Fatalf("submissions = %+v, want one notice saying nothing of an end", got)
			}
		})
	}
}

func TestASubscriptionTakenAgainSinceTheReadStandsAndIsNotToldItEnded(t *testing.T) {
	// Arrange
	f := newFixture(fakeSubscriptions{subs: []wsm.MergeQueueSubscription{sub(alice, repoDoom)}, takenAgain: true})

	// Act
	f.s.deliver(context.Background(), f.prompts, landedLast)

	// Assert
	if got := f.prompts.submissions(); len(got) != 1 || fieldsOf(t, got[0].text)[3] != Lifetime(false) {
		t.Fatalf("submissions = %+v, want one notice saying nothing of an end", got)
	}
	record, ok := dlog.FindRecord(f.logs.Records(), dlog.RecordQuery{Level: "info", Operation: op, Message: "a merge queue subscription was taken again after the depletion was read; the new one stands"})
	if !ok || record.Context["subscriber"] != string(alice) || record.Context["scope"] != string(repoDoom) {
		t.Fatalf("no record of the subscription taken again: %+v", f.logs.Records())
	}
}

func TestAnUnendableSubscriptionIsRecordedAndItsNoticeSaysItStands(t *testing.T) {
	// Arrange
	f := newFixture(fakeSubscriptions{subs: []wsm.MergeQueueSubscription{sub(alice, repoDoom)}, endErr: errors.New("the store is gone")})

	// Act
	f.s.deliver(context.Background(), f.prompts, landedLast)

	// Assert
	if got := f.prompts.submissions(); len(got) != 1 || fieldsOf(t, got[0].text)[3] != Lifetime(false) {
		t.Fatalf("submissions = %+v, want the notice still told, saying nothing of an end", got)
	}
	record, ok := dlog.FindRecord(f.logs.Records(), dlog.RecordQuery{Level: "error", Operation: op, Message: "could not end a merge queue subscription as its queue depleted; it stands, and its notice does not say it ended"})
	if !ok || record.Context["cause"] != "the store is gone" || record.Context["subscriber"] != string(alice) {
		t.Fatalf("no error record naming the subscription and its cause: %+v", f.logs.Records())
	}
}

func TestASubscriberWithOneSubscriptionLeftStandingIsNotToldItEnded(t *testing.T) {
	// Arrange: alice's scoped subscription ends; her unscoped one was taken
	// again since the read, so she still holds a subscription.
	f := newFixture(fakeSubscriptions{subs: []wsm.MergeQueueSubscription{sub(alice, wsm.EveryRepository), sub(alice, repoDoom)}})
	f.s.deps.Subscriptions = &partialEnd{fakeSubscriptions: f.subs, stands: wsm.EveryRepository}

	// Act
	f.s.deliver(context.Background(), f.prompts, landedLast)

	// Assert
	if got := f.prompts.submissions(); len(got) != 1 || fieldsOf(t, got[0].text)[3] != Lifetime(false) {
		t.Fatalf("submissions = %+v, want one notice saying nothing of an end", got)
	}
}

// partialEnd ends every subscription but the one in scope stands.
type partialEnd struct {
	*fakeSubscriptions
	stands wsm.RepoKey
}

func (p *partialEnd) EndMergeQueueSubscription(ctx context.Context, sub wsm.MergeQueueSubscription) (bool, error) {
	if sub.Repo == p.stands {
		return false, nil
	}
	return p.fakeSubscriptions.EndMergeQueueSubscription(ctx, sub)
}

func TestInScopeIsTheChangesQueueOrEveryQueue(t *testing.T) {
	tests := []struct {
		name   string
		sub    wsm.MergeQueueSubscription
		change merge.QueueChange
		want   bool
	}{
		{name: "the change's own queue", sub: sub(alice, repoDoom), change: landedLast, want: true},
		{name: "every queue", sub: sub(alice, wsm.EveryRepository), change: landedLast, want: true},
		{name: "another queue", sub: sub(alice, repoOther), change: landedLast, want: false},
		{name: "a deploy, from another queue", sub: sub(alice, repoOther), change: deployed(merge.DeployEnd{}), want: true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got := InScope(tt.sub, tt.change)

			// Assert
			if got != tt.want {
				t.Fatalf("InScope = %v, want %v", got, tt.want)
			}
		})
	}
}
