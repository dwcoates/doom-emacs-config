package workspacesubs

import (
	"context"
	"errors"
	"fmt"
	"os"
	"reflect"
	"strings"
	"sync"
	"testing"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	conversationv1 "agentrepl/proto/conversation/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/feedid"
	"agent-repld/internal/ids"
	"agent-repld/internal/prompthandler"
	"agent-repld/internal/wsm"
)

const (
	followed  = ids.WorkspaceID("ws-followed")
	watcherA  = ids.WorkspaceID("ws-a")
	watcherB  = ids.WorkspaceID("ws-b")
	briefText = "notice|"
)

type submission struct {
	ws     ids.WorkspaceID
	text   string
	origin conversationv1.PromptOrigin
	defer_ bool
}

type fakePrompts struct {
	mu      sync.Mutex
	got     []submission
	failFor map[ids.WorkspaceID]error
}

func (p *fakePrompts) Submit(_ context.Context, ws ids.WorkspaceID, said *conversationv1.UserSaid, _ string, origin conversationv1.PromptOrigin, _ *ids.WorkspaceID, delivery wsm.Delivery, _ *feedid.Ref) (prompthandler.Outcome, error) {
	if err := p.failFor[ws]; err != nil {
		return prompthandler.Outcome{}, err
	}
	p.mu.Lock()
	defer p.mu.Unlock()
	p.got = append(p.got, submission{ws: ws, text: said.GetContent().GetBlocks()[0].GetText().GetText(), origin: origin, defer_: delivery == wsm.DeliveryDeferred})
	return prompthandler.Outcome{Turn: ids.TurnID(fmt.Sprintf("turn-%s-%d", ws, len(p.got)))}, nil
}

type fakeSubscribers struct {
	subs []wsm.WorkspaceSubscription
	err  error
}

func (f *fakeSubscribers) WorkspaceSubscribers(context.Context, wsm.WorkspaceID) ([]wsm.WorkspaceSubscription, error) {
	return f.subs, f.err
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

// fakeUntold is the durable untold-notice store in memory.
type fakeUntold struct {
	rows      map[[2]ids.WorkspaceID]wsm.UntoldWorkspaceNotice
	readErr   error
	recordErr error
}

func (u *fakeUntold) UntoldWorkspaceNotice(_ context.Context, subscriber, followed ids.WorkspaceID) (wsm.UntoldWorkspaceNotice, bool, error) {
	if u.readErr != nil {
		return wsm.UntoldWorkspaceNotice{}, false, u.readErr
	}
	n, ok := u.rows[[2]ids.WorkspaceID{subscriber, followed}]
	return n, ok, nil
}

func (u *fakeUntold) RecordUntoldWorkspaceNotice(_ context.Context, n wsm.UntoldWorkspaceNotice) error {
	if u.recordErr != nil {
		return u.recordErr
	}
	u.rows[[2]ids.WorkspaceID{n.Subscriber, n.Followed}] = n
	return nil
}

type fixture struct {
	s         *Service
	prompts   *fakePrompts
	subs      *fakeSubscribers
	holds     *fakeHolds
	untold    *fakeUntold
	status    func() *agentreplv1.WorkspaceStatus
	statusErr error
	briefErr  error
	logs      *dlog.TestSurfaces
}

func newFixture() *fixture {
	f := &fixture{prompts: &fakePrompts{failFor: map[ids.WorkspaceID]error{}}, subs: &fakeSubscribers{},
		holds: &fakeHolds{retired: true}, untold: &fakeUntold{rows: map[[2]ids.WorkspaceID]wsm.UntoldWorkspaceNotice{}},
		status: idleStatus, logs: dlog.NewTestSurfaces()}
	keys := 0
	f.s = New(Deps{
		Subscribers: f.subs,
		Holds:       f.holds,
		Untold:      f.untold,
		Status: func(_ context.Context, ws ids.WorkspaceID) (*agentreplv1.WorkspaceStatus, error) {
			if f.statusErr != nil {
				return nil, f.statusErr
			}
			return f.status(), nil
		},
		Briefs: func(name string, values map[string]string) (string, error) {
			if f.briefErr != nil {
				return "", f.briefErr
			}
			if name != BriefNotice {
				return "", fmt.Errorf("unknown brief %q", name)
			}
			return briefText + values["label"] + "|" + values["why"] + "|" + values["event"] + "|" + values["status"] + "|" + values["stop"], nil
		},
		MintKey: func(prefix string) (string, error) {
			keys++
			return fmt.Sprintf("%s-%d", prefix, keys), nil
		},
		Log: f.logs,
	})
	return f
}

func idleStatus() *agentreplv1.WorkspaceStatus {
	return &agentreplv1.WorkspaceStatus{
		Workspace:   &workspacev1.WorkspaceRef{Id: string(followed)},
		Name:        "followed",
		Branch:      "agent/followed",
		MergeTarget: &agentreplv1.WorkspaceStatusMergeTarget{Target: &agentreplv1.WorkspaceStatusMergeTarget_Primary{Primary: &agentreplv1.WorkspaceStatusMergeTargetPrimary{}}},
		Standing: &agentreplv1.WorkspaceStatusStanding{Standing: &agentreplv1.WorkspaceStatusStanding_Open{Open: &agentreplv1.WorkspaceStatusOpen{
			Activity: &agentreplv1.WorkspaceStatusActivity{Activity: &agentreplv1.WorkspaceStatusActivity_Idle{Idle: &agentreplv1.WorkspaceStatusActivityIdle{}}}}}},
	}
}

// deliverAll delivers every pending edge through the fixture's prompts.
func (f *fixture) deliverAll() {
	ctx, cancel := context.WithCancel(context.Background())
	cancel()
	f.s.Run(ctx, f.prompts)
}

func subscribed(subscriber ids.WorkspaceID, events ...wsm.WorkspaceEvent) wsm.WorkspaceSubscription {
	return wsm.WorkspaceSubscription{Subscriber: subscriber, Followed: followed, Events: wsm.NewWorkspaceEvents(events...)}
}

func TestEachEdgeIsToldToTheSubscribersThatAskedForIt(t *testing.T) {
	tests := []struct {
		name  string
		edge  func(s *Service)
		event wsm.WorkspaceEvent
		words string
	}{
		{name: "a turn starting", edge: func(s *Service) { s.TurnStarted(followed) }, event: wsm.WorkspaceEventTurnStarted, words: "started a turn."},
		{name: "a turn ending", edge: func(s *Service) { s.TurnEnded(followed) }, event: wsm.WorkspaceEventTurnEnded, words: "ended a turn."},
		{name: "an ask opening", edge: func(s *Service) { s.NeedsInput(followed) }, event: wsm.WorkspaceEventNeedsInput, words: "is waiting on the user"},
		{name: "a landing", edge: func(s *Service) { s.Landed(followed) }, event: wsm.WorkspaceEventLanded, words: "landed its branch."},
		{name: "a close", edge: func(s *Service) { s.Closed(wsm.Workspace{ID: followed}) }, event: wsm.WorkspaceEventClosed, words: "closed."},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newFixture()
			f.subs.subs = []wsm.WorkspaceSubscription{subscribed(watcherA, tt.event), subscribed(watcherB, otherEvent(tt.event))}

			// Act
			tt.edge(f.s)
			f.deliverAll()

			// Assert
			if len(f.prompts.got) != 1 || f.prompts.got[0].ws != watcherA {
				t.Fatalf("submitted = %+v, want only the subscriber that asked for %s", f.prompts.got, tt.event)
			}
			got := f.prompts.got[0]
			if got.origin != Origin || !got.defer_ || !strings.Contains(got.text, tt.words) || !strings.Contains(got.text, "Its status now: `followed`") {
				t.Errorf("notice = %+v, want a deferred workspace notice naming the edge and the status", got)
			}
		})
	}
}

func TestAParentIsToldAsTheParentAndASubscriberAsASubscriber(t *testing.T) {
	tests := []struct {
		name          string
		parentFollows bool
		want          []string
		wantNot       string
	}{
		{name: "a parent following its child", parentFollows: true,
			want:    []string{"Your child workspace `followed` changed; as its parent you follow it without subscribing.", "These notices end when the child closes"},
			wantNot: "unsubscribe"},
		{name: "a subscriber", parentFollows: false,
			want:    []string{"A workspace you subscribed to changed.", "unsubscribe through `agent-repld workspace unsubscribe`"},
			wantNot: "Your child workspace"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newFixture()
			sub := subscribed(watcherA, wsm.WorkspaceEventTurnEnded)
			sub.ParentFollows = tt.parentFollows
			f.subs.subs = []wsm.WorkspaceSubscription{sub}

			// Act
			f.s.TurnEnded(followed)
			f.deliverAll()

			// Assert
			if len(f.prompts.got) != 1 {
				t.Fatalf("submitted = %+v, want one notice", f.prompts.got)
			}
			text := f.prompts.got[0].text
			for _, want := range tt.want {
				if !strings.Contains(text, want) {
					t.Errorf("notice %q lacks %q", text, want)
				}
			}
			if strings.Contains(text, tt.wantNot) {
				t.Errorf("notice %q says %q", text, tt.wantNot)
			}
		})
	}
}

// otherEvent is an event other than E.
func otherEvent(e wsm.WorkspaceEvent) wsm.WorkspaceEvent {
	if e == wsm.WorkspaceEventClosed {
		return wsm.WorkspaceEventLanded
	}
	return wsm.WorkspaceEventClosed
}

func TestATurnEndNoDaemonWatchedIsToldToNobody(t *testing.T) {
	// Arrange
	f := newFixture()
	f.subs.subs = []wsm.WorkspaceSubscription{subscribed(watcherA, wsm.WorkspaceEventTurnEnded)}

	// Act
	f.s.TurnEndedUnobserved(followed)
	f.deliverAll()

	// Assert
	if len(f.prompts.got) != 0 {
		t.Errorf("submitted = %+v, want nothing: a change no daemon served is never noticed", f.prompts.got)
	}
}

func TestAnEdgeNobodyAskedForReadsNoStatus(t *testing.T) {
	// Arrange
	f := newFixture()
	f.statusErr = errors.New("must not be read")
	f.subs.subs = []wsm.WorkspaceSubscription{subscribed(watcherA, wsm.WorkspaceEventLanded)}

	// Act
	f.s.TurnStarted(followed)
	f.deliverAll()

	// Assert
	if len(f.prompts.got) != 0 || len(f.logs.Records()) != 0 {
		t.Errorf("submitted %+v, records %+v; want nothing", f.prompts.got, f.logs.Records())
	}
}

func TestAnEdgeThatCannotBeToldIsRecorded(t *testing.T) {
	tests := []struct {
		name    string
		arrange func(f *fixture)
		message string
	}{
		{name: "the subscribers cannot be read", arrange: func(f *fixture) { f.subs.err = errors.New("db gone") },
			message: "could not read the workspace's subscribers; nobody is told of the edge"},
		{name: "the status cannot be read", arrange: func(f *fixture) { f.statusErr = errors.New("db gone") },
			message: "could not read the followed workspace's status; its subscribers are not told of the edge"},
		{name: "the brief cannot be composed", arrange: func(f *fixture) { f.briefErr = errors.New("brief gone") },
			message: "could not compose a workspace notice; the subscriber is not told of the edge"},
		{name: "the submission fails", arrange: func(f *fixture) { f.prompts.failFor[watcherA] = errors.New("queue gone") },
			message: "could not submit a workspace notice to its subscriber"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newFixture()
			f.subs.subs = []wsm.WorkspaceSubscription{subscribed(watcherA, wsm.WorkspaceEventLanded)}
			tt.arrange(f)

			// Act
			f.s.Landed(followed)
			f.deliverAll()

			// Assert
			if len(f.prompts.got) != 0 {
				t.Errorf("submitted = %+v, want nothing", f.prompts.got)
			}
			record, ok := dlog.FindRecord(f.logs.Records(), dlog.RecordQuery{Level: "error", Operation: op, Message: tt.message})
			if !ok {
				t.Fatalf("no error record %q: %+v", tt.message, f.logs.Records())
			}
			if record.Context["workspace"] != string(followed) || record.Context["event"] != "landed" {
				t.Errorf("record context = %+v, want the edge", record.Context)
			}
		})
	}
}

func TestStatusWordsStatesTheStatus(t *testing.T) {
	ownerRef := &workspacev1.WorkspaceRef{Id: "ws-owner", Dir: "/wt/owner"}
	into := &agentreplv1.WorkspaceStatusMergeTarget{Target: &agentreplv1.WorkspaceStatusMergeTarget_Workspace{Workspace: &agentreplv1.WorkspaceStatusMergeTargetWorkspace{
		Workspace: ownerRef}}}
	closed := &agentreplv1.WorkspaceStatusStanding{Standing: &agentreplv1.WorkspaceStatusStanding_Closed{Closed: &agentreplv1.WorkspaceStatusClosed{}}}
	tests := []struct {
		name   string
		status func() *agentreplv1.WorkspaceStatus
		want   string
	}{
		{name: "an open workspace on the primary branch", status: idleStatus,
			want: "Its status now: `followed`, on branch `agent/followed`, lands on the primary branch; open, idle."},
		{name: "a closed hidden child its owner lands", status: func() *agentreplv1.WorkspaceStatus {
			s := idleStatus()
			s.MergeTarget, s.Standing, s.Hidden = into, closed, true
			s.Owner = &agentreplv1.WorkspaceStatusOwner{Workspace: ownerRef, OwnerLands: true}
			return s
		}, want: "Its status now: `followed`, on branch `agent/followed`, lands into the workspace at `/wt/owner` and is landed by its owner; closed, hidden."},
		{name: "an open peer landing into another workspace, which lands itself", status: func() *agentreplv1.WorkspaceStatus {
			s := idleStatus()
			s.MergeTarget = into
			return s
		}, want: "Its status now: `followed`, on branch `agent/followed`, lands into the workspace at `/wt/owner`; open, idle."},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got := StatusWords(tt.status())

			// Assert
			if got != tt.want {
				t.Errorf("StatusWords =\n%q\nwant\n%q", got, tt.want)
			}
		})
	}
}

func TestANewEdgeSupersedesTheSubscribersUntoldNoticeAndSaysItsChangesFirst(t *testing.T) {
	// Arrange: a turn starts, and ends before its notice is told.
	f := newFixture()
	f.subs.subs = []wsm.WorkspaceSubscription{subscribed(watcherA, wsm.WorkspaceEventTurnStarted, wsm.WorkspaceEventTurnEnded)}
	f.s.TurnStarted(followed)

	// Act
	f.s.TurnEnded(followed)
	f.deliverAll()

	// Assert
	last := f.prompts.got[len(f.prompts.got)-1].text
	want := "It changed 2 times since the last notice you were told, oldest first:\n- `followed` started a turn.\n- `followed` ended a turn."
	if !reflect.DeepEqual(f.holds.asked, []ids.TurnID{"turn-ws-a-1"}) || !strings.Contains(last, want) {
		t.Fatalf("superseded %v, last notice %q; want the first notice superseded and both edges said", f.holds.asked, last)
	}
}

func TestTheSupersedingNoticeCarriesTheStatusAsItNowStands(t *testing.T) {
	// Arrange: the workspace ran a turn, then closed.
	f := newFixture()
	f.subs.subs = []wsm.WorkspaceSubscription{subscribed(watcherA, wsm.WorkspaceEventTurnStarted, wsm.WorkspaceEventClosed)}
	reads := 0
	f.status = func() *agentreplv1.WorkspaceStatus {
		reads++
		st := idleStatus()
		if reads > 1 {
			st.Standing = &agentreplv1.WorkspaceStatusStanding{Standing: &agentreplv1.WorkspaceStatusStanding_Closed{Closed: &agentreplv1.WorkspaceStatusClosed{}}}
		}
		return st
	}
	f.s.TurnStarted(followed)

	// Act
	f.s.Closed(wsm.Workspace{ID: followed})
	f.deliverAll()

	// Assert
	last := f.prompts.got[len(f.prompts.got)-1].text
	if !strings.Contains(last, "closed.") || strings.Contains(last, "open, idle") {
		t.Fatalf("last notice %q, want the workspace closed as it now stands", last)
	}
}

func TestTheUntoldWorkspaceNoticeSurvivesARestart(t *testing.T) {
	// Arrange
	f := newFixture()
	f.subs.subs = []wsm.WorkspaceSubscription{subscribed(watcherA, wsm.WorkspaceEventTurnStarted, wsm.WorkspaceEventTurnEnded)}
	f.s.TurnStarted(followed)
	f.deliverAll()
	restarted := New(f.s.deps)

	// Act
	restarted.TurnEnded(followed)
	ctx, cancel := context.WithCancel(context.Background())
	cancel()
	restarted.Run(ctx, f.prompts)

	// Assert
	if !reflect.DeepEqual(f.holds.asked, []ids.TurnID{"turn-ws-a-1"}) {
		t.Fatalf("superseded = %v, want the first daemon's notice superseded", f.holds.asked)
	}
}

func TestAnUntoldWorkspaceNoticeThatCannotBeReadOrRecordedIsRecorded(t *testing.T) {
	tests := []struct {
		name    string
		arrange func(f *fixture)
		message string
	}{
		{name: "unreadable", arrange: func(f *fixture) { f.untold.readErr = errors.New("db gone") },
			message: "could not read the subscriber's untold notice; the next one says only the new change"},
		{name: "unrecordable", arrange: func(f *fixture) { f.untold.recordErr = errors.New("disk full") },
			message: "could not record the subscriber's untold notice; the next notice cannot supersede it"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newFixture()
			f.subs.subs = []wsm.WorkspaceSubscription{subscribed(watcherA, wsm.WorkspaceEventLanded)}
			tt.arrange(f)

			// Act
			f.s.Landed(followed)
			f.deliverAll()

			// Assert
			if len(f.prompts.got) != 1 {
				t.Fatalf("submitted = %+v, want the notice still told", f.prompts.got)
			}
			if _, ok := dlog.FindRecord(f.logs.Records(), dlog.RecordQuery{Level: "error", Operation: op, Message: tt.message}); !ok {
				t.Fatalf("no error record %q: %+v", tt.message, f.logs.Records())
			}
		})
	}
}

func TestBothNotifiersShareOneSupersession(t *testing.T) {
	// Arrange: the supersede-and-carry decision lives in internal/untoldnotice
	// alone; a notifier that hand-rolled its own would call Supersede itself.
	var calls int
	for _, path := range []string{"workspacesubs.go", "../mergenotice/mergenotice.go"} {
		src, err := os.ReadFile(path)
		if err != nil {
			t.Fatalf("read %s: %v", path, err)
		}

		// Act
		if strings.Contains(string(src), ".Supersede(") {
			t.Errorf("%s calls Supersede itself; take untoldnotice.Carried", path)
		}
		calls += strings.Count(string(src), "untoldnotice.Carried(")
	}

	// Assert
	if calls != 2 {
		t.Fatalf("untoldnotice.Carried calls = %d, want one per notifier", calls)
	}
}
