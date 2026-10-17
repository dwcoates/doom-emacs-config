package workspacesubs

import (
	"context"
	"errors"
	"fmt"
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

func (p *fakePrompts) Submit(_ context.Context, ws ids.WorkspaceID, said *conversationv1.UserSaid, _ string, origin conversationv1.PromptOrigin, delivery wsm.Delivery, _ *feedid.Ref) (prompthandler.Outcome, error) {
	if err := p.failFor[ws]; err != nil {
		return prompthandler.Outcome{}, err
	}
	p.mu.Lock()
	defer p.mu.Unlock()
	p.got = append(p.got, submission{ws: ws, text: said.GetContent().GetBlocks()[0].GetText().GetText(), origin: origin, defer_: delivery == wsm.DeliveryDeferred})
	return prompthandler.Outcome{Turn: ids.TurnID("turn-" + string(ws))}, nil
}

type fakeSubscribers struct {
	subs []wsm.WorkspaceSubscription
	err  error
}

func (f *fakeSubscribers) WorkspaceSubscribers(context.Context, wsm.WorkspaceID) ([]wsm.WorkspaceSubscription, error) {
	return f.subs, f.err
}

type fixture struct {
	s         *Service
	prompts   *fakePrompts
	subs      *fakeSubscribers
	statusErr error
	briefErr  error
	logs      *dlog.TestSurfaces
}

func newFixture() *fixture {
	f := &fixture{prompts: &fakePrompts{failFor: map[ids.WorkspaceID]error{}}, subs: &fakeSubscribers{}, logs: dlog.NewTestSurfaces()}
	keys := 0
	f.s = New(Deps{
		Subscribers: f.subs,
		Status: func(_ context.Context, ws ids.WorkspaceID) (*agentreplv1.WorkspaceStatus, error) {
			if f.statusErr != nil {
				return nil, f.statusErr
			}
			return idleStatus(), nil
		},
		Briefs: func(name string, values map[string]string) (string, error) {
			if f.briefErr != nil {
				return "", f.briefErr
			}
			if name != BriefNotice {
				return "", fmt.Errorf("unknown brief %q", name)
			}
			return briefText + values["label"] + "|" + values["event"] + "|" + values["status"], nil
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
			message: "could not compose a workspace notice; its subscribers are not told of the edge"},
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
	owner := &agentreplv1.WorkspaceStatusMergeTarget{Target: &agentreplv1.WorkspaceStatusMergeTarget_Owner{Owner: &agentreplv1.WorkspaceStatusMergeTargetOwner{
		Owner: &workspacev1.WorkspaceRef{Id: "ws-owner"}, OwnerLands: true}}}
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
			s.MergeTarget, s.Standing, s.Hidden = owner, closed, true
			return s
		}, want: "Its status now: `followed`, on branch `agent/followed`, lands into the workspace `ws-owner` and is landed by its owner; closed, hidden."},
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
