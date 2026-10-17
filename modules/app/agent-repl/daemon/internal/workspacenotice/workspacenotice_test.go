package workspacenotice

import (
	"context"
	"errors"
	"fmt"
	"reflect"
	"slices"
	"strings"
	"sync"
	"testing"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/feedid"
	"agent-repld/internal/ids"
	"agent-repld/internal/prompthandler"
	"agent-repld/internal/wsm"
)

const (
	owner     = ids.WorkspaceID("ws-owner")
	source    = ids.WorkspaceID("ws-source")
	requester = ids.WorkspaceID("ws-requester")
	child     = ids.WorkspaceID("ws-child")
)

func idp(id ids.WorkspaceID) *ids.WorkspaceID { return &id }

// fakeWorkspaces answers the registry: the named workspaces, closed when
// listed in closed, and err for every id in failFor.
type fakeWorkspaces struct {
	byID    map[ids.WorkspaceID]wsm.Workspace
	failFor map[ids.WorkspaceID]error
}

func (f fakeWorkspaces) Workspace(_ context.Context, id wsm.WorkspaceID) (wsm.Workspace, error) {
	if err := f.failFor[id]; err != nil {
		return wsm.Workspace{}, err
	}
	ws, ok := f.byID[id]
	if !ok {
		return wsm.Workspace{}, wsm.ErrNotFound
	}
	return ws, nil
}

// submission is one captured notice.
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

var _ prompthandler.Submitter = (*fakePrompts)(nil)

func (p *fakePrompts) Submit(_ context.Context, ws ids.WorkspaceID, said *conversationv1.UserSaid, _ string, origin conversationv1.PromptOrigin, delivery wsm.Delivery, _ *feedid.Ref) (prompthandler.Outcome, error) {
	if err := p.failFor[ws]; err != nil {
		return prompthandler.Outcome{}, err
	}
	p.mu.Lock()
	defer p.mu.Unlock()
	p.got = append(p.got, submission{ws: ws, text: said.GetContent().GetBlocks()[0].GetText().GetText(), origin: origin, defer_: delivery == wsm.DeliveryDeferred})
	return prompthandler.Outcome{Turn: ids.TurnID("turn-" + string(ws))}, nil
}

type fixture struct {
	s       *Service
	prompts *fakePrompts
	logs    *dlog.TestSurfaces
	reg     fakeWorkspaces
}

// newFixture builds a service over a registry holding owner, source and
// requester (open), whose briefs splice their placeholders in a fixed order.
func newFixture() *fixture {
	f := &fixture{prompts: &fakePrompts{}, logs: dlog.NewTestSurfaces()}
	f.reg = fakeWorkspaces{byID: map[ids.WorkspaceID]wsm.Workspace{
		owner:     {ID: owner, Name: "owner"},
		source:    {ID: source, Name: "source"},
		requester: {ID: requester, Name: "asker"},
	}, failFor: map[ids.WorkspaceID]error{}}
	keys := 0
	f.s = New(Deps{
		Workspaces: f.reg,
		Briefs: func(name string, values map[string]string) (string, error) {
			switch name {
			case BriefDerived:
				return "derived|" + values["label"] + "|" + values["relation"] + "|" + values["event"], nil
			case BriefOwner:
				return "owner|" + values["child"] + "|" + values["requester"] + "|" + values["notice"], nil
			}
			return "", fmt.Errorf("unknown brief %q", name)
		},
		MintKey: func(prefix string) (string, error) {
			keys++
			return fmt.Sprintf("%s-%d", prefix, keys), nil
		},
		Log: f.logs,
	})
	return f
}

// deliverNow runs one event through the delivery path synchronously.
func (f *fixture) deliverNow(event Event) {
	f.s.deliver(context.Background(), f.prompts, event)
}

func derived(parent, contextFrom, requestedBy *ids.WorkspaceID, ownerLands bool) wsm.Workspace {
	return wsm.Workspace{ID: child, Name: "child", Branch: "b-child", Parent: parent, ContextFrom: contextFrom, RequestedBy: requestedBy, OwnerLands: ownerLands}
}

func TestRecipients(t *testing.T) {
	tests := []struct {
		name string
		ws   wsm.Workspace
		want []Recipient
	}{
		{name: "owner and a distinct context source are each told", ws: derived(idp(owner), idp(source), nil, true),
			want: []Recipient{{Source: owner, Owner: true}, {Source: source, Context: true}}},
		{name: "a workspace that is both sources is told once", ws: derived(idp(owner), idp(owner), nil, false),
			want: []Recipient{{Source: owner, Owner: true, Context: true}}},
		{name: "the requester is never told about its own act", ws: derived(idp(owner), idp(requester), idp(requester), true),
			want: []Recipient{{Source: owner, Owner: true}}},
		{name: "a fresh top-level workspace has nobody to tell", ws: derived(nil, nil, idp(requester), false)},
		{name: "a top-level fork tells its context source", ws: derived(nil, idp(source), nil, false),
			want: []Recipient{{Source: source, Context: true}}},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got := Recipients(tc.ws)

			// Assert
			if !reflect.DeepEqual(got, tc.want) {
				t.Errorf("Recipients = %+v, want %+v", got, tc.want)
			}
		})
	}
}

func TestDeliverTellsAnOwnerThatLandsTheOwnerNoticeAndTheContextSourceTheBriefOne(t *testing.T) {
	// Arrange
	f := newFixture()

	// Act
	f.deliverNow(Event{Kind: Created, Workspace: derived(idp(owner), idp(source), idp(requester), true), OwnerNotice: "fixing the parser"})

	// Assert
	want := []submission{
		{ws: owner, text: "owner|child|the workspace `asker`|fixing the parser", origin: OriginOwner, defer_: true},
		{ws: source, text: "derived|workspace `child`: created|was created with this conversation forked|It was just created, at branch `b-child`.", origin: OriginDerived, defer_: true},
	}
	if !reflect.DeepEqual(f.prompts.got, want) {
		t.Errorf("submissions = %+v, want %+v", f.prompts.got, want)
	}
}

func TestDeliverTellsEachEventKindBriefly(t *testing.T) {
	tests := []struct {
		kind  Kind
		label string
		event string
	}{
		{kind: Created, label: "created", event: "It was just created, at branch `b-child`."},
		{kind: Landed, label: "landed", event: "Its branch `b-child` just landed."},
		{kind: ClosedWithoutLanding, label: "closed without landing", event: "It just closed without its branch `b-child` landing."},
	}
	for _, tc := range tests {
		t.Run(tc.kind.String(), func(t *testing.T) {
			// Arrange
			f := newFixture()

			// Act: a self-landing child, so even its creation is the brief notice.
			f.deliverNow(Event{Kind: tc.kind, Workspace: derived(idp(owner), idp(owner), nil, false)})

			// Assert
			want := []submission{{ws: owner, origin: OriginDerived, defer_: true,
				text: "derived|workspace `child`: " + tc.label + "|was cut from this workspace's branch, with this conversation forked, and lands into this workspace|" + tc.event}}
			if !reflect.DeepEqual(f.prompts.got, want) {
				t.Errorf("submissions = %+v, want %+v", f.prompts.got, want)
			}
		})
	}
}

func TestDeliverLandingOfAnOwnerLandedChildIsTheBriefNotice(t *testing.T) {
	// Arrange
	f := newFixture()

	// Act
	f.deliverNow(Event{Kind: Landed, Workspace: derived(idp(owner), nil, nil, true)})

	// Assert
	if len(f.prompts.got) != 1 || f.prompts.got[0].origin != OriginDerived {
		t.Errorf("submissions = %+v, want one brief notice", f.prompts.got)
	}
}

func TestDeliverDoesNotTellAClosedSource(t *testing.T) {
	// Arrange
	f := newFixture()
	closed := f.reg.byID[source]
	closed.Closed = true
	f.reg.byID[source] = closed

	// Act
	f.deliverNow(Event{Kind: Created, Workspace: derived(nil, idp(source), nil, false)})

	// Assert
	if len(f.prompts.got) != 0 {
		t.Errorf("submissions = %+v, want none", f.prompts.got)
	}
	if _, ok := dlog.FindRecord(f.logs.Records(), dlog.RecordQuery{Level: "info", Operation: op, Message: "a derived workspace's source is closed; it runs no session to tell"}); !ok {
		t.Errorf("no record of the closed source: %+v", f.logs.Records())
	}
}

func TestDeliverFailures(t *testing.T) {
	tests := []struct {
		name    string
		arrange func(f *fixture)
		message string
	}{
		{name: "the source cannot be read", arrange: func(f *fixture) { f.reg.failFor[source] = errors.New("disk gone") },
			message: "could not read a derived workspace's source; it is not told"},
		{name: "the submission fails", arrange: func(f *fixture) { f.prompts.failFor = map[ids.WorkspaceID]error{source: errors.New("no session")} },
			message: "could not submit a derived workspace notice to its source"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			f := newFixture()
			tc.arrange(f)

			// Act
			f.deliverNow(Event{Kind: Created, Workspace: derived(nil, idp(source), nil, false)})

			// Assert
			if len(f.prompts.got) != 0 {
				t.Errorf("submissions = %+v, want none", f.prompts.got)
			}
			record, ok := dlog.FindRecord(f.logs.Records(), dlog.RecordQuery{Level: "error", Operation: op, Message: tc.message})
			if !ok {
				t.Fatalf("no error record %q: %+v", tc.message, f.logs.Records())
			}
			if record.Context["workspace"] != string(child) || record.Context["source"] != string(source) || record.Context["event"] != "created" {
				t.Errorf("record context = %+v, want the child, its source and the event", record.Context)
			}
		})
	}
}

func TestDeliverBriefFailureIsRecordedAndTellsNobody(t *testing.T) {
	// Arrange
	f := newFixture()
	f.s.deps.Briefs = func(string, map[string]string) (string, error) { return "", errors.New("brief missing") }

	// Act
	f.deliverNow(Event{Kind: Created, Workspace: derived(nil, idp(source), nil, false)})

	// Assert
	if len(f.prompts.got) != 0 {
		t.Errorf("submissions = %+v, want none", f.prompts.got)
	}
	if _, ok := dlog.FindRecord(f.logs.Records(), dlog.RecordQuery{Level: "error", Operation: op, Message: "could not compose a derived workspace notice; the source is not told"}); !ok {
		t.Errorf("no error record: %+v", f.logs.Records())
	}
}

func TestRunDeliversEverythingReportedBeforeItsContextEnds(t *testing.T) {
	// Arrange
	f := newFixture()
	ctx, cancel := context.WithCancel(context.Background())
	f.s.Report(Event{Kind: Created, Workspace: derived(nil, idp(source), nil, false)})
	f.s.Report(Event{Kind: Landed, Workspace: derived(nil, idp(source), nil, false)})
	cancel()

	// Act
	f.s.Run(ctx, f.prompts)

	// Assert
	if len(f.prompts.got) != 2 {
		t.Fatalf("submissions = %+v, want both events told", f.prompts.got)
	}
	if !strings.Contains(f.prompts.got[1].text, "landed") {
		t.Errorf("second notice = %q, want the landing", f.prompts.got[1].text)
	}
}

func TestReportAfterRunEndedIsRecordedAndNotTold(t *testing.T) {
	// Arrange
	f := newFixture()
	ctx, cancel := context.WithCancel(context.Background())
	cancel()
	f.s.Run(ctx, f.prompts)

	// Act
	f.s.Report(Event{Kind: Created, Workspace: derived(nil, idp(source), nil, false)})

	// Assert
	if len(f.prompts.got) != 0 {
		t.Errorf("submissions = %+v, want none", f.prompts.got)
	}
	if _, ok := dlog.FindRecord(f.logs.Records(), dlog.RecordQuery{Level: "info", Operation: op}); !ok {
		t.Errorf("no record of the untold event: %+v", f.logs.Records())
	}
}

// pendingKinds answers the kinds the service holds, undelivered.
func pendingKinds(s *Service) []Kind {
	var kinds []Kind
	ctx, cancel := context.WithCancel(context.Background())
	cancel()
	s.loop.Run(ctx, func(_ context.Context, e Event) { kinds = append(kinds, e.Kind) })
	return kinds
}

func TestTheEdgesReportOnlyTheDerivedEvents(t *testing.T) {
	landedAt := time.Unix(1, 0)
	tests := []struct {
		name string
		edge func(s *Service)
		want []Kind
	}{
		{name: "a turn starting reports nothing", edge: func(s *Service) { s.TurnStarted(child) }},
		{name: "a turn ending reports nothing", edge: func(s *Service) { s.TurnEnded(child) }},
		{name: "an ask opening reports nothing", edge: func(s *Service) { s.NeedsInput(child) }},
		{name: "a landing reports the landing", edge: func(s *Service) { s.Landed(child) }, want: []Kind{Landed}},
		{name: "a close after a landing reports nothing more", edge: func(s *Service) { s.Closed(wsm.Workspace{ID: child, MergedAt: &landedAt}) }},
		{name: "a close without a landing reports it", edge: func(s *Service) { s.Closed(wsm.Workspace{ID: child}) }, want: []Kind{ClosedWithoutLanding}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newFixture()

			// Act
			tt.edge(f.s)

			// Assert
			if got := pendingKinds(f.s); !slices.Equal(got, tt.want) {
				t.Errorf("reported = %v, want %v", got, tt.want)
			}
		})
	}
}

func TestALandingToldByIDReadsTheWorkspaceAtDelivery(t *testing.T) {
	// Arrange
	f := newFixture()
	f.reg.byID[child] = derived(idp(owner), nil, nil, false)

	// Act
	f.deliverNow(Event{Kind: Landed, Workspace: wsm.Workspace{ID: child}, unread: true})

	// Assert
	if len(f.prompts.got) != 1 || f.prompts.got[0].ws != owner {
		t.Fatalf("submitted = %+v, want the owner told", f.prompts.got)
	}
}

func TestALandingWhoseWorkspaceCannotBeReadTellsNobody(t *testing.T) {
	// Arrange
	f := newFixture()
	f.reg.failFor[child] = errors.New("db gone")

	// Act
	f.deliverNow(Event{Kind: Landed, Workspace: wsm.Workspace{ID: child}, unread: true})

	// Assert
	if len(f.prompts.got) != 0 {
		t.Fatalf("submitted = %+v, want nobody told", f.prompts.got)
	}
	if _, ok := dlog.FindRecord(f.logs.Records(), dlog.RecordQuery{Level: "error", Operation: op, Message: "could not read the workspace a derived event is about; its sources are not told"}); !ok {
		t.Errorf("no error record: %+v", f.logs.Records())
	}
}
