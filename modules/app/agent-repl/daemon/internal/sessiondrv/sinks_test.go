package sessiondrv

import (
	"sync"
	"testing"

	corev1 "agentrepl/proto/agentshim/core/v1"
	datav1 "agentrepl/proto/agentshim/data/v1"
	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"google.golang.org/protobuf/types/known/anypb"
)

// fakePusher records every frontend push for assertions.
type fakePusher struct {
	mu       sync.Mutex
	convo    []*frontendv1.ConversationDelta
	typing   []*frontendv1.TypingDelta
	catalog  []*frontendv1.TaskCatalog
	degraded []*frontendv1.DegradedNotice
	state    []*frontendv1.WorkspaceState
}

func (p *fakePusher) PushConversationDelta(c *frontendv1.ConversationDelta) {
	p.mu.Lock()
	p.convo = append(p.convo, c)
	p.mu.Unlock()
}
func (p *fakePusher) PushTypingDelta(t *frontendv1.TypingDelta) {
	p.mu.Lock()
	p.typing = append(p.typing, t)
	p.mu.Unlock()
}
func (p *fakePusher) PushTaskCatalog(c *frontendv1.TaskCatalog) {
	p.mu.Lock()
	p.catalog = append(p.catalog, c)
	p.mu.Unlock()
}
func (p *fakePusher) PushDegradedNotice(n *frontendv1.DegradedNotice) {
	p.mu.Lock()
	p.degraded = append(p.degraded, n)
	p.mu.Unlock()
}
func (p *fakePusher) PushWorkspaceState(w *frontendv1.WorkspaceState) {
	p.mu.Lock()
	p.state = append(p.state, w)
	p.mu.Unlock()
}

// fakeApplier records applied events and optionally returns an error.
type fakeApplier struct {
	applied []*corev1.Event
	err     error
}

func (a *fakeApplier) Apply(ev *corev1.Event) error {
	a.applied = append(a.applied, ev)
	return a.err
}

func newTestConsumer(push Pusher, applier StateApplier) *consumer {
	c := newConsumer("ws", "s1", push, applier, nil, nil)
	c.now = func() int64 { return 1000 }
	return c
}

func TestConsumeContentDeltaPushesTyping(t *testing.T) {
	// Arrange.
	push := &fakePusher{}
	c := newTestConsumer(push, &fakeApplier{})

	// Act.
	c.Consume(&corev1.Event{
		SessionId: "s1",
		Payload:   &corev1.Event_ContentDelta{ContentDelta: &corev1.ContentDelta{Uuid: "u1", Delta: &corev1.ContentDelta_Text{Text: "hi"}}},
	})

	// Assert.
	if len(push.typing) != 1 {
		t.Fatalf("expected 1 typing push, got %d", len(push.typing))
	}
	if got := push.typing[0]; got.GetKind() != "text" || got.GetDelta() != "hi" {
		t.Errorf("typing delta: got kind=%q delta=%q, want text/hi", got.GetKind(), got.GetDelta())
	}
}

func TestConsumeHeartbeatProgressPushesProgressTyping(t *testing.T) {
	// Arrange.
	push := &fakePusher{}
	c := newTestConsumer(push, &fakeApplier{})

	// Act.
	c.Consume(&corev1.Event{
		SessionId: "s1",
		Payload:   &corev1.Event_HeartbeatProgress{HeartbeatProgress: &corev1.HeartbeatProgress{ToolUseId: "tu1", ElapsedSeconds: 12}},
	})

	// Assert.
	if len(push.typing) != 1 {
		t.Fatalf("expected 1 typing push, got %d", len(push.typing))
	}
	if got := push.typing[0]; got.GetKind() != "progress" || got.GetUuid() != "tu1" {
		t.Errorf("progress typing: got kind=%q uuid=%q, want progress/tu1", got.GetKind(), got.GetUuid())
	}
}

func TestConsumeVendorPushesConversationDeltaWithThroughSeq(t *testing.T) {
	// Arrange.
	push := &fakePusher{}
	c := newTestConsumer(push, &fakeApplier{})
	assistant := &datav1.ClaudeStreamMessage{
		Msg: &datav1.ClaudeStreamMessage_Assistant{Assistant: &datav1.AssistantMessage{
			Uuid:    "u1",
			Message: &datav1.ApiAssistantMessage{Content: []*datav1.ContentBlock{{Block: &datav1.ContentBlock_Text{Text: &datav1.TextBlock{Text: "hello"}}}}},
		}},
	}
	any, err := anypb.New(assistant)
	if err != nil {
		t.Fatalf("anypb.New: %v", err)
	}

	// Act.
	c.Consume(&corev1.Event{SessionId: "s1", Seq: 9, Payload: &corev1.Event_Vendor{Vendor: any}})

	// Assert.
	if len(push.convo) != 1 {
		t.Fatalf("expected 1 conversation push, got %d", len(push.convo))
	}
	if got := push.convo[0].GetThroughSeq(); got != 9 {
		t.Errorf("through_seq: got %d, want 9", got)
	}
}

func TestApplyForwardsToSSMAndRefreshesTaskCatalogOnTaskEvents(t *testing.T) {
	// Arrange.
	push := &fakePusher{}
	applier := &fakeApplier{}
	c := newTestConsumer(push, applier)

	// Act.
	c.Apply(&corev1.Event{SessionId: "s1", Seq: 1, Payload: &corev1.Event_TaskStarted{TaskStarted: &corev1.TaskStarted{TaskId: "t1"}}})

	// Assert.
	if len(applier.applied) != 1 {
		t.Fatalf("expected 1 SSM apply, got %d", len(applier.applied))
	}
	if len(push.catalog) != 1 {
		t.Fatalf("expected 1 task-catalog push on a task event, got %d", len(push.catalog))
	}
}

func TestApplyNonTaskEventDoesNotPushCatalog(t *testing.T) {
	// Arrange.
	push := &fakePusher{}
	c := newTestConsumer(push, &fakeApplier{})

	// Act.
	c.Apply(&corev1.Event{SessionId: "s1", Seq: 1, Payload: &corev1.Event_TurnStarted{TurnStarted: &corev1.TurnStarted{}}})

	// Assert.
	if len(push.catalog) != 0 {
		t.Fatalf("turn event must not refresh the task catalog; got %d pushes", len(push.catalog))
	}
}

func TestApplyFiresOnSessionStarted(t *testing.T) {
	// Arrange.
	var seen *corev1.SessionStarted
	c := newConsumer("ws", "s1", &fakePusher{}, &fakeApplier{}, nil, func(ss *corev1.SessionStarted) { seen = ss })

	// Act.
	c.Apply(&corev1.Event{SessionId: "s1", Payload: &corev1.Event_SessionStarted{SessionStarted: &corev1.SessionStarted{Source: corev1.SessionSource_SESSION_SOURCE_RESUME}}})

	// Assert.
	if seen == nil || seen.GetSource() != corev1.SessionSource_SESSION_SOURCE_RESUME {
		t.Fatal("onSessionStarted must fire with the SessionStarted payload")
	}
}

func TestDegradedReporterPushesNotices(t *testing.T) {
	// Arrange.
	push := &fakePusher{}
	c := newTestConsumer(push, &fakeApplier{})

	// Act.
	c.Degraded("s1", &corev1.DegradedState{Component: "store", Reason: "down"})
	c.ConnectionDegraded("s1", "no traffic")
	c.ConnectionRecovered("s1")

	// Assert.
	if len(push.degraded) != 3 {
		t.Fatalf("expected 3 degraded pushes, got %d", len(push.degraded))
	}
	if push.degraded[0].GetComponent() != "store" {
		t.Errorf("first degraded component: got %q, want store", push.degraded[0].GetComponent())
	}
	if push.degraded[1].GetComponent() != "shim-connection" || push.degraded[1].GetRecovered() {
		t.Errorf("connection-degraded notice malformed: %+v", push.degraded[1])
	}
	if !push.degraded[2].GetRecovered() {
		t.Error("connection-recovered notice must set recovered=true")
	}
}

func TestResyncReplaysRetainedConversationDeltas(t *testing.T) {
	// Arrange: consume one vendor event, then resync from 0.
	push := &fakePusher{}
	c := newTestConsumer(push, &fakeApplier{})
	assistant := &datav1.ClaudeStreamMessage{Msg: &datav1.ClaudeStreamMessage_Assistant{Assistant: &datav1.AssistantMessage{
		Uuid:    "u1",
		Message: &datav1.ApiAssistantMessage{Content: []*datav1.ContentBlock{{Block: &datav1.ContentBlock_Text{Text: &datav1.TextBlock{Text: "x"}}}}},
	}}}
	any, err := anypb.New(assistant)
	if err != nil {
		t.Fatalf("anypb.New: %v", err)
	}
	c.Consume(&corev1.Event{SessionId: "s1", Seq: 5, Payload: &corev1.Event_Vendor{Vendor: any}})

	// Act.
	c.resync(0)

	// Assert: one live push + one replayed push = 2 total for the same event.
	if len(push.convo) != 2 {
		t.Fatalf("expected 2 conversation pushes (live + replay), got %d", len(push.convo))
	}
}

func TestResyncRespectsFromSeq(t *testing.T) {
	// Arrange.
	push := &fakePusher{}
	c := newTestConsumer(push, &fakeApplier{})
	mk := func(seq uint64) *corev1.Event {
		a, err := anypb.New(&datav1.ClaudeStreamMessage{Msg: &datav1.ClaudeStreamMessage_Assistant{Assistant: &datav1.AssistantMessage{
			Uuid:    "u",
			Message: &datav1.ApiAssistantMessage{Content: []*datav1.ContentBlock{{Block: &datav1.ContentBlock_Text{Text: &datav1.TextBlock{Text: "x"}}}}},
		}}})
		if err != nil {
			t.Fatalf("anypb.New: %v", err)
		}
		return &corev1.Event{SessionId: "s1", Seq: seq, Payload: &corev1.Event_Vendor{Vendor: a}}
	}
	c.retain(mk(3))
	c.retain(mk(7))

	// Act.
	c.resync(5)

	// Assert: only seq>=5 replays.
	if len(push.convo) != 1 {
		t.Fatalf("expected 1 replayed delta (seq>=5), got %d", len(push.convo))
	}
	if push.convo[0].GetThroughSeq() != 7 {
		t.Errorf("replayed through_seq: got %d, want 7", push.convo[0].GetThroughSeq())
	}
}
