package sessiondrv

import (
	"errors"
	"sync"
	"testing"

	corev1 "agentrepl/proto/agentshim/core/v1"
	datav1 "agentrepl/proto/agentshim/data/v1"
	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"google.golang.org/protobuf/types/known/anypb"
)

// fakePusher records every frontend push for assertions.
type fakePusher struct {
	mu         sync.Mutex
	convo      []*frontendv1.ConversationDelta
	typing     []*frontendv1.TypingDelta
	catalog    []*frontendv1.TaskCatalog
	degraded   []*frontendv1.DegradedNotice
	state      []*frontendv1.WorkspaceState
	inits      []*frontendv1.SessionInitView
	heartbeats []*frontendv1.HeartbeatView
	queues     []*frontendv1.QueueView
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
func (p *fakePusher) PushSessionInitView(v *frontendv1.SessionInitView) {
	p.mu.Lock()
	p.inits = append(p.inits, v)
	p.mu.Unlock()
}
func (p *fakePusher) PushHeartbeatView(h *frontendv1.HeartbeatView) {
	p.mu.Lock()
	p.heartbeats = append(p.heartbeats, h)
	p.mu.Unlock()
}
func (p *fakePusher) PushQueueView(q *frontendv1.QueueView) {
	p.mu.Lock()
	p.queues = append(p.queues, q)
	p.mu.Unlock()
}

// queueViews returns a copy of the recorded queue pushes.
func (p *fakePusher) queueViews() []*frontendv1.QueueView {
	p.mu.Lock()
	defer p.mu.Unlock()
	return append([]*frontendv1.QueueView(nil), p.queues...)
}

// lastQueue returns the most recent queue push, or nil when none landed.
func (p *fakePusher) lastQueue() *frontendv1.QueueView {
	p.mu.Lock()
	defer p.mu.Unlock()
	if len(p.queues) == 0 {
		return nil
	}
	return p.queues[len(p.queues)-1]
}

// permissionResolutions extracts, in push order, the resolution of every
// permission ConversationItem keyed by uuid across the recorded deltas.
func (p *fakePusher) permissionResolutions(uuid string) []corev1.PermissionItem_Resolution {
	p.mu.Lock()
	defer p.mu.Unlock()
	var out []corev1.PermissionItem_Resolution
	for _, d := range p.convo {
		for _, it := range d.GetItems() {
			if it.GetUuid() == uuid {
				if pi := it.GetPermission(); pi != nil {
					out = append(out, pi.GetResolution())
				}
			}
		}
	}
	return out
}

// lastPermissionDenyMessage returns the deny_message of the last permission
// item keyed by uuid across the recorded deltas ("" when none carried one).
func lastPermissionDenyMessage(p *fakePusher, uuid string) string {
	p.mu.Lock()
	defer p.mu.Unlock()
	msg := ""
	for _, d := range p.convo {
		for _, it := range d.GetItems() {
			if it.GetUuid() == uuid {
				if pi := it.GetPermission(); pi != nil {
					msg = pi.GetDenyMessage()
				}
			}
		}
	}
	return msg
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
	c := newConsumer("ws", "s1", push, applier, nil, nil, nil, nil)
	c.now = func() int64 { return 1000 }
	return c
}

// fakeProgress records what the consumer folds into the progress resolver.
type fakeProgress struct {
	applied    []*corev1.Event
	workspaces []string
	err        error
}

func (p *fakeProgress) Apply(workspace string, ev *corev1.Event) error {
	p.workspaces = append(p.workspaces, workspace)
	p.applied = append(p.applied, ev)
	return p.err
}
func (p *fakeProgress) SetCounts(string, int64, int64)  {}
func (p *fakeProgress) NoteTurnAccepted(string, string) {}

func newProgressConsumer(prog ProgressResolver) *consumer {
	c := newConsumer("ws", "s1", &fakePusher{}, &fakeApplier{}, prog, nil, nil, nil)
	c.now = func() int64 { return 1000 }
	return c
}

func TestLifecycleEventsReachTheProgressResolver(t *testing.T) {
	// Arrange
	prog := &fakeProgress{}
	c := newProgressConsumer(prog)
	// Act — the lifecycle plane carries the turn boundaries the footer clocks.
	c.Apply(&corev1.Event{
		SessionId: "s1",
		Payload:   &corev1.Event_TurnStarted{TurnStarted: &corev1.TurnStarted{}},
	})
	// Assert
	if len(prog.applied) != 1 {
		t.Fatalf("progress applied %d events, want 1", len(prog.applied))
	}
	if prog.workspaces[0] != "ws" {
		t.Fatalf("progress workspace = %q, want %q", prog.workspaces[0], "ws")
	}
}

func TestDataEventsReachTheProgressResolver(t *testing.T) {
	// Arrange
	prog := &fakeProgress{}
	c := newProgressConsumer(prog)
	// Act — the data plane carries the tickers and windows.
	c.Consume(&corev1.Event{
		SessionId: "s1",
		Payload:   &corev1.Event_ContentDelta{ContentDelta: &corev1.ContentDelta{Uuid: "u1"}},
	})
	// Assert
	if len(prog.applied) != 1 {
		t.Fatalf("progress applied %d events, want 1", len(prog.applied))
	}
}

func TestProgressFoldFailureDoesNotStopTheStream(t *testing.T) {
	// Arrange — the resolver rejects everything.
	prog := &fakeProgress{err: errors.New("boom")}
	push := &fakePusher{}
	c := newConsumer("ws", "s1", push, &fakeApplier{}, prog, nil, nil, nil)
	c.now = func() int64 { return 1000 }
	// Act
	c.Consume(&corev1.Event{
		SessionId: "s1",
		Payload:   &corev1.Event_ContentDelta{ContentDelta: &corev1.ContentDelta{Uuid: "u1", Delta: &corev1.ContentDelta_Text{Text: "hi"}}},
	})
	// Assert — the footer degrading is not a reason to stop delivering
	// conversation, so the typing relay still went out.
	if len(push.typing) != 1 {
		t.Fatalf("typing pushes = %d, want 1 despite the progress fold failing", len(push.typing))
	}
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

	// Assert: the ContentDelta is embedded in the TypingDelta unchanged (S9).
	if len(push.typing) != 1 {
		t.Fatalf("expected 1 typing push, got %d", len(push.typing))
	}
	got := push.typing[0].GetDelta()
	if got.GetUuid() != "u1" || got.GetText() != "hi" {
		t.Errorf("embedded content delta: got uuid=%q text=%q, want u1/hi", got.GetUuid(), got.GetText())
	}
}

func TestResyncReplaysLatestPermissionItem(t *testing.T) {
	// Arrange: a permission goes pending then allowed on the same request_id.
	push := &fakePusher{}
	c := newTestConsumer(push, &fakeApplier{})
	req := &corev1.PermissionRequest{RequestId: "r1", ToolName: "Bash"}
	c.pushPermission(permissionItem(req, corev1.PermissionItem_RESOLUTION_PENDING, ""))
	c.pushPermission(permissionItem(req, corev1.PermissionItem_RESOLUTION_ALLOWED, ""))
	push.mu.Lock()
	push.convo = nil // drop the live pushes; only the resync replay should remain
	push.mu.Unlock()

	// Act.
	c.resync(0)

	// Assert: exactly one replay carrying the LATEST resolution (allowed).
	got := push.permissionResolutions("r1")
	if len(got) != 1 || got[0] != corev1.PermissionItem_RESOLUTION_ALLOWED {
		t.Fatalf("resync replay resolutions = %v, want [ALLOWED]", got)
	}
}

func TestConsumeHeartbeatProgressPushesHeartbeatView(t *testing.T) {
	// Arrange: under S9 this event had no frontend.v1 arm and was dropped; E4
	// added HeartbeatView, so it must now reach the frontend.
	push := &fakePusher{}
	c := newTestConsumer(push, &fakeApplier{})

	// Act.
	c.Consume(&corev1.Event{
		SessionId: "s1",
		Payload:   &corev1.Event_HeartbeatProgress{HeartbeatProgress: &corev1.HeartbeatProgress{ToolUseId: "tu1", ElapsedSeconds: 12}},
	})

	// Assert.
	if len(push.heartbeats) != 1 {
		t.Fatalf("expected 1 heartbeat push, got %d", len(push.heartbeats))
	}
}

func TestConsumeHeartbeatProgressEmbedsProgressUnchanged(t *testing.T) {
	// Arrange: the relay must carry the core.v1 payload verbatim, exactly as
	// TypingDelta carries its ContentDelta — this layer never re-types.
	push := &fakePusher{}
	c := newTestConsumer(push, &fakeApplier{})

	// Act.
	c.Consume(&corev1.Event{
		SessionId: "s1",
		Payload: &corev1.Event_HeartbeatProgress{HeartbeatProgress: &corev1.HeartbeatProgress{
			ToolUseId:       "tu1",
			ToolName:        "Bash",
			ParentToolUseId: "tu0",
			ElapsedSeconds:  12.5,
		}},
	})

	// Assert.
	got := push.heartbeats[0].GetProgress()
	if got.GetToolUseId() != "tu1" || got.GetToolName() != "Bash" ||
		got.GetParentToolUseId() != "tu0" || got.GetElapsedSeconds() != 12.5 {
		t.Errorf("embedded progress = %+v, want tu1/Bash/tu0/12.5", got)
	}
}

func TestConsumeHeartbeatProgressStampsWorkspaceAndSession(t *testing.T) {
	// Arrange: the relay is scope-filtered per connection, so it must carry the
	// identity scopeFrame keys on.
	push := &fakePusher{}
	c := newTestConsumer(push, &fakeApplier{})

	// Act.
	c.Consume(&corev1.Event{
		SessionId: "s1",
		Payload:   &corev1.Event_HeartbeatProgress{HeartbeatProgress: &corev1.HeartbeatProgress{ToolUseId: "tu1"}},
	})

	// Assert.
	hv := push.heartbeats[0]
	if hv.GetWorkspace() != "ws" || hv.GetSessionId() != "s1" {
		t.Errorf("heartbeat identity = %q/%q, want ws/s1", hv.GetWorkspace(), hv.GetSessionId())
	}
}

func TestConsumeHeartbeatProgressPushesNothingForNilProgress(t *testing.T) {
	// Arrange: a heartbeat arm with no payload must push nothing rather than an
	// empty frame the frontend would have to defend against.
	push := &fakePusher{}
	c := newTestConsumer(push, &fakeApplier{})

	// Act.
	c.Consume(&corev1.Event{
		SessionId: "s1",
		Payload:   &corev1.Event_HeartbeatProgress{HeartbeatProgress: nil},
	})

	// Assert.
	if len(push.heartbeats) != 0 {
		t.Fatalf("expected no heartbeat push for nil progress, got %d", len(push.heartbeats))
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
	c := newConsumer("ws", "s1", &fakePusher{}, &fakeApplier{}, nil, nil, func(ss *corev1.SessionStarted) { seen = ss }, nil)

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
