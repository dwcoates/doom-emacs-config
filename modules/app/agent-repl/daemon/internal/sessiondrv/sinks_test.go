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
	// reconciled records one entry per ReconcileTasks call, as
	// (sessionID, liveTaskIDs).
	reconciled  []reconcileCall
	reconcErr   error
	reconcMutex sync.Mutex
	// backfills records one entry per ApplyBackfillState call, as the
	// (workspace, state) pair the driver pushed onto the SSM's backfill axis.
	backfills []backfillCall
}

// backfillCall is one backfill outcome the driver applied to the SSM.
type backfillCall struct {
	workspace string
	state     string
}

func (f *fakeApplier) ApplyBackfillState(workspace, state string) error {
	f.reconcMutex.Lock()
	defer f.reconcMutex.Unlock()
	f.backfills = append(f.backfills, backfillCall{workspace: workspace, state: state})
	return nil
}

// reconcileCall is one authoritative live-task set the driver adopted.
type reconcileCall struct {
	sessionID string
	taskIDs   []string
}

func (a *fakeApplier) Apply(ev *corev1.Event) error {
	a.applied = append(a.applied, ev)
	return a.err
}

func (a *fakeApplier) ReconcileTasks(sessionID string, liveTaskIDs []string) error {
	a.reconcMutex.Lock()
	a.reconciled = append(a.reconciled, reconcileCall{sessionID: sessionID, taskIDs: liveTaskIDs})
	a.reconcMutex.Unlock()
	return a.reconcErr
}

// reconcileCalls returns a copy of the recorded reconciliations.
func (a *fakeApplier) reconcileCalls() []reconcileCall {
	a.reconcMutex.Lock()
	defer a.reconcMutex.Unlock()
	return append([]reconcileCall(nil), a.reconciled...)
}

func newTestConsumer(push Pusher, applier StateApplier) *consumer {
	c := newConsumer("ws", "s1", push, applier, nil, nil, nil, nil, nil)
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
	c := newConsumer("ws", "s1", &fakePusher{}, &fakeApplier{}, prog, nil, nil, nil, nil)
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

func TestMessageLatencyReachesTheProgressResolver(t *testing.T) {
	// Arrange
	prog := &fakeProgress{}
	c := newProgressConsumer(prog)
	// Act — the ttft relay is a progress fact and nothing else.
	c.Consume(&corev1.Event{
		SessionId: "s1",
		Payload:   &corev1.Event_MessageLatency{MessageLatency: &corev1.MessageLatency{Uuid: "m1", TtftMs: 865}},
	})
	// Assert
	if len(prog.applied) != 1 {
		t.Fatalf("progress applied %d events, want 1", len(prog.applied))
	}
}

func TestMessageLatencyPushesNoConversationFrame(t *testing.T) {
	// Arrange — it is footer input, not conversation content.
	push := &fakePusher{}
	c := newTestConsumer(push, &fakeApplier{})
	// Act
	c.Consume(&corev1.Event{
		SessionId: "s1",
		Payload:   &corev1.Event_MessageLatency{MessageLatency: &corev1.MessageLatency{Uuid: "m1", TtftMs: 865}},
	})
	// Assert
	if n := len(push.typing) + len(push.convo) + len(push.heartbeats); n != 0 {
		t.Fatalf("frontend pushes = %d, want 0 for a latency relay", n)
	}
}

// --- F2: the never-blue backfill derivation ---------------------------------

// backfillConsumer builds a consumer recording its backfill transitions.
func backfillConsumer(states *[]string) *consumer {
	c := newConsumer("ws", "s1", &fakePusher{}, &fakeApplier{}, nil, nil, nil, nil,
		func(state string) { *states = append(*states, state) })
	c.now = func() int64 { return 1000 }
	return c
}

// transcriptEvent wraps a file-plane TranscriptLine as a vendor event.
func transcriptEvent(t *testing.T) *corev1.Event {
	t.Helper()
	a, err := anypb.New(&datav1.TranscriptLine{})
	if err != nil {
		t.Fatalf("anypb.New: %v", err)
	}
	return &corev1.Event{SessionId: "s1", Payload: &corev1.Event_Vendor{Vendor: a}}
}

// streamEventOf wraps a stream-plane ClaudeStreamMessage as a vendor event.
func streamEventOf(t *testing.T) *corev1.Event {
	t.Helper()
	a, err := anypb.New(&datav1.ClaudeStreamMessage{})
	if err != nil {
		t.Fatalf("anypb.New: %v", err)
	}
	return &corev1.Event{SessionId: "s1", Payload: &corev1.Event_Vendor{Vendor: a}}
}

func TestFilePlaneEventMarksTheBackfillDone(t *testing.T) {
	// Arrange
	var states []string
	c := backfillConsumer(&states)
	// Act — a TranscriptLine means the file plane really delivered.
	c.Consume(transcriptEvent(t))
	// Assert
	if len(states) != 1 || states[0] != BackfillDone {
		t.Fatalf("backfill states = %v, want [done]", states)
	}
}

func TestStreamPlaneEventProvesNothingAboutTheBackfill(t *testing.T) {
	// Arrange
	var states []string
	c := backfillConsumer(&states)
	// Act — a live turn writes stream events for a session whose history never
	// arrived; that is PRECISELY the blue-but-live case this signal exists for.
	c.Consume(streamEventOf(t))
	// Assert
	if len(states) != 0 {
		t.Fatalf("backfill states = %v, want none from a stream-plane event", states)
	}
}

func TestBackfillDoneIsReportedOnlyOnce(t *testing.T) {
	// Arrange — a long transcript is many events.
	var states []string
	c := backfillConsumer(&states)
	// Act
	for i := 0; i < 5; i++ {
		c.Consume(transcriptEvent(t))
	}
	// Assert — the in-memory latch is what keeps this off the registry record.
	if len(states) != 1 {
		t.Fatalf("backfill states = %v, want exactly one write", states)
	}
}

func TestSidecarUnparsedEventMarksTheBackfillFailed(t *testing.T) {
	// Arrange
	var states []string
	c := backfillConsumer(&states)
	// Act — the one sidecar read failure that reaches the daemon durably.
	c.Consume(&corev1.Event{
		SessionId: "s1",
		Payload: &corev1.Event_Unparsed{Unparsed: &corev1.UnparsedEvent{
			Producer: sidecarProducer, SourcePath: "/p/u.jsonl", ByteOffset: 42, Error: "bad json",
		}},
	})
	// Assert
	if len(states) != 1 || states[0] != BackfillFailed {
		t.Fatalf("backfill states = %v, want [failed]", states)
	}
}

func TestAnUnparsedEventFromTheSHIMIsNotABackfillFailure(t *testing.T) {
	// Arrange — the stream plane's own producer says nothing about the
	// transcript read.
	var states []string
	c := backfillConsumer(&states)
	// Act
	c.Consume(&corev1.Event{
		SessionId: "s1",
		Payload: &corev1.Event_Unparsed{Unparsed: &corev1.UnparsedEvent{
			Producer: "claude-shim", Error: "bad json",
		}},
	})
	// Assert
	if len(states) != 0 {
		t.Fatalf("backfill states = %v, want none for a shim-produced unparsed event", states)
	}
}

func TestBackfillFailedIsTerminalForTheSession(t *testing.T) {
	// Arrange — a transcript the sidecar could not fully read.
	var states []string
	c := backfillConsumer(&states)
	c.Consume(&corev1.Event{
		SessionId: "s1",
		Payload: &corev1.Event_Unparsed{Unparsed: &corev1.UnparsedEvent{
			Producer: sidecarProducer, Error: "bad json",
		}},
	})
	// Act — later lines of the SAME transcript still arrive fine.
	c.Consume(transcriptEvent(t))
	// Assert — letting a good line flip it back to DONE would hide exactly the
	// partial-history case this signal exists to surface.
	if len(states) != 1 || states[0] != BackfillFailed {
		t.Fatalf("backfill states = %v, want [failed] to be terminal", states)
	}
}

func TestProgressFoldFailureDoesNotStopTheStream(t *testing.T) {
	// Arrange — the resolver rejects everything.
	prog := &fakeProgress{err: errors.New("boom")}
	push := &fakePusher{}
	c := newConsumer("ws", "s1", push, &fakeApplier{}, prog, nil, nil, nil, nil)
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
	c := newConsumer("ws", "s1", &fakePusher{}, &fakeApplier{}, nil, nil, func(ss *corev1.SessionStarted) { seen = ss }, nil, nil)

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

func TestResyncReportsTheRingFloor(t *testing.T) {
	// Arrange — the oldest retained seq is what the ring replay could cover.
	push := &fakePusher{}
	c := newTestConsumer(push, &fakeApplier{})
	c.retain(&corev1.Event{SessionId: "s1", Seq: 6108})
	c.retain(&corev1.Event{SessionId: "s1", Seq: 7117})

	// Act.
	floor, haveFloor := c.resync(0)

	// Assert.
	if !haveFloor || floor != 6108 {
		t.Fatalf("resync floor = (%d, %v), want (6108, true)", floor, haveFloor)
	}
}

func TestResyncReportsNoFloorForAnEmptyRing(t *testing.T) {
	// Arrange — a freshly restarted daemon has retained nothing yet, so the
	// ring cannot say where the live window begins.
	c := newTestConsumer(&fakePusher{}, &fakeApplier{})

	// Act.
	_, haveFloor := c.resync(0)

	// Assert.
	if haveFloor {
		t.Fatal("an empty ring must report no floor, not a floor of 0")
	}
}

func TestResyncIgnoresSeqlessItemsWhenReportingTheFloor(t *testing.T) {
	// Arrange — a daemon-composed permission item carries no store seq, so it
	// says nothing about how far back the ring reaches.
	c := newTestConsumer(&fakePusher{}, &fakeApplier{})
	c.retain(&corev1.Event{SessionId: "s1", Seq: 0})
	c.retain(&corev1.Event{SessionId: "s1", Seq: 42})

	// Act.
	floor, haveFloor := c.resync(0)

	// Assert.
	if !haveFloor || floor != 42 {
		t.Fatalf("resync floor = (%d, %v), want (42, true)", floor, haveFloor)
	}
}

// vendorEvent wraps a ClaudeStreamMessage as a vendor core Event.
func vendorEvent(t *testing.T, csm *datav1.ClaudeStreamMessage, seq uint64) *corev1.Event {
	t.Helper()
	a, err := anypb.New(csm)
	if err != nil {
		t.Fatalf("anypb.New: %v", err)
	}
	return &corev1.Event{SessionId: "s1", Seq: seq, Payload: &corev1.Event_Vendor{Vendor: a}}
}

func initEvent(t *testing.T, seq uint64, commands ...string) *corev1.Event {
	t.Helper()
	return vendorEvent(t, &datav1.ClaudeStreamMessage{Msg: &datav1.ClaudeStreamMessage_SystemInit{
		SystemInit: &datav1.SystemInit{Model: "opus", SlashCommands: commands},
	}}, seq)
}

func commandsChangedEvent(t *testing.T, seq uint64, names ...string) *corev1.Event {
	t.Helper()
	cmds := make([]*datav1.SlashCommandRef, 0, len(names))
	for _, n := range names {
		cmds = append(cmds, &datav1.SlashCommandRef{Name: n})
	}
	return vendorEvent(t, &datav1.ClaudeStreamMessage{Msg: &datav1.ClaudeStreamMessage_CommandsChanged{
		CommandsChanged: &datav1.CommandsChanged{Commands: cmds},
	}}, seq)
}

func TestCommandsChangedRepublishesSessionInitView(t *testing.T) {
	// Arrange: an init has landed, so there is a snapshot to refresh.
	push := &fakePusher{}
	c := newTestConsumer(push, &fakeApplier{})
	c.Consume(initEvent(t, 1, "compact"))

	// Act: the CLI discovers a skill mid-session.
	c.Consume(commandsChangedEvent(t, 2, "compact", "brand-new-skill"))

	// Assert: a second SessionInitView carries the replaced list.
	if len(push.inits) != 2 {
		t.Fatalf("expected 2 SessionInitView pushes, got %d", len(push.inits))
	}
	got := push.inits[1].GetInit().GetSlashCommands()
	if len(got) != 2 || got[1] != "brand-new-skill" {
		t.Errorf("refreshed slash commands: got %v, want [compact brand-new-skill]", got)
	}
}

func TestCommandsChangedReplacesRatherThanMerges(t *testing.T) {
	// Arrange: the SDK contract for this push is REPLACE -- the payload is
	// the complete current list, so a command that vanished must vanish.
	push := &fakePusher{}
	c := newTestConsumer(push, &fakeApplier{})
	c.Consume(initEvent(t, 1, "compact", "debug-logs"))

	// Act.
	c.Consume(commandsChangedEvent(t, 2, "compact"))

	// Assert.
	got := push.inits[1].GetInit().GetSlashCommands()
	if len(got) != 1 || got[0] != "compact" {
		t.Errorf("replaced slash commands: got %v, want [compact]", got)
	}
}

func TestCommandsChangedDoesNotMutateThePublishedSnapshot(t *testing.T) {
	// Arrange: frontends still hold the pointer from the first push, so the
	// refresh must clone rather than rewrite history under them.
	push := &fakePusher{}
	c := newTestConsumer(push, &fakeApplier{})
	c.Consume(initEvent(t, 1, "compact"))

	// Act.
	c.Consume(commandsChangedEvent(t, 2, "other"))

	// Assert: the FIRST view still reports what it reported when pushed.
	first := push.inits[0].GetInit().GetSlashCommands()
	if len(first) != 1 || first[0] != "compact" {
		t.Errorf("first SessionInitView was mutated: got %v, want [compact]", first)
	}
}

func TestCommandsChangedBeforeInitPushesNothing(t *testing.T) {
	// Arrange: no init has landed, so there is no snapshot to fold into.
	push := &fakePusher{}
	c := newTestConsumer(push, &fakeApplier{})

	// Act.
	c.Consume(commandsChangedEvent(t, 1, "compact"))

	// Assert: dropped (loud-logged), never a half-built init view.
	if len(push.inits) != 0 {
		t.Fatalf("expected no SessionInitView push, got %d", len(push.inits))
	}
}

// --- BackgroundTasksChanged: the authoritative live-task set ----------------

// backgroundTasksEvent wraps a live-set snapshot as a vendor core Event.
func backgroundTasksEvent(t *testing.T, seq uint64, taskIDs ...string) *corev1.Event {
	t.Helper()
	refs := make([]*datav1.BackgroundTaskRef, 0, len(taskIDs))
	for _, id := range taskIDs {
		refs = append(refs, &datav1.BackgroundTaskRef{TaskId: id})
	}
	a, err := anypb.New(&datav1.ClaudeStreamMessage{
		Msg: &datav1.ClaudeStreamMessage_BackgroundTasksChanged{
			BackgroundTasksChanged: &datav1.BackgroundTasksChanged{Tasks: refs},
		},
	})
	if err != nil {
		t.Fatalf("anypb.New: %v", err)
	}
	return &corev1.Event{SessionId: "s1", Seq: seq, Payload: &corev1.Event_Vendor{Vendor: a}}
}

func TestBackgroundTasksChangedReconcilesTheSSM(t *testing.T) {
	// Arrange
	applier := &fakeApplier{}
	c := newTestConsumer(&fakePusher{}, applier)

	// Act.
	c.Consume(backgroundTasksEvent(t, 5, "a1", "b1"))

	// Assert.
	calls := applier.reconcileCalls()
	if len(calls) != 1 {
		t.Fatalf("reconciliations = %d, want 1", len(calls))
	}
	if len(calls[0].taskIDs) != 2 || calls[0].taskIDs[0] != "a1" || calls[0].taskIDs[1] != "b1" {
		t.Fatalf("reconciled task ids = %v, want [a1 b1]", calls[0].taskIDs)
	}
}

func TestBackgroundTasksChangedReconcilesUnderTheEventSessionID(t *testing.T) {
	// Arrange — the SSM resolves a workspace from the identity the EVENT
	// carries, which for a store event is the vendor uuid.
	applier := &fakeApplier{}
	c := newTestConsumer(&fakePusher{}, applier)
	ev := backgroundTasksEvent(t, 5, "a1")
	ev.SessionId = "vendor-uuid"

	// Act.
	c.Consume(ev)

	// Assert.
	calls := applier.reconcileCalls()
	if len(calls) != 1 || calls[0].sessionID != "vendor-uuid" {
		t.Fatalf("reconciled under %v, want vendor-uuid", calls)
	}
}

func TestBackgroundTasksChangedRepublishesTheTaskCatalog(t *testing.T) {
	// Arrange — a ghost the roster is still showing as running.
	push := &fakePusher{}
	c := newTestConsumer(push, &fakeApplier{})
	c.Apply(&corev1.Event{
		SessionId: "s1", Seq: 1, ProducedAtMs: 100,
		Payload: &corev1.Event_TaskStarted{TaskStarted: &corev1.TaskStarted{TaskId: "ghost"}},
	})
	push.mu.Lock()
	push.catalog = nil
	push.mu.Unlock()

	// Act — the session reports nothing running.
	c.Consume(backgroundTasksEvent(t, 5))

	// Assert.
	push.mu.Lock()
	defer push.mu.Unlock()
	if len(push.catalog) != 1 {
		t.Fatalf("task catalog pushes = %d, want 1", len(push.catalog))
	}
	if got := push.catalog[0].GetTasks()[0].GetStatus(); got != "lost" {
		t.Fatalf("ghost status = %q, want lost", got)
	}
}

func TestBackgroundTasksChangedStillRefreshesTheRosterWhenTheSSMFails(t *testing.T) {
	// Arrange — the two task planes are independent; losing both over one
	// failure would be strictly worse than losing one.
	push := &fakePusher{}
	c := newTestConsumer(push, &fakeApplier{reconcErr: errors.New("ssm down")})

	// Act.
	c.Consume(backgroundTasksEvent(t, 5, "a1"))

	// Assert.
	push.mu.Lock()
	defer push.mu.Unlock()
	if len(push.catalog) != 1 {
		t.Fatalf("task catalog pushes = %d, want 1 despite the SSM failure", len(push.catalog))
	}
}

func TestNonTaskVendorEventReconcilesNothing(t *testing.T) {
	// Arrange — every vendor event shares one Any type URL, so the inner arm
	// must be the discriminator.
	applier := &fakeApplier{}
	c := newTestConsumer(&fakePusher{}, applier)

	// Act.
	c.Consume(initEvent(t, 1, "compact"))

	// Assert.
	if got := len(applier.reconcileCalls()); got != 0 {
		t.Fatalf("reconciliations = %d for a non-task vendor event, want 0", got)
	}
}

// --- F3: the REOPEN wedge ---------------------------------------------------
//
// observeBackfill can only witness a backfill HAPPENING. A session reopened
// with its transcript already fully ingested produces no new line to witness
// — the sidecar's cursor sits at that file's tail — so waiting for one waits
// forever, and the workspace sits blue despite complete, replayable history.

func TestReopenWithIngestedHistorySettlesTheBackfill(t *testing.T) {
	// Arrange — a session the daemon has durably observed store events for.
	var states []string
	c := backfillConsumer(&states)
	// Act — no event ever arrives; only the high-water speaks.
	c.settleBackfillFromStore(4200)
	// Assert
	if len(states) != 1 || states[0] != BackfillDone {
		t.Fatalf("backfill states = %v, want [done] from the store high-water", states)
	}
}

func TestReopenSettlesWithoutAnyEventArriving(t *testing.T) {
	// Arrange
	var states []string
	c := backfillConsumer(&states)
	// Act
	c.settleBackfillFromStore(1)
	// Assert — the point is that NO Consume() call was needed.
	if c.backfill != BackfillDone {
		t.Fatalf("backfill = %q, want %q with no event consumed", c.backfill, BackfillDone)
	}
}

func TestZeroHighWaterLeavesTheBackfillToTheLivePath(t *testing.T) {
	// Arrange — a genuinely fresh session: nothing durably observed.
	var states []string
	c := backfillConsumer(&states)
	// Act
	c.settleBackfillFromStore(0)
	// Assert — claiming DONE here would assert a backfill that never happened.
	if len(states) != 0 {
		t.Fatalf("backfill states = %v, want none for a zero high-water", states)
	}
}

// FAILED is terminal: a transcript the sidecar could not fully read does not
// become readable by reopening the session.
func TestStoreHighWaterNeverOverwritesAFailedBackfill(t *testing.T) {
	// Arrange
	var states []string
	c := backfillConsumer(&states)
	c.noteBackfill(BackfillFailed)
	// Act
	c.settleBackfillFromStore(9999)
	// Assert
	if c.backfill != BackfillFailed {
		t.Fatalf("backfill = %q, want it to stay %q", c.backfill, BackfillFailed)
	}
}

func TestStoreSettleIsIdempotent(t *testing.T) {
	// Arrange
	var states []string
	c := backfillConsumer(&states)
	// Act
	c.settleBackfillFromStore(10)
	c.settleBackfillFromStore(20)
	// Assert — one transition, not one per call.
	if len(states) != 1 {
		t.Fatalf("backfill states = %v, want exactly one transition", states)
	}
}
