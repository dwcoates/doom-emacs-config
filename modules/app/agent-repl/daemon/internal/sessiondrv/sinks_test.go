package sessiondrv

import (
	"errors"
	"fmt"
	"reflect"
	"strings"
	"sync"
	"testing"

	corev1 "agentrepl/proto/agentshim/core/v1"
	datav1 "agentrepl/proto/agentshim/data/v1"
	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"claude-repld/internal/errclass"
	"claude-repld/internal/ssm"

	"google.golang.org/protobuf/types/known/anypb"
)

// fakePusher records every frontend push for assertions.
type fakePusher struct {
	mu         sync.Mutex
	convo      []*frontendv1.ConversationDelta
	typing     []*frontendv1.TypingDelta
	catalog    []*frontendv1.TaskCatalog
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
	applied   []*corev1.Event
	err       error
	turns     []string
	starts    map[string]uint64
	ends      map[string]uint64
	bridges   []*corev1.Event
	bridgeErr error
	// reconciled records one entry per ReconcileTasks call, as
	// (sessionID, liveTaskIDs).
	reconciled  []reconcileCall
	reconcErr   error
	reconcMutex sync.Mutex
	// backfills records one entry per ApplyBackfillState call, as the
	// (workspace, state) pair the driver pushed onto the SSM's backfill axis.
	backfills []backfillCall
	// degradations records one entry per ApplyConnectionDegraded call (F4) —
	// the transport-level miss that used to reach no state axis at all.
	degradations []degradedCall
	degradedErr  error
	// interruptMarks records one workspace per MarkTurnInterrupted call — the
	// user-commanded stops that will paint their turn's end `interrupted`.
	interruptMarks   []string
	interruptMarkErr error
	// rotations records one entry per ApplySessionRotated call — the vendor
	// session uuid rotations the driver reconciled onto the agent axis.
	rotations   []rotationCall
	rotationErr error
	// cuts records one entry per ApplyClearing / ApplyCompacting call — the
	// two context-cut axes the driver opens and closes.
	cuts   []cutCall
	cutErr error
	// permissions records one entry per ApplyPermission call — the permission
	// row's open and close edges, driven off the workspace's pending count.
	permissions []permissionCall
	permErr     error
	// wirings records one entry per ApplyWired call — every edge of the WIRED
	// axis this package produces (bring-up, ShimReady, exit, hibernation,
	// rotation bounce).
	wirings  []wiringCall
	wiredErr error
	// current is what Current resolves per workspace — the hibernation settled
	// guard's only input. Absent means "nothing resolved", which is what every
	// test that does not care about the guard wants.
	current    map[string]*frontendv1.WorkspaceState
	currentErr error
	// staleTurnCloses records one entry per CloseStaleTurn call — the
	// teardown's guaranteed agent-axis close, which is what makes a
	// daemon-initiated shim stop unable to strand a live turn.
	staleTurnCloses []staleTurnCloseCall
	staleTurnClosed bool
	staleTurnErr    error
}

// wiringCall is one WIRED-axis edge the driver applied.
type wiringCall struct {
	workspace string
	wiring    ssm.Wiring
	reason    string
}

// permissionCall is one permission-row edge the driver applied.
type permissionCall struct {
	workspace string
	pending   bool
	reason    string
}

// cutCall is one context-cut axis edge the driver applied.
type cutCall struct {
	axis      string // "clearing" | "compacting"
	workspace string
	open      bool
	reason    string
}

// rotationCall is one vendor session rotation the driver reconciled.
type rotationCall struct {
	workspace string
	previous  string
	next      string
}

// backfillCall is one backfill outcome the driver applied to the SSM.
type backfillCall struct {
	workspace string
	state     string
}

// degradedCall is one connection-degraded transition the driver applied.
type degradedCall struct {
	workspace string
	degraded  bool
	reason    string
}

func (f *fakeApplier) ApplyBackfillState(workspace, state string) error {
	f.reconcMutex.Lock()
	defer f.reconcMutex.Unlock()
	f.backfills = append(f.backfills, backfillCall{workspace: workspace, state: state})
	return nil
}

func (f *fakeApplier) ApplyConnectionDegraded(workspace string, degraded bool, reason string) error {
	f.reconcMutex.Lock()
	defer f.reconcMutex.Unlock()
	f.degradations = append(f.degradations, degradedCall{workspace: workspace, degraded: degraded, reason: reason})
	return f.degradedErr
}

// MarkTurnInterrupted records the workspaces whose running turn a
// user-commanded stop was delivered to (I1).
func (f *fakeApplier) MarkTurnInterrupted(workspace string) error {
	f.reconcMutex.Lock()
	defer f.reconcMutex.Unlock()
	f.interruptMarks = append(f.interruptMarks, workspace)
	return f.interruptMarkErr
}

// ApplySessionRotated records the vendor session rotations reconciled onto the
// agent axis.
func (f *fakeApplier) ApplySessionRotated(workspace, previous, next string) error {
	f.reconcMutex.Lock()
	defer f.reconcMutex.Unlock()
	f.rotations = append(f.rotations, rotationCall{workspace: workspace, previous: previous, next: next})
	return f.rotationErr
}

// ApplyClearing records the clearing-axis edges the driver applied.
func (f *fakeApplier) ApplyClearing(workspace string, clearing bool, reason string) error {
	f.reconcMutex.Lock()
	defer f.reconcMutex.Unlock()
	f.cuts = append(f.cuts, cutCall{axis: "clearing", workspace: workspace, open: clearing, reason: reason})
	return f.cutErr
}

// ApplyCompacting records the compacting-axis edges the driver applied.
func (f *fakeApplier) ApplyCompacting(workspace string, compacting bool, reason string) error {
	f.reconcMutex.Lock()
	defer f.reconcMutex.Unlock()
	f.cuts = append(f.cuts, cutCall{axis: "compacting", workspace: workspace, open: compacting, reason: reason})
	return f.cutErr
}

// ApplyPermission records the permission-row edges the driver applied.
func (f *fakeApplier) ApplyPermission(workspace string, pending bool, reason string) error {
	f.reconcMutex.Lock()
	defer f.reconcMutex.Unlock()
	f.permissions = append(f.permissions, permissionCall{workspace: workspace, pending: pending, reason: reason})
	return f.permErr
}

func (f *fakeApplier) ApplyWired(workspace string, wiring ssm.Wiring, reason string) error {
	f.reconcMutex.Lock()
	defer f.reconcMutex.Unlock()
	f.wirings = append(f.wirings, wiringCall{workspace: workspace, wiring: wiring, reason: reason})
	return f.wiredErr
}

// Current is the one READ on the applier, used only by the hibernation settled
// guard. The zero value answers "nothing resolved", which the guard treats as a
// workspace with no turn to interrupt — so a test arranges an unsettled workspace
// by SETTING current, and every existing test keeps hibernating freely.
func (f *fakeApplier) Current(workspace string) (*frontendv1.WorkspaceState, bool, error) {
	f.reconcMutex.Lock()
	defer f.reconcMutex.Unlock()
	if f.currentErr != nil {
		return nil, false, f.currentErr
	}
	st, ok := f.current[workspace]
	if !ok {
		return nil, false, nil
	}
	return st, true, nil
}

// staleTurnCloseCall is one teardown axis close the driver asked the SSM for.
type staleTurnCloseCall struct {
	workspace  string
	sessionID  string
	reason     string
	soleDriver bool
}

// CloseStaleTurn records the teardown's guaranteed agent-axis close. The zero
// value answers "there was nothing stale to close", which is what every test
// that does not care about the invariant wants; a test exercising it sets
// staleTurnClosed or staleTurnErr.
func (f *fakeApplier) CloseStaleTurn(workspace, sessionID, reason string, soleDriver bool) (bool, error) {
	f.reconcMutex.Lock()
	defer f.reconcMutex.Unlock()
	f.staleTurnCloses = append(f.staleTurnCloses, staleTurnCloseCall{
		workspace: workspace, sessionID: sessionID, reason: reason, soleDriver: soleDriver,
	})
	return f.staleTurnClosed, f.staleTurnErr
}

// staleTurnClosesApplied returns the recorded teardown axis closes, taken under
// the lock so a driver goroutine cannot race the read.
func (f *fakeApplier) staleTurnClosesApplied() []staleTurnCloseCall {
	f.reconcMutex.Lock()
	defer f.reconcMutex.Unlock()
	return append([]staleTurnCloseCall(nil), f.staleTurnCloses...)
}

// setCurrent arranges the resolved state the settled guard will read.
func (f *fakeApplier) setCurrent(workspace string, st *frontendv1.WorkspaceState) {
	f.reconcMutex.Lock()
	defer f.reconcMutex.Unlock()
	if f.current == nil {
		f.current = map[string]*frontendv1.WorkspaceState{}
	}
	f.current[workspace] = st
}

// wiringsApplied returns the recorded WIRED-axis edges, taken under the lock so
// a driver goroutine cannot race the read.
func (f *fakeApplier) wiringsApplied() []wiringCall {
	f.reconcMutex.Lock()
	defer f.reconcMutex.Unlock()
	out := make([]wiringCall, len(f.wirings))
	copy(out, f.wirings)
	return out
}

// permissionsApplied returns the recorded permission-row edges, taken under the
// lock so a concurrent handler goroutine cannot race the read.
func (f *fakeApplier) permissionsApplied() []permissionCall {
	f.reconcMutex.Lock()
	defer f.reconcMutex.Unlock()
	out := make([]permissionCall, len(f.permissions))
	copy(out, f.permissions)
	return out
}

// cutsApplied returns the recorded context-cut edges, taken under the lock.
func (f *fakeApplier) cutsApplied() []cutCall {
	f.reconcMutex.Lock()
	defer f.reconcMutex.Unlock()
	return append([]cutCall(nil), f.cuts...)
}

// rotationsApplied returns the recorded rotations, taken under the lock.
func (f *fakeApplier) rotationsApplied() []rotationCall {
	f.reconcMutex.Lock()
	defer f.reconcMutex.Unlock()
	return append([]rotationCall(nil), f.rotations...)
}

// interruptMarked returns the recorded marks, taken under the lock.
func (f *fakeApplier) interruptMarked() []string {
	f.reconcMutex.Lock()
	defer f.reconcMutex.Unlock()
	return append([]string(nil), f.interruptMarks...)
}

// degradedCalls returns the recorded transitions, taken under the lock.
func (f *fakeApplier) degradedCalls() []degradedCall {
	f.reconcMutex.Lock()
	defer f.reconcMutex.Unlock()
	return append([]degradedCall(nil), f.degradations...)
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

func (a *fakeApplier) ResolveTurnLifecycle(_ string, _ string, ev *corev1.Event) (before, after []string, replayed bool, err error) {
	a.reconcMutex.Lock()
	defer a.reconcMutex.Unlock()
	if a.starts == nil {
		a.starts = make(map[string]uint64)
		a.ends = make(map[string]uint64)
	}
	before = append([]string(nil), a.turns...)
	id := turnID(ev)
	key := fmt.Sprintf("%s:%d", id, ev.GetSeq())
	switch ev.GetPayload().(type) {
	case *corev1.Event_TurnStarted:
		if prior, ok := a.starts[key]; ok && prior == ev.GetSeq() {
			return before, before, true, nil
		}
		for _, active := range a.turns {
			if id != "" && active == id {
				return before, before, false, fmt.Errorf("duplicate active turn %q", id)
			}
		}
		a.starts[key] = ev.GetSeq()
		a.turns = append(a.turns, id)
	case *corev1.Event_TurnEnded:
		if prior, ok := a.ends[key]; ok && prior == ev.GetSeq() {
			return before, before, true, nil
		}
		if len(a.turns) == 0 || a.turns[0] != id {
			return before, before, false, fmt.Errorf("turn end %q has no matching durable claim", id)
		}
		a.ends[key] = ev.GetSeq()
		a.turns = append([]string(nil), a.turns[1:]...)
	}
	return before, append([]string(nil), a.turns...), false, nil
}

func (a *fakeApplier) ResolveTurnClaimBridge(_ string, _ string, ev *corev1.Event) (bool, error) {
	a.reconcMutex.Lock()
	defer a.reconcMutex.Unlock()
	a.bridges = append(a.bridges, ev)
	if a.bridgeErr != nil {
		return false, a.bridgeErr
	}
	id := ev.GetTurnClaimBridge().GetTurnId()
	for _, active := range a.turns {
		if active == id {
			return false, nil
		}
	}
	a.turns = append(a.turns, id)
	return false, nil
}

func (a *fakeApplier) ReconcileTurnHandshake(_ string, _ string, ids []string, legacyActive bool) (before, after []string, err error) {
	a.reconcMutex.Lock()
	defer a.reconcMutex.Unlock()
	before = append([]string(nil), a.turns...)
	switch {
	case len(ids) > 0 && len(a.turns) == 0:
		a.turns = append([]string(nil), ids...)
	case len(ids) > 0 && !reflect.DeepEqual(ids, a.turns):
		return before, before, fmt.Errorf("handshake ids %v disagree with durable ids %v", ids, a.turns)
	case legacyActive && len(a.turns) == 0:
		a.turns = []string{""}
	}
	return before, append([]string(nil), a.turns...), nil
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
	c := newConsumer("ws", "s1", push, applier, nil, newFakeClearCompactStore(), nil, nil, nil, nil, nil, nil)
	c.now = func() int64 { return 1000 }
	return c
}

func TestTurnClaimBridgeTouchesOnlyDurableLedger(t *testing.T) {
	push := &fakePusher{}
	applier := &fakeApplier{}
	progress := &fakeProgress{}
	turnNotifications := 0
	c := newConsumer(
		"ws", "s1", push, applier, progress, newFakeClearCompactStore(),
		func(string, ...any) {}, nil,
		func(bool) { turnNotifications++ }, nil, nil, nil,
	)
	bridge := &corev1.Event{
		SessionId: "vendor-new",
		Seq:       2,
		Plane:     corev1.Plane_PLANE_STREAM,
		Class:     corev1.EventClass_EVENT_CLASS_PERSISTENT,
		RequestId: "turn-1",
		Payload: &corev1.Event_TurnClaimBridge{TurnClaimBridge: &corev1.TurnClaimBridge{
			TurnId: "turn-1", PreviousSessionId: "vendor-old",
		}},
	}

	if err := c.ApplyTurnClaimBridge(bridge); err != nil {
		t.Fatalf("ApplyTurnClaimBridge: %v", err)
	}
	if len(applier.bridges) != 1 || applier.bridges[0] != bridge {
		t.Fatalf("durable bridge calls = %v, want exactly the proof event", applier.bridges)
	}
	if len(applier.applied) != 0 {
		t.Fatalf("SSM Apply received bridge: %v", applier.applied)
	}
	if len(progress.applied) != 0 {
		t.Fatalf("progress received bridge: %v", progress.applied)
	}
	if turnNotifications != 0 {
		t.Fatalf("onTurn calls = %d, want 0", turnNotifications)
	}
	if len(c.ring) != 0 {
		t.Fatalf("retained ring length = %d, want 0", len(c.ring))
	}
	if len(push.convo)+len(push.typing)+len(push.catalog)+len(push.state)+
		len(push.inits)+len(push.heartbeats)+len(push.queues) != 0 {
		t.Fatal("frontend received a push from turn claim proof")
	}

	if err := c.Apply(bridge); err == nil ||
		!strings.Contains(err.Error(), "must use ApplyTurnClaimBridge") {
		t.Fatalf("misrouted bridge error = %v", err)
	}
	if len(applier.applied) != 0 || len(progress.applied) != 0 ||
		turnNotifications != 0 || len(c.ring) != 0 {
		t.Fatal("misrouted bridge mutated an outward lifecycle consumer")
	}
}

// fakeProgress records what the consumer folds into the progress resolver.
type fakeProgress struct {
	mu         sync.Mutex
	applied    []*corev1.Event
	workspaces []string
	err        error
	// interrupts records one entry per NoteInterrupt call — the interrupt
	// windows a USER-COMMANDED stop opened.
	interrupts []interruptNote
}

// interruptNote is one opened interrupt window.
type interruptNote struct {
	workspace string
	sessionID string
	outcome   corev1.InterruptOutcome
}

func (p *fakeProgress) Apply(workspace string, ev *corev1.Event) error {
	p.workspaces = append(p.workspaces, workspace)
	p.applied = append(p.applied, ev)
	return p.err
}
func (p *fakeProgress) SetCounts(string, int64, int64)  {}
func (p *fakeProgress) NoteTurnAccepted(string, string) {}

func (p *fakeProgress) NoteInterrupt(workspace, sessionID string, outcome corev1.InterruptOutcome) {
	p.mu.Lock()
	defer p.mu.Unlock()
	p.interrupts = append(p.interrupts, interruptNote{workspace: workspace, sessionID: sessionID, outcome: outcome})
}

// interruptNotes returns the recorded windows, taken under the lock.
func (p *fakeProgress) interruptNotes() []interruptNote {
	p.mu.Lock()
	defer p.mu.Unlock()
	return append([]interruptNote(nil), p.interrupts...)
}

func newProgressConsumer(prog ProgressResolver) *consumer {
	c := newConsumer("ws", "s1", &fakePusher{}, &fakeApplier{}, prog, newFakeClearCompactStore(), nil, nil, nil, nil, nil, nil)
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
		Plane:     corev1.Plane_PLANE_STREAM,
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
	c := newConsumer("ws", "s1", &fakePusher{}, &fakeApplier{}, nil, newFakeClearCompactStore(), nil, nil, nil, func(state string) { *states = append(*states, state) }, nil, nil)
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
	for range 5 {
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
	c := newConsumer("ws", "s1", push, &fakeApplier{}, prog, newFakeClearCompactStore(), nil, nil, nil, nil, nil, nil)
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

func TestApplyTaskProgressReachesSSMAndRingWithoutPushingCatalog(t *testing.T) {
	// Arrange.
	push := &fakePusher{}
	applier := &fakeApplier{}
	c := newTestConsumer(push, applier)
	progress := &corev1.Event{
		SessionId: "s1",
		Seq:       2,
		Payload: &corev1.Event_TaskProgress{TaskProgress: &corev1.TaskProgress{
			TaskId: "t1",
		}},
	}

	// Act.
	c.Apply(progress)

	// Assert — progress remains part of the ordered live event window and SSM
	// input, but it cannot change the TaskCatalog vocabulary.
	if len(applier.applied) != 1 || applier.applied[0] != progress {
		t.Fatalf("SSM applied events = %v, want the TaskProgress event", applier.applied)
	}
	ring := c.snapshotRing()
	if len(ring) != 1 || ring[0] != progress {
		t.Fatalf("retained ring = %v, want the TaskProgress event", ring)
	}
	if len(push.catalog) != 0 {
		t.Fatalf("task progress must not refresh the unchanged catalog; got %d pushes", len(push.catalog))
	}
}

func TestApplyNonTaskEventDoesNotPushCatalog(t *testing.T) {
	// Arrange.
	push := &fakePusher{}
	c := newTestConsumer(push, &fakeApplier{})

	// Act.
	c.Apply(&corev1.Event{SessionId: "s1", Seq: 1, Plane: corev1.Plane_PLANE_STREAM, Payload: &corev1.Event_TurnStarted{TurnStarted: &corev1.TurnStarted{}}})

	// Assert.
	if len(push.catalog) != 0 {
		t.Fatalf("turn event must not refresh the task catalog; got %d pushes", len(push.catalog))
	}
}

func TestApplyRejectsFileTurnEndBeforeQueueAndStateConsumers(t *testing.T) {
	applier := &fakeApplier{}
	var boundaries []bool
	var logs []string
	c := newConsumer(
		"ws", "s1", &fakePusher{}, applier, nil, newFakeClearCompactStore(),
		func(format string, args ...any) { logs = append(logs, fmt.Sprintf(format, args...)) },
		nil,
		func(active bool) { boundaries = append(boundaries, active) },
		nil, nil, nil,
	)
	c.Apply(turnStartEvent(corev1.Plane_PLANE_STREAM, 12885, "turn-new"))
	c.Apply(turnEndEvent(corev1.Plane_PLANE_FILE, 12891, ""))

	if len(applier.applied) != 1 || applier.applied[0].GetTurnStarted() == nil {
		t.Fatalf("SSM applied = %+v, want only the stream TurnStarted", applier.applied)
	}
	if !reflect.DeepEqual(boundaries, []bool{true}) {
		t.Fatalf("queue boundaries = %v, want only active=true", boundaries)
	}
	joined := strings.Join(logs, "\n")
	if !strings.Contains(joined, "decision=reject_non_authoritative_plane") ||
		!strings.Contains(joined, "seq=12891") ||
		!strings.Contains(joined, "active_after=[turn-new]") {
		t.Fatalf("turn authority log = %q", joined)
	}
}

func TestApplyFiresOnSessionStarted(t *testing.T) {
	// Arrange.
	var seen *corev1.SessionStarted
	c := newConsumer("ws", "s1", &fakePusher{}, &fakeApplier{}, nil, newFakeClearCompactStore(), nil, func(ss *corev1.SessionStarted) { seen = ss }, nil, nil, nil, nil)

	// Act.
	c.Apply(&corev1.Event{SessionId: "s1", Payload: &corev1.Event_SessionStarted{SessionStarted: &corev1.SessionStarted{Source: corev1.SessionSource_SESSION_SOURCE_RESUME}}})

	// Assert.
	if seen == nil || seen.GetSource() != corev1.SessionSource_SESSION_SOURCE_RESUME {
		t.Fatal("onSessionStarted must fire with the SessionStarted payload")
	}
}

// --- Degraded as a self-resolving card (F4) ---------------------------------
//
// The banner was chrome that scrolled away, carried no correlation id, and
// threw dropped_count away in translation. Its replacement is a conversation
// card whose two edges reconcile onto ONE uuid, plus the SSM row the
// transport-level miss never wrote.

// failureItems returns the system-failure items among a fake pusher's
// conversation deltas, which is where the degraded account now lives.
func failureItems(push *fakePusher) []*frontendv1.SystemFailureItem {
	var out []*frontendv1.SystemFailureItem
	for _, cd := range push.convo {
		for _, item := range cd.GetItems() {
			if f := item.GetSystemFailure(); f != nil {
				out = append(out, f)
			}
		}
	}
	return out
}

// failureUUIDs returns the ConversationItem uuids of the pushed failure cards.
func failureUUIDs(push *fakePusher) []string {
	var out []string
	for _, cd := range push.convo {
		for _, item := range cd.GetItems() {
			if item.GetSystemFailure() != nil {
				out = append(out, item.GetUuid())
			}
		}
	}
	return out
}

func TestDegradedStateBecomesAFailureCard(t *testing.T) {
	// Arrange.
	push := &fakePusher{}
	c := newTestConsumer(push, &fakeApplier{})

	// Act.
	c.Degraded("s1", &corev1.DegradedState{Component: "store", Reason: "down"})

	// Assert.
	got := failureItems(push)
	if len(got) != 1 {
		t.Fatalf("failure cards = %d, want 1", len(got))
	}
	if got[0].GetErrorType() != string(errclass.TypeShimStoreWriteRejected) {
		t.Fatalf("error_type = %q, want %q", got[0].GetErrorType(), errclass.TypeShimStoreWriteRejected)
	}
}

func TestDegradedStateCardCarriesTheDroppedCount(t *testing.T) {
	// Arrange: how much conversation was lost — the fact translation discarded.
	push := &fakePusher{}
	c := newTestConsumer(push, &fakeApplier{})

	// Act.
	c.Degraded("s1", &corev1.DegradedState{Component: "store", Reason: "down", DroppedCount: 12})

	// Assert.
	if got := failureItems(push)[0].GetSourceDetail(); !strings.Contains(got, "dropped=12") {
		t.Fatalf("source_detail = %q, want the dropped count", got)
	}
}

func TestConnectionDegradedAppendsTheSSMRow(t *testing.T) {
	// Arrange: the half that never existed. A heartbeat miss produced a banner
	// and NO workspace color, so retiring the banner without this would have
	// lost the ambience entirely.
	applier := &fakeApplier{}
	c := newTestConsumer(&fakePusher{}, applier)

	// Act.
	c.ConnectionDegraded("s1", "no traffic")

	// Assert.
	got := applier.degradedCalls()
	if len(got) != 1 || !got[0].degraded || got[0].workspace != "ws" {
		t.Fatalf("degraded transitions = %+v, want one degraded=true on ws", got)
	}
}

func TestConnectionRecoveredClearsTheSSMAxis(t *testing.T) {
	// Arrange.
	applier := &fakeApplier{}
	c := newTestConsumer(&fakePusher{}, applier)

	// Act.
	c.ConnectionRecovered("s1")

	// Assert.
	got := applier.degradedCalls()
	if len(got) != 1 || got[0].degraded {
		t.Fatalf("degraded transitions = %+v, want one degraded=false", got)
	}
}

func TestConnectionDegradedFailureIsLoudNotSwallowed(t *testing.T) {
	// Arrange: a workspace whose color silently failed to move is the exact
	// misreport this axis exists to prevent.
	var logged []string
	applier := &fakeApplier{degradedErr: errors.New("db gone")}
	c := newConsumer("ws", "s1", &fakePusher{}, applier, nil, newFakeClearCompactStore(), func(f string, a ...any) { logged = append(logged, f) }, nil, nil, nil, nil, nil)

	// Act.
	c.ConnectionDegraded("s1", "no traffic")

	// Assert.
	if len(logged) == 0 {
		t.Fatal("an SSM apply failure passed SILENTLY")
	}
}

func TestAConnectionWindowIsOneCardNotTwo(t *testing.T) {
	// Arrange: the degraded edge and its recovery.
	push := &fakePusher{}
	c := newTestConsumer(push, &fakeApplier{})

	// Act.
	c.ConnectionDegraded("s1", "no traffic")
	c.ConnectionRecovered("s1")

	// Assert: two pushes, ONE uuid — the feed reconciles in place rather than
	// accumulating an alarm and a separate all-clear.
	ids := failureUUIDs(push)
	if len(ids) != 2 {
		t.Fatalf("failure pushes = %d, want 2", len(ids))
	}
	if ids[0] != ids[1] {
		t.Fatalf("uuids = %v, want both edges under one uuid", ids)
	}
}

func TestTheOpeningEdgeIsUnresolved(t *testing.T) {
	// Arrange.
	push := &fakePusher{}
	c := newTestConsumer(push, &fakeApplier{})

	// Act.
	c.ConnectionDegraded("s1", "no traffic")

	// Assert.
	if got := failureItems(push)[0].GetResolvedAtMs(); got != 0 {
		t.Fatalf("resolved_at_ms = %d, want 0 while the window is open", got)
	}
}

func TestTheClosingEdgeStampsResolution(t *testing.T) {
	// Arrange.
	push := &fakePusher{}
	c := newTestConsumer(push, &fakeApplier{})

	// Act.
	c.ConnectionDegraded("s1", "no traffic")
	c.ConnectionRecovered("s1")

	// Assert: a settled card, not a permanent alarm about something that ended.
	if got := failureItems(push)[1].GetResolvedAtMs(); got == 0 {
		t.Fatal("resolved_at_ms = 0 on the closing edge; the card would stand as a permanent alarm")
	}
}

func TestAResyncReplaysTheSettledCard(t *testing.T) {
	// Arrange: a window that opened and closed, then a reconnect.
	push := &fakePusher{}
	c := newTestConsumer(push, &fakeApplier{})
	c.ConnectionDegraded("s1", "no traffic")
	c.ConnectionRecovered("s1")
	before := len(failureItems(push))

	// Act.
	c.resync(0)

	// Assert: exactly one replayed card, and it is the RESOLVED one.
	replayed := failureItems(push)[before:]
	if len(replayed) != 1 {
		t.Fatalf("replayed failure cards = %d, want 1", len(replayed))
	}
	if replayed[0].GetResolvedAtMs() == 0 {
		t.Fatal("the resync re-opened a window that had already closed")
	}
}

func TestAFailureCardCarriesItsOwnAddressing(t *testing.T) {
	// Arrange: what lets a footer row scroll the feed to this card.
	push := &fakePusher{}
	c := newTestConsumer(push, &fakeApplier{})

	// Act.
	c.ConnectionDegraded("s1", "no traffic")

	// Assert.
	if got := failureItems(push)[0].GetItemUuid(); got != failureUUIDs(push)[0] {
		t.Fatalf("item_uuid = %q, want it to match the envelope uuid %q", got, failureUUIDs(push)[0])
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

// --- Session death (F4) -----------------------------------------------------
//
// Nothing marked a record terminal on shim death, so the SSM resolved the
// workspace RENDER_STATE_DEAD while the record still claimed the session was
// alive — the color and its account on two disconnected axes.

func TestSessionEndedReportsTheDeath(t *testing.T) {
	// Arrange.
	var ended int
	c := newConsumer("ws", "s1", &fakePusher{}, &fakeApplier{}, nil, newFakeClearCompactStore(), nil, nil, nil, nil, nil, func() { ended++ })

	// Act.
	c.Apply(&corev1.Event{Payload: &corev1.Event_SessionEnded{SessionEnded: &corev1.SessionEnded{}}})

	// Assert.
	if ended != 1 {
		t.Fatalf("session-ended reports = %d, want 1", ended)
	}
}

func TestATurnEndDoesNotReportADeath(t *testing.T) {
	// Arrange: a turn ending is not a session ending.
	var ended int
	c := newConsumer("ws", "s1", &fakePusher{}, &fakeApplier{}, nil, newFakeClearCompactStore(), nil, nil, nil, nil, nil, func() { ended++ })

	// Act.
	c.Apply(&corev1.Event{Plane: corev1.Plane_PLANE_STREAM, Payload: &corev1.Event_TurnEnded{TurnEnded: &corev1.TurnEnded{}}})

	// Assert.
	if ended != 0 {
		t.Fatalf("session-ended reports = %d, want 0", ended)
	}
}

// --- the prompt round-trip receipt ------------------------------------------

func TestUserTurnReceiptReadsAPromptString(t *testing.T) {
	// Arrange — a delta carrying one string-content user prompt.
	cd := &frontendv1.ConversationDelta{Items: []*frontendv1.ConversationItem{{
		RequestId: "fe-9-abcd",
		Item: &frontendv1.ConversationItem_UserMessage{UserMessage: &datav1.ApiUserMessage{
			Content: &datav1.ApiUserMessage_ContentString{ContentString: "hello there"},
		}},
	}}}
	// Act
	requestID, textLen := userTurnReceipt(cd)
	// Assert
	if requestID != "fe-9-abcd" || textLen != len("hello there") {
		t.Fatalf("receipt = (%q, %d), want (fe-9-abcd, %d)", requestID, textLen, len("hello there"))
	}
}

func TestUserTurnReceiptSumsTextBlocks(t *testing.T) {
	// Arrange — a block-content prompt with two text blocks.
	cd := &frontendv1.ConversationDelta{Items: []*frontendv1.ConversationItem{{
		RequestId: "fe-3-cafe",
		Item: &frontendv1.ConversationItem_UserMessage{UserMessage: &datav1.ApiUserMessage{
			Content: &datav1.ApiUserMessage_ContentBlocks{ContentBlocks: &datav1.ApiContentBlocks{
				Blocks: []*datav1.ContentBlock{
					{Block: &datav1.ContentBlock_Text{Text: &datav1.TextBlock{Text: "ab"}}},
					{Block: &datav1.ContentBlock_Text{Text: &datav1.TextBlock{Text: "cde"}}},
				},
			}},
		}},
	}}}
	// Act
	_, textLen := userTurnReceipt(cd)
	// Assert
	if textLen != 5 {
		t.Fatalf("textLen = %d, want 5", textLen)
	}
}

func TestUserTurnReceiptIgnoresPureToolFeedback(t *testing.T) {
	// Arrange — a user_message carrying only a tool_result block: rendered as
	// a tool result, never a prompt bubble, so no receipt (logging one per
	// tool call would bury the per-prompt line this exists for).
	cd := &frontendv1.ConversationDelta{Items: []*frontendv1.ConversationItem{{
		RequestId: "fe-4-feed",
		Item: &frontendv1.ConversationItem_UserMessage{UserMessage: &datav1.ApiUserMessage{
			Content: &datav1.ApiUserMessage_ContentBlocks{ContentBlocks: &datav1.ApiContentBlocks{
				Blocks: []*datav1.ContentBlock{
					{Block: &datav1.ContentBlock_ToolResult{ToolResult: &datav1.ToolResultBlock{}}},
				},
			}},
		}},
	}}}
	// Act
	_, textLen := userTurnReceipt(cd)
	// Assert
	if textLen != 0 {
		t.Fatalf("textLen = %d, want 0 for pure tool feedback", textLen)
	}
}

func TestUserTurnReceiptIgnoresNonUserItems(t *testing.T) {
	// Arrange — an assistant-only delta.
	cd := &frontendv1.ConversationDelta{Items: []*frontendv1.ConversationItem{{
		RequestId: "fe-5-0000",
		Item:      &frontendv1.ConversationItem_AssistantMessage{AssistantMessage: &datav1.ApiAssistantMessage{}},
	}}}
	// Act
	_, textLen := userTurnReceipt(cd)
	// Assert
	if textLen != 0 {
		t.Fatalf("textLen = %d, want 0 for a non-user delta", textLen)
	}
}
