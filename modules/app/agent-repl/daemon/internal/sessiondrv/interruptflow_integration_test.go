package sessiondrv

import (
	"context"
	"errors"
	"path/filepath"
	"sync"
	"testing"

	corev1 "agentrepl/proto/agentshim/core/v1"
	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"claude-repld/internal/errclass"
	"claude-repld/internal/progress"
	"claude-repld/internal/shimclient"
	"claude-repld/internal/ssm"

	"google.golang.org/protobuf/proto"
)

// THE INTERRUPT, END TO END, AS A FRONTEND SEES IT.
//
// The unit seams are pinned elsewhere and are deliberately not repeated here:
// ssm/interrupted_test.go pins the mark's lifetime, progress/interrupt_test.go
// pins the window's open/clear rules, interrupt_test.go pins the driver's
// routing and the queue's pause, and server/interruptgate_test.go pins the
// confirm gate's four cases. Every one of those substitutes a double for at
// least one of the other three.
//
// What none of them can show is the COMPOSITION: that a user's stop travelling
// through a live Manager, a REAL ssm.Manager and a REAL progress.Manager
// produces the frames a frontend actually renders — an open window on the
// ProgressView, `interrupted` on the WorkspaceState, and both superseded by
// the next turn. The rig below therefore fakes exactly one thing, the shim
// client, and mirrors the production subscribe loops (server/agentshimwire.go
// lines 265-283) so what these tests read is what a connected client is sent.

const (
	interruptFlowWorkspace = "/ws/interrupt-flow"
	interruptFlowSessionID = "s_interrupt_flow"
	// interruptFlowNowMs is the frozen clock both resolvers and the driver
	// read, so a window's since_ms is an assertable constant rather than a
	// wall-clock reading.
	interruptFlowNowMs int64 = 1_700_000_000_000
)

// interruptFlowResolver binds the session id to its workspace for the SSM.
type interruptFlowResolver map[string]string

func (r interruptFlowResolver) Workspace(sessionID string) (string, bool) {
	ws, ok := r[sessionID]
	return ws, ok
}

// interruptFlowRig is one workspace driven end to end: a live Manager over a
// real SSM and a real progress resolver, with the frames both fan out recorded
// off their own subscriptions.
type interruptFlowRig struct {
	t      *testing.T
	m      *Manager
	ssm    *ssm.Manager
	prog   *progress.Manager
	push   *fakePusher
	client *fakeClient
	seq    uint64

	mu     sync.Mutex
	states []*frontendv1.WorkspaceState
	views  []*frontendv1.ProgressView
}

func newInterruptFlowRig(t *testing.T) *interruptFlowRig {
	t.Helper()

	mgr, err := ssm.Open(ssm.Options{
		DBPath:   filepath.Join(t.TempDir(), "ssm.db"),
		Resolver: interruptFlowResolver{interruptFlowSessionID: interruptFlowWorkspace},
		Logf:     t.Logf,
	})
	if err != nil {
		t.Fatalf("open ssm: %v", err)
	}
	t.Cleanup(func() { _ = mgr.Close() })

	// CoalesceWindow negative disables coalescing outright, so every frame this
	// rig asserts on is the one the resolver decided to send rather than one a
	// timer happened to flush.
	prog := progress.New(progress.Options{
		Logf:           t.Logf,
		Clock:          func() int64 { return interruptFlowNowMs },
		CoalesceWindow: -1,
	})
	t.Cleanup(func() { _ = prog.Close() })

	rig := &interruptFlowRig{t: t, ssm: mgr, prog: prog, push: &fakePusher{}}

	// The production fan-out, mirrored: SSM transitions become WorkspaceState
	// pushes AND feed the progress resolver's live-task adoption; progress
	// changes become ProgressView pushes on their own subscription.
	states, cancelStates := mgr.Subscribe()
	views, cancelViews := prog.Subscribe()
	var wg sync.WaitGroup
	wg.Add(2)
	go func() {
		defer wg.Done()
		for ws := range states {
			rig.mu.Lock()
			rig.states = append(rig.states, ws)
			rig.mu.Unlock()
			if err := prog.ObserveWorkspaceState(ws); err != nil {
				t.Errorf("progress observe workspace state: %v", err)
			}
		}
	}()
	go func() {
		defer wg.Done()
		for v := range views {
			rig.mu.Lock()
			rig.views = append(rig.views, v)
			rig.mu.Unlock()
		}
	}()
	t.Cleanup(func() {
		cancelStates()
		cancelViews()
		wg.Wait()
	})

	var mu sync.Mutex
	var last *fakeClient
	m, err := New(Config{
		Push:              rig.push,
		SSM:               mgr,
		Progress:          prog,
		Spawner:           &fakeSpawner{},
		Locator:           fakeLocator{m: map[string]string{interruptFlowWorkspace: interruptFlowSessionID}},
		SeqStore:          &fakeSeqStore{seq: map[string]uint64{}},
		ClearCompactStore: newFakeClearCompactStore(),
		Registrar:         &fakeRegistrar{},
		ProtocolVersion:   "1",
		Logf:              t.Logf,
		now:               func() int64 { return interruptFlowNowMs },
		Source:            stubSource{},
		newClient: func(c shimclient.Config) sessionClient {
			fc := &fakeClient{cfg: c}
			mu.Lock()
			last = fc
			mu.Unlock()
			return fc
		},
	})
	if err != nil {
		t.Fatalf("New: %v", err)
	}
	t.Cleanup(m.Close)
	if err := m.Ensure(interruptFlowWorkspace); err != nil {
		t.Fatalf("Ensure: %v", err)
	}
	mu.Lock()
	rig.m, rig.client = m, last
	mu.Unlock()
	return rig
}

// apply feeds one lifecycle event in at the seam the shim's demux uses, so the
// SSM, the progress resolver and the queue's turn-boundary hook all see it the
// way they see a real stream.
func (r *interruptFlowRig) apply(payload any) {
	r.t.Helper()
	r.seq++
	ev := &corev1.Event{SessionId: interruptFlowSessionID, Seq: r.seq, ProducedAtMs: interruptFlowNowMs}
	switch p := payload.(type) {
	case *corev1.SessionStarted:
		ev.Payload = &corev1.Event_SessionStarted{SessionStarted: p}
	case *corev1.TurnStarted:
		ev.Payload = &corev1.Event_TurnStarted{TurnStarted: p}
	case *corev1.TurnEnded:
		ev.Payload = &corev1.Event_TurnEnded{TurnEnded: p}
	default:
		r.t.Fatalf("interruptFlowRig.apply: unsupported payload %T", payload)
	}
	d, err := r.m.existing(interruptFlowWorkspace)
	if err != nil {
		r.t.Fatalf("existing: %v", err)
	}
	d.consumer.Apply(ev)
}

// settleReady brings the workspace up to a resolved, non-blue baseline: a
// started session and a settled backfill.
func (r *interruptFlowRig) settleReady() {
	r.t.Helper()
	r.apply(&corev1.SessionStarted{Model: "test-model", Cwd: interruptFlowWorkspace})
	if err := r.ssm.ApplyBackfillState(interruptFlowWorkspace, BackfillDone); err != nil {
		r.t.Fatalf("apply backfill done: %v", err)
	}
}

// ackWith arms the fake shim's interrupt ack with a specific outcome.
func (r *interruptFlowRig) ackWith(outcome corev1.InterruptOutcome) {
	r.client.mu.Lock()
	r.client.interruptOutcome = outcome
	r.client.mu.Unlock()
}

// interrupt runs the USER-COMMANDED stop — the same call the frontend
// interrupt command handler makes.
func (r *interruptFlowRig) interrupt() error {
	r.t.Helper()
	return r.m.Interrupt(context.Background(), interruptFlowWorkspace)
}

// submit submits a prompt for the workspace.
func (r *interruptFlowRig) submit(text string) error {
	r.t.Helper()
	return r.m.SubmitPrompt(context.Background(), interruptFlowWorkspace, "", text, "")
}

// lastView returns the newest ProgressView a subscriber was sent, or nil.
func (r *interruptFlowRig) lastView() *frontendv1.ProgressView {
	r.mu.Lock()
	defer r.mu.Unlock()
	if len(r.views) == 0 {
		return nil
	}
	return r.views[len(r.views)-1]
}

// lastState returns the newest WorkspaceState a subscriber was sent, or nil.
func (r *interruptFlowRig) lastState() *frontendv1.WorkspaceState {
	r.mu.Lock()
	defer r.mu.Unlock()
	if len(r.states) == 0 {
		return nil
	}
	return r.states[len(r.states)-1]
}

// allStates returns every WorkspaceState pushed so far.
func (r *interruptFlowRig) allStates() []*frontendv1.WorkspaceState {
	r.mu.Lock()
	defer r.mu.Unlock()
	return append([]*frontendv1.WorkspaceState(nil), r.states...)
}

// allViews returns every ProgressView pushed so far.
func (r *interruptFlowRig) allViews() []*frontendv1.ProgressView {
	r.mu.Lock()
	defer r.mu.Unlock()
	return append([]*frontendv1.ProgressView(nil), r.views...)
}

// waitView blocks until the NEWEST pushed ProgressView satisfies ok, and
// returns it. Deadline-based, never a sleep for a guessed duration.
func (r *interruptFlowRig) waitView(what string, ok func(*frontendv1.ProgressView) bool) *frontendv1.ProgressView {
	r.t.Helper()
	var found *frontendv1.ProgressView
	waitFor(r.t, what, func() bool {
		v := r.lastView()
		if v == nil || !ok(v) {
			return false
		}
		found = v
		return true
	})
	return found
}

// waitState blocks until the NEWEST pushed WorkspaceState resolves to want.
func (r *interruptFlowRig) waitState(want frontendv1.RenderState) *frontendv1.WorkspaceState {
	r.t.Helper()
	var found *frontendv1.WorkspaceState
	waitFor(r.t, "the pushed workspace state to be "+want.String(), func() bool {
		s := r.lastState()
		if s == nil || s.GetState() != want {
			return false
		}
		found = s
		return true
	})
	return found
}

// paused reads the queue's pause posture under the manager mutex.
func (r *interruptFlowRig) paused() bool {
	r.t.Helper()
	d, err := r.m.existing(interruptFlowWorkspace)
	if err != nil {
		r.t.Fatalf("existing: %v", err)
	}
	r.m.mu.Lock()
	defer r.m.mu.Unlock()
	return d.paused
}

// entryTexts returns the queued entries' prompt texts, front to back. The
// driver is resolved BEFORE the lock because existing() takes the same mutex.
func (r *interruptFlowRig) entryTexts() []string {
	r.t.Helper()
	d, err := r.m.existing(interruptFlowWorkspace)
	if err != nil {
		r.t.Fatalf("existing: %v", err)
	}
	r.m.mu.Lock()
	defer r.m.mu.Unlock()
	out := []string{}
	for _, e := range d.queue.entries {
		out = append(out, e.text)
	}
	return out
}

// entryIDs returns the queued entries' ids, front to back.
func (r *interruptFlowRig) entryIDs() []string {
	r.t.Helper()
	d, err := r.m.existing(interruptFlowWorkspace)
	if err != nil {
		r.t.Fatalf("existing: %v", err)
	}
	r.m.mu.Lock()
	defer r.m.mu.Unlock()
	out := []string{}
	for _, e := range d.queue.entries {
		out = append(out, e.id)
	}
	return out
}

// interruptFlowQueueTexts is the texts of the LAST QueueView frame pushed to
// the frontend, which is the chip row a client is currently rendering.
func interruptFlowQueueTexts(p *fakePusher) []string {
	v := p.lastQueue()
	if v == nil {
		return nil
	}
	out := []string{}
	for _, e := range v.GetEntries() {
		out = append(out, e.GetText())
	}
	return out
}

// --- 1. the whole command path ----------------------------------------------

// THE COMPOSED PATH. A stop on a live turn produces three frontend-visible
// facts in order — an open window carrying the shim's verdict, an
// `interrupted` workspace state on that turn's own end, and both superseded
// when the next turn starts — and every one of them is read off a pushed
// frame rather than off package state.
func TestAnInterruptedTurnIsReportedThenSupersededAcrossBothFanouts(t *testing.T) {
	// Arrange — a settled workspace with a turn in flight.
	rig := newInterruptFlowRig(t)
	rig.settleReady()
	rig.apply(&corev1.TurnStarted{})
	rig.waitState(frontendv1.RenderState_RENDER_STATE_THINKING)
	rig.ackWith(corev1.InterruptOutcome_INTERRUPT_OUTCOME_INTERRUPTED)

	// Act — the user stops it, the stopped turn ends, and a new one begins.
	if err := rig.interrupt(); err != nil {
		t.Fatalf("interrupt: %v", err)
	}
	opened := rig.waitView("the interrupt window to open", func(v *frontendv1.ProgressView) bool {
		return v.GetInterrupt().GetActive()
	})
	rig.apply(&corev1.TurnEnded{StopReason: "aborted"})
	stopped := rig.waitState(frontendv1.RenderState_RENDER_STATE_INTERRUPTED)
	rig.apply(&corev1.TurnStarted{})

	// Assert — the window carried the ack's own verdict...
	if got := opened.GetInterrupt().GetOutcome(); got != corev1.InterruptOutcome_INTERRUPT_OUTCOME_INTERRUPTED {
		t.Fatalf("window outcome = %s, want INTERRUPTED", got)
	}
	if got := opened.GetInterrupt().GetSinceMs(); got != interruptFlowNowMs {
		t.Fatalf("window since_ms = %d, want %d", got, interruptFlowNowMs)
	}
	// ...the stopped turn's own end painted the outcome...
	if got := stopped.GetWorkspace(); got != interruptFlowWorkspace {
		t.Fatalf("interrupted state was pushed for %q, want %q", got, interruptFlowWorkspace)
	}
	// ...and the next turn superseded BOTH surfaces.
	rig.waitView("the interrupt window to clear on the next turn", func(v *frontendv1.ProgressView) bool {
		return !v.GetInterrupt().GetActive()
	})
	rig.waitState(frontendv1.RenderState_RENDER_STATE_THINKING)
}

// --- 2. live and connect must agree -----------------------------------------

// A CLIENT THAT CONNECTS AFTERWARDS SEES THE SAME THING. The connect-time
// StateSnapshot is assembled from exactly two sources for these two surfaces —
// the SSM's Snapshot and the progress resolver's Snapshot (server's
// ssmSnapshotProvider.Snapshot drains both) — so a settled interrupt that a
// live subscriber saw must still be there for a client that was not connected
// when it happened.
func TestASettledInterruptReadsTheSameLiveAndFromTheConnectSnapshot(t *testing.T) {
	// Arrange — a stop that settled: the window open and the turn painted.
	rig := newInterruptFlowRig(t)
	rig.settleReady()
	rig.apply(&corev1.TurnStarted{})
	rig.waitState(frontendv1.RenderState_RENDER_STATE_THINKING)
	rig.ackWith(corev1.InterruptOutcome_INTERRUPT_OUTCOME_INTERRUPTED)
	if err := rig.interrupt(); err != nil {
		t.Fatalf("interrupt: %v", err)
	}
	rig.waitView("the interrupt window to open", func(v *frontendv1.ProgressView) bool {
		return v.GetInterrupt().GetActive()
	})
	rig.apply(&corev1.TurnEnded{StopReason: "aborted"})
	liveState := rig.waitState(frontendv1.RenderState_RENDER_STATE_INTERRUPTED)
	liveView := rig.lastView()

	// Act — a fresh client connects and is handed the snapshot.
	snapStates, err := rig.ssm.Snapshot()
	if err != nil {
		t.Fatalf("ssm snapshot: %v", err)
	}
	snapViews := rig.prog.Snapshot()

	// Assert — the same open window and the same INTERRUPTED state.
	var snapView *frontendv1.ProgressView
	for _, v := range snapViews {
		if v.GetWorkspace() == interruptFlowWorkspace {
			snapView = v
		}
	}
	if snapView == nil {
		t.Fatalf("connect snapshot carried no progress view for %q", interruptFlowWorkspace)
	}
	if !proto.Equal(liveView.GetInterrupt(), snapView.GetInterrupt()) {
		t.Fatalf("snapshot window = %+v, want the live one %+v", snapView.GetInterrupt(), liveView.GetInterrupt())
	}
	var snapState *frontendv1.WorkspaceState
	for _, s := range snapStates {
		if s.GetWorkspace() == interruptFlowWorkspace {
			snapState = s
		}
	}
	if snapState == nil {
		t.Fatalf("connect snapshot carried no workspace state for %q", interruptFlowWorkspace)
	}
	if snapState.GetState() != liveState.GetState() {
		t.Fatalf("snapshot state = %s, want the live %s", snapState.GetState(), liveState.GetState())
	}
	if snapState.GetState() != frontendv1.RenderState_RENDER_STATE_INTERRUPTED {
		t.Fatalf("snapshot state = %s, want INTERRUPTED", snapState.GetState())
	}
}

// --- 3. the repaint guard ----------------------------------------------------

// ADVERSARIAL. The stop raced the turn's natural end and LOST: the shim
// answered ALREADY_COMPLETE, so nothing was stopped and the end that arrives
// afterwards is the turn's own clean one. It must paint `done`, and no frame
// on the way may have said `interrupted` — the window is the only place the
// near-miss is reported at all.
func TestAnAlreadyCompleteStopNeverRepaintsTheCleanTurn(t *testing.T) {
	// Arrange — a turn the daemon still believes is live.
	rig := newInterruptFlowRig(t)
	rig.settleReady()
	rig.apply(&corev1.TurnStarted{})
	rig.waitState(frontendv1.RenderState_RENDER_STATE_THINKING)
	rig.ackWith(corev1.InterruptOutcome_INTERRUPT_OUTCOME_ALREADY_COMPLETE)

	// Act — the stop lands too late, then the turn's own clean end arrives.
	if err := rig.interrupt(); err != nil {
		t.Fatalf("an already-complete stop is a quiet success: %v", err)
	}
	view := rig.waitView("the interrupt window to open", func(v *frontendv1.ProgressView) bool {
		return v.GetInterrupt().GetActive()
	})
	rig.apply(&corev1.TurnEnded{StopReason: "end_turn"})
	rig.waitState(frontendv1.RenderState_RENDER_STATE_DONE)

	// Assert — the window reports the near-miss...
	if got := view.GetInterrupt().GetOutcome(); got != corev1.InterruptOutcome_INTERRUPT_OUTCOME_ALREADY_COMPLETE {
		t.Fatalf("window outcome = %s, want ALREADY_COMPLETE", got)
	}
	// ...and no pushed state ever claimed the turn was interrupted.
	for _, s := range rig.allStates() {
		if s.GetState() == frontendv1.RenderState_RENDER_STATE_INTERRUPTED {
			t.Fatalf("a state frame reported INTERRUPTED; nothing was stopped, so the clean turn must stay DONE")
		}
	}
}

// --- 4. machinery stays invisible -------------------------------------------

// ADVERSARIAL, AND THE WHOLE ROUTING CLAIM. An interject sends the SAME
// Interrupt to the same shim on a held prompt's behalf. End to end, none of
// the three user-stop consequences may appear on any frontend surface: no
// window frame, no `interrupted` state, and no pause — the drain must carry
// straight on through the boundary.
func TestAnInterjectsStopReachesNoFrontendSurface(t *testing.T) {
	// Arrange — a running turn with a prompt held behind it.
	rig := newInterruptFlowRig(t)
	rig.settleReady()
	rig.apply(&corev1.TurnStarted{})
	rig.waitState(frontendv1.RenderState_RENDER_STATE_THINKING)
	if err := rig.submit("later"); err != nil {
		t.Fatalf("submit: %v", err)
	}
	waitFor(t, "the prompt to be queued", func() bool { return len(rig.entryTexts()) == 1 })

	// Act — the user forces it, which runs the interject sequence, and the
	// interrupted turn then ends.
	if err := rig.m.ForceQueueEntry(interruptFlowWorkspace, rig.entryIDs()[0]); err != nil {
		t.Fatalf("force: %v", err)
	}
	waitFor(t, "the interject's stop to reach the shim", func() bool { return rig.client.interruptCount() == 1 })
	rig.apply(&corev1.TurnEnded{StopReason: "aborted"})

	// Assert — the held prompt was delivered by the ordinary drain...
	waitFor(t, "the held prompt to be delivered", func() bool {
		got := rig.client.promptTexts()
		return len(got) == 1 && got[0] == "later"
	})
	if rig.paused() {
		t.Fatal("an interject must not pause the queue")
	}
	// ...and neither fan-out reported a user stop.
	for _, v := range rig.allViews() {
		if v.GetInterrupt().GetActive() {
			t.Fatalf("a progress frame carried an interrupt window: %+v", v.GetInterrupt())
		}
	}
	for _, s := range rig.allStates() {
		if s.GetState() == frontendv1.RenderState_RENDER_STATE_INTERRUPTED {
			t.Fatal("a state frame reported INTERRUPTED for a stop the user never commanded")
		}
	}
}

// --- 5. the queue, through frames and deliveries ----------------------------

// THE PAUSE AS THE USER EXPERIENCES IT. Two prompts are held, the user stops
// the agent, and what they type next runs ALONE while the chip row keeps
// showing both retained prompts. Its clean end resumes the drain and the
// retained prompts reach the shim in their original order.
func TestAPausedQueueRunsTheNewPromptAloneThenDrainsInOrder(t *testing.T) {
	// Arrange — a running turn with two prompts held behind it.
	rig := newInterruptFlowRig(t)
	rig.settleReady()
	rig.apply(&corev1.TurnStarted{})
	rig.waitState(frontendv1.RenderState_RENDER_STATE_THINKING)
	if err := rig.submit("first"); err != nil {
		t.Fatalf("submit first: %v", err)
	}
	if err := rig.submit("second"); err != nil {
		t.Fatalf("submit second: %v", err)
	}
	waitFor(t, "both prompts queued", func() bool { return len(rig.entryTexts()) == 2 })
	rig.ackWith(corev1.InterruptOutcome_INTERRUPT_OUTCOME_INTERRUPTED)

	// Act — the user stops the turn, it ends, and they type something new.
	if err := rig.interrupt(); err != nil {
		t.Fatalf("interrupt: %v", err)
	}
	rig.apply(&corev1.TurnEnded{StopReason: "aborted"})
	rig.waitState(frontendv1.RenderState_RENDER_STATE_INTERRUPTED)
	heldDuringPause := interruptFlowQueueTexts(rig.push)
	if err := rig.submit("urgent"); err != nil {
		t.Fatalf("submit urgent: %v", err)
	}

	// Assert — it ran alone, with both retained prompts still on the chip row.
	waitFor(t, "the new prompt to be delivered alone", func() bool {
		got := rig.client.promptTexts()
		return len(got) == 1 && got[0] == "urgent"
	})
	if len(heldDuringPause) != 2 || heldDuringPause[0] != "first" || heldDuringPause[1] != "second" {
		t.Fatalf("queue frame during the pause = %v, want both retained entries", heldDuringPause)
	}

	// Act — the lone run finishes cleanly, and each retained prompt in turn.
	rig.apply(&corev1.TurnStarted{})
	rig.apply(&corev1.TurnEnded{StopReason: "end_turn"})
	waitFor(t, "the first retained prompt to be delivered", func() bool {
		return len(rig.client.promptTexts()) == 2
	})
	rig.apply(&corev1.TurnStarted{})
	rig.apply(&corev1.TurnEnded{StopReason: "end_turn"})

	// Assert — the drain resumed in the original order and emptied the row.
	waitFor(t, "the second retained prompt to be delivered", func() bool {
		return len(rig.client.promptTexts()) == 3
	})
	got := rig.client.promptTexts()
	if got[1] != "first" || got[2] != "second" {
		t.Fatalf("deliveries = %v, want the retained prompts in their original order", got)
	}
	if rig.paused() {
		t.Fatal("a clean lone run must resume the drain")
	}
	waitFor(t, "the chip row to empty", func() bool { return len(interruptFlowQueueTexts(rig.push)) == 0 })
}

// --- 7. the undeliverable stop ----------------------------------------------

// A STOP THAT NEVER LANDED still gets a window — it is the only surface that
// reports it, since no workspace phase moves — AND it keeps its ordinary
// classified failure. The window must not be mistaken for a report that
// replaces the error path.
func TestAnUndeliverableStopOpensAFailedWindowAndStillErrors(t *testing.T) {
	// Arrange — a turn in flight and a shim that cannot deliver the stop.
	rig := newInterruptFlowRig(t)
	rig.settleReady()
	rig.apply(&corev1.TurnStarted{})
	rig.waitState(frontendv1.RenderState_RENDER_STATE_THINKING)
	rig.ackWith(corev1.InterruptOutcome_INTERRUPT_OUTCOME_FAILED)

	// Act.
	err := rig.interrupt()

	// Assert — the classified failure still fires...
	if !errors.Is(err, errclass.ErrInterruptUndelivered) {
		t.Fatalf("Interrupt = %v, want the undelivered-interrupt failure", err)
	}
	// ...and the footer reports the outcome the failure alone cannot name.
	view := rig.waitView("the failed interrupt window", func(v *frontendv1.ProgressView) bool {
		return v.GetInterrupt().GetActive()
	})
	if got := view.GetInterrupt().GetOutcome(); got != corev1.InterruptOutcome_INTERRUPT_OUTCOME_FAILED {
		t.Fatalf("window outcome = %s, want FAILED", got)
	}
	if rig.paused() {
		t.Fatal("a FAILED stop delivered nothing; the queue must keep draining")
	}
}
