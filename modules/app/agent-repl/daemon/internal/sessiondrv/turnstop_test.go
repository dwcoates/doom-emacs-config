package sessiondrv

import (
	"context"
	"errors"
	"strings"
	"testing"
	"time"

	corev1 "agentrepl/proto/agentshim/core/v1"
	frontendv1 "agentrepl/proto/agentshim/frontend/v1"
)

// ---------------------------------------------------------------------------
// THE INVARIANT: a session whose shim this daemon stops cannot leave a live
// turn standing on the agent axis.
//
// These tests pin both halves separately — the graceful drain that asks the
// shim for an honest turn end, and the funnel's guaranteed close, which holds
// on every path including a stop that failed and a shim that was already dead.
// ---------------------------------------------------------------------------

// stubInterrupter is a shim connection whose interrupt answer a test dictates,
// including one that never answers until the caller's deadline fires. It is the
// injectable boundary that makes the timeout branch deterministic.
type stubInterrupter struct {
	outcome corev1.InterruptOutcome
	err     error
	// block makes Interrupt wait for the caller's context instead of
	// answering, which is exactly what a wedged shim does.
	block bool
	calls int
}

func (s *stubInterrupter) Interrupt(ctx context.Context) (corev1.InterruptOutcome, error) {
	s.calls++
	if s.block {
		<-ctx.Done()
		return corev1.InterruptOutcome_INTERRUPT_OUTCOME_UNSPECIFIED, ctx.Err()
	}
	return s.outcome, s.err
}

// newTurnStopRig builds a Manager with no drivers: turnstop.go's two entry
// points take everything they need as arguments, so a live bring-up would only
// add noise.
func newTurnStopRig(t *testing.T) (*Manager, *fakeSpawner, *fakeApplier, *logCapture) {
	t.Helper()
	spawner := &fakeSpawner{}
	applier := &fakeApplier{}
	cl := &logCapture{}
	m, err := New(Config{
		Logf:              cl.logf,
		Push:              &fakePusher{},
		Progress:          &fakeProgress{},
		SSM:               applier,
		Spawner:           spawner,
		Locator:           fakeLocator{m: map[string]string{"ws": "s1"}},
		SeqStore:          &fakeSeqStore{seq: map[string]uint64{}},
		ClearCompactStore: newFakeClearCompactStore(),
		Registrar:         &fakeRegistrar{},
		ProtocolVersion:   "1",
		Source:            stubSource{},
		FileDiagnostics:   fakeFileDiagnosticPersister{},
	})
	if err != nil {
		t.Fatalf("New: %v", err)
	}
	t.Cleanup(func() { m.Close() })
	return m, spawner, applier, cl
}

// thinkingState is a workspace the SSM resolves as mid-turn.
func thinkingState() *frontendv1.WorkspaceState {
	return &frontendv1.WorkspaceState{
		State:      frontendv1.RenderState_RENDER_STATE_THINKING,
		TurnActive: true,
	}
}

// A live turn gets the shim's own interrupt, so its end is reported honestly
// rather than synthesized.
func TestDrainInterruptsALiveTurn(t *testing.T) {
	// Arrange.
	m, _, applier, cl := newTurnStopRig(t)
	applier.setCurrent("ws", thinkingState())
	client := &stubInterrupter{outcome: corev1.InterruptOutcome_INTERRUPT_OUTCOME_INTERRUPTED}
	// Act.
	m.drainLiveTurnForStop("ws", "s1", "hibernate_session", client)
	// Assert.
	if client.calls != 1 {
		t.Fatalf("interrupt calls = %d, want 1", client.calls)
	}
	if !cl.contains(`sessiondrv: teardown turn drain interrupt DELIVERED ws="ws" session=s1 path=hibernate_session outcome=INTERRUPT_OUTCOME_INTERRUPTED`) {
		t.Fatalf("missing the canonical delivered record; log:\n%s", strings.Join(cl.lines, "\n"))
	}
}

// An idle workspace is spared the round trip.
func TestDrainSkipsAWorkspaceWithNoLiveTurn(t *testing.T) {
	// Arrange.
	m, _, applier, cl := newTurnStopRig(t)
	applier.setCurrent("ws", &frontendv1.WorkspaceState{State: frontendv1.RenderState_RENDER_STATE_DONE})
	client := &stubInterrupter{}
	// Act.
	m.drainLiveTurnForStop("ws", "s1", "hibernate", client)
	// Assert.
	if client.calls != 0 {
		t.Fatalf("interrupt calls = %d, want 0 — nothing was running", client.calls)
	}
	if !cl.contains(`sessiondrv: teardown turn drain SKIPPED ws="ws" session=s1 path=hibernate state=RENDER_STATE_DONE`) {
		t.Fatalf("missing the canonical skip record; log:\n%s", strings.Join(cl.lines, "\n"))
	}
}

// A workspace the log has never seen has no turn to interrupt.
func TestDrainSkipsAnUnknownWorkspace(t *testing.T) {
	// Arrange — nothing is resolved for this workspace.
	m, _, _, cl := newTurnStopRig(t)
	client := &stubInterrupter{}
	// Act.
	m.drainLiveTurnForStop("ws", "s1", "hibernate", client)
	// Assert.
	if client.calls != 0 {
		t.Fatalf("interrupt calls = %d, want 0", client.calls)
	}
	if !cl.contains("the log knows nothing about this workspace") {
		t.Fatalf("missing the canonical unknown-workspace record; log:\n%s", strings.Join(cl.lines, "\n"))
	}
}

// THE ALREADY-DEAD-SHIM BRANCH. There is no connection to interrupt over, and
// the drain says so instead of pretending it tried.
func TestDrainSkipsWhenTheShimConnectionIsAlreadyGone(t *testing.T) {
	// Arrange.
	m, _, applier, cl := newTurnStopRig(t)
	applier.setCurrent("ws", thinkingState())
	// Act.
	m.drainLiveTurnForStop("ws", "s1", "driver_exit", nil)
	// Assert.
	if !cl.contains(`sessiondrv: teardown turn drain SKIPPED ws="ws" session=s1 path=driver_exit — there is no live shim connection`) {
		t.Fatalf("missing the canonical no-connection record; log:\n%s", strings.Join(cl.lines, "\n"))
	}
}

// THE TIMEOUT BRANCH. A shim that stops answering is logged as the failure it
// is; the teardown then closes the axis itself.
func TestDrainReportsAnInterruptThatNeverAnswers(t *testing.T) {
	// Arrange — the interrupt blocks until the drain's own deadline fires.
	m, _, applier, cl := newTurnStopRig(t)
	applier.setCurrent("ws", thinkingState())
	client := &stubInterrupter{block: true}
	m.mu.Lock()
	m.interruptDrain = time.Millisecond
	m.mu.Unlock()
	// Act.
	m.drainLiveTurnForStop("ws", "s1", "hibernate_session", client)
	// Assert.
	if client.calls != 1 {
		t.Fatalf("interrupt calls = %d, want 1", client.calls)
	}
	if !cl.contains(`sessiondrv: teardown turn drain interrupt FAILED ws="ws" session=s1 path=hibernate_session`) ||
		!cl.contains("timeout=1ms: context deadline exceeded") {
		t.Fatalf("missing the canonical interrupt-failure record; log:\n%s", strings.Join(cl.lines, "\n"))
	}
}

// A nack is the same kind of answer as a timeout here, and neither stops the
// teardown.
func TestDrainReportsANackedInterrupt(t *testing.T) {
	// Arrange.
	m, _, applier, cl := newTurnStopRig(t)
	applier.setCurrent("ws", thinkingState())
	client := &stubInterrupter{err: errors.New("shim nacked")}
	// Act.
	m.drainLiveTurnForStop("ws", "s1", "hibernate_session", client)
	// Assert.
	if !cl.contains("shim nacked") ||
		!cl.contains("teardown turn drain interrupt FAILED") {
		t.Fatalf("missing the canonical interrupt-failure record; log:\n%s", strings.Join(cl.lines, "\n"))
	}
}

// A state read that fails interrupts ANYWAY. The read is advisory — it exists
// to spare an idle workspace a round trip — so an unreadable log must not cost
// the shim's own turn end.
func TestDrainInterruptsAnywayWhenTheStateReadFails(t *testing.T) {
	// Arrange.
	m, _, applier, cl := newTurnStopRig(t)
	applier.currentErr = errors.New("state db is gone")
	client := &stubInterrupter{outcome: corev1.InterruptOutcome_INTERRUPT_OUTCOME_ALREADY_COMPLETE}
	// Act.
	m.drainLiveTurnForStop("ws", "s1", "hibernate_session", client)
	// Assert.
	if client.calls != 1 {
		t.Fatalf("interrupt calls = %d, want 1 — the read is advisory, not an authorization", client.calls)
	}
	if !cl.contains("teardown turn drain state read FAILED") || !cl.contains("state db is gone") {
		t.Fatalf("missing the canonical state-read-failure record; log:\n%s", strings.Join(cl.lines, "\n"))
	}
}

// The guarantee: the stop closes the axis, and says so as a SYNTHESIZED close
// because no shim-sourced end reached the log.
func TestStopShimSettlingTurnSynthesizesTheCloseWhenNothingElseDid(t *testing.T) {
	// Arrange.
	m, spawner, applier, cl := newTurnStopRig(t)
	applier.staleTurnClosed = true
	// Act.
	if err := m.stopShimSettlingTurn("ws", "s1", "hibernate_session", true); err != nil {
		t.Fatalf("stopShimSettlingTurn: %v", err)
	}
	// Assert.
	if got := spawner.stoppedSessions(); len(got) != 1 || got[0] != "s1" {
		t.Fatalf("stopped = %v, want [s1]", got)
	}
	closes := applier.staleTurnClosesApplied()
	want := staleTurnCloseCall{workspace: "ws", sessionID: "s1", reason: "hibernate_session", soleDriver: true}
	if len(closes) != 1 || closes[0] != want {
		t.Fatalf("closes = %+v, want exactly %+v", closes, want)
	}
	if !cl.contains(`sessiondrv: teardown axis close SYNTHESIZED ws="ws" session=s1 path=hibernate_session`) {
		t.Fatalf("missing the canonical synthesized-close record; log:\n%s", strings.Join(cl.lines, "\n"))
	}
}

// An honest shim-sourced close is reported DIFFERENTLY, which is the whole
// point of the SSM answering whether it wrote anything.
func TestStopShimSettlingTurnReportsAnHonestCloseDistinguishably(t *testing.T) {
	// Arrange — the SSM found nothing stale, so the shim's own end got there.
	m, _, applier, cl := newTurnStopRig(t)
	applier.staleTurnClosed = false
	// Act.
	if err := m.stopShimSettlingTurn("ws", "s1", "hibernate", true); err != nil {
		t.Fatalf("stopShimSettlingTurn: %v", err)
	}
	// Assert.
	if !cl.contains(`sessiondrv: teardown axis close NOT NEEDED ws="ws" session=s1 path=hibernate`) {
		t.Fatalf("missing the canonical honest-close record; log:\n%s", strings.Join(cl.lines, "\n"))
	}
	if cl.contains("SYNTHESIZED") {
		t.Fatalf("an honest close was reported as a synthesized one; log:\n%s", strings.Join(cl.lines, "\n"))
	}
}

// A STOP THAT FAILED STILL CLOSES THE AXIS. The turn is no more reportable
// after a failed SIGTERM than after a successful one, and the stop's error is
// still returned unchanged.
func TestStopShimSettlingTurnClosesTheAxisEvenWhenTheStopFails(t *testing.T) {
	// Arrange.
	m, spawner, applier, cl := newTurnStopRig(t)
	spawner.stopErr = errors.New("no such process")
	applier.staleTurnClosed = true
	// Act.
	err := m.stopShimSettlingTurn("ws", "s1", "hibernate_session", true)
	// Assert.
	if err == nil || !strings.Contains(err.Error(), "no such process") {
		t.Fatalf("err = %v, want the stop failure returned unchanged", err)
	}
	if len(applier.staleTurnClosesApplied()) != 1 {
		t.Fatalf("closes = %+v, want exactly one — a failed stop must not skip the close", applier.staleTurnClosesApplied())
	}
	if !cl.contains(`sessiondrv: teardown shim stop FAILED ws="ws" session=s1 path=hibernate_session`) {
		t.Fatalf("missing the canonical stop-failure record; log:\n%s", strings.Join(cl.lines, "\n"))
	}
}

// A close that itself fails is logged loudly and never folded into the stop's
// error, which callers read as "no live shim" versus a real failure.
func TestStopShimSettlingTurnLogsAFailedCloseWithoutFailingTheStop(t *testing.T) {
	// Arrange.
	m, _, applier, cl := newTurnStopRig(t)
	applier.staleTurnErr = errors.New("state db is gone")
	// Act.
	err := m.stopShimSettlingTurn("ws", "s1", "hibernate", true)
	// Assert.
	if err != nil {
		t.Fatalf("err = %v, want nil — the shim did stop", err)
	}
	if !cl.contains(`sessiondrv: teardown axis close FAILED ws="ws" session=s1 path=hibernate sole_driver=true: state db is gone`) {
		t.Fatalf("missing the canonical close-failure record; log:\n%s", strings.Join(cl.lines, "\n"))
	}
}

// A session-scoped stop with no workspace behind it has no axis to close, and
// the gap is loud rather than silent.
func TestStopShimSettlingTurnSkipsTheCloseWithNoWorkspace(t *testing.T) {
	// Arrange.
	m, _, applier, cl := newTurnStopRig(t)
	// Act.
	if err := m.stopShimSettlingTurn("", "s1", "hibernate_session", true); err != nil {
		t.Fatalf("stopShimSettlingTurn: %v", err)
	}
	// Assert.
	if got := applier.staleTurnClosesApplied(); len(got) != 0 {
		t.Fatalf("closes = %+v, want none", got)
	}
	if !cl.contains("sessiondrv: teardown axis close SKIPPED session=s1 path=hibernate_session") {
		t.Fatalf("missing the canonical skipped-close record; log:\n%s", strings.Join(cl.lines, "\n"))
	}
}

// The invariant reaches the workspace-scoped hibernation, end to end.
func TestHibernateClosesTheAxisThroughTheFunnel(t *testing.T) {
	// Arrange.
	m, _, applier, _ := newClosingRig(t)
	applier.staleTurnClosed = true
	// Act.
	if err := m.Hibernate("ws"); err != nil {
		t.Fatalf("Hibernate: %v", err)
	}
	// Assert.
	closes := applier.staleTurnClosesApplied()
	want := staleTurnCloseCall{workspace: "ws", sessionID: "s1", reason: "hibernate", soleDriver: true}
	if len(closes) != 1 || closes[0] != want {
		t.Fatalf("closes = %+v, want exactly %+v", closes, want)
	}
}

// The session-scoped stand-down — the delete and supersede path, which carries
// NO settled guard and is therefore the one that stranded turns.
func TestHibernateSessionClosesTheAxisThroughTheFunnel(t *testing.T) {
	// Arrange.
	m, _, applier, _ := newClosingRig(t)
	applier.staleTurnClosed = true
	// Act.
	if err := m.HibernateSession("ws", "s1"); err != nil {
		t.Fatalf("HibernateSession: %v", err)
	}
	// Assert.
	closes := applier.staleTurnClosesApplied()
	want := staleTurnCloseCall{workspace: "ws", sessionID: "s1", reason: "hibernate_session", soleDriver: true}
	if len(closes) != 1 || closes[0] != want {
		t.Fatalf("closes = %+v, want exactly %+v", closes, want)
	}
}

// A stop aimed at a SUPERSEDED record while a different session drives the
// workspace passes soleDriver=false, so it can never spend a claim that may
// belong to the replacement.
func TestSupersededSessionStopNeverClaimsSoleDriver(t *testing.T) {
	// Arrange — s1 drives the workspace; the stop names an older record.
	m, _, applier, _ := newClosingRig(t)
	// Act.
	if err := m.HibernateSession("ws", "s-old"); err != nil {
		t.Fatalf("HibernateSession: %v", err)
	}
	// Assert.
	closes := applier.staleTurnClosesApplied()
	want := staleTurnCloseCall{workspace: "ws", sessionID: "s-old", reason: "hibernate_session_superseded", soleDriver: false}
	if len(closes) != 1 || closes[0] != want {
		t.Fatalf("closes = %+v, want exactly %+v", closes, want)
	}
}
