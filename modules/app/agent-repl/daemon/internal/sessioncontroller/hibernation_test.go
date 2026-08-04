package sessioncontroller

import (
	"context"
	"errors"
	"fmt"
	"sync"
	"testing"

	corev1 "agentrepl/proto/agentshim/core/v1"
	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"claude-repld/internal/registry"
)

// fakeHibernations is an in-memory HibernationRegistrar. It records every
// write so a test can assert the SINGLE-TRANSITION invariant by counting them
// rather than by inspecting only the final state, which two writes agreeing on
// the same cause would hide.
type fakeHibernations struct {
	mu      sync.Mutex
	details map[string]registry.HibernationDetail
	writes  []registry.HibernationDetail
	turnEnd map[string]int64
	// writeErr, when set, fails every HibernationChanged.
	writeErr error
}

func newFakeHibernations() *fakeHibernations {
	return &fakeHibernations{
		details: map[string]registry.HibernationDetail{},
		turnEnd: map[string]int64{},
	}
}

func (f *fakeHibernations) HibernationChanged(sessionID string, detail registry.HibernationDetail) error {
	f.mu.Lock()
	defer f.mu.Unlock()
	if f.writeErr != nil {
		return f.writeErr
	}
	f.details[sessionID] = detail
	f.writes = append(f.writes, detail)
	return nil
}

func (f *fakeHibernations) HibernationOf(sessionID string) (registry.HibernationDetail, bool) {
	f.mu.Lock()
	defer f.mu.Unlock()
	d, ok := f.details[sessionID]
	return d, ok
}

func (f *fakeHibernations) TurnEndObserved(sessionID string, atMs int64) {
	f.mu.Lock()
	defer f.mu.Unlock()
	if atMs > f.turnEnd[sessionID] {
		f.turnEnd[sessionID] = atMs
	}
}

func (f *fakeHibernations) setAsleep(sessionID string, detail registry.HibernationDetail) {
	f.mu.Lock()
	defer f.mu.Unlock()
	f.details[sessionID] = detail
}

func (f *fakeHibernations) writeCount() int {
	f.mu.Lock()
	defer f.mu.Unlock()
	return len(f.writes)
}

func (f *fakeHibernations) lastTurnEnd(sessionID string) int64 {
	f.mu.Lock()
	defer f.mu.Unlock()
	return f.turnEnd[sessionID]
}

// newHibernationRig is newWiredRig plus a hibernation registrar and a session
// settled enough for the teardown's own gate to admit.
func newHibernationRig(t *testing.T) (*Manager, *fakeApplier, *fakeHibernations) {
	t.Helper()
	m, applier, _ := newWiredRig(t)
	hib := newFakeHibernations()
	m.cfg.Hibernations = hib
	if err := m.Ensure("ws"); err != nil {
		t.Fatalf("Ensure: %v", err)
	}
	waitForWirings(applier, 1)
	m.onConnected("ws", "s1", &corev1.ShimHello{})
	applier.setCurrent("ws", &frontendv1.WorkspaceState{State: frontendv1.RenderState_RENDER_STATE_READY})
	return m, applier, hib
}

// ---------------------------------------------------------------------------
// The one transition
// ---------------------------------------------------------------------------

// EVERY CAUSE REACHES THE SAME TRANSITION, and each one's own evidence lands
// on the record. A cause that stored only its name would report the CURRENT
// config's numbers for a sleep taken under different ones.
func TestHibernateWithCausePersistsEachCausesEvidence(t *testing.T) {
	tests := []struct {
		name    string
		account registry.HibernationDetail
	}{
		{
			name:    "idle cutoff carries the cutoff that tripped",
			account: registry.HibernationDetail{Cause: registry.HibernationCauseIdleCutoff, CutoffMs: 21_600_000},
		},
		{
			name:    "forced carries only its cause",
			account: registry.HibernationDetail{Cause: registry.HibernationCauseForced},
		},
		{
			name:    "cache expired carries the elapsed time and the TTL it exceeded",
			account: registry.HibernationDetail{Cause: registry.HibernationCauseCacheExpired, ElapsedMs: 7_200_000, TTLMs: 3_600_000},
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			m, _, hib := newHibernationRig(t)

			// Act.
			if err := m.HibernateWithCause("ws", tc.account); err != nil {
				t.Fatalf("HibernateWithCause: %v", err)
			}

			// Assert.
			got, ok := hib.HibernationOf("s1")
			if !ok {
				t.Fatal("no hibernation detail was persisted")
			}
			if got.Cause != tc.account.Cause || got.CutoffMs != tc.account.CutoffMs ||
				got.ElapsedMs != tc.account.ElapsedMs || got.TTLMs != tc.account.TTLMs {
				t.Fatalf("persisted detail = %+v, want the cause's own evidence %+v", got, tc.account)
			}
		})
	}
}

// The transition STAMPS the instant when the caller did not, so the revival
// gate can always say how long the session has been asleep.
func TestHibernateWithCauseStampsSinceMs(t *testing.T) {
	// Arrange.
	m, _, hib := newHibernationRig(t)

	// Act.
	if err := m.HibernateWithCause("ws", registry.HibernationDetail{Cause: registry.HibernationCauseForced}); err != nil {
		t.Fatalf("HibernateWithCause: %v", err)
	}

	// Assert.
	got, _ := hib.HibernationOf("s1")
	if got.SinceMs <= 0 {
		t.Fatalf("since_ms = %d, want the transition's own instant", got.SinceMs)
	}
}

// THE SINGLE-TRANSITION INVARIANT. A forced hibernate and the sweeper's idle
// cutoff can genuinely arrive at once. Exactly one may take the transition and
// write an account; the other is refused, so the record cannot end up telling
// whichever story landed second.
func TestHibernateWithCauseRacingCausesTakeOneTransition(t *testing.T) {
	// Arrange.
	m, _, hib := newHibernationRig(t)
	causes := []string{registry.HibernationCauseForced, registry.HibernationCauseIdleCutoff}
	errs := make([]error, len(causes))
	var wg sync.WaitGroup

	// Act.
	wg.Add(len(causes))
	for i, cause := range causes {
		go func() {
			defer wg.Done()
			errs[i] = m.HibernateWithCause("ws", registry.HibernationDetail{Cause: cause})
		}()
	}
	wg.Wait()

	// Assert.
	succeeded := 0
	for _, err := range errs {
		if err == nil {
			succeeded++
		}
	}
	if succeeded != 1 {
		t.Fatalf("%d of %d racing causes succeeded, want exactly one", succeeded, len(causes))
	}
	if n := hib.writeCount(); n != 1 {
		t.Fatalf("%d durable hibernation writes, want exactly one", n)
	}
}

// A session already asleep refuses a second transition by name, so a caller can
// tell "already done" from a real failure.
func TestHibernateWithCauseRefusesAnAlreadyHibernatedSession(t *testing.T) {
	// Arrange.
	m, _, hib := newHibernationRig(t)
	hib.setAsleep("s1", registry.HibernationDetail{Cause: registry.HibernationCauseForced, SinceMs: 42})

	// Act.
	err := m.HibernateWithCause("ws", registry.HibernationDetail{Cause: registry.HibernationCauseIdleCutoff})

	// Assert.
	if !errors.Is(err, ErrAlreadyHibernated) {
		t.Fatalf("HibernateWithCause on a sleeping session = %v, want ErrAlreadyHibernated", err)
	}
}

// A cause this binary cannot name is refused BEFORE anything is stopped: a
// sleep the revival gate could not explain is not one to take.
func TestHibernateWithCauseRefusesAnUnknownCause(t *testing.T) {
	// Arrange.
	m, _, hib := newHibernationRig(t)

	// Act.
	err := m.HibernateWithCause("ws", registry.HibernationDetail{Cause: "invented"})

	// Assert.
	if err == nil {
		t.Fatal("HibernateWithCause with an unknown cause = nil, want a refusal")
	}
	if n := hib.writeCount(); n != 0 {
		t.Fatalf("%d durable writes for a refused transition, want none", n)
	}
}

// A LIVE TURN REFUSES THE TRANSITION, and nothing is persisted. The teardown's
// settled gate is what makes a teal tab over a working agent unreachable, and
// the transition must not record a sleep the teardown declined to take.
func TestHibernateWithCauseRefusesAnUnsettledWorkspace(t *testing.T) {
	// Arrange.
	m, applier, hib := newHibernationRig(t)
	applier.setCurrent("ws", &frontendv1.WorkspaceState{
		State: frontendv1.RenderState_RENDER_STATE_THINKING, TurnActive: true,
	})

	// Act.
	err := m.HibernateWithCause("ws", registry.HibernationDetail{Cause: registry.HibernationCauseForced})

	// Assert.
	if !errors.Is(err, ErrNotSettled) {
		t.Fatalf("HibernateWithCause over a live turn = %v, want ErrNotSettled", err)
	}
	if n := hib.writeCount(); n != 0 {
		t.Fatalf("%d durable writes for a refused transition, want none", n)
	}
}

// Without a registrar the transition REFUSES rather than stopping a shim it
// could not record the sleep of. An unrecorded sleep is revived implicitly by
// the next daemon, which is the silent un-sleeping the durable flag prevents.
func TestHibernateWithCauseRefusesWithoutARegistrar(t *testing.T) {
	// Arrange.
	m, applier, _ := newWiredRig(t)
	if err := m.Ensure("ws"); err != nil {
		t.Fatalf("Ensure: %v", err)
	}
	waitForWirings(applier, 1)
	m.onConnected("ws", "s1", &corev1.ShimHello{})
	applier.setCurrent("ws", &frontendv1.WorkspaceState{State: frontendv1.RenderState_RENDER_STATE_READY})

	// Act.
	err := m.HibernateWithCause("ws", registry.HibernationDetail{Cause: registry.HibernationCauseForced})

	// Assert.
	if err == nil {
		t.Fatal("HibernateWithCause with no registrar = nil, want a refusal")
	}
}

// A durable-mark failure is SURFACED, not swallowed. The shim is already down
// and cannot be un-stopped, so the honest report is the loud one.
func TestHibernateWithCauseSurfacesADurableMarkFailure(t *testing.T) {
	// Arrange.
	m, _, hib := newHibernationRig(t)
	hib.writeErr = fmt.Errorf("state store is unavailable")

	// Act.
	err := m.HibernateWithCause("ws", registry.HibernationDetail{Cause: registry.HibernationCauseForced})

	// Assert.
	if err == nil {
		t.Fatal("HibernateWithCause with a failing registrar = nil, want the failure surfaced")
	}
}

// THE TRANSITION ENDS KEEP-ALIVE ELIGIBILITY BY CONSTRUCTION: it removes the
// workspace from the live fleet, and the ping path requires a live controller.
// "Hibernated but still pinging" is unrepresentable rather than merely guarded.
func TestHibernateWithCauseLeavesNoLiveController(t *testing.T) {
	// Arrange.
	m, _, _ := newHibernationRig(t)

	// Act.
	if err := m.HibernateWithCause("ws", registry.HibernationDetail{Cause: registry.HibernationCauseIdleCutoff}); err != nil {
		t.Fatalf("HibernateWithCause: %v", err)
	}

	// Assert.
	if m.Live("ws") {
		t.Fatal("the workspace still has a live session controller after hibernation")
	}
}

// ---------------------------------------------------------------------------
// The revival gate
// ---------------------------------------------------------------------------

// A hibernated session's prompt is a LOUD NACK. Before the gate existed every
// one of the submission paths funnelled into a bring-up that silently revived
// the session, so the revival choice was never put to the user at all.
func TestSubmitPromptRefusedOnAHibernatedSession(t *testing.T) {
	// Arrange.
	m, _, hib := newHibernationRig(t)
	hib.setAsleep("s1", registry.HibernationDetail{Cause: registry.HibernationCauseIdleCutoff, SinceMs: 99})

	// Act.
	err := m.SubmitPrompt(context.Background(), "ws", "req-1", "hello", "", corev1.PromptOrigin_PROMPT_ORIGIN_USER_SENT)

	// Assert.
	if !errors.Is(err, ErrHibernated) {
		t.Fatalf("SubmitPrompt on a hibernated session = %v, want ErrHibernated", err)
	}
}

// A REHYDRATED hibernated session meets the gate too. The daemon that marked
// the sleep is gone and no in-memory flag survived it; the record is the only
// thing that knows, which is exactly why the gate reads the record.
func TestRehydratedHibernatedSessionStillMeetsTheGate(t *testing.T) {
	// Arrange: a manager that never took the transition itself, standing in for
	// a daemon that booted onto an already-sleeping record.
	m, _ := newTestManager(t, fakeLocator{m: map[string]string{"ws": "s1"}}, &fakeSpawner{})
	hib := newFakeHibernations()
	hib.setAsleep("s1", registry.HibernationDetail{Cause: registry.HibernationCauseCacheExpired, SinceMs: 7})
	m.cfg.Hibernations = hib

	// Act.
	err := m.SubmitPrompt(context.Background(), "ws", "req-1", "hello", "", corev1.PromptOrigin_PROMPT_ORIGIN_USER_SENT)

	// Assert.
	if !errors.Is(err, ErrHibernated) {
		t.Fatalf("SubmitPrompt after rehydration = %v, want ErrHibernated", err)
	}
}

// THE GATE REFUSES BEFORE THE BRING-UP. A refusal that had already spawned a
// shim would have un-slept the very session it was refusing for.
func TestRevivalGateRefusesWithoutSpawningAShim(t *testing.T) {
	// Arrange.
	spawner := &fakeSpawner{}
	m, _ := newTestManager(t, fakeLocator{m: map[string]string{"ws": "s1"}}, spawner)
	hib := newFakeHibernations()
	hib.setAsleep("s1", registry.HibernationDetail{Cause: registry.HibernationCauseForced, SinceMs: 1})
	m.cfg.Hibernations = hib

	// Act.
	_ = m.SubmitPrompt(context.Background(), "ws", "req-1", "hello", "", corev1.PromptOrigin_PROMPT_ORIGIN_USER_SENT)

	// Assert.
	if m.Live("ws") {
		t.Fatal("the refused prompt brought a session controller up; the gate must refuse before ensure()")
	}
}

// A NON-USER PRODUCER gets the same loud error rather than an implicit
// revival: a workspace-create's initial prompt belongs to somebody who is not
// looking at the revival gate.
func TestWorkspaceInitialPromptRefusedOnAHibernatedSession(t *testing.T) {
	// Arrange.
	m, _, hib := newHibernationRig(t)
	hib.setAsleep("s1", registry.HibernationDetail{Cause: registry.HibernationCauseForced, SinceMs: 5})

	// Act.
	err := m.SubmitWorkspaceInitialPrompt(context.Background(), "ws", "job-1", "go", "")

	// Assert.
	if !errors.Is(err, ErrHibernated) {
		t.Fatalf("SubmitWorkspaceInitialPrompt on a hibernated session = %v, want ErrHibernated", err)
	}
}

// An AWAKE session is not gated. The zero hibernation detail is the ordinary
// case and must cost a prompt nothing.
func TestSubmitPromptUngatedOnAnAwakeSession(t *testing.T) {
	// Arrange.
	m, _, _ := newHibernationRig(t)

	// Act.
	err := m.SubmitPrompt(context.Background(), "ws", "req-1", "hello", "", corev1.PromptOrigin_PROMPT_ORIGIN_USER_SENT)

	// Assert.
	if errors.Is(err, ErrHibernated) {
		t.Fatalf("SubmitPrompt on an awake session = %v, want no revival gate", err)
	}
}

// clearHibernation is the revival's ONE write, and it retires the sleep so the
// gate stops standing.
func TestClearHibernationRetiresTheGate(t *testing.T) {
	// Arrange.
	m, _, hib := newHibernationRig(t)
	hib.setAsleep("s1", registry.HibernationDetail{Cause: registry.HibernationCauseForced, SinceMs: 3})

	// Act.
	if err := m.clearHibernation("ws", "s1"); err != nil {
		t.Fatalf("clearHibernation: %v", err)
	}

	// Assert.
	if detail, asleep := hib.HibernationOf("s1"); asleep && detail.Cause != "" {
		t.Fatalf("hibernation detail = %+v after clearing, want the sleep retired", detail)
	}
}
