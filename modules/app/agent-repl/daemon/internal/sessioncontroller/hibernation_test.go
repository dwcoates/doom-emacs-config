package sessioncontroller

import (
	"context"
	"errors"
	"fmt"
	"sync"
	"testing"
	"time"

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
	// writeSeen, when non-nil, receives every attempted HibernationChanged —
	// successful or failed. It is the DETERMINISTIC SEAM an asynchronous
	// hibernation is awaited on: a test that has dispatched one blocks on this
	// channel rather than sleeping and hoping.
	writeSeen chan registry.HibernationDetail
	// onWrite runs INSIDE HibernationChanged, before anything is recorded and
	// before the fake's own mutex is taken, so a test can snapshot the manager's
	// state at the exact instant the sleep is being made durable. Taking no lock
	// here is what lets the snapshot call back into the manager.
	onWrite func()
}

func newFakeHibernations() *fakeHibernations {
	return &fakeHibernations{
		details: map[string]registry.HibernationDetail{},
		turnEnd: map[string]int64{},
	}
}

func (f *fakeHibernations) HibernationChanged(sessionID string, detail registry.HibernationDetail) error {
	if f.onWrite != nil {
		f.onWrite()
	}
	if f.writeSeen != nil {
		f.writeSeen <- detail
	}
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

func (f *fakeHibernations) LastTurnEndOf(sessionID string) (int64, bool) {
	f.mu.Lock()
	defer f.mu.Unlock()
	atMs, ok := f.turnEnd[sessionID]
	return atMs, ok
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

// newClockedHibernationRig is newHibernationRig over an explicit Config.Now,
// so a test can put the gate on the SAME injected clock the idle sweeper
// measures with. A nil now leaves the field unset (wall clock).
func newClockedHibernationRig(t *testing.T, now func() int64) (*Manager, *fakeHibernations) {
	t.Helper()
	m, _ := newClockedTestManager(t, fakeLocator{m: map[string]string{"ws": "s1"}}, &fakeSpawner{}, now)
	applier := m.cfg.SSM.(*fakeApplier)
	hib := newFakeHibernations()
	m.cfg.Hibernations = hib
	if err := m.Ensure("ws"); err != nil {
		t.Fatalf("Ensure: %v", err)
	}
	waitForWirings(applier, 1)
	m.onConnected("ws", "s1", &corev1.ShimHello{})
	applier.setCurrent("ws", &frontendv1.WorkspaceState{State: frontendv1.RenderState_RENDER_STATE_READY})
	return m, hib
}

// ---------------------------------------------------------------------------
// ONE CLOCK AUTHORITY. The idle sweeper decides against server.Config.Now and
// this gate re-validates that decision; a gate reading its own clock refuses
// every sleep the sweeper legitimately took under an injected one.
// ---------------------------------------------------------------------------

// THE GATE READS THE INJECTED CLOCK. The turn ended at real wall-clock now, so
// a gate on its own wall clock measures ~0ms elapsed and refuses; on the
// sweeper's clock it measures the three hours the sweeper measured and admits.
func TestHibernateWithCauseReValidatesAgainstTheInjectedClock(t *testing.T) {
	// Arrange.
	lastEndMs := time.Now().UnixMilli()
	const threeHoursMs = int64(3 * 60 * 60 * 1000)
	m, hib := newClockedHibernationRig(t, func() int64 { return lastEndMs + threeHoursMs })
	hib.TurnEndObserved("s1", lastEndMs)

	// Act.
	err := m.HibernateWithCause("ws", registry.HibernationDetail{
		Cause: registry.HibernationCauseIdleCutoff, CutoffMs: 60 * 60 * 1000, ElapsedMs: threeHoursMs,
	})

	// Assert.
	if err != nil {
		t.Fatalf("HibernateWithCause under the sweeper's own clock = %v, want the sleep taken", err)
	}
}

// THE WALL CLOCK IS STILL THE DEFAULT. Production leaves nothing unset, but an
// unset seam must keep measuring real elapsed time rather than degrading to a
// zero clock that would call every session infinitely idle.
func TestHibernateWithCauseDefaultsToTheWallClock(t *testing.T) {
	// Arrange — a turn that ended three hours ago in real time.
	const threeHoursMs = int64(3 * 60 * 60 * 1000)
	m, hib := newClockedHibernationRig(t, nil)
	hib.TurnEndObserved("s1", time.Now().UnixMilli()-threeHoursMs)

	// Act.
	if err := m.HibernateWithCause("ws", registry.HibernationDetail{
		Cause: registry.HibernationCauseIdleCutoff, CutoffMs: 60 * 60 * 1000, ElapsedMs: threeHoursMs,
	}); err != nil {
		t.Fatalf("HibernateWithCause: %v", err)
	}

	// Assert.
	got, ok := hib.HibernationOf("s1")
	if !ok {
		t.Fatal("no hibernation detail was persisted")
	}
	if got.ElapsedMs < threeHoursMs || got.ElapsedMs > threeHoursMs+time.Minute.Milliseconds() {
		t.Fatalf("persisted elapsed_ms = %d, want the wall-clock reading ~%d", got.ElapsedMs, threeHoursMs)
	}
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

// THE STALE-SNAPSHOT REFUSAL. The sweeper decides on a registry snapshot; a
// session that finished a turn between that snapshot and the claim is active,
// and the claim's own fresh read is what refuses the sleep.
func TestHibernateWithCauseRefusesAStaleAutomaticDecision(t *testing.T) {
	// Arrange — the decision was taken against a one-hour TTL, but the durable
	// record now says a turn ended one minute ago.
	m, _, hib := newHibernationRig(t)
	const nowMs = int64(10_000_000_000)
	m.now = func() int64 { return nowMs }
	hib.TurnEndObserved("s1", nowMs-60_000)

	// Act.
	err := m.HibernateWithCause("ws", registry.HibernationDetail{
		Cause: registry.HibernationCauseCacheExpired, TTLMs: 3_600_000, ElapsedMs: 3_700_000,
	})

	// Assert.
	if !errors.Is(err, ErrHibernationNoLongerIdle) {
		t.Fatalf("HibernateWithCause on a session that has since worked = %v, want ErrHibernationNoLongerIdle", err)
	}
}

// NOTHING IS PERSISTED BY THE REFUSAL. A durable sleep written for a session
// that had just worked is the exact outcome the re-read exists to prevent, so
// the record must be untouched.
func TestHibernateWithCauseWritesNothingOnAStaleDecision(t *testing.T) {
	// Arrange.
	m, _, hib := newHibernationRig(t)
	const nowMs = int64(10_000_000_000)
	m.now = func() int64 { return nowMs }
	hib.TurnEndObserved("s1", nowMs-60_000)

	// Act.
	_ = m.HibernateWithCause("ws", registry.HibernationDetail{
		Cause: registry.HibernationCauseIdleCutoff, CutoffMs: 21_600_000,
	})

	// Assert.
	if got := hib.writeCount(); got != 0 {
		t.Fatalf("hibernation writes = %d, want 0; a refused transition must leave the record exactly as it found it", got)
	}
}

// THE ACCOUNT COMES FROM THE FRESH READ. A sleep that really is due still
// records the elapsed measured at claim time, not the sweep's older figure,
// so the revival gate reports what was true when the session was stopped.
func TestHibernateWithCauseRecordsTheFreshElapsed(t *testing.T) {
	// Arrange — the sweep measured two hours; the durable instant says three.
	m, _, hib := newHibernationRig(t)
	const nowMs = int64(10_000_000_000)
	m.now = func() int64 { return nowMs }
	hib.TurnEndObserved("s1", nowMs-10_800_000)

	// Act.
	if err := m.HibernateWithCause("ws", registry.HibernationDetail{
		Cause: registry.HibernationCauseCacheExpired, TTLMs: 3_600_000, ElapsedMs: 7_200_000,
	}); err != nil {
		t.Fatalf("HibernateWithCause: %v", err)
	}

	// Assert.
	got, _ := hib.HibernationOf("s1")
	if got.ElapsedMs != 10_800_000 {
		t.Fatalf("persisted elapsed_ms = %d, want the claim-time measurement 10800000", got.ElapsedMs)
	}
}

// A FORCED HIBERNATION IS NOT RE-VALIDATED. Idleness was never its premise:
// the user asked for the sleep over a session they are looking at.
func TestHibernateWithCauseDoesNotReValidateAForcedCause(t *testing.T) {
	// Arrange — a turn that ended one second ago.
	m, _, hib := newHibernationRig(t)
	const nowMs = int64(10_000_000_000)
	m.now = func() int64 { return nowMs }
	hib.TurnEndObserved("s1", nowMs-1_000)

	// Act.
	err := m.HibernateWithCause("ws", registry.HibernationDetail{Cause: registry.HibernationCauseForced})

	// Assert.
	if err != nil {
		t.Fatalf("forced HibernateWithCause over a freshly ended turn = %v, want the sleep the user asked for", err)
	}
}

// THE MERGE LEASE OUTRANKS AN AUTOMATIC CAUSE TOO. The forced path checked the
// lease; the sweeper's causes did not, so an idle-cutoff sleep could stop a
// shim merge.Coordinator was actively driving.
func TestHibernateWithCauseRefusesAnAutomaticCauseWhileTheMergeLeaseIsHeld(t *testing.T) {
	// Arrange.
	m, applier, _ := newHibernationRig(t)
	applier.mergeLeases = map[string]bool{"ws": true}

	// Act.
	err := m.HibernateWithCause("ws", registry.HibernationDetail{
		Cause: registry.HibernationCauseIdleCutoff, CutoffMs: 21_600_000,
	})

	// Assert.
	if !errors.Is(err, ErrHibernationMergeLeaseHeld) {
		t.Fatalf("HibernateWithCause under a merge lease = %v, want ErrHibernationMergeLeaseHeld", err)
	}
}

// THE REFUSAL STOPS NOTHING. A merge lease is refused before the teardown, so
// the shim the merge is driving is still there afterwards.
func TestHibernateWithCauseUnderAMergeLeaseStopsNothing(t *testing.T) {
	// Arrange.
	m, applier, hib := newHibernationRig(t)
	applier.mergeLeases = map[string]bool{"ws": true}

	// Act.
	_ = m.HibernateWithCause("ws", registry.HibernationDetail{
		Cause: registry.HibernationCauseCacheExpired, TTLMs: 3_600_000,
	})

	// Assert.
	if got := hib.writeCount(); got != 0 {
		t.Fatalf("hibernation writes = %d, want 0 under a held merge lease", got)
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

// ---------------------------------------------------------------------------
// THE REWIRE IS THE GATE'S CLOSING EDGE (closeHibernationGateOnWire). A hard
// restart wires a shim over a record that still claims a sleep, and before this
// nothing retired it: the session was driveable and every prompt was nacked
// with the cause of a sleep that was already over.
// ---------------------------------------------------------------------------

// THE ORDINARY CASE. A session whose record says hibernated becomes driveable,
// and the wire edge itself retires the gate.
func TestWiringASleepingSessionRetiresItsGate(t *testing.T) {
	// Arrange.
	m, _, hib := newHibernationRig(t)
	hib.setAsleep("s1", registry.HibernationDetail{Cause: registry.HibernationCauseCacheExpired, SinceMs: 7})

	// Act.
	m.noteWired("ws", "s1")

	// Assert.
	if detail, asleep := hib.HibernationOf("s1"); asleep && detail.Cause != "" {
		t.Fatalf("hibernation detail = %+v after the rewire, want the gate retired by the wire edge", detail)
	}
}

// AND THE PROMPT THAT WAS BEING REFUSED GOES THROUGH. The record is what the
// gate reads, so retiring it is only worth anything if the refusal stops.
func TestWiringASleepingSessionStopsTheGateRefusingPrompts(t *testing.T) {
	// Arrange.
	m, _, hib := newHibernationRig(t)
	hib.setAsleep("s1", registry.HibernationDetail{Cause: registry.HibernationCauseIdleCutoff, SinceMs: 7})
	m.noteWired("ws", "s1")

	// Act.
	err := m.SubmitPrompt(context.Background(), "ws", "req-1", "hello", "", corev1.PromptOrigin_PROMPT_ORIGIN_USER_SENT)

	// Assert.
	if errors.Is(err, ErrHibernated) {
		t.Fatalf("SubmitPrompt after a rewire = %v, want the revival gate gone", err)
	}
}

// A GATED REVIVAL KEEPS ITS GATE. The compact-first and clear modes bring the
// session up ON PURPOSE while the record still says hibernated, because that
// record is what delays prompts until the cut lands.
func TestWiringDuringAGatedRevivalKeepsTheGate(t *testing.T) {
	// Arrange.
	m, _, hib := newHibernationRig(t)
	hib.setAsleep("s1", registry.HibernationDetail{Cause: registry.HibernationCauseCacheExpired, SinceMs: 7})
	release, _, err := m.claimRevival("ws", "s1")
	if err != nil {
		t.Fatalf("claimRevival: %v", err)
	}
	defer release()

	// Act.
	m.noteWired("ws", "s1")

	// Assert.
	if detail, asleep := hib.HibernationOf("s1"); !asleep || detail.Cause == "" {
		t.Fatalf("hibernation detail = %+v during a gated revival, want the gate still standing", detail)
	}
}

// AN AWAKE SESSION IS WRITTEN NOTHING. A clear against a record with no sleep
// in it would be a durable write per bring-up on every healthy session.
func TestWiringAnAwakeSessionWritesNoHibernationRecord(t *testing.T) {
	// Arrange.
	m, _, hib := newHibernationRig(t)
	before := hib.writeCount()

	// Act.
	m.noteWired("ws", "s1")

	// Assert.
	if after := hib.writeCount(); after != before {
		t.Fatalf("hibernation writes = %d after wiring an awake session, want the %d it started with", after, before)
	}
}

// A CLEAR THAT FAILED LEAVES THE GATE STANDING rather than pretending the
// session is driveable. The wire edge has no caller to return the error to, so
// the safe direction is the honest refusal the user can still revive out of.
func TestWiringSurfacesAFailedGateRetirement(t *testing.T) {
	// Arrange.
	m, _, hib := newHibernationRig(t)
	hib.setAsleep("s1", registry.HibernationDetail{Cause: registry.HibernationCauseForced, SinceMs: 7})
	hib.mu.Lock()
	hib.writeErr = errors.New("registry is unwritable")
	hib.mu.Unlock()

	// Act.
	m.noteWired("ws", "s1")

	// Assert.
	if detail, asleep := hib.HibernationOf("s1"); !asleep || detail.Cause == "" {
		t.Fatalf("hibernation detail = %+v after a failed retirement, want the sleep still recorded", detail)
	}
}
