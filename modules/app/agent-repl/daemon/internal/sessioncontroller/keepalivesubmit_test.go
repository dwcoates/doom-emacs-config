package sessioncontroller

import (
	"context"
	"errors"
	"testing"

	corev1 "agentrepl/proto/agentshim/core/v1"
	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"claude-repld/internal/errclass"
	"claude-repld/internal/keepalive"
	"claude-repld/internal/registry"
	"claude-repld/internal/statedb"
)

// lastPromptText is the newest prompt the workspace's fake shim client took.
func lastPromptText(t *testing.T, m *Manager, workspace string) string {
	t.Helper()
	c := fakeClientFor(t, m, workspace)
	c.mu.Lock()
	defer c.mu.Unlock()
	if len(c.prompts) == 0 {
		t.Fatal("no prompt reached the shim")
	}
	return c.prompts[len(c.prompts)-1]
}

// lastPromptOrigin is the newest prompt's vendor-visible attribution.
func lastPromptOrigin(t *testing.T, m *Manager, workspace string) corev1.PromptOrigin {
	t.Helper()
	c := fakeClientFor(t, m, workspace)
	c.mu.Lock()
	defer c.mu.Unlock()
	if len(c.promptOrigins) == 0 {
		t.Fatal("no prompt reached the shim")
	}
	return c.promptOrigins[len(c.promptOrigins)-1]
}

// fakeClientFor reaches the workspace's live fake shim client.
func fakeClientFor(t *testing.T, m *Manager, workspace string) *fakeClient {
	t.Helper()
	m.mu.Lock()
	defer m.mu.Unlock()
	d, ok := m.byWS[workspace]
	if !ok {
		t.Fatalf("workspace %q has no live session controller", workspace)
	}
	c, ok := d.client.(*fakeClient)
	if !ok {
		t.Fatalf("workspace %q is not driven by a fake client", workspace)
	}
	return c
}

// fakeKeepAliveWindows is an in-memory window ledger.
type fakeKeepAliveWindows struct {
	opened   []KeepAliveWindowRecord
	closed   map[string]int64
	openErr  error
	coverAll bool
}

func newFakeKeepAliveWindows() *fakeKeepAliveWindows {
	return &fakeKeepAliveWindows{closed: map[string]int64{}}
}

func (f *fakeKeepAliveWindows) Open(w KeepAliveWindowRecord) error {
	if f.openErr != nil {
		return f.openErr
	}
	f.opened = append(f.opened, w)
	return nil
}

func (f *fakeKeepAliveWindows) Close(turnID string, endedAtMs int64) error {
	f.closed[turnID] = endedAtMs
	return nil
}

func (f *fakeKeepAliveWindows) Covers(string, int64) (bool, error) { return f.coverAll, nil }

// keepAliveRig is a settled, awake, brought-up session with a window ledger.
func keepAliveRig(t *testing.T) (*Manager, *fakeApplier, *fakeKeepAliveWindows) {
	t.Helper()
	m, applier, _ := newHibernationRig(t)
	windows := newFakeKeepAliveWindows()
	m.cfg.KeepAliveWindows = windows
	return m, applier, windows
}

// THE PING IS AN ORDINARY PROMPT with a distinguishing origin. A control frame
// instead would have meant the vendor never refreshed the cache, which is the
// one thing the ping is for.
func TestKeepAlivePingSubmitsWithTheKeepAliveOrigin(t *testing.T) {
	// Arrange.
	m, _, _ := keepAliveRig(t)

	// Act.
	turnID, err := m.SubmitKeepAlivePing(context.Background(), "ws")

	// Assert.
	if err != nil {
		t.Fatalf("SubmitKeepAlivePing: %v", err)
	}
	if got := lastPromptOrigin(t, m, "ws"); got != corev1.PromptOrigin_PROMPT_ORIGIN_CACHE_KEEP_ALIVE {
		t.Fatalf("prompt origin = %s, want CACHE_KEEP_ALIVE", got)
	}
	if turnID == "" {
		t.Fatal("SubmitKeepAlivePing returned no turn id; the queue hold has nothing to name")
	}
}

// The ping text is EXACT. Every consumer excludes on the origin, but a human
// reading a raw transcript has only the text, so it is a constant rather than
// anything assembled per call.
func TestKeepAlivePingSubmitsTheExactText(t *testing.T) {
	// Arrange.
	m, _, _ := keepAliveRig(t)

	// Act.
	if _, err := m.SubmitKeepAlivePing(context.Background(), "ws"); err != nil {
		t.Fatalf("SubmitKeepAlivePing: %v", err)
	}

	// Assert.
	if got := lastPromptText(t, m, "ws"); got != keepalive.PingText {
		t.Fatalf("prompt text = %q, want the exact ping literal %q", got, keepalive.PingText)
	}
}

// NOT DURING A TURN. The cache is already being refreshed by real work, and a
// ping would be a second turn racing it.
func TestKeepAlivePingDeclinedDuringALiveTurn(t *testing.T) {
	// Arrange.
	m, _, _ := keepAliveRig(t)
	m.mu.Lock()
	m.byWS["ws"].turn = turnRecord{phase: turnPhaseNamed, turnID: "t1"}
	m.mu.Unlock()

	// Act.
	_, err := m.SubmitKeepAlivePing(context.Background(), "ws")

	// Assert.
	if !errors.Is(err, ErrKeepAliveNotEligible) {
		t.Fatalf("SubmitKeepAlivePing during a turn = %v, want ErrKeepAliveNotEligible", err)
	}
}

// NOT WITH PROMPTS QUEUED. Real work is already waiting for the session, and
// pinging would put a machine-generated turn ahead of the user's own.
func TestKeepAlivePingDeclinedWithPromptsQueued(t *testing.T) {
	// Arrange.
	m, _, _ := keepAliveRig(t)
	m.mu.Lock()
	m.byWS["ws"].queue.add(&queueEntry{id: "q1", text: "real work"})
	m.mu.Unlock()

	// Act.
	_, err := m.SubmitKeepAlivePing(context.Background(), "ws")

	// Assert.
	if !errors.Is(err, ErrKeepAliveNotEligible) {
		t.Fatalf("SubmitKeepAlivePing with a queued prompt = %v, want ErrKeepAliveNotEligible", err)
	}
}

// NOT A HIBERNATED SESSION. Reaching this check at all would mean the one
// transition's construction had failed, so it is refused AND stated.
func TestKeepAlivePingDeclinedOnAHibernatedSession(t *testing.T) {
	// Arrange.
	m, _, _ := keepAliveRig(t)
	m.cfg.Hibernations.(*fakeHibernations).setAsleep("s1",
		registry.HibernationDetail{Cause: registry.HibernationCauseForced, SinceMs: 1})

	// Act.
	_, err := m.SubmitKeepAlivePing(context.Background(), "ws")

	// Assert.
	if !errors.Is(err, ErrKeepAliveNotEligible) {
		t.Fatalf("SubmitKeepAlivePing on a hibernated session = %v, want ErrKeepAliveNotEligible", err)
	}
}

// NOT UNDER A MERGE LEASE. A ping is still a turn, and it would land in the
// middle of conflict resolution. The cache is allowed to go cold instead.
func TestKeepAlivePingDeclinedUnderAMergeLease(t *testing.T) {
	// Arrange.
	m, applier, _ := keepAliveRig(t)
	applier.mergeLeases = map[string]bool{"ws": true}

	// Act.
	_, err := m.SubmitKeepAlivePing(context.Background(), "ws")

	// Assert.
	if !errors.Is(err, ErrKeepAliveNotEligible) {
		t.Fatalf("SubmitKeepAlivePing under a merge lease = %v, want ErrKeepAliveNotEligible", err)
	}
}

// NO DOUBLE-SUBMIT. A second tick while a ping is already in flight must not
// start another — including across a restart, where the durable last-turn-end
// has not moved and the policy would still say the window is open.
func TestKeepAlivePingDeclinedWhileOneIsAlreadyInFlight(t *testing.T) {
	// Arrange.
	m, _, _ := keepAliveRig(t)
	if _, err := m.SubmitKeepAlivePing(context.Background(), "ws"); err != nil {
		t.Fatalf("first SubmitKeepAlivePing: %v", err)
	}

	// Act.
	_, err := m.SubmitKeepAlivePing(context.Background(), "ws")

	// Assert.
	if !errors.Is(err, ErrKeepAliveNotEligible) {
		t.Fatalf("second SubmitKeepAlivePing = %v, want ErrKeepAliveNotEligible", err)
	}
}

// THE WINDOW OPENS BEFORE THE PROMPT REACHES THE SHIM. A ping the vendor ran
// with no window behind it renders as though the user typed it.
func TestKeepAlivePingOpensItsWindow(t *testing.T) {
	// Arrange.
	m, _, windows := keepAliveRig(t)

	// Act.
	turnID, err := m.SubmitKeepAlivePing(context.Background(), "ws")
	if err != nil {
		t.Fatalf("SubmitKeepAlivePing: %v", err)
	}

	// Assert.
	if len(windows.opened) != 1 || windows.opened[0].TurnID != turnID {
		t.Fatalf("opened windows = %+v, want exactly one naming turn %s", windows.opened, turnID)
	}
}

// A PING WHOSE WINDOW CANNOT BE RECORDED IS NOT SUBMITTED. Running it
// unwindowed would put the daemon's own plumbing in the user's conversation
// with nothing able to tell it apart afterwards.
func TestKeepAlivePingAbandonedWhenItsWindowCannotBeRecorded(t *testing.T) {
	// Arrange.
	m, _, windows := keepAliveRig(t)
	windows.openErr = errors.New("state store is unavailable")

	// Act.
	_, err := m.SubmitKeepAlivePing(context.Background(), "ws")

	// Assert.
	if err == nil {
		t.Fatal("SubmitKeepAlivePing with an unwritable window = nil, want a refusal rather than an unattributable ping")
	}
	if _, inFlight := m.KeepAliveTurnID("ws"); inFlight {
		t.Fatal("the ping claim was left standing after the window failed; nothing would ever release it")
	}
}

// THE PING EARNS NO RECEIPT. The user did not say it, and a durable receipt
// would replay that bubble across every reconnect.
func TestKeepAlivePingMintsNoPromptReceipt(t *testing.T) {
	// Arrange.
	m, _, _ := keepAliveRig(t)
	receipts := &countingReceipts{}
	m.cfg.PromptReceipts = receipts
	m.mu.Lock()
	m.byWS["ws"].consumer.receipts = receipts
	m.mu.Unlock()

	// Act.
	if _, err := m.SubmitKeepAlivePing(context.Background(), "ws"); err != nil {
		t.Fatalf("SubmitKeepAlivePing: %v", err)
	}

	// Assert.
	if receipts.recorded != 0 {
		t.Fatalf("%d prompt receipt(s) recorded for a keep-alive ping, want none", receipts.recorded)
	}
}

// ---------------------------------------------------------------------------
// The queue hold
// ---------------------------------------------------------------------------

// A REAL PROMPT ARRIVING MID-PING IS HELD, named to the ping's turn, and NOT
// classified: the turn in front of it is a machine-generated ping there is
// nothing to interject into.
func TestPromptDuringAPingIsHeldAndUnclassified(t *testing.T) {
	// Arrange.
	m, _, _ := keepAliveRig(t)
	turnID, err := m.SubmitKeepAlivePing(context.Background(), "ws")
	if err != nil {
		t.Fatalf("SubmitKeepAlivePing: %v", err)
	}

	// Act.
	if err := m.SubmitPrompt(context.Background(), "ws", "req-1", "real work", "",
		corev1.PromptOrigin_PROMPT_ORIGIN_USER_SENT); err != nil {
		t.Fatalf("SubmitPrompt: %v", err)
	}

	// Assert.
	m.mu.Lock()
	defer m.mu.Unlock()
	entries := m.byWS["ws"].queue.entries
	if len(entries) != 1 {
		t.Fatalf("%d queue entries, want the one held prompt", len(entries))
	}
	if entries[0].keepAliveHoldTurnID != turnID {
		t.Fatalf("hold turn = %q, want the in-flight ping %q", entries[0].keepAliveHoldTurnID, turnID)
	}
	if entries[0].classification != frontendv1.QueueClassification_QUEUE_CLASSIFICATION_HOLD {
		t.Fatalf("classification = %s, want HOLD; PENDING would claim a classifier is running that never will",
			entries[0].classification)
	}
}

// THE HOLD IS PROJECTED so the webapp renders a dedicated "waiting on a
// keep-alive response" bubble instead of the classifier bubble.
func TestKeepAliveHoldIsProjectedOntoTheQueueView(t *testing.T) {
	// Arrange.
	m, _, _ := keepAliveRig(t)
	turnID, err := m.SubmitKeepAlivePing(context.Background(), "ws")
	if err != nil {
		t.Fatalf("SubmitKeepAlivePing: %v", err)
	}
	if err := m.SubmitPrompt(context.Background(), "ws", "req-1", "real work", "",
		corev1.PromptOrigin_PROMPT_ORIGIN_USER_SENT); err != nil {
		t.Fatalf("SubmitPrompt: %v", err)
	}

	// Act.
	m.mu.Lock()
	view := m.byWS["ws"].queue.view("ws", "s1")
	m.mu.Unlock()

	// Assert.
	if len(view.GetEntries()) != 1 {
		t.Fatalf("%d view entries, want one", len(view.GetEntries()))
	}
	if got := view.GetEntries()[0].GetKeepAliveHold().GetTurnId(); got != turnID {
		t.Fatalf("keep_alive_hold.turn_id = %q, want %q", got, turnID)
	}
}

// A HELD ENTRY IS NOT DELIVERABLE by the ordinary drain: it must wait for the
// ping to finish so the rewind can run first.
func TestKeepAliveHeldEntryIsNotDeliverable(t *testing.T) {
	// Arrange.
	q := &promptQueue{}
	q.add(&queueEntry{id: "q1", keepAliveHoldTurnID: "ka_1"})

	// Act.
	got := q.popFrontDeliverable()

	// Assert.
	if got != nil {
		t.Fatalf("popFrontDeliverable returned %q, want nothing while the ping is in flight", got.id)
	}
}

// THERE IS NO FORCE-THROUGH. Forcing would submit the user's prompt on top of
// the keep-alive turns the rewind exists to discard, making the ping permanent
// context. The refusal is NAMED so the client renders it rather than an
// internal fault.
func TestForceQueueEntryRefusesAKeepAliveHeldPrompt(t *testing.T) {
	// Arrange.
	m, _, _ := keepAliveRig(t)
	if _, err := m.SubmitKeepAlivePing(context.Background(), "ws"); err != nil {
		t.Fatalf("SubmitKeepAlivePing: %v", err)
	}
	if err := m.SubmitPrompt(context.Background(), "ws", "req-1", "real work", "",
		corev1.PromptOrigin_PROMPT_ORIGIN_USER_SENT); err != nil {
		t.Fatalf("SubmitPrompt: %v", err)
	}
	m.mu.Lock()
	entryID := m.byWS["ws"].queue.entries[0].id
	m.mu.Unlock()

	// Act.
	err := m.ForceQueueEntry("ws", entryID)

	// Assert.
	if !errors.Is(err, errclass.ErrQueueEntryKeepAliveHeld) {
		t.Fatalf("ForceQueueEntry on a keep-alive-held prompt = %v, want ErrQueueEntryKeepAliveHeld", err)
	}
}

// QUEUE CANCEL STILL WORKS. It is one of the hold's only two exits, and the
// prompt is still the user's to take back.
func TestCancelQueueEntryWorksOnAKeepAliveHeldPrompt(t *testing.T) {
	// Arrange.
	m, _, _ := keepAliveRig(t)
	if _, err := m.SubmitKeepAlivePing(context.Background(), "ws"); err != nil {
		t.Fatalf("SubmitKeepAlivePing: %v", err)
	}
	if err := m.SubmitPrompt(context.Background(), "ws", "req-1", "real work", "",
		corev1.PromptOrigin_PROMPT_ORIGIN_USER_SENT); err != nil {
		t.Fatalf("SubmitPrompt: %v", err)
	}
	m.mu.Lock()
	entryID := m.byWS["ws"].queue.entries[0].id
	m.mu.Unlock()

	// Act.
	err := m.CancelQueueEntry("ws", entryID)

	// Assert.
	if err != nil {
		t.Fatalf("CancelQueueEntry on a keep-alive-held prompt = %v, want it cancellable", err)
	}
}

// Releasing the hold restores an ordinary queued prompt: the HOLD stamp stood
// in for a classification that never ran, and leaving it would render a chip
// claiming the prompt is still waiting on something.
func TestReleaseKeepAliveHoldRestoresAnOrdinaryEntry(t *testing.T) {
	// Arrange.
	q := &promptQueue{}
	q.add(&queueEntry{
		id: "q1", keepAliveHoldTurnID: "ka_1",
		classification: frontendv1.QueueClassification_QUEUE_CLASSIFICATION_HOLD,
	})

	// Act.
	released := q.releaseKeepAliveHold("ka_1")

	// Assert.
	if released != 1 {
		t.Fatalf("released %d entries, want 1", released)
	}
	if q.entries[0].keepAliveHeld() {
		t.Fatal("the entry is still held after its ping ended")
	}
	if q.entries[0].classification != frontendv1.QueueClassification_QUEUE_CLASSIFICATION_PENDING {
		t.Fatalf("classification = %s after release, want PENDING", q.entries[0].classification)
	}
}

// A hold naming a DIFFERENT ping is untouched, so one ping's end cannot free a
// prompt another still owns.
func TestReleaseKeepAliveHoldLeavesOtherHoldsAlone(t *testing.T) {
	// Arrange.
	q := &promptQueue{}
	q.add(&queueEntry{id: "q1", keepAliveHoldTurnID: "ka_other"})

	// Act.
	released := q.releaseKeepAliveHold("ka_1")

	// Assert.
	if released != 0 || !q.entries[0].keepAliveHeld() {
		t.Fatalf("released=%d held=%v, want another ping's hold untouched", released, q.entries[0].keepAliveHeld())
	}
}

// countingReceipts counts durable prompt-receipt writes.
type countingReceipts struct{ recorded int }

func (c *countingReceipts) Record(statedb.PromptReceipt) error {
	c.recorded++
	return nil
}
func (c *countingReceipts) Retire(string) (bool, error)                { return false, nil }
func (c *countingReceipts) RetireWorkspace(string, int64) (int, error) { return 0, nil }
func (c *countingReceipts) Outstanding(string) ([]statedb.PromptReceipt, error) {
	return nil, nil
}

// ---------------------------------------------------------------------------
// The hold spans the rewind
// ---------------------------------------------------------------------------

// THE GAP IS CLOSED. Between the ping turn's end and the rewound session coming
// back up, a fresh prompt must still park: admitted, it would start a real turn
// the rewind then SIGTERMs and truncates out of the transcript.
func TestPromptDuringTheRewindIsHeldBehindThePingTurn(t *testing.T) {
	// Arrange.
	m, _, _ := keepAliveRig(t)
	m.mu.Lock()
	m.claimKeepAliveRewindLocked("ws", "ka_1")
	m.mu.Unlock()

	// Act.
	if err := m.SubmitPrompt(context.Background(), "ws", "req-1", "real work", "",
		corev1.PromptOrigin_PROMPT_ORIGIN_USER_SENT); err != nil {
		t.Fatalf("SubmitPrompt: %v", err)
	}

	// Assert.
	m.mu.Lock()
	defer m.mu.Unlock()
	entries := m.byWS["ws"].queue.entries
	if len(entries) != 1 || entries[0].keepAliveHoldTurnID != "ka_1" {
		t.Fatalf("queue = %+v, want the prompt held behind the rewinding ping ka_1", entries)
	}
}

// NO SECOND PING DURING A REWIND EITHER. The transcript is mid-truncation and
// the shim is being replaced; a ping submitted now would be a turn against a
// session that is about to stop existing.
func TestKeepAlivePingDeclinedWhileARewindIsInFlight(t *testing.T) {
	// Arrange.
	m, _, _ := keepAliveRig(t)
	m.mu.Lock()
	m.claimKeepAliveRewindLocked("ws", "ka_1")
	m.mu.Unlock()

	// Act.
	_, err := m.SubmitKeepAlivePing(context.Background(), "ws")

	// Assert.
	if !errors.Is(err, ErrKeepAliveNotEligible) {
		t.Fatalf("SubmitKeepAlivePing during a rewind = %v, want ErrKeepAliveNotEligible", err)
	}
}

// The release matches on turn id for noteKeepAliveTurnEndedLocked's reason: a
// late tail must not free the workspace a LATER ping's rewind now owns.
func TestReleaseKeepAliveRewindLeavesALaterPingsClaimAlone(t *testing.T) {
	// Arrange.
	m, _, _ := keepAliveRig(t)
	m.mu.Lock()
	defer m.mu.Unlock()
	m.claimKeepAliveRewindLocked("ws", "ka_2")

	// Act.
	m.releaseKeepAliveRewindLocked("ws", "ka_1")

	// Assert.
	if got := m.keepAliveRewinds["ws"]; got != "ka_2" {
		t.Fatalf("rewind claim = %q after an earlier ping's release, want ka_2 untouched", got)
	}
}
