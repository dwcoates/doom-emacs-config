package sessioncontroller

import (
	"context"
	"errors"
	"sync"
	"testing"

	corev1 "agentrepl/proto/agentshim/core/v1"

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

// lastPromptRequestID reports the id the last prompt was submitted UNDER — the
// id the shim adopts as that turn's turn_id.
func lastPromptRequestID(t *testing.T, m *Manager, workspace string) string {
	t.Helper()
	c := fakeClientFor(t, m, workspace)
	c.mu.Lock()
	defer c.mu.Unlock()
	if len(c.requestIDs) == 0 {
		t.Fatal("no prompt reached the shim")
	}
	return c.requestIDs[len(c.requestIDs)-1]
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
	closeErr error
	coverAll bool
	// hasTurnErr fails the identity lookup, the exclusion's primary question.
	hasTurnErr error
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
	if f.closeErr != nil {
		return f.closeErr
	}
	f.closed[turnID] = endedAtMs
	return nil
}

func (f *fakeKeepAliveWindows) Covers(string, int64) (bool, error) { return f.coverAll, nil }

// HasTurn answers off the rows Open wrote, exactly as the real ledger answers
// off its primary key — so a test asserting on identity is asserting against
// the same evidence production uses.
func (f *fakeKeepAliveWindows) HasTurn(workspace, turnID string) (bool, error) {
	if f.hasTurnErr != nil {
		return false, f.hasTurnErr
	}
	for _, w := range f.opened {
		if w.TurnID == turnID && w.Workspace == workspace {
			return true, nil
		}
	}
	return false, nil
}

// lastOpened reports the newest window row written for turnID, and whether one
// exists. The re-stamp rewrites the row rather than adding one, so the newest
// entry is the bound the ledger currently holds.
func (f *fakeKeepAliveWindows) lastOpened(turnID string) (KeepAliveWindowRecord, bool) {
	for i := len(f.opened) - 1; i >= 0; i-- {
		if f.opened[i].TurnID == turnID {
			return f.opened[i], true
		}
	}
	return KeepAliveWindowRecord{}, false
}

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

// THE PING IS SUBMITTED UNDER THE VERY ID ITS CLAIM, ITS HOLDS AND ITS WINDOW
// ROW ARE KEYED BY. The shim adopts the accepted SubmitPrompt's request_id as
// the turn_id of the boundaries it produces, so any other id on the wire would
// give the turn a second name — and every match the daemon makes at the ping's
// end (the claim release, the window close, the rewind's dropped-turn list)
// would then be against a name the turn does not carry.
func TestKeepAlivePingSubmitsUnderItsOwnTurnID(t *testing.T) {
	// Arrange.
	m, _, _ := keepAliveRig(t)

	// Act.
	turnID, err := m.SubmitKeepAlivePing(context.Background(), "ws")
	if err != nil {
		t.Fatalf("SubmitKeepAlivePing: %v", err)
	}

	// Assert.
	if got := lastPromptRequestID(t, m, "ws"); got != turnID {
		t.Fatalf("submitted request_id = %q, want the ping's own turn id %q — the shim stamps this id on the turn the daemon must recognize at its end", got, turnID)
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
	if entries[0].classification != VerdictHold {
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
	if got := view.GetEntries()[0].GetKeepAlive().GetTurnId(); got != turnID {
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
		classification: VerdictHold,
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
	if q.entries[0].classification != VerdictPending {
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

// ---------------------------------------------------------------------------
// The window's closing edge
// ---------------------------------------------------------------------------

// errPingSubmitFailed stands in for a shim that refuses the ping's submit.
var errPingSubmitFailed = errors.New("the shim refused the submit")

// A PING WHOSE SUBMIT FAILS CLOSES ITS OWN WINDOW. The Open that preceded the
// submit and this Close are one acquire/release pair: leaving the window open
// would exclude every later item on the workspace forever, over a ping that
// never even reached the shim.
func TestFailedKeepAlivePingClosesItsWindow(t *testing.T) {
	// Arrange.
	m, _, windows := keepAliveRig(t)
	fakeClientFor(t, m, "ws").submitErrOnce = errPingSubmitFailed

	// Act.
	if _, err := m.SubmitKeepAlivePing(context.Background(), "ws"); err == nil {
		t.Fatal("SubmitKeepAlivePing with a refused submit = nil, want the failure surfaced")
	}

	// Assert.
	if len(windows.opened) != 1 {
		t.Fatalf("opened windows = %+v, want exactly one", windows.opened)
	}
	if _, closed := windows.closed[windows.opened[0].TurnID]; !closed {
		t.Fatal("the window is still open after the ping failed; it would withhold every later item on this workspace forever")
	}
}

// AN ABANDONED PING'S WINDOW CLOSES AT ITS OWN START. Nothing ran inside it, so
// the honest interval is the instant the daemon committed to the ping — a
// later end would exclude conversation the ping never produced.
func TestFailedKeepAlivePingClosesItsWindowAtItsStart(t *testing.T) {
	// Arrange.
	m, _, windows := keepAliveRig(t)
	fakeClientFor(t, m, "ws").submitErrOnce = errPingSubmitFailed

	// Act.
	if _, err := m.SubmitKeepAlivePing(context.Background(), "ws"); err == nil {
		t.Fatal("SubmitKeepAlivePing with a refused submit = nil, want the failure surfaced")
	}

	// Assert.
	opened := windows.opened[0]
	if got := windows.closed[opened.TurnID]; got != opened.StartedAtMs {
		t.Fatalf("ended_at_ms = %d, want the window's own start %d", got, opened.StartedAtMs)
	}
}

// A PROMPT HELD BEHIND A PING WHOSE SUBMIT FAILS IS RELEASED, not stranded. Its
// hold names a turn id nothing will ever end — the sole ordinary release is the
// ping's own turn boundary — so the claim's release and the holds' release have
// to be the same operation.
func TestFailedKeepAlivePingReleasesThePromptsHeldBehindIt(t *testing.T) {
	// Arrange: a real prompt arrives INSIDE the ping's submit, which is the
	// exact window in which the claim is published and the ping is not yet
	// known to have failed.
	m, _, _ := keepAliveRig(t)
	c := fakeClientFor(t, m, "ws")
	var once sync.Once
	c.submitErrOnce = errPingSubmitFailed
	c.onSubmit = func() {
		once.Do(func() {
			if err := m.SubmitPrompt(context.Background(), "ws", "req-1", "real work", "",
				corev1.PromptOrigin_PROMPT_ORIGIN_USER_SENT); err != nil {
				t.Errorf("SubmitPrompt during the ping: %v", err)
			}
		})
	}

	// Act.
	if _, err := m.SubmitKeepAlivePing(context.Background(), "ws"); err == nil {
		t.Fatal("SubmitKeepAlivePing with a refused submit = nil, want the failure surfaced")
	}

	// Assert.
	waitFor(t, "the released prompt to reach the shim", func() bool {
		c.mu.Lock()
		defer c.mu.Unlock()
		for _, p := range c.prompts {
			if p == "real work" {
				return true
			}
		}
		return false
	})
}

// THE CLAIM AND THE HOLDS GO TOGETHER on every abandonment. A claim cleared
// without its holds leaves prompts naming a turn that will never end, waiting
// for a boundary that is never coming.
func TestAbandonKeepAlivePingReleasesTheClaimAndItsHoldsTogether(t *testing.T) {
	// Arrange: a turn is running, so the released entry stays in the queue
	// rather than being delivered out from under the assertion.
	m, _, _ := keepAliveRig(t)
	m.mu.Lock()
	d := m.byWS["ws"]
	d.keepAliveTurnID = "ka_1"
	d.turn = turnRecord{phase: turnPhaseNamed, turnID: "t1"}
	d.queue.add(&queueEntry{
		id: "q1", keepAliveHoldTurnID: "ka_1",
		classification: VerdictHold,
	})
	m.mu.Unlock()

	// Act.
	released := m.abandonKeepAlivePing(d, "ka_1")

	// Assert.
	if released != 1 {
		t.Fatalf("released %d hold(s), want 1", released)
	}
	m.mu.Lock()
	defer m.mu.Unlock()
	if d.keepAliveTurnID != "" {
		t.Fatalf("the claim is still %q after the ping was abandoned", d.keepAliveTurnID)
	}
	if d.queue.entries[0].keepAliveHeld() {
		t.Fatal("the prompt is still held behind a ping that will never end")
	}
}

// A HOLD NAMING ANOTHER PING SURVIVES an abandonment: one ping's failure must
// not free a prompt a different, live ping still owns.
func TestAbandonKeepAlivePingLeavesAnotherPingsHoldAlone(t *testing.T) {
	// Arrange.
	m, _, _ := keepAliveRig(t)
	m.mu.Lock()
	d := m.byWS["ws"]
	d.turn = turnRecord{phase: turnPhaseNamed, turnID: "t1"}
	d.queue.add(&queueEntry{id: "q1", keepAliveHoldTurnID: "ka_other"})
	m.mu.Unlock()

	// Act.
	released := m.abandonKeepAlivePing(d, "ka_1")

	// Assert.
	m.mu.Lock()
	defer m.mu.Unlock()
	if released != 0 || !d.queue.entries[0].keepAliveHeld() {
		t.Fatalf("released=%d held=%v, want another ping's hold untouched",
			released, d.queue.entries[0].keepAliveHeld())
	}
}

// THE WINDOW CLOSES AT THE BOUNDARY'S OWN INSTANT, not at a clock read taken
// while handling it. The bound is compared against vendor-authored record
// timestamps, so a daemon clock running ahead would keep excluding after the
// ping was over and swallow the leading edge of the user's next real turn.
func TestKeepAliveWindowClosesAtTheBoundarysOwnInstant(t *testing.T) {
	// Arrange.
	m, _, windows := keepAliveRig(t)
	turnID, err := m.SubmitKeepAlivePing(context.Background(), "ws")
	if err != nil {
		t.Fatalf("SubmitKeepAlivePing: %v", err)
	}
	m.mu.Lock()
	d := m.byWS["ws"]
	d.turn = turnRecord{phase: turnPhaseNamed, turnID: turnID}
	m.mu.Unlock()

	// Act.
	const endedAtMs int64 = 1_700_000_000_123
	m.onTurnBoundary(d, false, endedAtMs)

	// Assert.
	if got := windows.closed[turnID]; got != endedAtMs {
		t.Fatalf("ended_at_ms = %d, want the boundary's own instant %d", got, endedAtMs)
	}
}

// A CLOSE THAT FAILS IS A NAMED FAILURE, not a log line. The row stays open and
// withholds the workspace's whole conversation from here on; a user watching
// their next prompt vanish has no other way to learn why.
func TestKeepAliveWindowCloseFailureSurfacesANamedFailure(t *testing.T) {
	// Arrange.
	m, _, windows := keepAliveRig(t)
	turnID, err := m.SubmitKeepAlivePing(context.Background(), "ws")
	if err != nil {
		t.Fatalf("SubmitKeepAlivePing: %v", err)
	}
	windows.closeErr = errors.New("state store is unavailable")
	m.mu.Lock()
	d := m.byWS["ws"]
	d.turn = turnRecord{phase: turnPhaseNamed, turnID: turnID}
	m.mu.Unlock()

	// Act.
	m.onTurnBoundary(d, false, 1_700_000_000_123)

	// Assert.
	if !pushedFailureType(m, string(errclass.TypeKeepAliveWindowUnclosed)) {
		t.Fatalf("no %s failure reached the frontend; a permanent rendering blackout was left silent",
			errclass.TypeKeepAliveWindowUnclosed)
	}
}

// pushedFailureType reports whether any pushed conversation item carries a
// system failure of the given error type.
func pushedFailureType(m *Manager, errorType string) bool {
	p := m.cfg.Push.(*fakePusher)
	p.mu.Lock()
	defer p.mu.Unlock()
	for _, cd := range p.convo {
		for _, item := range cd.GetItems() {
			if errclass.TypeName(item.GetFailureCard()) == errorType {
				return true
			}
		}
	}
	return false
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

// THE WINDOW'S LOWER BOUND MOVES ONTO THE TURN'S OWN CLOCK. The pre-submit
// stamp is a DAEMON clock read taken before the ping existed; the items the
// window is compared against are stamped by the vendor. Left as it was, any
// skew between the two put the ping's own records outside its own window.
func TestKeepAliveWindowStartRestampsAtTheTurnsOwnBoundary(t *testing.T) {
	// Arrange.
	m, _, windows := keepAliveRig(t)
	turnID, err := m.SubmitKeepAlivePing(context.Background(), "ws")
	if err != nil {
		t.Fatalf("SubmitKeepAlivePing: %v", err)
	}

	m.mu.Lock()
	d := m.byWS["ws"]
	m.mu.Unlock()

	// Act — the ping's own TurnStarted, on the vendor's clock.
	const startedAtMs int64 = 1_700_000_000_000
	m.restampKeepAliveWindowStart(d, turnID, startedAtMs)

	// Assert.
	got, ok := windows.lastOpened(turnID)
	if !ok {
		t.Fatalf("no window row for %s", turnID)
	}
	if got.StartedAtMs != startedAtMs {
		t.Fatalf("started_at_ms = %d, want the boundary's own instant %d", got.StartedAtMs, startedAtMs)
	}
}

// THE PING'S OWN START BOUNDARY IS WHAT DRIVES THE RE-STAMP. The hook has to be
// bound on the live consumer, because the first boundary a ping produces is the
// only one carrying that instant and there is no second chance to observe it.
func TestKeepAliveWindowStartRestampsFromTheConsumersStartBoundary(t *testing.T) {
	// Arrange.
	m, _, windows := keepAliveRig(t)
	turnID, err := m.SubmitKeepAlivePing(context.Background(), "ws")
	if err != nil {
		t.Fatalf("SubmitKeepAlivePing: %v", err)
	}
	m.mu.Lock()
	d := m.byWS["ws"]
	m.mu.Unlock()

	// Act — the ping's TurnStarted, arriving over the authoritative plane.
	const startedAtMs int64 = 1_700_000_000_000
	if err := d.consumer.Apply(&corev1.Event{
		SessionId:    "vendor-uuid",
		Seq:          11,
		Plane:        corev1.Plane_PLANE_STREAM,
		ProducedAtMs: startedAtMs,
		RequestId:    turnID,
		Payload:      &corev1.Event_TurnStarted{TurnStarted: &corev1.TurnStarted{TurnId: turnID}},
	}); err != nil {
		t.Fatalf("Apply: %v", err)
	}

	// Assert.
	got, ok := windows.lastOpened(turnID)
	if !ok {
		t.Fatalf("no window row for %s", turnID)
	}
	if got.StartedAtMs != startedAtMs {
		t.Fatalf("started_at_ms = %d, want the start boundary's own instant %d", got.StartedAtMs, startedAtMs)
	}
}

// THE RE-STAMP IS MATCHED ON THE TURN ID. A real user turn starting here would
// otherwise drag the ping's lower bound onto itself, and the interval fallback
// would then hand the user's own records to the exclusion.
func TestKeepAliveWindowStartIgnoresANonPingTurnsBoundary(t *testing.T) {
	// Arrange.
	m, _, windows := keepAliveRig(t)
	turnID, err := m.SubmitKeepAlivePing(context.Background(), "ws")
	if err != nil {
		t.Fatalf("SubmitKeepAlivePing: %v", err)
	}
	before, ok := windows.lastOpened(turnID)
	if !ok {
		t.Fatalf("no window row for %s", turnID)
	}

	m.mu.Lock()
	d := m.byWS["ws"]
	m.mu.Unlock()

	// Act.
	m.restampKeepAliveWindowStart(d, "req_user", 1_700_000_000_000)

	// Assert.
	after, _ := windows.lastOpened(turnID)
	if after.StartedAtMs != before.StartedAtMs {
		t.Fatalf("started_at_ms = %d after a stranger's boundary, want the ping's own bound %d",
			after.StartedAtMs, before.StartedAtMs)
	}
}

// AN INVERTED CLOSE IS ITS OWN NAMED FAILURE, not the unclosed-window one. The
// row IS bounded, so nothing is blacked out; what is broken is the pair of
// clocks behind the two instants, and a reader told "the conversation is being
// withheld" would hunt a blackout that is not there.
func TestKeepAliveWindowInvertedCloseSurfacesItsOwnNamedFailure(t *testing.T) {
	// Arrange.
	m, _, windows := keepAliveRig(t)
	turnID, err := m.SubmitKeepAlivePing(context.Background(), "ws")
	if err != nil {
		t.Fatalf("SubmitKeepAlivePing: %v", err)
	}
	windows.closeErr = statedb.ErrKeepAliveWindowInverted
	m.mu.Lock()
	d := m.byWS["ws"]
	d.turn = turnRecord{phase: turnPhaseNamed, turnID: turnID}
	m.mu.Unlock()

	// Act.
	m.onTurnBoundary(d, false, 1_700_000_000_123)

	// Assert.
	if !pushedFailureType(m, string(errclass.TypeKeepAliveWindowInverted)) {
		t.Fatalf("no %s failure reached the frontend; a ping that stopped being excluded was left silent",
			errclass.TypeKeepAliveWindowInverted)
	}
}
